open System
open System.IO
open AITeam.KnowledgeSight

let printUsage () =
    eprintfn "AITeam.KnowledgeSight — knowledge/doc intelligence for any repo"
    eprintfn ""
    eprintfn "Usage:"
    eprintfn "  knowledge-sight index [--repo <path>]                Build/update index"
    eprintfn "  knowledge-sight catalog [--repo <path>]              Show topic map"
    eprintfn "  knowledge-sight search <js> [--repo <path>]          Run a query"
    eprintfn "  knowledge-sight orphans [--repo <path>]              Find unlinked docs"
    eprintfn "  knowledge-sight broken [--repo <path>]               Find broken links"
    eprintfn ""

let parseArgs (args: string[]) =
    let mutable repo = Environment.CurrentDirectory
    let mutable command = ""
    let mutable query = ""
    let mutable i = 0
    while i < args.Length do
        match args.[i] with
        | "--repo" when i + 1 < args.Length ->
            repo <- args.[i + 1]
            i <- i + 2
        | "index" | "catalog" | "orphans" | "broken" ->
            command <- args.[i]
            i <- i + 1
        | "search" when i + 1 < args.Length ->
            command <- "search"
            query <- args.[i + 1]
            i <- i + 2
        | s when command = "" && not (s.StartsWith("--")) ->
            command <- "search"
            query <- s
            i <- i + 1
        | _ -> i <- i + 1
    repo, command, query

[<EntryPoint>]
let main args =
    if args.Length = 0 then printUsage(); 0
    else

    let repo, command, query = parseArgs args
    let cfg = Config.load repo

    match command with
    | "index" ->
        eprintfn "▶ Indexing docs in %s" repo
        let docFiles = Config.findDocFiles cfg
        eprintfn "  Found %d markdown files" docFiles.Length
        if docFiles.Length = 0 then
            eprintfn "  No .md files found. Check docDirs in knowledge-sight.json."
            1
        else

        // Incremental: check hashes
        let hashesPath = Path.Combine(cfg.IndexDir, "hashes.json")
        let oldHashes = FileHashing.loadHashes hashesPath
        let relFiles = docFiles |> Array.map (fun f -> Path.GetRelativePath(repo, f))
        let changed, unchanged, removed, currentHashes = FileHashing.diffFiles relFiles oldHashes
        let relToAbs = Array.zip relFiles docFiles |> Map.ofArray

        if changed.Length = 0 && removed.Length = 0 then
            eprintfn "Index is up to date (%d files, no changes)" docFiles.Length
        else
            eprintfn "  Changed: %d, Unchanged: %d, Removed: %d" changed.Length unchanged.Length removed.Length
            let unchangedSet = Set.ofArray unchanged

            // Parse changed files
            let changedAbs = changed |> Array.map (fun rel -> Map.find rel relToAbs)
            eprintfn "▶ Parsing %d changed files..." changed.Length
            let newChunks, newLinks, newFm = MarkdownChunker.chunkAll changedAbs repo
            eprintfn "  %d chunks, %d links from changed files" newChunks.Length newLinks.Length

            // Load cached source chunks for unchanged files
            let cachedSourceChunks =
                match IndexStore.loadSourceChunks cfg.IndexDir with
                | Some cached -> cached |> Array.filter (fun c -> unchangedSet.Contains(Path.GetRelativePath(repo, c.FilePath)))
                | None -> [||]

            let allSourceChunks = Array.append cachedSourceChunks newChunks

            // Load existing index for unchanged chunks
            let existingIdx = IndexStore.load cfg.IndexDir
            let oldChunks =
                match existingIdx with
                | Some idx -> idx.Chunks |> Array.filter (fun c -> unchangedSet.Contains(Path.GetRelativePath(repo, c.FilePath)))
                | None -> [||]
            let oldEmbeddings =
                match existingIdx with
                | Some idx ->
                    idx.Chunks |> Array.indexed
                    |> Array.filter (fun (_, c) -> unchangedSet.Contains(Path.GetRelativePath(repo, c.FilePath)))
                    |> Array.map (fun (i, _) -> if i < idx.Embeddings.Length then idx.Embeddings.[i] else [||])
                | None -> [||]
            let oldLinks =
                match existingIdx with
                | Some idx -> idx.Links |> Array.filter (fun l -> unchangedSet.Contains(Path.GetRelativePath(repo, l.SourceFile)))
                | None -> [||]
            let oldFm =
                match existingIdx with
                | Some idx -> idx.Frontmatters |> Map.filter (fun k _ -> unchangedSet.Contains(Path.GetRelativePath(repo, k)))
                | None -> Map.empty

            // Build chunk entries from new chunks
            let newEntries =
                newChunks |> Array.map (fun c ->
                    { FilePath = c.FilePath; Heading = c.Heading; HeadingPath = c.HeadingPath
                      Level = c.Level; StartLine = c.StartLine; EndLine = c.EndLine
                      Summary = c.Summary
                      Tags = c.Tags |> String.concat ","
                      LinkCount = c.OutLinks.Length
                      WordCount = c.Content.Split([| ' '; '\n'; '\t' |], StringSplitOptions.RemoveEmptyEntries).Length })

            let allEntries = Array.append oldChunks newEntries
            let allLinks = Array.append oldLinks newLinks
            let allFm = Map.fold (fun acc k v -> Map.add k v acc) oldFm newFm

            // Embed new chunks
            eprintfn "▶ Embedding %d new chunks..." newEntries.Length
            let textsToEmbed =
                newChunks |> Array.map (fun c ->
                    sprintf "search_document: %s\n%s" c.HeadingPath (if c.Summary <> "(no summary)" then c.Summary else c.Content.Substring(0, min 200 c.Content.Length)))

            let newEmbeddings =
                if textsToEmbed.Length = 0 then [||]
                else
                    textsToEmbed
                    |> Array.chunkBySize cfg.EmbeddingBatchSize
                    |> Array.collect (fun batch ->
                        eprintfn "  Embedding batch of %d..." batch.Length
                        match EmbeddingService.embed cfg.EmbeddingUrl batch |> Async.AwaitTask |> Async.RunSynchronously with
                        | Some embs -> embs
                        | None ->
                            eprintfn "  ⚠ Embedding failed — using zero vectors"
                            Array.init batch.Length (fun _ -> [||]))

            let allEmbeddings = Array.append oldEmbeddings newEmbeddings
            let dim = if allEmbeddings.Length > 0 && allEmbeddings.[0].Length > 0 then allEmbeddings.[0].Length else 0

            let index = { Chunks = allEntries; Embeddings = allEmbeddings; Links = allLinks
                          Frontmatters = allFm; EmbeddingDim = dim }
            IndexStore.save cfg.IndexDir index
            IndexStore.saveSourceChunks cfg.IndexDir allSourceChunks

            // Save hashes
            Directory.CreateDirectory(cfg.IndexDir) |> ignore
            FileHashing.saveHashes hashesPath (currentHashes |> Map.filter (fun k _ -> not (Set.contains k (Set.ofArray removed))))

            eprintfn "✓ Index complete: %d chunks, %d links, %d docs with frontmatter, dim=%d"
                allEntries.Length allLinks.Length allFm.Count dim
        0

    | "catalog" ->
        match IndexStore.load cfg.IndexDir with
        | None -> eprintfn "No index found. Run: knowledge-sight index"; 1
        | Some index ->
            let result = Primitives.catalog index
            printfn "%s" (Format.formatValue (box result))
            0

    | "orphans" ->
        match IndexStore.load cfg.IndexDir with
        | None -> eprintfn "No index found. Run: knowledge-sight index"; 1
        | Some index ->
            let result = Primitives.orphans index
            printfn "%d orphaned docs (no incoming links):" result.Length
            for d in result do
                let file = string d.["file"]
                let title = string d.["title"]
                let sections = d.["sections"] :?> int
                printfn "  %s — %s (%d sections)" file title sections
            0

    | "broken" ->
        match IndexStore.load cfg.IndexDir with
        | None -> eprintfn "No index found. Run: knowledge-sight index"; 1
        | Some index ->
            let result = Primitives.broken index
            printfn "%d broken links:" result.Length
            for d in result do
                printfn "  %s → %s (in %s)" (string d.["from"]) (string d.["target"]) (string d.["section"])
            0

    | "search" | _ when query <> "" ->
        match IndexStore.load cfg.IndexDir with
        | None -> eprintfn "No index found. Run: knowledge-sight index"; 1
        | Some index ->
            let chunks = IndexStore.loadSourceChunks cfg.IndexDir
            let engine = QueryEngine.create index chunks cfg.EmbeddingUrl cfg.IndexDir
            let result = QueryEngine.eval engine query
            printfn "%s" result
            0

    | _ ->
        printUsage()
        0
