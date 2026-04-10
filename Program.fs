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
    eprintfn "  knowledge-sight stale [--repo <path>]                Find docs drifting from source"
    eprintfn "  knowledge-sight health [--repo <path>]               All checks: orphans + broken + stale"
    eprintfn "  knowledge-sight check <text|file> [--repo <path>]    Find novel knowledge in text"
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
        | "index" | "catalog" | "orphans" | "broken" | "stale" | "health" ->
            command <- args.[i]
            i <- i + 1
        | "check" when i + 1 < args.Length ->
            command <- "check"
            query <- args.[i + 1]
            i <- i + 2
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
        let changed, unchanged, removed, currentHashes = FileHashing.diffFiles relFiles oldHashes repo
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

    | "stale" | "health" ->
        match IndexStore.load cfg.IndexDir with
        | None -> eprintfn "No index found. Run: knowledge-sight index"; 1
        | Some index ->

        // Staleness check: for each doc with frontmatter, check if related source files
        // are newer than the doc itself.
        // related: can contain doc IDs (ignored here) or file paths/globs.
        let staleResults = ResizeArray<string * string * DateTime * DateTime>()
        let mentionResults = ResizeArray<string * string * string>()

        for kv in index.Frontmatters do
            let docPath = kv.Key
            let fm = kv.Value
            if not (File.Exists docPath) then () else
            let docMtime = File.GetLastWriteTimeUtc(docPath)

            for rel in fm.Related do
                // Check if this looks like a file path (has extension or path separator)
                let isFilePath = rel.Contains(".") && (rel.Contains("/") || rel.Contains("\\") || rel.EndsWith(".fs") || rel.EndsWith(".cs") || rel.EndsWith(".js") || rel.EndsWith(".ts") || rel.EndsWith(".py") || rel.EndsWith(".md"))
                if isFilePath then
                    // Try to resolve: relative to repo root, or as-is
                    let candidates = [
                        Path.Combine(repo, rel)
                        rel
                    ]
                    match candidates |> List.tryFind File.Exists with
                    | Some sourcePath ->
                        let sourceMtime = File.GetLastWriteTimeUtc(sourcePath)
                        if sourceMtime > docMtime then
                            staleResults.Add(Path.GetFileName docPath, rel, docMtime, sourceMtime)
                    | None -> ()

        // Also: scan doc content for code file mentions (e.g., "Orchestrator.fs", "Program.cs")
        // and check if those files exist and are newer
        let sourceExtensions = [| ".fs"; ".cs"; ".js"; ".ts"; ".py"; ".go"; ".rs" |]
        let codeFileRegex = System.Text.RegularExpressions.Regex(@"\b(\w+(?:\.\w+)*\.(?:fs|cs|js|ts|py|go|rs))\b", System.Text.RegularExpressions.RegexOptions.Compiled)

        match IndexStore.loadSourceChunks cfg.IndexDir with
        | None -> ()
        | Some chunks ->
            let docFiles = chunks |> Array.map (fun c -> c.FilePath) |> Array.distinct
            for docPath in docFiles do
                if File.Exists docPath then
                    let docMtime = File.GetLastWriteTimeUtc(docPath)
                    let docContent = chunks |> Array.filter (fun c -> c.FilePath = docPath) |> Array.map (fun c -> c.Content) |> String.concat "\n"
                    let codeRefs = codeFileRegex.Matches(docContent) |> Seq.cast<System.Text.RegularExpressions.Match> |> Seq.map (fun m -> m.Groups.[1].Value) |> Seq.distinct |> Seq.toArray

                    for codeRef in codeRefs do
                        // Try to find the file in the repo
                        let found =
                            try
                                Directory.EnumerateFiles(repo, codeRef, SearchOption.AllDirectories)
                                |> Seq.filter (fun f -> not (f.Contains("node_modules")) && not (f.Contains("bin")) && not (f.Contains("obj")))
                                |> Seq.tryHead
                            with _ -> None
                        match found with
                        | Some sourcePath ->
                            let sourceMtime = File.GetLastWriteTimeUtc(sourcePath)
                            if sourceMtime > docMtime then
                                let daysBehind = (sourceMtime - docMtime).TotalDays
                                if daysBehind > 1.0 then
                                    mentionResults.Add(Path.GetFileName docPath, codeRef, sprintf "%.0f days behind" daysBehind)
                        | None -> ()

        if command = "health" then
            // Run all checks
            let orphanResult = Primitives.orphans index
            let brokenResult = Primitives.broken index
            printfn "═══ Knowledge Health Report ═══"
            printfn ""
            printfn "📊 Index: %d chunks, %d links, %d docs with frontmatter" index.Chunks.Length index.Links.Length index.Frontmatters.Count
            printfn ""
            printfn "🔗 Orphans: %d docs with no incoming links" orphanResult.Length
            for d in orphanResult |> Array.truncate 5 do
                printfn "   %s — %s" (string d.["file"]) (string d.["title"])
            if orphanResult.Length > 5 then printfn "   ... and %d more" (orphanResult.Length - 5)
            printfn ""
            printfn "💔 Broken links: %d" brokenResult.Length
            for d in brokenResult |> Array.truncate 5 do
                printfn "   %s → %s" (string d.["from"]) (string d.["target"])
            if brokenResult.Length > 5 then printfn "   ... and %d more" (brokenResult.Length - 5)
            printfn ""
            printfn "⏰ Stale (related source newer than doc): %d" staleResults.Count
            for (doc, source, _, _) in staleResults do
                printfn "   %s ← %s changed" doc source
            printfn ""
            printfn "📝 Mentions (doc references code that changed since): %d" mentionResults.Count
            for (doc, codeRef, age) in mentionResults |> Seq.truncate 10 |> Seq.toList do
                printfn "   %s mentions %s (%s)" doc codeRef age
            if mentionResults.Count > 10 then printfn "   ... and %d more" (mentionResults.Count - 10)
            printfn ""
            let total = orphanResult.Length + brokenResult.Length + staleResults.Count + mentionResults.Count

            // Folder density check — count files (not chunks) per directory
            let fileDirCounts =
                index.Chunks
                |> Array.map (fun c ->
                    let rel = Path.GetRelativePath(repo, c.FilePath).Replace("\\", "/")
                    let parts = rel.Split('/')
                    let dir = if parts.Length >= 2 then parts.[.. parts.Length - 2] |> String.concat "/" else "."
                    dir, Path.GetFileName c.FilePath)
                |> Array.distinct
                |> Array.countBy fst
            let denseDirs = fileDirCounts |> Array.filter (fun (_, count) -> count >= 8) |> Array.sortByDescending snd
            if denseDirs.Length > 0 then
                printfn "📁 Dense folders (consider splitting into subfolders):"
                for (dir, count) in denseDirs do
                    printfn "   %s/ — %d docs (use: knowledge-sight search 'cluster(\"%s\")')" dir count dir
                printfn ""
            let total = total + denseDirs.Length

            if total = 0 then printfn "✅ All clean!"
            else printfn "⚠ %d issues found" total
        else
            // Just stale
            printfn "Stale docs (related source newer than doc): %d" staleResults.Count
            for (doc, source, docTime, srcTime) in staleResults do
                printfn "  %s ← %s (source: %s, doc: %s)" doc source (srcTime.ToString("yyyy-MM-dd")) (docTime.ToString("yyyy-MM-dd"))
            printfn ""
            printfn "Docs mentioning changed code files: %d" mentionResults.Count
            for (doc, codeRef, age) in mentionResults |> Seq.truncate 20 |> Seq.toList do
                printfn "  %s mentions %s (%s)" doc codeRef age
            if mentionResults.Count > 20 then printfn "  ... and %d more" (mentionResults.Count - 20)
        0

    | "check" when query <> "" ->
        match IndexStore.load cfg.IndexDir with
        | None -> eprintfn "No index found. Run: knowledge-sight index"; 1
        | Some index ->
            // Input can be a file path or inline text
            let text =
                if File.Exists query then File.ReadAllText(query)
                elif File.Exists(Path.Combine(repo, query)) then File.ReadAllText(Path.Combine(repo, query))
                elif query = "-" then
                    // Read from stdin
                    use reader = new StreamReader(Console.OpenStandardInput())
                    reader.ReadToEnd()
                else query
            let results = Primitives.novelty index cfg.EmbeddingUrl text 0.75
            let novel = results |> Array.filter (fun d -> string d.["status"] = "novel")
            let covered = results |> Array.filter (fun d -> string d.["status"] = "covered")
            let musing = results |> Array.filter (fun d -> string d.["status"] = "musing")
            let offTopic = results |> Array.filter (fun d -> string d.["status"] = "off-topic")

            printfn "═══ Knowledge Check ═══"
            printfn "  %d paragraphs analyzed" results.Length
            printfn "  🆕 %d novel (new knowledge to capture)" novel.Length
            printfn "  ✅ %d covered (already in knowledge base)" covered.Length
            printfn "  💭 %d musings (discussion, not knowledge)" musing.Length
            printfn "  ❌ %d off-topic (unrelated to project)" offTopic.Length

            if novel.Length > 0 then
                printfn ""
                printfn "── Novel knowledge to capture ──"
                for d in novel do
                    let score = d.["score"] :?> float
                    let signal = d.["signal"] :?> int
                    printfn "  [%.2f sig=%d] %s" score signal (string d.["paragraph"])
                    printfn "       nearest: %s > %s" (string d.["nearDoc"]) (string d.["nearSection"])
                    printfn ""

            if covered.Length > 0 then
                printfn "── Already covered ──"
                for d in covered |> Array.truncate 5 do
                    printfn "  [%.2f] %s" (d.["score"] :?> float) (string d.["paragraph"])
                    printfn "       in: %s > %s" (string d.["nearDoc"]) (string d.["nearSection"])
                if covered.Length > 5 then printfn "  ... and %d more" (covered.Length - 5)
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
