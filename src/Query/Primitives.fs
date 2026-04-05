namespace AITeam.KnowledgeSight

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

/// Ref tracking for expand/neighborhood across queries.
type QuerySession(indexDir: string) =
    let refsPath = Path.Combine(indexDir, "refs.json")
    let refs = Dictionary<string, int>()
    let mutable counter = 0

    do
        if File.Exists refsPath then
            try
                let json = File.ReadAllText(refsPath)
                let doc = System.Text.Json.JsonDocument.Parse(json)
                for prop in doc.RootElement.EnumerateObject() do
                    refs.[prop.Name] <- prop.Value.GetInt32()
                    let num = prop.Name.Substring(1) |> int
                    if num > counter then counter <- num
            with _ -> ()

    member _.NextRef(chunkIdx) =
        counter <- counter + 1
        let id = sprintf "R%d" counter
        refs.[id] <- chunkIdx
        try
            let dict = Dictionary<string, int>()
            for kv in refs do dict.[kv.Key] <- kv.Value
            let json = System.Text.Json.JsonSerializer.Serialize(dict)
            File.WriteAllText(refsPath, json)
        with _ -> ()
        id

    member _.GetRef(id: string) =
        match refs.TryGetValue(id) with true, v -> Some v | _ -> None

/// All query primitives for knowledge/doc operations.
module Primitives =

    let private embedQuery (url: string) (query: string) =
        EmbeddingService.embed url [| sprintf "search_query: %s" query |]
        |> Async.AwaitTask |> Async.RunSynchronously
        |> Option.map (fun e -> e.[0])

    /// Find source chunk matching an index entry.
    let private findSource (chunks: DocChunk[] option) (c: ChunkEntry) =
        chunks |> Option.bind (fun chs ->
            chs |> Array.tryFind (fun ch ->
                ch.FilePath = c.FilePath && ch.Heading = c.Heading && ch.StartLine = c.StartLine))

    // ── catalog (like modules in code-sight) ──

    let catalog (index: DocIndex) =
        index.Chunks |> Array.groupBy (fun c ->
            let parts = c.FilePath.Replace("\\", "/").Split('/')
            // Group by first directory under repo (e.g., "knowledge", "design", "pocs")
            let dotsIdx = parts |> Array.tryFindIndex (fun p -> p.StartsWith("."))
            match dotsIdx with
            | Some i when i + 1 < parts.Length -> parts.[i] + "/" + parts.[i + 1]
            | Some i -> parts.[i]
            | None ->
                if parts.Length >= 2 then parts.[parts.Length - 2]
                else "root")
        |> Array.sortBy fst
        |> Array.map (fun (dir, chunks) ->
            let fileNames = chunks |> Array.map (fun c -> Path.GetFileName c.FilePath) |> Array.distinct |> Array.sort
            let allTags = chunks |> Array.collect (fun c -> c.Tags.Split(',') |> Array.filter ((<>) "")) |> Array.distinct |> Array.truncate 8
            let topTitles = chunks |> Array.filter (fun c -> c.Level <= 1) |> Array.truncate 3 |> Array.map (fun c -> c.Heading)
            mdict [ "directory", box dir; "docs", box fileNames.Length; "sections", box chunks.Length
                    "fileList", box (fileNames |> String.concat ", ")
                    "topTags", box (allTags |> String.concat ", ")
                    "titles", box (topTitles |> String.concat "; ") ])

    // ── search ──

    let search (index: DocIndex) (session: QuerySession) (chunks: DocChunk[] option) (embeddingUrl: string)
               (query: string) (limit: int) (tag: string) (filePattern: string) =
        match embedQuery embeddingUrl query with
        | None -> [||]
        | Some qEmb ->
            IndexStore.search index qEmb (limit * 3)
            |> Array.filter (fun (i, _) ->
                let c = index.Chunks.[i]
                (String.IsNullOrEmpty(tag) || c.Tags.Contains(tag, StringComparison.OrdinalIgnoreCase)) &&
                (String.IsNullOrEmpty(filePattern) || c.FilePath.Contains(filePattern, StringComparison.OrdinalIgnoreCase)))
            |> Array.truncate limit
            |> Array.map (fun (i, sim) ->
                let c = index.Chunks.[i]
                let id = session.NextRef(i)
                mdict [ "id", box id; "score", box (Math.Round(float sim, 3))
                        "heading", box c.Heading; "headingPath", box c.HeadingPath
                        "file", box (Path.GetFileName c.FilePath); "path", box c.FilePath
                        "line", box c.StartLine; "summary", box c.Summary
                        "tags", box c.Tags; "links", box c.LinkCount; "words", box c.WordCount ])

    // ── context ──

    let context (index: DocIndex) (session: QuerySession) (fileName: string) =
        let fileChunks = IndexStore.fileChunks index fileName
        let backlinks = IndexStore.backlinks index fileName
        let outlinks = IndexStore.outlinks index fileName
        let fm = index.Frontmatters |> Map.tryFind fileName
                 |> Option.orElseWith (fun () ->
                    index.Frontmatters |> Map.toSeq |> Seq.tryFind (fun (k, _) -> IndexStore.matchFile k fileName) |> Option.map snd)
        mdict [
            "file", box fileName
            "title", box (fm |> Option.map (fun f -> f.Title) |> Option.defaultValue "")
            "status", box (fm |> Option.map (fun f -> f.Status) |> Option.defaultValue "")
            "tags", box (fm |> Option.map (fun f -> f.Tags |> String.concat ", ") |> Option.defaultValue "")
            "related", box (fm |> Option.map (fun f -> f.Related |> String.concat ", ") |> Option.defaultValue "")
            "sections", box (fileChunks |> Array.map (fun (i, c) ->
                let id = session.NextRef(i)
                mdict [ "id", box id; "heading", box c.Heading; "level", box c.Level
                        "line", box c.StartLine; "summary", box c.Summary
                        "words", box c.WordCount; "links", box c.LinkCount ]))
            "backlinks", box (backlinks |> Array.map (fun l ->
                mdict [ "from", box (Path.GetFileName l.SourceFile); "section", box l.SourceHeading; "text", box l.LinkText ]))
            "outlinks", box (outlinks |> Array.map (fun l ->
                let resolved = if l.TargetResolved <> "" then Path.GetFileName l.TargetResolved else sprintf "⚠ %s" l.TargetPath
                mdict [ "to", box resolved; "text", box l.LinkText; "section", box l.SourceHeading ]))
        ]

    // ── expand ──

    let expand (index: DocIndex) (session: QuerySession) (chunks: DocChunk[] option) (refId: string) =
        match session.GetRef(refId) with
        | None -> mdict [ "error", box (sprintf "ref %s not found" refId) ]
        | Some chunkIdx ->
            let c = index.Chunks.[chunkIdx]
            let content = findSource chunks c |> Option.map (fun ch -> ch.Content) |> Option.defaultValue "(source not loaded)"
            let backlinks = IndexStore.backlinks index (Path.GetFileName c.FilePath)
            mdict [ "id", box refId; "heading", box c.Heading; "headingPath", box c.HeadingPath
                    "file", box (Path.GetFileName c.FilePath); "line", box c.StartLine
                    "endLine", box c.EndLine; "summary", box c.Summary
                    "tags", box c.Tags; "content", box content
                    "backlinks", box (backlinks |> Array.map (fun l -> sprintf "%s (%s)" (Path.GetFileName l.SourceFile) l.LinkText)) ]

    // ── neighborhood ──

    let neighborhood (index: DocIndex) (session: QuerySession) (chunks: DocChunk[] option) (refId: string) (beforeCount: int) (afterCount: int) =
        match session.GetRef(refId) with
        | None -> mdict [ "error", box (sprintf "ref %s not found" refId) ]
        | Some chunkIdx ->
            let target = index.Chunks.[chunkIdx]
            let fileChunks = index.Chunks |> Array.indexed |> Array.filter (fun (_, c) -> c.FilePath = target.FilePath) |> Array.sortBy (fun (_, c) -> c.StartLine)
            let targetPos = fileChunks |> Array.tryFindIndex (fun (i, _) -> i = chunkIdx) |> Option.defaultValue 0
            let mkCompact (i, c: ChunkEntry) =
                let id = session.NextRef(i)
                mdict [ "id", box id; "heading", box c.Heading; "level", box c.Level
                        "line", box c.StartLine; "summary", box c.Summary; "words", box c.WordCount ]
            let beforeChunks = fileChunks.[max 0 (targetPos - beforeCount) .. max 0 (targetPos - 1)] |> Array.map mkCompact
            let afterChunks = fileChunks.[min (fileChunks.Length - 1) (targetPos + 1) .. min (fileChunks.Length - 1) (targetPos + afterCount)] |> Array.filter (fun (i, _) -> i <> chunkIdx) |> Array.map mkCompact
            let targetContent = findSource chunks target |> Option.map (fun ch -> ch.Content) |> Option.defaultValue "(source not loaded)"
            mdict [ "file", box (Path.GetFileName target.FilePath)
                    "before", box beforeChunks
                    "target", box (mdict [ "id", box refId; "heading", box target.Heading; "level", box target.Level
                                           "line", box target.StartLine; "summary", box target.Summary; "content", box targetContent ])
                    "after", box afterChunks ]

    // ── similar ──

    let similar (index: DocIndex) (session: QuerySession) (refId: string) (limit: int) =
        match session.GetRef(refId) with
        | None -> [||]
        | Some chunkIdx ->
            IndexStore.similar index chunkIdx limit
            |> Array.map (fun (i, sim) ->
                let c = index.Chunks.[i]
                let id = session.NextRef(i)
                mdict [ "id", box id; "score", box (Math.Round(float sim, 3))
                        "heading", box c.Heading; "file", box (Path.GetFileName c.FilePath)
                        "line", box c.StartLine; "summary", box c.Summary; "tags", box c.Tags ])

    // ── grep ──

    let grep (index: DocIndex) (session: QuerySession) (chunks: DocChunk[] option) (pattern: string) (limit: int) (filePattern: string) =
        match chunks with
        | None -> [||]
        | Some allChunks ->
            let regex = try Regex(pattern, RegexOptions.IgnoreCase ||| RegexOptions.Compiled) with _ -> Regex(Regex.Escape(pattern), RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
            let results = ResizeArray()
            for i in 0..index.Chunks.Length-1 do
                if results.Count < limit then
                    let c = index.Chunks.[i]
                    if String.IsNullOrEmpty(filePattern) || c.FilePath.Contains(filePattern, StringComparison.OrdinalIgnoreCase) then
                        match findSource (Some allChunks) c with
                        | Some ch when regex.IsMatch(ch.Content) ->
                            let matchLine = ch.Content.Split('\n') |> Array.tryFind (fun l -> regex.IsMatch(l)) |> Option.map (fun l -> l.Trim()) |> Option.defaultValue ""
                            let id = session.NextRef(i)
                            results.Add(mdict [ "id", box id; "heading", box c.Heading; "file", box (Path.GetFileName c.FilePath)
                                                "path", box c.FilePath; "line", box c.StartLine; "matchLine", box matchLine
                                                "summary", box c.Summary; "tags", box c.Tags ])
                        | _ -> ()
            results.ToArray()

    // ── mentions (like refs in code-sight) ──

    let mentions (index: DocIndex) (session: QuerySession) (chunks: DocChunk[] option) (term: string) (limit: int) =
        match chunks with
        | None -> [||]
        | Some allChunks ->
            let regex = Regex(sprintf @"\b%s\b" (Regex.Escape term), RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
            let results = ResizeArray()
            for i in 0..index.Chunks.Length-1 do
                if results.Count < limit then
                    let c = index.Chunks.[i]
                    match findSource (Some allChunks) c with
                    | Some ch when regex.IsMatch(ch.Content) ->
                        let matchLine = ch.Content.Split('\n') |> Array.tryFind (fun l -> regex.IsMatch(l)) |> Option.map (fun l -> l.Trim()) |> Option.defaultValue ""
                        let count = regex.Matches(ch.Content).Count
                        let id = session.NextRef(i)
                        results.Add(mdict [ "id", box id; "heading", box c.Heading; "file", box (Path.GetFileName c.FilePath)
                                            "line", box c.StartLine; "matchLine", box matchLine; "count", box count
                                            "summary", box c.Summary; "tags", box c.Tags ])
                    | _ -> ()
            results.ToArray()

    // ── files ──

    let files (index: DocIndex) (pattern: string) =
        index.Chunks |> Array.groupBy (fun c -> c.FilePath)
        |> Array.choose (fun (filePath, chunks) ->
            let fileName = Path.GetFileName(filePath)
            if String.IsNullOrEmpty(pattern) || fileName.Contains(pattern, StringComparison.OrdinalIgnoreCase) || filePath.Contains(pattern, StringComparison.OrdinalIgnoreCase) then
                let fm = index.Frontmatters |> Map.tryFind filePath
                let title = fm |> Option.map (fun f -> f.Title) |> Option.defaultValue ""
                let tags = fm |> Option.map (fun f -> f.Tags |> String.concat ",") |> Option.defaultValue ""
                let backlinks = IndexStore.backlinks index filePath
                Some (mdict [ "file", box fileName; "path", box filePath; "sections", box chunks.Length
                              "title", box title; "tags", box tags; "backlinks", box backlinks.Length
                              "words", box (chunks |> Array.sumBy (fun c -> c.WordCount)) ])
            else None)
        |> Array.sortBy (fun d -> string d.["file"])

    // ── backlinks ──

    let backlinks (index: DocIndex) (session: QuerySession) (fileName: string) =
        IndexStore.backlinks index fileName
        |> Array.map (fun l ->
            mdict [ "from", box (Path.GetFileName l.SourceFile); "section", box l.SourceHeading
                    "text", box l.LinkText; "line", box l.Line
                    "resolved", box (if l.TargetResolved <> "" then "✓" else "✗") ])

    // ── links (outgoing) ──

    let links (index: DocIndex) (fileName: string) =
        IndexStore.outlinks index fileName
        |> Array.map (fun l ->
            let resolved = if l.TargetResolved <> "" then Path.GetFileName l.TargetResolved else sprintf "⚠ %s" l.TargetPath
            mdict [ "to", box resolved; "text", box l.LinkText; "section", box l.SourceHeading; "line", box l.Line ])

    // ── orphans — docs with no incoming links ──

    let orphans (index: DocIndex) =
        let allFiles = index.Chunks |> Array.map (fun c -> c.FilePath) |> Array.distinct
        let linkedFiles = index.Links |> Array.choose (fun l -> if l.TargetResolved <> "" then Some l.TargetResolved else None) |> Set.ofArray
        allFiles
        |> Array.filter (fun f -> not (linkedFiles.Contains f))
        |> Array.map (fun f ->
            let fm = index.Frontmatters |> Map.tryFind f
            let title = fm |> Option.map (fun f -> f.Title) |> Option.defaultValue ""
            let sections = index.Chunks |> Array.filter (fun c -> c.FilePath = f)
            mdict [ "file", box (Path.GetFileName f); "path", box f; "title", box title; "sections", box sections.Length ])

    // ── broken — links pointing to nonexistent docs ──

    let broken (index: DocIndex) =
        index.Links
        |> Array.filter (fun l -> l.TargetResolved = "")
        |> Array.map (fun l ->
            mdict [ "from", box (Path.GetFileName l.SourceFile); "target", box l.TargetPath
                    "text", box l.LinkText; "section", box l.SourceHeading; "line", box l.Line ])

    // ── placement — where should new content go? ──

    let placement (index: DocIndex) (embeddingUrl: string) (content: string) (limit: int) =
        match embedQuery embeddingUrl content with
        | None -> [||]
        | Some qEmb ->
            // Find most similar sections, then group by file to suggest placement
            let hits = IndexStore.search index qEmb (limit * 3)
            let byFile =
                hits |> Array.groupBy (fun (i, _) -> index.Chunks.[i].FilePath)
                |> Array.map (fun (file, matches) ->
                    let avgScore = matches |> Array.averageBy (fun (_, s) -> float s)
                    let bestMatch = matches |> Array.maxBy snd
                    let bestChunk = index.Chunks.[fst bestMatch]
                    file, avgScore, bestChunk.Heading, bestChunk.HeadingPath)
                |> Array.sortByDescending (fun (_, score, _, _) -> score)
                |> Array.truncate limit
            byFile |> Array.map (fun (file, score, heading, headingPath) ->
                let fm = index.Frontmatters |> Map.tryFind file
                let title = fm |> Option.map (fun f -> f.Title) |> Option.defaultValue ""
                mdict [ "file", box (Path.GetFileName file); "score", box (Math.Round(score, 3))
                        "nearSection", box heading; "sectionPath", box headingPath; "title", box title ])

    // ── walk — traverse the link graph ──

    let walk (index: DocIndex) (session: QuerySession) (startFile: string) (maxDepth: int) (direction: string) =
        let visited = HashSet<string>()
        let results = ResizeArray<Dictionary<string, obj>>()

        let rec trace (file: string) (depth: int) (trail: string list) =
            if depth > maxDepth || visited.Contains(file) || results.Count >= maxDepth * 10 then ()
            else
                visited.Add(file) |> ignore
                let neighbors =
                    if direction = "in" then
                        IndexStore.backlinks index file |> Array.map (fun l -> l.SourceFile, l.LinkText)
                    else
                        IndexStore.outlinks index file |> Array.map (fun l -> l.TargetResolved, l.LinkText)
                    |> Array.filter (fun (f, _) -> f <> "" && not (visited.Contains f))
                    |> Array.distinctBy fst

                for (nextFile, linkText) in neighbors do
                    let nextTrail = trail @ [sprintf "%s (%s)" (Path.GetFileName nextFile) linkText]
                    results.Add(mdict [
                        "hop", box depth; "file", box (Path.GetFileName nextFile)
                        "path", box nextFile; "via", box linkText
                        "trail", box (nextTrail |> String.concat " → ")
                    ])
                    trace nextFile (depth + 1) nextTrail

        let startResolved =
            index.Chunks |> Array.tryFind (fun c -> IndexStore.matchFile c.FilePath startFile) |> Option.map (fun c -> c.FilePath) |> Option.defaultValue startFile
        trace startResolved 1 [Path.GetFileName startResolved]
        results.ToArray()

    // ── novelty — what's new in this text vs existing knowledge? ──

    /// Heuristic: does this paragraph look like knowledge vs casual musing?
    let private knowledgeSignal (para: string) (index: DocIndex) =
        let lower = para.ToLowerInvariant()
        let mutable score = 0

        // Prescriptive language (knowledge patterns)
        let prescriptive = [| " should "; " must "; " always "; " never "; " when "; " ensure "; " requires "; " depends on "; " means that " |]
        for p in prescriptive do if lower.Contains(p) then score <- score + 2

        // Causal connectors (reasoning)
        let causal = [| " because "; " therefore "; " so that "; " in order to "; " consequence "; " implies "; " leads to " |]
        for c in causal do if lower.Contains(c) then score <- score + 2

        // Declarative structure (definitions/facts)
        let declarative = [| " is a "; " are "; " defines "; " represents "; " consists of "; " handles "; " processes " |]
        for d in declarative do if lower.Contains(d) then score <- score + 1

        // Hedging / uncertainty (musing patterns — deduct)
        let hedging = [| " maybe "; " perhaps "; " i wonder "; " not sure "; " might "; " could be "; " i think "; "?" |]
        for h in hedging do if lower.Contains(h) then score <- score - 2

        // Concrete code references (file names, types from the index)
        let codeRefRegex = Regex(@"\b\w+\.(fs|cs|js|ts|py|md)\b", RegexOptions.Compiled)
        let codeRefs = codeRefRegex.Matches(para).Count
        score <- score + codeRefs * 2

        // Type/module names from the index
        let indexNames = index.Chunks |> Array.map (fun c -> c.Heading) |> Array.distinct
        let nameHits = indexNames |> Array.filter (fun name -> name.Length > 3 && para.Contains(name, StringComparison.OrdinalIgnoreCase))
        score <- score + nameHits.Length

        // Length bonus — very short paragraphs are rarely knowledge
        if para.Length < 50 then score <- score - 2

        score

    /// Split text into paragraphs, embed each, compare to index.
    /// Classifies each paragraph as: off-topic, musing, novel, or covered.
    let novelty (index: DocIndex) (embeddingUrl: string) (text: string) (threshold: float) =
        let paragraphs =
            text.Split([| "\n\n"; "\r\n\r\n" |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun p -> p.Trim())
            |> Array.filter (fun p -> p.Length > 30 && not (p.StartsWith("```")) && not (p.StartsWith("|")))

        if paragraphs.Length = 0 then [||]
        else
            let prefixed = paragraphs |> Array.map (fun p -> sprintf "search_query: %s" (p.Substring(0, min 200 p.Length)))
            let embeddings =
                match EmbeddingService.embed embeddingUrl prefixed |> Async.AwaitTask |> Async.RunSynchronously with
                | Some embs -> embs
                | None -> [||]

            if embeddings.Length = 0 then [||]
            else
                paragraphs |> Array.mapi (fun i para ->
                    if i >= embeddings.Length || embeddings.[i].Length = 0 then
                        mdict [ "paragraph", box (para.Substring(0, min 80 para.Length) + "..."); "status", box "error"; "score", box 0.0 ]
                    else
                        let hits = IndexStore.search index embeddings.[i] 1
                        let bestScore, bestChunk =
                            if hits.Length > 0 then
                                let idx, sim = hits.[0]
                                float sim, Some index.Chunks.[idx]
                            else 0.0, None

                        let kSignal = knowledgeSignal para index
                        let status =
                            if bestScore < 0.5 then "off-topic"       // not in the project's semantic space
                            elif kSignal < 0 then "musing"            // in-space but reads like discussion, not knowledge
                            elif kSignal < 1 && bestScore < 0.6 then "musing" // weak signal + low relevance = not knowledge
                            elif bestScore >= threshold then "covered" // already captured
                            else "novel"                              // relevant, looks like knowledge, not yet captured

                        let preview = if para.Length > 120 then para.Substring(0, 120) + "..." else para
                        let nearDoc = bestChunk |> Option.map (fun c -> Path.GetFileName c.FilePath) |> Option.defaultValue ""
                        let nearHeading = bestChunk |> Option.map (fun c -> c.Heading) |> Option.defaultValue ""
                        mdict [ "paragraph", box preview; "status", box status
                                "score", box (Math.Round(bestScore, 3)); "signal", box kSignal
                                "nearDoc", box nearDoc; "nearSection", box nearHeading ])

    // ── cluster — suggest subfolder groupings for an overcrowded directory ──

    /// Cosine similarity between two vectors.
    let private cosine (a: float32[]) (b: float32[]) =
        if a.Length = 0 || b.Length = 0 then 0.0f
        else
            let mutable dot = 0.0f
            let mutable na = 0.0f
            let mutable nb = 0.0f
            for i in 0 .. a.Length - 1 do
                dot <- dot + a.[i] * b.[i]
                na <- na + a.[i] * a.[i]
                nb <- nb + b.[i] * b.[i]
            if na = 0.0f || nb = 0.0f then 0.0f
            else dot / (sqrt na * sqrt nb)

    /// Simple greedy clustering: assign each doc to the nearest existing cluster center,
    /// or start a new cluster if similarity to all centers is below threshold.
    let private greedyCluster (items: (string * float32[])[]) (threshold: float) =
        let clusters = ResizeArray<ResizeArray<string> * float32[]>()
        for (name, emb) in items do
            let mutable bestIdx = -1
            let mutable bestSim = 0.0f
            for ci in 0 .. clusters.Count - 1 do
                let _, center = clusters.[ci]
                let sim = cosine emb center
                if sim > bestSim then bestSim <- sim; bestIdx <- ci
            if float bestSim >= threshold && bestIdx >= 0 then
                let members, _ = clusters.[bestIdx]
                members.Add(name)
            else
                let members = ResizeArray<string>()
                members.Add(name)
                clusters.Add((members, emb))
        clusters |> Seq.map (fun (members, _) -> members.ToArray()) |> Seq.toArray

    /// Suggest subfolder groupings for docs in a directory.
    /// Uses embeddings to cluster docs by semantic similarity.
    let cluster (index: DocIndex) (dir: string) (threshold: float) =
        let normDir = dir.Replace("\\", "/").TrimEnd('/')
        // Find docs in the target directory
        let docsInDir =
            index.Chunks
            |> Array.filter (fun c -> c.Level <= 1) // top-level sections only (one per doc)
            |> Array.filter (fun c ->
                let rel = c.FilePath.Replace("\\", "/")
                // Match docs directly in the target dir (not in subdirs)
                if normDir = "" || normDir = "." then
                    not (Path.GetFileName(rel) <> rel) // root-level only
                else
                    // Check if file is in this dir (works with both absolute and relative paths)
                    let dirWithSlash = normDir + "/"
                    let inDir = rel.StartsWith(dirWithSlash) || rel.Contains("/" + dirWithSlash)
                    if not inDir then false
                    else
                        // Only direct children, not in subdirs
                        let startIdx =
                            let i = rel.IndexOf(dirWithSlash)
                            if i >= 0 then i + dirWithSlash.Length else dirWithSlash.Length
                        let afterDir = rel.Substring(startIdx)
                        not (afterDir.Contains("/")))
            |> Array.distinctBy (fun c -> c.FilePath)

        if docsInDir.Length < 4 then
            // Not enough docs to warrant splitting
            [| mdict [ "suggestion", box "Folder has fewer than 4 docs — no split needed."; "docs", box docsInDir.Length ] |]
        else
            // Get embeddings for these docs
            let docEmbeddings =
                docsInDir |> Array.choose (fun c ->
                    let idx = index.Chunks |> Array.tryFindIndex (fun ch -> ch.FilePath = c.FilePath && ch.Heading = c.Heading)
                    match idx with
                    | Some i when i < index.Embeddings.Length && index.Embeddings.[i].Length > 0 ->
                        Some (Path.GetFileName c.FilePath, index.Embeddings.[i])
                    | _ -> None)

            if docEmbeddings.Length < 4 then
                [| mdict [ "suggestion", box "Not enough embedded docs to cluster."; "docs", box docEmbeddings.Length ] |]
            else
                let clusters = greedyCluster docEmbeddings threshold
                // Find common terms in each cluster for suggested folder names
                clusters
                |> Array.mapi (fun i members ->
                    let nameHint =
                        if members.Length = 1 then members.[0].Replace(".md", "")
                        else
                            // Find common prefix or common word
                            let words =
                                members
                                |> Array.collect (fun m -> m.Replace(".md", "").Replace("-", " ").Split(' '))
                                |> Array.countBy id
                                |> Array.sortByDescending snd
                                |> Array.truncate 2
                                |> Array.map fst
                            if words.Length > 0 then words |> String.concat "-"
                            else sprintf "group-%d" (i + 1)
                    mdict [ "suggestedFolder", box nameHint
                            "docs", box members.Length
                            "files", box (members |> String.concat ", ") ])
