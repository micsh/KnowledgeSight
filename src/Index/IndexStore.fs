namespace AITeam.KnowledgeSight

open System
open System.IO
open System.Numerics.Tensors

/// Index persistence and query operations.
module IndexStore =

    // ── Source chunk cache ──

    let saveSourceChunks (dir: string) (chunks: DocChunk[]) =
        let path = Path.Combine(dir, "source-chunks.jsonl")
        use writer = new StreamWriter(path)
        for c in chunks do
            let json = System.Text.Json.JsonSerializer.Serialize({|
                filePath = c.FilePath; heading = c.Heading; headingPath = c.HeadingPath
                level = c.Level; startLine = c.StartLine; endLine = c.EndLine
                content = c.Content; summary = c.Summary
                tags = c.Tags; outLinks = c.OutLinks |})
            writer.WriteLine(json)
        eprintfn "  Cached %d source chunks → source-chunks.jsonl" chunks.Length

    let loadSourceChunks (dir: string) : DocChunk[] option =
        let path = Path.Combine(dir, "source-chunks.jsonl")
        if not (File.Exists path) then None
        else
            try
                let chunks =
                    File.ReadAllLines(path)
                    |> Array.choose (fun line ->
                        try
                            let doc = System.Text.Json.JsonDocument.Parse(line)
                            let r = doc.RootElement
                            let str (p: string) = match r.TryGetProperty(p) with true, v -> v.GetString() | _ -> ""
                            let int' (p: string) = match r.TryGetProperty(p) with true, v -> v.GetInt32() | _ -> 0
                            let strArr (p: string) =
                                match r.TryGetProperty(p) with
                                | true, v when v.ValueKind = System.Text.Json.JsonValueKind.Array ->
                                    v.EnumerateArray() |> Seq.map (fun x -> x.GetString()) |> Seq.toArray
                                | _ -> [||]
                            Some { FilePath = str "filePath"; Heading = str "heading"; HeadingPath = str "headingPath"
                                   Level = int' "level"; StartLine = int' "startLine"; EndLine = int' "endLine"
                                   Content = str "content"; Summary = str "summary"
                                   Tags = strArr "tags"; OutLinks = strArr "outLinks" }
                        with _ -> None)
                Some chunks
            with _ -> None

    // ── Persistence ──

    let private escape (s: string) =
        s.Replace("\t", " ").Replace("\n", " ").Replace("\r", "")

    let private writeEmbeddings (path: string) (embeddings: float32[][]) =
        use fs = File.Create(path)
        use bw = new BinaryWriter(fs)
        bw.Write(embeddings.Length)
        if embeddings.Length > 0 then
            bw.Write(embeddings.[0].Length)
            for emb in embeddings do
                for v in emb do bw.Write(v)

    let private readEmbeddings (path: string) =
        if not (File.Exists path) then None
        else
            use fs = File.OpenRead(path)
            use br = new BinaryReader(fs)
            let count = br.ReadInt32()
            if count = 0 then Some [||]
            else
                let dim = br.ReadInt32()
                Some (Array.init count (fun _ -> Array.init dim (fun _ -> br.ReadSingle())))

    let save (dir: string) (index: DocIndex) =
        Directory.CreateDirectory(dir) |> ignore

        let header = "#fields:FilePath\tHeading\tHeadingPath\tLevel\tStartLine\tEndLine\tSummary\tTags\tLinkCount\tWordCount"
        let chunkLines =
            index.Chunks |> Array.map (fun c ->
                sprintf "%s\t%s\t%s\t%d\t%d\t%d\t%s\t%s\t%d\t%d"
                    (escape c.FilePath) (escape c.Heading) (escape c.HeadingPath)
                    c.Level c.StartLine c.EndLine (escape c.Summary)
                    (escape c.Tags) c.LinkCount c.WordCount)
        File.WriteAllLines(Path.Combine(dir, "chunks.tsv"), Array.append [| header |] chunkLines)

        writeEmbeddings (Path.Combine(dir, "embeddings.emb")) index.Embeddings

        let linkLines =
            index.Links |> Array.map (fun l ->
                sprintf "%s\t%s\t%s\t%s\t%s\t%d"
                    (escape l.SourceFile) (escape l.SourceHeading)
                    (escape l.TargetPath) (escape l.TargetResolved)
                    (escape l.LinkText) l.Line)
        File.WriteAllLines(Path.Combine(dir, "links.tsv"), linkLines)

        // Save frontmatters
        let fmLines =
            index.Frontmatters |> Map.toArray |> Array.map (fun (file, fm) ->
                sprintf "%s\t%s\t%s\t%s\t%s\t%s"
                    (escape file) (escape fm.Id) (escape fm.Title) (escape fm.Status)
                    (fm.Tags |> String.concat ",") (fm.Related |> String.concat ","))
        File.WriteAllLines(Path.Combine(dir, "frontmatters.tsv"), fmLines)

        eprintfn "  Index saved: %d chunks, %d links, %d docs with frontmatter → %s"
            index.Chunks.Length index.Links.Length index.Frontmatters.Count dir

    let load (dir: string) : DocIndex option =
        let chunkFile = Path.Combine(dir, "chunks.tsv")
        let embFile = Path.Combine(dir, "embeddings.emb")
        if not (File.Exists chunkFile) || not (File.Exists embFile) then None
        else
            let allLines = File.ReadAllLines(chunkFile)
            let dataLines =
                if allLines.Length > 0 && allLines.[0].StartsWith("#fields:") then allLines.[1..]
                else allLines
            let chunks =
                dataLines |> Array.choose (fun line ->
                    let p = line.Split('\t')
                    if p.Length >= 10 then
                        Some { FilePath = p.[0]; Heading = p.[1]; HeadingPath = p.[2]
                               Level = int p.[3]; StartLine = int p.[4]; EndLine = int p.[5]
                               Summary = p.[6]; Tags = p.[7]; LinkCount = int p.[8]; WordCount = int p.[9] }
                    else None)
            let embeddings = readEmbeddings embFile |> Option.defaultValue [||]
            let links =
                let f = Path.Combine(dir, "links.tsv")
                if File.Exists f then
                    File.ReadAllLines(f) |> Array.choose (fun line ->
                        let p = line.Split('\t')
                        if p.Length >= 6 then
                            Some { SourceFile = p.[0]; SourceHeading = p.[1]
                                   TargetPath = p.[2]; TargetResolved = p.[3]
                                   LinkText = p.[4]; Line = int p.[5] }
                        else None)
                else [||]
            let frontmatters =
                let f = Path.Combine(dir, "frontmatters.tsv")
                if File.Exists f then
                    File.ReadAllLines(f) |> Array.choose (fun line ->
                        let p = line.Split('\t')
                        if p.Length >= 6 then
                            Some (p.[0], { Id = p.[1]; Title = p.[2]; Status = p.[3]
                                           Tags = p.[4].Split(',') |> Array.filter ((<>) "")
                                           Related = p.[5].Split(',') |> Array.filter ((<>) "")
                                           Extra = Map.empty })
                        else None)
                    |> Map.ofArray
                else Map.empty
            let dim = if embeddings.Length > 0 then embeddings.[0].Length else 0
            Some { Chunks = chunks; Embeddings = embeddings; Links = links
                   Frontmatters = frontmatters; EmbeddingDim = dim }

    // ── Query functions ──

    let search (index: DocIndex) (queryEmbedding: float32[]) (k: int) =
        if index.Embeddings.Length = 0 || queryEmbedding.Length = 0 then [||]
        else
            index.Embeddings
            |> Array.mapi (fun i emb ->
                if emb.Length = 0 || emb.Length <> queryEmbedding.Length then i, -1f
                else i, TensorPrimitives.CosineSimilarity(ReadOnlySpan(queryEmbedding), ReadOnlySpan(emb)))
            |> Array.sortByDescending snd
            |> Array.take (min k index.Embeddings.Length)

    let similar (index: DocIndex) (chunkIdx: int) (k: int) =
        if index.Embeddings.Length = 0 || chunkIdx >= index.Embeddings.Length then [||]
        else
            let target = index.Embeddings.[chunkIdx]
            if target.Length = 0 then [||]
            else
                index.Embeddings
                |> Array.mapi (fun i emb ->
                    if i = chunkIdx || emb.Length = 0 || emb.Length <> target.Length then i, -1f
                    else i, TensorPrimitives.CosineSimilarity(ReadOnlySpan(target), ReadOnlySpan(emb)))
                |> Array.sortByDescending snd
                |> Array.take (min k index.Embeddings.Length)

    /// Normalize file input: may be full path, relative, or just filename.
    let matchFile (filePath: string) (input: string) =
        let inputLower = input.Replace("\\", "/").ToLowerInvariant()
        let pathLower = filePath.Replace("\\", "/").ToLowerInvariant()
        let fileNameLower = Path.GetFileName(filePath).ToLowerInvariant()
        fileNameLower = inputLower
        || pathLower = inputLower
        || pathLower.EndsWith("/" + inputLower)
        || inputLower.EndsWith("/" + fileNameLower)

    let fileChunks (index: DocIndex) (fileName: string) =
        index.Chunks |> Array.indexed
        |> Array.filter (fun (_, c) -> matchFile c.FilePath fileName)

    let backlinks (index: DocIndex) (fileName: string) =
        index.Links |> Array.filter (fun l -> matchFile l.TargetResolved fileName || matchFile l.TargetPath fileName)

    let outlinks (index: DocIndex) (fileName: string) =
        index.Links |> Array.filter (fun l -> matchFile l.SourceFile fileName)
