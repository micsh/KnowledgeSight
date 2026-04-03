namespace AITeam.KnowledgeSight

open System
open System.IO
open System.Text.Json

type KnowledgeSightConfig = {
    RepoRoot: string
    DocDirs: string[]
    Exclude: string[]
    IndexDir: string
    EmbeddingUrl: string
    EmbeddingBatchSize: int
}

module Config =

    let private defaultExclude = [| "node_modules"; "bin"; "obj"; ".git"; "wwwroot"; "dist"; ".code-intel" |]

    /// Auto-detect directories containing markdown files.
    let private detectDocDirs (repoRoot: string) =
        // Check common doc locations
        let candidates = [| ".agents"; "docs"; "doc"; "wiki"; "knowledge"; "." |]
        let found = candidates |> Array.filter (fun d ->
            let dir = Path.Combine(repoRoot, d)
            Directory.Exists(dir) &&
            Directory.EnumerateFiles(dir, "*.md", SearchOption.AllDirectories) |> Seq.truncate 1 |> Seq.length > 0)
        if found.Length > 0 then found
        else [| "." |]

    let load (repoRoot: string) =
        let configPath = Path.Combine(repoRoot, "knowledge-sight.json")
        let indexDir = Path.Combine(repoRoot, ".knowledge-sight")

        if File.Exists configPath then
            let json = File.ReadAllText(configPath)
            let doc = JsonDocument.Parse(json)
            let root = doc.RootElement
            let str (p: string) d = match root.TryGetProperty(p) with true, v -> v.GetString() | _ -> d
            let int' (p: string) d = match root.TryGetProperty(p) with true, v -> v.GetInt32() | _ -> d
            let strArr (p: string) d =
                match root.TryGetProperty(p) with
                | true, v -> v.EnumerateArray() |> Seq.map (fun x -> x.GetString()) |> Seq.toArray
                | _ -> d
            {
                RepoRoot = repoRoot
                DocDirs = strArr "docDirs" (detectDocDirs repoRoot)
                Exclude = strArr "exclude" defaultExclude
                IndexDir = str "indexDir" indexDir
                EmbeddingUrl = str "embeddingUrl" "http://localhost:1234/v1/embeddings"
                EmbeddingBatchSize = int' "embeddingBatchSize" 50
            }
        else
            {
                RepoRoot = repoRoot
                DocDirs = detectDocDirs repoRoot
                Exclude = defaultExclude
                IndexDir = indexDir
                EmbeddingUrl = "http://localhost:1234/v1/embeddings"
                EmbeddingBatchSize = 50
            }

    /// Find all .md files under the configured doc dirs.
    let findDocFiles (cfg: KnowledgeSightConfig) =
        cfg.DocDirs
        |> Array.collect (fun dir ->
            let absDir = Path.Combine(cfg.RepoRoot, dir)
            if Directory.Exists absDir then
                Directory.EnumerateFiles(absDir, "*.md", SearchOption.AllDirectories)
                |> Seq.filter (fun f ->
                    let rel = Path.GetRelativePath(cfg.RepoRoot, f).Replace("\\", "/")
                    cfg.Exclude |> Array.forall (fun ex -> not (rel.Contains(ex))))
                |> Seq.toArray
            else [||])
        |> Array.distinct
        |> Array.sort
