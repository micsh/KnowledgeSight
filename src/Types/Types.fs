namespace AITeam.KnowledgeSight

open System.Collections.Generic

/// A section chunk extracted from a markdown document.
type DocChunk = {
    FilePath: string
    Heading: string       // section heading text (or "(intro)" for pre-heading content)
    HeadingPath: string   // full chain: "Parent > Child > Subsection"
    Level: int            // 0 = intro/frontmatter, 1 = h1, 2 = h2, etc.
    StartLine: int
    EndLine: int
    Content: string
    Summary: string       // first meaningful sentence or LLM-generated
    Tags: string[]        // from frontmatter or inferred
    OutLinks: string[]    // outgoing markdown links from this section
}

/// A link between documents.
type DocLink = {
    SourceFile: string
    SourceHeading: string
    TargetPath: string    // raw link target (may be relative)
    TargetResolved: string // resolved absolute path (empty if broken)
    LinkText: string
    Line: int
}

/// Frontmatter metadata.
type Frontmatter = {
    Id: string
    Title: string
    Status: string
    Tags: string[]
    Related: string[]
    Extra: Map<string, string>
}

/// Index entry (persisted, without full content).
type ChunkEntry = {
    FilePath: string
    Heading: string
    HeadingPath: string
    Level: int
    StartLine: int
    EndLine: int
    Summary: string
    Tags: string
    LinkCount: int
    WordCount: int
}

/// The full in-memory index.
type DocIndex = {
    Chunks: ChunkEntry[]
    Embeddings: float32[][]
    Links: DocLink[]
    Frontmatters: Map<string, Frontmatter>
    EmbeddingDim: int
}

/// Mutable dictionary builder — Jint needs writable dictionaries.
[<AutoOpen>]
module DictHelper =
    let mdict (pairs: (string * obj) list) =
        let d = Dictionary<string, obj>()
        for (k, v) in pairs do d.[k] <- v
        d
