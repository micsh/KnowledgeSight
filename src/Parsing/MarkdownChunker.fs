namespace AITeam.KnowledgeSight

open System
open System.IO
open System.Text.RegularExpressions

/// Pure F# markdown chunker — no external dependencies.
/// Splits by headings, extracts frontmatter, extracts links.
module MarkdownChunker =

    let private headingRegex = Regex(@"^(#{1,6})\s+(.+)$", RegexOptions.Compiled)
    let private linkRegex = Regex(@"\[([^\]]*)\]\(([^)]+)\)", RegexOptions.Compiled)
    let private wikiLinkRegex = Regex(@"\[\[([^\]]+)\]\]", RegexOptions.Compiled)

    /// Extract YAML frontmatter from --- ... --- block.
    let parseFrontmatter (lines: string[]) : Frontmatter option * int =
        if lines.Length < 2 || lines.[0].Trim() <> "---" then None, 0
        else
            let endIdx = lines |> Array.skip 1 |> Array.tryFindIndex (fun l -> l.Trim() = "---")
            match endIdx with
            | None -> None, 0
            | Some i ->
                let fmLines = lines.[1 .. i]
                let mutable id = ""
                let mutable title = ""
                let mutable status = ""
                let tags = ResizeArray<string>()
                let related = ResizeArray<string>()
                let extra = System.Collections.Generic.Dictionary<string, string>()

                for line in fmLines do
                    let colonIdx = line.IndexOf(':')
                    if colonIdx > 0 then
                        let key = line.Substring(0, colonIdx).Trim().ToLowerInvariant()
                        let value = line.Substring(colonIdx + 1).Trim().Trim('"')
                        match key with
                        | "id" -> id <- value
                        | "title" -> title <- value
                        | "status" -> status <- value
                        | "tags" ->
                            value.Split([| ','; ';' |], StringSplitOptions.RemoveEmptyEntries)
                            |> Array.iter (fun t -> tags.Add(t.Trim()))
                        | "related" ->
                            value.Split([| ','; ';' |], StringSplitOptions.RemoveEmptyEntries)
                            |> Array.iter (fun r -> related.Add(r.Trim()))
                        | _ -> extra.[key] <- value

                let fm = {
                    Id = id; Title = title; Status = status
                    Tags = tags.ToArray(); Related = related.ToArray()
                    Extra = extra |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq
                }
                Some fm, i + 2  // skip past closing ---

    /// Extract all markdown and wiki-style links from text.
    let extractLinks (text: string) =
        let mdLinks =
            linkRegex.Matches(text)
            |> Seq.cast<Match>
            |> Seq.filter (fun m ->
                let target = m.Groups.[2].Value
                // Only keep relative links to .md files (not http, not images)
                not (target.StartsWith("http")) && not (target.StartsWith("#"))
                && (target.EndsWith(".md") || not (target.Contains("."))))
            |> Seq.map (fun m -> m.Groups.[1].Value, m.Groups.[2].Value)
            |> Seq.toArray
        let wikiLinks =
            wikiLinkRegex.Matches(text)
            |> Seq.cast<Match>
            |> Seq.map (fun m -> m.Groups.[1].Value, m.Groups.[1].Value)
            |> Seq.toArray
        Array.append mdLinks wikiLinks

    /// Extract a summary: first non-empty line that isn't a heading, link-only, or bullet.
    let extractSummary (content: string) (maxLen: int) =
        let lines = content.Split('\n')
        let candidate =
            lines |> Array.tryFind (fun l ->
                let t = l.Trim()
                t.Length > 10
                && not (t.StartsWith("#"))
                && not (t.StartsWith("-"))
                && not (t.StartsWith("*") && t.Length < 40)
                && not (t.StartsWith("|"))
                && not (t.StartsWith("```"))
                && not (t.StartsWith("!["))
                && not (Regex.IsMatch(t, @"^\[.+\]\(.+\)$")))  // not a link-only line
        match candidate with
        | Some line ->
            let trimmed = line.Trim()
            if trimmed.Length > maxLen then trimmed.Substring(0, maxLen) + "..."
            else trimmed
        | None -> "(no summary)"

    /// Chunk a single markdown file into sections.
    let chunkFile (filePath: string) : DocChunk[] * Frontmatter option * DocLink[] =
        let lines = File.ReadAllLines(filePath)
        if lines.Length = 0 then [||], None, [||]
        else

        let fm, contentStart = parseFrontmatter lines
        let contentLines = lines.[contentStart..]

        // Build heading-based sections
        let sections = ResizeArray<{| heading: string; level: int; startLine: int; lines: ResizeArray<string> |}>()
        let mutable currentHeading = fm |> Option.map (fun f -> if f.Title <> "" then f.Title else "(intro)") |> Option.defaultValue "(intro)"
        let mutable currentLevel = 0
        let mutable currentStart = contentStart + 1  // 1-based
        let mutable currentLines = ResizeArray<string>()

        for i in 0..contentLines.Length-1 do
            let line = contentLines.[i]
            let m = headingRegex.Match(line)
            if m.Success then
                // Flush current section
                if currentLines.Count > 0 || sections.Count = 0 then
                    sections.Add({| heading = currentHeading; level = currentLevel; startLine = currentStart; lines = currentLines |})
                currentHeading <- m.Groups.[2].Value.Trim()
                currentLevel <- m.Groups.[1].Value.Length
                currentStart <- contentStart + i + 1
                currentLines <- ResizeArray<string>()
            else
                currentLines.Add(line)

        // Flush final section
        sections.Add({| heading = currentHeading; level = currentLevel; startLine = currentStart; lines = currentLines |})

        // Build heading path stack
        let headingStack = System.Collections.Generic.Stack<string * int>()
        let buildHeadingPath (heading: string) (level: int) =
            while headingStack.Count > 0 && snd (headingStack.Peek()) >= level do
                headingStack.Pop() |> ignore
            headingStack.Push(heading, level)
            headingStack |> Seq.rev |> Seq.map fst |> String.concat " > "

        // Convert to DocChunks + collect links
        let allLinks = ResizeArray<DocLink>()
        let chunks =
            sections |> Seq.toArray |> Array.mapi (fun idx sec ->
                let content = sec.lines |> Seq.toArray |> String.concat "\n"
                let headingPath = buildHeadingPath sec.heading sec.level
                let links = extractLinks content
                let summary = extractSummary content 120
                let tags =
                    match fm with
                    | Some f -> f.Tags
                    | None -> [||]

                // Collect links for the link index
                for (text, target) in links do
                    allLinks.Add({
                        SourceFile = filePath; SourceHeading = sec.heading
                        TargetPath = target; TargetResolved = ""
                        LinkText = text; Line = sec.startLine
                    })

                let endLine = if idx + 1 < sections.Count then sections.[idx + 1].startLine - 1
                              else contentStart + contentLines.Length

                { FilePath = filePath; Heading = sec.heading; HeadingPath = headingPath
                  Level = sec.level; StartLine = sec.startLine; EndLine = endLine
                  Content = content; Summary = summary; Tags = tags
                  OutLinks = links |> Array.map snd })

        chunks, fm, allLinks.ToArray()

    /// Resolve link targets to absolute paths.
    let resolveLinks (repoRoot: string) (links: DocLink[]) =
        links |> Array.map (fun link ->
            let sourceDir = Path.GetDirectoryName(link.SourceFile)
            let resolved =
                // Try relative to source file
                let rel = Path.Combine(sourceDir, link.TargetPath)
                if File.Exists rel then Path.GetFullPath(rel)
                // Try relative to repo root
                elif File.Exists(Path.Combine(repoRoot, link.TargetPath)) then
                    Path.GetFullPath(Path.Combine(repoRoot, link.TargetPath))
                // Try with .md extension
                elif File.Exists(rel + ".md") then Path.GetFullPath(rel + ".md")
                elif File.Exists(Path.Combine(repoRoot, link.TargetPath + ".md")) then
                    Path.GetFullPath(Path.Combine(repoRoot, link.TargetPath + ".md"))
                else ""
            { link with TargetResolved = resolved })

    /// Chunk all markdown files.
    let chunkAll (files: string[]) (repoRoot: string) =
        let allChunks = ResizeArray<DocChunk>()
        let allLinks = ResizeArray<DocLink>()
        let allFm = System.Collections.Generic.Dictionary<string, Frontmatter>()

        for file in files do
            try
                let chunks, fm, links = chunkFile file
                allChunks.AddRange(chunks)
                allLinks.AddRange(links)
                match fm with Some f -> allFm.[file] <- f | None -> ()
            with ex ->
                eprintfn "  Warning: failed to parse %s: %s" (Path.GetFileName file) ex.Message

        let resolvedLinks = resolveLinks repoRoot (allLinks.ToArray())

        allChunks.ToArray(),
        resolvedLinks,
        allFm |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq
