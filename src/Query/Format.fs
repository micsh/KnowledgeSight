namespace AITeam.KnowledgeSight

open System.Collections.Generic
open System.Text.Json

/// Result formatting for LLM and CLI consumption.
module Format =

    let private fmtScore (d: IDictionary<string, obj>) =
        match d.TryGetValue("score") with
        | true, v ->
            match v with
            | :? float as f -> sprintf "%.2f  " f
            | _ -> try sprintf "%.2f  " (System.Convert.ToDouble(v)) with _ -> ""
        | _ -> ""

    let private sourceKey = "__ks_source__"

    let private getSource (d: IDictionary<string, obj>) =
        match d.TryGetValue(sourceKey) with true, v -> string v | _ -> ""

    /// Format a single dict item, using source annotation when available.
    let rec private formatItem (d: IDictionary<string, obj>) =
        match d.TryGetValue("error") with
        | true, err -> sprintf "Error: %O" err
        | _ ->
        let source = getSource d
        match source with
        | "catalog" ->
            let dir = match d.TryGetValue("directory") with true, v -> string v | _ -> "?"
            let docs = match d.TryGetValue("docs") with true, v -> string v | _ -> "?"
            let sections = match d.TryGetValue("sections") with true, v -> string v | _ -> "?"
            let tags = match d.TryGetValue("topTags") with true, v when string v <> "" -> sprintf "\n    Tags: %s" (string v) | _ -> ""
            let titles = match d.TryGetValue("titles") with true, v when string v <> "" -> sprintf "\n    %s" (string v) | _ -> ""
            sprintf "%-30s %s docs, %s sections%s%s" dir docs sections tags titles
        | "orphans" | "files" ->
            let file = match d.TryGetValue("file") with true, v -> string v | _ -> ""
            let title = match d.TryGetValue("title") with true, v when string v <> "" -> sprintf " — %s" (string v) | _ -> ""
            let sections = match d.TryGetValue("sections") with true, v -> sprintf " (%s sections)" (string v) | _ -> ""
            let path = match d.TryGetValue("path") with true, v when string v <> "" -> sprintf "  %s" (string v) | _ -> ""
            sprintf "%s%s%s%s" file title sections path
        | "broken" ->
            let from' = match d.TryGetValue("from") with true, v -> string v | _ -> ""
            let target = match d.TryGetValue("target") with true, v -> string v | _ -> ""
            let section = match d.TryGetValue("section") with true, v when string v <> "" -> sprintf " (in %s)" (string v) | _ -> ""
            let line = match d.TryGetValue("line") with true, v when string v <> "" -> sprintf " line %s" (string v) | _ -> ""
            sprintf "%s → %s%s%s" from' target section line
        | "backlinks" | "links" ->
            let from' = match d.TryGetValue("from") with true, v -> string v | _ -> ""
            let to' = match d.TryGetValue("to") with true, v -> string v | _ -> ""
            let target = if from' <> "" then from' else to'
            let section = match d.TryGetValue("section") with true, v when string v <> "" -> sprintf " (in %s)" (string v) | _ -> ""
            let text = match d.TryGetValue("text") with true, v when string v <> "" -> sprintf " \"%s\"" (string v) | _ -> ""
            sprintf "%s%s%s" target section text
        | "novelty" ->
            let para = match d.TryGetValue("paragraph") with true, v -> string v | _ -> ""
            let status = match d.TryGetValue("status") with true, v -> string v | _ -> ""
            let score = fmtScore d
            let closest = match d.TryGetValue("closestSection") with true, v when string v <> "" -> sprintf " → %s" (string v) | _ -> ""
            sprintf "[%s] %s%s%s" status score para closest
        | "cluster" ->
            let folder = match d.TryGetValue("suggestedFolder") with true, v -> string v | _ -> ""
            let docs = match d.TryGetValue("docs") with true, v -> string v | _ -> ""
            sprintf "%s  (%s)" folder docs
        | _ ->
            // Fallback: use shape-based detection
            let id = match d.TryGetValue("id") with true, v -> string v | _ -> ""
            let heading = match d.TryGetValue("heading") with true, v -> string v | _ -> ""
            let file = match d.TryGetValue("file") with true, v -> string v | _ -> ""
            let line = match d.TryGetValue("line") with true, v -> string v | _ -> ""
            let score = fmtScore d
            let summary = match d.TryGetValue("summary") with true, v -> string v | _ -> ""
            let content = match d.TryGetValue("content") with true, v -> string v | _ -> ""
            let matchLine = match d.TryGetValue("matchLine") with true, v when string v <> "" -> sprintf "\n       ▸ %s" (string v) | _ -> ""
            let tags = match d.TryGetValue("tags") with true, v when string v <> "" -> sprintf " [%s]" (string v) | _ -> ""
            if content <> "" then
                sprintf "[%s] %s (%s:%s)%s\n%s" id heading file line tags content
            elif id <> "" then
                sprintf "[%s] %s%s (%s:%s)\n       %s%s%s" id score heading file line summary matchLine tags
            else
                // No recognized shape — preserve all fields
                d |> Seq.filter (fun kv -> kv.Key <> sourceKey)
                |> Seq.map (fun kv -> sprintf "%s: %s" kv.Key (formatValue kv.Value))
                |> String.concat " | "

    and formatValue (v: obj) : string =
        match v with
        | :? (IDictionary<string, obj>) as d ->
            let lines = ResizeArray<string>()
            match d.TryGetValue("error") with
            | true, err -> lines.Add(sprintf "Error: %O" err)
            | _ ->
                let id = match d.TryGetValue("id") with true, v -> string v | _ -> ""
                let heading = match d.TryGetValue("heading") with true, v -> string v | _ -> ""
                let file = match d.TryGetValue("file") with true, v -> string v | _ -> ""
                let line = match d.TryGetValue("line") with true, v -> string v | _ -> ""
                let score = fmtScore d
                let summary = match d.TryGetValue("summary") with true, v -> string v | _ -> ""
                let content = match d.TryGetValue("content") with true, v -> string v | _ -> ""
                let matchLine = match d.TryGetValue("matchLine") with true, v when string v <> "" -> sprintf "\n       ▸ %s" (string v) | _ -> ""
                let tags = match d.TryGetValue("tags") with true, v when string v <> "" -> sprintf " [%s]" (string v) | _ -> ""

                if content <> "" then
                    lines.Add(sprintf "[%s] %s (%s:%s)%s" id heading file line tags)
                    lines.Add(content)
                elif id <> "" then
                    lines.Add(sprintf "[%s] %s%s (%s:%s)\n       %s%s%s" id score heading file line summary matchLine tags)
                else
                    for kv in d do
                        if kv.Key <> sourceKey then
                            lines.Add(sprintf "%s: %s" kv.Key (formatValue kv.Value))
            lines |> String.concat "\n"

        | :? (obj[]) as arr when arr.Length > 0 && (arr.[0] :? IDictionary<string, obj>) ->
            arr |> Array.map (fun item -> formatItem (item :?> IDictionary<string, obj>))
            |> String.concat "\n"

        | :? (obj[]) as arr when arr.Length > 0 && (arr.[0] :? string) -> arr |> Array.map string |> String.concat "\n"
        | :? (string[]) as arr -> arr |> String.concat "\n"
        | :? (obj[]) as arr when arr.Length = 0 -> "(no results)"
        | null -> "(no results)"
        | :? string as s -> s
        | :? int as i -> string i
        | :? float as f -> if f = System.Math.Floor(f) && abs f < 1e15 then sprintf "%.0f" f else sprintf "%.3f" f
        | :? bool as b -> if b then "true" else "false"
        | other ->
            try JsonSerializer.Serialize(other, JsonSerializerOptions(WriteIndented = true))
            with _ -> string other
