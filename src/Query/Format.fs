namespace AITeam.KnowledgeSight

open System.Collections.Generic
open System.Text.Json

/// Result formatting for LLM and CLI consumption.
module Format =

    let rec formatValue (v: obj) : string =
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
                let score = match d.TryGetValue("score") with true, v -> sprintf "%.2f  " (unbox<float> v) | _ -> ""
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
                        lines.Add(sprintf "%s: %s" kv.Key (formatValue kv.Value))
            lines |> String.concat "\n"

        | :? (obj[]) as arr when arr.Length > 0 && (arr.[0] :? IDictionary<string, obj>) ->
            arr |> Array.map (fun item ->
                let d = item :?> IDictionary<string, obj>
                // Catalog format (like modules in code-sight)
                match d.TryGetValue("directory") with
                | true, dir ->
                    let docs = match d.TryGetValue("docs") with true, v -> string v | _ -> "?"
                    let sections = match d.TryGetValue("sections") with true, v -> string v | _ -> "?"
                    let tags = match d.TryGetValue("topTags") with true, v when string v <> "" -> sprintf "\n    Tags: %s" (string v) | _ -> ""
                    let titles = match d.TryGetValue("titles") with true, v when string v <> "" -> sprintf "\n    %s" (string v) | _ -> ""
                    sprintf "%-30s %s docs, %s sections%s%s" (string dir) docs sections tags titles
                | _ ->
                let id = match d.TryGetValue("id") with true, v -> string v | _ -> ""
                let heading = match d.TryGetValue("heading") with true, v -> string v | _ -> ""
                let file = match d.TryGetValue("file") with true, v -> string v | _ -> ""
                let line = match d.TryGetValue("line") with true, v -> string v | _ -> ""
                let score = match d.TryGetValue("score") with true, v -> sprintf "%.2f  " (unbox<float> v) | _ -> ""
                let summary = match d.TryGetValue("summary") with true, v -> string v | _ -> ""
                let matchLine = match d.TryGetValue("matchLine") with true, v when string v <> "" -> sprintf "\n       ▸ %s" (string v) | _ -> ""
                let tags = match d.TryGetValue("tags") with true, v when string v <> "" -> sprintf " [%s]" (string v) | _ -> ""
                if id <> "" then sprintf "[%s] %s%s (%s:%s)\n       %s%s%s" id score heading file line summary matchLine tags
                elif file <> "" then sprintf "%s (score: %s) %s" file score heading
                else
                    d |> Seq.map (fun kv -> sprintf "%s=%s" kv.Key (formatValue kv.Value)) |> String.concat ", " |> sprintf "{%s}")
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
