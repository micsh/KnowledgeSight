namespace AITeam.KnowledgeSight

open System
open System.Collections.Generic
open System.IO
open Jint

/// Jint-based query engine. Wires all primitives, evaluates JS, formats results.
module QueryEngine =

    let create (index: DocIndex) (chunks: DocChunk[] option) (embeddingUrl: string) (indexDir: string) =
        let session = QuerySession(indexDir)
        let engine = new Engine()

        // catalog
        engine.SetValue("catalog", Func<obj>(fun () -> box (Primitives.catalog index))) |> ignore

        // search
        engine.SetValue("search", Func<string, obj, obj>(fun query opts ->
            let limit, tag, file =
                match opts with
                | :? Jint.Native.JsObject as o ->
                    let l = match o.Get("limit") with v when not (v.IsUndefined()) -> int (v.AsNumber()) | _ -> 5
                    let t = match o.Get("tag") with v when not (v.IsUndefined()) && not (v.IsNull()) -> v.AsString() | _ -> ""
                    let f = match o.Get("file") with v when not (v.IsUndefined()) && not (v.IsNull()) -> v.AsString() | _ -> ""
                    l, t, f
                | _ -> 5, "", ""
            box (Primitives.search index session chunks embeddingUrl query limit tag file))) |> ignore

        // context
        engine.SetValue("context", Func<string, obj>(fun f -> box (Primitives.context index session f))) |> ignore

        // expand
        engine.SetValue("expand", Func<string, obj>(fun id -> box (Primitives.expand index session chunks id))) |> ignore

        // neighborhood
        engine.SetValue("neighborhood", Func<string, obj, obj>(fun id opts ->
            let before, after =
                match opts with
                | :? Jint.Native.JsObject as o ->
                    let b = match o.Get("before") with v when not (v.IsUndefined()) -> int (v.AsNumber()) | _ -> 3
                    let a = match o.Get("after") with v when not (v.IsUndefined()) -> int (v.AsNumber()) | _ -> 3
                    b, a
                | _ -> 3, 3
            box (Primitives.neighborhood index session chunks id before after))) |> ignore

        // similar
        engine.SetValue("similar", Func<string, obj, obj>(fun id opts ->
            let limit =
                match opts with
                | :? Jint.Native.JsObject as o ->
                    match o.Get("limit") with v when not (v.IsUndefined()) -> int (v.AsNumber()) | _ -> 5
                | _ -> 5
            box (Primitives.similar index session id limit))) |> ignore

        // grep
        engine.SetValue("grep", Func<string, obj, obj>(fun pattern opts ->
            let limit, file =
                match opts with
                | :? Jint.Native.JsObject as o ->
                    let l = match o.Get("limit") with v when not (v.IsUndefined()) -> int (v.AsNumber()) | _ -> 10
                    let f = match o.Get("file") with v when not (v.IsUndefined()) && not (v.IsNull()) -> v.AsString() | _ -> ""
                    l, f
                | _ -> 10, ""
            box (Primitives.grep index session chunks pattern limit file))) |> ignore

        // mentions
        engine.SetValue("mentions", Func<string, obj, obj>(fun term opts ->
            let limit =
                match opts with
                | :? Jint.Native.JsObject as o ->
                    match o.Get("limit") with v when not (v.IsUndefined()) -> int (v.AsNumber()) | _ -> 20
                | _ -> 20
            box (Primitives.mentions index session chunks term limit))) |> ignore

        // files
        engine.SetValue("files", Func<string, obj>(fun p -> box (Primitives.files index (if isNull p then "" else p)))) |> ignore

        // backlinks
        engine.SetValue("backlinks", Func<string, obj>(fun f -> box (Primitives.backlinks index session f))) |> ignore

        // links
        engine.SetValue("links", Func<string, obj>(fun f -> box (Primitives.links index f))) |> ignore

        // orphans
        engine.SetValue("orphans", Func<obj>(fun () -> box (Primitives.orphans index))) |> ignore

        // broken
        engine.SetValue("broken", Func<obj>(fun () -> box (Primitives.broken index))) |> ignore

        // placement
        engine.SetValue("placement", Func<string, obj, obj>(fun content opts ->
            let limit =
                match opts with
                | :? Jint.Native.JsObject as o ->
                    match o.Get("limit") with v when not (v.IsUndefined()) -> int (v.AsNumber()) | _ -> 3
                | _ -> 3
            box (Primitives.placement index embeddingUrl content limit))) |> ignore

        // walk
        engine.SetValue("walk", Func<string, obj, obj>(fun file opts ->
            let depth, direction =
                match opts with
                | :? Jint.Native.JsObject as o ->
                    let d = match o.Get("depth") with v when not (v.IsUndefined()) -> int (v.AsNumber()) | _ -> 2
                    let dir = match o.Get("direction") with v when not (v.IsUndefined()) && not (v.IsNull()) -> v.AsString() | _ -> "out"
                    d, dir
                | _ -> 2, "out"
            box (Primitives.walk index session file depth direction))) |> ignore

        // novelty
        engine.SetValue("novelty", Func<string, obj, obj>(fun text opts ->
            let threshold =
                match opts with
                | :? Jint.Native.JsObject as o ->
                    match o.Get("threshold") with v when not (v.IsUndefined()) -> v.AsNumber() | _ -> 0.75
                | _ -> 0.75
            box (Primitives.novelty index embeddingUrl text threshold))) |> ignore

        engine

    /// Evaluate JS with IIFE wrapping.
    let eval (engine: Engine) (js: string) : string =
        try
            // Strip JS single-line comments
            let stripped = js.Split('\n') |> Array.map (fun line ->
                let commentIdx = line.IndexOf("//")
                if commentIdx >= 0 then line.Substring(0, commentIdx) else line) |> String.concat "\n"
            let trimmed = stripped.Trim()
            let toEval =
                if trimmed.StartsWith("(") && trimmed.EndsWith(")") then trimmed
                else
                    let lines = trimmed.Split('\n') |> Array.map (fun s -> s.Trim()) |> Array.filter (fun s -> s <> "")
                    let joined = lines |> String.concat " "
                    let lastSemi = joined.LastIndexOf(';')
                    if lastSemi > 0 && lastSemi < joined.Length - 2 then
                        let stmts = joined.Substring(0, lastSemi + 1)
                        let expr = joined.Substring(lastSemi + 1).Trim()
                        if expr.Length > 0 then
                            sprintf "(function() { %s return %s; })()" stmts expr
                        else
                            sprintf "(function() { return %s; })()" joined
                    elif joined.StartsWith("let ") || joined.StartsWith("const ") || joined.StartsWith("var ") then
                        sprintf "(function() { %s })()" joined
                    else
                        sprintf "(function() { return %s; })()" joined

            engine.SetValue("__result__", engine.Evaluate(toEval)) |> ignore
            let jsonResult = engine.Evaluate("typeof __result__ === 'string' ? __result__ : JSON.stringify(__result__, null, 2)")
            let text = jsonResult.AsString()

            if text.StartsWith("[R") || text.StartsWith("──") then text
            else
                let native = engine.Evaluate("__result__").ToObject()
                try Format.formatValue native
                with _ -> text
        with ex -> sprintf "Error: %s" ex.Message
