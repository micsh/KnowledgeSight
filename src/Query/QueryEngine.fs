namespace AITeam.KnowledgeSight

open System
open System.Collections.Generic
open System.IO
open Jint

/// Jint-based query engine. Wires all primitives, evaluates JS, formats results.
module QueryEngine =

    let private sourceKey = "__ks_source__"

    /// Extract a string property from a JS options object, handling JsObject / ObjectInstance / JsValue.
    let private jsStr (opts: obj) (key: string) (def: string) =
        match opts with
        | :? Jint.Native.JsValue as v when v.IsObject() ->
            let o = v.AsObject()
            let prop = o.Get(key)
            if prop.IsUndefined() || prop.IsNull() then def else prop.AsString()
        | :? System.Dynamic.ExpandoObject as eo ->
            let dict = eo :> IDictionary<string, obj>
            match dict.TryGetValue(key) with
            | true, v when not (isNull v) -> string v
            | _ -> def
        | _ -> def

    /// Extract an int property from a JS options object.
    let private jsInt (opts: obj) (key: string) (def: int) =
        match opts with
        | :? Jint.Native.JsValue as v when v.IsObject() ->
            let o = v.AsObject()
            let prop = o.Get(key)
            if prop.IsUndefined() || prop.IsNull() then def else int (prop.AsNumber())
        | :? System.Dynamic.ExpandoObject as eo ->
            let dict = eo :> IDictionary<string, obj>
            match dict.TryGetValue(key) with
            | true, v when not (isNull v) -> int (System.Convert.ToDouble(v))
            | _ -> def
        | _ -> def

    /// Extract a float property from a JS options object.
    let private jsFloat (opts: obj) (key: string) (def: float) =
        match opts with
        | :? Jint.Native.JsValue as v when v.IsObject() ->
            let o = v.AsObject()
            let prop = o.Get(key)
            if prop.IsUndefined() || prop.IsNull() then def else prop.AsNumber()
        | :? System.Dynamic.ExpandoObject as eo ->
            let dict = eo :> IDictionary<string, obj>
            match dict.TryGetValue(key) with
            | true, v when not (isNull v) -> System.Convert.ToDouble(v)
            | _ -> def
        | _ -> def

    /// Stamp each result item with its source primitive name for format disambiguation.
    let private stamp (source: string) (results: Dictionary<string, obj>[]) =
        for d in results do d.[sourceKey] <- box source
        results

    let private stamp1 (source: string) (result: Dictionary<string, obj>) =
        result.[sourceKey] <- box source
        result

    let create (index: DocIndex) (chunks: DocChunk[] option) (embeddingUrl: string) (indexDir: string) (repoRoot: string) =
        let session = QuerySession(indexDir)
        let engine = new Engine()

        // catalog
        engine.SetValue("catalog", Func<obj>(fun () -> box (stamp "catalog" (Primitives.catalog index)))) |> ignore

        // search
        engine.SetValue("search", Func<string, obj, obj>(fun query opts ->
            let limit = jsInt opts "limit" 5
            let tag = jsStr opts "tag" ""
            let file = jsStr opts "file" ""
            box (stamp "search" (Primitives.search index session chunks embeddingUrl query limit tag file)))) |> ignore

        // context
        engine.SetValue("context", Func<string, obj>(fun f -> box (stamp1 "context" (Primitives.context index session f)))) |> ignore

        // expand
        engine.SetValue("expand", Func<string, obj>(fun id -> box (stamp1 "expand" (Primitives.expand index session chunks id)))) |> ignore

        // neighborhood
        engine.SetValue("neighborhood", Func<string, obj, obj>(fun id opts ->
            let before = jsInt opts "before" 3
            let after = jsInt opts "after" 3
            box (stamp1 "neighborhood" (Primitives.neighborhood index session chunks id before after)))) |> ignore

        // similar
        engine.SetValue("similar", Func<string, obj, obj>(fun id opts ->
            let limit = jsInt opts "limit" 5
            box (stamp "similar" (Primitives.similar index session id limit)))) |> ignore

        // grep
        engine.SetValue("grep", Func<string, obj, obj>(fun pattern opts ->
            let limit = jsInt opts "limit" 10
            let file = jsStr opts "file" ""
            box (stamp "grep" (Primitives.grep index session chunks pattern limit file)))) |> ignore

        // mentions
        engine.SetValue("mentions", Func<string, obj, obj>(fun term opts ->
            let limit = jsInt opts "limit" 20
            box (stamp "mentions" (Primitives.mentions index session chunks term limit)))) |> ignore

        // files
        engine.SetValue("files", Func<string, obj>(fun p -> box (stamp "files" (Primitives.files index (if isNull p then "" else p))))) |> ignore

        // backlinks
        engine.SetValue("backlinks", Func<string, obj>(fun f -> box (stamp "backlinks" (Primitives.backlinks index session f)))) |> ignore

        // links
        engine.SetValue("links", Func<string, obj>(fun f -> box (stamp "links" (Primitives.links index f)))) |> ignore

        // orphans
        engine.SetValue("orphans", Func<obj>(fun () -> box (stamp "orphans" (Primitives.orphans index)))) |> ignore

        // broken
        engine.SetValue("broken", Func<obj>(fun () -> box (stamp "broken" (Primitives.broken index)))) |> ignore

        // placement
        engine.SetValue("placement", Func<string, obj, obj>(fun content opts ->
            let limit = jsInt opts "limit" 3
            box (stamp "placement" (Primitives.placement index embeddingUrl content limit)))) |> ignore

        // walk
        engine.SetValue("walk", Func<string, obj, obj>(fun file opts ->
            let depth = jsInt opts "depth" 2
            let direction = jsStr opts "direction" "out"
            box (stamp "walk" (Primitives.walk index session file depth direction)))) |> ignore

        // novelty
        engine.SetValue("novelty", Func<string, obj, obj>(fun text opts ->
            let threshold = jsFloat opts "threshold" 0.75
            box (stamp "novelty" (Primitives.novelty index embeddingUrl text threshold)))) |> ignore

        // cluster
        engine.SetValue("cluster", Func<string, obj, obj>(fun dir opts ->
            let threshold = jsFloat opts "threshold" 0.7
            box (stamp "cluster" (Primitives.cluster index dir threshold)))) |> ignore

        // gaps — use JsValue to avoid Jint's ToObject() conversion
        engine.SetValue("gaps", Func<Jint.Native.JsValue, obj>(fun opts ->
            let scope, minDocs, signal =
                if isNull (box opts) || opts.IsUndefined() || opts.IsNull() then "", 1, ""
                elif opts.IsString() then opts.AsString(), 1, ""
                elif opts.IsObject() then
                    let o = opts.AsObject()
                    let s = match o.Get("scope") with v when not (v.IsUndefined()) && not (v.IsNull()) -> v.AsString() | _ -> ""
                    let m = match o.Get("min_docs") with v when not (v.IsUndefined()) -> int (v.AsNumber()) | _ -> 1
                    let sig' = match o.Get("signal") with v when not (v.IsUndefined()) && not (v.IsNull()) -> v.AsString() | _ -> ""
                    s, m, sig'
                else "", 1, ""
            box (stamp "gaps" (Primitives.gaps index chunks scope minDocs signal)))) |> ignore

        // Composition helpers
        engine.SetValue("print", Action<obj>(fun v ->
            eprintfn "%s" (Format.formatValue v))) |> ignore

        engine.Execute("""
            function pipe(value) {
                var fns = Array.prototype.slice.call(arguments, 1);
                return fns.reduce(function(acc, fn) { return fn(acc); }, value);
            }
            function tap(value, fn) { fn(value); return value; }
            function mergeBy(key) {
                var arrays = Array.prototype.slice.call(arguments, 1);
                var seen = {};
                var result = [];
                for (var i = 0; i < arrays.length; i++) {
                    var arr = arrays[i];
                    if (!arr) continue;
                    for (var j = 0; j < arr.length; j++) {
                        var item = arr[j];
                        var k = item[key];
                        if (k !== undefined && !seen[k]) { seen[k] = true; result.push(item); }
                        else if (k === undefined) { result.push(item); }
                    }
                }
                return result;
            }
        """) |> ignore

        // Load user-defined functions
        let userFns = FunctionStore.load repoRoot
        let fnDecls = FunctionStore.toJsDeclarations userFns
        for decl in fnDecls do
            try engine.Execute(decl) |> ignore
            with ex -> eprintfn "Warning: failed to load function: %s" ex.Message

        engine

    /// Wrap JS in an IIFE for evaluation.
    let private wrapIIFE (js: string) =
        let stripped = js.Split('\n') |> Array.map (fun line ->
            let commentIdx = line.IndexOf("//")
            if commentIdx >= 0 then line.Substring(0, commentIdx) else line) |> String.concat "\n"
        let trimmed = stripped.Trim()
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

    /// Evaluate JS with IIFE wrapping — human-readable formatted output.
    let eval (engine: Engine) (js: string) : string =
        try
            let toEval = wrapIIFE js
            engine.SetValue("__result__", engine.Evaluate(toEval)) |> ignore
            let jsonResult = engine.Evaluate("typeof __result__ === 'string' ? __result__ : JSON.stringify(__result__, null, 2)")
            let text = jsonResult.AsString()

            if text.StartsWith("[R") || text.StartsWith("──") then text
            else
                let native = engine.Evaluate("__result__").ToObject()
                try Format.formatValue native
                with _ -> text
        with ex -> sprintf "Error: %s" ex.Message

    /// Evaluate JS with IIFE wrapping — raw JSON output for machine consumption.
    let evalJson (engine: Engine) (js: string) : string =
        try
            let toEval = wrapIIFE js
            engine.SetValue("__result__", engine.Evaluate(toEval)) |> ignore
            let jsonResult = engine.Evaluate("typeof __result__ === 'string' ? __result__ : JSON.stringify(__result__, null, 2)")
            jsonResult.AsString()
        with ex ->
            let escaped = ex.Message.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\n", " ")
            sprintf """{"error":"%s"}""" escaped
