open System
open System.IO
open System.Threading.Tasks
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Text.Json

type Entry =
    | Start of nextwords:Set<string>
    | Word of nextwords:Set<string>
    | End

module Entry =
    let getNextWords = function
        | Start ws -> ws
        | Word ws -> ws
        | End -> Set.empty

module EntryParser =
    open Entry

    [<Literal>]
    let endWord = "."

    let (|WordToken|_|) str =
        if str <> endWord
        then Some str
        else None

    let (|EndToken|_|) str =
        if str = endWord
        then Some endWord
        else None

    let inline private createEntry' prevEntry words =
        match prevEntry with
        | Some (End)
            -> Start words
        | Some (Word _)
        | Some (Start _)
        | None
            -> Word words

    let private createEntry w nextw prevEntry dict =
        match Map.tryFind w dict with
        | Some e ->
            getNextWords e
            |> Set.union nextw
            |> createEntry' prevEntry
        | None ->
            nextw
            |> createEntry' prevEntry

    let private window1Parser tokens prevEntry dict =
        match tokens with
        // a b
        | (WordToken w)::nextw::tail ->
            Some (w, createEntry w (Set([nextw])) prevEntry dict, nextw::tail)
        // . a
        | (EndToken w)::nextw::tail ->
            Some (w, End, nextw::tail)
        // a $
        | (WordToken w)::tail ->
            Some (w, createEntry w Set.empty prevEntry dict, tail)
        // . $
        | (EndToken w)::tail ->
            Some (w, End, tail)
        | _ ->
            None

    let private window2Parser tokens prevEntry dict =
        match tokens with
        // a b c
        // a b .
        | (WordToken w1)::(WordToken w2)::nextw::tail ->
            let w = $"{w1} {w2}"
            Some (w, createEntry w (Set([nextw])) prevEntry dict, w2::nextw::tail)
        // a . b
        | (WordToken w1)::(EndToken w2)::nextw::tail ->
            Some (w1, End, w2::nextw::tail)
        // . b c
        | (EndToken w)::w2::nextw::tail ->
            Some (w, End, w2::nextw::tail)
        // a b $
        // $ - eol
        | (WordToken w1)::(WordToken w2)::tail ->
            let w = $"{w1} {w2}"
            Some (w, createEntry w Set.empty prevEntry dict, w2::tail)
        // a . $
        | (WordToken w1)::(EndToken w2)::tail ->
            Some (w1, End, w2::tail)
        // . b $
        | (EndToken w)::nextw::tail ->
            Some (w, End, nextw::tail)
        // a $
        | (WordToken w)::tail ->
            Some (w, createEntry w Set.empty prevEntry dict, tail)
        // . $
        | (EndToken w)::tail ->
            Some (w, End, tail)
        | _ ->
            None

    let private parseEntries windowParser (seed: Map<string, Entry>) (tokens: string list): Map<string, Entry> =
        let rec parse tokens prevEntry dict =
            match windowParser tokens prevEntry dict with
            | None ->
                dict
            | Some (w, entry, tokens) ->
                dict
                |> Map.add w entry
                |> parse tokens (Some entry)
        parse tokens None seed

    let parseEntries1 = parseEntries window1Parser
    let parseEntries2 = parseEntries window2Parser

    let parseTokens strs =
        strs
        |> Seq.filter (String.IsNullOrWhiteSpace >> not)
        |> Seq.map(fun x -> x.ToLowerInvariant())
        |> Seq.toList

open Entry
open EntryParser

let getEntriesFromTxt () =
    use f = File.OpenText("./corpus.txt")
    let mutable tokens = Map.empty
    while f.EndOfStream |> not do
        tokens <-
            Regex.Split(f.ReadLine(), "(\s|\.)", RegexOptions.CultureInvariant ||| RegexOptions.IgnoreCase)
            |> parseTokens
            |> parseEntries1 tokens
    tokens

let getEntriesFromJson () =
    use f = File.OpenText("./result.json")
    use doc = JsonDocument.Parse(f.ReadToEnd())
    let msgs = doc.RootElement.GetProperty("messages").EnumerateArray()
    let mutable tokens = []
    for message in msgs do
        let m = message.GetProperty("text")
        if m.ValueKind = JsonValueKind.String then
            tokens <-
                tokens @
                (Regex.Split($"{endWord}{m.GetString()}{endWord}", "(\s|\.)", RegexOptions.CultureInvariant ||| RegexOptions.IgnoreCase)
                |> parseTokens)
    tokens

let dict = getEntriesFromJson () |> parseEntries1 Map.empty
dict
|> Map.iter (fun k v -> printfn "%A: %A" k v)

let genPhrase (dict: Map<string, Entry>) : string list =
    let random = Random()
    let tryRandKey (keys: string[]) =
        if keys.Length > 0
        then Some (keys.[random.Next(0, keys.Length)])
        else None
    let inline findEntry w (dict: Map<string, Entry>) =
        let randomKey =
            dict
            |> Seq.filter (fun e -> e.Key = w || e.Key.Split(' ').[0] = w)
            |> Seq.map (fun e -> e.Key)
            |> Seq.toArray
            |> tryRandKey
        match randomKey with
        | Some key ->
            match Map.tryFind key dict with
            | Some entry -> Some entry
            | None -> None
        | None -> None
    let rec gen res startToken =
        match startToken with
        | EndToken _ ->
            res
        | _ ->
            match findEntry startToken dict with
            | Some entry ->
                let randToken =
                    getNextWords entry
                    |> Seq.toArray
                    |> tryRandKey
                match randToken with
                | None -> res @ [startToken]
                | Some token -> gen (res @ [startToken]) token
            | None ->
                res @ [startToken]
    let randStartKey =
        dict
        |> Seq.choose (fun p ->
            match p.Value with
            | Entry.Start _ -> Some p.Key
            | _ -> None)
        |> Seq.toArray
        |> tryRandKey
    match randStartKey with
    | None -> []
    | Some key -> gen [] key

String.Join (" ", genPhrase dict)
