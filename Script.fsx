#r "nuget: Npgsql, 6.0.3"
#r "nuget: Dapper, 2.0.123"
#r "nuget: FSharp.Control.AsyncSeq, 3.2.1"

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Text.Json
open FSharp.Control

type Word = string
type Repeat = uint32
type NextWords = Map<Word, Repeat>
type Entry =
    | Start of NextWords
    | Word of NextWords
    | End
type Window =
    | Window1 of firstComponent:Word
    | Window2 of firstComponent:Word * secondComponent:Word
type Entries = Map<Window, Entry>
type Corpus = { CorpusId: int64; Name: string }

[<RequireQualifiedAccess>]
module NextWords =
    let mostRepeatableWord (nw: NextWords) : Word option =
        let most = nw |> Seq.sortByDescending (fun x -> x.Value) |> Seq.tryHead
        match most with
        | Some w -> Some w.Key
        | None -> None

    let randomWord (nw: NextWords) : Word option =
        let most = nw |> Seq.map (fun p -> (Guid.NewGuid(), p.Key)) |> Seq.sortBy fst |> Seq.tryHead
        match most with
        | Some (_, w) -> Some w
        | None -> None

    let add (w: Word) (nw: NextWords) : NextWords =
        let repeat =
            match Map.tryFind w nw with
            | Some r -> r
            | None -> 0u
        Map.add w (repeat + 1u) nw

    let ofEntry = function
        | Start nw -> nw
        | Word nw -> nw
        | End -> Map.empty

[<RequireQualifiedAccess>]
module Window =
    let inline first win =
        match win with
        | Window1 x -> x
        | Window2 (f, _) -> f

    let toWord = function
        | Window1 x -> x
        | Window2 (f, s) -> $"{f} {s}"

    let toArray = function
        | Window1 x -> [| x |]
        | Window2 (f, s) -> [| f; s |]

    let ofWord (w: Word) = Window1 w

[<RequireQualifiedAccess>]
module Entry =
    let inline create (prevEntry: Entry option) (words: NextWords) : Entry =
        match prevEntry with
        | Some (End)
            -> Start words
        | Some (Word _)
        | Some (Start _)
        | None
            -> Word words

module EntryParser =
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

    let private createEntry w nextw prevEntry dict =
        let addNextWord =
            match nextw with
            | Some nw -> NextWords.add nw
            | None -> fun x -> x
        match Map.tryFind w dict with
        | Some e ->
            NextWords.ofEntry e
            |> addNextWord
            |> Entry.create prevEntry
        | None ->
            Map.empty
            |> addNextWord
            |> Entry.create prevEntry

    let private window1Parser tokens prevEntry dict =
        match tokens with
        // a b
        | (WordToken w)::nextw::tail ->
            let win = Window1 w
            Some (win, createEntry win (Some nextw) prevEntry dict, nextw::tail)
        // . a
        | (EndToken w)::nextw::tail ->
            Some (Window1 w, End, nextw::tail)
        // a $
        | (WordToken w)::tail ->
            let win = Window1 w
            Some (win, createEntry win None prevEntry dict, tail)
        // . $
        | (EndToken w)::tail ->
            Some (Window1 w, End, tail)
        | _ ->
            None

    let private window2Parser tokens prevEntry dict =
        match tokens with
        // a b c
        // a b .
        | (WordToken w1)::(WordToken w2)::nextw::tail ->
            let w = Window2 (w1, w2)
            Some (w, createEntry w (Some nextw) prevEntry dict, w2::nextw::tail)
        // a . b
        | (WordToken w1)::(EndToken w2)::nextw::tail ->
            Some (Window1 w1, End, w2::nextw::tail)
        // . b c
        | (EndToken w)::w2::nextw::tail ->
            Some (Window1 w, End, w2::nextw::tail)
        // a b $
        // $ - eol
        | (WordToken w1)::(WordToken w2)::tail ->
            let w = Window2 (w1, w2)
            Some (w, createEntry w None prevEntry dict, w2::tail)
        // a . $
        | (WordToken w1)::(EndToken w2)::tail ->
            Some (Window1 w1, End, w2::tail)
        // . b $
        | (EndToken w)::nextw::tail ->
            Some (Window1 w, End, nextw::tail)
        // a $
        | (WordToken w)::tail ->
            let win = Window1 w
            Some (win, createEntry win None prevEntry dict, tail)
        // . $
        | (EndToken w)::tail ->
            Some (Window1 w, End, tail)
        | _ ->
            None

    let private parseEntries windowParser (seed: Map<Window, Entry>) (tokens: string list): Map<Window, Entry> =
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

let dict = getEntriesFromJson () |> parseEntries2 Map.empty
dict
|> Map.iter (fun k v -> printfn "%A: %A" k v)

type Phrase = Phrase of Word list

[<RequireQualifiedAccess>]
module Phrase =
    let empty = Phrase []
    let add (w: Word) (Phrase ws) : Phrase = Phrase (ws @ [w])
    let addFromWindow (win: Window) (p: Phrase) : Phrase = add (win |> Window.toWord) p
    let toString (Phrase ws) = String.Join (" ", ws)

let genPhrase (dict: Map<Window, Entry>) : Phrase =
    let random = Random()
    let tryRandItem (keys: 'a[]) : 'a option =
        if Array.length keys > 0
        then Some (keys.[random.Next(0, keys.Length)])
        else None
    let inline findPairByWindow (win: Window) (dict: Map<Window, Entry>) =
        let randomPair =
            dict
            |> Seq.filter (fun e -> e.Key = win || ((Window.first e.Key) = (Window.first win)))
            |> Seq.toArray
            |> tryRandItem
        match randomPair with
        | Some pair -> Some (pair.Key, pair.Value)
        | None -> None
    let rec gen (result: Phrase) (win: Window): Phrase =
        match Window.toWord win with
        | EndToken _ ->
            result
        | _ ->
            match findPairByWindow win dict with
            | Some (key, entry) ->
                let randWord =
                    entry
                    |> NextWords.ofEntry
                    |> NextWords.randomWord
                let updatedPhrase = Phrase.addFromWindow key result
                match randWord with
                | None -> updatedPhrase
                | Some token -> gen updatedPhrase (Window.ofWord token)
            | None ->
                Phrase.addFromWindow win result
    let randStartKey =
        dict
        |> Seq.choose (fun p ->
            match p.Value with
            | Entry.Start _ -> Some p.Key
            | _ -> None)
        |> Seq.toArray
        |> tryRandItem
    match randStartKey with
    | None -> Phrase.empty
    | Some key -> gen Phrase.empty key

dict
|> genPhrase
|> Phrase.toString
