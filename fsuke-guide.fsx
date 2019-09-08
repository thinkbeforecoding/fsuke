let title =
    """
            //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//
            //          Ukulele Tabs in F#            //
            //                                        //
            //                ( o )==::               //
            //                                        //
            //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//
    """

let speaker =
    "Jeremie Chassaing"

let company = 
    [ "D-Edge"
      "Accor" ]

let website = "https://thinkbeforecoding.com"

let twiter = @"thinkb4coding"























__SOURCE_DIRECTORY__ + "/UkeChords.jpg"
|> System.Diagnostics.Process.Start

































// Semitone measure





























// Note 
// op note + semitone


























/// %% Positive modulo on semitones
/// SemitonesPerOctave


































// chroma
// op chroma + semitone -> chroma
// op chroma - chroma -> semitone

























// c, d, e ...: Chroma

























// chroma: Note -> Chroma
// octave Chroma -> int -> Note



























// C, D, E : Note























(*
let chromaNames = ["C"; "C#"; "D"; "D#"; "E"; "F"; "F#"; "G"; "G#"; "A"; "Bb"; "B"]
let chromaName (Chroma c)= chromaNames.[int c % 12]
fsi.AddPrinter(chromaName)
fsi.AddPrinter(fun (n: Note) -> sprintf "%s%d" (chromaName (chroma n)) (oct n) )
*)

/// Flat (~-) Chroma
/// Sharp (~+) Chroma
























// type Interval : Chroma -> Chroma
// intervals


























// minor : Interval -> Interval
























// Chord: Chroma Set
// chord : Interval list -> Chroma -> Chord
// major / minor




























// String = Note
// Fret =  String * int<st> 
























(*
module String =
    let G = String (G 4)
    let C = String (C 4)
    let E = String (E 4)
    let A = String (A 4)
*)


// fret : String -> Chroma -> Fret list 
























// frets : String -> Chord -> Fret list seq 























// combine : (Chord -> Fret list seq) -> (Chord -> Fret list seq) -> (Chord Fret list seq) 
let combine lx ly =
    fun ch ->
        seq {
            for x in lx ch do
            for y in ly ch do
            yield x @ y }

let (<.>) = combine


// fretChoices Chord -> Fret list seq


























// fretChroma: Fret -> Chroma
// tabChord: Fret list -> Chord
// difficulty : Fret list -> int
// tabs: Chord -> Fret list 
























(*
// Printers
type TabPrinter = int -> string


let stringPrinter fret : TabPrinter =
    let pressed (Fret (_, f)) : TabPrinter = fun n -> if n = int f - 1 then "●" else "│"
    let bar : TabPrinter = fun n -> if n = 0 then "╤" else "┼"

    fun n ->
        if n % 2 = 0 then
            bar (n/2)
        else
            pressed fret (n/2)

module TabPrinter =
    let space : TabPrinter = fun _ -> " "
    let start : TabPrinter =  fun _ -> ""

    let concat (lx: TabPrinter) (ly: TabPrinter) : TabPrinter =
        fun n -> lx n + ly n

let (@@) = TabPrinter.concat 

let printChord c = c |> List.map stringPrinter |> List.reduce TabPrinter.concat

// let concatTabs  lx ly = lx @@ TabPrinter.space @@ printChord (tabs ly)

// let (<++>) = concatTabs

let print f = 
    for i in 0 .. 12 do printfn "%s" (f i)


fsi.AddPrinter<TabPrinter> (fun f ->
    print f
    string ""
)

fsi.AddPrinter<Chord>  (fun (c) ->
    print (printChord (tabs c))
    string ""
)

fsi.AddPrinter<Chord list> (fun cs ->
    print (List.fold (fun a c -> a @@ TabPrinter.space @@ printChord (tabs c) ) TabPrinter.start cs)
    string ""
)


[ m b; M d; m +f; M e ]
*)