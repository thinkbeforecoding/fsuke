//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//
//          Ukulele Tabs in F#            //
//                                        //
//                ( o )==::               //
//                                        //
// By: Jeremie Chassaing                  //
//                                        //
// (Availpro - FastBooking / Accor)       //
//                                        //
// https://thinkbeforecoding.com          //
//                                        //
//                         @thinkb4coding //
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//



























__SOURCE_DIRECTORY__ + "/UkeChords.jpg"
|> System.Diagnostics.Process.Start















// Semitone measure

























// Note 
// op note + semitone



























/// %% Positive modulo

// chroma
// op chroma + semitone -> chroma
// op chroma - chroma -> semitone

































// chroma: Note -> Chroma
// octave Chroma -> int -> Note
























//let chromaNames = ["C"; "C#"; "D"; "D#"; "E"; "F"; "F#"; "G"; "G#"; "A"; "Bb"; "B"]
//fsi.AddPrinter(fun (Chroma c) -> chromaNames.[int c % 12] )
























/// Flat (~-) Chroma
/// Sharp (~+) Chroma
























// c, d, e ...: Chroma


























// C, D, E : Note























// type Interval : Chroma -> Chroma























// minor : Interval -> Interval























// Chord: Chroma Set
// chord : Interval list -> Chroma -> Chord
// major / minor























// String = Note
// Fret =  String * int<st> 


// module String =
//     let G = String (G 4)
//     let C = String (C 4)
//     let E = String (E 4)
//     let A = String (A 4)























// fret : String -> Chroma -> Fret list 

// frets : String -> Chord -> Fret list seq 






















// combine : (Chord -> Fret list list) -> (Chord -> Fret list list) -> (Chord Fret list list) 

// let (<.>) = combine
























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