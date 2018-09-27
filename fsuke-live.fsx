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
// It's better than having plain int
type [<Measure>] st

// Note 
// op note + semitone
type [<Struct>] Note = Note of int<st>
    with
    // this operatore enable us to 
    // add semitones to a Note to get another Note
    static member (+) (Note n, offset) = Note(n + offset)


/// %% Positive modulo
// .net % is behaving strangely with negative numbers
// so we correct it by adding the modulus to get a positive result
let (%%) n m =
    if n >= 0<st> then
        n % m
    else
        (n % m) + m


// chroma
// op chroma + semitone -> chroma
// op chroma - chroma -> semitone
/// A Chroma represents the name of a Note, whatever the octave
type Chroma = Chroma of int<st>
    with
    static member (+) (Chroma c, offset) = Chroma( (c + offset) %% 12<st> )
    static member (-) (Chroma x, Chroma y) = (x - y) %% 12<st>
    


// chroma: Note -> Chroma
// octave Chroma -> int -> Note

/// Gets a Chroma from a Note
let chroma (Note n) =
    Chroma (n %% 12<st>)

/// Gets a Note from a Chroma for given octave
let octave (Chroma c) o =
    Note( c + o * 12<st>)

// This is a pretty printer for chromas.
let chromaNames = ["C"; "C#"; "D"; "D#"; "E"; "F"; "F#"; "G"; "G#"; "A"; "Bb"; "B"]
fsi.AddPrinter(fun (Chroma c) -> chromaNames.[int c % 12] )


/// Flat (~-) Chroma
// Flat (b) is just one semitone under a given Chroma 
let (~-) (c: Chroma) = c + -1<st>

/// Sharp (~+) Chroma
// Sharp (#) is one semitone over a given Chroma
let (~+) (c: Chroma) = c + 1<st>

// c, d, e ...: Chroma

/// We define c as Chroma 0
let c = Chroma 0<st>
// d is 2 semitons after c 
let d = c + 2<st>
// e is 2 semitons after d
let e = d + 2<st>
// there is only one semitone between e and f...
let f = e + 1<st>
//..
let g = f + 2<st>
// ..
let a = g + 2<st>
// ..
let b = a + 2<st>

// C, D, E : Note

// we can define function that give the note for a given octave:
let C = octave c
let D = octave d
let E = octave e
let F = octave f
let G = octave g
let A = octave a
let B = octave b


// type Interval : Chroma -> Chroma
// A interval function takes a base Chroma and
// returns a Chroma after applying the interval
type Interval = Chroma -> Chroma

let interval distance : Interval = 
    fun (c: Chroma) -> c + distance

// unison is the interval defined by the distance between c and ... c
let unison = interval (c - c)
// the second in the interval defined by the distance between d and c...
let second = interval (d - c)
let third = interval (e - c)
let fourth = interval (f - c)
let fifth = interval (g - c)
let sixth = interval (a - c)
let seventh = interval (b - c) 

// minor : Interval -> Interval
// a minor interval is 1 semiton less than given interval
let minor (interval: Interval) : Interval =
    fun c -> interval c + -1<st>


// Chord: Chroma Set
// chord : Interval list -> Chroma -> Chord
// major / minor

// A Chord is a defined by a set of Chroma.
// Whatever the octave and the order of Chroma, Chords are the sames 
type Chord = Chord of Chroma Set

// So we can define a chords by taking an interval list,
// applying each to the fundamental Chroma
// and make a set from resulting Chromas
let chord (intervals : Interval list) fundamental =
    intervals
    |> List.map (fun interval -> interval fundamental)
    |> set
    |> Chord

/// The Major chord. Provide the fundamental and you'll get
/// the chroma it contains
let M = chord [ unison; third; fifth]
/// The minor chord. it like Major but with a minor third
let m = chord [ unison; minor third; fifth]


// String = Note
// Fret =  String * int<st> 
// A string has a specific Note when no fret is pressed.
type String = String of Note 

/// This are the Ukulele 4 Strings
module String =
    let G = String (G 4)
    let C = String (C 4)
    let E = String (E 4)
    let A = String (A 4)


/// The represents pressing the nth fret on a given String
type Fret = Fret of String * int<st>


// fret : String -> Chroma -> Fret list 
// This fonction indicates how to play given Chroma on given String.
// the - operation uses the %% to get a number between 0 and 11...
let fret (String s) (c: Chroma) =
    [Fret (String s, c - chroma s)]


// frets : String -> Chord -> Fret list seq 
// A chord contains several chromas, and we just find the fret
// for each one.
let frets (s: String) (Chord chromas) =
    chromas
    |> Seq.map (fret s)


// combine : (Chord -> Fret list list) -> (Chord -> Fret list list) -> (Chord Fret list list) 
// The outer list (in Fret list list) is to enumerate possibilites
// the inner list is used to concatenates Frets from different strings.
// the combine operation combines functions that take a chord and
// return possibilities by combining inner list in all possible ways 
let combine lx ly =
    fun ch ->
        seq {
            for x in lx ch do
            for y in ly ch do
            yield x @ y }

// a infix operator will make reading easier in next function
let (<.>) = combine


// fretChoices Chord -> Fret list seq
// this function takes a chord, and returns all possibilies
// of how to press frets on the four strings.
// it just use the combine function to enumerate all possibilities
let fretChoices =
    frets String.G
    <.> frets String.C    
    <.> frets String.E
    <.> frets String.A



// fretChroma: Fret -> Chroma
// this function returns the chroma for a Fret on a string
let fretChroma (Fret(String s, f)) =
    chroma (s + f)

// tabChord: Fret list -> Chord
// the reconstruct a Chord from a fret list
// it will be used to check that by pressing the frets, all the chroma
// of the chord are actually played.
let tabChord frets =
    frets
    |> List.map fretChroma
    |> set
    |> Chord

// difficulty : Fret list -> int
// A naive difficulty function. It just sums the square of the frets.
// we assume that player frets close to the top is far easier.
let difficutly frets =
    frets
    |> List.sumBy (fun (Fret(_,f)) -> f * f)

// tabs: Chord -> Fret list 
// this is our final function:
// it takes a Chord and return Frets for each String.
let tabs chord =
    chord
    |> fretChoices      // We enumerate the possibilites
    |> Seq.filter (fun fs -> tabChord fs = chord) // filter out possibilites when not all chromas appear
    |> Seq.sortBy difficutly // sort them by difficulty
    |> Seq.head // take the easiest.

// it works !!
tabs (M g)
tabs (m c)



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

// Now you can play Get Lucky !
[ m b; M d; m +f; M e ]

// Like the legend of the phoenix...


