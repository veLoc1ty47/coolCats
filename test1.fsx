open System
open System.Windows.Forms
open System.Drawing
(*open Graph*)

let fst3 = function
    | (a,_,_) -> a

let snd3 = function
    | (_,b,_) -> b

let trd3 = function
    | (_,_,c) -> c

let (.+) v1 v2 =
    (fst3 v1 + fst3 v2, snd3 v1 + snd3 v2, trd3 v1 + trd3 v2)

let Add2DVectors v1 v2 =
    (fst v1 + fst v2, snd v1 + snd v2)

let (.-) v1 v2 =
    (fst3 v1 - fst3 v2, snd3 v1 - snd3 v2, trd3 v1 - trd3 v2)

let (.*) v a =
    (a * fst3 v, a * snd3 v, a * trd3 v)

let DegToRad n = n * Math.PI / 180.0

let SphericalToCartesian pos =
    let x = trd3 pos * Math.Sin (DegToRad(snd3 pos + 90.0)) *
            Math.Cos (DegToRad (fst3 pos))
    let y = trd3 pos * Math.Sin (DegToRad(snd3 pos + 90.0)) *
            Math.Sin (DegToRad (fst3 pos))
    let z = trd3 pos * Math.Cos (DegToRad(snd3 pos + 90.0))
    (x,y,z)

let SizeVector v =
    let a = Math.Sqrt((fst3 v) ** 2.0 + (snd3 v) ** 2.0 + (trd3 v) ** 2.0)
    a**3.0

let Acceleration r =
    let GMSolen = 2.959122082322128 * (10.0 ** -4.0)
    let frac = -((GMSolen) / (SizeVector r))
    let pos = r .* frac
    pos

type PlanetDay(r : float * float * float, v : float * float * float,
               a : float * float * float) =
    member val r = r
    member val v = v
    member val a = a

let TimePeriod days pS dT =
    let rec TPHelper n pS dT =
        match n with
        | 1 ->
            [pS]
        | n ->
            let earlierDays = (TPHelper (n-1) pS dT)
            let prevPD : PlanetDay = earlierDays.[0]

            let r = prevPD.r .+ (prevPD.v .* dT)
            let a = Acceleration prevPD.r
            let v = prevPD.v .+ prevPD.a .* dT

            (new PlanetDay(r, v, a)) :: earlierDays

    TPHelper days pS dT
    |> List.rev

let makeCoords n windowSize =
    let center =  (float (fst windowSize) / 2.0, float (snd windowSize) / 2.0)
    n
    |> List.map (fun (elem : PlanetDay) -> elem.r)
    |> List.map (fun elem -> (fst3 elem, snd3 elem))
    |> List.map (fun elem -> (fst elem * 10.0, snd elem * 10.0))
//    |> List.map (fun elem -> Add2DVectors center elem)

type Planet(t0SP : float * float * float,
            t1SP : float * float * float, days : int, deltaT : float,
            windowSize : int * int, color : Color) =

    let t0CP = SphericalToCartesian t0SP 
    let t1CP = SphericalToCartesian t1SP
    let v0 = t1CP .- t0CP

    let planetDay1 = new PlanetDay(t0CP, v0, Acceleration t0CP)
    let time : PlanetDay list = TimePeriod days planetDay1 deltaT

    member this.Coords = makeCoords time windowSize
    member this.Color = color

let days = 365
let deltaT = 1.0
let windowSize = (600, 600)

(*** Oprettelse af alle planeter og deres positioner de næste *days* dage ****)
(*****************************************************************************)
let t0SPEarth = (99.7590,-0.0020,0.983313645229)
let t1SPEarth = (100.7782,-0.0020,0.983306196628)
let earth = new Planet(t0SPEarth, t1SPEarth, days, deltaT, windowSize,
                Color.Blue)

let calcR (radius: float, long: float, lat: float) =
    let toRad n =
        (n*System.Math.PI)/180.0

    ((radius) * sin(toRad (lat+90.0))*cos(toRad long), (radius) * sin(toRad (lat+90.0))*sin(toRad long))

let mutable nasacoords = []

let loadFile (fileName : string) =
    let openFile = System.IO.File.OpenText fileName
    let mutable k = 'b'
    while k <> '$' do
        k <- char(openFile.Read ())
    for i = 0 to 4 do
        k <- char(openFile.Read ())

    while char(openFile.Peek()) <> '$' do
        let mutable p = (openFile.ReadLine ()).Split ([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
        match p with
        | [|a; b; c; d; e|] -> nasacoords <- nasacoords @ [calcR (float(d), float(b), float(c))]
        | _ -> ()

    openFile.Close()

let displayData planet n =
    let diff our nasa =
        match our, nasa with
        | (a, b), (c, d) -> (sqrt((c-a)**2.0 + (d-b)**2.0)) 

    printfn "\t\t\tShowing calculations for %A" planet
    printfn "----------------------------------------------------------------------------------"
    printfn "  \tOurs \t\t\t\t Nasa \t\t\t\t Diff"
    for l = 0 to n do
        match earth.Coords.[l], nasacoords.[l] with
            | (a, b), (c, d) -> printfn "%i. \t(%.5f, %.5f) \t\t(%.5f, %.5f) \t\t %.5f" (l+1) a b c d (diff earth.Coords.[l] nasacoords.[l])

loadFile ("Earth.txt")
displayData "Earth" 364
