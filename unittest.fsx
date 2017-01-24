open System
open System.Windows.Forms
open System.Drawing

let Add2DVectors v1 v2 =
    (fst v1 + fst v2, snd v1 + snd v2)

let Subtract2DVectors v1 v2 =
    (fst v1 - fst v2, snd v1 - snd v2)

let fst3 = function
    | (a,_,_) -> a

let snd3 = function
    | (_,b,_) -> b

let trd3 = function
    | (_,_,c) -> c

let (.+) v1 v2 =
    (fst3 v1 + fst3 v2, snd3 v1 + snd3 v2, trd3 v1 + trd3 v2)

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
    a

let Acceleration r =
    let GMSolen = 2.959122082322128 * (10.0 ** -4.0)
    let frac = -((GMSolen) / ((SizeVector r) ** 3.0))
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
            let v = prevPD.v .+ (prevPD.a .* dT) // Parantes omkring prevPd.a .* dT

            (new PlanetDay(r, v, a)) :: earlierDays

    TPHelper days pS dT
    |> List.rev

let makeCoords n windowSize =
    let center =  (float (fst windowSize) / 2.0, float (snd windowSize) / 2.0)
    n
    |> List.map (fun (elem : PlanetDay) -> elem.r)
    |> List.map (fun elem -> (fst3 elem, snd3 elem))
    |> List.map (fun elem -> (fst elem * 10.0, snd elem * 10.0))
    |> List.map (fun elem -> Add2DVectors center elem)

type Planet(t0SP : float * float * float,
            t1SP : float * float * float, days : int, deltaT : float,
            windowSize : int * int, color : Color, name : string) =

    let t0CP = SphericalToCartesian t0SP 
    let t1CP = SphericalToCartesian t1SP
    let v0 = t1CP .- t0CP

    let planetDay1 = new PlanetDay(t0CP, v0, Acceleration t0CP)
    let time : PlanetDay list = TimePeriod days planetDay1 deltaT

    member this.Time
        with get() = time
    member this.Coords = makeCoords time windowSize
    member this.Color = color
    member this.Name = name
    static member Size = Size(10, 10)

let makeCoordsInverse n windowSize =
    let center =  (float (fst windowSize) / 2.0, float (snd windowSize) / 2.0)
    n
    |> List.map (fun (elem : PlanetDay) -> elem.r)
    |> List.map (fun elem -> (fst3 elem, snd3 elem))
    |> List.map (fun elem -> Subtract2DVectors elem center)
    |> List.map (fun elem -> (fst elem / 10.0, snd elem / 10.0))

printfn "Tests for makeCoords"
let a = new PlanetDay((10.0, 10.0, 10.0), (10.0, 10.0, 10.0), (10.0, 10.0, 10.0))
let b = new PlanetDay((20.0, 20.0, 20.0), (10.0, 10.0, 10.0), (10.0, 10.0, 10.0))
let c = new PlanetDay((30.0, 30.0, 30.0), (10.0, 10.0, 10.0), (10.0, 10.0, 10.0))
let ae = makeCoords [a; b; c] (750,750)
let be = makeCoords [a; b; c] (400, 400)
let ce = makeCoords [a; b; c] (0, 0)
printfn "---------------------------------------------------------------"
printfn "Test 1a: %b" (ae = [(475.0, 475.0); (575.0, 575.0); (675.0, 675.0)])
printfn "Test 1b: %b" (be = [(300.0, 300.0); (400.0, 400.0); (500.0, 500.0)])
printfn "Test 1c: %b" (ce = [(100.0, 100.0); (200.0, 200.0); (300.0, 300.0)])

printfn "\nTests for Planet"
printfn "---------------------------------------------------------------"
let bob = new Planet((100.0, 100.0, 100.0), (200.0, 200.0, 200.0), 3, 1.0, (750, 750), Color.FromArgb(255,0,0,255), "Bob")
//printfn "Test 1a: %b" (bob.Time = )
//printfn "Test 1b: %b" (bob.Coords = "lol")
printfn "Test 1c: %b" (bob.Color = Color.FromArgb(255,0,0,255))
printfn "Test 1d: %b" (bob.Name = "Bob")

printfn "\n Tests for makeCoordsInverse"
printfn "---------------------------------------------------------------"
let d = new PlanetDay((475.0, 475.0, 0.0), (575.0, 575.0, 0.0), (675.0, 675.0, 0.0))
let e = new PlanetDay((300.0, 300.0, 0.0), (400.0, 400.0, 0.0), (500.0, 500.0, 0.0))
let f = new PlanetDay((100.0, 100.0, 0.0), (200.0, 200.0, 0.0), (300.0, 300.0, 0.0))
printfn "Test 1a: %A" (makeCoordsInverse [d] (750,750)) //= [(10.0, 10.0); (20.0, 20.0); (30.0, 30.0)])
printfn "Test 1b: %A" (makeCoordsInverse [e] (400,400)) // = [(10.0, 10.0); (20.0, 20.0); (30.0, 30.0)])
printfn "Test 1c: %A" (makeCoordsInverse [f] (0, 0)) // = [(10.0, 10.0); (20.0, 20.0); (30.0, 30.0)])
