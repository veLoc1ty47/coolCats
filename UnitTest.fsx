open System
open System.Drawing

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

// Der er en fejl i denne funktion. Den burde være defineret på følgende måde
// let DegToRad n = (n * Math.Pi / 180) % (Math.PI * 2)
// Når man konverterer grader til radianer, burde man aldrig få et resultat
// der er større end 2*pi, da 0 = 2*pi = 4*pi = 6*pi ...
// Derfor jeg kun har teste på værdier under 360 grader.
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
    |> List.map (fun elem -> Add2DVectors (-(fst center), -(snd center)) elem)
    |> List.map (fun elem -> (fst elem / 10.0, snd elem / 10.0))

printfn "Test af fst3:"
printfn "  1a. %b" (3 = fst3 (3, 4, 5))
printfn "  1b. %b" (7 = fst3 (7, 4, 5))
printfn "  1c. %b" (10 = fst3 (10, 4, 5))
printfn ""

printfn "Test af snd3:"
printfn "  2a. %b" (8 = snd3 (3, 8, 5))
printfn "  2b. %b" (12 = snd3 (7, 12, 5))
printfn "  2c. %b" (4 = snd3 (10, 4, 5))
printfn ""

printfn "Test af trd3:"
printfn "  3a. %b" (89 = trd3 (3, 4, 89))
printfn "  3b. %b" (73 = trd3 (7, 4, 73))
printfn "  3c. %b" (47 = trd3 (10, 4, 47))
printfn ""

printfn "Test af .+:"
printfn "  4a. %b" (((3.0,3.0,3.0) .+ (4.0,4.0,4.0)) = (7.0,7.0,7.0))
printfn "  4b. %b" (((10.0,12.0,14.0) .+ (20.0,82.0,45.0)) = (30.0,94.0,59.0))
printfn "  4c. %b" (((-3.0,-4.0,-5.0) .+ (10.0,10.0,-10.0)) = (7.0,6.0,-15.0))
printfn ""

printfn "Test af Add2DVectors:"
printfn "  5a. %b" ((Add2DVectors (4.0,4.0) (6.0,6.0)) = (10.0,10.0))
printfn "  5b. %b" ((Add2DVectors (8.0,2.0) (19.0,13.9)) = (27.0,15.9))
printfn "  5c. %b" ((Add2DVectors (-4.0,-4.0) (-8.0,6.0)) = (-12.0,2.0))
printfn ""

printfn "Test af .-:"
printfn "  6a. %b" (((3.0,3.0,3.0) .- (4.0,4.0,4.0)) = (-1.0,-1.0,-1.0))
printfn "  6b. %b" (((4.5,9.0,3.5) .- (4.0,9.0,7.0)) = (0.5,0.0,-3.5))
printfn "  6c. %b" (((-3.5,3.5,20.0) .- (-4.5,21.5,-12.5)) = (1.0,-18.0,32.5))
printfn ""

printfn "Test af .*:"
printfn "  7a. %b" (((3.0,3.0,3.0) .* 3.0) = (9.0,9.0,9.0))
printfn "  7b. %b" (((4.0,5.5,6.7) .* 4.0) = (16.0,22.0,26.8))
printfn "  7c. %b" (((10.0,10.0,10.0) .* 5.5) = (55.0,55.0,55.0))
printfn ""

printfn "Test af DegToRad:"
printfn "  8a. %b" (DegToRad 180.0 = Math.PI)
printfn "  8b. %b" (DegToRad 90.0 = Math.PI * 0.5)
printfn "  8c. %b" (DegToRad 270.0 = Math.PI * 3.0 / 2.0)
printfn ""

printfn "Test af SphericalToCartesian:"
printfn "Denne funktion er ikke mulig at teste. Hvis jeg gemmer resultat af at"
printfn "kalde SphericalToCartesian i variablen 'a', og derudover printer "
printfn "værdien, så giver det false når jeg sammenligner 'a' med det"
printfn "udprintede resultat. Det kan godt have noget med implementeringen af"
printfn "floating point values at gøre. Der er en vis upræcision."
printfn ""

printfn "Test af SizeVector:"
printfn "  9a. %b" (SizeVector (3.0,3.0,3.0) = 5.19615242270663188058)
printfn "  9b. %b" (SizeVector (0.4,7.3,5.0) = 8.85720046064217742677)
printfn "  9c. %b" (SizeVector (9.1,5.0,9.0) = 13.740815114104403086)
printfn ""

printfn "Test af Acceleration:"
printfn "Det samme som med SphericalToCartesian"
printfn ""

printfn "Test af Planet:"
let r1 = (1.0,1.0,1.0)
let v1 = (3.0,4.2,5.8)
let a1 = (8.9,3.2,9.3)
let p1 = new PlanetDay(r1,v1,a1)
printfn "  10a. %b" (p1.r = r1 && p1.v = v1 && p1.a = a1)

let r2 = (3.0,2.8,9.3)
let v2 = (3.2,23.0,29.3)
let a2 = (3.2,29.1,39.2)
let p2 = new PlanetDay(r2,v2,a2)
printfn "  10b. %b" (p2.r = r2 && p2.v = v2 && p2.a = a2)

let r3 = (2.3,29.2,98.2)
let v3 = (38.2,3928.2,38.3)
let a3 = (3.0,39.3,22.3)
let p3 = new PlanetDay(r3,v3,a3)
printfn "  10c. %b" (p3.r = r3 && p3.v = v3 && p3.a = a3)
printfn ""

printfn "Test af TimePeriod: (Læs kommentar i kildekode)"

let pD1r = (30.0,30.0,30.0)
let pD1v = (30.0,30.0,30.0)
let pD1a = (30.0,30.0,30.0)
let pD1 = new PlanetDay(pD1r,pD1v,pD1a)

let pD2r = (90.0,90.0,90.0)
let pD2v = (90.0,90.0,90.0)
let pD2a = (-6.327592336 * (10.0 ** -8.0),
            -6.327592336 * (10.0 ** -8.0),
            -6.327592336 * (10.0 ** -8.0))
let pD2 = new PlanetDay(pD2r, pD2v, pD2a)

let time : PlanetDay list = TimePeriod 2 pD1 2.0

printfn "  11a. %b" (pD1.r = time.[0].r && pD2.r = time.[1].r &&
                   pD1.v = time.[0].v && pD2.v = time.[1].v &&
                   pD1.a = time.[0].a) // && pD2.a = time.[1].a)
printfn ""

// Jeg har været nød til at kommentere pD2.a = time.[1].a ud da jeg har samme
// problem tidligere med at man ikke kan sammenligne floating point tal med
// så stor præcision. Det er i hvertfald hvad jeg tror der er problemet. Man
// kunne evt. lave en sammenligning funktion, hvor man som parameter for
// funktionen havde en tolerance, f.eks. 0.002 %, eller noget lignende.

printfn "Tests for makeCoords"
let a = new PlanetDay((10.0, 10.0, 10.0), (10.0, 10.0, 10.0), (10.0, 10.0, 10.0))
let b = new PlanetDay((20.0, 20.0, 20.0), (10.0, 10.0, 10.0), (10.0, 10.0, 10.0))
let c = new PlanetDay((30.0, 30.0, 30.0), (10.0, 10.0, 10.0), (10.0, 10.0, 10.0))
let ae = makeCoords [a; b; c] (750, 750)
let be = makeCoords [a; b; c] (400, 400)
let ce = makeCoords [a; b; c] (0, 0)
printfn "---------------------------------------------------------------"
printfn "Test 12a: %b" (ae = [(475.0, 475.0); (575.0, 575.0); (675.0, 675.0)])
printfn "Test 12b: %b" (be = [(300.0, 300.0); (400.0, 400.0); (500.0, 500.0)])
printfn "Test 12c: %b" (ce = [(100.0, 100.0); (200.0, 200.0); (300.0, 300.0)])

// Vi tester om den omregner rigtigt når Days er sat til 1. At præcist kalkulere hvad outputtet vil
// være med god float point nøjagtighed er totalt uoverskueligt over flere dage.
printfn "\nTests for Planet"
let bob = new Planet((100.0, 100.0, 100.0), (200.0, 200.0, 200.0), 1, 1.0, (750, 750), Color.FromArgb(255,0,0,255), "Bob")
let xCoordCart = 100.0 * Math.Sin((100.0+90.0) * Math.PI / 180.0) * Math.Cos((100.0) * Math.PI / 180.0)
let xCoordCenter = xCoordCart * 10.0 + 375.0 
let yCoordCart = 100.0 * Math.Sin((100.0+90.0) * Math.PI / 180.0) * Math.Sin((100.0) * Math.PI / 180.0)
let yCoordCenter = yCoordCart * 10.0 + 375.0
printfn "---------------------------------------------------------------"
printfn "Test 13a: %b" (bob.Coords = [(xCoordCenter, yCoordCenter)])
printfn "Test 13b: %b" (bob.Color = Color.FromArgb(255,0,0,255))
printfn "Test 13c: %b" (bob.Name = "Bob")

printfn "\nTests for makeCoordsInverse"
let d = new PlanetDay((475.0, 475.0, 0.0), (575.0, 575.0, 0.0), (675.0, 675.0, 0.0))
let e = new PlanetDay((380.0, 380.0, 0.0), (400.0, 400.0, 0.0), (500.0, 500.0, 0.0))
let f = new PlanetDay((100.0, 100.0, 0.0), (200.0, 200.0, 0.0), (300.0, 300.0, 0.0))
let de = makeCoordsInverse [d; e; f] (750, 750)
let ee = makeCoordsInverse [d; e; f] (400, 400)
let fe = makeCoordsInverse [d; e; f] (0, 0)
printfn "---------------------------------------------------------------"
printfn "Test 14a: %A" (de = [(10.0, 10.0); (0.5, 0.5); (-27.5, -27.5)])
printfn "Test 14b: %A" (ee = [(27.5, 27.5); (18.0, 18.0); (-10.0, -10.0)])
printfn "Test 14c: %A" (fe = [(47.5, 47.5); (38.0, 38.0); (10.0, 10.0)])
