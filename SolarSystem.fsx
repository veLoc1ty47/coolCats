open System
open System.Windows.Forms
open System.Drawing

(* En masse funktioner som bliver brugt senere er defineret her *)
(******************************************************************************)
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
    a ** 3.0 // a skal nok være opløftet i tredje, altså a ** 3.0

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
            windowSize : int * int, color : Color) =

    let t0CP = SphericalToCartesian t0SP 
    let t1CP = SphericalToCartesian t1SP
    let v0 = t1CP .- t0CP

    let planetDay1 = new PlanetDay(t0CP, v0, Acceleration t0CP)
    let time : PlanetDay list = TimePeriod days planetDay1 deltaT

    member this.Coords = makeCoords time windowSize
    member this.Color = color
    static member Size = Size(10, 10)
(******************************************************************************)


(* Disse værdier kan ændres, og programmet vil køre derefter *)
(******************************************************************************)
let mutable days = 3000
let deltaT = 1.0
let windowSize = (600, 600)
(******************************************************************************)


(* Oprettelse af alle planeter og deres positioner de næste *days* dage *)
(*****************************************************************************)
let t0SPEarth = (99.7590,-0.0020,0.983313645229)
let t1SPEarth = (100.7782,-0.0020,0.983306196628)
let colorEarth = Color.FromArgb(255, 0, 0, 255)
let earth = new Planet(t0SPEarth, t1SPEarth, days, deltaT, windowSize,
                colorEarth)

let t0SPJupiter = (162.9198, 1.1555, 5.415869377566)
let t1SPJupiter = (162.9964, 1.1563, 5.416060985675)
let colorJupiter = Color.FromArgb(255, 222, 184, 135)
let jupiter = new Planet(t0SPJupiter, t1SPJupiter, days, deltaT, windowSize,
                colorJupiter)

let t0SPMars = (174.1074, 1.5217, 1.657734803227)
let t1SPMars = (174.5484, 1.5135, 1.657327090727)
let colorMars = Color.FromArgb(255, 189, 183, 107)
let mars = new Planet(t0SPMars, t1SPMars, days, deltaT, windowSize,
                colorMars)

let t0SPMercury = (30.3790, -2.1662, 0.325304334680)
let t1SPMercury = (36.0860, -1.4902, 0.321243721300)
let colorMercury = Color.FromArgb(255, 95, 158, 160)
let mercury = new Planet(t0SPMercury, t1SPMercury, days, deltaT, windowSize,
                colorMercury)

let t0SPNeptune = (338.9131, -0.8074, 29.959661175883)
let t1SPNeptune = (338.9191, -0.8075, 29.959637509324)
let colorNeptune = Color.FromArgb(255, 30, 144, 255)
let neptune = new Planet(t0SPNeptune, t1SPNeptune, days, deltaT, windowSize,
                colorNeptune)

let t0SPPluto = (284.9817, 1.6359, 33.013492974393)
let t1SPPluto = (284.9870, 1.6343, 33.014108060669)
let colorPluto = Color.FromArgb(255, 211, 211, 211)
let pluto = new Planet(t0SPPluto, t1SPPluto, days, deltaT, windowSize,
                colorPluto)

let t0SPSaturn = (248.2076, 1.7709, 10.010709670611)
let t1SPSaturn = (248.2379, 1.7700, 10.010839369394)
let colorSaturn = Color.FromArgb(255, 240, 230, 140)
let saturn = new Planet(t0SPSaturn, t1SPSaturn, days, deltaT, windowSize,
                colorSaturn)

let t0SPUranus = (19.1521, -0.6310, 19.975233976928)
let t1SPUranus = (19.1629, -0.6309, 19.975143209926)
let colorUranus = Color.FromArgb(255, 0, 101, 255)
let uranus = new Planet(t0SPUranus, t1SPUranus, days, deltaT, windowSize,
                colorUranus)

let t0SPVenus = (184.6680, 3.2280, 0.720361799843)
let t1SPVenus = (186.2854, 3.1971, 0.720471891014)
let colorVenus = Color.FromArgb(255, 184, 134, 11)
let venus = new Planet(t0SPVenus, t1SPVenus, days, deltaT, windowSize,
                colorVenus)

let t0SPSun = (0.0, 0.0, 0.0)
let t1SPSun = (0.0, 0.0, 0.0)
let colorSun = Color.FromArgb(255, 255, 215, 0)
let sun = new Planet(t0SPSun, t1SPSun, days, deltaT, windowSize,
                colorSun)


let planets : Planet list = [earth; jupiter; mars; mercury; neptune; pluto;
                             saturn; uranus; venus]
(******************************************************************************)


(* All them graphics *)
(******************************************************************************)
let mutable time = 0

let drawPlanet (planet : Planet) (time : int byref) (e : PaintEventArgs) = 
    let pairToPoint p =
        Point (int (round (fst p)), int (round (snd p)))

    let coord = planet.Coords.[time]
    let pos = pairToPoint coord
    let color = planet.Color
    let brush = new SolidBrush (color)
    let size = Planet.Size
    let rect = Rectangle (pos.X, pos.Y, size.Width, size.Height)
    e.Graphics.FillEllipse(brush, rect)

let updatePlanet (form : Form) (time : int byref) (timer : Timer) showTime =
    if time = days then
        timer.Stop()
    printfn "%A" time
    time <- time + 1
    form.Refresh()

let win = new Form()
win.BackColor <- Color.White
win.Size <- Size(600,600)
win.Text <- "Solar system"
for planet in planets do
    win.Paint.Add (drawPlanet planet &time)

let mutable timer = new Timer()
timer.Interval <- 10
timer.Enabled <- true
for planet in planets do
    timer.Tick.Add (updatePlanet win &time timer)

Application.Run win
(******************************************************************************)
