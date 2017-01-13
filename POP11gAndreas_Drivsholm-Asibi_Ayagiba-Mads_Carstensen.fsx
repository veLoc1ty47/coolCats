open System.Windows.Forms
open System.Drawing

type pen = Color * float

////////////////////////////////////////////////////////////////////////////////////////////////////

type Planet () = class
    let deltaT = 1.0
    let mutable veloVector = (0.0, 0.0)

    // Beregner r
    let calcR (radius: float, long: float, lat: float) =
        let toRad n =
            n/360.0*2.0*System.Math.PI
        
        let p = ((radius) * sin(toRad (lat+90.0))*cos(toRad long), (radius) * sin(toRad (lat+90.0))*sin(toRad long))
        p

    // Array der indeholder alle udregnte positioner for given planet
    let locationArray = Array.init 365 (fun x -> (calcR (0.98329, 100.6001, 0.0044)))
    
    // Beregner a_i
    member x.calcA r = 
        (((2.959122082322128*10.0**(-4.0))/(((fst r)**2.0+(snd r)**2.0)**0.5)**3.0)*(fst r), ((2.959122082322128*10.0**(-4.0))/(((fst r)**2.0+(snd r)**2.0)**0.5)**3.0)*(snd r))

    // Beregner V(t_{n+1})
    member x.calcV (velo : float * float) (accel : float * float) (deltaT : float) =
        match velo, accel with
        | (a, b), (c, d) -> veloVector <- (a + c * deltaT, b + d * deltaT)

    member x.pArray = locationArray
    
    // Beregner r_0
    member x.veloHelp (point1 : float * float * float) (point2 : float * float * float) =
        match (calcR point1), (calcR point2) with
        | (a, b), (c, d) -> veloVector <- (c-a, d-b) 

    // Beregner alle r 
    member x.position (n) = 
        for i = 1 to n do 
            let s = (fst (locationArray.[i-1]) + fst (veloVector) * deltaT, snd (locationArray.[i-1]) + snd (veloVector) * deltaT)
            locationArray.[i] <- s        
            x.calcV veloVector (x.calcA s) deltaT 
end

let bob = new Planet()

bob.veloHelp (0.98329, 100.6001, 0.0044) (0.98328, 101.6192, 0.0044)
bob.position 364

////////////////////////////////////////////////////////////////////////////////////////////////////

let createWindow bgcolor (width, height) title (*draw*) =
    let win = new Form ()
    win.Text <- title
    win.ClientSize <- Size (width, height)
    win.BackColor <- bgcolor
    //win.Paint.Add draw
    win

let createEllipse (color : Color) (p1 : int) (p2 : int) (p3 : int) (p4 : int) (e : PaintEventArgs) = 
    let pen = new Pen (color, single 2.0)
    e.Graphics.DrawEllipse(pen, p1, p2, p3, p4)

let mutable point1 = int(round(-8.0879))+400
let mutable point2 = int(round(86.6510))+400
let mutable point3 = 10
let mutable point4 = 10

let win = createWindow (Color.White) (800, 800) ("Hello motherfucker") //(createEllipse Color.Black (int(round(-8.0879))+400) (int(round(68.6510))+400) 10 10)

let mutable counter = 0

let paintFunction() =
    win.Paint.Add (createEllipse Color.White point1 point2 point3 point4)
    match bob.pArray.[counter] with
    | (a,b) ->
        point1 <- int(round(a*100.0))-10+400
        if b > 0.0 then
            point2 <- int(round(b*100.0))-10+400
        else 
            point2 <- int(round(b*100.0))+10+400
    win.Paint.Add (createEllipse Color.Black point1 point2 point3 point4)
    counter <- counter + 1
    win.Invalidate()
    printfn "Point 1: %A. Point 2 : %A." point1 point2

for j = 1 to 50 do
    paintFunction()
    
Application.Run win

