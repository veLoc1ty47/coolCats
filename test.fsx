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

for elm in bob.pArray do
    printfn "%A" elm
