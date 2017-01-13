type Planet() = class
    let deltaT = 1.0
    let mutable veloVector = (0.0, 0.0)

    // Beregner r
    let calcR (radius: float, long: float, lat: float) =
        let toRad n =
            (n*System.Math.PI)/180.0

        ((radius) * sin(toRad (lat+90.0))*cos(toRad long), (radius) * sin(toRad (lat+90.0))*sin(toRad long))

    // Array der indeholder alle udregnte positioner for given planet
    let locationArray = Array.init 365 (fun x -> (calcR (0.98329, 100.6001, 0.0044)))

    let GMSolen = 2.959122082322128*(10.0**(-4.0))
    
    // Beregner a_i
    member x.calcA r = 
        let lengthR = ((((fst r)**2.0)+((snd r)**2.0))**0.5)**3.0
        (-((GMSolen)/(lengthR))*(fst r), (-(GMSolen)/(lengthR)*(snd r)))

    // Beregner V(t_{n+1})
    member x.calcV (velo : float * float) (accel : float * float) (deltaT : float) =
        match velo, accel with
        | (a, b), (c, d) -> 
            veloVector <- (a + c * deltaT, b + d * deltaT)

    member x.pArray = locationArray
    
    // Beregner r_0
    member x.veloHelp (point1 : float * float * float) (point2 : float * float * float) =
        match (calcR point1), (calcR point2) with
        | (a, b), (c, d) -> veloVector <- (c-a, d-b) 

    // Beregner alle r 
    member x.position (n) = 
        for i = 1 to n do 
            locationArray.[i] <- (fst (locationArray.[i-1]) + fst (veloVector) * deltaT, snd (locationArray.[i-1]) + snd (veloVector) * deltaT)        
            x.calcV veloVector (x.calcA (locationArray.[i-1])) deltaT 
            printfn "Beregning nummer: %A" i
            printfn "---------------------------------------"
            printfn "Tidligere lokation: %A" locationArray.[i-1]
            printfn "A er: %A" (x.calcA (locationArray.[i-1]))
            printfn "V ændret til: %A" veloVector
            printfn "---------------------------------------\n"
end

let bob = new Planet()

bob.veloHelp (0.98329, 100.6001, 0.0044) (0.98328, 101.6192, 0.0044)
bob.position 364

for elm in bob.pArray do
    printfn "%A" elm
