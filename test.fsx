// Beregner r
let convert (radius: float, long: float, lat: float) =
    let toRad n =
        (n*System.Math.PI)/180.0

    ((radius) * sin(toRad (lat+90.0))*cos(toRad long), (radius) * sin(toRad (lat+90.0))*sin(toRad long))

type Planet() = class
    let deltaT = 1.0
    let mutable veloVector = (0.0, 0.0)


    // Array der indeholder alle udregnte positioner for given planet
    let locationArray = Array.init 365 (fun x -> (convert (0.983811175790, 88.2911, -0.0022)))

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
        match (convert point1), (convert point2) with
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

bob.veloHelp (0.983811175790, 88.2911, -0.0022) (0.983747567681, 89.3093, -0.0022)
bob.position 364

let mutable nasacoords = []

let loadFile (fileName : string) =
    let openFile = System.IO.File.OpenText fileName
    let mutable k = 'b'
    while k <> '$' do
        k <- char(openFile.Read ())
    for i = 0 to 4 do
        k <- char(openFile.Read ())
    
    let mutable skipForward = (openFile.ReadLine ()).Split ([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
    while (skipForward.[0] : string) <> "2457742.500000000" do
        skipForward <- (openFile.ReadLine ()).Split ([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
        printfn "SkipFoward = %A" skipForward

    match skipForward with
        | [|a; b; c; d; e|] -> nasacoords <- nasacoords @ [convert (float(d), float(b), float(c))]
        | _ -> ()

    while char(openFile.Peek()) <> '$' do
        let mutable p = (openFile.ReadLine ()).Split ([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
        printfn "Rigtige = %A" p
        match p with
        | [|a; b; c; d; e|] -> nasacoords <- nasacoords @ [convert (float(d), float(b), float(c))]
        | _ -> ()

    openFile.Close()

let displayData planet n =
    let diff our nasa =
        match our, nasa with
        | (a, b), (c, d) -> (sqrt((c-a)**2.0 + (d-b)**2.0)) 

    let mutable monthsCounter = 0 

    printfn "\t\t\tShowing calculations for %A" planet
    printfn "----------------------------------------------------------------------------------"
    printfn "  \tOurs \t\t\t\t Nasa \t\t\t\t Diff"
    for l = 0 to n do
        if l % 31 = 0 then
            match bob.pArray.[l], nasacoords.[monthsCounter] with
                | (a, b), (c, d) -> printfn "%i. \t(%.5f, %.5f) \t\t(%.5f, %.5f) \t\t %.5f" (l+1) a b c d (diff bob.pArray.[l] nasacoords.[monthsCounter])
            monthsCounter <- monthsCounter + 1
        else ()

loadFile ("Earth.txt")
displayData "Earth" 364
