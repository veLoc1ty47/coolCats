////////////////////////////////////////////////////////////////////////////////////////////////////

(*let loadFile (fileName : string) =
    let openFile = System.IO.File.OpenText fileName
    let mutable k = 'b'
    while k <> '$' do
        k <- char(openFile.Read ())
    for i = 0 to 4 do
        k <- char(openFile.Read ())
    let mutable p = (openFile.ReadLine ()).Split ([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
    let mutable l = [||]
    printfn "%A" p
    (*match p with
    | *) 
    openFile.Close()

loadFile ("Earth.txt")*)

////////////////////////////////////////////////////////////////////////////////////////////////////


(*let bob = new Timer()
bob.Interval <- 100
bob.Enabled <- true
bob.Tick.Add ()*)
