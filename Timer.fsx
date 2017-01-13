open System
let sleepWorkflow time = async{
    // printfn "Starting sleep workflow at %O" DateTime.Now.TimeOfDay
    
    // do! means to wait as well
    do! Async.Sleep time
    // printfn "Finished sleep workflow at %O" DateTime.Now.TimeOfDay
    }

//test
printfn "Hello "
Async.RunSynchronously (sleepWorkflow 5000)
printfn "World"
