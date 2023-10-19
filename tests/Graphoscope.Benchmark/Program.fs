open BenchmarkDotNet.Running

let defaultSwitch () = BenchmarkSwitcher [|typeof<Graphs.Graphs>; |]

[<EntryPoint>]
let Main args =
    let summary = defaultSwitch().Run args
    0
