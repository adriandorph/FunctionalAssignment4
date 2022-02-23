open MultiSet


let test a b =
    match a = b with
    | true -> "Passed"
    | false -> sprintf "Failed: Expected: %A Actual: %A" a b

[<EntryPoint>]
let main argv =
    printfn "MultiSet: %A" (test 1 (1))
    0