// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open System
open System.IO

type Statement =
        | Print of string
        | Sequence of Statement * Statement
        | IfStmt of Expression * Statement * Statement
    // Program expressions
    and Expression =
        | Integer of int
        | LessThan of Expression * Expression
        | GreaterThan of Expression * Expression

[<EntryPoint>]
let main (args : string[]) = 
    if args.Length <> 2 then
        failwith "AAAA, error!"
    let greeting, thing = args.[0], args.[1]
    let timeOfDay = DateTime.Now.ToString("hh:mm tt")
    printfn "%s, %s at %s" greeting thing timeOfDay
    let partition (func: 'a -> bool) (lst: 'a list) =        
        (List.filter func lst, List.filter (fun vl -> not (func vl)) lst)
    let x, y = partition (fun vl -> vl % 3 = 0) [1..2..20]
    let filter func lst =
        let rec inner innerF innerLst innerRes =
            if List.length innerLst = 0 then
                innerRes
            else
                let tempCheck = innerF (List.head innerLst)
                if tempCheck then
                    (innerLst |> List.head |> List.append innerRes)
                    |> ((innerLst |> List.tail)
                    |> inner innerF)
                else
                    inner 
                        innerF
                        (List.tail innerLst)
                        innerRes
        List.rev (inner func lst [])
    let sizeOfFolder folder =
        let filesInFolder : string [] =
            Directory.GetFiles(
                folder, "*.*",
                SearchOption.AllDirectories)
        
        let fileInfos : FileInfo [] = 
            Array.map 
                (fun (file : string) -> new FileInfo(file)) filesInFolder
        
        let fileSizes : int64 [] = 
            Array.map 
                (fun (info : FileInfo) -> info.Length) fileInfos
        
        let totalSize = Array.sum fileSizes
        
        printf "total size %d" totalSize
        totalSize
    ignore (sizeOfFolder @"D://Projects")
//    let sizeOfFolderComposed =
//        let getFiles folder =
//            Directory.GetFiles(folder, "*.*", SearchOption.AllDirectories)
//        getFiles
//        >> Array.map (fun file -> new FileInfo(file))
//        >> Array.map (fun info -> info.Length)
//        >> Array.sum
    fun str -> str + " is your string"
    <| sprintf "(%d, %d)" 1 2
    |> printf "format: %s "

    [ [1]; []; [4;5;6]; [3;4]; []; []; []; [9] ]
    |> List.filter (not << List.isEmpty)
    let xx = (<<) not List.isEmpty

    let highLowGame () =
        let rng = new Random()
        let secretNumber = rng.Next() % 100
        let rec highLowGameStep () =
            printfn "Guess the secret number:"
            let guessStr = Console.ReadLine()
            let guess = Int32.Parse(guessStr)
            match guess with
            | _ when guess > secretNumber
                -> printfn "The secret number is lower."
                   highLowGameStep()
            | _ when guess = secretNumber
                -> printfn "You've guessed correctly!"
                   ()
            | _ when guess < secretNumber
                -> printfn "The secret number is higher."
                   highLowGameStep()
        // Begin the game
        highLowGameStep()
    let program =
        IfStmt(
            GreaterThan(
                Integer(3),
                Integer(1)),
            Print("3 is greater than 1"),
            Sequence(
                Print("3 is not"),
                Print("greater than 1")
            )
        )
    let rec testing l =
        match l with
        | [] -> false        
        | _ :: tail -> 
                printf "still having tail"
                testing tail
    0