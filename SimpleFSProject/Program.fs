open System
open System.IO
open System.Diagnostics
open System.Collections.Generic
open System.Text.RegularExpressions

//OOP paradigm
//explicit constructor
type MyClass1<'a>(x : 'a) =
    static let mutable m_numLeft : int = 5
    member val Prop = x with get, set    
    new () =
        MyClass1(Unchecked.defaultof<'a>)
    member public inner.val2 = inner.Prop
    member internal self.val3 = self.Prop
//implicit constructor
type MyClass2(x : int) =
    let square a : int =
        a * a
    member __.Y = square x
    member __.X = x

//use let and do as initialization part
//use val as default values or in explicit constructors
//use member as Properties of the object
//inheritance
type BaseClass =
    val m_field1 : int
    new(x) = { m_field1 = x }
    member this.Field1 = this.m_field1

type DerivedClassImplicit(field1, field2) = 
    inherit BaseClass(field1)
    static let mutable x = 15
    let m_field2 : int = field2
    static do
        x <- 100
    new (field1, field2, field3) =        
        new DerivedClassImplicit(field1, field2 + field3)    
    member this.Field2 = m_field2
    static member val Field3 : int = x with get, set
    static member public Field4 = x

type DerivedClassExplicit =
    inherit BaseClass
    val m_field2 : int
    new(field1, field2) =
        let wat = field2 * 2
        {(*brackets are necessary in initialization*)
            inherit BaseClass(field1)
            m_field2 = wat
        }
    member this.Field2 = this.m_field2

type Sandwitch() as this =
    do
        ignore("blood"::this.Ingredients)
    abstract Ingredients : string list
    default this.Ingredients = []    

    abstract Calories : int
    default this.Calories = 0

type BLTSandwitch() = 
    inherit Sandwitch()

    override this.Ingredients = ["Bacon"; "Bread"; "Mayonese"]
    override this.Calories = 400

type TurkeySwissSandwich =
    inherit Sandwitch

    new () =
        {
            inherit Sandwitch()            
        }
    override this.Ingredients = ["Turkey"; "Swiss"]
    override this.Calories = 330

//implementing IDisposable
type MultiFileLogger() =
    let mutable disposed = false
    let m_logs = new List<StreamWriter>()
    abstract Dispose : bool -> unit
    default this.Dispose (disposing : bool) =
        if not disposed then
            disposed <- true

            if disposing then                
                ()
            printfn "Cleaning up..."
            m_logs |> Seq.iter (fun writer -> writer.Close())
            m_logs.Clear()
            ()
    member this.AttachLogFile file =
        let newLogFile = new StreamWriter(file, true)
        m_logs.Add(newLogFile)
    member this.LogMessage (msg : string) =
        m_logs |> Seq.iter (fun writer -> writer.WriteLine(msg))
    interface IDisposable with
        member this.Dispose() =
            this.Dispose true
            GC.SuppressFinalize(this)
    override this.Finalize () =
        this.Dispose false
//self defined simple style exceptions
exception MyOwnException of string * int
//discriminated unions
type Suit =
    |Spade
    |Club
    |Diamond
    |Heart    

type Card =
    |Ace of Suit
    |Queen of Suit
    |King of Suit
    |Jack of Suit
    |ValueCard of int * Suit

//enum
type ChessPiece =
    | Empty = 0
    | Pawn = 1
    | Knight = 3
    | Bishop = 4
    | Rook = 5
    | Queen = 8
    | King = 1000000

type BinaryTree =
    | Node of int * BinaryTree * BinaryTree
    | Empty

type Pet =
    | Dog of string
    | Cat of string * int

//defining a record with member
type Vector =
    { X : float; Y : float; Z : float }
        member this.Length =
            sqrt <| this.X ** 2.0 + this.Y ** 2.0 + this.Z ** 2.0

type Statement =
        | Print of string
        | Sequence of Statement * Statement
        | IfStmt of Expression * Statement * Statement
    // Mutual recursion needs and operator
    and Expression =
        | Integer of int
        | LessThan of Expression * Expression
        | GreaterThan of Expression * Expression

//measures metadata
[<Measure>] type Meters
[<Measure>] type Seconds

[<EntryPoint>]
let main (args : string[]) = 
    //partition and filter functions of the List class emulation
    let partition (func: 'a -> bool) (lst: 'a list) =        
        (List.filter func lst, List.filter (fun vl -> not (func vl)) lst)
    let x, y = partition (fun vl -> vl % 3 = 0) [1..2..20]
    //using |> pipeline
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
    //folder size imperative way
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

    //using >> composition operator
    let sizeOfFolderComposed =
        let getFiles folder =
            Directory.GetFiles(folder, "*.*", SearchOption.AllDirectories)
        getFiles
        >> Array.map (fun file -> new FileInfo(file))
        >> Array.map (fun info -> info.Length)
        >> Array.sum
    ignore (sizeOfFolderComposed @"D://Projects")

    fun str -> str + " is your string"
    <| sprintf "(%d, %d)" 1 2
    |> printf "format: %s "

    ignore (
        [ [1]; []; [4;5;6]; [3;4]; []; []; []; [9] ]
        |> List.filter (not << List.isEmpty)
        )

    let xx = (<<) not List.isEmpty

    //high-low game using pattern matching
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

    //DSL program using discriminated unions
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

    //pattern matching on lists
    let rec testing l =
        match l with
        | [] -> false        
        | _ :: tail -> 
                printf "still having tail"
                testing tail

    //list yield filling
    let deckOfCards =
        [
            for suit in [ Spade; Club; Heart; Diamond ] do
                yield Ace(suit)
                yield King(suit)
                yield Queen(suit)
                yield Jack(suit)
                for value in 2 .. 10 do
                    yield ValueCard(value, suit)
        ]
    
    //Tree structure printing using pattern matching and discriminated unions
    let rec printInOrder tree =
        match tree with
        | Node (data, left, right) -> 
            printInOrder left
            printfn "Node %d" data
            printInOrder right
        | Empty -> ()
    //try not to use wildcards    
    //queries (on the top of IQueryable)
    let windowedProcesses =
        query {
            for activeProcess in Process.GetProcesses() do
            where (activeProcess.MainWindowHandle <> nativeint 0) 
            select activeProcess
            }
    let printProcessList procSeq =
        Seq.iter (printfn "%A") procSeq
    //will give a compiler error: mutable values are always saved on the stack, so you can't capture them
    let invalidUseOfMutable() =
        let mutable x = 0
        let incrementX() = x <- x + 1
        incrementX()
        x
    let letters = ref [ "a"; "b"; "c"; "d"; "e"; "f" ]
    //:= to assign, ! to get values
    letters := !letters |> List.filter (fun p -> p <> "e")
    let array : int[][][] = Array.zeroCreate 5
    array.[0] <- Array.zeroCreate 2
    array.[0].[0] <- Array.zeroCreate 5
    let listOfElements = new List<string>()
    listOfElements.Add("first")
    listOfElements.Add("second")
    listOfElements.AddRange(!letters |> List.toArray)
    for i in listOfElements do
        printf "value: %s" i

    let pattern =
        function
        | Cat( _ , 5) ->
            printf "Cat 5"
        | Cat( _ , _ ) ->
            printf "Cat undefined"
        | Dog( _ ) ->
            printf "Dog"

    let pets = [| Dog("Doggie"); Cat("Pussy", 15); Cat("Yuppie", 3) |]
    for pattern in query {
                           for pet in pets do select pet } do
        printf "Using pattern"
    let divide2 x y =
        if y = 0 then raise <| new System.DivideByZeroException()
        x / y

    let exitCode =
        try
            let filePath = args.[0]
            printfn "Trying to gather information about file:"
            printfn "%s" filePath
            // Does the drive exist?
            let matchingDrive =
                Directory.GetLogicalDrives()
                |> Array.tryFind (fun drivePath -> drivePath.[0] = filePath.[0])
            if matchingDrive = None then
                raise <| new DriveNotFoundException(filePath)
            // Does the folder exist?
            let directory = Path.GetPathRoot(filePath)
            if not <| Directory.Exists(directory) then
                raise <| new DirectoryNotFoundException(filePath)
            // Does the file exist?
            if not <| File.Exists(filePath) then
                raise <| new FileNotFoundException(filePath)
            let fileInfo = new FileInfo(filePath)
            printfn "Created = %s" <| fileInfo.CreationTime.ToString()
            printfn "Access = %s" <| fileInfo.LastAccessTime.ToString()
            printfn "Size = %d" fileInfo.Length
            0        
        with
        // Combine patterns using Or
        | :? DriveNotFoundException
        | :? DirectoryNotFoundException as ex ->  
            printfn "Unhandled Drive or Directory not found exception"
            1
        // Bind the exception value to value ex
        | :? FileNotFoundException as ex -> 
            printfn "Unhandled FileNotFoundException: %s" ex.Message
            3
        | :? IOException as ex -> 
            printfn "Unhandled IOException: %s" ex.Message
            4
        | MyOwnException(message, value) ->
            printfn "MyOwnException with message: %s" message
            5
        // Use a wildcard match (ex will be of type System.Exception)
        | _ as ex -> 
            printfn "Unhandled Exception: %s" ex.Message
            6
        // Return the exit code
    printfn "Exiting with code %d" exitCode
    ignore exitCode
    let lst = new List<_>( [| 1; 2; 3; 4 |] )
    lst.Sort(
        {
            new IComparer<int> with
                member this.Compare (a, b)=
                    match (a, b) with
                    | _ when a > b -> 1
                    | _ when a < b -> -1
                    | _ when a = b -> 0
        })
    //enums test
    let createChessBoard() =
        let board = Array2D.init 8 8 (fun _ _ -> ChessPiece.Empty)
        // Place pawns
        for i = 0 to 7 do
            board.[1,i] <- ChessPiece.Pawn
            board.[6,i] <- enum<ChessPiece> (-1 * int ChessPiece.Pawn)
        // Place black pieces in order
        [| ChessPiece.Rook; ChessPiece.Knight; ChessPiece.Bishop; ChessPiece.Queen;
        ChessPiece.King; ChessPiece.Bishop; ChessPiece.Knight; ChessPiece.Rook |]
        |> Array.iteri(fun idx piece -> board.[0,idx] <- piece)
        // Place white pieces in order
        [| ChessPiece.Rook; ChessPiece.Knight; ChessPiece.Bishop; ChessPiece.King;
        ChessPiece.Queen; ChessPiece.Bishop; ChessPiece.Knight; ChessPiece.Rook |]
        |> Array.iteri(fun idx piece ->
                                        board.[7,idx] <- enum<ChessPiece> (-1 * int piece))
        // Return the board
        board
    //active patterns
    //1. Single-case
    let (|FileExtension|) filePath = Path.GetExtension(filePath)
    let determineFileType (filePath : string) =
        match filePath with
        // Without active patterns
        | filePath when Path.GetExtension(filePath) = ".txt"
            -> printfn "It is a text file."
        // Converting the data using an active pattern
        | FileExtension ".jpg"
        | FileExtension ".png"
        | FileExtension ".gif"
            -> printfn "It is an image file."
        // Binding a new value
        | FileExtension ext
            -> printfn "Unknown file extension [%s]" ext
    //2. Partial-case
    let (|ToBool|_|) x =
        let success, result = Boolean.TryParse(x)
        if success then Some(result)
        else None
    let (|ToInt|_|) x =
        let success, result = Int32.TryParse(x)
        if success then Some(result)
        else None
    let describeString str =
        match str with
        | ToBool b -> printfn "%s is a bool with value %b" str b
        | ToInt i -> printfn "%s is an integer with value %d" str i
        | _ -> printfn "%s is not a bool, or int" str
    //parametrized
    let (|RegexMatch3|_|) (pattern : string) (input : string) =
        let result = Regex.Match(input, pattern)
        if result.Success then
            match (List.tail [ for g in result.Groups -> g.Value ]) with
            | fst :: snd :: trd :: []
                -> Some (fst, snd, trd)
            | [] -> failwith <| "Match succeeded, but no groups found.\n" +
                                "Use '(.*)' to capture groups"
            | _ -> failwith "Match succeeded, but did not find exactly three groups."
        else None
    let parseTime input =
        match input with
        // Match input of the form "6/20/2008"
        | RegexMatch3 "(\d+)/(\d+)/(\d\d\d\d)" (month, day, year)
        // Match input of the form "2004-12-8"
        | RegexMatch3 "(\d\d\d\d)-(\d+)-(\d+)" (year, month, day)
            -> Some( new DateTime(int year, int month, int day) )
        | a -> 
            printfn "what we've got is %s" a
            None
    //Multicase
    let (|Paragraph|Sentence|Word|WhiteSpace|) (input : string) =
        let input = input.Trim()
        if input = "" then
            WhiteSpace
        elif input.IndexOf(".") <> -1 then
            let sentences = input.Split([|"."|], StringSplitOptions.None)
            Paragraph (sentences.Length, sentences)
        elif input.IndexOf(" ") <> -1 then
            Sentence (input.Split([|" "|], StringSplitOptions.None))
        else        
            Word (input)
    //feels like it just returns discriminated union to work with
    let rec countLetters str =
        match str with
        | WhiteSpace -> 0
        | Word x -> x.Length
        | Sentence words
            -> words
            |> Array.map countLetters
            |> Array.sum
        | Paragraph (_, sentences)
            -> sentences
            |> Array.map countLetters
            |> Array.sum
    0