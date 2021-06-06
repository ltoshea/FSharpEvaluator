module Evaluator

open System
open System.IO
open FSharp.Text.RegexProvider

type regexPattern = Regex< @"(?<Addr>\d+): (?<Instr>Add|Mult|Value) (?<ValueOrAddr>[-\d\s]+)">

// Note: Initiall was int32 but got overflow exception 
type Instr = Value of int64 | Add of int array | Mult of int array
type Program = Instr array

module EvaluateInstr = 

    let isSimplifiedForm(instr: Instr) = 
        match instr with
        | Value _ -> true
        | _ -> false

let parseLine (input:string) : int * Instr = 
    let rxMatch =  regexPattern().TypedMatch(input)
    let address = Int32.Parse rxMatch.Addr.Value
    let valueOrAddress = rxMatch.ValueOrAddr.Value
    let instruction = match rxMatch.Instr.Value with
        | "Add" -> valueOrAddress.Split(' ') |> Array.map Int32.Parse |> Add
        | "Mult" -> valueOrAddress.Split(' ') |> Array.map Int32.Parse |> Mult
        | "Value" -> Value(Int64.Parse valueOrAddress)
    address, instruction

let parseProgram (input: string seq) : int array * Program = 
    let program = Array.zeroCreate<Instr> 1000
    let instructions = Seq.map parseLine input
    let address = Seq.map fst instructions
    instructions |> Seq.iter (fun (addr, instr) -> program.[addr] <- instr)
    Array.ofSeq address, program

let evaluate (address: int array) (program: Program): int64 =

    let rec go (addr:int) : int64 =
        match program.[addr] with
        | Add address -> Seq.map go address |> Seq.sum
        | Mult address -> Seq.map go address |> Seq.reduce (*)
        | Value x -> x

    go (Array.head address)

let evaluateFast (addrs: int array) (program:Program) : int64 = 
   
   let tryGetValue(addr: int) : int64 option =
       match program.[addr] with
       | Value x -> Some x
       | _ -> None

   let traverse(addrs: int seq) : int64 seq option = 
       let values = Seq.map tryGetValue addrs
       if Seq.forall Option.isSome values then Some (Seq.choose id values) else None

   let reduce (instr: Instr) : int64 option = 
       match instr with
       | Add addrs -> traverse addrs |> Option.map Seq.sum
       | Mult addrs -> traverse addrs |> Option.map (Seq.reduce (*))
       | Value x -> Some x

   let pass () =
       for addr in addrs do
           reduce program.[addr] |> Option.iter (Value >> Array.set program addr)
    
   let startAddr = Array.head addrs
   
   while not (EvaluateInstr.isSimplifiedForm program.[startAddr]) do pass()

   let (Value x) = program.[startAddr]

   x

[<EntryPoint>]
let main argv =
    
    let lines = File.ReadLines(@"C:\Users\Liam\Downloads\Instruction Evaluator Exercise\input.txt")
    let addrs, program = parseProgram lines

    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let result = evaluate addrs program
    stopWatch.Stop()
    printfn "Time Taken (Seconds): %f" stopWatch.Elapsed.TotalSeconds
    printfn "Result: %i" result

    stopWatch.Reset()
    stopWatch.Start()
    let result = evaluateFast addrs program
    stopWatch.Stop()
    printfn "Time Taken (Seconds): %f" stopWatch.Elapsed.TotalSeconds
    printfn "Result: %i" result
    0