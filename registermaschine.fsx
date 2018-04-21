#nowarn "40"

let maxSteps = Some 100 //set to None to allow infinite loops

type Result = Bottom | Memory of int array

type Op =
   | Inc of int * Lazy<Op>
   | Dec of int * Lazy<Op>
   | Jeqz of int * Lazy<Op> * Lazy<Op>
   | Halt

let adder =
   let rec a = Jeqz (0, lazy d, lazy b)
      and b = Dec (0, lazy c)
      and c = Inc (2, lazy a)
      and d = Jeqz (1, lazy g, lazy e)
      and e = Dec (1, lazy f)
      and f = Inc (2, lazy d)
      and g = Halt
   in a

let subtractor =
   let rec a = Jeqz (1, lazy e, lazy b)
      and b = Jeqz (0, lazy b, lazy c)
      and c = Dec (0, lazy d)
      and d = Dec (1, lazy a)
      and e = Jeqz (0, lazy h, lazy f)
      and f = Dec (0, lazy g)
      and g = Inc (2, lazy e)
      and h = Halt
   in a

let rec eval op steps (memory : int array ref) =
   let bottom = begin
      match maxSteps with
      | None -> false
      | Some number -> steps > number
   end
   if bottom then Bottom
   else
      let grow N = (* grow memory array if needed *)
         let n = (!memory).Length
         if N >= n
         then memory := Array.append !memory (Array.zeroCreate (N + 1 - n))
      match op with
         | Inc (register, next) -> 
            grow register
            (!memory).[register] <- (!memory).[register] + 1
            eval next.Value (steps + 1) memory
         | Dec (register, next) ->
            grow register
            (!memory).[register] <- max ((!memory).[register] - 1) 0
            eval next.Value (steps + 1) memory
         | Jeqz (register, ifZero, ifNonZero) ->
            grow register
            let next = if (!memory).[register] = 0 then ifZero else ifNonZero
            eval next.Value (steps + 1) memory
         | Halt -> Memory !memory

ref [|1;5;7|] |>  eval adder 0 |> printfn "%A"
ref [|8;3|] |>  eval subtractor 0 |> printfn "%A"
ref [|3;8|] |>  eval subtractor 0 |> printfn "%A"
