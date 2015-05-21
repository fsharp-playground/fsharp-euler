///Project Euler Problem 2
///Each new term in the Fibonacci sequence is generated by adding the previous two terms. 
///By starting with 1 and 2, the first 10 terms will be:
///
///                           1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
///
///Find the sum of all the even-valued terms in the sequence which do not exceed four million.
//-----------------------------------------------------------------------
// <copyright file="fib.fs" >
//     Copyright � Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
open System
open Euler

#nowarn "40"  
let rec fib = memoize(function
  | 1 -> 1
  | 2 -> 1
  | n -> fib(n-1) + fib(n-2))

let fibs =
    let rec fb n =
        seq {
            yield fib n
            yield! fb (n+1)
        }
    fb 2

let solution = fibs |> Seq.takeWhile((>=) 4000000 ) |> Seq.filter(fun fib -> fib % 2 = 0) |> Seq.sum
solution |> printfn "solution: %d"

Console.ReadKey(true) |> ignore


