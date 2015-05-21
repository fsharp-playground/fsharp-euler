///Project Euler http://projecteuler.net
///Solutions to problems 01 to 50
//-----------------------------------------------------------------------
// <copyright file="Euler.fs" >
//     Copyright © Cesar Mendoza. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------
module Euler

open System
open System.Collections.Generic
open Primegen

let primes = 
    seq { 
        let primes = new Primes()
        primes.skipto (2UL)
        while true do
            yield primes.next()
    }

//this function came from here: http://geekswithblogs.net/Erik/archive/2008/02/19/119734.aspx
let primeFactors (num : float) = 
    let get num2 = 
        let sq = Math.Sqrt(num2)
        let div = ref 2.0
        while ((not (num2 % !div = 0.0)) && (!div < sq)) do
            if (!div = 2.0) then div := !div + 1.0
            else div := !div + 2.0
        div
    
    let divSeq = 
        num |> Seq.unfold (fun x -> 
                   let sq = Math.Sqrt(x)
                   let divisor = get x
                   if (x = 1.0) then None
                   else if (sq < !divisor) then Some(x, 1.0) // x is prime! 
                   else Some(!divisor, x / (!divisor)))
    
    divSeq |> Seq.countBy id

let memoize f = 
    let cache = Dictionary<_, _>()
    fun x -> 
        let ok, res = cache.TryGetValue(x)
        if ok then res
        else 
            let res = f x
            cache.[x] <- res
            res

// test 
#nowarn "40"

open System
open System.Collections.Generic

let __testMemoize() = 

    let mem f = 
        let cache = Dictionary<_, _>()
        fun x -> 
            let ok, res = cache.TryGetValue(x)
            if ok then res
            else 
                let res = f x
                cache.[x] <- res
                res
    
    // modify
    let rec _fib = 
        mem ( fun x -> 
            x |> printfn "%d"
            match x with
            | 1 -> 1
            | 2 -> 1
            | k -> _fib (k - 1) + _fib (k - 2))
    
    let fibs = 
        let rec fb n = 
            seq { 
                yield _fib n
                yield! fb (n + 1)
            }
        fb 2
    
    let solution = 
        fibs
        |> Seq.takeWhile ((>=) 40)
        |> Seq.filter (fun fib -> fib % 2 = 0)
        |> Seq.sum
    
    solution |> printfn "%d"
    ()

let __test() = 
    // original
    let rec fib = 
        memoize (function 
            | 1 -> 1
            | 2 -> 1
            | n -> fib (n - 1) + fib (n - 2))
    
    let fibs = 
        let rec fb n = 
            n |> printfn "%d"
            seq { 
                yield fib n
                yield! fb (n + 1)
            }
        fb 2
    
    let solution = 
        fibs
        |> Seq.takeWhile ((>=) 4000000)
        |> Seq.filter (fun fib -> fib % 2 = 0)
        |> Seq.sum
    
    solution |> printfn "solution: %d"
    let _fibs = 
        let rec fb n = 
            seq { 
                yield n
                yield! fb (n + 1)
            }
        fb 1
    // arithmetic operation resulted in an overflow
    _fibs
    |> Seq.takeWhile ((>=) 10)
    |> Seq.sum
    |> printfn "%d"
    _fibs
    |> Seq.takeWhile ((>=) 40000000)
    |> Seq.filter (fun fib -> fib % 2 = 0)
    |> Seq.sum
    |> printfn "%d"
