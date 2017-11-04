module Extras =
    open System

    let duration problem f = 
        let timer = new System.Diagnostics.Stopwatch()
        printfn "\nStarted %i" problem
        timer.Start()
        let returnValue = f()
        printfn "Elapsed Time: %s ms" (String.Format ("{0:#,0}", timer.ElapsedMilliseconds))
        returnValue

    let primesUpTo (n: uint64) =
        let primes =  ref [2UL]
        let anyDivisibleBy (i: uint64) foundPrimes = 
            not (List.exists (fun (y: uint64) -> y <> 1UL && i % y = 0UL) !primes)

        seq {yield 2UL; 
             for i in  3UL .. 2UL .. n do
                if anyDivisibleBy i !primes then 
                    primes := List.append !primes [i]
                    yield i }

    let primeFactorization (n: uint64) =
        let top = (float >> sqrt >> uint64) n

        primesUpTo top
        |> Seq.filter (fun prime -> n % prime = 0UL) 


module Problem1 =
    let solution top =
        seq {1 .. (top - 1)}
        |> Seq.filter (fun x -> x % 3 = 0 || x % 5 = 0)
        |> Seq.sum

    printfn "Total is %d" (solution 1000)


module Problem2 =
    open System
    let solution top =
        seq {
            for i in 1..2 do yield i
            let currentPair = ref [1; 2]
            while (!currentPair).Item 1 < top do
                match !currentPair with
                | [first; second] -> let newSecond = first + second
                                     currentPair := [second; newSecond]
                                     yield newSecond
                | _ -> printfn "Not supposed to happen"}
        |> Seq.filter (fun n -> n % 2 = 0)
        |> Seq.sum

    let answer = solution 4000000
    let formatted =  String.Format("{0:#,0}", answer)
    printfn "Total is %s" formatted


module Problem3 = 
    let largestPrimeFactor (n: uint64) =
        Extras.primeFactorization n
        |> Seq.max

    let result = Extras.duration 3 (fun _ -> largestPrimeFactor 15UL) //600851475143UL)
    printfn "Largest factor? %d" result 


module Problem4 =
    let isPalindrome (asString: string) =
        let asList = [for c in asString -> c]
        asList = List.rev asList

    let largestPalindromeProduct digits =
        let magnitude = float digits
        let range = seq {(10.0 ** magnitude) - 1.0 .. -1.0 .. (10.0 ** (magnitude - 1.0))}
                    |> Seq.map int

        seq { for i in range do 
                for j in range do 
                    yield (i, j, i * j) }
        |> Seq.filter (fun (_, _, product) -> isPalindrome (string product))
        |> Seq.maxBy (fun (_, _, product) -> product)

    let result = Extras.duration 4 (fun _ -> largestPalindromeProduct 3)
    printfn "%A" result


module Problem5 =
    open System
    let areAllMultiples i range = 
        Seq.forall (fun divisor -> i % divisor = 0UL) range

    let rec removeUnnecessaryMultiples = function
        | first :: last -> let withoutMultiples = List.filter (fun x -> first % x <> 0) last
                           List.append [first] (removeUnnecessaryMultiples withoutMultiples)
        | _ -> []

    let smallestMultiple top = 
        let range = removeUnnecessaryMultiples [ top .. -1 .. 1 ]
        let necessaryMultiples = removeUnnecessaryMultiples range
                                 |> List.map uint64
        seq {uint64 top .. uint64 top .. UInt64.MaxValue}
        |> Seq.tryFind (fun x -> Seq.forall (fun (y: uint64) -> x % y = 0UL) necessaryMultiples)

    let result = Extras.duration 5 (fun x -> (smallestMultiple 20).Value)
    printfn "Smallest multiple of 1 .. 40 is %d" result

module Problem6 =
    let intSquare n =
        (float n) ** 2.0
        |> uint64

    let sumOfSquares numbers = 
        numbers
        |> Seq.map intSquare
        |> Seq.sum

    let squareOfSum numbers =
        numbers
        |> Seq.map uint64
        |> Seq.sum
        |> intSquare

    let difference numbers = squareOfSum numbers - sumOfSquares numbers

    let result = Extras.duration 6 (fun _ -> difference [1UL .. 100UL])
    printfn "Sum square difference %i" result


module Problem7 =
    open System

    let nthPrime n =
        Extras.primesUpTo UInt64.MaxValue
        |> Seq.item (n - 1)

    let result = Extras.duration 7 (fun _ -> nthPrime 10001)
    printfn "%u" result


module Problem8 =
    let digitsToProduct =
        Seq.reduce (fun acc curr -> acc * curr)

    let formatSolution digits =
        (digits, digitsToProduct digits)

    let firstTransform =
        Array.map (string >> uint64)


    let largestProductIn n length =
        Seq.windowed length (string n)
        |> Seq.map firstTransform
        |> Seq.maxBy digitsToProduct
        |> formatSolution

    let rediculousN =  "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
    let result = Extras.duration 8 (fun _ -> largestProductIn rediculousN 13)
    printfn "%A" result


module Problem9 =
    let isPythagoreanTriple triple =
        let asFloats = List.map float triple
        match asFloats with
        | [a; b; c] when a ** 2.0 + b ** 2.0 = c ** 2.0 -> true
        | _ -> false

    let doesTripleMatchSum sum triple =
        match triple with
        | [a; b; c] -> a + b + c = sum
        | _ -> false
       

    let solution total = 
        seq {for a in [1 .. total] do
                for b in [a + 1 .. total - a] do
                    for c in [b .. total - b] do
                        yield [a; b; c]}
        |> Seq.filter (doesTripleMatchSum total)             
        |> Seq.filter isPythagoreanTriple
        |> Seq.tryFind (fun _ -> true)

    let result = Extras.duration 9 (fun _ ->  solution 25)
    match result with
    | Some(a) -> printfn "%A" a
    | _ -> printfn "Something's wrong..."