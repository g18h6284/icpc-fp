module ICPC
open System

let rec checkChars input =
    match input with
    | [] -> true
    | x::rest -> match x with
                 | 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z'|' '|','|'.' ->
                    checkChars rest 
                 | _ -> false

let checkFirst input =
    match input with
    | x::rest -> match x with 
                 |  'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' ->
                    true
                 | _ -> false
    | [] -> false

let rec checkSpacing input =
    match input with 
    | x::a::rest -> match x, a with   
                    | ',', ' ' -> checkSpacing (a::rest)//this is valid  
                    | ',', _ -> false
                    | '.', ' ' -> checkSpacing (a::rest)//this is valid 
                    | ' ', ' ' -> false // if there's a space, then make sure it's only a single space
                    | ' ', '.' -> false //no spaces before the '.'
                    | ' ', _ -> checkSpacing (a::rest)
                    | '.', '.' -> false //only single '.'
                    | _ -> checkSpacing (a::rest) //no error cases... must be true
    | x::rest-> match x with 
                | '.' -> checkSpacing rest //string ends with full stop
                | _ -> false
    | [] -> true
    //"one, two... one tree."|>buildCharLists|>checkSpacing;;

let checkValidInput input = 
    match List.length input >= 2 with
    | false -> None
    | true ->   let a = Seq.toList input
                match checkChars a with
                | false -> None
                | true -> match checkFirst a with 
                          | false -> None
                          | true -> match checkSpacing a with
                                    | false -> None
                                    | true -> Some(input)
                                              
let buildCharLists (input: string) = //words = [ ['O';'n';'e';','];  ['t';'w';'o';','];  ['t';'h';'r';'e';'e';'.'] ]
  let wordList = Seq.toList(input.Split [|' '|])
  let words = List.map (Seq.toList) wordList//Seq.toList wordList
  let b = Seq.toList input
  words
  //b

let rec buildRuleOneWordList input wordList =
    match input with    
    | [] -> wordList
    | x::a::rest -> match (x|>List.rev|>List.head) with 
                    | (',') ->  match List.rev a with
                                | p::tale -> match p with
                                             | (','|'.') -> buildRuleOneWordList (a::rest) ((List.rev (tale)::wordList))
                                             | _ -> buildRuleOneWordList (a::rest) (a::wordList)
                                | _ -> []                                    
                    | _ -> buildRuleOneWordList (a::rest) wordList
    | x::rest -> buildRuleOneWordList rest wordList

//
let rec findRuleOneComma (input: char list list) wordList =
    match wordList with
    | [] -> input
    | x::rest-> let input = List.mapi (fun i a -> match i with 
                                                  | 0 -> a
                                                  | _ -> let edit = 
                                                             match (a|>List.rev) with
                                                                |p::tale-> match p with
                                                                           |(','|'.')-> List.rev tale
                                                                           | _ -> List.rev (p::tale)
                                                                | [] -> []
                                                         let prev =input.[i-1]
                                                         match (edit = x) && ((prev.[(List.length (prev))-1]) <> ',') && ((prev.[(List.length (prev))-1]) <> '.') with
                                                         | true -> ','::' '::a
                                                         | false -> a ) input
                (*let input = List.choose (fun elem -> match (elem = x) with
                                                     | true -> Some(','::' '::elem)
                                                     | false -> Some(elem)) input *)
                findRuleOneComma input rest

//
let rec concatWspace2 (input:char list list) =
    match input with
    | [] -> []
    | x::rest -> match x.[(List.length x)-1], rest with
                 |'.', [] -> x:: (concatWspace2 rest)
                 |',', [[' ']] -> x:: (concatWspace2 rest) //|',', _ -> x:: (concatWspace rest)
                 | _ -> x::[' ']:: (concatWspace2 rest)
                 
let rec concatWspace1 (input:char list list) =
    match input with
    | [] -> []
    | x::rest -> match x.[(List.length x)-1], rest with
                 |'.', [] -> x:: (concatWspace1 rest)
                 |',', _ -> x::[' ']::(concatWspace1 rest) //|',', _ -> x:: (concatWspace rest)
                 | _ -> x::[' ']:: (concatWspace1 rest)

let rec removeFirstDoubleComma screened (items: char list) =
    match items with
    | [x] ->  screened@[x]
    | x::a::rest -> match x, a with
                    | ',', ','-> (screened@a::rest) 
                    | ' ', ',' -> (screened@a::rest)
                    | '.', ',' -> (screened@x::rest)
                    | _ -> removeFirstDoubleComma (screened@[x]) (a::rest)
    | [] -> screened  
  
let rec removeAllDoubleComma (input: char list) =
    match input with
    | x::a::rest -> match x, a with
                    | ',', ',' -> let next = removeFirstDoubleComma [] input
                                  removeAllDoubleComma next
                    | ' ', ',' -> let next = removeFirstDoubleComma [] input
                                  removeAllDoubleComma next
                    | '.', ',' -> let next = removeFirstDoubleComma [] input
                                  removeAllDoubleComma next
                    | _ -> x::(removeAllDoubleComma (a::rest))
    | x::rest -> x::(removeAllDoubleComma (rest))
    | _ -> [] 

let ruleOne input =
    let s = buildCharLists input
    let wordList = buildRuleOneWordList s []
    let s = findRuleOneComma s wordList
    let s = concatWspace1 s
    let s = List.concat s
    let s = removeAllDoubleComma s
    let s = System.String.Concat(Array.ofList(s))
    printf "%A" wordList
    s

let rec buildRuleTwoWordList (input:char list list) wordList =
    match input with    
    | [] -> wordList
    | x::rest -> match x|>List.rev with
                 | ','::tale -> let word = List.rev tale
                                buildRuleTwoWordList rest (word::wordList)
                 | _ -> buildRuleTwoWordList rest wordList

let rec findRuleTwoComma (input: char list list) wordList =
    match wordList with
    | [] -> input
    | x::rest -> let input = List.choose (fun elem -> match elem = x with
                                                      | true -> Some(elem@','::[])
                                                      | false -> Some(elem)) input 
                 findRuleTwoComma input rest

let ruleTwo input = 
    let s = buildCharLists input
    let wordList = buildRuleTwoWordList s []
    let s = findRuleTwoComma s wordList
    let s = concatWspace2 s
    let s = List.concat s
    let s = removeAllDoubleComma s
    let s = System.String.Concat(Array.ofList(s))
    printf "%A" wordList
    s

//"one, two two two three blue, three one two. four. blue five."|> ruleOne |> ruleTwo;;
//"one, two, two, two, three blue, three one, two. four. blue, five."
//['o'; 'n'; 'e'; ',']; ['t'; 'w'; 'o']; [','; ' '; 't'; 'w'; 'o'];[','; ' '; 't'; 'w'; 'o']; [1't'; 'h'; 'r'; 'e'; 'e'];['b'; 'l'; 'u'; 'e'; ',']; ['t'; 'h'; 'r'; 'e'; 'e']; ['o'; 'n'; 'e'];['t'; 'w'; 'o'; '.']; ['f'; 'o'; 'u'; 'r'; '.']; ['b'; 'l'; 'u'; 'e'];['f'; 'i'; 'v'; 'e'; '.']]
//"please, sit spot. sit spot, sit. spot, here now, here."                                         

let commaSprinkler input = //failwith "Not implemented"
    let b = Seq.toList input
    match checkValidInput b with
    | Some(x) -> let s = input|>ruleOne|>ruleTwo|>ruleOne|>ruleTwo|>ruleOne|>ruleTwo
                 Some(s)
    | None -> None

//input validation
let rec checkCharsRivers input = 
    match input = Seq.toList "" with
    | true -> false
    | false -> match input with
               | x::rest ->  match x with
                             | 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z'|
                               'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'|
                               ' ' -> checkCharsRivers rest
                             | _ -> false
               | [] -> true


let rec checkSingleSpaceRivers input = 
    match input with
    | x::a::rest ->  match x, a, rest.[0] with
                     | _, ' ', ' ' -> false
                     | _, ' ', _ -> checkSingleSpaceRivers rest
                     | _ -> checkSingleSpaceRivers (a::rest)
    | x::rest -> checkSingleSpaceRivers []
    | [] -> true

let rec checkWordLength input count =
    match input with
    | x::rest -> match x, count>80 with
                 | _, true -> false
                 | ' ', false -> checkWordLength rest 0
                 | _, false -> checkWordLength rest (count+1)
    | [] -> true

let rec countWords input count =
    match input with
    |x::a::rest -> match x, a with
                   | ' ', _ -> countWords (a::rest) (count+1)
                   | _ -> countWords (a::rest) count
    | x::rest -> countWords rest count
    | [] -> count

let checkValidRiver input =
    let s = Seq.toList input
    match checkCharsRivers s with
    | false -> None
    | true -> match checkSingleSpaceRivers s with
              | false -> None
              | true -> match checkWordLength s 0 with
                        | false -> None
                        | true -> match ((countWords s 0) >= 2) with
                                  | false -> None
                                  | true -> Some(s)
                                  
let rec buildRiverWordsList input word (wordList: char list list) =
    match input with
    | x::rest ->    match x with
                    | ' ' -> let word = List.rev word
                             let wordList = word::wordList
                             let word = []
                             buildRiverWordsList rest word wordList
                    | _ ->  let word = x::word
                            buildRiverWordsList rest word wordList
    | [] -> let word = List.rev word
            let wordList = word::wordList
            List.rev wordList

let rec getLongestWord input max =
    match input with
    | x::rest -> let length = List.length x
                 match length > max with
                 | true ->  let max = length
                            getLongestWord rest max
                 | false -> getLongestWord rest max
    | [] -> max

//
let rec getLines (input:char list list) (line:char list list) (lines: char list list list) lineWidth remainingLine =
    match input with
    |x::rest -> match ((List.length x)) <= remainingLine-1 with
                |true -> let x = x@[' ']
                         let line = x::line
                         printf "True \n %A \n" line
                         let remainingLine = remainingLine - (List.length x)
                         getLines rest line lines lineWidth remainingLine
                |false -> 
                          let lines = [line]@lines
                          printf "False \n %A \n" lines
                          let line = []
                          let x = x@[' ']
                          let line = x::line
                          let remainingLine = lineWidth - (List.length x)
                          getLines rest line lines lineWidth remainingLine
    | [] -> 
            let lines = [line]@lines
            lines   

//
let rec charLine (input:char list list) (output:char list) =
    match input with
    |x::rest -> charLine rest (x@output)
    |[] ->  output

//
let rec joinLines (input: char list list list) (result: char list list) =
    match input with 
    |x::rest -> let line = charLine x []
                joinLines rest (line::result)
    |[] ->  result 

//
let rec buildRiverLineList input lineWidth remainingLine =
    match lineWidth with
    | 0 -> let lineWidth = (getLongestWord input 0)+1
           let remainingLine = lineWidth
           buildRiverLineList input lineWidth remainingLine
    | _ -> let lines = getLines input [] [] lineWidth remainingLine
           lines  

let formatRivers input = 
    let s = Seq.toList input
    let s = buildRiverWordsList s [] []
    let s = buildRiverLineList s 0 0
    let s = joinLines s []
    //this is the stage we have gotten up to and we will come back to visit in the future 
    //possibly when we've forgottrn what the code does
    //these are our saving and cursing comments, we are done. for now...
    s
//"one two three seveneleven this is not over twelve"|> formatRivers;;


let rivers input = //failwith "Not implemented"
    match checkValidRiver input with
    | None -> None
    | Some(x) -> Some(x)//Some(line width of string with longest possible river; Longest river)

[<EntryPoint>]
let main argv =
    printf "%s" "Hello World from F#"
    //printf "%s" "(commaSprinkler "one, two two two three blue, three one two. four. blue five.")"
    0 // return an integer exit code
