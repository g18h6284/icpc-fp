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
    | x::rest -> match x with 
                 | ',' -> match rest with // if there's a comma, then make sure it's followed by a single space
                          | x::rest -> match x with
                                       | ' ' -> match rest with 
                                                | x::rest -> match x with
                                                             | 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' ->
                                                                checkSpacing rest
                                                             | _ -> false
                                                | [] -> false
                                       | _ -> false
                          | [] -> false
                 | '.' -> match rest with  // if there's a full stop, then make sure it's followed by a single space
                          | x::rest -> match x with
                                       | ' ' -> match rest with 
                                                | x::rest -> match x with
                                                             | 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' ->
                                                                checkSpacing rest
                                                             | _ -> false
                                                | [] -> false
                                       | _ -> false
                          | [] -> false
                 | ' ' -> match rest with  // if there's a space, then make sure it's only a single space
                          | x::rest -> match x with
                                       | 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' ->
                                            checkSpacing rest
                                       | _ -> false
                          | [] -> false
                 | _ -> checkSpacing rest
    | [] -> true

let rec checkFullStop input = 
    match input with 
    | x::rest -> match rest with
                 | [] -> match x with
                         | '.' -> true
                         | _ -> false
                 | _ -> checkFullStop rest 
    | [] -> false

let checkValidInput input = 
    match String.length input >= 2 with
    | false -> None
    | true ->   let a = Seq.toList input
                match checkChars a with
                | false -> None
                | true -> match checkFirst a with 
                          | false -> None
                          | true -> match checkSpacing a with
                                    | false -> None
                                    | true -> match checkFullStop a with
                                              | false -> None
                                              | true -> Some(input)
                                                 
let rec getWord input word =
    match input with
    |x::rest ->  match x with
                 | 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' ->
                    let word = x::word
                    getWord rest word
                 | _ -> (List.toSeq (List.rev word)), x::rest
    |[] -> (List.toSeq (List.rev word)), []
    
let rec getPunc input = 
    match input with
    | x::a::rest -> match x, a with  
                    | (','|'.'), ' ' -> let punc = x::a::[]
                                        (List.toSeq punc), rest
                    | ' ', _ -> let punc = x::[]
                                (List.toSeq punc), a::rest
                    | _ -> getPunc (a::rest)
    | x::rest ->    match x, rest with 
                    | '.', [] ->    let punc = x::[]
                                    (List.toSeq punc), rest
                    | _ -> getPunc rest
    | [] -> (List.toSeq []), []
    
let rec buildSeq input list=
    match input with
    | x::rest ->    match x with
                    | 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z'->
                        let word, tale = getWord input []
                        let word = new string(Seq.toArray word)
                        let list = (word)::list
                        buildSeq tale list
                    | _ ->  let punc, tale = getPunc input
                            let punc = new string(Seq.toArray punc)
                            let list = punc::list
                            buildSeq tale list
    |[] -> List.rev list

let rec removeFirstComma screened items =
    match items with
    | x::rest -> match x with
                 | ", "-> screened@rest
                 | _ -> removeFirstComma (screened@[x]) rest
    | [] -> List.rev screened  

let rec removeSecondComma screened (items: string list) =
    match items with
    | x::a::rest -> match x.[(String.length x)-1], a.[0] with
                    | ' ', ',' -> screened@x::rest
                    | _ -> removeSecondComma (screened@[x]) (a::rest)
    | x::rest -> removeSecondComma (screened@[x]) rest
    | [] -> List.rev screened  

let rec removeFirstSpace screened (items: string list) =
    match items with
    | [x] ->  screened@[x]
    | x::a::rest -> match x, (a.[0]) with
                    | " ", ','-> (screened@a::rest) 
                    | _ -> removeFirstSpace (screened@[x]) (a::rest)
    | [] -> screened  
    
let rec removeAllSpaces (input: string list) =
    match input with
    | x::a::rest -> match x.[String.length x-1], (a.[0]) with
                    | ' ', ',' -> let next = removeFirstSpace [] input
                                  removeAllSpaces next
                    | _ -> x::(removeAllSpaces (a::rest))
    | x::rest -> x::(removeAllSpaces (rest))
    | _ -> []               

//RULE ONE
let rec findRuleOneComma input =
    match input with
    | x::a::rest -> match x, a with
                    | ", ", a ->  let next = List.mapi (fun i b -> match (i <> 0) && (b=a) && (a.[0] <> ',') with
                                                                   | true -> ", " + b
                                                                   | false -> b) (" "::[a]@rest)
                                  findRuleOneComma next
                    | _ -> x::(findRuleOneComma (a::rest))
    | x::rest -> x::(findRuleOneComma (rest))
    | _ -> []

//RULE TWO
let rec findRuleTwoComma (input: string list) =
    match input with
    | x::a::rest -> match x, a, rest with
                    | x, ", ", rest -> let next = List.map (fun b -> match ((b=x) && (rest.[0] <> (".") && rest.[0] <> (","))) with
                                                                     | true -> b + ", "
                                                                     | false -> b) ([x]@rest)
                                       let next = removeSecondComma [] next
                                       next
                    | _ -> x::findRuleTwoComma (a::rest)
    | x::rest -> x::(findRuleTwoComma (rest))
    | [] -> [] 

let rec buildString list = 
    match list with
    | [] -> "" 
    | head::tail -> head + (buildString tail)

let ruleOne input =
    let s = Seq.toList input
    let s = buildSeq s []
    let s = findRuleOneComma s
    //let s = removeAllSpaces s
    //let s = buildString s
    s

let ruleTwo input = 
    let s = Seq.toList input
    let s = findRuleTwoComma (buildSeq s [])
    //let s = removeAllSpaces s
    let s = buildString s
    s

//"one, two two two three blue, three one two. four. blue five."|> ruleOne |> ruleTwo;;
//"one, two, two, two, three blue, three one, two. four. blue, five."
                                         

let commaSprinkler input = //failwith "Not implemented"
    checkValidInput input

let rivers input =
    failwith "Not implemented"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
