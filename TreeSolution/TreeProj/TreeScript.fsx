let guardian = @"C:\Users\Silas\Dropbox\5. Semester\02257 - Anvendt funktionsprogrammering\Project 2\GuardedCommands\GuardedCommands\GuardedCommands\"
let localPath = @"C:\Users\Silas\Dropbox\5. Semester\02257 - Anvendt funktionsprogrammering\Project 3\Trees\TreeSolution\TreeProj\"

#r @".\bin\Debug\FSharp.PowerPack.dll";

#load "AST.fs"
#load "Parser.fs"
#load "Lexer.fs"
#load "Util.fs"

open Tree.Util
open Tree.AST
open Parser
open Lexer
open ParserUtil

System.IO.Directory.SetCurrentDirectory localPath;

let file = "temp.gc" |> parseFromFile

type Tree<'a> = Node of 'a * (Tree<'a> list)

let rec parseToNode (P (decs,stms)) =  Node ("Program",[parseDecs decs;parseStms stms])
   
   and parseDecs decs = Node("Decs",List.collect parseDec decs)
   and parseStms stms = Node("Stms",List.collect parseStm stms)

   and parseDec = function
      | VarDec (t,s) -> [Node ("VarDec",[Node(s,[]);Node(tString t,[])])]
      | FunDec _     -> [Node ("FUNCTION",[])]

   and parseStm = function
      | PrintLn ex -> [Node("PrintLn",[])]
      | Ass _      -> [Node("Ass",[])]
      | Return _     -> [Node("Return",[])]
      | Alt _ -> [Node("If",[])]
      | Do _ -> [Node("While",[])]
      | Block (decs,stms) -> [Node("Block",[parseDecs decs;parseStms stms])]
      | Call _ -> [Node("Call",[])]

   and tString = function 
      | ITyp                          -> "ITyp"
      | BTyp                          -> "BTyp"
      | PTyp _                        -> "PTyp"
      | ATyp(t,None)                  -> "ATyp"
      | ATyp(t,Some i)                -> "ATyp"
      | FTyp (ts,None)                -> "Proc" 
      | FTyp (ts,Some t)              -> "Func"

let test = parseToNode file

type Extent = (float * float) list

let n1 = Node ("1",[Node("2",[Node("3",[]);Node("4",[])]);Node("5",[]);Node("6",[Node("7",[]);Node("8",[])])])
let a = ("Rod",[])

let movetree (Node((label,x:float),subtrees),y:float) = Node((label,x+y),subtrees)

let moveextent (e:Extent,x) = List.map (fun (p,q) -> (p+x,q+x)) e

let rec merge = function
   | ([],qs)               -> qs
   | (ps,[])               -> ps
   | ((p,_)::ps,(q,_)::qs) -> (p,q):: merge(ps,qs)

let mergelist es = List.fold (fun acc ex -> merge (acc,ex)) [] es

//let b = [(-5.0,-3.0);(-7.0,-3.0)]
//let c = [(2.0,4.0);(3.0,7.0)]
//let ls = [b;c]
//mergelist ls

// type fit: Extent*Extent -> float
let rec fit = function
   | ((_:float,p)::ps,(q,_:float)::qs) -> System.Math.Max(fit(ps,qs), p - q + 1.0)
   | _                     -> 0.0 

let fitlist1 es = 
   let rec fitlist1' acc = function
   | [] -> []
   | (e::es) -> let x = fit(acc,e)
                x::fitlist1' (merge(acc,moveextent(e,x))) es
   fitlist1' [] es

let rev xs = List.fold (fun rs x -> x::rs) [] xs;;

let fitlistr es =
   let rec fitlistr' acc = function
      | [] -> []
      | (e::es) -> let x = -fit(e,acc)
                   x::fitlistr' (merge (moveextent (e,x),acc)) es
   rev (fitlistr' [] (rev es)) 

// alternative to fitlistr    
let flipextent ex = List.map (fun (p,q) -> (-q,-p)) ex
let fitlistr2 es = 
   rev es |> List.map flipextent |> fitlist1 |> List.map (-) |> rev

let mean (x:float,y:float) = (x+y)/2.0

// type fitlist: Extent list -> float list
let fitlist es = List.map mean (List.zip (fitlist1 es) (fitlistr es))

let design tree = 
   let rec design' (Node (label,subtrees)) = 
      let (trees,extents) = List.unzip (List.map design' subtrees)
      let positions = fitlist extents
      let ptrees = List.map movetree (List.zip trees positions)
      let pextents = List.map moveextent (List.zip extents positions)
      let resultextent = (0.0,0.0)::(mergelist pextents)
      let resulttree = Node((label,0.0),ptrees)

      (resulttree,resultextent)

   fst (design' tree)

let rec reflect (Node (v,subtrees)) = Node (v,List.map reflect (rev subtrees))
let rec reflectpos (Node((v,x),subtrees)) = Node((v,-x),List.map reflectpos subtrees)


design test