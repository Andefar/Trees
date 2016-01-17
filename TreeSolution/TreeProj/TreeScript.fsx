let guardian = @"C:\Users\Silas\Dropbox\5. Semester\02257 - Anvendt funktionsprogrammering\Project 2\GuardedCommands\GuardedCommands\GuardedCommands\"
let localPath = @"C:\Users\Silas\Dropbox\5. Semester\02257 - Anvendt funktionsprogrammering\Project 3\Trees\TreeSolution\TreeProj\"

#r @".\bin\Debug\FSharp.PowerPack.dll";

#load "AST.fs"
#load "Parser.fs"
#load "Lexer.fs"
#load "Util.fs"

open Tree
open AST
open Parser
open Lexer
open ParserUtil

System.IO.Directory.SetCurrentDirectory localPath;

type Tree<'a> = Node of 'a * (Tree<'a> list)
type Extent = (float * float) list

let rec parseToNode (P (decs,stms)) =  Node ("Program",[parseDecs decs;parseStms stms])
   
and parseDecs decs = Node("Decs",List.collect Dec decs)
and parseStms stms = Node("Stms",List.collect Stm stms)
and parseExps exps = Node("Exps",List.collect Exp exps)

and Exp = function
   | N i             -> [Node("Int",[Node(string(i),[])])]
   | B b             -> [Node("Bool",[Node(string(b),[])])]
   | Access acc      -> [Node("Access",Access acc)]
   | Apply (s,exps)  -> [Node("Apply: " + s,[parseExps exps])]
   | _               -> failwith "Project 2 doesnt support this" 

and Access = function
   | AVar s             -> [Node("Var: " + s,[])]
   | AIndex (acc,exp)   -> [Node("AIndex",Access acc@Exp exp)]
   | _                  -> failwith "Project 2 doesnt support this" 

and Dec = function
   | VarDec (t,s) -> [Node ("VarDec",[Node(s,[]);Node(tString t,[])])]
   | FunDec _     -> [Node ("FUNCTION",[])]

and Stm = function
   | PrintLn ex         -> [Node("PrintLn",[])]
   | Ass (acc,exp)      -> [Node("Ass",Access acc@Exp exp)]
   | Return (Some exp)  -> [Node("Return",Exp exp)]
   | Alt (GC gcs)       -> [Node("If",List.collect GC gcs)] // not done
   | Do (GC gcs)        -> [Node("While",List.collect GC gcs)] // not done
   | Block (decs,stms)  -> [Node("Block",[parseDecs decs;parseStms stms])]
   | Call (s,exps)      -> [Node("Procedure: " + s,[parseExps exps])]
   | _                  -> failwith "Not impl"


and GC (exp,stms)= [Node("GC",Exp exp @ [parseStms stms])] 


and tString = function 
   | ITyp                          -> "ITyp"
   | BTyp                          -> "BTyp"
   | PTyp _                        -> "PTyp"
   | ATyp(t,None)                  -> "ATyp"
   | ATyp(t,Some i)                -> "ATyp"
   | FTyp (ts,None)                -> "Proc" 
   | FTyp (ts,Some t)              -> "Func"


let rec movetree (Node((label,x:float),subtrees),y:float) = Node((label,x+y),subtrees)

and moveextent (e:Extent,x) = List.map (fun (p,q) -> (p+x,q+x)) e

and merge = function
   | ([],qs)               -> qs
   | (ps,[])               -> ps
   | ((p,_)::ps,(q,_)::qs) -> (p,q):: merge(ps,qs)

and mergelist es = List.fold (fun acc ex -> merge (acc,ex)) [] es

// type fit: Extent*Extent -> float
and fit = function
   | ((_:float,p)::ps,(q,_:float)::qs) -> System.Math.Max(fit(ps,qs), p - q + 1.0)
   | _                     -> 0.0 

and fitlist1 es = 
   let rec fitlist1' acc = function
   | [] -> []
   | (e::es) -> let x = fit(acc,e)
                x::fitlist1' (merge(acc,moveextent(e,x))) es
   fitlist1' [] es

and rev xs = List.fold (fun rs x -> x::rs) [] xs

and fitlistr es =
   let rec fitlistr' acc = function
      | [] -> []
      | (e::es) -> let x = -fit(e,acc)
                   x::fitlistr' (merge (moveextent (e,x),acc)) es
   rev (fitlistr' [] (rev es)) 

// alternative to fitlistr    
and flipextent ex = List.map (fun (p,q) -> (-q,-p)) ex
and fitlistr2 es = 
   rev es |> List.map flipextent |> fitlist1 |> List.map (-) |> rev

and mean (x:float,y:float) = (x+y)/2.0

// type fitlist: Extent list -> float list
and fitlist es = List.map mean (List.zip (fitlist1 es) (fitlistr es))

and design (tree:Tree<string>) = 
   let rec design' (Node (label,subtrees)) = 
      let (trees,extents) = List.unzip (List.map design' subtrees)
      let positions = fitlist extents
      let ptrees = List.map movetree (List.zip trees positions)
      let pextents = List.map moveextent (List.zip extents positions)
      let resultextent = (0.0,0.0)::(mergelist pextents)
      let resulttree = Node((label,0.0),ptrees)

      (resulttree,resultextent)

   fst (design' tree)

and reflect (Node (v,subtrees)) = Node (v,List.map reflect (rev subtrees))
and reflectpos (Node((v,x),subtrees)) = Node((v,-x),List.map reflectpos subtrees)

let file = "temp.gc" |> parseFromFile
let test = parseToNode file
let des = design test


