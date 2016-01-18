﻿module ParseToNode

open Tree.AST

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
   | PrintLn ex         -> [Node("PrintLn",Exp ex)]
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