module ParseToNode

open Tree.AST

type Tree<'a> = Node of 'a * (Tree<'a> list)
type Extent = (float * float) list

let rec parseToNode (P (decs,stms)) =  Node ("Program",[parseDecs decs;parseStms stms])
   
and parseDecs decs   = Node("Decs",List.map Dec decs)
and parseParams pms  = Node("Params",List.map Dec pms)
and parseStms stms   = Node("Stms",List.map Stm stms)
and parseExps exps   = Node("Exps",List.map Exp exps)

and Exp = function
   | N i                -> Node("Int",[Node(string(i),[])])
   | B b                -> Node("Bool",[Node(string(b),[])])
   | Access acc         -> Access acc
   | Apply (s,[e])      -> Node("Apply", [Node(s,[]);Exp e])
   | Apply (s,[e1;e2])  -> Node("Apply",[Exp e1;Node(s,[]);Exp e2])
   | Apply (s,exps)     -> Node("Apply",[Node(s,[]);parseExps exps])
   | _                  -> failwith "Project 2 doesnt support this" 

and Access = function 
   | AVar s             -> Node("Var",[Node(s,[])])
   | AIndex (acc,exp)   -> Node("Array",[Access acc;Exp exp])
   | _                  -> failwith "Project 2 doesnt support this" 

and Dec = function
   | VarDec (t,s)                   -> Node ("VarDec",[Node(s,[]);Node(tString t,[])])
   | FunDec (Some t,s,pms,stm)      -> Node ("Function",[Node(s,[]);Node(tString t,[]);parseParams pms;Stm stm])
   | FunDec (None,s,pms,stm)        -> Node ("Procedure",[Node(s,[]);parseParams pms;Stm stm])

and Stm = function 
   | PrintLn ex         -> Node("PrintLn",[Exp ex])
   | Ass (acc,exp)      -> Node("Assign",[Access acc;Exp exp])
   | Return (Some exp)  -> Node("Return",[Exp exp])
   | Alt (GC gcs)       -> Node("If",List.map GC gcs) 
   | Do (GC gcs)        -> Node("While",List.map GC gcs) 
   | Block (decs,stms)  -> Node("Block",[parseDecs decs;parseStms stms])
   | Call (s,exps)      -> Node("Procedure: " + s,[parseExps exps])
   | _                  -> failwith "Project 2 doesnt support this"

and GC (exp,stms)= Node("GC",[Exp exp;parseStms stms])

and tString = function 
   | ITyp                          -> "ITyp"
   | BTyp                          -> "BTyp"
   | PTyp _                        -> "PTyp"
   | ATyp(t,None)                  -> "ATyp"
   | ATyp(t,Some i)                -> "ATyp"
   | FTyp (ts,None)                -> "Proc" 
   | FTyp (ts,Some t)              -> "Func"