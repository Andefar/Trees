module Design

open Translate


let rec movetree (Node((label,x:float),subtrees),y:float) = Node((label,x+y),subtrees)

and moveextent (e:Extent,x) = List.map (fun (p,q) -> (p+x,q+x)) e

and merge = function
   | ([],qs)               -> qs
   | (ps,[])               -> ps
   | ((p,_)::ps,(_,q)::qs) -> (p,q):: merge(ps,qs)

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