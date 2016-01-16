type Tree<'a> = Node of 'a * (Tree<'a> list)
type Extent = (float * float) list

let a = ("Rod",[])

let movetree (Node((label,x),subtrees),y) = Node((label,x+y),subtrees)

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

let rec fit = function
   | ((p,_)::ps,(q,_)::qs) -> System.Math.Max(fit(ps,qs), p - q + 1.0)
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

let mean (x,y) = (x+y)/2.0

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
