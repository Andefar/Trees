﻿
module Draw

open System.IO

type Tree<'a> = Node of (string*float) * (Tree<'a> list)

//let path = @"/Users/AndreasLauritzen/tree.ps"
let path = @"C:\Users\Silas\Dropbox\5. Semester\02257 - Anvendt funktionsprogrammering\Project 3\Trees\TreeSolution\TreeProj\tree.ps"

let output = new System.Text.StringBuilder()
let mutable first = true

let initPS() = 
    let start = ["%!\n";
                 "<</PageSize[1400 1000]/ImagingBBox null>> setpagedevice\n";
                 "1 1 scale\n";
                 "700 999 translate\n";
                 "newpath\n";
                 "/Times-Roman findfont 10 scalefont setfont\n"]
    String.concat "" start

let endPS() = "showpage" 

let toFile(sb:System.Text.StringBuilder) = File.WriteAllText(path,sb.ToString())

let stroke() = "stroke\n"

let label(s,(x,y)) =
    let strings = [x.ToString();" ";y.ToString();" moveto\n";
                   " (";s;") dup stringwidth pop 2 div neg 0 rmoveto show\n"] 
    String.concat "" strings

let line ((xStart,yStart),(xEnd,yEnd)) = 
    let strings = [xStart.ToString();" ";yStart.ToString();" moveto\n";
                   xEnd.ToString();" ";yEnd.ToString();" lineto\n";] 
    String.concat "" strings

let rec drawList (x:float,y:float) subtree =
    String.concat "" (List.map (fun tree -> drawSub (x,y) tree) subtree)

and drawSub (x:float,y:float) = function
    | Node((lab,offset),[])      -> let newX = (x+(offset*60.0))
                                    String.concat "" [label(lab,(newX,y));line((newX,y+10.0),(newX,y+30.0));line((newX,y+30.0),(x,y+30.0));stroke()]
     
    | Node((lab,offset),subtree) when first -> first <- false
                                               let newX = (x+(offset*60.0))
                                               let newY = y-60.0
                                               String.concat "" [label(lab,(newX,y));line((newX,y-10.0),(newX,y-30.0));line((newX,y+30.0),(x,y+30.0));(drawList (newX,newY) subtree);stroke()]   

    | Node((lab,offset),subtree) -> let newX = (x+(offset*60.0))
                                    let newY = y-60.0
                                    String.concat "" [label(lab,(newX,y));line((newX,y+10.0),(newX,y+30.0));line((newX,y-10.0),(newX,y-30.0));line((newX,y+30.0),(x,y+30.0));(drawList (newX,newY) subtree);stroke()]




let draw ((tree: Tree<string * float>),(sb:System.Text.StringBuilder)) = 
                ignore(sb.Append (initPS()))
                ignore(sb.Append (drawSub (0.0,-50.0) tree )) 
                ignore(sb.Append (endPS()))
                toFile(sb)




(* TESTS *)
//let testTree = Node(("1", 0.0),
//                               [Node (("2", -1.0),
//                                                  [Node (("3", -0.5),[]);
//                                                   Node (("4", 0.5),[])
//                                                  ]);
//                                                   Node (("5", 0.0),[]);
//                                                   Node (("6", 1.0),
//                                                                    [Node (("7", -0.5),[]);
//                                                                     Node (("8", 0.5),[])
//                                                                    ]
//                                                         )
//                                ]
//                     )
//
//let test2 =   Node
//                  (("Program", 0.0),
//                    [Node
//                       (("Decs", -1.25),
//                        [Node
//                           (("VarDec", -1.0),[Node (("a", -0.5),[]); Node (("ITyp", 0.5),[])]);
//                         Node
//                           (("VarDec", 1.0),[Node (("b", -0.5),[]); Node (("BTyp", 0.5),[])])]);
//                     Node (("Stms", 1.25),[Node (("Ass", -0.5),[]); Node (("Ass", 0.5),[])])])
//
//draw (testTree,output);;
