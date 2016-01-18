﻿module Draw

open System.IO
open Translate

// global scales. Change only this to obtain different look of the visual tree
let mutable scale = 40.0                      // optimal: 40.0 - scaling x axis - everthing from 20.0 to 100.0 will work fine in terms of looks
let mutable height = 40.0                     // optimal: 40.0 - scaling y axis - everthing from 20.0 to 100.0 will work fine in terms of looks

let mutable path = ""  
let fontSize = scale/6.0
let lScale = height/7.5 + 0.1/fontSize
let labDiff = height/12.0


let setScale fl = scale <- fl
let setHeight fl = height <- fl
  
let changePath localPath s = path <- String.concat "" [localPath;s;".ps"]
 
let mutable first = true

let initPS() = 
    let start = ["%!\n";
                 "<</PageSize[1400 1000]/ImagingBBox null>> setpagedevice\n";
                 "1 1 scale\n";
                 "700 999 translate\n";
                 "newpath\n";
                 "/Times-Roman findfont "+ string(fontSize) + " scalefont setfont\n"]
    String.concat "" start

let endPS() = "showpage" 

let toFile(sb:System.Text.StringBuilder) = File.WriteAllText(path,sb.ToString())

let stroke() = "stroke\n"

let label(s,(x,y)) =
    let xI = (int x)
    let yI = (int y)
    let strings = [xI.ToString();" ";yI.ToString();" moveto\n";
                   " (";s;") dup stringwidth pop 2 div neg 0 rmoveto show\n"] 
    String.concat "" strings

let line ((xStart,yStart),(xEnd,yEnd)) = 
    let xStartI = (int xStart)
    let yStartI = (int yStart)
    let xEndI = (int xEnd)
    let yEndI = (int yEnd)
    let strings = [xStartI.ToString();" ";yStartI.ToString();" moveto\n";
                   xEndI.ToString();" ";yEndI.ToString();" lineto\n";] 
    String.concat "" strings

let rec drawList (x:float,y:float) subtree =
    String.concat "" (List.map (fun tree -> drawSub (x,y) tree) subtree)

and drawSub (x:float,y:float) = function
    //leaf
    | Node((lab,offset),[])      -> let newX = (x+(offset*scale)) 
                                    String.concat "" [label(lab,(newX,y-labDiff)); //Node label
                                                      line((newX,y+(height/lScale)),(newX,y+height/2.0)); //Line above
                                                      line((newX,y+height/2.0),(x,y+height/2.0)); //Line to center
                                                      stroke()]
    // first node
    | Node((lab,offset),subtree) when first -> first <- false
                                               let newX = (x+(offset*scale))
                                               let newY = y-height
                                               String.concat "" [label(lab,(newX,y-labDiff)); //Node label
                                                                 line((newX,y-(height/lScale)),(newX,y-height/2.0)); //Line beneath
                                                                 (drawList (newX,newY) subtree)]   
    // intermediate node
    | Node((lab,offset),subtree) -> let newX = (x+(offset*scale))
                                    let newY = y-height
                                    String.concat "" [label(lab,(newX,y-labDiff)); //Node label
                                                      line((newX,y+(height/lScale)),(newX,y+height/2.0)); //Line above
                                                      line((newX,y-(height/lScale)),(newX,y-height/2.0)); //Line beneath
                                                      line((newX,y+height/2.0),(x,y+height/2.0)); //Line to center
                                                      (drawList (newX,newY) subtree)]

let draw' (tree,sb:System.Text.StringBuilder) = 
                ignore(sb.Append (initPS()))
                ignore(sb.Append (drawSub (0.0,-50.0) tree )) 
                ignore(sb.Append (endPS()))
                toFile(sb)

let draw tree = draw'(tree,new System.Text.StringBuilder())