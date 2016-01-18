module Draw

open System.IO
open ParseToNode

let mutable path = ""
let output = new System.Text.StringBuilder()
let scale = 40.0
let height = 50.0
let fontSize = scale/6.0
let lScale = height/7.5 + 0.1/fontSize
let labDiff = height/15.0

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
    | Node((lab,offset),[])      -> let newX = (x+(offset*scale))
                                    String.concat "" [label(lab,(newX,y-labDiff));line((newX,y+(height/lScale)),(newX,y+height/2.0));line((newX,y+height/2.0),(x,y+height/2.0));stroke()]
     
    | Node((lab,offset),subtree) when first -> first <- false
                                               let newX = (x+(offset*scale))
                                               let newY = y-height
                                               String.concat "" [label(lab,(newX,y-labDiff));line((newX,y-(height/lScale)),(newX,y-height/2.0));(drawList (newX,newY) subtree);stroke()]   

    | Node((lab,offset),subtree) -> let newX = (x+(offset*scale))
                                    let newY = y-height
                                    String.concat "" [label(lab,(newX,y-labDiff));line((newX,y+(height/lScale)),(newX,y+height/2.0));line((newX,y-(height/lScale)),(newX,y-height/2.0));line((newX,y+height/2.0),(x,y+height/2.0));(drawList (newX,newY) subtree);stroke()]

let draw' ((tree: Tree<string * float>),(sb:System.Text.StringBuilder)) = 
                ignore(sb.Append (initPS()))
                ignore(sb.Append (drawSub (0.0,-50.0) tree )) 
                ignore(sb.Append (endPS()))
                toFile(sb)

let draw tree = draw'(tree,output)