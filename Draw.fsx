open System.IO
let path = @"/Users/AndreasLauritzen/tree.ps"
let output = new System.Text.StringBuilder()

let initPS() = 
    let start = ["%!\n";
                 "<</PageSize[1400 1000]/ImagingBBox null>> setpagedevice\n";
                 "1 1 scale\n";
                 "700 999 translate\n";
                 "newpath\n";
                 "/Times-Roman findfont 10 scalefont setfont\n"]
    output.Append (String.concat "" start)

let endPS() = output.Append "showpage"

let toFile() = File.WriteAllText(path,output.ToString())


let stroke() = output.Append "stroke\n"

let label(s,(x,y)) =
    let strings = [x.ToString();" ";y.ToString();" moveto\n";
                   " (";s;") dup stringwidth pop 2 div neg 0 rmoveto show\n"] 
    output.Append (String.concat "" strings)

let line ((xStart,yStart),(xEnd,yEnd)) = 
    let strings = [xStart.ToString();" ";yStart.ToString();" moveto\n";
                   xEnd.ToString();" ";yEnd.ToString();" lineto\n";] 
    output.Append (String.concat "" strings)

 
(* TESTS *)
initPS();;
label("hej",(0,-50));;
line((0,-60),(50,-80))
stroke();;
endPS();;
toFile();;
