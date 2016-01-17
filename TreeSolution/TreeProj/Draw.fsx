open System.IO
let path = @"/Users/AndreasLauritzen/tree.ps"
let output = new System.Text.StringBuilder()

let initPS() = output.Append "%!\n<</PageSize[1400 1000]/ImagingBBox null>> setpagedevice\n1 1 scale\nnewpath\n/Times-Roman findfont 10 scalefont setfont\n"

let endPS() = output.Append "showpage"

let toFile() = File.WriteAllText(path,output.ToString())

let label(s) =
    let strings = [" (";s;") dup stringwidth pop 2 div neg 0 rmoveto show\n"] 
    let fullString = String.concat ", " strings
    output.Append fullString



initPS();;
label("hej");;
endPS();;
toFile();;
