let localPath = @"C:\Users\Silas\Dropbox\5. Semester\02257 - Anvendt funktionsprogrammering\Project 3\Trees\TreeSolution\TreeProj\"
let gcPath = localPath

#r @".\bin\Debug\FSharp.PowerPack.dll";

#load "AST.fs"
#load "Parser.fs"
#load "Lexer.fs"
#load "Util.fs"
#load "Translate.fs"
#load "Design.fs"
#load "Draw.fs"

open Tree
open AST
open Parser
open Lexer
open ParserUtil
open Translate
open Design
open Draw

// this directory is only used when downloading .gc files to parse
System.IO.Directory.SetCurrentDirectory gcPath;

// passing through parser from project 2
let parse name = changePath localPath name
                 (name + ".gc") |> parseFromFile

// this is the extension done for project 3 
let transDesignAndDraw tree =  tree |> translate |> design |> draw

// draws and creates .ps file with the given "name"
let draw name = parse name |> transDesignAndDraw

// counting all nodes in the the given program
let rec countNodes' = function
   | Node(_,[]) -> 1
   | Node(_,ls) -> 1 + (List.sum (List.map countNodes' ls)) 

let countNodes name = parse name |> translate |> countNodes'

// this function only takes time on operations from project 3, calculating mean of 40 samples
let takeTime name = let a = parse name |> translate
                    let mutable totalTime = 0.0
                    for i in [1..40] do
                       let stopWatch = System.Diagnostics.Stopwatch.StartNew()
                       ignore(a |> design)
                       stopWatch.Stop()
                       totalTime <- totalTime + stopWatch.Elapsed.TotalMilliseconds
                    totalTime/40.0


// -------------------- Draw and create .ps file from file name (raw filename) ---------------------

// Commando to draw and create .ps file
draw "example"

(*
// ------------------------------- Extra possible dynamiclly changes -------------------------------
// change scales, every other value will dynamically update
// Reasonable interval (20.0-80.0), default 40.0 - use smaller value for bigger programs - changing x axis 
setScale 40.0
// Reasonable interval (20.0-80.0), default 40.0 - changing distance between each layer - changing y axis
setHeight 40.0   

// Commando to see Node tree
parse "fact" |> translate

// Commandos to measure time and nodes
countNodes "temp"               
takeTime "temp"
*)