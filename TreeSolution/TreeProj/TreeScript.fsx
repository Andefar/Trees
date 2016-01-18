let guardian = @"C:\Users\Silas\Dropbox\5. Semester\02257 - Anvendt funktionsprogrammering\Project 2\GuardedCommands\GuardedCommands\GuardedCommands\"
let localPath = @"C:\Users\Silas\Dropbox\5. Semester\02257 - Anvendt funktionsprogrammering\Project 3\Trees\TreeSolution\TreeProj\"

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

System.IO.Directory.SetCurrentDirectory guardian;


let translate name = changePath localPath name
                     (name + ".gc") |> parseFromFile |> translate

let designAndDraw tree =  tree |> design |> draw

let rec countNodes = function
   | Node(_,[]) -> 1
   | Node(_,ls) -> 1 + (List.sum (List.map countNodes ls)) 

//#time "on"
//ignore(parseToNodes "wide" |> countNodes)
//translate "wide" |> designAndDraw

setScale 30.0

let a = translate "QuickSortV1"
a |> designAndDraw