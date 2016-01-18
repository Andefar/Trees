//let guardian = @"C:\Users\Silas\Dropbox\5. Semester\02257 - Anvendt funktionsprogrammering\Project 2\GuardedCommands\GuardedCommands\GuardedCommands\"
let localPath = @"C:\Users\Silas\Dropbox\5. Semester\02257 - Anvendt funktionsprogrammering\Project 3\Trees\TreeSolution\TreeProj\"

#r @".\bin\Debug\FSharp.PowerPack.dll";

#load "AST.fs"
#load "Parser.fs"
#load "Lexer.fs"
#load "Util.fs"
#load "ParseToNode.fs"
#load "Design.fs"
#load "Draw.fs"

open Tree
open AST
open Parser
open Lexer
open ParserUtil
open ParseToNode
open Design
open Draw

System.IO.Directory.SetCurrentDirectory localPath;

let drawFile name =  changePath localPath name
                     (name + ".gc") |> parseFromFile |> parseToNode |> design |> draw
                     (name + ".gc") |> parseFromFile |> parseToNode |> design



drawFile "temp"