// Michael R. Hansen 05-01-2016

namespace Tree

open System.IO
open System.Text
open Microsoft.FSharp.Text.Lexing

open AST
open Parser

module ParserUtil = 

   let parseString (text:string) =
      let lexbuf = LexBuffer<_>.FromBytes(Encoding.UTF8.GetBytes(text))
      try
           Main Lexer.tokenize lexbuf
      with e ->
           let pos = lexbuf.EndPos
           printfn "Error near line %d, character %d\n" pos.Line pos.Column
           failwith "parser termination"

// Parse a file. (A statement is parsed) 
   let parseFromFile filename =
      if File.Exists(filename)    
      then parseString(File.ReadAllText(filename))
      else invalidArg "ParserUtil" "File not found"
