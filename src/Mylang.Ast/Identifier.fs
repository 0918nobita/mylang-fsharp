module Mylang.Ast.Identifier

open Parsec.SourcePos

type Identifier =
    { Text: string
      SourceRange: SourceRange }
