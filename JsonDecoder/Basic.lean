import Lean.Data.Json

open Lean

inductive PathSeg where
  | key (n : String)
  | idx (i : Nat)
  deriving Repr

abbrev Path := List PathSeg

private def Path.render : Path -> String
| [] => "$"
| head :: tail =>
  render tail ++ 
    match head with
    | .key n => s!".{n}"
    | .idx i => s!"[{i}]"

instance : ToString Path where
  toString := Path.render

structure DecodeCtx where
  json : Json
  path : Path

inductive DecodeError.Kind where
  | missing (name : String)
  | outOfBound (i : Nat)
  | wrongType (expecting : String)
  | invalid (reason : String)
  deriving Repr, BEq

structure DecodeError where
  ctx : DecodeCtx
  kind : DecodeError.Kind


private def DecodeError.render (err : DecodeError) : String :=
  let pre := s!"at {err.ctx.path} "
  pre ++
    match err.kind with
    | .missing name => s!"field {name} not found"
    | .outOfBound i => s!"index {i} out of bound"
    | .wrongType expecting => s!"expecting {expecting} but got {kind err.ctx.json}"
    | .invalid reason => s!"invalid: {reason}"
where
  kind : Json -> String
  | .null => "null"
  | .bool _ => "bool"
  | .num _ => "number"
  | .str _ => "string"
  | .arr _ => "array"
  | .obj _ => "object"

instance : ToString DecodeError where
  toString := DecodeError.render

abbrev Decoder α :=
  ReaderT DecodeCtx (Except DecodeError) α

