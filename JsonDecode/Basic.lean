import Lean.Data.Json

open Lean

inductive PathSeg where
  | key (n : String)
  | idx (i : Nat)
  deriving Repr

abbrev Path :=
  List PathSeg

private def Path.render : Path -> String
  | [] => "$"
  | head :: tail =>
    render tail ++
      match head with
      | .key n => s!".{n}"
      | .idx i => s!"[{i}]"

instance : ToString Path where
  toString := Path.render

namespace Lean.Json

def kind : Json -> String
  | .null => "null"
  | .bool _ => "bool"
  | .num _ => "number"
  | .str _ => "string"
  | .arr _ => "array"
  | .obj _ => "object"

end Lean.Json

namespace Decode

structure Context where
  json : Json
  path : Path

inductive Error.Kind where
  | missing (name : String)
  | outBound (i : Nat)
  | wrongType (expecting : String)
  | invalid (reason : String)

structure Error where
  context : Context
  kind : Error.Kind

private def Error.render (err : Error) : String :=
  let pre := s!"at {err.context.path} "
  pre ++
    match err.kind with
    | .missing name => s!"field {name} not found"
    | .outBound i => s!"index {i} out of bound"
    | .wrongType expecting => s!"expecting {expecting} but got {err.context.json.kind}"
    | .invalid reason => s!"invalid: {reason}"

instance : ToString Error where
  toString := Error.render

end Decode

abbrev Decode α :=
  ReaderT Decode.Context (Except Decode.Error) α

namespace Decode

/- primities -/
def str : Decode String := do
  let ctx <- read
  match ctx.json with
  | .str x =>
    pure x
  | _ =>
    throw ⟨ctx, .wrongType "string"⟩

def nat : Decode Nat := do
  let ctx <- read
  match ctx.json with
  | .num _ =>
    match ctx.json.getNat? with
    | .ok x =>
      pure x
    | _ =>
      throw ⟨ctx, .invalid "expecting a non-negative integer"⟩
  | _ => throw ⟨ctx, .wrongType "number"⟩

def int : Decode Int := do
  let ctx <- read
  match ctx.json with
  | .num _ =>
    match ctx.json.getInt? with
    | .ok x =>
      pure x
    | _ =>
      throw ⟨ctx, .invalid "expecting an integer"⟩
  | _ => throw ⟨ctx, .wrongType "number"⟩

def float : Decode Float := do
  let ctx <- read
  match ctx.json with
  | .num x =>
    pure x.toFloat
  | _ =>
    throw ⟨ctx, .wrongType "float"⟩

def parse (f : α -> Except String β) (dec: Decode α) : Decode β := do
  let ctx <- read
  let x <- dec
  match f x with
  | .ok y => pure y
  | .error err => throw ⟨ctx, .invalid err⟩

def as (α : Type) [FromJson α] : Decode α := do
  let ctx <- read
  match FromJson.fromJson? ctx.json with
  | .ok x =>
    pure x
  | .error err =>
    throw ⟨ctx, .invalid err⟩

/- combinantors -/
def field (name : String) (dec : Decode α) : Decode α := do
  let ctx <- read
  match ctx.json.getObjVal? name with
  | .ok x =>
    withReader (fun ctx => { path := .key name :: ctx.path, json := x }) dec
  | .error _ =>
    throw ⟨ctx, .missing name⟩

def index (i : Nat) (dec : Decode α) : Decode α := do
  let ctx <- read
  match ctx.json.getArrVal? i with
  | .ok x =>
    withReader (fun ctx => { path := .idx i :: ctx.path, json := x }) dec
  | .error _ =>
    throw ⟨ctx, .outBound i⟩

def array (dec : Decode α) : Decode (Array α) := do
  let ctx <- read
  match ctx.json with
  | .arr xs =>
    xs.zipIdx.mapM fun (x, i) => do
        withReader (fun ctx => { path := .idx i :: ctx.path, json := x }) dec
  | _ =>
    throw ⟨ctx, .wrongType "array"⟩

def decode (dec : Decode α) (j : Json) (path : Path := []) : Except Error α :=
  dec.run ⟨j, path⟩

end Decode
