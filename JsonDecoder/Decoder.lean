import JsonDecoder.Basic

namespace Decoder

open Lean

def fail (kind : DecodeError.Kind) : Decoder α := do
  let ctx <- read
  throw { ctx, kind }

def json : Decoder Json := do
  pure (<- read).json

def str : Decoder String := do
  match (<- json) with
  | .str x => pure x
  | _ => fail (.wrongType "string")

def int : Decoder Int := do
  let j <- json
  match j with
  | .num _ => 
    match j.getInt? with
    | .ok x => pure x
    | _ => fail (.invalid "expecting an integer")
  | _ => fail (.wrongType "integer")

def nat : Decoder Nat := do
  let j <- json
  match j with 
  | .num _ =>
    match j.getNat? with
    | .ok x => pure x
    | _ => fail (.invalid "expecting a non-negative integer")
  | _ => fail (.wrongType "number")

def float : Decoder Float := do
  match <- json with
  | .num x => pure x.toFloat
  | _ => fail (.wrongType "float")

def parse (f : α -> Except String β) (dec: Decoder α) : Decoder β := do
  match f (<- dec) with
  | .ok x => pure x
  | .error err => fail (.invalid err)

def field (name : String) (dec : Decoder α) : Decoder α := do
  match <- json with
  | .obj kvs =>
    match kvs.get? name with   -- or getObjVal?
    | some x => withReader (fun ctx => { path := .key name :: ctx.path, json := x }) dec
    | none => fail (.missing name)
  | _ => fail (.wrongType "object")

def optField (name : String) (dec : Decoder α) : Decoder (Option α) := do
  match <- json with
  | .obj kvs =>
    match kvs.get? name with
    | none | some .null => pure none
    | some x => some <$> withReader (fun ctx => { path := .key name :: ctx.path, json := x }) dec
  | _ => fail (.wrongType "object")
  
def index (i : Nat) (dec : Decoder α) : Decoder α := do
  match <- json with
  | .arr xs =>
    match xs[i]? with
    | some x => withReader (fun ctx => { path := .idx i :: ctx.path, json := x }) dec
    | none => fail (.outOfBound i)
  | _ => fail (.wrongType "array")

def array (dec : Decoder α) : Decoder (Array α) := do
  match <- json with
  | .arr xs =>
    xs.zipIdx.mapM fun (x, i) => do
      withReader (fun ctx => { path := .idx i :: ctx.path , json := x}) dec
  | _ =>
    fail (.wrongType "array")

def orNone (dec : Decoder α) : Decoder (Option α) := do
  (.some <$> dec) <|> pure .none

def decode (dec : Decoder α) (json : Json) (path : Path := []) : Except DecodeError α :=
  dec.run { json, path }

end Decoder

