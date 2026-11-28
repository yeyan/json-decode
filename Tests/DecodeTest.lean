import JsonDecoder

open Lean

namespace Tests

/-! ## Helpers -/

/-- Parse a JSON literal; panics on malformed input (tests only). -/
def j (s : String) : Json :=
  match Json.parse s with
  | .ok v => v
  | .error e => panic! s!"bad test JSON: {e}"

/-- Succeeded with exactly this value. -/
def okEq [BEq α] (r : Except DecodeError α) (expected : α) : Bool :=
  match r with
  | .ok x => x == expected
  | .error _ => false

/-- Failed with this kind, at this rendered path (e.g. "$.a[1].b"). -/
def errIs (r : Except DecodeError α) (kind : DecodeError.Kind) (path : String) : Bool :=
  match r with
  | .ok _ => false
  | .error e => e.kind == kind && toString e.ctx.path == path

/-- Failed with exactly this rendered message. -/
def errMsg (r : Except DecodeError α) (msg : String) : Bool :=
  match r with
  | .ok _ => false
  | .error e => toString e == msg

open Decoder

/-! ## Primitives -/

#guard okEq (decode str (j "\"hi\"")) "hi"
#guard errIs (decode str (j "1")) (.wrongType "string") "$"

#guard okEq (decode int (j "5")) (5 : Int)
#guard okEq (decode int (j "-3")) (-3 : Int)
#guard errIs (decode int (j "\"x\"")) (.wrongType "integer") "$"
-- Documents current behaviour: numbers with a fractional/exponent part are rejected.
#guard errIs (decode int (j "5.5")) (.invalid "expecting an integer") "$"

#guard okEq (decode nat (j "7")) (7 : Nat)
#guard okEq (decode nat (j "0")) (0 : Nat)
#guard errIs (decode nat (j "-1")) (.invalid "expecting a non-negative integer") "$"
#guard errIs (decode nat (j "true")) (.wrongType "number") "$"

#guard okEq (decode float (j "1.5")) (1.5 : Float)
#guard okEq (decode float (j "2")) (2.0 : Float)
#guard errIs (decode float (j "null")) (.wrongType "float") "$"

/-! ## field -/

#guard okEq (decode (field "a" int) (j "{\"a\": 1}")) (1 : Int)
#guard errIs (decode (field "b" int) (j "{\"a\": 1}")) (.missing "b") "$"
#guard errIs (decode (field "a" int) (j "[1]")) (.wrongType "object") "$"
#guard errIs (decode (field "a" int) (j "{\"a\": \"x\"}")) (.wrongType "integer") "$.a"

-- nested paths
#guard okEq (decode (field "a" (field "b" int)) (j "{\"a\":{\"b\":1}}")) (1 : Int)
#guard errIs (decode (field "a" (field "b" int)) (j "{\"a\":{\"b\":\"x\"}}"))
  (.wrongType "integer") "$.a.b"
#guard errIs (decode (field "a" (field "b" int)) (j "{\"a\":{}}")) (.missing "b") "$.a"

/-! ## optField -/

#guard okEq (decode (optField "x" int) (j "{\"x\": 4}")) (some (4 : Int))
#guard okEq (decode (optField "x" int) (j "{}")) (none : Option Int)
#guard okEq (decode (optField "x" int) (j "{\"x\": null}")) (none : Option Int)
-- present but wrong type must be an error, not silently `none`
#guard errIs (decode (optField "x" int) (j "{\"x\": \"s\"}")) (.wrongType "integer") "$.x"
#guard errIs (decode (optField "x" int) (j "[]")) (.wrongType "object") "$"

/-! ## index -/

#guard okEq (decode (index 1 int) (j "[10, 20]")) (20 : Int)
#guard errIs (decode (index 2 int) (j "[10, 20]")) (.outOfBound 2) "$"
#guard errIs (decode (index 1 int) (j "[10, \"x\"]")) (.wrongType "integer") "$[1]"
-- Requires `index` to check for an array first (see the suggested fix):
#guard errIs (decode (index 0 int) (j "{\"a\": 1}")) (.wrongType "array") "$"

/-! ## array -/

#guard okEq (decode (array int) (j "[1,2,3]")) (#[1, 2, 3] : Array Int)
#guard okEq (decode (array int) (j "[]")) (#[] : Array Int)
#guard errIs (decode (array int) (j "{}")) (.wrongType "array") "$"
#guard errIs (decode (array int) (j "[1,\"x\",3]")) (.wrongType "integer") "$[1]"
#guard errIs (decode (field "xs" (array (field "n" int))) (j "{\"xs\":[{\"n\":1},{\"n\":\"a\"}]}"))
  (.wrongType "integer") "$.xs[1].n"
#guard errIs (decode (field "xs" (array (field "n" int))) (j "{\"xs\":[{\"n\":1},{}]}"))
  (.missing "n") "$.xs[1]"

/-! ## parse -/

def nonEmpty (s : String) : Except String Nat :=
  if s.isEmpty then .error "empty" else .ok s.length

#guard okEq (decode (parse nonEmpty str) (j "\"abc\"")) (3 : Nat)
#guard errIs (decode (parse nonEmpty str) (j "\"\"")) (.invalid "empty") "$"
-- inner decoder failure passes through unchanged
#guard errIs (decode (parse nonEmpty str) (j "1")) (.wrongType "string") "$"

/-! ## orNone -/

#guard okEq (decode (orNone int) (j "3")) (some (3 : Int))
#guard okEq (decode (orNone int) (j "\"x\"")) (none : Option Int)

/-! ## decode with a starting path -/

#guard errIs (decode int (j "\"x\"") [.key "root"]) (.wrongType "integer") "$.root"
#guard errIs (decode int (j "\"x\"") [.idx 0, .key "root"]) (.wrongType "integer") "$.root[0]"

/-! ## Error rendering -/

#guard errMsg (decode (field "b" int) (j "{}")) "at $ field b not found"
#guard errMsg (decode (index 3 int) (j "[]")) "at $ index 3 out of bound"
#guard errMsg (decode (field "a" int) (j "{\"a\":\"x\"}")) "at $.a expecting integer but got string"
#guard errMsg (decode (field "a" (field "b" nat)) (j "{\"a\":{\"b\":-1}}"))
  "at $.a.b invalid: expecting a non-negative integer"

/-! ## A record decoder end to end -/

structure User where
  name : String
  age : Nat
  email : Option String
  deriving BEq, Repr

def user : Decoder User := do
  let name <- field "name" str
  let age <- field "age" nat
  let email <- optField "email" str
  pure { name, age, email }

#guard okEq (decode user (j "{\"name\":\"ann\",\"age\":30}"))
  ({ name := "ann", age := 30, email := none } : User)
#guard okEq (decode user (j "{\"name\":\"ann\",\"age\":30,\"email\":\"a@b.c\"}"))
  ({ name := "ann", age := 30, email := some "a@b.c" } : User)
#guard okEq (decode user (j "{\"name\":\"ann\",\"age\":30,\"email\":null}"))
  ({ name := "ann", age := 30, email := none } : User)
#guard errIs (decode user (j "{\"name\":\"ann\"}")) (.missing "age") "$"
#guard errIs (decode user (j "{\"name\":\"ann\",\"age\":-2}"))
  (.invalid "expecting a non-negative integer") "$.age"
#guard errIs (decode (array user) (j "[{\"name\":\"a\",\"age\":1},{\"name\":\"b\"}]"))
  (.missing "age") "$[1]"

end Tests
