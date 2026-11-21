import JsonDecode

open Lean

/-! ## Helpers -/

/-- Parse `s` as JSON, then run the decoder starting at `path`. Errors are rendered to strings. -/
def runAt (path : Path) (dec : Decode α) (s : String) : Except String α := do
  let j ← Json.parse s
  (Decode.decode dec j path).mapError toString

def run (dec : Decode α) (s : String) : Except String α :=
  runAt [] dec s

def succeeds [BEq α] (r : Except String α) (expected : α) : Bool :=
  match r with
  | .ok x => x == expected
  | .error _ => false

def failsWith (r : Except String α) (msg : String) : Bool :=
  match r with
  | .error e => e == msg
  | .ok _ => false

def fails (r : Except String α) : Bool :=
  match r with
  | .error _ => true
  | .ok _ => false

/-! ## Path rendering -/

#guard toString ([] : Path) == "$"

#guard toString ([.key "a"] : Path) == "$.a"

#guard toString ([.key "name", .idx 2, .key "items"] : Path) == "$.items[2].name"

/-! ## Primitives -/

-- str
#guard succeeds (run Decode.str "\"hi\"") "hi"

#guard failsWith (run Decode.str "1") "at $ expecting string but got number"

-- nat
#guard succeeds (run Decode.nat "42") 42

#guard succeeds (run Decode.nat "0") 0

#guard failsWith (run Decode.nat "-1") "at $ invalid: expecting a non-negative integer"

#guard failsWith (run Decode.nat "1.5") "at $ invalid: expecting a non-negative integer"

#guard failsWith (run Decode.nat "\"x\"") "at $ expecting number but got string"

-- int
#guard succeeds (run Decode.int "-7") (-7 : Int)

#guard succeeds (run Decode.int "7") (7 : Int)

#guard failsWith (run Decode.int "1.5") "at $ invalid: expecting an integer"

#guard failsWith (run Decode.int "null") "at $ expecting number but got null"

-- float
#guard succeeds (run Decode.float "2.5") 2.5

#guard succeeds (run Decode.float "3") 3.0

#guard failsWith (run Decode.float "true") "at $ expecting float but got bool"

-- as (message text comes from FromJson, so only check that it fails)
#guard succeeds (run (Decode.as Nat) "5") 5

#guard succeeds (run (Decode.as (Array String)) "[\"a\",\"b\"]") #["a", "b"]

#guard fails (run (Decode.as Nat) "\"x\"")

/-! ## field / index -/

#guard succeeds (run (Decode.field "a" Decode.nat) "{\"a\": 1}") 1

#guard failsWith (run (Decode.field "a" Decode.nat) "{}") "at $ field a not found"

-- documents current behaviour: a non-object also reports "not found"
#guard failsWith (run (Decode.field "a" Decode.nat) "[]") "at $ field a not found"

-- the error path points inside the nested value
#guard
  failsWith (run (Decode.field "a" (Decode.field "b" Decode.nat)) "{\"a\": {\"b\": \"x\"}}")
    "at $.a.b expecting number but got string"

-- a missing field is reported at the parent
#guard
  failsWith (run (Decode.field "a" (Decode.field "b" Decode.nat)) "{\"a\": {}}")
    "at $.a field b not found"

#guard succeeds (run (Decode.index 1 Decode.nat) "[10, 20, 30]") 20

#guard failsWith (run (Decode.index 5 Decode.nat) "[10, 20, 30]") "at $ index 5 out of bound"

#guard failsWith (run (Decode.index 0 Decode.nat) "{}") "at $ index 0 out of bound"

/-! ## array -/

#guard succeeds (run (Decode.array Decode.nat) "[1, 2, 3]") #[1, 2, 3]

#guard succeeds (run (Decode.array Decode.nat) "[]") #[]

#guard failsWith (run (Decode.array Decode.nat) "{}") "at $ expecting array but got object"

#guard
  failsWith (run (Decode.array Decode.nat) "[1, \"x\", 3]")
    "at $[1] expecting number but got string"

#guard
  failsWith (run (Decode.array (Decode.array Decode.nat)) "[[1], [2, \"x\"]]")
    "at $[1][1] expecting number but got string"

-- full path through object -> array -> object
#guard
  failsWith
    (run (Decode.field "items" (Decode.array (Decode.field "name" Decode.str)))
      "{\"items\": [{\"name\": \"a\"}, {\"name\": 5}]}")
    "at $.items[1].name expecting string but got number"

/-! ## parse -/

private def status : Decode Bool :=
  Decode.parse
    (fun s =>
      if s == "open" then .ok true
      else if s == "closed" then .ok false else .error s!"unknown status {s}")
    Decode.str

#guard succeeds (run status "\"open\"") true

#guard succeeds (run status "\"closed\"") false

#guard failsWith (run status "\"wat\"") "at $ invalid: unknown status wat"

-- inner decoder failure is not masked
#guard failsWith (run status "1") "at $ expecting string but got number"

-- error is reported at the field
#guard
  failsWith (run (Decode.field "s" status) "{\"s\": \"wat\"}") "at $.s invalid: unknown status wat"

/-! ## decode with a starting path -/

#guard failsWith (runAt [.key "root"] Decode.str "1") "at $.root expecting string but got number"

/-! ## A realistic record decoder -/

structure User where
  name : String
  age : Nat
  tags : Array String
  deriving BEq

namespace User.Example

def decoder : Decode User := do
  let name ← Decode.field "name" Decode.str
  let age ← Decode.field "age" Decode.nat
  let tags ← Decode.field "tags" (Decode.array Decode.str)
  pure { name, age, tags }

end User.Example

#guard
  succeeds (run User.Example.decoder "{\"name\": \"ann\", \"age\": 30, \"tags\": [\"a\", \"b\"]}")
    { name := "ann", age := 30, tags := #["a", "b"] }

-- the first failure wins
#guard
  failsWith (run User.Example.decoder "{\"name\": \"ann\", \"tags\": []}")
    "at $ field age not found"

#guard
  failsWith (run User.Example.decoder "{\"name\": \"ann\", \"age\": 30, \"tags\": [\"a\", 1]}")
    "at $.tags[1] expecting string but got number"
