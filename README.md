# json-decode

Explicit, composable JSON decoders for Lean 4, designed for parsing API responses.

- **No typeclass magic.** You pick the decoder, so one Lean type can be decoded from many different API shapes.
- **Precise errors.** Every failure reports the exact path, e.g. `at $.items[1].name expecting string but got number`.
- **Small.** A handful of primitives and combinators on top of `ReaderT Context (Except Error)`, so `do`-notation just works.

## Installation

Add the package to your `lakefile.toml`:

```toml
[[require]]
name = "json-decode"
git = "https://github.com/yeyan/json-decode"
rev = "main"
```

Or in `lakefile.lean`:

```lean
require «json-decode» from git "https://github.com/yeyan/json-decode" @ "main"
```

Then:

```lean
import JsonDecode
open Lean
```

## Quick start

A decoder is a value of type `Decode α`. Build one from primitives and combinators, then run it on a `Json` value with `Decode.decode`.

```lean
import JsonDecode
open Lean

structure User where
  name : String
  age  : Nat
  tags : Array String

def userDecoder : Decode User := do
  let name <- Decode.field "name" Decode.str
  let age  <- Decode.field "age" Decode.nat
  let tags <- Decode.field "tags" (Decode.array Decode.str)
  pure { name, age, tags }

def main : IO Unit := do
  let json := Json.parse "{\"name\": \"ann\", \"age\": 30, \"tags\": [\"a\", \"b\"]}"
  match json with
  | .error e => IO.eprintln e
  | .ok j =>
    match Decode.decode userDecoder j with
    | .ok user  => IO.println s!"{user.name} is {user.age}"
    | .error e  => IO.eprintln e
```

`Decode.error` values have a `ToString` instance, so they print directly.

## API

### Primitives

| Decoder | Type | Notes |
| --- | --- | --- |
| `Decode.str` | `Decode String` | JSON string |
| `Decode.nat` | `Decode Nat` | Non-negative integer |
| `Decode.int` | `Decode Int` | Integer |
| `Decode.float` | `Decode Float` | Any JSON number |
| `Decode.as α` | `Decode α` | Escape hatch: uses an existing `FromJson α` instance |

### Combinators

| Combinator | Description |
| --- | --- |
| `Decode.field name dec` | Run `dec` on the value of object field `name` |
| `Decode.index i dec` | Run `dec` on element `i` of an array |
| `Decode.array dec` | Run `dec` on every element of an array |
| `Decode.parse f dec` | Run `dec`, then validate/convert the result with `f : α -> Except String β` |

### Running a decoder

```lean
Decode.decode (dec : Decode α) (j : Json) (path : Path := []) : Except Decode.Error α
```

`path` sets the starting position used in error messages. It is useful when you decode a sub-tree of a larger document and want errors to show where it came from.

## Error messages

Errors carry the path to the offending value and one of four kinds:

| Kind | Example message |
| --- | --- |
| `missing` | `at $ field age not found` |
| `outBound` | `at $ index 5 out of bound` |
| `wrongType` | `at $.tags[1] expecting string but got number` |
| `invalid` | `at $ invalid: expecting a non-negative integer` |

`wrongType` means the JSON kind is wrong. `invalid` means the kind is right but the value is unacceptable (a negative `nat`, a failed `parse`, a failed `as`).

Paths render from the root: `$.items[2].name` is the `name` field of the third element of `items`.

## Nesting

Decoders nest freely, and the path grows as you go in:

```lean
def names : Decode (Array String) :=
  Decode.field "items" (Decode.array (Decode.field "name" Decode.str))
```

Decoding `{"items": [{"name": "a"}, {"name": 5}]}` fails with:

```
at $.items[1].name expecting string but got number
```

Pick out a single element with `index`:

```lean
def firstTag : Decode String :=
  Decode.field "tags" (Decode.index 0 Decode.str)
```

## Validating and converting with `parse`

`parse` lifts any fallible function into a decoder. Use it for enums, dates, UUIDs, URLs, or anything else that arrives as a primitive but means something more specific.

```lean
inductive Status where
  | open_
  | closed

def status : Decode Status :=
  Decode.parse
    (fun s =>
      match s with
      | "open"   => .ok .open_
      | "closed" => .ok .closed
      | other    => .error s!"unknown status {other}")
    Decode.str
```

Failures are reported at the field being decoded:

```
at $.state invalid: unknown status wat
```

The library has no built-in date type on purpose. APIs disagree on the format (ISO 8601, epoch seconds, epoch milliseconds, ...), so you build the decoder you need:

```lean
-- parseIso : String -> Except String Timestamp, from whichever date library you use
def isoTimestamp : Decode Timestamp :=
  Decode.parse parseIso Decode.str

-- epoch milliseconds
def epochMillis : Decode Timestamp :=
  Decode.parse
    (fun ms => if ms < 0 then .error "negative timestamp" else .ok (Timestamp.ofMillis ms))
    Decode.int
```

## One type, many API shapes

Because decoders are plain values and not instances, give each API its own namespace:

```lean
structure User where
  name : String
  age  : Nat
  tags : Array String

namespace User.Github
open Decode

def decoder : Decode User := do
  let name <- field "login" str
  let age  <- field "age" nat
  let tags <- field "topics" (array str)
  pure { name, age, tags }

end User.Github

namespace User.Gitlab
open Decode

def decoder : Decode User := do
  let name <- field "profile" (field "username" str)
  let age  <- field "profile" (field "age" nat)
  let tags <- field "labels" (array str)
  pure { name, age, tags }

end User.Gitlab
```

The call site states which shape it expects:

```lean
Decode.decode User.Github.decoder json
```

## Using existing `FromJson` instances

If a type already derives or defines `FromJson`, `Decode.as` reuses it:

```lean
def count : Decode Nat := Decode.field "count" (Decode.as Nat)
def raw   : Decode (Array Nat) := Decode.as (Array Nat)
```

Error messages from `as` come from the `FromJson` instance and are reported as `invalid`. Errors inside a value decoded with `as` point at that value's path and not at the deeper location. When you need precise paths, write a `Decode` for the type instead.

## Writing custom decoders

`Decode α` is `ReaderT Decode.Context (Except Decode.Error) α`, so you can read the current position and throw errors directly:

```lean
def nonEmptyStr : Decode String := do
  let s <- Decode.str
  if s.isEmpty then
    throw ⟨← read, .invalid "expecting a non-empty string"⟩
  else
    pure s
```

`Decode.Context` holds the current `path` and the `json` node under focus.

## Testing

Tests live in `Tests/` and use `#guard`, so a failing check fails the build:

```
lake build Tests
```

## License

<!-- add your license here -->
