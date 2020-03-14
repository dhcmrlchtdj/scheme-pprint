type types =
  [ `Bool
  | `Arrow of types * types
  | `Unit
  | `UserDef of string
  | `Tuple of types list
  | `Record of (string * types) list
  | `Variant of (string * types) list
  | `List of types
  ]

type terms =
  [ `Bool of bool
  | `Var of string
  | `Abs of string * types * terms
  | `App of terms * terms
  | `Unit
  | `As of string * types
  | `Let of string * terms * terms
  | `Tuple of terms list
  | `TupleItem of terms * terms
  | `Record of (string * terms) list
  | `RecordItem of terms * string
  | `Tagging of string * string * types
  | `Case of terms * (string * string * terms) list
  | `Fix of terms
  | `ListNil of types
  | `ListCons of types * terms * terms
  | `ListIsNil of types * terms
  | `ListHead of types * terms
  | `ListTail of types * terms
  ]

type values =
  [ `Bool of bool
  | `Abs of string * types * terms
  | `Unit
  | `Tuple of values list
  | `Record of (string * values) list
  | `ListNil of types
  | `ListCons of types * terms * terms
  ]
