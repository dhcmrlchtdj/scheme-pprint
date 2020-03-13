type types =
  [ `Bool
  | `Arrow of types * types
  ]

type terms =
  [ `Bool of bool
  | `Var of string
  | `Abs of string * types * terms
  | `App of terms * terms
  | `Error of string
  | `Raise of terms
  | `Try of terms * terms
  ]

type values =
  [ `Bool of bool
  | `Abs of string * types * terms
  | `Error of string
  ]

type val_context = (string * terms) list

type typ_context = (string * types) list
