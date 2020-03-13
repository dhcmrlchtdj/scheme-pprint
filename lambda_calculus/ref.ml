type types =
  [ `Bool
  | `Arrow of types * types
  | `Unit
  | `Ref of types
  ]

type terms =
  [ `Bool of bool
  | `Var of string
  | `Abs of string * types * terms
  | `App of terms * terms
  | `Unit
  | `Ref of terms
  | `Deref of terms
  | `Assign of terms * terms
  | `Loc of int
  ]

type values =
  [ `Bool of bool
  | `Abs of string * types * terms
  | `Unit
  | `Loc of int
  ]

type val_context = (string * terms) list

type typ_context = (string * types) list

type stores = (int * values) list
