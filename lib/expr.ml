open Tokens

module Expr = struct
  type t =
    | Binary of t * token * t
    | Unary of token * t
    | Grouping of t
    | Literal of literal
end
