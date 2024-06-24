open Tokens

module Expr = struct
  type t =
    | Binary of binary_expr
    | Unary of unary_expr
    | Grouping of t
    | Literal of literal

  (* The other ones are implicitly defined - since it was wasteful to write it all out. *)
  and binary_expr = { left : t; operator : token; right : t }
  and unary_expr = { left : t; operator : token }
end
