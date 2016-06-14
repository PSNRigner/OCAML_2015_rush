type field = All | Id | Name | Surname | Age | Email | Phone;;
type contact = {mutable surname : string; mutable name : string; mutable age : int; mutable email : string; mutable phone : string};;

val add : contact list -> string * string * int * string * string -> contact list
val getId : contact list -> field -> string -> int
val remove : contact list -> int -> contact list
val replace : contact list -> int -> string * string * int * string * string -> contact list
val print : contact list -> field -> string -> unit