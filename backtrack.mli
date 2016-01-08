type state = { vars : Grid.variable array; grid : string array; }
val copy : state -> state
val is_instantiated : state -> bool
val select_var : state -> int
val bt : Grid.variable array -> string array -> (int * string) list -> bool
