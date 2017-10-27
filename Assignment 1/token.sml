datatype TOKEN = UNMINUS of int * int
| BINADD of int * int
| BINSUB of int * int
| BINDIV of int * int
| BINMUL of int * int
| BINMOD of int * int
| NEG of int * int
| AND of int * int
| OR of int * int
| ASSIGN of int * int
| EQ of int * int
| NE of int * int
| LT of int * int
| LTE of int * int
| GT of int * int
| GTE of int * int
| LP of int * int
| RP of int * int
| LB of int * int
| RB of int * int
| EOS of int * int
| COMMA of int * int
| INT of int * int
| BOOL of int * int
| IF of int * int
| THEN of int * int
| ELSE of int * int
| WHILE of int * int
| PROC of int * int
| PRINT of int * int
| READ of int * int
| ERROR of int * int * int
| INTLIT of int * int * int
| IDENT of int * int * string
| BOOLVAL of int * int * bool