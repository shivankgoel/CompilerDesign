fun getclist (filename:string) = 
    let val f = TextIO.getInstream(TextIO.openIn filename)
	fun loop (clist, f) = 
	    case TextIO.StreamIO.input1 f of
		SOME (c, f') => loop (c::clist, f')
	      | NONE =>  (TextIO.StreamIO.closeIn; clist)
    in  rev(loop ([], f))
    end;


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
| BOOLVAL of int * int * bool;

fun toString TOKEN =
case TOKEN of
UNMINUS(a,b) => "UNMINUS(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| BINADD(a,b) => "BINADD(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| BINSUB(a,b) => "BINSUB(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| BINDIV(a,b) => "BINDIV(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| BINMUL(a,b) => "BINMUL(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| BINMOD(a,b) => "BINMOD(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| NEG(a,b) => "NEG(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| AND(a,b) => "AND(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| OR(a,b) => "OR(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| ASSIGN(a,b) => "ASSIGN(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| EQ(a,b) => "EQ(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| NE(a,b) => "NE(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| LT(a,b) => "LT(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| LTE(a,b) => "LTE(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| GT(a,b) => "GT(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| GTE(a,b) => "GTE(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| LP(a,b) => "LP(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| RP(a,b) => "RP(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| LB(a,b) => "LB(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| RB(a,b) => "RB(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| EOS(a,b) => "EOS(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| COMMA(a,b) => "COMMA(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| INT(a,b) => "INT(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| BOOL(a,b) => "BOOL(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| IF(a,b) => "IF(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| THEN(a,b) => "THEN(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| ELSE(a,b) => "ELSE(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| WHILE(a,b) => "WHILE(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| PROC(a,b) => "PROC(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| PRINT(a,b) => "PRINT(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| READ(a,b) => "READ(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
| ERROR(a,b,c)=>"ERROR(" ^ Int.toString a ^ "," ^ Int.toString b ^ "," ^ Int.toString c ^")\n"
| INTLIT(a,b,c)=>"INTLIT(" ^ Int.toString a ^ "," ^ Int.toString b ^ "," ^ Int.toString c ^")\n"
| IDENT(a,b,c)=>"IDENT(" ^ Int.toString a ^ "," ^ Int.toString b ^ "," ^ c ^ ")\n"
| BOOLVAL(a,b,c)=>"BOOLVAL("^ Int.toString a ^ "," ^ Int.toString b ^ "," ^ Bool.toString c ^")\n";

fun printtoken (outfile:string , t:TOKEN) = 
     TextIO.output ( TextIO.openAppend outfile , toString(t) );

fun printtoken2 =
	let
     val writestream = TextIO.openAppend "output.txt"; 
     in
     TextIO.output (writestream, "My message"); 
     TextIO.closeOut writestream;
     end;