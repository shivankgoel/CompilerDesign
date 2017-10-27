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


fun issymbol c =
if( c = #"+" orelse
c = #"-" orelse
c = #"~" orelse
c = #"/" orelse 
c = #"*" orelse
c = #"%" orelse
c = #"!" orelse
c = #"&" orelse
c = #"|" orelse
c = #":" orelse
c = #"=" orelse
c = #"<" orelse
c = #">" orelse
c = #"(" orelse
c = #")" orelse
c = #"{" orelse
c = #"}" orelse
c = #";" orelse
c = #"," ) then true
else false;

fun isnumber c =
if( c = #"0" orelse
c = #"1" orelse
c = #"2" orelse
c = #"3" orelse 
c = #"4" orelse
c = #"5" orelse
c = #"6" orelse
c = #"7" orelse
c = #"8" orelse
c = #"9" ) then true
else false;

fun isalpha c =
if( 
c = #"a" orelse
c = #"b" orelse
c = #"c" orelse 
c = #"d" orelse
c = #"e" orelse
c = #"f" orelse
c = #"g" orelse
c = #"h" orelse
c = #"i" orelse
c = #"j" orelse
c = #"k" orelse
c = #"l" orelse
c = #"m" orelse
c = #"n" orelse
c = #"o" orelse
c = #"p" orelse
c = #"q" orelse
c = #"r" orelse
c = #"s" orelse
c = #"t" orelse
c = #"u" orelse
c = #"v" orelse
c = #"w" orelse
c = #"x" orelse
c = #"y" orelse
c = #"z" orelse
c = #"A" orelse
c = #"B" orelse
c = #"C" orelse 
c = #"D" orelse
c = #"E" orelse
c = #"F" orelse
c = #"G" orelse
c = #"H" orelse
c = #"I" orelse
c = #"J" orelse
c = #"K" orelse
c = #"L" orelse
c = #"M" orelse
c = #"N" orelse
c = #"O" orelse
c = #"P" orelse
c = #"Q" orelse
c = #"R" orelse
c = #"S" orelse
c = #"T" orelse
c = #"U" orelse
c = #"V" orelse
c = #"W" orelse
c = #"X" orelse
c = #"Y" orelse
c = #"Z" ) then true
else false;

fun getnextstate ( s , c ) =
if( s = 1 andalso c = #"i" ) then 2 
else if( s = 1 andalso c = #"p" ) then 6 
else if( s = 1 andalso c = #"b" ) then 13
else if( s = 1 andalso c = #"t" ) then 17
else if( s = 1 andalso c = #"e" ) then 22
else if( s = 1 andalso c = #"f" ) then 26
else if( s = 1 andalso c = #"w" ) then 28
else if( s = 1 andalso isnumber(c)) then 33
else if( s = 1 andalso isalpha(c)) then 100

else if( s = 1 andalso issymbol(c)) then 50

else if( s = 2 andalso c = #"f" ) then 3
else if( s = 2 andalso c = #"n" ) then 4
else if( s = 2 andalso isnumber(c)) then 100
else if( s = 2 andalso isalpha(c)) then 100

else if( s = 4 andalso c = #"t" ) then 5
else if( s = 4 andalso isnumber(c)) then 100
else if( s = 4 andalso isalpha(c)) then 100
else if( s = 1 andalso issymbol(c)) then 50

else if( s = 6 andalso c = #"r" ) then 7
else if( s = 6 andalso isnumber(c)) then 100
else if( s = 6 andalso isalpha(c)) then 100

else if( s = 7 andalso c = #"i" ) then 8
else if( s = 7 andalso c = #"o" ) then 11
else if( s = 7 andalso isnumber(c)) then 100
else if( s = 7 andalso isalpha(c)) then 100

else if( s = 8 andalso c = #"n" ) then 9
else if( s = 8 andalso isnumber(c)) then 100
else if( s = 8 andalso isalpha(c)) then 100

else if( s = 9 andalso c = #"t" ) then 10
else if( s = 9 andalso isnumber(c)) then 100
else if( s = 9 andalso isalpha(c)) then 100

else if( s = 11 andalso c = #"c" ) then 12
else if( s = 11 andalso isnumber(c)) then 100
else if( s = 11 andalso isalpha(c)) then 100

else if( s = 13 andalso c = #"o" ) then 14
else if( s = 13 andalso isnumber(c)) then 100
else if( s = 13 andalso isalpha(c)) then 100

else if( s = 14 andalso c = #"o" ) then 15
else if( s = 14 andalso isnumber(c)) then 100
else if( s = 14 andalso isalpha(c)) then 100

else if( s = 15 andalso c = #"l" ) then 16
else if( s = 15 andalso isnumber(c)) then 100
else if( s = 15 andalso isalpha(c)) then 100

else if( s = 17 andalso c = #"t" ) then 18
else if( s = 17 andalso c = #"h" ) then 19
else if( s = 17 andalso isnumber(c)) then 100
else if( s = 17 andalso isalpha(c)) then 100

else if( s = 19 andalso c = #"e" ) then 20
else if( s = 19 andalso isnumber(c)) then 100
else if( s = 19 andalso isalpha(c)) then 100

else if( s = 20 andalso c = #"n" ) then 21
else if( s = 20 andalso isnumber(c)) then 100
else if( s = 20 andalso isalpha(c)) then 100

else if( s = 22 andalso c = #"l" ) then 23
else if( s = 22 andalso isnumber(c)) then 100
else if( s = 22 andalso isalpha(c)) then 100

else if( s = 23 andalso c = #"s" ) then 24
else if( s = 23 andalso isnumber(c)) then 100
else if( s = 23 andalso isalpha(c)) then 100

else if( s = 24 andalso c = #"e" ) then 25
else if( s = 24 andalso isnumber(c)) then 100
else if( s = 24 andalso isalpha(c)) then 100

else if( s = 26 andalso c = #"f" ) then 27
else if( s = 26 andalso isnumber(c)) then 100
else if( s = 26 andalso isalpha(c)) then 100

else if( s = 28 andalso c = #"h" ) then 29
else if( s = 28 andalso isnumber(c)) then 100
else if( s = 28 andalso isalpha(c)) then 100

else if( s = 29 andalso c = #"i" ) then 30
else if( s = 29 andalso isnumber(c)) then 100
else if( s = 29 andalso isalpha(c)) then 100

else if( s = 30 andalso c = #"l" ) then 31
else if( s = 30 andalso isnumber(c)) then 100
else if( s = 30 andalso isalpha(c)) then 100

else if( s = 31 andalso c = #"e" ) then 32
else if( s = 31 andalso isnumber(c)) then 100
else if( s = 31 andalso isalpha(c)) then 100

else if( s = 33 andalso  isnumber(c) ) then 33

else if( s=100  andalso isnumber(c)) then 100
else if( s=100  andalso isalpha(c)) then 100 
else 1;

fun delfirst [] = []|
delfirst (h::t) = t;
