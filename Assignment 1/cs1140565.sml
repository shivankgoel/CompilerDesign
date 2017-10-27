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

fun returni (i,[]) = #" " |
returni (i,(h::t)) = if i=1 then h
					else returni(i-1,t);


fun gettoken( i , l , infile:string ) =
let
val list = getclist(infile)
val c = returni (i,list)
val m = returni (i+1,list)
in
if( (issymbol(c) = false) andalso (c <> #" ") andalso (c <> #"\n"))  then gettoken(i+1,c::l,infile) 
else if ( l <> [] ) then rev(l) 
else if ( c = #":" andalso (m = (#"=") ) ) then [ #":", #"="]
else if ( c = #"<" andalso (m = (#">") ) ) then [ #"<", #">"]
else if ( c = #"&" andalso (m = (#"&") ) ) then [ #"&", #"&"]
else if ( c = #"|" andalso (m = (#"|") ) ) then [ #"|", #"|"]
else if ( c = #"<" andalso (m = (#"=") ) ) then [ #"<", #"="]
else if ( c = #">" andalso (m = (#"=") ) ) then [ #">", #"="]
else [c]
end;


fun isintlit (s:string) =
let 
val x = String.explode s;
fun isnumberlist [] = true|
	isnumberlist (h::t) = (isnumber(h) andalso isnumberlist(t) )
in
   if (s <> "") then (isnumberlist(x)) else false
end;


fun isident s =
let
val x = String.explode s;
fun isnumcharlist [] = true|
	isnumcharlist (h::t) = ( (isnumber(h) orelse isalpha(h)) andalso isnumcharlist(t) )
in
    if (s <> "" ) then (isalpha(hd x)  andalso isnumcharlist(x)) else false
end;

fun output (s:string , x:int ,y:int , outfile ) =
  if ( s = "~" ) then printtoken(outfile,UNMINUS(1,1))
  else if ( s = "+" ) then printtoken(outfile,BINADD(x,y))
  else if ( s = "-" ) then printtoken(outfile,BINSUB(x,y))
  else if ( s = "/" ) then printtoken(outfile,BINDIV(x,y))
  else if ( s = "*" ) then printtoken(outfile,BINMUL(x,y))
  else if ( s = "%" ) then printtoken(outfile,BINMOD(x,y))
  else if ( s = "!" ) then printtoken(outfile,NEG(x,y))
  else if ( s = "&&" ) then printtoken(outfile,AND(x,y))
  else if ( s = "||" ) then printtoken(outfile,OR(x,y))
  else if ( s = ":=" ) then printtoken(outfile,ASSIGN(x,y))
  else if ( s = "=" ) then printtoken(outfile,EQ(x,y))
  else if ( s = "<>" ) then printtoken(outfile,NE(x,y))
  else if ( s = "<" ) then printtoken(outfile,LT(x,y))
  else if ( s = ">" ) then printtoken(outfile,GT(x,y))
  else if ( s = "<=" ) then printtoken(outfile,LTE(x,y))
  else if ( s = ">=" ) then printtoken(outfile,GTE(x,y))
  else if ( s = "(" ) then printtoken(outfile,LP(x,y))
  else if ( s = ")" ) then printtoken(outfile,RP(x,y))
  else if ( s = "{" ) then printtoken(outfile,LB(x,y))
  else if ( s = "}" ) then printtoken(outfile,RB(x,y))
  else if ( s = ";" ) then printtoken(outfile,EOS(x,y))
  else if ( s = "," ) then printtoken(outfile,COMMA(x,y))
  else if( s = "int" ) then printtoken(outfile,INT(x,y))
  else if( s = "bool" ) then printtoken(outfile,BOOL(x,y))
  else if( s = "if" ) then printtoken(outfile,IF(x,y))
  else if( s = "then" ) then printtoken(outfile,THEN(x,y))
  else if( s = "else" ) then printtoken(outfile,ELSE(x,y))
  else if( s = "while" ) then printtoken(outfile,WHILE(x,y))
  else if( s = "proc" ) then printtoken(outfile,PROC(x,y))
  else if( s = "print" ) then printtoken(outfile,PRINT(x,y))
  else if( s = "read" ) then printtoken(outfile,READ(x,y))
  else if( s = "tt" ) then printtoken(outfile,BOOLVAL(x,y,true))
  else if( s = "ff" ) then printtoken(outfile,BOOLVAL(x,y,false))
  else if( isintlit(s) ) then printtoken(outfile,INTLIT(x,y,Option.getOpt(Int.fromString(s),0)))
  else if( isident(s) ) then printtoken(outfile,IDENT(x,y,s))
  else if( s="" orelse s=" " orelse s="\n") then print("") 
  else printtoken(outfile,ERROR(x,y,size(s)));

fun incy ( t, m ) = if( t = "\n"  ) then (m + 1) else m;

fun incx ( t, n ) = if( t = "\n"  ) then 1 else (n + size(t) ) ;

fun main( i:int , size:int , x1:int , y1:int , outfile:string ,infile:string) =
let 
  val t =gettoken(i, [], infile)
  val s = String.implode (t)
  val m = incx (s,x1)
  val n = incy (s,y1)
  val _ = output (s,y1,x1,outfile)
  val _ = print(s ^ "\n");
in 
if i <=size then main ( (i+List.length(t)),size , m , n , outfile , infile) else NONE
end;

fun start (i:int , infile:string , outfile:string ) =
let 
val x1=1;
val y1=1;
val x:int =List.length (getclist(infile))
in 
main(i,x , x1 , y1 , outfile , infile)
end;

val args = CommandLine.arguments();
val infile = returni ( 1, args);
val outfile = return1 (2,args);
val _ = start(1,infile,outfile);

