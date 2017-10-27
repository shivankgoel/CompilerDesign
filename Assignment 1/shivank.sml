use "functions.sml";

fun returni (i,[]) = #" " |
returni (i,(h::t)) = if i=1 then h
					else returni(i-1,t);


fun gettoken( i , l ) =
let
val c = returni (i,getclist("input.txt"))
in
if( issymbol(c) = false andalso (c <> #" ") andalso (c <> #"\n"))  then gettoken(i+1,c::l) 
else if ( l <> [] ) then rev(l)
else if ( c = #":" andalso (returni (i+1,getclist("input.txt")) = #"=" ) ) then [ #":", #"="]
else if ( c = #"<" andalso (returni (i+1,getclist("input.txt")) = #">" ) ) then [ #"<", #">"]
else if ( c = #"&" andalso (returni (i+1,getclist("input.txt")) = #"&" ) ) then [ #"&", #"&"]
else if ( c = #"|" andalso (returni (i+1,getclist("input.txt")) = #"|" ) ) then [ #"|", #"|"]
else if ( c = #"<" andalso (returni (i+1,getclist("input.txt")) = #"=" ) ) then [ #"<", #"="]
else if ( c = #">" andalso (returni (i+1,getclist("input.txt")) = #"=" ) ) then [ #">", #"="]
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

fun output (s:string , x:int , y:int ) =
  if ( s = "~" ) then printtoken("output.txt",UNMINUS(1,1))
  else if ( s = "+" ) then printtoken("output.txt",BINADD(x,y))
  else if ( s = "-" ) then printtoken("output.txt",BINSUB(x,y))
  else if ( s = "/" ) then printtoken("output.txt",BINDIV(x,y))
  else if ( s = "*" ) then printtoken("output.txt",BINMUL(x,y))
  else if ( s = "%" ) then printtoken("output.txt",BINMOD(x,y))
  else if ( s = "!" ) then printtoken("output.txt",NEG(x,y))
  else if ( s = "&&" ) then printtoken("output.txt",AND(x,y))
  else if ( s = "||" ) then printtoken("output.txt",OR(x,y))
  else if ( s = ":=" ) then printtoken("output.txt",ASSIGN(x,y))
  else if ( s = "=" ) then printtoken("output.txt",EQ(x,y))
  else if ( s = "<>" ) then printtoken("output.txt",NE(x,y))
  else if ( s = "<" ) then printtoken("output.txt",LT(x,y))
  else if ( s = ">" ) then printtoken("output.txt",GT(x,y))
  else if ( s = "<=" ) then printtoken("output.txt",LTE(x,y))
  else if ( s = ">=" ) then printtoken("output.txt",GTE(x,y))
  else if ( s = "(" ) then printtoken("output.txt",LP(x,y))
  else if ( s = ")" ) then printtoken("output.txt",RP(x,y))
  else if ( s = "{" ) then printtoken("output.txt",LB(x,y))
  else if ( s = "}" ) then printtoken("output.txt",RB(x,y))
  else if ( s = ";" ) then printtoken("output.txt",EOS(x,y))
  else if ( s = "," ) then printtoken("output.txt",COMMA(x,y))
  else if( s = "int" ) then printtoken("output.txt",INT(x,y))
  else if( s = "bool" ) then printtoken("output.txt",BOOL(x,y))
  else if( s = "if" ) then printtoken("output.txt",IF(x,y))
  else if( s = "then" ) then printtoken("output.txt",THEN(x,y))
  else if( s = "else" ) then printtoken("output.txt",ELSE(x,y))
  else if( s = "while" ) then printtoken("output.txt",WHILE(x,y))
  else if( s = "proc" ) then printtoken("output.txt",PROC(x,y))
  else if( s = "print" ) then printtoken("output.txt",PRINT(x,y))
  else if( s = "read" ) then printtoken("output.txt",READ(x,y))
  else if( s = "tt" orelse s = "ff" ) then printtoken("output.txt",BOOLVAL(x,y,true))
  else if( isintlit(s) ) then printtoken("output.txt",INTLIT(x,y,Option.getOpt(Int.fromString(s),0)))
  else if( isident(s) ) then printtoken("output.txt",IDENT(x,y,s))
  else if( s="" orelse s=" " orelse s="\n") then print("") 
  else printtoken("output.txt",ERROR(x,y,size(s)));

fun incy ( t, m ) = if( t = "\n"  ) then (m + 1) else m;

fun incx ( t, n ) = if( t = "\n"  ) then 1 else (n + size(t) ) ;

fun main( i:int , size:int , x1:int , y1:int) =
let 
  val t =gettoken(i,[])
  val s = String.implode (t)
  val m = incx (s,x1)
  val n = incy (s,y1)
  val _ = output (s,y1,x1)
in 
if i <=size then main ( (i+List.length(t)),size , m , n ) else NONE
end;

fun start (i:int) =
let 
val x1=1;
val y1=1;
val x:int =List.length (getclist("input.txt"))
in 
main(i,x , x1 , y1)
end;
