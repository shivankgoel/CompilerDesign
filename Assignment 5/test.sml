fun lick (filename:string) =
    let val f = TextIO.getInstream(TextIO.openIn filename)
	fun loop (accum: string list, f) =
	    case (TextIO.StreamIO.inputLine f) of 
	        SOME(chunk, f') => loop (chunk::accum, f')
	      | NONE => (TextIO.StreamIO.closeIn f; accum)
            (* esac *)
    in  rev(loop ([], f))
    end;


fun delimiter a = if(a= #" " orelse a= #"\n")then true else false;

fun printlist (s : {key:string,value:string} list ) =
if(s=[]) then TextIO.output ( TextIO.openAppend "ans.txt" , " end " )
else 
let
val _ = TextIO.output ( TextIO.openAppend "ans.txt" , ((#key(hd(s))) ^ "  => " ^ (#value(hd(s))) ^ "\t") );
in
 printlist(tl(s))
end;

fun start( s ,outfile) =
	let 
	val i = lick(s);
	in 
	checkinst(i,[],0,outfile)
end

and getpcthinst (i,pc) =
(
if(pc=0) then hd(i)
else getpcthinst (tl(i),pc-1)
)

and checkinst(i ,s,pc,outfile) =
let
val topinst = getpcthinst(i,pc);
val instelems = if(i<>[]) then String.tokens delimiter topinst else []; 
val first = if(instelems<>[]) then hd(instelems) else "";
val second = if(instelems<>[]) then hd(tl(instelems)) else "";
val third = if(instelems<>[]) then hd(tl(tl(instelems))) else "";
val fourth = if(instelems<>[]) then hd(tl(tl(tl(instelems)))) else "";
in
if(i=[]) then  endofcode(first,second,third,fourth,tl(i),s,pc,outfile) 
else if(first = "DECLARE_INT" )   then declareint(first,second,third,fourth,i,s,pc,outfile)
else if(first = "DECLARE_BOOL" )  then declarebool(first,second,third,fourth,i,s,pc,outfile)
else if(first = "DECLARE_PROC" )  then declareproc(first,second,third,fourth,i,s,pc,outfile)
else if(first = "PRINT" )   then printf(first,second,third,fourth,i,s,pc,outfile)
else if(first = "READ" )    then readf(first,second,third,fourth,i,s,pc,outfile)
else if(first = "CALL" )    then callf(first,second,third,fourth,i,s,pc,outfile)
else if(first = "IF" )       then iff(first,second,third,fourth,i,s,pc,outfile)
else if(first = "GOTO" )     then gotof(first,second,third,fourth,i,s,pc,outfile)
else if(first = "RETURN" )     then returnf(first,second,third,fourth,i,s,pc,outfile)
else if(first = "ASSIGN" )       then assignf(first,second,third,fourth,i,s,pc,outfile)
else if(OPER_BINARY(first) )    then operbinary(first,second,third,fourth,i,s,pc,outfile)
else if(OPER_UNARY(first) )     then operunary(first,second,third,fourth,i,s,pc,outfile)
else  endofcode(first,second,third,fourth,i,s,pc,outfile)
end

and OPER_BINARY(s) =
	(if(s="PLUS" orelse s="MINUS" orelse s="MULT" orelse s="DIV" orelse s="MOD" orelse s="GEQ" orelse s="GT" orelse s="LEQ" orelse s="LT" orelse s="NEQ" orelse s="EQ" orelse s="AND" orelse s="OR") then true else false)


and OPER_UNARY(s) =
	(if(s="UPLUS" orelse s="UMINUS" orelse s="NOT") then true else false)

and declareint(first,second,third,fourth,i,s,pc,outfile) = 
let
val snew = [{key=second,value=""}]@s;
val pcnew = pc+1;
in
checkinst(i,snew,pcnew,outfile)
end

and declarebool(first,second,third,fourth,i,s,pc,outfile) = 
let
val snew = [{key=second,value=""}]@s;
val pcnew = pc+1;
in
checkinst(i,snew,pcnew,outfile)
end

and declareproc(first,second,third,fourth,i,s,pc,outfile) = 
let
val snew = [{key=second,value=third}]@s;
val pcnew = pc+1;
in
checkinst(i,snew,pcnew,outfile)
end

and findkey(k,s) =
(
if (s=[]) then ""
else if ( (#key( hd(s) )) = k ) then (#value( hd(s) ))
else findkey(k,tl(s))
 )
 
 
and findkeypos(k,s,i) =
(
if (s=[]) then ~1
else if ( (#key( hd(s) )) = k ) then i
else findkeypos(k,tl(s),i+1)
)
 
 
and getfirstpartofstack (pos,s,l) =
if(pos=0) then l
else getfirstpartofstack (pos-1,tl(s),l@[hd(s)])

and emptystacktillkey(k:string,s) =
(
if (s=[]) then []
else if ( (#key( hd(s) )) = k ) then s
else emptystacktillkey(k,tl(s))  
)


and replacekey(k,s,newval) =
let
val pos = findkeypos(k,s,0);
val f = getfirstpartofstack(pos,s,[]);
val temp = emptystacktillkey(k,s);
val sec = tl(temp);
val mid = {key = (#key(hd(temp))) , value = newval };
in
f@[mid]@sec
end


and printf(first,second,third,fourth,i,s,pc,outfile) = 
let
val snew = s;
val pcnew = pc+1;
val ans = findkey(second,s);
val _ = print(ans,outfile);
in
checkinst(i,snew,pcnew,outfile)
end


and print (s:string ,outfile ) = 
     TextIO.output ( TextIO.openAppend outfile , (s ^ "\n") )


and read() = (TextIO.inputLine TextIO.stdIn)

and readf(first,second,third,fourth,i,s,pc,outfile) = 
let
val v1 = valOf(read());
val v2 = String.substring(v1,0, size(v1)-1);
val snew = replacekey(fourth,s,v2);
val pcnew = pc+1;
in
checkinst(i,snew,pcnew,outfile)
end

and callf(first,second,third,fourth,i,s,pc,outfile) = 
let
val pcs = findkey(second,s);
val snew = [{key="proc call",value=Int.toString(pc)}]@s;
val pcnew = valOf(Int.fromString(pcs));
in
checkinst(i,snew,pcnew,outfile)
end

and iff(first,second,third,fourth,i,s,pc,outfile) = 
let
val m = findkey(second,s);
val snew = s;
val pcnew = if(m = "tt") then pc+1 else valOf(Int.fromString(third));
in
checkinst(i,snew,pcnew,outfile)
end

and gotof(first,second,third,fourth,i,s,pc,outfile) = 
let
val snew = s;
val pcnew = valOf(Int.fromString(third));
in
checkinst(i,snew,pcnew,outfile)
end

and returnf(first,second,third,fourth,i,s,pc,outfile) = 
let
val stemp = if(findkey("proc call",s) <> "" ) then emptystacktillkey("proc call",s) else [];
val topentry = if(stemp<>[]) then hd(stemp) else {key="",value=""} ;
val pcs = if(stemp<>[]) then valOf(Int.fromString(#value(topentry))) else 0;
val snew = if(stemp<>[]) then tl(stemp) else s;
val pcnew = if(stemp<>[]) then pcs+1 else pc;
in
checkinst(i,snew,pcnew,outfile)
end


and isnumberchar (c:char) =
if(c = #"0" orelse c = #"1" orelse c = #"2" orelse c = #"3" orelse c = #"4" orelse c = #"5" orelse c = #"6" orelse c = #"7" orelse c = #"8" orelse c = #"9") then true
else false

and isnumber(s:string) =
let
fun isnumbertemp(clist) = if(List.filter isnumberchar clist = clist) then true else false;
val m = explode(s);
val x = if(hd(m) = #"~") then tl(m) else m;
in
if(s="") then false else isnumbertemp(x)
end

and assignf(first,second,third,fourth,i,s,pc,outfile) = 
let
val snew = if(second="tt" orelse second="ff" orelse isnumber(second)) then replacekey(fourth,s,second) else  replacekey(fourth,s,findkey(second,s)) ;    
val pcnew = pc+1;
in
checkinst(i,snew,pcnew,outfile)
end

and operbinary(first,second,third,fourth,i,s,pc,outfile) = 
let
val v1 = findkey(second,s);
val v2 = findkey(third,s);

fun inttype (v1,v2,first) =
		if(first = "PLUS") then
		BigInt.bi2str(BigInt.add(BigInt.str2bi(v1),BigInt.str2bi(v2)))
		else if(first = "MINUS") then 
		BigInt.bi2str(BigInt.sub(BigInt.str2bi(v1),BigInt.str2bi(v2)))
		else if(first = "MULT") then 
		BigInt.bi2str(BigInt.mul(BigInt.str2bi(v1),BigInt.str2bi(v2)))
		else if(first = "DIV") then
		BigInt.bi2str(BigInt.div4bigint(BigInt.str2bi(v1),BigInt.str2bi(v2)))
		else if(first = "MOD") then
		BigInt.bi2str(BigInt.mod4bigint(BigInt.str2bi(v1),BigInt.str2bi(v2)))
		else if(first = "NEQ") then 
		(if(v1<>v2) then "tt" else "ff")
		else if(first = "EQ") then
		(if(v1=v2) then "tt" else "ff")
		else if(first = "GEQ") then
		(if( BigInt.geq(BigInt.str2bi(v1),BigInt.str2bi(v2)) ) then "tt" else "ff")
        else if(first = "GT") then
		(if( BigInt.gt(BigInt.str2bi(v1),BigInt.str2bi(v2)) ) then "tt" else "ff")
        else if(first = "LEQ") then
		(if( BigInt.leq(BigInt.str2bi(v1),BigInt.str2bi(v2)) ) then "tt" else "ff")
		else if(first = "LT") then
		(if( BigInt.lt(BigInt.str2bi(v1),BigInt.str2bi(v2)) ) then "tt" else "ff")
		else "" ;

fun booltype (v1,v2,first) =
let 
	    val v11 = if(v1="tt") then true else false;
        val v22 = if(v2="tt") then true else false;
in
        if(first = "NEQ") then (if(v11<>v22) then "tt" else "ff")
        else if(first = "EQ") then (if(v11=v22) then "tt" else "ff")
        else if(first = "AND") then (if(v11 andalso v22) then "tt" else "ff")
        else if(first = "OR") then (if(v11 orelse v22) then "tt" else "ff")
		else if(first = "LT") then (if(v1 = "ff" andalso v2 = "tt") then "tt" else "ff")
		else if(first = "GT") then (if(v1 = "tt" andalso v2 = "ff") then "tt" else "ff")
		else if(first = "LEQ") then (if(v1 = "ff" andalso v2 = "tt") then "tt" else "ff")
		else if(first = "GEQ") then (if(v1 = "tt" andalso v2 = "ff") then "tt" else "ff")
		else ""
end;

val ans1 = if(v1<>"tt" andalso v1 <> "ff") then inttype(v1,v2,first) else booltype(v1,v2,first);

val snew = replacekey(fourth,s,ans1);
val pcnew = pc+1;
in
checkinst(i,snew,pcnew,outfile)
end

and operunary(first,second,third,fourth,i,s,pc,outfile) = 
let

val v1 = findkey(second,s);

fun helperunary (v1,first)  =
     	if(first = "UMINUS") then BigInt.bi2str(BigInt.unminus(BigInt.str2bi(v1)))
		else if(first = "NOT") then (if(v1 ="tt") then "ff" else "tt")
		else ("") ;

val ans1 = helperunary(v1,first);
val snew = replacekey(fourth,s,ans1);
val pcnew = pc+1;
in
checkinst(i,snew,pcnew,outfile)
end

and endofcode(first,second,third,fourth,i,s,pc,outfile) = 
"";


fun finalfun(infile,outfile) = start(infile,outfile);
val args = CommandLine.arguments();
val infile = hd(args);
val outfile = hd(tl(args));
val _ = finalfun(infile,outfile);

