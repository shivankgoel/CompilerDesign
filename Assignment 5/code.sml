signature BigInt =
sig
type bigint=int list
val getbigint: int -> bigint
val bi2str : bigint -> string
val str2bi : string -> bigint
val lt : bigint * bigint -> bool
val leq : bigint * bigint -> bool
val gt : bigint * bigint -> bool
val geq : bigint * bigint -> bool
val eq : bigint * bigint -> bool
val neq : bigint * bigint -> bool
val div4bigint : bigint * bigint -> bigint
val mul : bigint * bigint -> bigint
val add : bigint * bigint -> bigint
val sub : bigint * bigint -> bigint
val mod4bigint : bigint * bigint -> bigint
val unminus : bigint -> bigint
end;

structure BigInt : BigInt = 
struct
	
	type bigint = int list;
	
	fun getbigint ( a:int ) =
		let 
		val str = Int.toString(a);
		val l1 = explode(str);
		fun map f [] = []
        | map f (x::xs) = f(x) :: map f xs;
		fun chartoint a = valOf(Int.fromString(Char.toString(a)));
		val mag = if(hd(l1) = #"~") then tl(l1) else l1;
		val intlist = map chartoint mag;
		val ans = if(hd(l1) = #"~") then [1]@intlist else [0]@intlist;
		in
		ans
		end;

	fun bi2str (i:bigint) =
		let 

		fun removezeroes (l)=
				 if(l=[0] orelse l =[]) then [0]
				 else if(hd(l)<>0)then l
				 else removezeroes(tl(l));

				
		fun bi2strtemp ( b:bigint ) = 
		let 
		val mag = tl(b);
		fun map f [] = []
        | map f (x::xs) = f(x) :: map f xs;
		fun inttochar a = chr(a+48);
		val charlist = map inttochar mag;
		val a1 = implode(charlist);
		val ans = if(hd(b)=1) then "~"^a1 else a1;
		in ans
        end;

		in 
			if(hd(i)=0) then bi2strtemp([0]@removezeroes(i)) else bi2strtemp([1]@removezeroes(tl(i)))

		end;

	fun str2bi (a:string)=
			let
				val l1 = explode(a);
				val mag = if(hd(l1) = #"~") then tl(l1) else l1;
				fun map f [] = []
                | map f (x::xs) = f(x) :: map f xs;
		        fun chartoint a = valOf(Int.fromString(Char.toString(a)));
				val intlist = map chartoint mag;
		        val ans = if(hd(l1) = #"~") then [1]@intlist else [0]@intlist;  
			in (ans)
			end ;
			
	fun lt ((a:bigint) ,(b:bigint)) =
			let
				fun removezeroes (l)=
				if(l=[0] orelse l =[]) then [0]
				else if(hd(l)<>0)then l
				else removezeroes(tl(l));

				fun cmpeqlength(a,b)=
				if(a=b)then false
				else if(hd(a)<hd(b))then true 
				else if(hd(a)>hd(b)) then false
				else cmpeqlength(tl(a),tl(b));
				
				fun ltmag a b = 
				if(length(a) > length(b)) then false 
				else if(length(a) < length(b)) then true
				else cmpeqlength(a,b);
				
				val maga = removezeroes(tl(a));
				val magb = removezeroes(tl(b));
				
				fun ans(a,b) = 
				if(maga = [0] andalso magb =[0])then false
				else if(hd(a)=0 andalso hd(b)=0) then (ltmag maga magb)
				else if(hd(a)=1 andalso hd(b)=1) then (ltmag magb maga)
				else if(hd(a)=0 andalso hd(b)=1) then false
				else true;
				
			in(ans(a,b))
			end;

	fun eq ( a:bigint  , b:bigint  ) = 
			let
			fun removezeroes (l)=
				if(l=[0] orelse l =[]) then [0]
				else if(hd(l)<>0)then l
				else removezeroes(tl(l));
				
			val maga = removezeroes(tl(a));
			val magb = removezeroes(tl(b));	
			val ans  = ( hd(a) = hd(b) ) andalso ( maga = magb );
			in
			if(maga=[0] andalso magb=[0]) then true else ans
			end;
			
			
	fun neq( a:bigint , b:bigint ) = not( eq (a,b ) ); 
			
	fun leq (a:bigint , b:bigint )= eq ( a , b ) orelse lt ( a, b);
			
	fun gt ( a:bigint , b:bigint ) = not ( leq ( a, b ) );
			
	fun geq (a:bigint , b:bigint ) = not( lt( a , b ) );
	
	fun unminus a = if(hd(a)=1)
							then (
								[0]@(tl(a)))
							else [1]@(tl(a));
			

	fun add (a:bigint, b:bigint) =	
			let 
				fun addeq1 (a:bigint , b:bigint , carry:int,  l) = 
					if (length(a) = 0 andalso carry = 0) then l 
					else if (length(a) = 0 andalso carry <> 0) then [carry]@l 
					else if( ( hd(rev(a)) +  hd(rev(b)) + carry )>9 ) then 
						(addeq1 ( rev(tl(rev(a))) , rev(tl(rev(b))) , 1 , ([((hd(rev(a)) +  hd(rev(b)) + carry )-10)]@l) ) )
		            else
		            	(addeq1 (rev(tl(rev(a))) , rev(tl(rev(b))) , 0 , ([((hd(rev(a)) +  hd(rev(b)) + carry ))]@l) ) );

		        fun add1 (a:bigint , carry:int , l) = 
		        	if (length(a) = 0 andalso carry = 0) then l 
					else if (length(a) = 0 andalso carry <> 0) then [carry]@l 
					else if( ( hd(rev(a)) + carry )>9 ) then 
						(add1 ( rev(tl(rev(a))) , 1 , ([((hd(rev(a)) + carry )-10)]@l) ) )
		            else
		            	(add1 (rev(tl(rev(a))) ,  0 , ([((hd(rev(a)) + carry ))]@l) ) );

		        fun getk(l,k:int,l1) =
				if(k=0) then l1
				else getk(tl(l),k-1,l1@[hd(l)]);
		
				fun cutlist (l, c:int) = 
					if(length(l) <= c) then l
					else 
					let 
						val m = rev(l);
						val x = rev(getk(m,c,[]));
					in (x)
					end;

					
				fun final1(a:bigint , b:bigint) =
							if(length(a)=length(b)) then ([hd(a)]@addeq1(tl(a),tl(b),0,[]))
							else if (length(a)<length(b)) then 
								let
								val a1 = tl(a);
								val b1 = tl(b);
								val first = getk(b1,length(b1)-length(a1),[]);
								val second = cutlist(b1,length(a1));
								val addpost= addeq1(a1,second,0,[]);
								val ans = 
									if (length(addpost)>length(a1)) then add1(first,hd(addpost),[])@cutlist(addpost,length(a1))
									else   first@addpost;
								in [hd(a)]@ans
								end
							else 
								let
								val a1 = tl(a);
								val b1 = tl(b);
								val first = getk(a1,length(a1)-length(b1),[]);
								val second = cutlist(a1,length(b1));
								val addpost= addeq1(b1,second,0,[]);
								val ans = 
									if (length(addpost)>length(b1)) then add1(first,hd(addpost),[])@cutlist(addpost,length(b1))
									else   first@addpost;
								in [hd(a)]@ans
								end;


					fun sub1(a,b) =
						let 
							val n = length a;
							fun subfrom9(l,l1) =
							 if(length(l)=0) then l1 
							 else subfrom9(tl(l),l1@[9-hd(l)]);
							fun extend(b,r) = 
								if(length(b) >= r) then b
								else 
								extend([9]@b,r);
							val b1 = extend(subfrom9(b,[]),length(a));
							val negb1 = add1(b1,1,[]);
							fun cut(l,n) = if(length(l)<=n) then l else tl(l);
							val subans = final1([0]@a,[0]@cut(negb1,n));
						in
							cutlist(subans,n)
						end;

				fun final2(a:bigint,b:bigint) =
					let
					 val sign = if( geq([0]@tl(a),[0]@tl(b)) ) then hd(a) else hd(b);
					 val mag =	if( geq([0]@tl(a),[0]@tl(b)) ) then sub1(tl(a),tl(b)) else sub1(tl(b),tl(a));
					in
				[sign]@mag
				end;

				fun finalans(a,b) = if(hd(a)=hd(b)) then final1(a,b) else final2(a,b);

			in 
			 if(a=[]) then b 
			 else if(b=[]) then a 
			 else finalans(a,b)
		end;

		fun sub (a:bigint, b:bigint) =
		let 
			fun revsign(b) = if(hd(b)=0)then 1 else 0;
		in
			add(a,[revsign(b)]@tl(b))
		end;





	fun mul(a:bigint,b:bigint) =
		let 

			fun multemp(a:bigint,b:bigint)=
			let

				fun check0(b) =
					if(b=[]) then true
					else if(hd(b)=0)then check0(tl(b))
					else false; 

				fun repeatadd(a,b,c) =
					if(check0(b)) then c
					else repeatadd(a, sub(b,[0,1]) , add(a,c) );
			in repeatadd([0]@a,[0]@b,[0])
			end;

			fun mul2(a:bigint,b:int,carry,l) =
			if(a=[] andalso carry=0)then l
			else if(a=[])then [carry]@l
			else if((hd(rev(a))*b+carry)>9) then mul2( rev(tl(rev(a))) , b, ((hd(rev(a))*b+carry)) div 10 , [((hd(rev(a))*b+carry)) mod 10]@l )
			else  mul2( rev(tl(rev(a))) , b, 0 , [((hd(rev(a))*b+carry))]@l );

			fun mulwhole(a,b,l) =
			let
				val newb = if(b<> []) then tl(b) else []; 
				val mulab0 = if(b<>[]) then mul2(a,hd(b),0,[]) else [];
				val templ = tl(add([0]@l,[0]@mulab0));
				val newl = templ@[0];
			in
				if(b = [])then l
				else mulwhole(a,newb, newl)
			end;


			fun mulans(a,b)=
			if(hd(a)=hd(b)) then [0]@rev(tl(rev(mulwhole(tl(a),tl(b),[]))))
			else [1]@rev(tl(rev(mulwhole(tl(a),tl(b),[]))));

		in 

			mulans(a,b)
		end;


		
	fun div4bigint (a:bigint,b:bigint) =
		let

		fun divtemp(a,b) =
		let

			fun removezeroes (l)=
			if(l=[0] orelse l=[]) then [0]
			else if(hd(l)<>0)then l
				 else removezeroes(tl(l));

			fun ltmag(a,b) =
			lt([0]@removezeroes(a),[0]@removezeroes(b));
			
			fun extend0initial(k,b) = 
				if(k<=0) then b else extend0initial(k-1,b@[0]);

			fun findnumber(a,b,k) = 
					if (ltmag([0]@a,mul([0]@b,[0,k]))) then (k-1) else findnumber(a,b,k+1);  

		    fun mulbbyapp(a,b) = tl(mul([0]@b,getbigint(findnumber(a,b,1))));

		    fun getansdiv (a,b,b1,l) =
		    if(b=[] orelse lt([0]@b,[0]@b1)) then l
		    else 
		    	let
		    	  val r = tl(sub([0]@a,[0]@mulbbyapp(a,b)));
		    	  val newl = l@tl(getbigint(findnumber(a,b,1)));
		    in 
		    	getansdiv(r,rev(tl(rev(b))),b1,newl)
		    end;

		in 
			getansdiv(a,extend0initial(length(a)-length (b),b),b,[])
		end;

		fun removezeroes (l)=
			if(l=[0] orelse l=[]) then [0]
			else if(hd(l)<>0)then l
				 else removezeroes(tl(l));


		
		val a1 = tl(a);
		val b1= tl(b);
		val mag = if( lt([0]@a1,[0]@b1) ) then [0] else if(hd(a)<>hd(b)) then tl(add([0]@divtemp(a1,b1),[0,1])) else divtemp(a1,b1);
		val sign = if(hd(a)=hd(b)) then 0 else 1;

		in 

		[sign]@removezeroes(mag)

		end;  

	fun mod4bigint(a,b) =

		let
		 	val q =  div4bigint(a,b);
		 	val qmulb= mul(q,b);
		 	val modans = sub(a,qmulb);
		in
			modans
		end;
			
	

end;





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

