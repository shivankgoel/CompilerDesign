fun getclistNoNewline  (filename:string) = 
    let val f = TextIO.getInstream(TextIO.openIn filename)
	fun loopNoNewline (clist, f) = 
	    case TextIO.StreamIO.input1 f of
		SOME (#"\n", f') => loopNoNewline (clist, f')
		  | SOME (#"\t", f') => loopNoNewline (clist, f')
		  | SOME (#" ", f') => loopNoNewline (clist, f')
	      | SOME (c, f') => loopNoNewline (c::clist, f')
	      | NONE =>  (TextIO.StreamIO.closeIn; clist)
    in  rev(loopNoNewline ([], f))
    end;
	

fun print (s:string , outfile:string ) = 
     TextIO.output ( TextIO.openAppend outfile , s  );
	 

fun deletestring (a ,pos ,len) = 
let
val first = substring(a,0,pos);
fun getblankstring(s,a) =
if(s<=0) then a
else " "^getblankstring(s-1,a);
val middle = getblankstring(len,"");
val second = String.extract(a,pos+len,NONE);
in 
if(a <> " " ) then (first^middle)^second else first^second
end;

fun deletelocs(a,l,len) =
if (l=[]) then a
else (deletelocs(deletestring(a,hd(l),len),tl(l),len));


fun findstring (a, b ,i) = 
if (size(String.extract(a,i,NONE))<size(b)) then ~1
else if( b="Command"  andalso size(String.extract(a,i,NONE))>=10 ) then  (if(substring(a,i,size(b))=b andalso substring(a,i,10)<>"CommandSeq" ) then i else findstring(a, b ,i+1) ) 
else if( b=",Command"  andalso size(String.extract(a,i,NONE))>=11 ) then  (if(substring(a,i,size(b))=b andalso substring(a,i,11)<>",CommandSeq" ) then i else findstring(a, b ,i+1) ) 
else if(substring(a,i,size(b))=b )then i 
else findstring(a, b ,i+1); 



fun findlocs (a,b,l,i) =
if(findstring (a, b ,i) = ~1 )then l
else findlocs(a,b,[findstring (a, b ,i)]@l,findstring (a, b ,i)+1); 


fun deletethesestrings (a,sl)=
let
val firsts = if(sl<>[]) then hd(sl) else "";
val locs = if(sl<>[]) then findlocs(a,firsts,[],0) else [];
val newa = if(sl<>[]) then deletelocs(a,locs,size(firsts)) else "";
in
if ( sl = [] ) then a else deletethesestrings(newa,tl(sl))
end;


fun mainremovefunction(infile) =
let 
val a = implode(getclistNoNewline(infile));
val l = ["Program",",ProcDecls",",BoolVarDecls",",IntVarDecls",",VarDecls",",VarDef1",",VarDef",",IntExpression",",IntE",",IntT1",",IntT",",IntF1",",IntF",",BoolExpression",",BoolE",",BoolF1",",BoolF",
         ",BoolG1",",BoolG",",BoolH1",",BoolH",",BoolI",",BoolJ",",Command","ProcDecls","BoolVarDecls","IntVarDecls","VarDecls","VarDef1","VarDef","IntExpression","IntE","IntT1","IntT","IntF1","IntF","BoolExpression","BoolE","BoolF1","BoolF",
         "BoolG1","BoolG","BoolH1","BoolH","BoolI","BoolJ","Command",",[EPSILON]","[EPSILON],","[EPSILON]",",EPSILON","EPSILON,","EPSILON",",[EOS]","[EOS],","[EOS]",",EOS","EOS,","EOS",",LB","LB,","LB",",RB","RB,","RB",",LP","LP,","LP",",RP","RP,","RP",
		 ",COMMA","COMMA,","COMMA"];
in
deletethesestrings(a,l)
end;	


fun returnpos (cl,p,i) =
if(cl = [] orelse tl(cl)=[] ) then p
else if (hd(cl) = #"[" andalso hd(tl(cl)) = #"[") then returnpos(tl(cl),p@[i],i+1)
else returnpos(tl(cl),p,i+1);

fun returni (i,[]) = #" " |
returni (i,(h::t)) = if i=0 then h
					else returni(i-1,t);



fun findclosepos(cl,p,i) =
let 
fun findclosepostemp(cl,p,i) =
if ( cl = [] ) then ~1
else if ( i = ~1 ) then p-1 
else if ( returni(p,cl) = #"[" ) then findclosepostemp(cl,p+1,i+1)
else if ( returni(p,cl) = #"]" ) then findclosepostemp(cl,p+1,i-1)
else findclosepostemp(cl,p+1,i);
in
findclosepostemp(cl,p+1,i)
end;


fun getsublist(cl,ip,sl,l) =
if(cl = [] ) then []
else if(sl=0) then l
else getsublist(cl,ip+1,sl-1,l@[returni(ip,cl)]); 


fun replaceposinlist (cl,pos,a) =
let
val a = getsublist(cl,0,pos,[]); 
val b = getsublist(cl,pos+1,length(cl)-pos-1,[]);
in
a@[#" "]@b
end;

fun replacebrackets (cl) =
let
val a = returnpos(cl,[],0);

fun replacebracketstemp (cl,posnsarray,anslist) =
if(posnsarray = [] ) then anslist
else 
let
val opos = hd(posnsarray);
val cpos = findclosepos(cl,opos,0);
val list1 = replaceposinlist (anslist,opos,#" ");
in
replacebracketstemp (cl,tl(posnsarray),replaceposinlist (list1,cpos,#" ") )
end;
in
replacebracketstemp (cl,a,cl)
end;

fun removeblanksinchar (cl,ansl) =
if(cl=[]) then ansl
else if(hd(cl)= #" " ) then removeblanksinchar (tl(cl),ansl)
else removeblanksinchar (tl(cl),ansl@[hd(cl)]);


fun finalfunction(infile,outfile) =
let
val a1 = mainremovefunction(infile);
val a2 = removeblanksinchar(explode (a1) , [] );
val a3 = replacebrackets(a2);
val a4 = removeblanksinchar(a3,[]);
val a5 = implode(a4);
in
print(a5,outfile)
end;

val args = CommandLine.arguments();
val infile = hd(args);
val outfile = hd(tl(args));
val ans = finalfunction(infile,outfile);


