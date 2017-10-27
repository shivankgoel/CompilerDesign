use "getclist.sml";



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


val list = getclist "sample_input.txt";

fun delfirst [] = []|
delfirst (h::t) = t;

val row=0;
val col=0;
val x = [#"\n"];

fun gettokens =
if(list <> [])
val c = hd list;
val list = delfirst list;
val ps = 1;
val ns = getnextstate(c,ps);

if(ns=50) then 
val ns = getnextstate1(c,)











