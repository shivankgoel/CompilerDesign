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
