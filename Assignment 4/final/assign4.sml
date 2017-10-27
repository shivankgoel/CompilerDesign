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

structure bigstruct : BigInt = 
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


