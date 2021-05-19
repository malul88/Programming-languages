/* Michael Malul 316153865 michaelmalul@campus.technion.ac.il
   Omer Gino 205416209 omergino@campus.technion.ac.il */

(* Part 1 *)
	datatype ('a, 'b) heterolist = nil
		| ::: of  'a * ('b, 'a) heterolist;
	infixr 5 ::: ;

(* Part 2 *)
fun build4 (x, one, y, two) =
x:::one:::y:::two:::nil;

(* Part 3 *)
val x:::one:::y:::two:::nil = build4 ("x",1,"y",2)
val ("x",1,"y",2) = (x,one,y,two)

(* Part 4 *)
local
	fun listA nil = []
		| listA (a:::b:::heteroL) = a::listA(heteroL)
	fun listB nil = []
		| listB (a:::b:::heteroL) = b::listB(heteroL)
in
	fun unzip nil = ([],[])
	  | unzip hetero = (listA hetero, listB hetero)
	end;

(* Part 5 *)
local
	fun is_same_lenght (list1, list2) = if length(list1) = length(list2) then true else false
	fun makeHetero([], []) = nil
	  | makeHetero(l1::ls1,l2::ls2) = l1:::l2:::makeHetero(ls1,ls2)
in
	fun zip(list1, list2) =
		if(is_same_lenght(list1,list2)) then makeHetero(list1,list2) else raise Empty
end;
