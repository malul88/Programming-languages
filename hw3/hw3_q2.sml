(* Michael Malul 316153865 michaelmalul@campus.technion.ac.il
   Omer Gino 205416209 omergino@campus.technion.ac.il *)
(* Part 1 *)

fun to_binary y = let
fun findn (0, n) = ~1
| findn (x, n) =let
fun exp2 0 = 1
    | exp2 n = 2*exp2(n-1)
in
if x<exp2(n) then n-1 else findn(x, n+1)
end;
fun frac (~1,l,n) = l
|   frac (x,l,n) = let
 fun exp2 0 = 1
    | exp2 n = 2*exp2(n-1)
in
if (n= ~1) then l else if (x>=exp2(n)) then frac((x - exp2(n)) ,l@[1],n-1) else frac(x ,l@[0], n-1)
end;
in
frac(y,[],findn(y,0))
end;


(* Part 2 *)

fun encode l =let
fun count ([],n) = n div 2
| count (l,n) = if hd(l)=1 then count(tl(l),n+1) else count(tl(l),n-1)
fun replace ([],n,indx) = to_binary(indx)
| replace (l,0,indx) = l@to_binary(indx)
| replace (l,n,indx) = if hd(l)=1 then 0::replace(tl(l),n-1,indx+1) else 1::replace(tl(l),n+1,indx+1)
in
replace(l,count(l,0),0)
end;

(* Part 3 *)

fun decode (list, 0) = []
  | decode (list, len) =let
fun get_len ([],n) =n
  | get_len (l,n) = get_len (tl(l),n+1)
fun get_stop (l,0) = l
  | get_stop (l,n) = get_stop (tl(l),n-1)
fun get_start(l,0) = []
  | get_start(l,n) = hd(l)::get_start(tl(l),n-1)
fun from_binary (l,1) = hd(l)
  | from_binary (l,n) = let
fun exp2 0 = 1
    | exp2 n = 2*exp2(n-1)
in
exp2(n-1)*hd(l)+from_binary(tl(l),n-1)
end;
fun replace ([],n) = []
| replace (l,0) = l
| replace (l,n) = if hd(l)=1 then 0::replace(tl(l),n-1) else 1::replace(tl(l),n-1)
in
replace(get_start(list,len),from_binary(get_stop(list,len),get_len(list,0)-len))
end;
