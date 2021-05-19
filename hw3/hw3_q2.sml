/* Part 1 */

fun to_binary y =
let
  fun findn (0, n) = ~1
    | findn (x, n) =
  let
    fun exp2 0 = 1
      | exp2 n = 2*exp2(n-1)
  in
      if x < exp2(n) then n-1 else findn(x, n+1)
  end;
  fun frac (~1,l,n) = l
  |   frac (x,l,n) =
    let
  fun exp2 0 = 1
    | exp2 n = 2*exp2(n-1)
      in
  if (n= ~1) then l else if (x>=exp2(n)) then frac((x - exp2(n)) ,l@[1],n-1) else frac(x ,l@[0], n-1)
  end;
in
  frac(y,[],findn(y,0))
end;

/* Part 2 */

fun encode l =let
  fun count ([],n) = n
  | count (l,n) = if hd(l)=1 then count(tl(l),n+1) else count(tl(l),n-1)
  fun replace (l,0)=l
  | replace (l,n) =let

  fun repl1 ([],n,res,indx) = res
  | repl1 (l,0,res,indx) = res@l@to_binary(indx)
  | repl1 (l,n,res,indx) = if hd(l)=1 then repl1(tl(l),n-1,0::res,indx+1) else repl1(tl(l),n,0::res,indx+1)

  fun repl0 ([],n,res,indx) = res
  | repl0 (l,0,res,indx) = res@l@to_binary(indx)
  | repl0 (l,n,res,indx) = if hd(l)=0 then repl0(tl(l),n-1,1::res,indx+1) else repl0(tl(l),n,1::res,indx+1)
    in
      if (n<0) then repl0(l,((~n) div 2),[],0) else repl1(l,n div 2,[],0)
    end;
in
    replace(l,count(l,0))
end;

/* Part 3 */

// TODO
