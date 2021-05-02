/* Michael Malul 316153865 michaelmalul@campus.technion.ac.il
   Omer Gino 205416209 omergino@campus.technion.ac.il */

/* part1 */


fun valAt list index =
    let
      val left = List.take(list, index +1)
    in
      List.last(left)
    end;


fun sumAtIndices list indexes =
    let
      fun valAt list index =
          let
            val left = List.take(list, index +1)
          in
            List.last(left)
          end
      val head = List.hd(indexes)
      val tail = List.tl(indexes)
    in
      if List.null tail then valAt list head
      else valAt list head  + sumAtIndices list tail
    end;

/* part2 */

fun toLower s = let
   fun hlp1low (hs ,s, n) = let
      fun rplace c = let
          fun chkCase (c:char) = if c > #"z" orelse c < #"a" then ((ord c)       -(ord #"A") + (ord #"a")) else ord c
           in
           chr(chkCase(c))
           end;
      in
      if (size(s)>n) then hlp1low(hs^(str(rplace(String.sub(s,n)))),s,n+1)  else hs
       end;
 in
hlp1low(“”,s,0)
end;


fun countOccurrs (s,c)= 
  let
    fun counter n =
      let
        val new_s= toLower(s)
      in
        if (n=size(new_s)) then 0 else if (String.sub(new_s,n)=c) then 1+counter(n+1) else counter(n+1)
      end;
  in
    counter(0)
  end;



fun areAnagrams (s1,s2)= (getAllOccurrs(s1) = getAllOccurrs(s2)w);

/* part3 */
