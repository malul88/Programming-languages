b/* Michael Malul 316153865 michaelmalul@campus.technion.ac.il
   Omer Gino 205416209 omergino@campus.technion.ac.il */

/* part1 */

fun chk (c:char) = if c >= #"0" andalso c <= #"9"
  then true
  else false;
fun rc (n,s) = if (n>=size(s)) then true
  else if chk(String.sub(s,n)) then rc(n+1,s)
  else false;
fun isNumber s = rc(0,s);



/* part2 */

fun exp10 n = if n=0 then 1 else 10*exp10(n-1);
fun n2d c = ord(c) - ord(#"0");
fun rc2 (s,n) = if n<1 then 0
else n2d(String.sub(s,n-1)) * exp10(size(s)-n) +rc2(s,n-1);
fun atoi s = rc2(s,size(s));



/* part3 */

  val spaceA = 32;
  val index = 0;
  fun deli x = ord(x) = spaceA;
  val str2list = String.tokens deli;
  fun fixString (str,i) = if i = String.size(str) then str
  else if String.sub(str,i) = #"("
  then fixString(String.substring(str,0,i)^" ( "^String.extract(str,i+1,NONE),i+2)
  else if String.sub(str,i) = #")"
  then fixString(String.substring(str,0,i)^" ) "^String.extract(str,i+1,NONE),i+2)
  else fixString(str, i+1);
  fun tokenize str = str2list(fixString(str,index));
