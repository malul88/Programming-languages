/* Michael Malul 316153865 michaelmalul@campus.technion.ac.il
   Omer Gino 205416209 omergino@campus.technion.ac.il */

/* part1 */
fun sig1 a = fn b => fn f:('a*'b -> 'b ) => f(a,b);
fun sig2 (x:int, y:real) = fn f:(real ->sring) => true;
fun sig3 f x y z = f x y;
fun sig4 a = fn b => fn x:int => fn y:int => 4;
fun sig5 f = fn a => fn g:('b*'b -> 'c) => g(f(a), f(a));
fun sig6 (x:unit) (y:unit) = 7;
fun sig7 f:('a*'a -> 'a) = fn x => f;
fun sig8 (x:int,(y:string, z:string)) = (x,y,z);

/* part2 */
fun curry f a b = f(a,b);
fun uncurry f(a,b) f(a)(b);
