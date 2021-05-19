

/* Part 1 */
	datatype ('a, 'b) heterolist = nil
		| ::: of 'b * ('a, 'b) heterolist;
	infixr 5 ::: ;

/* Part 2 */
fun build4 (x, one, y, two) =
x:::one:::y:::two:::nil;

/* Part 3 */
val (x,one,y,two) = build4 ("x",1,"y",2)
val ("x",1,"y",2) = (x,one,y,two)

/* Part 4 */

fun unzip l : ('a, 'b) heterolist =
