
/* Part 1 */

datatype 'a AVLTree = Nil | Br of ((int*('a))*('a AVLTree)*('a AVLTree));

fun legalAVL Nil = true
  | legalAVL (t : 'a AVLTree)  = let
    fun right (Br (m,(l:'a AVLTree),(r:'a AVLTree))) = r
    fun left (Br (m,(l:'a AVLTree),(r:'a AVLTree))) = r
    fun depth Nil = 0
      | depth (t : 'a AVLTree) = let
      fun right (Br (_,_,r) : 'a AVLTree)  = r
      fun left (Br (m,(l:'a AVLTree),(r:'a AVLTree))) = r
    in
      if depth(left(t)) > depth(right(t)) then 1 + depth(left(t)) else
      1 + depth(right(t))
    end
in
  (depth(left(t)) - depth(right(t))) <= 1 andalso (depth(left(t)) - depth(right(t))) >= (~1)
end;


/* Part 2 */
// TODO
