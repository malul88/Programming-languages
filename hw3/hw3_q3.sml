(* Michael Malul 316153865 michaelmalul@campus.technion.ac.il
   Omer Gino 205416209 omergino@campus.technion.ac.il *)

(* Part 1 *)

datatype 'a AVLTree =
    Nil
  | Br of ((int*('a))*('a AVLTree)*('a AVLTree));

fun legalAVL Nil = true
  | legalAVL (Br (v,l,r))  =
let
    fun depth Nil = 0
      | depth (Br (v,t1,t2)) =
      1 + Int.max (depth(t1), depth(t2))
in
  (depth(l) - depth(r)) <= 1 andalso (depth(l) - depth(r)) >= (~1)
end;


(* Part 2 *)

local
  fun depth Nil = 0
  | depth (Br (v,t1,t2)) =
  1 + Int.max (depth(t1), depth(t2))

  fun legalAVL Nil = true
    | legalAVL (Br (v,l,r))  =
    (depth(l) - depth(r)) <= 1 andalso (depth(l) - depth(r)) >= (~1)

  fun bal_factor(Nil) = 0
  | bal_factor(Br(_,l,r)) = depth(l) - depth(r)

  fun br (v, l, r) =
   Br (v, l, r)

  fun rotate_l (t) =
  case t
    of Br(x, a, Br(y, b, c)) => br(y, br(x, a, b), c)
     | _ => t
  fun rotate_r (t) =
  case t
    of Br(x, Br(y,a,b), c) => br(y, a, br(x, b, c))
     | _ => t
  fun balance Nil = Nil
    | balance (n as Br(v, l, r)) =
  case (bal_factor n, bal_factor l, bal_factor r)
    of ( 2, ~1, _) => rotate_r(br(v, rotate_l l, r))
     | ( 2, _, _)  => rotate_r(n)
     | (~2, _, 1)  => rotate_l(br(v, l, rotate_r r))
     | (~2, _, _)  => rotate_l(n)
     | _ => n


  val cmp = Int.compare

  fun compare (k1,_) (k2,_) = cmp (k1, k2)
in
  fun insert ((a,b), Nil) = Br ((a,b),Nil,Nil)
    | insert ((a,b), (Br (v,left,right))) =
      case compare (a,b) v of
       EQUAL   => Br ((a,b),left,right)
      |LESS    => balance(br(v, insert((a,b),left), right))
      |GREATER => balance(br(v, left, insert((a,b), right)))
end;
