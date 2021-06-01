(* Michael Malul 316153865 michaelmalul@campus.technion.ac.il
   Omer Gino 205416209 omergino@campus.technion.ac.il *)
exception Undefined;
exception Empty;

fun initEnv () (s:string)= raise Undefined;


fun define (s:string) f a = fn str => if str=s then a
                      else f(str);

fun emptyNestedEnv () = [initEnv ()];

fun pushEnv a list = a::list;

fun popEnv list = tl list;

fun topEnv list = hd list;

fun defineNested s list a =
  let
    val env = topEnv(list)
    val env = define s env a
    val list = popEnv list
  in
    pushEnv env list
  end;

fun find (s:string) [] = raise Empty
  | find s list =
  let
   val env = topEnv(list)
   val tail = popEnv(list)
  in
    env s
    handle Undefined => find s tail
    handle Empty => raise Undefined
  end;
