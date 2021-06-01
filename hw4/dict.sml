(* Michael Malul 316153865 michaelmalul@campus.technion.ac.il
   Omer Gino 205416209 omergino@campus.technion.ac.il *)


datatype ('a, 'b) dictionary = Nil
  | Dict of {key: 'a, value: 'b} list;
exception ItemNotPresent;

(* Part 1 *)

local
    fun enlist Nil = nil
      | enlist (Dict (first)) = first
    fun key {key = k, value = v} = k
in
    fun insert Nil k v = Dict [{key = k, value = v}]
    | insert (Dict (nil)) k v = Dict [{key = k, value = v}]
    | insert (Dict(d :: ds)) k v =
        if k = key(d) then Dict({key = k, value = v} :: ds)
        else Dict(d :: enlist(insert (Dict ds) k v))
end;


(* Part 2 *)

fun find Nil k = raise ItemNotPresent
  | find (Dict (nil)) k = raise ItemNotPresent
  | find (Dict(d :: ds)) k =
    let
      fun key {key = k, value = v} = k
      fun value {key = k, value = v} = v
    in
      if k = key(d) then value(d)
      else find (Dict ds) k
    end;

(* Part 3 *)

fun remove Nil k = raise ItemNotPresent
  | remove (Dict (nil)) k = raise ItemNotPresent
  | remove (Dict(d :: ds)) k =
  let
    fun enlist Nil = nil
      | enlist (Dict (first)) = first
    fun key {key = k, value = v} = k
  in
    if k = key(d) andalso ds = nil then Nil
    else if k = key(d) then Dict(ds)
    else Dict(d:: enlist(remove (Dict ds) k))
  end;

(* Part 4 *)

fun keys Nil = []
  | keys (Dict(nil)) = []
  | keys (Dict(d::ds)) =
  let
    fun key {key = k, value = v} = k
  in
    key(d)::keys(Dict(ds))
  end;

(* Part 5 *)

fun values Nil = []
  | values (Dict(nil)) = []
  | values (Dict(d::ds)) =
  let
    fun value {key = k, value = v} = v
  in
    value(d)::values(Dict(ds))
  end;
