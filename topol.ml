(* Sortowanie topologiczne *)
(* Wojciech Weremczuk *)

open List

(* Implementacja mapy *)

type ('k, 'v) map =
  | Empty
  | Node of ('k, 'v) map * 'k * 'v * ('k, 'v) map * int

type ('k, 'v) t =
  {
    cmp : 'k -> 'k -> int;
    map : ('k, 'v) map;
  }

let height = function
  | Node (_, _, _, _, h) -> h
  | Empty -> 0

let make l k v r = Node (l, k, v, r, max (height l) (height r) + 1)

let bal l k v r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lv, lr, _) ->
        if height ll >= height lr then make ll lk lv (make lr k v r)
        else
          (match lr with
          | Node (lrl, lrk, lrv, lrr, _) ->
              make (make ll lk lv lrl) lrk lrv (make lrr k v r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rv, rr, _) ->
        if height rr >= height rl then make (make l k v rl) rk rv rr
        else
          (match rl with
          | Node (rll, rlk, rlv, rlr, _) ->
              make (make l k v rll) rlk rlv (make rlr rk rv rr)
          | Empty -> assert false)
    | Empty -> assert false
  else Node (l, k, v, r, max hl hr + 1)

let create cmp = { cmp = cmp; map = Empty }
let empty = { cmp = Stdlib.compare; map = Empty }

let is_empty x = 
	x.map = Empty

let add x d { cmp = cmp; map = map } =
  let rec loop = function
    | Node (l, k, v, r, h) ->
        let c = cmp x k in
        if c = 0 then Node (l, x, d, r, h)
        else if c < 0 then
          let nl = loop l in
          bal nl k v r
        else
          let nr = loop r in
          bal l k v nr
    | Empty -> Node (Empty, x, d, Empty, 1) in
  { cmp = cmp; map = loop map }

let find x { cmp = cmp; map = map } =
  let rec loop = function
    | Node (l, k, v, r, _) ->
        let c = cmp x k in
        if c < 0 then loop l
        else if c > 0 then loop r
        else v
    | Empty -> raise Not_found in
  loop map

let mem x { cmp = cmp; map = map } =
  let rec loop = function
    | Node (l, k, v, r, _) ->
        let c = cmp x k in
        c = 0 || loop (if c < 0 then l else r)
    | Empty -> false in
  loop map

(* W??a??ciwa cz?????? programu *)

exception Cykliczne
(* wyjatek rzucany przez [topol] gdy zaleznosci sa cykliczne *)
  
let topol l = 
  let n = ref 0 in            (* liczba r????nych element??w na wej??ciu, pocz??tkowo ustawiamy na 0, potem aktualizujemy *)
  let indeks = ref empty in   (* mapa przyporz??dkowuj??ca elementom z listy wej??ciowej indeksy *)
  let wartosc = ref empty in  (* mapa pami??taj??ca dla ka??dego indeksu, jakiemu elementowi on odpowiada *)
  iter (fun (a, lp) ->        (* wyznaczenie n, stworzenie map indeks i warto???? *)
      iter (fun x -> 
        if not (mem x !indeks) then(
          n := !n + 1;
          indeks := add x !n !indeks;
          wartosc := add !n x !wartosc;
        )) (a::lp)
  ) l;
  let odw = Array.make (!n + 1) 0 in        (* tablica odwiedzonych *)
  let sasiedzi = Array.make(!n + 1) [] in   (* lista s??siedztwa *)
  let stos = ref [] in
  iter (fun (a, lp) ->        (* utworzenie listy s??siedztwa *)
    let v = find a !indeks in 
    iter (fun x -> 
      let u = find x !indeks in 
      sasiedzi.(v) <- u::(sasiedzi.(v))
    ) lp
  ) l;
  let rec dfs v = 
    odw.(v) <- 1;   (* wej??cie do v zaznaczamy jako 1 *)
    iter (fun u ->
      if odw.(u) = 0 then dfs u                 (* je??eli element nie by?? odwiedzony, to do niego wchodzimy *)
      else if odw.(u) = 1 then raise Cykliczne  (* je??eli weszli??my do elementu i jeszcze z niego nie wyszli??my 
                                                   (czyli odw ma warto???? 1) to znale??li??my cykl *)
    ) sasiedzi.(v);
    odw.(v) <- 2;   (* wyj??cie do v zaznaczamy jako 2 *)
    stos := v::!stos  (* na stosie odk??adamy elementy posortowane topologicznie *)
  in
  for i = 1 to !n do 
    if odw.(i) = 0 then dfs i
  done;
  fold_right (fun v acc -> (find v !wartosc)::acc) !stos []  (* przerobienie listy indeks??w na list?? element??w z wej??cia *)
