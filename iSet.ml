(* Modyfikacja drzew *)
(* Wojciech Weremczuk *)

(* Typ t jest typem warunkowym. Przyjmuje wartości:
  - Empty (pusty wierzchołek)
  - Node (l, itv, r, h, cnt) (wierzchołek drzewa), gdzie 
    l - lewe poddrzewo, 
    itv = (b, e) - (początek przedziału, koniec przedziału) (przedziały są domknięte),
    r - prawe poddrzewo, 
    h - wysokość poddrzewa, 
    cnt - ilość elementów w poddrzewie *)
type t =
  | Empty
  | Node of t * (int * int) * t * int * int

(* height dostaje drzewo i zwraca jego wysokość *)
let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0

(* count dostaje drzewo i zwraca ilość elementów w nim zawartych (ilość elementów we wszystkich przedziałach) *)
let count = function
  | Node (_, _, _, _, cnt) -> cnt
  | Empty -> 0

(* check_inf dostaje liczbę, która jest co najwyżej dwukrotnością max_int, 
  sprawdza czy nie przekroczyła max_int i zwraca typ bool *)
let check_inf a = 
  if a > 0 then a else max_int

(* length dostaje przedział i zwraca jego długość (z uwzględnieniem tego, że może ona przekraczać max_int) *)
let length (b, e) = 
  check_inf (e - b + 1)

(* suma dwóch liczb nieujemnych *)
let sum2 a b =
  if a = max_int || b = max_int then
    max_int
  else
    check_inf (a + b)

(* suma trzech liczb nieujemnych *)
let sum3 a b c =
  if a = max_int || b = max_int || c = max_int then
    max_int
  else
    sum2 a (sum2 b c)

(* make dostaje dwa drzewa l,r oraz przedział itv i tworzy z nich jedno drzewo *)
let make l itv r = 
  Node (l, itv, r, max (height l) (height r) + 1, sum3 (count l) (count r) (length itv))

(* make_simple dostaje przedział i tworzy drzewo zawierające tylko ten przedział *)
let make_simple itv =
  make Empty itv Empty

(* bal dostaje dwa drzewa l,r oraz przedział itv i tworzy z nich zbalansowane drzewo, 
  czyli takie, że różnica wysokości lewego i prawego poddrzewa jest nie większa niż 1 *)
let rec bal l itv r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 1 then
    match l with
    | Node (ll, litv, lr, _, _) ->
        if height ll >= height lr then bal ll litv (bal lr itv r)
        else
          (match lr with
          | Node (lrl, lritv, lrr, _, _) ->
              bal (bal ll litv lrl) lritv (bal lrr itv r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 1 then
    match r with
    | Node (rl, ritv, rr, _, _) ->
        if height rr >= height rl then bal (bal l itv rl) ritv rr
        else
          (match rl with
          | Node (rll, rlitv, rlr, _, _) ->
              bal (bal l itv rll) rlitv (bal rlr ritv rr)
          | Empty -> assert false)
    | Empty -> assert false
  else make l itv r

(* min_elt dostaje drzewo i zwraca jego najmniejszy element (przedział) lub podnosi wyjątek jeżeli drzewo puste *)
let rec min_elt = function
  | Node (Empty, itv, _, _, _) -> itv
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found

(* remove_min_elt dostaje drzewo i zwraca to samo drzewo bez najmniejszego elementu *)
let rec remove_min_elt = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, itv, r, _, _) -> bal (remove_min_elt l) itv r
  | Empty -> invalid_arg "iSet.remove_min_elt"

(* merge dostaje dwa drzewa i łączy je w jedno zbalansowane drzewo *)
let merge s1 s2 =
  match s1, s2 with
  | Empty, _ -> s2
  | _, Empty -> s1
  | _ ->
      let itv = min_elt s2 in
      bal s1 itv (remove_min_elt s2)

(* empty tworzy puste drzewo *)
let empty = Empty

(* is_empty sprawdza czy drzewo jest puste *)
let is_empty s = 
  s = Empty

(* smaller dostaje int-a x oraz drzewo s i zwraca drzewo składające się tylko z elementów mniejszych niż x *)
let rec smaller x s =
  match s with
  | Empty -> Empty
  | Node (l, (b, e), r, _, _) ->
    if x <= b then 
      smaller x l
    else if x <= e then
      merge l (make_simple (b, x - 1))
    else
      bal l (b, e) (smaller x r)

(* greater dostaje int-a x oraz drzewo s i zwraca drzewo składające się tylko z elementów większych niż x *)
let rec greater y s =
  match s with
  | Empty -> Empty
  | Node (l, (b, e), r, _, _) ->
    if y >= e then
      greater y r
    else if y >= b then
      merge (make_simple (y + 1, e)) r
    else
      bal (greater y l) (b, e) r

(* remove dostaje przedział [x, y] oraz drzewo s i zwraca s z usuniętym przedziałem [x, y] *)
let remove (x, y) s =
  merge (smaller x s) (greater y s)
  
(* add dostaje przedział [x, y] oraz drzewo s i zwraca s z dodanym przedziałem [x, y] *)
let add (x, y) s =
  let rec find_begin x s =
    if x = min_int then x else
    match s with
    | Empty -> x
    | Node (l, (b, e), r, _, _) ->
      if x - 1 < b then find_begin x l
      else if x - 1 <= e then b
      else find_begin x r
  in
  let rec find_end y s =
    if y = max_int then y else 
    match s with
    | Empty -> y
    | Node (l, (b, e), r, _, _) ->
      if y + 1 > e then find_end y r
      else if y + 1 >= b then e
      else find_end y l
  in
  let x = find_begin x s in 
  let y = find_end y s in 
  bal (smaller x s) (x, y) (greater y s)

(* mem dostaje int-a x oraz drzewo s i sprawdza czy x występuje w jakimś przedziale znajdującym się w s *)
let rec mem x s =
  match s with
  | Empty -> false
  | Node (l, (b, e), r, _, _) ->
    if x < b then mem x l
    else if x <= e then true
    else mem x r

(* iter dostaje funkcję f oraz drzewo s i aplikuje f do wszystkich przedziałów s w kolejności rosnącej *)
let rec iter f s =
  match s with
  | Empty -> ()
  | Node (l, itv, r, _, _) ->
    iter f l; f itv; iter f r

(* fold dostaje funkcję f, drzewo s oraz akumulator acc i zwraca wynik złożenia funkcji [(f xN ... (f x2 (f x1 a))...)], 
  gdzie x1, ..., xN to przedziały z s w kolejności rosnącej *)
let rec fold f s acc =
  match s with
  | Empty -> acc
  | Node (l, itv, r, _, _) ->
    fold f r (f itv (fold f l acc))

(* elements dostaje drzewo i zwraca listę wszystkich jego przedziałów w kolejności rosnącej *)
let rec elements s = 
  let rec loop s acc = 
    match s with
    | Empty -> acc
    | Node (l, itv, r, _, _) -> 
      loop l (itv :: (loop r acc)) 
  in
  loop s []

(* below dostaje int-a x oraz drzewo s i zwraca ilość elementów w przedziałach s mniejszych bądź równych x *)
let below x s =
  let rec loop s acc =
    match s with
    | Empty -> acc
    | Node (l, (b, e), r, _, _) -> 
      if x < b then loop l acc
      else if x <= e then sum3 acc (length (b, x)) (count l)
      else loop r (sum3 acc (length (b, e)) (count l))
  in
  loop s 0

(* split dostaje int-a x oraz drzewo s i zwraca trójkę: 
  - drzewo zawierające elementy mniejsze od x
  - boola, czy x należy do drzewa s
  - drzewo zawierające elementy większe od x *)
let split x s = 
  ((smaller x s), (mem x s), (greater x s))