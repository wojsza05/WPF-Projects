(* Origami *)
(* Wojciech Weremczuk *)

open List;;
let epsilon = 0.000001;;

(* Punkt na płaszczyźnie *)
type point = float * float

(* Poskładana kartka: ile razy kartkę przebije szpilka wbita w danym punkcie *)
type kartka = point -> int

(* funkcja podnosząca float'a do kwadratu *)
let pot_kw x = x *. x

(* funkcja zwracająca kartkę w kształcie prostokąta *)
let prostokat (p1 : point) (p2 : point) : kartka =
  function (x, y) -> 
    if (x >= fst p1 -. epsilon) && (x <= fst p2 +. epsilon) 
        && (y >= snd p1 -. epsilon) && (y <= snd p2 +. epsilon) then 1 else 0

(* funkcja zwracająca kartkę w kształcie koła *)
let kolko (p : point) r : kartka =
  function (x, y) ->
    if pot_kw (x -. fst p) +. pot_kw (y -. snd p) <= pot_kw r +. epsilon then 1 else 0

(* funkcja wyliczająca iloczyn wektorowy wektorów p1-p2 i p1-p *)
let iloczyn_wektorowy (p1 : point) (p2 : point) (p : point) =
  let x1 = (fst p) -. (fst p1) in 
  let x2 = (fst p2) -. (fst p1) in 
  let y1 = (snd p) -. (snd p1) in 
  let y2 = (snd p2) -. (snd p1) in
  (x1 *. y2) -. (x2 *. y1) 

(* funkcja wyliczająca punkt będący odbiciem symetrcznym punktu (x, y) 
względem prostej przechodzącej przez punkty (x1, y1), (x2, y2) 
y = a*x + b to prosta przechodząca przez punkty (x1, y1), (x2, y2)
y = a1*x + b1 to prosta prostopadła przechodząca przez punkt (x, y)
(xt, yt) to punkt przecięcia tych prostych
(xs, ys) to punkt (x, y) odbity symetrycznie względem prostej y = a*x + b *)
let symetria ((x1, y1) : point) ((x2, y2) : point) ((x, y) : point) : point =
  if x1 = x2 then (2. *. x1 -. x, y) else
  if y1 = y2 then (x, 2. *. y1 -. y) else 
  let a = (y2 -. y1) /. (x2 -. x1) in 
  let b = y1 -. a *. x1 in 
  let a1 = -1. /. a in 
  let b1 = y -. a1 *. x in 
  let xt = (b1 -. b) /. (a -. a1) in 
  let yt = a *. xt +. b in 
  let xs = 2. *. xt -. x in 
  let ys = 2. *. yt -. y in 
  (xs, ys)

(* funkcja składająca kartkę [k] wzdłuż prostej przechodzącej przez punkty [p1] i [p2] *)  
let zloz (p1 : point) (p2 : point) (k : kartka) : kartka =
  function (p : point) ->
    let iloczyn = iloczyn_wektorowy p1 p2 p in 
    if abs_float iloczyn < epsilon then
      k p
    else if iloczyn < 0. then 
      k p + (k (symetria p1 p2 p))
    else 0

(* funkcja składająca kartkę [k] kolejno wzdłuż wszystkich prostych z listy [l] *)    
let skladaj l (k : kartka) : kartka = 
  function (p : point) ->
    (fold_left 
      (fun acc (p1, p2) -> zloz p1 p2 acc) k l) p