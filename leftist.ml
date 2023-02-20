(*
Wojciech Weremczuk

Drzewa Lewicowe
 
Typ 'a queue jest typem warunkowym 
- Jeżeli kolejka jest pusta typ przyjmuje wartość Leaf
- W przeciwnym razie tworzymy wierzchołek (Node), który trzyma swoją wartość, wskaźniki do poddrzew oraz wysokość skrajnie prawego liścia
Kolejka priorytetowa jest drzewem, dlatego zamiennie będę posługiwał się określeniami kolejka/drzewo
*)
type 'a queue = Leaf | Node of 'a * 'a queue * 'a queue * int;;

exception Empty;;

(* Utworzenie pustej kolejki *)
let empty = Leaf;;

(* Sprawdzenie czy kolejka jest pusta sprowadza się do sprawdzenia czy jest typu Leaf *)
let is_empty q = 
  match q with
  | Leaf            -> true
  | Node (_,_,_,_)  -> false;;

(* Funkcja budująca drzewo z danego wierzchołka i jego poddrzew, tak aby zachować lewicowość *)
let rec make_a_tree x q1 q2 =
  match (q1, q2) with
  | (_, Leaf) -> Node (x, q1, q2, 1)
  | (Leaf, _) -> Node (x, q2, q1, 1)
  | (Node(_,_,_, height_q1), Node(_,_,_, height_q2)) ->
    if height_q2 > height_q1 then
      make_a_tree x q2 q1   (* Chcemy aby wysokość skrajnie prawego liścia w q2 była niewiększa niż w q1 *)
    else
      Node (x, q1, q2, height_q2 + 1);;

(* Złączenie dwóch kolejek priorytetowych *)
let rec join q1 q2 =
  match (q1, q2) with
  | (_ ,Leaf) -> q1
  | (Leaf, _) -> q2
  | (Node (x,left, right,_), Node (x2,_,_,_)) -> 
    if x > x2 then
      join q2 q1    (* Chcemy aby pierwszym argumentem funkcji join było drzewo z mniejszym wierzchołkiem w korzeniu (lub równym) *)
    else
      let subtree = join right q2 in  (* Złączenie prawego poddrzewa q1 z kolejką q2 *)
      make_a_tree x left subtree      (* W wyniku otrzymamy gotową kolejkę priorytetową *)

(* Dodanie elementu do kolejki jest tym samym co złącznie jej z kolejką zawierającą tylko element x *)
let add x q =
  join q (Node (x, Leaf, Leaf, 1));;

(* Po usunięciu najmniejszego elementu w kolejce pozostanie drzewo utworzone ze złączenia lewego i prawego poddrzewa *)
let delete_min q =
  match q with
  | Leaf                      -> raise Empty
  | Node (x, left, right, _)  -> (x, join left right);;