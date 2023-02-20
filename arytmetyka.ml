(* Arytmetyka *)
(* 
W rozwiązaniu korzystamy z faktu, że wykonując działania na przedziałach zawsze będziemy mieli 
przedział spójny lub różnicę zbioru liczb rzeczywistych i przedziału spójnego. 

Typ wartość przechowuje informację o tym czy przedział jest podwójny (składa się z dwóch spójnych przedziałów) 
oraz początki i końce tych przedziałów.

Jeżeli przedział składa się z jednego spójnego kawałka, to pocz1 = pocz2 i kon1 = kon2
Jeżeli gdzieś po drodze otrzymamy sprzeczność, to będziemy trzymali pocz1 = pocz2 = kon1 = kon2 = nan

Typ przedzial jest typem pomocniczym 
*)

type wartosc = {podwojny : bool; pocz1 : float; kon1 : float; pocz2 : float; kon2 : float}
type przedzial = {pocz : float; kon : float}

let wartosc_dokladna x = 
  {podwojny = false; pocz1 = x; kon1 = x; pocz2 = x; kon2 = x}
let wartosc_dokladnosc x p = 
  let pom1 = x -. (0.01 *. p *. x) in
  let pom2 = x +. (0.01 *. p *. x) in
  {podwojny = false; pocz1 = min pom1 pom2; kon1 = max pom1 pom2; pocz2 = min pom1 pom2; kon2 = max pom1 pom2}
let wartosc_od_do x y = 
  {podwojny = false; pocz1 = x; kon1 = y; pocz2 = x; kon2 = y}
let dwa_przedzialy x y = 
  {podwojny = true; pocz1 = neg_infinity; kon1 = x; pocz2 = y; kon2 = infinity}
let rzeczywiste = 
  {podwojny = false; pocz1 = neg_infinity; kon1 = infinity; pocz2 = neg_infinity; kon2 = infinity}
let sprzeczne = 
  {podwojny = false; pocz1 = nan; kon1 = nan; pocz2 = nan; kon2 = nan}
let zero = 
  {podwojny = false; pocz1 = 0.; kon1 = 0.; pocz2 = 0.; kon2 = 0.}
let stworz_przedzial x y = 
  {pocz = x; kon = y}

let in_wartosc a x = 
  (x >= a.pocz1 && x <= a.kon1) || (x >= a.pocz2 && x <= a.kon2)

let min_wartosc a = 
  min a.pocz1 a.pocz2

let max_wartosc a = 
  max a.kon1 a.kon2

let sr_wartosc a = 
  (min_wartosc a +. max_wartosc a) /. 2.0

let przeciecie a b =
  a.kon >= b.pocz

let czy_nan a = 
  compare a.pocz1 nan = 0

let czy_zero a =
  a.pocz1 = 0. && a.kon1 = 0.

(* dodawanie przedziałów, z których pierwszy jest podwójny a drugi pojedynczy *)
let polacz a b =
  let p1 = {pocz = a.pocz1; kon = a.kon1 +. b.kon1} in
  let p2 = {pocz = a.pocz2 +. b.pocz1; kon = a.kon2} in
  match przeciecie p1 p2 with
  | true  -> wartosc_od_do p1.pocz p2.kon
  | false -> dwa_przedzialy p1.kon p2.pocz

(* wyznaczanie przedziału przeciwnego do danego *)
let przeciwna a =
  {podwojny = a.podwojny; pocz1 = -.a.kon2; kon1 = -.a.pocz2; pocz2 = -.a.kon1; kon2 = -.a.pocz1}

(* mnożenie dwóch floatów *)
let iloczyn x y =
  if x = 0. || y = 0. 
    then 0.
  else x *. y

(* funkcja zwraca wynik dla danego działania - min/max 
   wykorzystywana przez funkcję mnozenie_roznych *)
let minmax1 dzialanie a b =
  (dzialanie (iloczyn a.kon1 b.pocz1)(iloczyn a.kon1 b.kon1), dzialanie (iloczyn a.pocz2 b.pocz1)(iloczyn a.pocz2 b.kon1))

(* funkcja zwraca wynik dla danego działania - min/max 
   wykorzystywana przez funkcję razy *)
let minmax2 dzialanie a b =
  dzialanie (dzialanie (iloczyn a.pocz1 b.pocz1) (iloczyn a.pocz1 b.kon1)) 
            (dzialanie (iloczyn a.kon1 b.pocz1) (iloczyn a.kon1 b.kon1))

(* mnożenie przedziałów, z których pierwszy jest podwójny a drugi pojedynczy *)
let mnozenie_roznych a b =
  if (b.pocz1 <= 0. && b.kon1 >= 0.)
    then rzeczywiste
  else 
    let para_min = minmax1 min a b in
    let para_max = minmax1 max a b in
    if b.pocz1 > 0.
      then dwa_przedzialy (fst para_max) (snd para_min)
    else
      dwa_przedzialy (snd para_max) (fst para_min)

(* sprawdzenie czy przedział podwójny sprowadza się do zbioru liczb rzeczywistych *)
let czy_rzeczywiste prz =
  match prz.kon1 >= prz.pocz2 with
  | true -> rzeczywiste
  | false -> prz

(* odwracanie przedziału *)
let odwrotna a =
  let k1 = 1. /. a.kon1 in
  let p1 = 1. /. a.pocz1 in
  let p2 = 1. /. a.pocz2 in
  if czy_zero a                      (*wartość dokładnie równa zero *)
    then sprzeczne
  else if a.podwojny then
    match (a.kon1, a.pocz2) with    (* przedział podwójny *)
    | (x, y) when x < 0. && y > 0.  -> wartosc_od_do k1 p2
    | (0., _)                       -> wartosc_od_do neg_infinity p2
    | (_, 0.)                       -> wartosc_od_do k1 infinity
    | (_, _)                        -> dwa_przedzialy p2 k1
  else 
    match (a.pocz1, a.kon1) with    (* przedział pojedynczy *)
    | (x, y) when x < 0. && y > 0.  -> dwa_przedzialy p1 k1
    | (0., _)                       -> wartosc_od_do k1 infinity
    | (_, 0.)                       -> wartosc_od_do neg_infinity p1
    | (_, _)                        -> wartosc_od_do k1 p1

let plus a b =
  if czy_nan a || czy_nan b
    then sprzeczne
  else 
    match (a.podwojny, b.podwojny) with
    | (true, true)  -> rzeczywiste
    | (true, _)     -> polacz a b
    | (_, true)     -> polacz b a
    | (_, _)        -> wartosc_od_do (a.pocz1 +. b.pocz1) (a.kon1 +. b.kon1)

let minus a b =
  plus a (przeciwna b)

let razy a b =
  if czy_nan a || czy_nan b
    then sprzeczne
  else if czy_zero a || czy_zero b
    then zero
  else 
    match (a.podwojny, b.podwojny) with
    | (true, true) -> 
      if a.kon1 >= 0. || a.pocz2 <= 0. || b.kon1 >= 0. || b.pocz2 <= 0.
        then rzeczywiste
      else
        let prawy = max (iloczyn a.kon1 b.pocz2) (iloczyn a.pocz2 b.kon1) in
        let lewy = min (iloczyn a.kon1 b.kon1) (iloczyn a.pocz2 b.pocz2) in
        czy_rzeczywiste (dwa_przedzialy prawy lewy)
    | (true, _)   -> czy_rzeczywiste (mnozenie_roznych a b)
    | (_, true)   -> czy_rzeczywiste (mnozenie_roznych b a)
    | (_, _)      -> wartosc_od_do (minmax2 min a b) (minmax2 max a b)

let podzielic a b =
  razy a (odwrotna b)