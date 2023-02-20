(* Przelewanka *)
(* Wojciech Weremczuk *)

let przelewanka szklanki =
  let odwiedzone = Hashtbl.create 100 in    (* tablica haszująca trzymająca odwiedzone konfiguracje *)
  let kolejka = Queue.create () in          (* kolejka trzymająca konfiguracje oczekujące na rozpatrzenie,
                                               konfiguracje są postaci (stan, czas) *)
  
  (* Funkcje pomocnicze *)
  let rec oblicz_nwd a b =                  (* funkcja wyliczająca najmniejszą wspólną wielokrotność dwóch liczb *)
    if b = 0 then a else oblicz_nwd b (a mod b)
  in

  let wyrzuc_zera wejscie =                 (* funkcja usuwająca z tablicy pary (0, 0) *)
    let ile_roznych_od_0 = ref 0 in 
    Array.iter (fun x -> 
      if x <> (0, 0) then incr ile_roznych_od_0
    ) wejscie;
    let szklanki = Array.make !ile_roznych_od_0 (0, 0) in 
    let poz = ref 0 in 
    Array.iter (fun x -> 
      if x <> (0, 0) then (
        szklanki.(!poz) <- x;
        incr poz
      )
    ) wejscie;
    szklanki
  in

  let z_gory_niemozliwe szklanki =  (* funkcja sprawdzająca przypadki szczególne - niezgodne nwd, brak pełnej/pustej szklanki wśród oczekiwanych pojemności *)
    let nwd = ref (fst szklanki.(0)) in 
    for i = 1 to (Array.length szklanki) - 1 do 
      nwd := oblicz_nwd !nwd (fst szklanki.(i))
    done;
    let czy_niemozliwe = ref false in 
    let war_kon = ref false in      (* warunek konieczny - jakaś szklanka musi pozostać pełna/pusta *)
    Array.iter (fun (x, y) -> 
      if y mod !nwd <> 0 then 
        czy_niemozliwe := true;
      if y = x || y = 0 then 
        war_kon := true;
    ) szklanki;
    !czy_niemozliwe || (not !war_kon)
  in 

  let dodaj pom czas =      (* funkcja pomocnicza, sprawdza czy dana konfiguracja "pom" znajduje się już wśród odwiedzonych, jeżeli nie, to ją dodajemy *)
    if not (Hashtbl.mem odwiedzone pom) then(
      Queue.add (pom, czas + 1) kolejka;
      Hashtbl.add odwiedzone pom true;
    );
    ()
  in
  
  let szklanki = wyrzuc_zera szklanki in        (* pozbywamy się par (0, 0), gdyż nic z nimi nie możemy zrobić *)
  let n = Array.length szklanki in              (* ilość niezerowych szklanek (mających dodatnią pojemność) *)
  if n = 0 then 0                               (* gdy nie ma niezerowych szklanek, to wynik jest równy 0, nic nie musimy robić *)
  else if z_gory_niemozliwe szklanki then -1    (* nie da się otrzymać wyniku, gdyż nie zgadza się nwd albo żadna szklanka nie jest pełna/pusta *)
  else
    let x = Array.init n (fun i -> fst szklanki.(i)) in   (* tablica zawierająca pojemności szklanek *)
    let y = Array.init n (fun i -> snd szklanki.(i)) in   (* tablica zawierająca oczekiwaną ilość wody w szklankach *)
    let wynik = ref (-1) in
    let stan_pocz = Array.make n 0 in 
    Hashtbl.add odwiedzone stan_pocz true;
    Queue.add (stan_pocz, 0) kolejka;

    while (!wynik = -1 && (not (Queue.is_empty kolejka))) do (* wykonujemy algorytm dopóki nie znajdziemy wyniku albo przejrzymy wszystkie konfiguracje *)
      let (stan, czas) = Queue.take kolejka in 
      if stan = y then            (* sprawdzamy czy znaleźliśmy się w końcowej konfiguracji *)
        wynik := czas
      else
        for i = 0 to n - 1 do 
          if stan.(i) > 0 then (
            let pom = Array.init n (fun i -> stan.(i)) in 
            pom.(i) <- 0;
            dodaj pom czas;       (* wylanie wody ze szklanki *)
            for j = 0 to n - 1 do 
              if i <> j && stan.(j) < x.(j) then(
                let ile = min stan.(i) ((x.(j)) - stan.(j)) in 
                let pom = Array.init n (fun i -> stan.(i)) in 
                pom.(i) <- stan.(i) - ile;
                pom.(j) <- stan.(j) + ile;
                dodaj pom czas;   (* przelanie wody ze szklanki i do szklanki j *)
              )
            done
          );
          if stan.(i) < x.(i) then (
            let pom = Array.init n (fun i -> stan.(i)) in 
            pom.(i) <- x.(i);
            dodaj pom czas;       (* nalanie wody do szklanki *)
          )
        done
    done;
    !wynik;;