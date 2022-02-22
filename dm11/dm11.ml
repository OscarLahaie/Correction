(*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*)
(* NOM :  LAHAIE                                                    *)
(* PrÃ©nom : Oscar                                                  *)
(*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*)

(*** I Tris de tableaux ***)

(** Question 1 **)

let echange tab i j =
  if i >= 0 && j >= 0 && j <= Array.length tab -1 && i <= Array.length tab -1 then
    begin
      let tmp = tab.(i) in
      tab.(i) <- tab.(j);
      tab.(j) <- tmp
    end 
  else failwith "Mauvais indices";
;;

(** Question 2 **)
let indice_mini tab depart = 
  let min = ref (tab.(depart)) in
  let imin = ref depart in
  for i = depart to Array.length tab - 1 do
    if !min > tab.(i) then
      begin 
        min := tab.(i);
        imin := i;
      end;
  done;
  !imin
;;


let tri_selection tab =
  let taille = Array.length tab in
  for i = 0 to taille - 2 do
  echange tab i (indice_mini tab (i));
  done;
;;

let tri_insertion tab =
  let taille = Array.length tab in
  for i = 0 to taille - 1 do
    let p = ref i in
    while !p>0 && tab.(!p-1) > tab.(!p) do
      echange tab !p (!p-1);
      decr p
    done;
  done;
;;

(** Question 3 **)


(* Partitionne tab[i..j] avec i < j suivant le pivot t.(i) et renvoie
l'indice index du pivot dans le tableau partionnÃ© (t.(i) <= t.(index)
pour i < index et t.(i) > t.(index) pour i > index. *)

let partition tab i j =
  let pivot = tab.(i) in
  let ipivot = ref i in
  for d = i + 1 to j do
  if tab.(d) <= pivot then
    begin
      if d == !ipivot + 1 then
        let () = echange tab !ipivot d in incr ipivot
      else 
        let () = echange tab (!ipivot + 1) d in
        let () = echange tab !ipivot (!ipivot + 1) in incr ipivot
      end;
  done;
  !ipivot
;;

(** Question 4 **)

(* Tri tab[i..j] *)
let rec tri_rapide_aux tab i j =
  if j-i < 1 then ()
  else 
    begin 
      let ipivot = partition tab i j in
      tri_rapide_aux tab i (ipivot - 1);
      tri_rapide_aux tab (ipivot + 1) j;
    end
;;

(** Question 5 **)

let tri_rapide tab =
  if Array.length tab = 0 then ()
  else 
    tri_rapide_aux tab 0 (Array.length tab - 1);
    tab
;;

(*** II Autour des permutations ***)

(** Question 6 **)

let est_permutation sigma = 
  let taille = Array.length sigma in
  let tab = Array.make taille 0 in
  let rec test i = 
    if i = taille then true
    else 
      if sigma.(i) < taille && sigma.(i) >= 0 then
        begin
          tab.(sigma.(i)) <- 1 + tab.(sigma.(i));
          test (i+1)
        end
      else false;
  in 
  if test 0 then
    let rec parcours i =
      if i = taille then true
      else
        if tab.(i) = 1 then 
          parcours (i+1)
        else false
    in parcours 0
  else false
;;

(** Question 7 **)

let support sigma =
  let rec aux liste i =
    if i = Array.length sigma then liste
    else if i <> sigma.(i) then aux (i::liste) (i+1)
    else aux liste (i+1)
  in aux [] 0
;;

let support_rev sigma =
  let rec aux liste i =
    if i = -1 then liste
    else if i <> sigma.(i) then aux (i::liste) (i-1)
    else aux liste (i-1)
  in aux [] (Array.length sigma - 1)
;;

(** Question 8 **)

let compose sigma1 sigma2 =
  let taille = Array.length sigma2 in
  if taille = 0 then sigma2
  else
    begin
      let tab = Array.make taille 0 in
      for i = 0 to taille - 1 do
        tab.(i) <- sigma1.(sigma2.(i));
      done;
      tab
    end;
;;

let compose_bis sigma1 sigma2 =
  let taille = Array.length sigma2 in
  if taille = 0 then sigma2
  else
    begin
      let tab = Array.make taille 0 in
      for i = 0 to taille - 1 do
        tab.(i) <- sigma2.(sigma1.(i));
      done;
      tab
    end;
;;
(** Question 9 **)

let inverse sigma =
  let taille = Array.length sigma in
  let inverse = Array.make taille 0 in
  for i = 0 to taille - 1 do 
  inverse.(sigma.(i)) <- i
  done;
inverse
;;