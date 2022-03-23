(*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*)
(* Nicolas Pécheux <info.cpge@cpge.info>                            *)
(* Friday, 04 February 2022                                         *)
(* http://cpge.info                                                 *)
(*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*)

let u0 = int_of_string Sys.argv.(1)

let make_u n =
  let u = Array.make (n + 1) u0 in
  for k = 1 to n do
    u.(k) <- 15091 * u.(k - 1) mod 64007
  done;
  u
;;

let u = make_u 6001
let () = Printf.printf "* Question 1)\n"
let () = Printf.printf "  a) %d\n" u.(10)
let () = Printf.printf "  b) %d\n" u.(100)
let () = Printf.printf "  c) %d\n" u.(1000)

type arbre =
  | F
  | N of arbre * arbre

let rec make_a n =
  if n = 0 then F else N (make_a (u.(2 * n) mod n), make_a (u.((2 * n) + 1) mod n))
;;

(* Rem : cette construction termine bien car, si n > 0, on a toujours
   u.(2*n) mod n < n et u.(2*n + 1) mod n < n *)

(* Rem : on pourrait entièrement se passer de la construction explite
   des arbres puisque l'on ne va travailler qu'avec les adresses, mais
   les premières questions me semblent plus intuitives avec et cela ne
   fait pas perdre de temps *)

let a10 = make_a 10
let a100 = make_a 100
let a500 = make_a 500
let a1000 = make_a 1000
let a3000 = make_a 3000

let rec fold f e a =
  match a with
  | F -> e
  | N (g, d) -> f (fold f e g) (fold f e d)
;;

let taille = fold (fun g d -> 1 + g + d) 1
let nb_feuilles = fold ( + ) 1

(* Rem : on peut utiliser la relation n = 2f - 1 pour ne calculer que
   l'un ou l'autre *)

let () = Printf.printf "* Question 2)\n"
let () = Printf.printf "  a) %d, %d\n" (taille a10) (nb_feuilles a10)
let () = Printf.printf "  b) %d, %d\n" (taille a100) (nb_feuilles a100)
let () = Printf.printf "  c) %d, %d\n" (taille a1000) (nb_feuilles a1000)

(* On créer et remplit un tableau m avec m_i = m.(i) *)
let make_m a =
  let n = taille a in
  let m = Array.make n (-1) in
  (* Remplit le tableau m avec le sous-arbre a à partir de l'indice i
     et renvoie m.(i) qui est donc l'indice juste avant lequel il
     faudra insérer par la suite, ce que l'on utilise ci-dessous *)
  let rec fill a i =
    match a with
    | F ->
      m.(i) <- i;
      m.(i)
    | N (g, d) ->
      m.(i) <- fill d (fill g (i + 1) + 1);
      m.(i)
  in
  ignore @@ fill a 0;
  m
;;

let m10 = make_m a10
let m100 = make_m a100
let m500 = make_m a500
let m1000 = make_m a1000
let m3000 = make_m a3000
let () = Printf.printf "* Question 3)\n"
let () = Printf.printf "  a) %d\n" m10.(3)
let () = Printf.printf "  b) %d\n" m100.(9)
let () = Printf.printf "  c) %d\n" m1000.(30)

(* On remarque que les noeuds du sous-arbre d'un noeud d'adresse (li,
   mi) ont pour pour adresse (lj, mj) avec li <= lj <= mj <= mi. Un
   noeud ni est une feuille si et seulement si les deux composantes de
   son adresse sont égales, c'est-à-dire li = mi. Le fils gauche d'un
   noeud d'adresse (li, mi) a pour adresse (li + 1, m_(li + 1)) et son
   fils droit a pour adresse (m_(li + 1) + 1, mi). Pour trouver son
   chemin d'un noeud ni d'adresse (li, mi) vers un noeud nj d'adresse
   (lj, _) on procède comme suit a) si li = lj c'est fini ; b) si ni a
   un fils gauche nk d'adresse (lk, mk) et que lk <= lj <= mk on
   descend vers nk et on continue récursivement c) sinon on procède de
   même pour l'éventuel fils droit; d) sinon on remonte vers le père.
   *)

(* Comme ici on ne sait pas remonter vers le père, on va partir du
   plus proche ancêtre commun nk et ajouter les profondeurs des deux
   chemins, en réalité la somme des profondeurs moins un, car on veut
   le nombre de noeuds traversés *)

(* On suppose que lj est dans le sous-arbre de racine li *)
let rec profondeur m li lj =
  assert (li <= lj && lj <= m.(li));
  if li = lj
  then 0
  else if lj <= m.(li + 1)
  then 1 + profondeur m (li + 1) lj
  else 1 + profondeur m (m.(li + 1) + 1) lj
;;

(* On suppose que lj et li sont dans le sous-arbre de racine lk *)
let rec distance m lk li lj =
  assert (lk <= li && li <= m.(lk) && lk <= lj && lj <= m.(lk));
  if lk = li
  then profondeur m li lj
  else if lk = lj
  then profondeur m lj li (* Cas tous les deux à gauche *)
  else if li <= m.(lk + 1) && lj <= m.(lk + 1)
  then distance m (lk + 1) li lj (* Cas tous les deux à droite *)
  else if li > m.(lk + 1) && lj > m.(lk + 1)
  then distance m (m.(lk + 1) + 1) li lj (* lk est le plus petit ancêtre commun *)
  else profondeur m lk li + profondeur m lk lj - 1 (* -1 car traversés *)
;;

let () = Printf.printf "* Question 4)\n"
let () = Printf.printf "  a) %d\n" @@ distance m10 0 3 9
let () = Printf.printf "  b) %d\n" @@ distance m100 0 5 30
let () = Printf.printf "  c) %d\n" @@ distance m1000 0 30 90

(* Il y a une erreur dans l'énoncé, k va de 0 à ceil(n / p) exclu et
   donc jusqu'à ceil(n / p) - 1. De plus n est ambigu. Il faut
   comprendre que c'est la taille de l'arbre et non pas l'indice de
   A_n *)

let moy_et_max_cartographie_simple m p =
  (* Nombre de cartes nécessaires sachant que l'on en a déjà nb et que
     la dernière carte contient li. Il en faut donc une de plus si lj
     n'est pas dans la même carte que li *)
  let ajout_carte li lj nb = nb + if li / p = lj / p then 0 else 1 in
  let rec nb_cartes li nb =
    (* Renvoie le coût max, la somme des coûts et le nombre de
       feuilles du sous-arbre enraciné en ni ayant déjà besoin de nb
       cartes *)
    if li = m.(li)
    then (* Feuille *)
      nb, nb, 1
    else (
      let lg = li + 1 in
      let ld = m.(lg) + 1 in
      let max_g, sum_g, f_g = nb_cartes lg (ajout_carte li lg nb) in
      let max_d, sum_d, f_d = nb_cartes ld (ajout_carte li ld nb) in
      max max_g max_d, sum_g + sum_d, f_g + f_d)
  in
  let m, s, f = nb_cartes 0 1 in
  float_of_int s /. float_of_int f, m
;;

(* La complexité est linéaire avec un seul parcours de l'arbre *)

let ( ||> ) (a, b) f = f a b
let () = Printf.printf "* Question 5)\n"
let () = moy_et_max_cartographie_simple m10 3 ||> Printf.printf "  a) %.3f %d\n"
let () = moy_et_max_cartographie_simple m100 6 ||> Printf.printf "  b) %.3f %d\n"
let () = moy_et_max_cartographie_simple m1000 9 ||> Printf.printf "  c) %.3f %d\n"

(* L'énoncé ne me semble pas très clair. Voici une autre formulation.

   Soit $A$ un arbre. Parmi toutes les cartographies qui minimisent le
   coût maximal des feuilles (il y a un nombre fini de cartographies
   (combiens ?) donc il en exite en existe au moins une qui minimise
   le coût maximal des feuilles), on considère celles dont la carte
   qui contient la racine est de cardinal minimum. On note alors p(A)
   la taille de la carte qui contient la racine dans ces cartographies
   (qui minimisent donc le coût maximal des feuilles puis la taille de
   la carte qui contient la racine).

   Remarquons que si $A$ contient moins de p noeuds, alors p(A) = p et
   le coût maximal des feuilles est de $1$.

   On reprend les notations de l'énoncé et on note ni la racine de A.
   On considère tout d'abord le cas Cmax(Ag) = Cmax(Ad). Si de plus
   p(Ag) + p(Ad) < p, on peut faire une carte commune avec ni en plus
   et on a Cmax(A) = Cmax(Ag) = Cmax(Ad) et p(A) = p(Ag) + p(Ad) + 1.
   Sinon, comme la racine ne va pouvoir au mieux faire partie que
   d'une carte de Ag ou de Ad, on aura besoin d'une carte de plus d'un
   côté ou de l'autre. Autant mettre ni seul dans sa carte (puisque
   l'on veut minimiser la taille de cette carte). On a alors p(A) = 1
   et Cmax(A) = Cmax(Ad) + 1 = Cmax(Ag) + 1. Sinon, quitte à échanger
   gauche et droite, supposons que Cmax(Ag) > Cmax(Ad). Si p(Ag) < p,
   alors on peut ajouter ni à une carte de cardinal p(Ag) et avoir
   Cmax(A) = Cmax(Ag). On a alors p(A) = p(Ag) + 1 et on ne peut pas
   faire mieux. En revanche si p(Ag) = p, alors ni ne peut faire
   partie d'aucune carte qui conserverait Cmax(Ag) (sinon on aurait pu
   prendre pour cela un des sommets de la carte de taille p(Ag) qui
   est supposée minimal). On a donc Cmax(A) = Cmax(Ag) + 1 et p(A) = 1
   puisque cela convient et que p(A) est alors évidemment minimal. *)

let max_et_min_pA_cartographie_minimise_cout_max m p =
  let rec aux li =
    (* Renvoie Cmax(A) et p(A) pour l'arbre A enraciné en li *)
    if li = m.(li)
    then 1, 1
    else (
      let lg = li + 1 in
      let ld = m.(lg) + 1 in
      let cg, pg = aux lg in
      let cd, pd = aux ld in
      if cg = cd
      then if pg + pd < p then cg, pg + pd + 1 else cg + 1, 1
      else (
        let cm, pm = max (cg, pg) (cd, pd) in
        if pm = p then cm + 1, 1 else cm, 1 + pm))
  in
  aux 0
;;

let () = Printf.printf "* Question 6)\n"

let () =
  max_et_min_pA_cartographie_minimise_cout_max m10 3 ||> Printf.printf "  a) %d %d\n"
;;

let () =
  max_et_min_pA_cartographie_minimise_cout_max m100 6 ||> Printf.printf "  b) %d %d\n"
;;

let () =
  max_et_min_pA_cartographie_minimise_cout_max m1000 9 ||> Printf.printf "  c) %d %d\n"
;;

(* Fusionner deux cartes ne peut que faire diminuer le coût d'une
   cartographie. Ainsi, on peut toujours supposer que la somme des
   cardinaux de deux cartes est strictement plus grand que p, sinon,
   il suffit de fusionner les cartes entre elles (on ne demande pas
   ici que les cartes soient convexes). Il y a donc au plus deux fois
   plus de cartes que le nombre minimal de cartes possibles. En effet,
   notons k = ceil(n / p) le nombre minimal de cartes possibles et k'
   le nombre minimal de cartes d'une cartographie qui minimise le coût
   maximum. En groupant les cartes par deux (et en ignorant une
   éventuelle dernière carte si k' est impair, on a donc n > p *
   floor(k' / 2) donc n / p > floor(k' / 2) ce qui est équivalent à
   ceil(n / p) > k' / 2 et d'où finalement 2 * k > k' *)

(* Donnons un exemple qui montre que cette inégalité n'est pas abusive
   et peut être atteinte, c'est-à-dire avec 2 * k = k' + 1. On peut
   prendre p = 3 et l'arbre suivant, où l'on a :
      C_max(A) = 2, k = 2, k' = 3

        x
       /
      x
     / \
    x   x
   /   /
  x   x

*)

(* ToDo : généraliser *)

(* L'ensemble des cartographies est fini et donc il en existe au moins
   une qui minimise le coût moyen. Soit une telle cartographie d'un
   arbre avec au moins p noeud. Ajouter à la carte contenant la racine
   le fils d'un des noeuds de cette carte (les cartes restent
   convexes) ne peut que faire diminuer le coût moyen et donc on peut
   toujours supposer que la carte contenant la racine est de cardinal
   maximal. On a donc k -> C(ni, k) croissante et on pourra donc
   prendre comme valeur minimal pour le coût moyen des feuilles, pour
   un noeud ni, la valeur C(ni, p) / f(ni) *)

(* On considère un noeud ni de fils ng et nd. On cherche à calculer,
   pour k dans 1..p la valeur C(ni, k). Si la carte qui contient ni
   contient au plus k noeuds (et donc en fait exactement k) alors elle
   en contient au plus g du sous-arbre gauche contenant ng (puisque
   les cartes sont convexes) et au plus d du sous-arbre droit
   contenant nd avec g + d + 1 = k. On n'ajoute ainsi pas de carte
   mais on se contente d'ajouter la racine à une carte de taille au
   plus g fusionnée avec une carte de taille au plus d, le coût moyen
   des feuilles ne change donc pas et le coût est alors C(ng, g) +
   C(nd, d). Il faut cependant que g et d soient des indices
   strictement positifs. Si par exemple, k = 1, alors la racine est
   seule dans sa carte et toutes les feuilles à gauche comme à droite
   vont avoir besoin d'une carte de plus. La somme des coûts à gauche
   va donc être la somme optimale des coûts à gauche, qui est C(ng, p)
   d'après la remarque précédente, auquel s'ajoute f(ng) puisque
   toutes les feuilles ont une carte de plus. De même à droite. Ceci
   se produit également si k > 1 mais si g ou d sont nuls : on choisit
   alors de ne pas prendre une des cartes issue d'un des deux
   sous-arbre dans la carte de la racine. Pour simplifier la formule,
   on note :

   C'(ng, g) = C(ng, g) si g > 0 et C'(ng, g) = C(ng, p) + f(ng) sinon
   et de même pour nd

   Comme on ne sait pas quel choix de g et d est le meilleur, on va
   tous les tester, en conservant le meilleur choix trouvé. Ce qui
   nous donne la formule :

   C(ni, k) = min_{g + d = k - 1 ; g, d >= 0} [C'(ng, g) + C'(nd, d)]

   On peut donc calculer récursivement cette formule pour tous les
   noeuds et pour tout k. Ci-dessous on va parcourir les noeuds par
   indices li décroissants, ce qui garantit d'avoir effectué les
   calculs pour les fils d'un noeud avant le calcul pour le noeud
   lui-même.

*)

(* On calcule pour chaque noeud ni un tableau C(ni, k) avec k dans
   [0..p] où C(ni, 0) contient f(ni) lorsque l'on en a besoin (et
   C(ni, p) + f(ni) ensuite) *)

let moy_cartographie_minimise_moy m p =
  let n = Array.length m in
  let c = Array.make_matrix n (p + 1) 1 in
  for li = n - 1 downto 0 do
    if li <> m.(li)
    then (
      let lg = li + 1 in
      let ld = m.(lg) + 1 in
      c.(li).(0) <- c.(lg).(0) + c.(ld).(0);
      (* On écrase ces valeurs mais on n'en aura plus besoin et c'est
         plus simple pour la suite. Maintenant C(ng, 0) = C'(ng, 0) et
         de même pour nd *)
      c.(lg).(0) <- c.(lg).(0) + c.(lg).(p);
      c.(ld).(0) <- c.(ld).(0) + c.(ld).(p);
      for k = 1 to p do
        c.(li).(k) <- max_int;
        for g = 0 to k - 1 do
          let d = k - 1 - g in
          if c.(li).(k) > c.(lg).(g) + c.(ld).(d)
          then c.(li).(k) <- c.(lg).(g) + c.(ld).(d)
        done
      done)
  done;
  float_of_int c.(0).(p) /. float_of_int c.(0).(0)
;;

(* Complexité en espace O(|a|p) et en temps O(|a|p^2). On peut
   peut-être améliorer mais ça ne semble pas évident. En espace on
   doit pouvoir faire du O(|a| + p) mais ce n'est pas si simple. En
   temps je ne vois pas directement (sinon je l'aurais fait :-)) *)

let () = Printf.printf "* Question 7)\n"
let () = Printf.printf "  a) %.3f\n" @@ moy_cartographie_minimise_moy m10 3
let () = Printf.printf "  b) %.3f\n" @@ moy_cartographie_minimise_moy m100 6
let () = Printf.printf "  c) %.3f\n" @@ moy_cartographie_minimise_moy m1000 9

(* On va procéder de la même manière, en calculant également à chaque
   étape Cmax(ni, k) en utilisant un coût infini pour Cmoy(ni, k) si
   jamais on dépasse le maximum autorisé pour Cmax(ni, k). On aura la
   même complexité en temps et en espace en ordre de grandeur.
   Contrairement au cas précédent, on ne peut plus supposer que la
   carte qui contient contient la racine est maximale, il faut donc
   bien garantir dans la fonction ci-desous que la valeur optimale
   Cmoy(ni, k) ne veut pas dire que la carte qui contient la racine
   contient k noeuds mais "au plus k" noeuds *)

(* Gros mais pas trop pour éviter les dépassements lors de la somme *)
let maxint = max_int / 4

let moy_cartographie_minimise_moy_contrainte_max_cout m p v =
  let n = Array.length m in
  let cmoy = Array.make_matrix n (p + 1) 1 in
  let cmax = Array.make_matrix n (p + 1) 1 in
  for li = n - 1 downto 0 do
    if li <> m.(li)
    then (
      let lg = li + 1 in
      let ld = m.(lg) + 1 in
      (* Mise à jour des feuilles *)
      cmoy.(li).(0) <- cmoy.(lg).(0) + cmoy.(ld).(0);
      (* Même convention : dans la case 0 le coût si on ne prend rien,
         ce qui augmente le coût maximal des feuilles de 1 *)
      cmax.(lg).(0) <- cmax.(lg).(p) + 1;
      cmax.(ld).(0) <- cmax.(ld).(p) + 1;
      (* Valeur infinie si on voudrait augmenter cmax au delà de la
         valeur limite *)
      cmoy.(lg).(0)
        <- (if cmax.(lg).(p) >= v then maxint else cmoy.(lg).(0) + cmoy.(lg).(p));
      cmoy.(ld).(0)
        <- (if cmax.(ld).(p) >= v then maxint else cmoy.(ld).(0) + cmoy.(ld).(p));
      for k = 1 to p do
        (* Initialiser avec la valeur précédente pour garantir la croissance *)
        cmax.(li).(k)
          <- (if k = 1 then max cmax.(lg).(0) cmax.(ld).(0) else cmax.(li).(k - 1));
        cmoy.(li).(k)
          <- (if k = 1 then cmoy.(lg).(0) + cmoy.(ld).(0) else cmoy.(li).(k - 1));
        for g = 0 to k - 1 do
          let d = k - 1 - g in
          if cmoy.(li).(k) > cmoy.(lg).(g) + cmoy.(ld).(d)
          then (
            cmoy.(li).(k) <- cmoy.(lg).(g) + cmoy.(ld).(d);
            cmax.(li).(k) <- max cmax.(lg).(g) cmax.(ld).(d))
        done
      done)
  done;
  float_of_int cmoy.(0).(p) /. float_of_int cmoy.(0).(0)
;;

let premiere_difference m =
  let rec cherche p =
    let cmax = fst (max_et_min_pA_cartographie_minimise_cout_max m p) in
    let cmoy_libre = moy_cartographie_minimise_moy m p in
    let cmoy_cmax = moy_cartographie_minimise_moy_contrainte_max_cout m p cmax in
    (* Cela suffit pour ce sujet, mais il vaudrait mieux travailler
       avec la somme des coûts et ne calculer la moyenne que pour
       l'affichage *)
    if abs_float (cmoy_libre -. cmoy_cmax) > 1e-5
    then p, cmoy_libre, cmoy_cmax
    else cherche (p + 1)
  in
  cherche 3
;;

let ( |||> ) (a, b, c) f = f a b c
let () = Printf.printf "* Question 8)\n"
let () = premiere_difference m500 |||> Printf.printf "  a) %d %.3f %.3f\n"
let () = premiere_difference m1000 |||> Printf.printf "  b) %d %.3f %.3f\n"
let () = premiere_difference m3000 |||> Printf.printf "  c) %d %.3f %.3f\n"
