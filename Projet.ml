
type utilisateur = {nom1:string ; prenom:string ; age:int};;
type page = {nom:string ; admins:utilisateur list};;
type sommet = Util of utilisateur | Page of page;;


(* 1. Connaître le nombre de sommets, d'arcs *)
let nbsommets = function (l:(sommet*(sommet list))list)->
  let rec aux l res =
    match l with
    |[] -> res
    |h::t -> aux t (res+1)
  in aux l 0;;

let nbarcs = function (l:(sommet*(sommet list))list)->
  let rec aux = fun l res->
    match l with 
    |[] -> res
    |h::t -> match (fst h) with
      |Util u -> aux t (res+(List.length(snd h)))
      |Page p -> aux t res
  in aux l 0;;



(* 2. Obtenir l'ensemble des sommets *)
let ensSom = function (l:(sommet*(sommet list))list)->
  let rec aux l res =
    match l with
    |[] -> res
    |h::t -> aux t ((fst h)::res)
  in aux l [];;

let affEnsSom = function (l: sommet list) ->
  let rec aux l res =
    match l with
    |[] -> res
    |h::t -> match h with
      |Util u -> aux t (u.nom1^"\n"^res)
      |Page p -> aux t (p.nom^"\n"^res)
in aux l "";;

let affEnsUtil = function (l: utilisateur list) ->
  let rec aux l res =
    match l with
    |[] -> res
    |h::t -> aux t (h.nom1^"\n"^res)
in aux l "";;


(* 3. Obtenir l'ensemble des sommets trié par nom *)
let getNom = function (s:sommet) ->
  match s with 
  |Util u -> u.nom1
  |Page p -> p.nom;;

let rec insertParNom = fun (s:sommet) (l:sommet list) -> 
  match l with
  |[] -> s::[]
  |h::t -> match s with
    |Util u -> if (u.nom1 < getNom h) then s::l
    else h::(insertParNom s t)
    |Page p -> if (p.nom < getNom h) then s::l
    else h::(insertParNom s t);;

let rec triParNom = function (l:sommet list) ->
  match l with 
  |[] -> []
  |h::t -> insertParNom h (triParNom t);;

let ensSomNom = function (l:(sommet*(sommet list))list)->
  triParNom (ensSom l);;



(* 4. Obtenir l'ensemble des sommets trié par degré sortant *)
let getDegSor = function (s:(sommet*(sommet list)))->
  List.length (snd s);;

let rec insertParDeg = fun (e:(sommet*(sommet list))) (l:(sommet*(sommet list))list) ->
  match l with
  |[]-> e::[]
  |h::t -> match (fst e) with 
    |Util u -> if getDegSor e <= getDegSor h then e::l
    else h::(insertParDeg e t)
    |Page p -> l@[e];;

let rec triParDeg = function (l:(sommet*(sommet list))list) ->
  match l with 
  |[]->[]
  |h::t -> insertParDeg h (triParDeg t);;

let ensSomDeg = function (l:(sommet*(sommet list))list)->
  ensSom (triParDeg l);;



(* 5. Obtenir l'ensemble des arcs *)
let ensArcs = function (l:(sommet*(sommet list))list) ->
  let rec couplesarcs = fun l e res->
    match l with
    |[] -> res
    |h::t -> couplesarcs t e (((fst e),h)::res)
  in let rec aux = fun res l ->
    match l with
    |[] -> res
    |h::t -> match (fst h) with
      |Util u -> aux (couplesarcs (snd h) h res) t
      |Page p -> aux res t
  in aux [] l ;;

let affEnsArcs = function (l: (sommet*sommet) list) ->
  let rec aux l res =
    match l with
    |[] -> res
    |h::t -> match h with
      |(Util u1,Util u2) -> aux t ("(" ^ u1.nom1 ^ " , " ^ u2.nom1 ^ ")\n" ^ res)
      |(Util u,Page p) -> aux t ("(" ^ u.nom1 ^ " , " ^ p.nom^ "\n" ^ res)
      |(_,_) -> aux t res
  in aux l "";;



(* 6. Ajouter/Supprimer un sommet (et les arcs qui lui sont liés) *)
exception Introuvable;;

let rec supprSom = fun (l:(sommet*(sommet list))list) s ->
  match l with 
  |[] -> raise Introuvable
  |h1::h2::t -> if ( s = fst h1 ) then h2::t
  else if ( s = fst h2 ) then h1::t
  else supprSom t s
  |h::[] -> if (s = (fst h)) then [] 
  else raise Introuvable;;

let ajoutSom = fun (l:(sommet*(sommet list))list) s -> 
  (s,[])::l ;;



(* 7. Ajouter/Supprimer un arc *)
let rec supprOcc =  fun (ls:sommet list) (s:sommet)->
  match ls with 
  |[] -> raise Introuvable
  |h1::h2::t ->if ( s = h1 ) then h2::t
  else if ( s = h2 ) then h1::t
  else supprOcc (h2::t) s
  |h::[]-> if (s=h) then [] else raise Introuvable;;

let rec supprArc = fun (l:(sommet*(sommet list))list) (a:sommet*sommet) ->
  match l with 
  |[] -> raise Introuvable
  |h::t -> if (fst a) = (fst h) then (fst h, supprOcc (snd h) (snd a))::t
  else h::(supprArc t a);;

let rec ajoutArc = fun (l:(sommet*(sommet list))list) (a:sommet*sommet) ->
  match l with
  |[]-> raise Introuvable
  |h::t -> if (fst h) = (fst a) then (fst h, (snd a)::(snd h))::t
  else ajoutArc t a;;



(* 8. Obtenir les informations d'un sommet en connaissant son nom *)
let rec infosom =  fun (l:(sommet*(sommet list))list) s ->
  match l with 
  |[] -> raise Introuvable
  |h::t -> match (fst h) with
    |Util u -> if u.nom1 = s then fst h else infosom t s
    |Page p -> if p.nom = s then fst h else infosom t s;;

let affUtil = fun u ->
  print_string ("Nom : " ^ u.nom1 ^ 
		"\nPrénom : " ^ u.prenom ^ 
		"\nAge : " ^ (string_of_int u.age) ^ " ans \n");;

let affPage = fun p ->
  print_string ("Nom : " ^ p.nom ^ "\n" ^
	       "Liste des administrateurs : " ^ (affEnsUtil p.admins) ^ "\n");;



(* 9. Calculer le nombre de comptes de type Utilisateur, Page *)
let nbUtil = fun (l:(sommet*(sommet list))list) ->
  let rec aux = fun l res ->
    match l with 
    |[] -> res
    |h::t -> match (fst h) with
      |Util u -> aux t (res+1)
      |_ -> aux t res
  in aux l 0;;

let nbPage = fun (l:(sommet*(sommet list))list) ->
  let rec aux = fun l res ->
    match l with 
    |[] -> res
    |h::t -> match (fst h) with
      |Page p -> aux t (res+1)
      |_ -> aux t res
  in aux l 0;;

let nbUP = fun (l:(sommet*(sommet list))list) ->
  "Nombre de pages : "^(string_of_int (nbPage l))
  ^"\n"^
  "Nombre d'utilisateurs : "^(string_of_int (nbUtil l));;



(* 10. Connaître l'âge moyen des Utilisateurs *)
let ageMoyenUtil = fun (l:(sommet*(sommet list))list) ->
  let rec aux = fun l somme ite ->
    match l with 
    |[] -> (somme/ite)
    |h::t -> match (fst h) with
      |Util u -> aux t (somme + u.age) (ite + 1)
      |_ -> aux t somme ite
  in aux l 0 0;; 



(* 11. Connaître tous les comptes Utilisateurs qui sont des administrateurs de Pages *)
let rec appartient = fun l s ->
  match l with
  |[] -> false
  |h::t -> if h = s then true else appartient t s;;

let adminsPage = fun (l:(sommet*(sommet list))list) ->

  let rec fusion = fun l1 l2 res ->
    match l1 with 
    |[] -> res
    |h::t -> if (appartient l2 h) then fusion t l2 res
    else fusion t l2 (res@[h]) 

  in let rec aux = fun l res ->
    match l with 
    |[] -> res
    |h::t -> match (fst h) with
      |Page p -> aux t (fusion p.admins res [] )
      |_ -> aux t res

  in aux l [];;



(* 12. Afficher le graphe sous forme de listes d'adjacences *)
let toString = fun (l:(sommet*(sommet list))list) ->

  let rec aux2 = fun l res ->
    match l with 
    |[]->res
    |h::t->match h with
      |Util u->u.prenom^" "^u.nom1^" , "^res
      |Page p->p.nom^" , "^res
     
  in let rec aux = fun l res ->
    match l with 
    |[]-> res
    |h::t -> match h with
      |(Util u, l1) ->  aux t ( u.prenom ^ u.nom1 ^ " : " ^ (aux2 l1 "") ^ "\n" ^ res )
      |(Page p, l2) ->  aux t ( p.nom ^ res )

  in print_string (aux l "");;



(* 13. Lire un graphe sous forme de liste d'adjacence *)
let creaUtil = fun () ->
  print_string("Nom :");
  let n = read_line() 
  in print_string("Prenom :");
  let p = read_line()
  in print_string("Age :");
  let a = read_int()
  in {nom1=n;prenom=p;age=a};;

let crealistadmins = fun l->
  let rec aux = fun res ->
    print_string ("A pour ajouter un utilisateur à la liste des administrateurs"^"\n"^
		  "Q pour quitter la creation de la liste"^"\n");
    let s = read_line() in
    match s with 
    |"Q" -> res
    |"A" -> print_string ("Entrez le nom d'un administrateur :");
	let a = read_line()
	in let Util adm = infosom l a
	in aux (adm::res)
    |_ -> aux res
  in aux [];;

let creaPage = fun l ->
  print_string("Nom de la page :");
  let n = read_line() 
  in let la = crealistadmins l
  in {nom=n;admins=la};;

let rec creasommet = fun l -> 
  print_string("U pour creer un utilisateur"^"\n"^
	       "P pour creer une page"^"\n");
  let s = read_line() in
  match s with 
  |"U" -> Util (creaUtil ())
  |"P" -> Page (creaPage l)
  |_ -> creasommet l;;

let rec  crealisteadj = fun lt ->
  let rec aux = fun (res:sommet list) ->
    print_string ("N pour ajouter un sommet a la liste d'adjacence"^"\n"^
		  "Q pour quitter la creation de la liste d'adjacence"^"\n");
    let str = read_line ()
    in match str with
    |"Q" -> res
    |"N" -> print_string ("Entrez le nom d'un sommet adjacent");
	let s = read_line ()
	in let s1 = infosom lt s
	in aux (s1::res)
    |_ -> aux res	  
  in (aux []);;

let rec lireGraphe = fun () ->
  let rec aux = fun res ->
    print_string("S pour creer un sommet"^"\n"^
		 "L pour creer la liste d'adjacence d'un sommet  existant"^"\n"^
		 "Q pour quitter la creation du graphe"^"\n");
    let s = read_line() in 
    match s with 
    |"S" -> aux ((creasommet res, [])::res);
    |"L" -> print_string("Entrez le nom du sommet source"); 
	let k1 = read_line()
	in let k = infosom res k1
	in aux ((k,crealisteadj res)::res)
    |"Q" -> res
    |_ -> aux res
  in aux [];;
	
	   
(* Interface *)
let rec top_level = function (l:(sommet*(sommet list))list) ->
  let s =
    "1 : Afficher le nombre de sommets"^"\n"^
    "2 : Afficher le nombre d'arcs"^"\n"^
    "3 : Obtenir l'ensemble des sommets"^"\n"^
    "4 : Obtenir l'ensemble des sommets trié par nom"^"\n"^
    "5 : Obtenir l'ensemble des sommets trié par degré sortant"^"\n"^
    "6 : Obtenir l'ensemble des arcs"^"\n"^
    "7 : Ajouter un sommet"^"\n"^
    "8 : Supprimer un sommet (et ses arcs)"^"\n"^
    "9 : Ajouter un arc"^"\n"^
    "10: Supprimer un arc"^"\n"^
    "11: Obtenir les informations d'un sommet en connaissant son nom"^"\n"^
    "12: Obtenir le nombre d'utilisateurs"^"\n"^
    "13: Obtenir le nombre de pages"^"\n"^
    "14: Obtenir l'âge moyen des utilisateurs"^"\n"^
    "15: Obtenir tous les comptes utilisateurs qui sont des administrateurs de pages"^"\n"^
    "16: Afficher le graphe sous la forme de listes d'adjacences"^"\n"^
    "17: Créer un nouveau graphe"^"\n"^
    "18: Quitter l'interface"^"\n"			 
  in print_string s;
  let c = read_int() in
  match c with

  |1 -> print_string ("Nombre de sommets : " ^ string_of_int(nbsommets l));
      top_level l

  |2 -> print_string ("Nombre d'arcs : " ^ string_of_int(nbarcs l));
      top_level l

  |3 -> print_string ("Ensemble des sommets du graphe : " ^ affEnsSom (ensSom l));
      top_level l

  |4 -> print_string ("Ensemble des sommets du graphe : " ^ affEnsSom (ensSomNom l));
      top_level l

  |5 -> print_string ("Ensemble des sommets du graphe : " ^ affEnsSom (ensSomDeg l));
      top_level l

  |6 -> print_string ("Ensemble des arcs du graphe : " ^ affEnsArcs (ensArcs l));
      top_level l

  |7 -> top_level (ajoutSom l (creasommet l))

  |8 -> print_string ("Nom du sommet a supprimer : ");
      let str = read_line() 
      in begin try (top_level (supprSom l (infosom l str))) 
      with |Introuvable -> print_string ("Sommet introuvable!"); top_level l end
	
  |9 ->  print_string ("Nom du sommet source de l'arc à créer : ");
      let str1 = read_line()
      in print_string ("Nom du second sommet de l'arc :");
      let str2 = read_line()
      in begin try (top_level (ajoutArc l ( infosom l str1 , infosom l str2 )))
      with
      |Introuvable -> print_string ("Sommet introuvable!"); top_level l end

  |10 -> print_string ("Nom du sommet source de l'arc à supprimer : ");
      let str1 = read_line()
      in print_string ("Nom du second sommet de l'arc :");
      let str2 = read_line()
      in begin try (top_level (supprArc l ( infosom l str1 , infosom l str2 ))) 
      with 
      |Introuvable -> print_string ("Arc introuvable!"); top_level l end

  |11 -> print_string ("Nom du sommet : ");
      let str = read_line()
      in let som = infosom l str
      in begin match som with
      |Util u -> affUtil u
      |Page p -> affPage p end;
      top_level l

  |12 -> print_string ("Nombre d'utilisateurs : " ^ string_of_int (nbUtil l)^"\n");
      top_level l

  |13 -> print_string ("Nombre de pages : " ^ string_of_int (nbPage l) ^"\n");
      top_level l

  |14 -> print_string ("Age moyen des utilisateurs : " ^ string_of_int (ageMoyenUtil l)^" ans\n");
      top_level l

  |15 -> print_string ("Ensemble des administrateurs de pages :" ^ affEnsUtil (adminsPage l) ^ "\n");
      top_level l

  |16 -> toString l;
      top_level l

  |17 -> top_level (lireGraphe ())

  |18 -> exit 0

  |_ -> top_level l;;


(*main*)
let main() = top_level [];;

main();;
