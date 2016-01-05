let grid_add = fun var chaine grid -> (* Recopie la chaine instanciee dans la grille *)
  if not var.Grid.word.vertical then (* horizontal *)
    begin
      let x = var.Grid.word.ligne_colonne in
      let y = var.Grid.word.debut in
      for i=0 to var.Grid.word.longueur-1 do
        grid.(x).[y+i] <- chaine.[i];
      done
    end
  else
    begin (* vertical *)
      let y = var.Grid.word.ligne_colonne in
      let x = var.Grid.word.debut in
      for i=0 to var.Grid.word.longueur do
        grid.(x+i).[y] <- chaine.[i];
      done
    end
in ()

let filter_crossed = fun id tab_var id_var-> (* Fonction bas niveau utilisée dans un List.iter : permet d'enlever un mot instancié des crossed *)
  let var = tab_var.(id_var) in
  let new_crossed = ref [] in
  List.iter (fun id1 -> if not (id1=id) then new_crossed := (id1 :: !new_crossed)) tab_var.(id_var).crossed;
  tab_var.(id_var).crossed <- !new_crossed;; (* ***** Met à jour le tableau ******** *)



let erase_from_crossed = fun var var_table ->

  List.iter (filter_crossed var.id var_table) var.crossed;
  




  
(* let erase_from_crossed = fun var1 var var_table  -> (\* Enleve la variable instanciee des listes crossed des variables croisees *\) *)
(*   let new_crossed = ref [] in *)
(*   let remaining_list = ref var1.crossed in *)
(*   let crossed_id = ref var.crossed in *)
(*   let rec erase_encore = fun () -> *)
(*     match !remaining_list with *)
(*       [] -> (var1.crossed <- !new_crossed) *)
(*     | id :: remaining -> *)
(*         if id = var.id then *)
(*           begin *)
(*             new_crossed := !new_crossed :: remaining; *)
(*             remaining_list := []; *)
(*             erase_encore (); *)
(*           end *)
(*         else *)
(*           begin *)
(*             new_crossed := id :: !new_crossed; *)
(*             remaining_list := remaining; *)
(*             erase_encore (); *)
(*           end *)
(*   and return_crossed = fun () -> *)
(*       (\* Renvoit la liste des variables croisees a mettre a jour *\) *)
(*     let crossed_words = ref [] in *)
(*     match !crossed_id with *)
(*       [] -> !crossed_words *)
(*     | id :: remaining -> *)
(*         begin *)
(*           crossed_id := remaining; *)
(*           crossed_words := var_table.(id) :: !crossed_words; (\* Dependra de l'implementation de var_table *\) *)
(*           erase_encore var_table.(id); (\* Appel a erase_from_crossed *\) *)
(*           return_crossed (); *)
(*         end *)
(*   in *)
(*   () *)

let get_var_from_id = fun var_table id ->
  var_table.(id);;




(* ************************************************** La vrai fonction ************************************************ *)

let instanciation = fun var grid var_table chaine -> (*Pour rendre vivant : \r%d%! *)
  
  grid_add var chaine grid;
  
  var.domain <- Dico_load.add_nlist chaine Dico_load.empty; (* Reduit le domaine de la variable au seul mot instancie *)
  
  erase_from_crossed var var_table;; (* supprime le mot instancie des crossed *)

(* ********************************************************************************************************************** *)











  
  






let restr_domain = fun grid var -> (* Reduit le domaine de la variable parametre par rapport à ce qu'il y a dans la grille *)

  let new_domain = ref Dico_load.empty in
  if var.Grid.word.vertical then begin
    let col = var.Grid.word.ligne_colonne and debut = var.Grid.word.debut and longueur = var.Grid.word.longueur in
    let rec list_reduc_v = fun domain_list ->
      match domain_list with
        [] -> var.domain <- !new_domain
      | mot :: reste ->
          for j=0 to longueur -1 do
            if grid.(debut + j).[col] = '_' || mot.[j] = grid.(debut + j).[col] then begin
              new_domain := Dico_load.add_nlist mot !new_domain;
              list_reduc_v reste;
            end
            else
              list_reduc_v reste
          done;
    in list_reduc_v var.domain.liste
  end
  else begin
    let ligne = var.ligne_colonne and debut = var.word.debut and longueur = var.word.longueur in
    let rec list_reduc_h = fun domain_list ->
      match domain_list with
        [] -> var.domain <- !new_domain
      | mot :: reste ->
          for i=0 to longueur -1 do
            if grid.(ligne).[debut + i] = '_' || mot.[i] = grid.(ligne).[debut + i] then begin
              new_domain := Dico_load.add_nlist mot !new_domain;
              list_reduc_h reste;
            end
            else list_reduc_h reste
          done;
    in list_reduc_h var.domain.liste
  end
in ();;


let test_domaine = fun var_table ->
  let range = Array.length var_table in
  let empty_domains = ref 0 in
  for i=0 to (range-1) do
    if var_table.(i).domain.taille <= 0 then incr empty_domains
   (* else if var_table.(i).domain.taille = 1 then match var_table.(i).domain.liste with elt::reste -> instanciation var_table.(i) grid var_table elt *)
  done;
  !empty_domains;;






let filtre = fun var var_table grid -> (* state = tableau des var*)
  let new_tab = var_table in
  let var_liste = var.crossed in
  
  let rec filter = fun liste_var ->
    match liste_var with
      [] -> ()
    | var::remaining ->
        restr_domain grid var;
        match var.domain.taille with
          0 -> raise Dico_load.Empty
        (*| 1 -> begin*)
          (*  match var.domain.liste with*)
          (*    [] -> failwith "domaine.liste vide"*)
          (*  | elt::reste ->  *)
         (*       let new_crossed = instanciation var elt grid var_table in (* Garder un oeil là dessus *) *)
        (*        filter (Array.to_list var_table); *)
       (* end*)
        | _ ->
            begin
                new_tab.[var.id] <- var;
            end
  in filter var_liste


(* let var_liste = fun tab_var id_liste -> *)
(*   let new_list = ref [] in *)
(*   let rec encore = fun tampon_list -> *)
(*     match tampon_list with  *)
(*       elt::reste -> begin new_liste := tab_var.(elt)::new_liste; encore reste end *)
(*     | [] -> !new_list *)
(*   in encore id_liste;; *)




let filtrage = fun var var_table grid ->
  let id_crossed = var.crossed in

  List.iter (fun id -> let taille = var_table.(id).domain.taille in if taille <= 0 then 0 else taille) id_crossed;
  let rec encore = fun liste ->
    match liste with
      elt::reste -> if elt = 0 then false else encore reste
    | [] -> true
  in encore id_crossed
