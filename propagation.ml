open Grid
open Dico_load

let grid_add = fun var chaine grid -> (* Recopie la chaine instanciee dans la grille *)
  if not var.Grid.word.Grid.vertical then (* horizontal *)
    begin
      let x = var.Grid.word.Grid.ligne_colonne in
      let y = var.Grid.word.Grid.debut in
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
    end;;

let filter_crossed = fun id (tab_var : Grid.variable array) id_var-> (* Fonction bas niveau utilisée dans un List.iter : permet d'enlever un mot instancié des crossed *)
  let new_crossed = ref [] in
  List.iter (fun id1 -> if not (id1=id) then new_crossed := (id1 :: !new_crossed)) tab_var.(id_var).crossed;
  tab_var.(id_var).crossed <- !new_crossed;; (* ***** Met à jour le tableau ******** *)



let erase_from_crossed = fun (var : Grid.variable) (var_table : Grid.variable array) ->

  List.iter (filter_crossed var.id var_table) var.crossed;;
  




  
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



(* ************************************************** La vrai fonction ************************************************ *)

let instanciation = fun (var : Grid.variable) grid (var_table : Grid.variable array) chaine -> (*Pour rendre vivant : \r%d%! *)
  
  grid_add var chaine grid;
  
  var.domain <- Dico_load.add_nlist chaine Dico_load.empty; (* Reduit le domaine de la variable au seul mot instancie *)
  
  erase_from_crossed var var_table;; (* supprime le mot instancie des crossed *)

(* ********************************************************************************************************************** *)











  
  






let restr_domain = fun grid (var : Grid.variable) -> (* Reduit le domaine de la variable parametre par rapport à ce qu'il y a dans la grille *)
  if var.Grid.word.vertical then begin
    let col = var.Grid.word.ligne_colonne and debut = var.Grid.word.debut and longueur = var.Grid.word.longueur in
    let filtered =
      List.filter
        (fun mot ->
          try
            for j=0 to longueur -1 do
              if grid.(debut + j).[col] <> '_' && mot.[j] <> grid.(debut + j).[col] then raise Exit
            done;
            true
          with Exit -> false)
        var.domain.liste in
  end
  else begin
    let ligne = var.Grid.word.ligne_colonne and debut = var.Grid.word.debut and longueur = var.Grid.word.longueur in
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
  end;;






(* let filtre = fun (var : Grid.variable) (var_table : Grid.variable array) grid -> (\* state = tableau des var*\) *)
(*   let new_tab = var_table in *)
(*   let var_liste = var.crossed in *)
  
(*   let rec filtre_encore = fun (liste_var : int list) -> *)
(*     match liste_var with *)
(*       [] -> () *)
(*     | id::remaining -> *)
(*         restr_domain grid var_table.(id); *)
(*         match var.domain.taille with *)
(*           0 -> raise Dico_load.Empty *)
(*         (\*| 1 -> begin*\) *)
(*           (\*  match var.domain.liste with*\) *)
(*           (\*    [] -> failwith "domaine.liste vide"*\) *)
(*           (\*  | elt::reste ->  *\) *)
(*          (\*       let new_crossed = instanciation var elt grid var_table in (\* Garder un oeil là dessus *\) *\) *)
(*         (\*        filter (Array.to_list var_table); *\) *)
(*        (\* end*\) *)
(*         | _ -> *)
(*             begin *)
(*                 new_tab.(var.id) <- var; *)
(*             end *)
(*   in filtre_encore var_liste *)


(* let var_liste = fun tab_var id_liste -> *)
(*   let new_list = ref [] in *)
(*   let rec encore = fun tampon_list -> *)
(*     match tampon_list with  *)
(*       elt::reste -> begin new_liste := tab_var.(elt)::new_liste; encore reste end *)
(*     | [] -> !new_list *)
(*   in encore id_liste;; *)




let filtrage = fun (var : Grid.variable) (var_table : Grid.variable array) grid ->

  restr_domain grid var;

  let id_crossed = var.crossed in
  let id_array = Array.of_list id_crossed in
  let flag = ref true in
  
  for i=0 to Array.length id_array do
    if var_table.(id_array.(i)).domain.taille <= 0 then flag := false
  done;
  !flag
