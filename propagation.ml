
let instanciation = fun var chaine grid var_table -> (*Pour rendre vivant : \r%d%! *)

  let grid_add = fun () -> (* Recopie la chaine instanciee dans la grille *)
    if not var.word.vertical then (* horizontal *)
      begin
        let x = var.word.ligne_colonne in
        let y = var.word.debut in
        for i=0 to var.word.longueur-1 do
          grid.(x).[y+i] <- chaine.[i];
        done
      end
    else
      begin (* vertical *)
        let y = var.word.ligne_colonne in
        let x = var.word.debut in
        for i=0 to var.word.longueur do
          grid.(x+i).[y] <- chaine.[i];
        done
      end
  in grid_add ();
  
  var.domain <- Dico_load.add_nlist chaine Dico_load.empty; (* Reduit le domaine de la variable au seul mot instancie *)
  
  let erase_from_crossed = fun var1 -> (* Enleve la variable instanciee des listes crossed des variables croisees *)
    let new_crossed = ref [] in
    let remaining_list = ref var1.crossed in
    let rec erase_encore = fun () ->
      match !remaining_list with
        [] -> (var1.crossed <- !new_crossed)
      | id :: remaining ->
          if id = var.id then
            begin
              new_crossed := !new_crossed :: remaining;
              remaining_list := [];
              erase_encore ();
            end
          else
            begin
              new_crossed := id :: !new_crossed;
              remaining_list := remaining;
              erase_encore ();
            end
    in
    let crossed_id = ref var.crossed in
    let rec return_crossed = fun () -> (* Renvoit la liste des variables croisees a mettre a jour *)
      let crossed_words = ref [] in
      match !crossed_id with
        [] -> !crossed_words
      | id :: remaining ->
          begin
            crossed_id := remaining;
            crossed_words := var_table.(id) :: !crossed_words; (* Dependra de l'implementation de var_table *)
            erase_from_crossed var_table.(id); (* Appel a erase_from_crossed *)
            return_crossed ();
          end;
    in return_crossed ();;
    





let restr_domain = fun grid var -> (* Reduit le domaine de la variable parametre par rapport à ce qu'il y a dans la grille *)

  let new_domain = ref Dico_load.empty in
  if var.word.vertical then begin
    let col = var.ligne_colonne and debut = var.word.debut and longueur = var.word.longueur in
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





let filtre = fun var_liste var_table grid -> (* state = tableau des var*)
  let new_tab = var_table in
  
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
