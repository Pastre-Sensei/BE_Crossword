type variable = {
    id : int;
    mutable domain : Dico_load.nlist;
    word : Grid.word;
    mutable crossed : int list};;


let instanciation = fun var state chaine grid var_table -> (*Pour rendre vivant : \r%d%! *)

  let grid_add = fun () -> (* Recopie la chaine instanciee dans la grille *)
    if not var.word.sens then (* horizontal *)
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
