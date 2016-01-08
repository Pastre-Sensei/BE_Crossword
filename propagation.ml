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
      for i=0 to var.Grid.word.longueur -1 do
        grid.(x+i).[y] <- chaine.[i];
      done
    end;;

let filter_crossed = fun id (tab_var : Grid.variable array) id_var-> (* Fonction bas niveau utilisée dans un List.iter : permet d'enlever un mot instancié des crossed *)
  let new_crossed = ref [] in
  List.iter (fun id1 -> if not (id1=id) then new_crossed := (id1 :: !new_crossed)) tab_var.(id_var).crossed;
  tab_var.(id_var).crossed <- !new_crossed;; (* ***** Met à jour le tableau ******** *)



let erase_from_crossed = fun (var : Grid.variable) (var_table : Grid.variable array) ->

  List.iter (filter_crossed var.id var_table) var.crossed;;
  





(* ************************************************** La vrai fonction ************************************************ *)

let instanciation = fun (var : Grid.variable) grid (var_table : Grid.variable array) chaine -> (*Pour rendre vivant : \r%d%! *)
  
  grid_add var chaine grid;
  
  var.domain <- Dico_load.add_nlist chaine Dico_load.empty; (* Reduit le domaine de la variable au seul mot instancie *)
  
  erase_from_crossed var var_table;; (* supprime le mot instancie des crossed *)

(* ********************************************************************************************************************** *)







let convert = fun grid (var : Grid.variable) -> (* Extrait la chaine de caractère de la grille pour restreindre le domaine*)
  let str = ref "" in

  if var.Grid.word.vertical then  (* Vertical *) begin
    let col = var.Grid.word.ligne_colonne and debut = var.Grid.word.debut and longueur = var.Grid.word.longueur in
    for j=0 to (longueur-1) do
      str := String.concat "" [!str; String.make 1 grid.(debut + j).[col]];
    done
  end
  else (* Horizontal *)
    begin
      let ligne = var.Grid.word.ligne_colonne and debut = var.Grid.word.debut and longueur = var.Grid.word.longueur in
      for i=0 to (longueur-1) do
        str := String.concat "" [!str; String.make 1 grid.(ligne).[debut + i]];
      done;
    end;
  Printf.printf "Chaine : %s\n" !str;
  !str;;





let restr_domain = fun grid (var : Grid.variable) -> (* Reduit le domaine de la variable parametre par rapport à ce qu'il y a dans la grille *)
  let gmot = convert grid var in
  let longueur = String.length gmot in
  let filtered =
    List.filter
      (fun mot ->
        try
          for j=0 to longueur-1 do
            if gmot.[j] <> '_' && mot.[j] <> gmot.[j] then raise Exit
          done;
          true
        with Exit -> false)
      var.domain.liste in
  let new_dom = Dico_load.create filtered in
  var.domain <- new_dom;;



let var_liste = fun tab_var id_liste ->
  let new_list = ref [] in
  let rec encore = fun tampon_list ->
    match tampon_list with
      elt::reste -> begin new_list := tab_var.(elt)::(!new_list); encore reste end
    | [] -> !new_list
  in encore id_liste;;




let filtrage = fun (var : Grid.variable) (var_table : Grid.variable array) grid ->
  
  let id_cross = var.crossed in
  let cross = var_liste var_table id_cross in
  List.iter (restr_domain grid) cross;

  let cross_array = Array.of_list cross in
  let flag = ref true in
  
  for i=0 to (Array.length cross_array)-1 do
    if cross_array.(i).domain.taille <= 0 then flag := false
  done;
  !flag;;



(* let () = *)
(*   Printf.printf "\n\n *********************** Propagation.ml **********************\n"; *)
(*   let dico = Dico_load.dico_array "dico.txt" 2 10 in *)
(*   let grid = Grid.get_grid "grille_test.txt" in *)
(*   let vars = Grid.get_vars grid dico in *)
(*   let var = vars.(3) in *)
(*   instanciation var grid vars (List.hd var.domain.liste); *)
(*   Array.iter (fun str -> Printf.printf "%s\n" str) grid; *)
(*   Printf.printf "Domaine de la variable : taille = %d\t" var.domain.taille; *)
(*   List.iter (fun str -> Printf.printf "%s\t" str) var.domain.liste; *)
(*   let crossed = var.crossed in *)
(*   List.iter ( *)
(*   fun id ->  *)
(*     begin  *)
(*       Printf.printf "\nVar %d : \t" id; *)
(*       List.iter (fun id -> Printf.printf "%d\t" id) vars.(id).crossed *)
(*     end) *)
(*     crossed; *)
  
(*   let flag = filtrage var vars grid in *)
(*   Printf.printf "\nfiltrage : %b\n" flag; *)
(*   List.iter (fun id -> Printf.printf "Var %d taille domaine %d\n" id vars.(id).domain.taille) var.crossed; *)

(*   let var2 = vars.(2) in *)
(*   instanciation var2 grid vars (List.hd var2.domain.liste); *)
(*   Array.iter (fun str -> Printf.printf "%s\n" str) grid; *)
(*   let flag2 = filtrage var2 vars grid in *)
(*   Printf.printf "\nfiltrage : %b\n" flag2; *)
(*   List.iter (fun id -> Printf.printf "Var %d taille domaine %d\n" id vars.(id).domain.taille) var2.crossed; *)
(*   Printf.printf "\nOn arrive au bout !\n" *)
  
(* ;; *)
