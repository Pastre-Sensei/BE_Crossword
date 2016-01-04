let nbre_backtrack = ref 0;

let rec backtrack = fun vars etat
  match vars with
    [] -> Printf.printf "afficher la grille\n"
  | var::reste ->
      List.iter (
      fun str ->
        (*verifier les crosseds*)
        for i = 0 to List.length etat - 1 do
          for j = 0 to List.length var.crossed -1 do
            if (List.nth etat i) = (List.nth var.crossed j) then
            (*recuperer les crosseds deja instanciées*)
              begin
              end
        done
     ) var.domain.liste

