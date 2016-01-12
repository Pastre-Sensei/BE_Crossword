open Grid
open Dico_load

type state = {
    vars: variable array;
    grid: string array
  };;

let copy = fun state ->
  Printf.printf "Copy\n";
  {vars = Array.copy state.vars;
    grid = Array.copy state.grid};;

let is_instantiated = fun state ->
  Printf.printf "Is_instanciated\n";
  try
    for i = 0 to (Array.length state.vars) -1 do (* verifier l'existence de variable non instanciée *)
      (* signifie qu'il y'a au moins une variable non instanciée *)
      if not state.vars.(i).instance then raise Exit 
    done;
    true
  with Exit -> false;;

let select_var = fun state ->
  Printf.printf "Select Var\n";
  let index = ref (-1) in
  let taille_min = ref max_int in
  Array.iter
    (fun var -> 
      if not (var.instance) then
        if var.domain.taille < !taille_min then
          begin
            index := var.id;
            taille_min := var.domain.taille;
          end)
    state.vars;
  !index;;

let bt = fun vars grid solution ->
  let nbre_backtrack = ref 0 in
  Printf.printf "BT\n";
  let rec bt_rec = fun state  ->
    if is_instantiated state then
      begin
        Printf.printf "Solution found in %d:\n" !nbre_backtrack;
        for k = 0 to (Array.length state.grid) -1 do
          Printf.printf "%s\n" state.grid.(k);
        done;
        true
      end
    else
      begin
        let index = select_var state in
        let var = state.vars.(index) in
        try
          List.iter
            (fun str ->
              let state_local = copy state in
              Printf.printf "mot a instancier : %s\n" str;
              Propagation.instanciation var state_local.grid state_local.vars str;
              Printf.printf "Instanciation OK\n";
              Printf.printf "nombre de bt : %d\n" !nbre_backtrack;
              for k = 0 to (Array.length state_local.grid) -1 do
                Printf.printf "%s\n" state_local.grid.(k);
              done;
              if Propagation.filtrage var state_local.vars state_local.grid then (* le mot instancié est bon *)
                begin
                  Printf.printf "filtrage OK !\n";
                  state_local.vars.(index).instance <- true;
                  Printf.printf "variable instanciee : %b\n" state_local.vars.(index).instance;
                  if bt_rec state_local  then raise Exit
                  
                end
              else (* le mot instancié n'est pas bon donc on annule notre instanciation*)
                incr nbre_backtrack)
          var.domain.liste;
        false
        with Exit -> true
      end in
  let state = {vars; grid} in
  bt_rec state



(* main *)
let () =
  Printf.printf "******************* BACKTRACK.ml*******************\n";
  let dico = Dico_load.dico_array "dico.txt" 2 10 in
  let grid = Grid.get_grid "grille_ok.txt" in
  let vars = Grid.get_vars grid dico in
  let boul = bt vars grid [] in
  Printf.printf "Resultat : %b\nOVER\n" boul;
  
  
