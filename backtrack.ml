open Grid
open Dico_load

type state = {
    vars: variable array;
    grid: string array
  }

let copy = fun state ->
  {vars = Array.copy state.vars;
    grid = Array.copy state.grid}

let is_instantiated = fun state ->
  try
    for i = 0 to Array.length vars -1 do (* verifier l'existence de variable non instanciée *)
      (* signifie qu'il y'a au moins une variable non instanciée *)
      if not state.vars.(i).instance then raise Exit 
    done;
    true
  with Exit -> false

let select_var = fun state ->
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
  !index

let bt = fun vars solution grid ->
  let nbre_backtrack = ref 0 in
  let rec bt_rec = fun state solution ->
    if is_instantiated state then
      begin
        Printf.printf "Solution found in %d:\n" !nbre_backtrack;
        for k = 0 to Array.length state.grid do
          Printf.printf "%s\n" state.grid.(k);
        done;
        true
      end
    else
      begin
        let index = select_var state in
        try
          List.iter
            (fun str ->
              let state_local = copy state in
              Propagation.instanciation state_local.grid state_local.vars str;
              if Propagation.filtrage var state_local.vars state_local.grid then (* le mot instancié est bon *)
                begin
                  state_local.vars.(index).instance <- true;
                  if bt_rec state_local ((index, str) :: solution) then raise Exit
                end
              else (* le mot instancié n'est pas bon donc on annule notre instanciation*)
                incr nbre_backtrack)
          var.domain.liste;
        false
      end in
  let state = {vars; grid} in
  bt_rec state solution

let backtrack = fun vars grid ->
  try
    bt vars [] grid
  with
    Fin ->


(* main *)
let () =
  Printf.printf "coucou\n";;
  let grid = Grid.read grid_file in
  let dico = Dico.read dico_file grid in
  backtrack vars 
