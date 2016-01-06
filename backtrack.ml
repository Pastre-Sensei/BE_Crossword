let nbre_backtrack = ref 0;;
exception Fin;;

let rec bt = fun (vars : Grid.variable array) solution grid ->
  let arret = ref true in
  for i = 0 to Array.length vars -1 do (* verifier l'existence de variable non instanciée *)
    if not vars.(i).instance then
      arret:=false (* signifie qu'il y'a au moins une variable non instanciée *)
  done;
    
  if !arret = true then (* toutes mes variables sont instanciées donc je m'arrete *)
    raise Fin
  else (* il y'a encore des variables à instancier *)
    begin
      let index = ref (-1) in
      let taille_min = ref max_int in
      Array.iter (
      fun (var : Grid.variable) -> 
        if not (var.instance) then
          if var.domain.taille < !taille_min then
            begin
              index := var.id;
              taille_min := var.domain.taille;
            end
     ) vars;
      let var = vars.(!index) in
      List.iter (
      fun str ->
        let grid_local = Array.copy grid in
        Propagation.instanciation var grid vars str;
        if (Propagation.filtrage var vars grid) then (* le mot instancié est bon *)
          begin
            (vars.(!index)).instance = true;
            solution := (!index, str)::!solution;
            bt vars solution grid;
          end
        else (* le mot instancié n'est pas bon donc on annule notre instanciation*)
          begin
            incr nbre_backtrack;
            for k = 0 to Array.length grid do
              grid.(k) <- grid_local.(k);
            done
          end
     ) var.domain.liste
    end 

let backtrack = fun vars solution grid ->
  try
    bt vars solution grid;
  with
    Fin ->
      for k = 0 to Array.length grid do
        Printf.printf "%s\n" grid.(k);
      done ;;
let () =
  Printf.printf "coucou\n";;
