type word = {
    sens : string;
    ligne_colonne : int;
    debut : int;
    longueur : int
};;

let fic_ouvre_toi = fun file_path ->
  try
    open_in file_path
  with exc ->
    Printf.printf "%s ne s ouvre pas en lecture\n" file_path;
    raise exc;;

(*
let close_file = fun channel ->
  try 
    close_in channel
  with exc ->
    Printf.printf "%s ne se ferme pas bien\n" "dico.txt";
    raise exc;;


let read_grid = fun file_path ->
  let file = fic_ouvre_toi file_path in
  let liste_lignes = ref [] in
  let compteur = ref 0 in
  let rec lecture = fun () ->
    let line = input_line file in
    if line <> "" then
      begin
        liste_lignes := line :: !liste_lignes;
        incr compteur;
        lecture ()
      end
    else
      raise End_of_file
  in
  begin
    try
      lecture ()
    with
      End_of_file -> close_file file;
  end;
  liste_lignes := List.rev !liste_lignes;
  
  let matrice_grid = Array.of_list liste_lignes in
  matrice;;
  
        
let gen_tab_words = fun matrice -> (* Genere le tableau de mots *)
  for i=0 to Array.length matrice
  do
    for j=0 to String.length matrice.(i)
    do 
      let line = matrice.(i) in
      let rec recognize = fun indice compteur ->
        match line.[j] with
          "*" -> begin
            if compteur >= 2 then 
              let new_word = {sens:"horizontal";ligne_colonne:i;debut:
          
        *)
        























let ligne = ref 1;;
let colonne = ref 0;;
let min_word  = ref max_int;;
let max_word  = ref 0;;

let remplir_matrice = fun file_path ->
  let flag = ref 0 in
  let var = ref ' ' in
  let liste = ref [] in
  let file = fic_ouvre_toi file_path in
  let rec encore = fun () ->
    var := input_char file;
    if (!var) != '\n' then
      begin
        liste:=(!var) :: (!liste);
        if !flag = 0 then
          incr colonne;
      end
    else
      begin
        incr ligne;
        flag:=1;
      end;
    encore () in
  try 
    encore () 
  with 
    End_of_file ->
      let matrix = Array.make_matrix !ligne !colonne '_' in
      for i = 0 to !ligne - 1  do
        for j = 0 to !colonne - 1  do
          begin
            match !liste with
            [] -> ();
            | x::xs ->
                liste:=xs;
                matrix.(!ligne -1 -i).(!colonne -1 -j) <- x;
          end
        done
      done;
      matrix;;

let tab_words = fun matrice ->
  let liste_mots = ref [] in
  let x_val = ref 0 in
  let cpt = ref 0 in
  let var = ref {sens="horizontal"; ligne_colonne=0; debut=0; longueur=0} in
  for i = 0 to !ligne - 1 do (*Horizontalement*)
      x_val := 0;
    for j = 0 to !colonne - 1 do
      if matrice.(i).(j)='*' then 
        begin
          if (!cpt) >= 2 then
            begin
              var := {sens="horizontal"; ligne_colonne=i; debut=(j-(!cpt)); longueur=(!cpt)};
              liste_mots := !var :: !liste_mots;
            end;
          cpt:=0;
        end
      else
        cpt:=!cpt+1;
      x_val := !x_val + 1;
    done;
    if (!cpt) != 0 then
      begin
        if (!cpt) >= 2 then
          begin
            var := {sens="horizontal"; ligne_colonne=i; debut=((!x_val)-(!cpt)); longueur=(!cpt)};
            liste_mots := !var :: !liste_mots;
          end;
        cpt:=0;
      end
  done;

  for j = 0 to !colonne - 1 do (*Verticalement*)
    x_val := 0;
    for i = 0 to !ligne - 1 do 
      if matrice.(i).(j)='*' then
          begin
            if (!cpt) >= 2 then
              begin
                var := {sens="vertical"; ligne_colonne=j; debut=((!x_val)-(!cpt)); longueur=(!cpt)};
                liste_mots := !var :: !liste_mots;
              end;
            cpt:=0; 
        end
      else
        cpt:=!cpt+1;
      x_val := !x_val + 1;
    done;
    if (!cpt) != 0 then
      begin
        if (!cpt) >= 2 then
          begin
            var := {sens="Vertical"; ligne_colonne=j; debut=((!x_val)-(!cpt)); longueur=(!cpt)};
            liste_mots := !var :: !liste_mots;
          end;
        cpt:=0;
      end
  done;
  let words_table = Array.make (List.length !liste_mots) {sens=""; ligne_colonne=0; debut=0; longueur=0} in
  let rec gen_tab = fun () ->
    match !liste_mots with
      [] -> words_table;
    | x::xs -> 
        words_table.((List.length !liste_mots) - 1) <- x; 
        if (!min_word) > x.longueur then
          min_word:=x.longueur;
        if (!max_word) < x.longueur then
          max_word:=x.longueur;
        liste_mots := xs; gen_tab () in 
  gen_tab ();;

let minmax_word = fun () ->
  (!min_word, !max_word);;


let () =
  let matrix = remplir_matrice "fichier.txt" in
  let table_mots = tab_words matrix in table_mots;
  Printf.printf "Succes\n";;
