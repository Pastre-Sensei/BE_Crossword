type word = {
    sens : string; (* Vraiment besoin du mutable ? *)
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
  
  let matrice_grid = Array.of_list !liste_lignes in
  matrice_grid;;
  
        
let gen_tab_words = fun matrice -> (* Genere le tableau de mots *)

  let compteur = ref 0 in
  let word_list = ref [] in
  for i=0 to Array.length matrice (* Trouve les mots horizontaux *)
  do
    for j=0 to String.length matrice.(i)
    do 
      let line = matrice.(i) in
      let char = line.[j] in
      if char = '_' then 
        if j = String.length line && !compteur >=2 then  (* Gere les blancs de fin de ligne *)
          begin
            let new_word = {sens="horizontal"; ligne_colonne=i; debut=(j - !compteur); longueur= (!compteur)} in
            word_list := new_word::!word_list;
            compteur := 0;
          end
        else incr compteur (* Si ce n'est pas en fin de ligne, on incrémente le compteur de l'eventuel mot trouvé *)
        
      else
        if char = '*' then
          if !compteur >=2 then (* Si lorsque l'on trouve une étoile le compteur est d'au moins 2, on cree un mot *)
            begin
              let new_word = {sens="horizontal"; ligne_colonne=i; debut=(j - !compteur); longueur= (!compteur)} in
              word_list := new_word::!word_list;
              compteur := 0
            end
          else
            compteur := 0
    done
  done;
  let longueur_ligne = String.length matrice.(0) in
  for j=0 to longueur_ligne
  do
    for i=0 to Array.length matrice
    do
      let char = matrice.(i).[j] in
      if char = '_' then 
        if i = Array.length matrice && !compteur >=2 then  (* Gere les blancs de fin de colomne *)
          begin
            let new_word = {sens="vertical"; ligne_colonne=j; debut=(i - !compteur); longueur=(!compteur)} in
            word_list := new_word::!word_list;
            compteur := 0;
          end
        else incr compteur (* Si ce n'est pas en fin de ligne, on incrémente le compteur de l'eventuel mot trouvé *)
        
      else
        if char = '*' then
          if !compteur >=2 then (* Si lorsque l'on trouve une étoile le compteur est d'au moins 2, on cree un mot *)
            begin
              let new_word = {sens="vertical"; ligne_colonne=j; debut=(i - !compteur); longueur=(!compteur)} in
              word_list := new_word::!word_list;
              compteur := 0
            end
          else
            compteur := 0
    done
  done;
  
  let word_table = Array.of_list !word_list in
  word_table;;
  
