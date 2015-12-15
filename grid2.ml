type sens = 
    H of bool
  | V of bool;;


type word = {
    sens : string; (* Vraiment besoin du mutable ? *)
    ligne_colonne : int;
    debut : int;
    longueur : int
  };;

let fic_ouvre_toi = fun file_path ->
  try
    open_in file_path;
  with exc ->
    Printf.printf "%s ne s ouvre pas en lecture\n" file_path;
    raise exc;;

let close_file = fun channel ->
  try 
    close_in channel
  with exc ->
    Printf.printf "%s ne se ferme pas bien\n" "dico.txt";
    raise exc;;


let read_grid = fun file_path -> (* Genere la matrice à partir du fichier lu *)
  let file = fic_ouvre_toi file_path in
  let liste_lignes = ref [] in
  try
    while true do
      let line = input_line file in
      liste_lignes := line :: !liste_lignes;
      Printf.printf "%s\n" line
    done;
    failwith "unreachable"
  with End_of_file ->
    close_file file;
    let rliste_lignes = List.rev !liste_lignes in
    Array.of_list rliste_lignes

        
let gen_tab_words = fun matrice -> (* Genere le tableau de mots *)
  Printf.printf "Gen_tab_words appelé\n";
  let min_length = ref 5 in
  let max_length = ref 2 in
  let i_length = Array.length matrice -1 in
  let compteur = ref 0 in
  let word_list = ref [] in
  for i=0 to i_length (* Trouve les mots horizontaux *)
  do
    let j_length = String.length matrice.(i) -1 in
    for j=0 to j_length
    do 
      let line = matrice.(i) in
      let char = line.[j] in
      if char = '_' then 
        begin
          incr compteur;
          if j = j_length && !compteur >=2 then  (* Gere les blancs de fin de ligne *)
            begin
              let new_word = {sens="horizontal"; ligne_colonne=i; debut=(j - !compteur +1); longueur= (!compteur)} in
              word_list := new_word::!word_list;
              if !compteur < !min_length then min_length := !compteur;
              if !compteur > !max_length then max_length := !compteur;
              compteur := 0;
            end; (* Si ce n'est pas en fin de ligne, on incrémente le compteur de l'eventuel mot trouvé *)
        end        
      else
        if char = '*' then
          if !compteur >=2 then (* Si lorsque l'on trouve une étoile le compteur est d'au moins 2, on cree un mot *)
            begin
              let new_word = {sens="horizontal"; ligne_colonne=i; debut=(j - !compteur); longueur= (!compteur)} in
              word_list := new_word::!word_list;
              if !compteur < !min_length then min_length := !compteur;
              if !compteur > !max_length then max_length := !compteur;
              compteur := 0
            end
          else
            compteur := 0
    done
  done;
  compteur := 0;
  let j_length = String.length matrice.(0) - 1 in (* Arbitraire pour l'istant *) (* Vertical *)
  for j=0 to j_length
  do
    for i=0 to i_length do
      let char = matrice.(i).[j] in
      if char = '_' then
        begin
          incr compteur;
          if i = i_length then  (* Gere les blancs de fin de ligne *)
            if !compteur>=2 then
              begin
                let new_word = {sens="vertical"; ligne_colonne=j; debut=(i - !compteur +1); longueur=(!compteur)} in
                word_list := new_word::!word_list;
                if !compteur < !min_length then min_length := !compteur;
                if !compteur > !max_length then max_length := !compteur;
                compteur := 0;
              end (* Si ce n'est pas en fin de ligne, on incrémente le compteur de l'eventuel mot trouvé *)
            else compteur:=0
        end
        
      else
        if char = '*' then
          if !compteur >=2 then (* Si lorsque l'on trouve une étoile le compteur est d'au moins 2, on cree un mot *)
            begin
              let new_word = {sens="vertical"; ligne_colonne=j; debut=(i - !compteur); longueur=(!compteur)} in
              word_list := new_word::!word_list;
              if !compteur < !min_length then min_length := !compteur;
              if !compteur > !max_length then max_length := !compteur;
              compteur := 0
            end
          else
            compteur := 0
    done
  done;
  
  let word_table = Array.of_list (List.rev !word_list) in
  Printf.printf "Min_length : %d\n" !min_length;
  Printf.printf "Max_length : %d\n" !max_length;
  word_table;;
  

let print_tab_words = fun tab_words ->
  Array.iteri (fun i word -> Printf.printf "Mot no %d : {sens : %s; ligne_col : %d; debut : %d; longueur : %d}\n" i word.sens word.ligne_colonne word.debut word.longueur) tab_words;;


let main = fun nom_fichier ->
  let matrice = read_grid nom_fichier in
  let tab_words = gen_tab_words matrice in
  print_tab_words tab_words;
  tab_words;;

main "grille_test.txt";;
  
