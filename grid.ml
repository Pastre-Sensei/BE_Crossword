type word = {
    vertical : bool; (* Vraiment besoin du mutable ? *)
    ligne_colonne : int;
    debut : int;
    longueur : int
  };;

let copy_word = fun wd ->
  {vertical = wd.vertical;
    ligne_colonne = wd.ligne_colonne;
    debut = wd.debut;
    longueur = wd.longueur} 

type variable = {
    id : int;
    mutable domain : Dico_load.nlist;
    word : word;
    mutable crossed : int list;
    mutable instance : bool};;

let copy_var = fun var -> 
  {id = var.id;
    domain = Dico_load.copy_domain var.domain;
    word = copy_word var.word;
    crossed =  Array.to_list (Array.copy (Array.of_list var.crossed));
    instance = var.instance
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


let get_grid = fun file_path -> (* Genere la matrice à partir du fichier lu *)
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




let print_tab_words = fun tab_words ->
  Array.iteri (fun i word -> Printf.printf "Mot no %d : {vertical : %B; ligne_col : %d; debut : %d; longueur : %d}\n" i word.vertical word.ligne_colonne word.debut word.longueur) tab_words;;


let min_length = ref max_int;;
let max_length = ref min_int;;

        
let gen_tab_words = fun matrice -> (* Genere le tableau de mots *)
  Printf.printf "Gen_tab_words appelé\n";
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
          if j = j_length then
            if !compteur >=2 then  (* Gere les blancs de fin de ligne *)
              begin
                let new_word = {vertical=false; ligne_colonne=i; debut=(j - !compteur +1); longueur= (!compteur)} in
                word_list := new_word::!word_list;
                if !compteur < !min_length then min_length := !compteur;
                if !compteur > !max_length then max_length := !compteur;
                compteur := 0;
              end (* Si ce n'est pas en fin de ligne, on incrémente le compteur de l'eventuel mot trouvé *)
            else
              compteur := 0;
        end        
      else
        if char = '*' then
          if !compteur >=2 then (* Si lorsque l'on trouve une étoile le compteur est d'au moins 2, on cree un mot *)
            begin
              let new_word = {vertical=false; ligne_colonne=i; debut=(j - !compteur); longueur= (!compteur)} in
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
                let new_word = {vertical=true; ligne_colonne=j; debut=(i - !compteur +1); longueur=(!compteur)} in
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
              let new_word = {vertical=true; ligne_colonne=j; debut=(i - !compteur); longueur=(!compteur)} in
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
  


let load_domain = fun var_table domain_table -> (* Charge les domaines des variables avec tous les mots du dictionnaire *)
  for i=0 to Array.length var_table -1 do
    let ln = var_table.(i).word.longueur in
    var_table.(i).domain <- domain_table.(ln)
  done;;

let var_table = fun table_mots domain_table ->

  let horizontaux_liste = ref [] in
  let verticaux_liste = ref [] in
  let table_var = Array.init (Array.length table_mots) (fun i -> {id=i; domain=Dico_load.empty; word=table_mots.(i); crossed=[]; instance=false}) in
  for i=0 to Array.length table_var -1 do
    if not table_var.(i).word.vertical then
      horizontaux_liste := table_var.(i) :: !horizontaux_liste
    else
      verticaux_liste := table_var.(i) :: !verticaux_liste
  done;
  let range_h = (List.length !horizontaux_liste) -1 in
  let range_v = (List.length !verticaux_liste) -1 in
  let horizontaux = Array.of_list !horizontaux_liste in
  let verticaux = Array.of_list !verticaux_liste in

  for i=0 to range_h do (* horizontal *)
    let h = horizontaux.(i) in
    for j=0 to range_v do
      let v = verticaux.(j) in
      if v.word.ligne_colonne >= h.word.debut && v.word.ligne_colonne <= (h.word.debut + h.word.longueur-1) && v.word.debut <= h.word.ligne_colonne && (v.word.debut + v.word.longueur - 1) >= h.word.ligne_colonne then begin
        h.crossed <- v.id :: h.crossed;
        v.crossed <- h.id :: v.crossed;
      end
    done;
  done;
  load_domain table_var domain_table;
  table_var;;
    

let get_vars = fun grid dico ->
  let word_table = gen_tab_words grid in
  var_table word_table dico;;





(* **** Printf **** *)    


let print_crossed = fun crossed ->
  List.iteri (fun i id -> Printf.printf "id %d : %d\n" i id) crossed;;

let print_var = fun ch (var : variable) ->
  Printf.fprintf ch "\n***Var %d***\n Mot :  {vertical : %B; ligne_col : %d; debut : %d; longueur : %d}\n *******\n" var.id var.word.vertical var.word.ligne_colonne var.word.debut var.word.longueur;
  Printf.fprintf ch "Taille domaine : %d\n" var.domain.Dico_load.taille;
  Printf.fprintf ch "dom : %a\n" Dico_load.fprint_domain var.domain;
  Printf.fprintf ch "Crossed : \t";
  List.iter (fun id -> Printf.fprintf ch "%d\t" id) var.crossed;
  Printf.fprintf ch "\nFin de la variable\n";;

let print_tab_var = fun ch tab_var ->
  Array.iter
    (fun v -> Printf.fprintf ch "%a\n" print_var v)
    tab_var;;


(* let () = *)
(*   (\* main *\) *)
(*   let gridfile = Sys.argv.(1) in *)
(*   let dico = Dico_load.dico_array "dico.txt" 2 10 in *)
(*   let matrice = get_grid gridfile in *)
(*   let tab_words = gen_tab_words matrice in *)
(*   print_tab_words tab_words; *)
(*   Printf.printf "Fini\n"; *)
(*   let tab_var = var_table tab_words dico in *)
(*   print_tab_var stdout tab_var; *)
(* ;; *)
