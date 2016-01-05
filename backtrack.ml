let nbre_backtrack = ref 0;;
exception Fin;;
let backtrack = fun vars solution grid ->
    let rec bt = fun ()->
      if Array.length vars = 0 then
        raise Fin
      else (* il y'a encore des variables à instancier *)
        begin
          let index = ref (-1) in
          let taille_min = ref max_int in
          Array.iter (fun var -> 
            if not var.instance then
              if var.domain.taille < !taille_min then
                begin
                  index := var.id;
                  taille_min := var.domain.taille;
                end;
                     ) vars;
          let var = vars.(!index) in
          List.iter (
          fun str ->
            let grid_local = Array.copy grid in
            Propagation.instanciation var grid vars str;
            if (Propagation.filtre var grid vars) then (* le mot instancié est bon *)
              begin
                (vars.(!index)).instance = true;
                solution := (!index, str)::!solution;
                bt vars solution;
              end
            else (* le mot instancié n'est pas bon donc on annule notre instanciation*)
              begin
                for k = 0 to Array.length grid do
                  grid.(k) <- grid_local.(k);
                done
              end
         ) var.domain.liste;
        end;
      try
        bt ();
      with
        Fin ->
          for k = 0 to Array.length grid do
            Printf.printf "%s\n" grid.(k);
          done
;;
