(* ------------ Covolutions matrix ------------ *)

let normalize tabCoeff = 
    let somme = ref 0. in 
    begin
    for i = 0 to 2 do
      for j = 0 to 2 do
	  somme := !somme +. tabCoeff.(i).(j);
      done;
    done;
    for i = 0 to 2 do
      for j = 0 to 2 do
	tabCoeff.(i).(j) <- tabCoeff.(i).(j) /. !somme;
      done
    done
    end

let applyLinearFilter img dst tabCoeff =
  let imgMatrix = T_matrix.imgToMatrix img in
  let dstMatrix = T_matrix.multMatrix imgMatrix tabCoeff
  in T_matrix.matrixToImg dstMatrix dst


let applyPasseHautFilter img dst =
  begin
    let  passeHaut = [| [|0.;-1.;0.|]; [|-1.;5.;-1.|]; [|0.;-1.;0.|] |]  in
    applyLinearFilter img dst passeHaut;
  end

let applyPasseBasFilter img dst =
  begin
    let  passeBas = [| [|1.;2.;1.|]; [|2.;4.;2.|]; [|1.;2.;1.|] |]  in
    normalize passeBas;
    applyLinearFilter img dst passeBas;
  end
