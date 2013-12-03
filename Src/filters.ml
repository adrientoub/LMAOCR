(* ------------ Median Filter Grey --------------- *)

(* Get the median value of an array of level *)
let getMedianArray tab =
  let length = Array.length tab in
    if length mod 2 = 1 then
     length/2 + 1
    else
      length/2

let getPlaceArray tab x =
  begin
  let length = Array.length tab
  and pos = ref 0 in
  for i = 0 to length - 1 do
    if x = tab.(i) then
      pos := i;
  done;
  !pos;
  end


(* Get the relaxed median value of an array of level *)
let getRelaxedMedianArrayGrey tab cp =
  let median = getMedianArray tab
  and posX = getPlaceArray tab cp in
  if (posX > median - 1) && (posX < median + 1) then
    tab.(posX)
  else
    tab.(median)

(* Put level of pixels around center pixel in a array (in a sorted way) *)
let squareToArrayGrey img x y =
  begin
    let cpt = ref 0 in
     for i = x-1 to x+1 do
       for j = y-1 to y+1 do
	 if Function.isInBound img x y then
	   cpt := !cpt + 1;
       done;
     done;
    let tabPixel = Array.make 9 0.
    and cpt2 = ref 0 in
    for i = x-1 to x+1 do
      for j = y-1 to y+1 do
	 if Function.isInBound img x y then
	   tabPixel.(!cpt2) <-
	      (Function.level(Sdlvideo.get_pixel_color img i j));
	   cpt2 := !cpt2 + 1;
       done;
     done;
    Array.sort (function x -> function y -> match (x,y) with
      (x,y) when x < y -> 1
    | (x,y) when x = y -> 0
    | _ -> -1) tabPixel;
    tabPixel;
  end

(* Put level of pixels around center pixel in a array (in a sorted way)
 assuming the pixel has 8 neighbors *)
let square3x3ToArrayGrey img x y =
   begin
     let tabPixel =  Array.make 9 0.
    and cpt = ref 0 in
    for i = x-1 to x+1 do
      for j = y-1 to y+1 do
	if Function.isInBound img i j then
	  tabPixel.(!cpt) <-
	    (Function.level (Sdlvideo.get_pixel_color img i j))
	else
	  tabPixel.(!cpt) <- 0.;
	cpt := !cpt + 1;
       done;
     done;
    Array.sort (function x -> function y -> match (x,y) with
      (x,y) when x < y -> 1
    | (x,y) when x = y -> 0
    | _ -> -1) tabPixel;
    tabPixel;
  end

(* Return the median level for a given pixel in a image
needed for mixed median filter, assume that the pixel is in bounds *)
let filterMedianGrey img x y =
  let squarePixelLevel = squareToArrayGrey img x y in
  let median = getMedianArray squarePixelLevel in
  let colorGrey = int_of_float(squarePixelLevel.(median) *. 255.) in
  colorGrey

(* Return the relaxed median level for a given pixel in a image *)
(* needed for mixed median filter, assume that the pixel is in bounds *)
let relaxedFilterMedianGrey img x y =
  let squarePixelLevel = square3x3ToArrayGrey img x y
  and centerPixelLevel = Function.level (Sdlvideo.get_pixel_color img x y) in
  let relaxedMedianLevel =
    getRelaxedMedianArrayGrey squarePixelLevel centerPixelLevel in
  let colorGrey = int_of_float (relaxedMedianLevel  *. 255.) in
  colorGrey
    
(* Apply median filter for a grey output using filterMedianGrey *)
let applyFilterMedianGrey img dst =
  let (w,h) = Function.get_dims img in
  for i = 0 to w-1 do
    for j = 0 to h-1 do
      (* if isInBound img i j then *)
	let colorGrey = filterMedianGrey img i j in
	  Sdlvideo.put_pixel_color dst i j (colorGrey,colorGrey,colorGrey)
    done
  done

(*Apply relaxed median filter for a grey output using relaxedFilterMedianGrey*)
let applyRelaxedFilterMedianGrey img dst =
  let (w,h) = Function.get_dims img in
  for i = 0 to w-1 do
    for j = 0 to h-1 do
      (* if isInBound img i j then *)
        let colorGrey = relaxedFilterMedianGrey img i j in
	  Sdlvideo.put_pixel_color dst i j (colorGrey,colorGrey,colorGrey)
    done
  done

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
    let  passeHaut =  [| [|0.;0.;0.|]; [|0.;0.;0.|]; [|0.;0.;0.|] |]
  (* [| [|0.;-1.;0.|]; [|-1.;5.;-1.|]; [|0.;-1.;0.|] |]*)  in
    applyLinearFilter img dst passeHaut;
  end

let applyPasseBasFilter img dst =
  begin
    let  passeBas = [| [|1.;2.;1.|]; [|2.;8.;2.|]; [|1.;2.;1.|] |]  in
    normalize passeBas;
    applyLinearFilter img dst passeBas;
  end

let applyPasseBandeFilter img dst =
  begin 
    applyPasseHautFilter img dst;
  (*  applyPasseBasFilter dst dst; *)
  end
