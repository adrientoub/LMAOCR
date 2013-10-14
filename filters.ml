(*  ------------ Mandatory fonctions --------------- *)

(* Get image's dimensions *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

(* Is the pixel (x,y) in bound of img ? *)
let isInBound img x y =
  let (w,h) = get_dims img in 
    (x >= 0) && (y >= 0) && (x <= w-1) && (y <= h-1)

(* upper bound et lower bound x by 255 and 0 *)
let borne x =
  if x < 0 then
    0
  else if x > 255 then
    255
  else 
    x

(* insert an elt in a sorted list *)
let rec addSort x list = 
  match list with
      [] -> [x]
    |h::t -> if x <= h then x::h::t else h::addSort x t

(* Get the median value in a list *)
let getMedianList list = 
  let length = List.length list in
   if length mod 2 = 1 then
     List.nth list (length/2 + 1)
   else 
     List.nth list (length/2)    


(* ------------- Image to grey --------------- *)

(* Calculate the luminosity of a pixel *)
let level (r,g,b) =
  let rf = float_of_int r and gf = float_of_int g and bf = float_of_int b
  in (0.3*.rf +. 0.59*.gf +. 0.11*.bf)/.255.

(* Get the grey color of the pixel using level *)  
let toGrey (r,g,b) = 
  let gris = int_of_float (level (r,g,b) *. 255.)
  in (gris,gris,gris)

(* Conversion of the image to grayscale *)
let imageToGrey img dst =
  let (w,h) = get_dims img in
  for i = 0 to w-1 do
    for j = 0 to h-1 do
      let greyColor = toGrey(Sdlvideo.get_pixel_color img i j) in
      Sdlvideo.put_pixel_color dst i j greyColor
    done
  done


(* ------------ Median Filter Grey --------------- *)

(* Get the median value of an array of level *)
let getMedianArrayGrey tab =
  let length = Array.length tab in
    if length mod 2 = 1 then
      tab.((length/2 + 1))
    else
      tab.((length/2))

(* Get the relaxed median value of an array of level *)
let getRelaxedMedianArrayGrey tab cp =
  let median = getMedianArrayGrey tab in
  if (cp > median -. 1.) && (cp < median +. 1.) then
    cp
  else
    median

(* Put level of pixels around center pixel in a list (in a sorted way) *)
let square3x3ToListGrey img x y = 
  begin
  let listPixel = ref [] in
  for i = x-1 to x+1 do
    for j = y-1 to y+1 do
      (* if isInBound img x y then *)
	listPixel := (1.)::(!listPixel)
    done;
  done;
  List.sort (function x -> function y -> match (x,y) with
      (x,y) when x < y -> 1
    | (x,y) when x = y -> 0
    | _ -> -1) (!listPixel)
  end

(* Put level of pixels around center pixel in a array (in a sorted way) *)
let squareToArrayGrey img x y = 
  begin       
    let cpt = ref 0 in
     for i = x-1 to x+1 do
       for j = y-1 to y+1 do
	 if isInBound img x y then
	   cpt := !cpt + 1;
       done;
     done;
    let tabPixel = Array.make !cpt 0.  (*Array.Init !cpt (function n -> 0.) *)
    and cpt2 = ref 0 in  
    for i = x-1 to x+1 do
      for j = y-1 to y+1 do
	 if isInBound img x y then
	   tabPixel.(!cpt2) <- (level(Sdlvideo.get_pixel_color img i j));
	   cpt2 := !cpt2 + 1;
       done;
     done;
    Array.sort (function x -> function y -> match (x,y) with
      (x,y) when x < y -> 1
    | (x,y) when x = y -> 0
    | _ -> -1) tabPixel;
    tabPixel;
  end

(* Put level of pixels around center pixel in a array (in a sorted way), assuming the pixel has 8 neighbors *)
let square3x3ToArrayGrey img x y = 
  begin
    let tabPixel = Array.make 9 0.  (*Array.Init !cpt (function n -> 0.) *)
    and cpt = ref 0 in  
    for i = x-1 to x+1 do
      for j = y-1 to y+1 do	 
	   tabPixel.(!cpt) <- (level(Sdlvideo.get_pixel_color img i j));
	   cpt := !cpt + 1;
       done;
     done;
    Array.sort (function x -> function y -> match (x,y) with
      (x,y) when x < y -> 1
    | (x,y) when x = y -> 0
    | _ -> -1) tabPixel;
    tabPixel;
  end

(* Return the median level for a given pixel in a image, needed for mixed median filter, assume that the pixel is in bounds *)
let filterMedianGrey img x y = 
  let squarePixelLevel = squareToArrayGrey img x y in
  let medianLevel = getMedianArrayGrey squarePixelLevel in
  let colorGrey = int_of_float(medianLevel *. 255.) in
  colorGrey

(* Return the relaxed median level for a given pixel in a image, needed for mixed median filter, assume that the pixel is in bounds *)
let relaxedFilterMedianGrey img x y =
  let squarePixelLevel = square3x3ToArrayGrey img x y 
  and centerPixelLevel = level (Sdlvideo.get_pixel_color img x y) in
  let relaxedMedianLevel = getRelaxedMedianArrayGrey squarePixelLevel centerPixelLevel in
  let colorGrey = int_of_float (relaxedMedianLevel  *. 255.) in
  colorGrey  
    
(* Apply median filter for a grey output using filterMedianGrey *)
let applyFilterMedianGrey img dst =
  let (w,h) = get_dims img in  
  for i = 0 to w-1 do
    for j = 0 to h-1 do
      (* if isInBound img i j then *)       	
	let colorGrey = filterMedianGrey img i j in
	  Sdlvideo.put_pixel_color dst i j (colorGrey,colorGrey,colorGrey)
    done 
  done  
  

(* Apply relaxed median filter for a grey output using relaxedFilterMedianGrey *)
let applyRelaxedFilterMedian img dst =
  let (w,h) = get_dims img in  
  for i = 0 to w-1 do
    for j = 0 to h-1 do  
      (* if isInBound img i j then *)     
        let colorGrey = relaxedFilterMedianGrey img i j in
	  Sdlvideo.put_pixel_color dst i j (colorGrey,colorGrey,colorGrey)
    done 
  done

let applyMixedFilterMedian img dst = 
  let (w,h) = get_dims img in
  for i = 0 to w - 1 do
    for j = 0 to h - 1 do
      if (i = 0) || (i = w - 1) || (j = 0) || (j = h - 1) then
	let colorGrey = filterMedianGrey img i j in
	  Sdlvideo.put_pixel_color dst i j (colorGrey, colorGrey, colorGrey)
      else
	let colorGrey = relaxedFilterMedianGrey img i j in
	  Sdlvideo.put_pixel_color dst i j (colorGrey, colorGrey, colorGrey)
    done
  done


(* ------------- Median Filter Color --------------- *)
(*
(* Put color of pixels around center pixel in a array (in a sorted way) *)
let squareToArrayColor img x y = 
  begin       
    let cpt = ref 0 in
     for i = x-1 to x+1 do
       for j = y-1 to y+1 do
	 if isInBound img x y then
	   cpt := !cpt + 1;
       done;
     done;
    let tabPixel = Array.make !cpt (0,0,0)  (*Array.Init !cpt (function n -> 0.) *)
    and cpt2 = ref 0 in  
    for i = x-1 to x+1 do
      for j = y-1 to y+1 do
	 if isInBound img x y then
	   tabPixel.(!cpt2) <- (Sdlvideo.get_pixel_color img i j);
	   cpt2 := !cpt2 + 1;
       done;
     done;
    Array.sort (function x -> function y -> match (x,y) with
      (x,y) when level(x) < level(y) -> 1
    | (x,y) when level(x) = level(y) -> 0
    | _ -> -1) tabPixel;
    tabPixel;
  end

(* Put the color of pixels around center pixel in a array (in a sorted way), assuming the pixel have 8 neighbors *)
let square3x3ToArrayColor img x y = 
  begin
    let tabPixel = Array.make 9 0.  (*Array.Init !cpt (function n -> 0.) *)
    and cpt = ref 0 in  
    for i = x-1 to x+1 do
      for j = y-1 to y+1 do	 
	   tabPixel.(!cpt) <- (Sdlvideo.get_pixel_color img i j);
	   cpt := !cpt + 1;
       done;
     done;
    Array.sort (function x -> function y -> match (x,y) with
      (x,y) when level(x) < level(y) -> 1
    | (x,y) when level(x) = level(y) -> 0
    | _ -> -1) tabPixel;
    tabPixel;
  end

(* Return the color (r,g,b) for a given pixel in a image, needed for mixed median filter,assume that the pixel is in bounds *)
let filterMedianColor img x y = 
  let squarePixel = squareToArrayColor img x y in
  let medianColor = getMedianArrayColor squarePixel in
   medianColor
  
  *)
(* ------------ "scrub?" ------------ *)

(* *)
let scrubMatrixMult img x y = 
  let rf = ref 0  and gf = ref 0  and bf = ref 0 in
  for i = x-1 to x+1 do
    for j = y-1 to y+1 do
      if isInBound img i j then		
        let (r,g,b) = Sdlvideo.get_pixel_color img i j in
	  match (i,j) with
	    (i,j) when (i,j) = (x,y) ->   begin
	                                  rf := !rf + 5*r;
	                                  gf := !gf + 5*g;
	                                  bf := !bf + 5*b
	                                  end

	    |(_,_) ->                     begin
	                                  rf := !rf + r;
	                                  gf := !gf + g;
	                                  bf := !bf + b;
	                                  end
    done;
  done;     
   (borne(!rf),borne(!gf),borne(!bf))

(* *)
let applyScrubFilter img dst = 
  let (w,h) = get_dims img in
  for i = 0 to w-1 do
    for j = 0 to h-1 do
      let color = scrubMatrixMult img i j in
      Sdlvideo.put_pixel_color dst i j color
    done
  done
