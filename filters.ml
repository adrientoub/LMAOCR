(* Get image's dimensions *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

(* Is the pixel (x,y) in bound of img ? *)
let isInBound img x y =
  let (w,h) = get_dims img in 
    (x >= 0) && (y >= 0) && (x < w-1) && (y < h-1)


(* ------------ Median Filter --------------- *)

(* Calculate the luminosity of a pixel *)
let level (r,g,b) =
  let rf = float_of_int r and gf = float_of_int g and bf = float_of_int b
  in (0.3*.rf +. 0.59*.gf +. 0.11*.bf)/.255.
  
let color2grey (r,g,b) = 
  let gris = int_of_float (level (r,g,b) *. 255.)
  in (gris,gris,gris)

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
     List.nth list length/2  

(* Put level of pixels around center pixel in a list (in a sorted way) *)
let square3x3ToList img x y = 
  begin
  let listPixel = ref [] in
  for i = x-1 to x+1 do
    for j = y-1 to y+1 do
      if isInBound img x y then
	listPixel := addSort (level (Sdlvideo.get_pixel_color img i j)) !listPixel;
    done
  done
  end 
    
(* dst must be completely white *)
let filtreMedian img dst =
  let (w,h) = get_dims img in  
  for i = 0 to w-1 do
    for j = 0 to h-1 do
      let color = int_of_float(getMedianList(square3x3ToList img i j) *. 255.) in
      Sdlvideo.put_pixel_color dst i j (color,color,color)
    done 
  done
  
(* ------------ "scrub?" ------------ *)

(* *)
let scrubMatrixMult img x y = 
  let (rf, gf, bf) = ref (0,0,0) in
  for i = x-1 to x+1 do
    for j = y-1 to y+1 do
      if isInBound img i j then
        let (r,g,b) = Sdlvideo.get_pixel_color img i j in
        if (i = x) && (j = y) then
	  (rf, gf, bf) := (!rf + 5*r,!gf + 5*g,!bf + 5*b)  
        else 
          (rf, gf, bf) := (!rf + r,!gf + g,!bf + b)
    done
  done
