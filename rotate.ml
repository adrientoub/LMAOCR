(*  ------------ Mandatory fonctions --------------- *)

(* upper bound and lower bound x by 255. and 0. *)
let borneFloat x =
  if x < 0. then
    0.
  else if x > 255. then
    255.
  else 
    x

(* Calculate the luminosity of a pixel *)
let level (r,g,b) =
  let rf = float_of_int r and gf = float_of_int g and bf = float_of_int b
  in (0.3*.rf +. 0.59*.gf +. 0.11*.bf)/.255.

(* Convert degree to  radian *)
let degreToRadian ang = 
  (ang /. 180.) *. 3.1415965359

(* Get dimensions (width, heigth) of an image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

(* Get the initial postion of the (x,y) pixel in the source image, using -ang in a rotation matrix *)
let initX x y cosAng sinAng cx cy =
  float_of_int cx +. float_of_int(x - cx) *. cosAng -. float_of_int(y - cy) *. sinAng

(* Get the initial postion of the (x,y) pixel in the source image, using -ang in a rotation matrix *) 
let initY x y cosAng sinAng cx cy =
  float_of_int cy +. float_of_int(x - cx) *. sinAng +. float_of_int(y - cy) *. cosAng

(* Is the pixel (x,y) in bound ? *) 
let isInBound img x y =
  let (w,h) = get_dims img in 
    (x >= 0) && (y >= 0) && (x < w-1) && (y < h-1)

(* Passe une image en blanc (provisoire) *)
let toWhite img = 
  let (w,h) = get_dims img in
  for i = 0 to w - 1 do
    for j = 0 to h - 1 do
      Sdlvideo.put_pixel_color img i j (255,255,255)
    done
  done

  
(* try to figure out where each pixel of the scanned image were in the original image (the image before scanning) *)
let rotate img dst angDegre =  
  let (w,h) = get_dims img
  and ang = degreToRadian angDegre  in
  let cosAng = cos(ang) and sinAng = sin(ang) in
  for i = 0 to w-1 do
    for j = 0 to h-1 do  
      if Sdlvideo.get_pixel_color img i j = (0,0,0) then
	let x = int_of_float (initX i j cosAng sinAng ((w-1)/2) ((h-1)/2))
	and y = int_of_float (initY i j cosAng sinAng ((w-1)/2) ((h-1)/2)) in
        if isInBound img x y then	  
	  Sdlvideo.put_pixel_color dst x y (0,0,0)
    done     
  done

let float_of_color (color, _, _) = 
  float_of_int color

(* Do a weighted average of the source pixel with its neighbors, according to the percentage it overlaps them. *)
let getWeightedColor matrix decX decY srcX srcY = 
  let xmore = if T_matrix.isInMatrix matrix (srcX - 1) srcY then srcX - 1 else srcX 
  and ymore = if T_matrix.isInMatrix matrix srcX (srcY - 1) then srcY - 1 else srcY in 
  let initColor = 
       (1. -. decX) *. (1. -. decY) *. level (T_matrix.get matrix srcX srcY)
    +. decX *. (1. -. decY) *. level (T_matrix.get matrix xmore srcY)
    +. (1. -. decX) *. decY *. level (T_matrix.get matrix srcX ymore)
    +. decX *. decY *. level (T_matrix.get matrix xmore ymore) in initColor
  
(* Weighted rotation *)
let rotateWeighted img dst angDegre =  
  begin
  let ang = degreToRadian angDegre in
  let (w,h) = get_dims img 
  and imgMatrix = T_matrix.imgToMatrix img in
  let dstMatrix = T_matrix.init w h
  and cosAng = cos(ang) and sinAng = sin(ang) in     
  for i = 0 to w-1 do
    for j = 0 to h-1 do         
    (*  if (T_matrix.get imgMatrix i j) = (0,0,0) then *)
	let srcX = initX i j cosAng sinAng ((w-1)/2) ((h-1)/2)
	and srcY = initY i j cosAng sinAng ((w-1)/2) ((h-1)/2)   in
	let decX = srcX -. floor(srcX)
	and decY = srcY -. floor(srcY) in        
	let x = truncate srcX and y = truncate srcY in	
	if T_matrix.isInMatrix imgMatrix x y then	  
	  let color = int_of_float ((getWeightedColor imgMatrix decX decY i j)*. 255.) in        
	  if T_matrix.isInMatrix dstMatrix x y then	 
	    T_matrix.set dstMatrix x y (color,color,color); 
    done;
  done; 
  T_matrix.matrixToImg dstMatrix dst
  end
  
