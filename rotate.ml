(*  ------------ Mandatory fonctions --------------- *)

(* upper bound and lower bound x by 255. and 0. *)
let borneFloat x =
  if x < 0. then
    0.
  else if x > 255. then
    255.
  else 
    x


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
  let initColor = int_of_float (
       (1. -. decX) *. (1. -. decY) *.(float_of_color (T_matrix.get matrix (truncate srcX) (truncate srcY)))
    +. decX *. (1. -. decY) *. (float_of_color (T_matrix.get matrix (1 + truncate srcX) (truncate srcY)))
    +. (1. -. decX) *. decY *. (float_of_color (T_matrix.get matrix (truncate srcX) (1 + truncate srcY)))
    +. decX *. decY *. (float_of_color (T_matrix.get matrix(1 + truncate srcX) (1 + truncate srcY)))) in
  if (initColor/4) > 127 then 255 
  else 0
  
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
      if (T_matrix.get imgMatrix i j) = (0,0,0) then
	let srcX = initX i j cosAng sinAng ((w-1)/2) ((h-1)/2)
	and srcY = initY i j cosAng sinAng ((w-1)/2) ((h-1)/2)   in
	let decX = srcX -. floor(srcX)
	and decY = srcY -. floor(srcY) in        
	let x = truncate srcX and y = truncate srcY in
	if T_matrix.isInMatrix imgMatrix x y then
	  let color = (getWeightedColor imgMatrix decX decY srcX srcY) in        
	  if T_matrix.isInMatrix dstMatrix x y then	  
	    T_matrix.set dstMatrix x y (color,color,color); 
    done;
  done;
  T_matrix.matrixToImg dstMatrix dst
  end
  

     
     

