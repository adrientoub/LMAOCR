(* Get the initial postion of the (x,y) pixel in the source image, using -ang in a rotation matrix *)
let initX x y cosAng sinAng cx cy =
  float_of_int cx +. float_of_int(x - cx) *. cosAng -. float_of_int(y - cy) *. sinAng

(* Get the initial postion of the (x,y) pixel in the source image, using -ang in a rotation matrix *) 
let initY x y cosAng sinAng cx cy =
  float_of_int cy +. float_of_int(x - cx) *. sinAng +. float_of_int(y - cy) *. cosAng
  
(* try to figure out where each pixel of the scanned image were in the original image (the image before scanning) *)
let rotate img dst angDegre =  
  let (w,h) = Function.get_dims img
  and ang = Function.degreToRadian angDegre  in
  let cosAng = cos(ang) and sinAng = sin(ang) in
  for i = 0 to w-1 do
    for j = 0 to h-1 do  
      if Sdlvideo.get_pixel_color img i j = (0,0,0) then
	let x = int_of_float (initX i j cosAng sinAng ((w-1)/2) ((h-1)/2))
	and y = int_of_float (initY i j cosAng sinAng ((w-1)/2) ((h-1)/2)) in
        if Function.isInBound img x y then	  
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
       (1. -. decX) *. (1. -. decY) *. Function.level (T_matrix.get matrix srcX srcY)
    +. decX *. (1. -. decY) *. Function.level (T_matrix.get matrix xmore srcY)
    +. (1. -. decX) *. decY *. Function.level (T_matrix.get matrix srcX ymore)
    +. decX *. decY *. Function.level (T_matrix.get matrix xmore ymore) in initColor
  
(* Weighted rotation *)
let rotateWeighted img dst angDegre =  
  begin
  let ang = Function.degreToRadian angDegre in
  let (w,h) = Function.get_dims img 
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
  
