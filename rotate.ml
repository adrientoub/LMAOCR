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
let getInitColor img srcX srcY decX decY = 
  let initColor = int_of_float (
       (1. -. decX) *. (1. -. decY) *.(float_of_color (Sdlvideo.get_pixel_color img (truncate srcX) (truncate srcY)))
    +. decX *. (1. -. decY) *. (float_of_color (Sdlvideo.get_pixel_color img (1 + truncate srcX) (truncate srcY)))
    +. (1. -. decX) *. decY *. (float_of_color (Sdlvideo.get_pixel_color img (truncate srcX) (1 + truncate srcY)))
    +. decX *. decY *. (float_of_color (Sdlvideo.get_pixel_color img (1 + truncate srcX) (1 + truncate srcY)))) in
  if initColor > 127 then 255 
  else 0
  
(* Weighted rotation *)
let rotateWeighted img dst angDegre =
  let ang = degreToRadian angDegre in
  let (w,h) = get_dims img in
  if ang <> 0. then
    let cosAng = cos(ang) and sinAng = sin(ang) in
    for i = 0 to w-1 do
      for j = 0 to h-1 do  
	if Sdlvideo.get_pixel_color img i j = (0,0,0) then
	  let srcX = initX i j cosAng sinAng ((w-1)/2) ((h-1)/2)
	  and srcY = initY i j cosAng sinAng ((w-1)/2) ((h-1)/2)   in
	  let decX = srcX -. floor(srcX)
	  and decY = srcY -. floor(srcY) in
	  let x = int_of_float srcX and y = int_of_float srcY in
	  let color = (getInitColor img srcX srcY decX decY) in
          if isInBound img x y then	  
	    Sdlvideo.put_pixel_color dst x y (color, color, color) 
      done
    done
  else
     for i = 0 to w-1 do
      for j = 0 to h-1 do  
	Sdlvideo.put_pixel_color dst i j (Sdlvideo.get_pixel_color img i j)
      done
     done

