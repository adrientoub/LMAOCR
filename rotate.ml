(* Convert degree to  radian *)
let degreToRadian ang = 
  ang /. 180. *. 3.1415965359

(* Get dimensions (width, heigth) of an image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

(* Get the initial postion of the (x,y) pixel in the source image, using -ang in a rotation matrix *)
let initX x y ang cx cy =
  cx + truncate((float_of_int(x - cx)) *. cos(ang) -. (float_of_int(y - cy)) *. sin(ang))

(* Get the initial postion of the (x,y) pixel in the source image, using -ang in a rotation matrix *) 
let initY x y ang cx cy =
  cy + truncate((float_of_int(x - cx)) *. (sin(ang)) +. (float_of_int(y - cy)) *. cos(ang))

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
  and ang = degreToRadian angDegre in
  for i = 0 to w-1 do
    for j = 0 to h-1 do
      if Sdlvideo.get_pixel_color img i j = (0,0,0) then
	let x = initX i j ang ((w-1)/2) ((h-1)/2)
	and y = initY i j ang ((w-1)/2) ((h-1)/2) in
        if isInBound img x y then
	  Sdlvideo.put_pixel_color dst x y (0,0,0)
    done
  done

