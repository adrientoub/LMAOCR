(* Convertit degré -> radian *)
let degreToRadian ang = 
  ang /. 180. *. 3.1415965359

(* Donne la position de x dans l'image source (l'image telle qu'elle était avant d'etre scannée pas droit)*)
(* x,y : coordonnés dans l'image scannée, ang : angle dont l'image est roté(du verbe francais rotationner sisi), cx,cy : centre de l'image *) 
let initX x y ang cx cy =
  cx + truncate((float_of_int(x - cx)) *. cos(ang) -. (float_of_int(y - cy)) *. sin(ang))

(* Donne la position de y dans l'image source *)
(* x,y : coordonnés dans l'image scannée, ang : angle dont l'image est roté, cx,cy : centre de l'image *) 
let initY x y ang cx cy =
 cy + truncate((float_of_int(x - cx)) *. (sin(ang)) +. (float_of_int(y - cy)) *. cos(ang))

(* Test si un pixel est bien dans l'image *) 
let isInBound x y width height =
	(x >= 0) && (y >= 0) && (x < width-1) && (y < height-1)
  
(* img = image source du scanner, dst = image de destination, version pas optimisé car la version opti need une image deja blanche de depart *)
let rotate img dst angDegre =
  let w = (Sdlvideo.surface_info img).Sdlvideo.w
  and h = (Sdlvideo.surface_info img).Sdlvideo.h
  and ang = degreToRadian angDegre  in
  
  for i = 0 to w-1 do
    for j = 0 to h-1 do     
      let x = initX i j ang ((w-1)/2) ((h-1)/2)
      and y = initY i j ang ((w-1)/2) ((h-1)/2)   in	
        if isInBound x y w h then
	  if Sdlvideo.get_pixel_color img i j = (0,0,0) then
	      Sdlvideo.put_pixel_color dst x y (0,0,0)
	  else 
               Sdlvideo.put_pixel_color dst x y (255,255,255)
    done
  done
