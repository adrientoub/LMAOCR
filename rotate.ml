(* Convertit degré -> radian *)
let degreToRadian ang = 
  ang /. 180. *. 3.1415965359

(* Donne la position de x dans l'image original (avant scannage) *)
(* x,y : coordonnés dans l'image scannée, ang : angle dont l'image est roté(du verbe francais rotationner sisi), cx,cy : centre de l'image *) 
let initX x y ang cx cy =
  cx + truncate((float_of_int(x - cx)) *. cos(ang) -. (float_of_int(y - cy)) *. sin(ang))

(* Donne la position de y dans l'image original (avant scannage) *)
(* x,y : coordonnés dans l'image scannée, ang : angle dont l'image est roté, cx,cy : centre de l'image *) 
let initY x y ang cx cy =
 cy + truncate((float_of_int(x - cx)) *. (sin(ang)) +. (float_of_int(y - cy)) *. cos(ang))

(* Test si un pixel est noire *)
let isBlack (r,g,b) =
  let rf = float_of_int r and gf = float_of_int g and bf = float_of_int b
  in 0. = (0.3*.rf +. 0.59*.gf +. 0.11*.bf)/.255.
  
(* img = image source du scanner, dst = image de destination, version pas optimisé car la version opti need une image deja blanche de depart *)
let rotate img dst angDegre =
  let w = (Sdlvideo.surface_info img).Sdlvideo.w
  and h = (Sdlvideo.surface_info img).Sdlvideo.h
  and ang = degreToRadian anDegre  in
  
  for i = 0 to w-1 do
    for j = 0 to h-1 do     
      let x = initX i j ang (w/2) (h/2)
      and y = initY i j ang (w/2) (h/2)   in	
        if (x >= 0) && (y >= 0) && (x < w-1) && (y < h-1) then
	  if (isBlack(Sdlvideo.get_pixel_color img i j)) then
	      Sdlvideo.put_pixel_color dst x y (0,0,0)
	  else 
               Sdlvideo.put_pixel_color dst x y (255,255,255)
    done
  done
