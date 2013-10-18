(*  ------------ Usefulls fonctions --------------- *)

(* Get image's dimensions *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

(* Is the pixel (x,y) in bound of img ? *)
let isInBound img x y =
  let (w,h) = get_dims img in 
    (x >= 0) && (y >= 0) && (x <= w-1) && (y <= h-1)

(* Calculate the luminosity of a pixel *)
let level (r,g,b) =
  let rf = float_of_int r and gf = float_of_int g and bf = float_of_int b
  in (0.3*.rf +. 0.59*.gf +. 0.11*.bf)/.255.

(* pi *)
let pi = 3.1415965359

(* Convert degree to  radian *)
let degreToRadian ang = 
  (ang /. 180.) *. pi

let deleteFirst l = match l with
    [] -> []
  | e::l -> l

(* return min value between x and y *)
let min x y =
  if x < y then x else y

(* upper bound and lower bound x by 255. and 0. *)
let borneFloat x =
  if x < 0. then
    0.
  else if x > 255. then
    255.
  else 
    x

(* upper bound et lower bound x by 255 and 0 *)
let borne x =
  if x < 0 then
    0
  else if x > 255 then
    255
  else 
    x
