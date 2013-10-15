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
  
*  ------------ Mandatory fonctions --------------- *)

(* upper bound and lower bound x by 255. and 0. *)
let borneFloat x =
  if x < 0. then
    0.
  else if x > 255. then
    255.
  else 
    x

(* ------------------- Matrix ---------------------- *)

(* type matrix composed by w = width, h = height and 2D array of color *)
type matrix = 
  {
    w : int;
    h : int;
    matrix : (int*int*int) array array;
  }

(* Create a new white matrix with given width and height *)
let init width height =
  let new_matrix =
    {
       w = width;
       h = height;       
       matrix = Array.create_matrix width height (0,0,0);
    }
  in new_matrix

(* Is x y in matrix's bounds ? *)
let isInMatrix matrix x y =
  (x >= 0) && (x < matrix.w) && (y >= 0) && (y < matrix.h)

(* Return the tab[x][y] color *) 
let get matrix x y =
  if isInMatrix matrix x y then
    matrix.matrix.(x).(y)
  else
    failwith "Out of matrix's bounds!"
    
(* Set the tab[x][y] color *)
let set matrix x y color =
  if isInMatrix matrix x y then
    begin
    matrix.matrix.(x).(y) <- color;
    matrix;
    end  
  else
    failwith "Out of matrix's bounds!"  

(* Copy matrix *)
let copy matrixToCopy =
  begin
  let matrixCopied = init (matrixToCopy.w) (matrixToCopy.h)   in 
  for i = 0 to matrixCopied.w - 1 do
    for j = 0 to matrixCopied.h -1 do
      matrixCopied.matrix.(i).(j) <- matrixToCopy.matrix.(i).(j);
    done;
  done;
  matrixCopied;
  end

(* Put the img in a matrix *)
let imgToMatrix img =  
  begin
  let (w,h) = get_dims img in
  let matrix = initMatrix w h in     
    for i = 0 to w-1 do
      for j = 0 to h-1 do
	matrix.matrix.(i).(j) <- Sdlvideo.get_pixel_color img i j
      done;
    done; 
  matrix;
  end
  
(* Save the matrix in a image, return the image? *)
let matrixToImg matrix = 
  begin
  let w = matrix.w and h = matrix.h
  and bpp = 8 and rmask = Int32.of_int(255) and gmask = Int32.of_int(255) and bmask = Int32.of_int(255) and amask = Int32.of_int(255) in
  let img = Sdlvideo.create_RGB_surface [] w h bpp rmask gmask bmask amask in
  for i = 0 to w-1 do
    for j = 0 to h-1 do
      Sdlvideo.put_pixel_color img i j matrix.matrix.(i).(j);
    done;
  done;
  img;
  end  

(* Return the color of the central pixel after application of the covolution matrix.
tabPixel and tabCoeff need to have the same size and be square (3x3 * 3x3, 5x5 * 5x5, ...) *)
let multLocal tabPixel tabCoeff = 
  let w_tabPixel = Array.length tabPixel and h_tabPixel = Array.length tabPixel.(0) 
  and w_tabCoeff = Array.length tabCoeff and h_tabCoeff = Array.length tabCoeff.(0) in
  if (w_tabPixel = w_tabCoeff) && (h_tabPixel = h_tabCoeff) then
      begin
	let rReslt = ref 0. and gReslt = ref 0. and bReslt = ref 0. in
	for i = 0 to w_tabPixel do
	  for j = 0 to w_tabPixel do
	    rReslt := !rReslt +. float_of_int(tabPixel.(i).(j).(0)) *. tabCoeff.(i).(j);
	    gReslt := !gReslt +. float_of_int(tabPixel.(i).(j).(1)) *. tabCoeff.(i).(j);
	    bReslt := !bReslt +. float_of_int(tabPixel.(i).(j).(2)) *. tabCoeff.(i).(j);
	  done;
	done;
	rReslt := borneFloat(!rReslt);
	gReslt := borneFloat(!gReslt);
	bReslt := borneFloat(!bReslt);
	let color = (truncate(!rReslt), truncate(!gReslt), truncate(!bReslt)) in
	color
      end
  else
    failwith "Arrays need to have the same length"


(*
(* Weighted rotation *)
let rotateWeighted img dst angDegre =
  let ang = degreToRadian angDegre in
  let (w,h) = get_dims img in
  imgMatrix = t_matrix.imgToMatrix img in
  if ang <> 0. then
    let cosAng = cos(ang) and sinAng = sin(ang) in
    for i = 0 to w-1 do
      for j = 0 to h-1 do  
	if imgMatrix.matrix = (0,0,0) then
	  let srcX = initX i j cosAng sinAng ((w-1)/2) ((h-1)/2)
	  and srcY = initY i j cosAng sinAng ((w-1)/2) ((h-1)/2)   in
	  let decX = srcX -. floor(srcX)
	  and decY = srcY -. floor(srcY) in
	  let x = int_of_float srcX and y = int_of_float srcY in
	  let tabCoeff = Array.make_matrix 3 3 0. in
	  tabCoeff = ((* ... *);
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
	

     *)
     

