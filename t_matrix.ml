(*  ------------ Mandatory fonctions --------------- *)

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
let initMatrix width height =
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


(* Apply the covolution matrix to the whole source matrix *)
let multMatrix matrix tabCoeff = 
  begin
    let (w,h) = (matrix.w, matrix.h) in
    for i = 0 to w-1 do
      for j = 0 to h-1 do
	let tabPixel = Array.create_matrix 3 3 (Array.make 3 0) in
	for i2 = i - 1 to i + 1 do
	  for j2 = j - 1 to j + 1 do
	    if isInMatrix matrix i2 j2 then
	      let (r,g,b) = get matrix i2 j2 in
	      tabPixel.(i2).(j2).(0) <- r;
	      tabPixel.(i2).(j2).(1) <- g;
	      tabPixel.(i2).(j2).(2) <- b;
	  done;
	done;
	let color = (multLocal tabPixel tabCoeff) in
	set matrix i j color
      done;
    done;
    matrix;
    end
	