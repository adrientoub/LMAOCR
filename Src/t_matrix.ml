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
       matrix = Array.create_matrix width height (255,255,255);
    }
  in new_matrix

(* Is x y in matrix's bounds ? *)
let isInMatrix matrix x y =
  (x >= 0) && (x < matrix.w) && (y >= 0) && (y < matrix.h)

(* Return the tab[x][y] color *)
let get matrix x y =
    matrix.matrix.(x).(y)
    
(* Set the tab[x][y] color *)
let set matrix x y color =
    matrix.matrix.(x).(y) <- color

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
  let (w,h) = Function.get_dims img in
  let matrix = init w h in
    for i = 0 to w-1 do
      for j = 0 to h-1 do
	matrix.matrix.(i).(j) <- Sdlvideo.get_pixel_color img i j
      done;
    done;
  matrix;
  end
  
(* Save the matrix in a image, return the image? *)
let matrixToImg matrix dst =
  begin
    let (w,h) = Function.get_dims dst in
    for i = 0 to w-1 do
      for j = 0 to h-1 do
	Sdlvideo.put_pixel_color dst i j matrix.matrix.(i).(j);
      done;
    done;
  end

(* Return the color of the central pixel after application of the covolution matrix.
tabPixel and tabCoeff need to have the same size and be square (3x3 * 3x3, 5x5 * 5x5, ...) *)
let multLocal tabPixel tabCoeff =
  let w_tabPixel = Array.length tabPixel and h_tabPixel = Array.length tabPixel.(0)
  and w_tabCoeff = Array.length tabCoeff and h_tabCoeff = Array.length tabCoeff.(0) in
  if (w_tabPixel = w_tabCoeff) && (h_tabPixel = h_tabCoeff) then
      begin
	let rReslt = ref 0. and gReslt = ref 0. and bReslt = ref 0. in
	for i = 0 to w_tabPixel - 1 do
	  for j = 0 to w_tabPixel - 1 do
	    rReslt := !rReslt +. float_of_int(tabPixel.(i).(j).(0)) *. tabCoeff.(i).(j);
	    gReslt := !gReslt +. float_of_int(tabPixel.(i).(j).(1)) *. tabCoeff.(i).(j);
	    bReslt := !bReslt +. float_of_int(tabPixel.(i).(j).(2)) *. tabCoeff.(i).(j);
	  done;
	done;
	rReslt := Function.borneFloat(!rReslt);
	gReslt := Function.borneFloat(!gReslt);
	bReslt := Function.borneFloat(!bReslt);	
	let color = (truncate(!rReslt), truncate(!gReslt), truncate(!bReslt)) in
	color
      end
  else
    failwith "Arrays need to have the same length"

(* Apply the covolution matrix to the whole source matrix *)
let multMatrix matrix tabCoeff =
  begin
    let matrixDst = copy matrix
    and (w,h) = (matrix.w, matrix.h) in
    for i = 0 to w-2 do
      for j = 0 to h-2 do
	let tabPixel = Array.create_matrix 3 3 (Array.make 3 0) in
	for i2 = - 1 to  1 do
	  for j2 = - 1 to  1 do
	    if isInMatrix matrix i2 j2 then
	      begin
		let (r,g,b) = get matrix (i + i2) (j + j2) in
		tabPixel.(i2).(j2).(0) <- r;
		tabPixel.(i2).(j2).(1) <- g;
		tabPixel.(i2).(j2).(2) <- b;
	      end;
	  done;
	done;
	let color = (multLocal tabPixel tabCoeff) in
	set matrixDst i j color;
      done;
    done;
    matrixDst;
  end
