(*  ------------ Mandatory fonctions --------------- *)

(* Get image's dimensions *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

(* Is the pixel (x,y) in bound of img ? *)
let isInBound img x y =
  let (w,h) = get_dims img in 
    (x >= 0) && (y >= 0) && (x < w-1) && (y < h-1)

(* upper bound et lower bound x by 255 and 0 *)
let borne x =
  if x < 0 then
    0
  else if x > 255 then
    255
  else 
    x

(* ---------------- Matrix ------------------- *)

(* type matrix composed by a 2D array of color, w = width, h = height *)
type matrix = 
  {
    w : int;
    h : int;
    matrix : (int*int*int) array array;
  }

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
let getColorInMatrix matrix x y =
  if isInMatrix matrix x y then
    matrix.matrix.(x).(y)
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
  

(* ------------- Image to grey --------------- *)

(* Calculate the luminosity of a pixel *)
let level (r,g,b) =
  let rf = float_of_int r and gf = float_of_int g and bf = float_of_int b
  in (0.3*.rf +. 0.59*.gf +. 0.11*.bf)/.255.

(* Get the grey color of the pixel using level *)  
let toGrey (r,g,b) = 
  let gris = int_of_float (level (r,g,b) *. 255.)
  in (gris,gris,gris)

(* Conversion of the image to grayscale *)
let imageToGrey img dst =
  let (w,h) = get_dims img in
  for i = 0 to w-1 do
    for j = 0 to h-1 do
      let greyColor = toGrey(Sdlvideo.get_pixel_color img i j) in
      Sdlvideo.put_pixel_color dst i j greyColor
    done
  done

(* ------------ Median Filter --------------- *)

(* insert an elt in a sorted list *)
let rec addSort x list = 
  match list with
      [] -> [x]
    |h::t -> if x <= h then x::h::t else h::addSort x t

(* Get the median value in a list *)
let getMedianList list = 
  let length = List.length list in
   if length mod 2 = 1 then
     List.nth list (length/2 + 1)
   else 
     List.nth list (length/2)  

(* Put level of pixels around center pixel in a list (in a sorted way) *)
let square3x3ToList img x y = 
  begin
  let listPixel = ref [] in
  for i = x-1 to x+1 do
    for j = y-1 to y+1 do
      if isInBound img x y then
	listPixel := addSort (level (Sdlvideo.get_pixel_color img i j)) !listPixel;
    done;
  done;
  !listPixel
  end 
    
(* Apply median filter *)
let applyFilterMedian img dst =
  let (w,h) = get_dims img in  
  for i = 0 to w-1 do
    for j = 0 to h-1 do
      let color = int_of_float(getMedianList(square3x3ToList img i j) *. 255.) in
      Sdlvideo.put_pixel_color dst i j (color,color,color)
    done 
  done
  
(* ------------ "scrub?" ------------ *)

(* *)
let scrubMatrixMult img x y = 
  let rf = ref 0  and gf = ref 0  and bf = ref 0 in
  for i = x-1 to x+1 do
    for j = y-1 to y+1 do
      if isInBound img i j then		
        let (r,g,b) = Sdlvideo.get_pixel_color img i j in
	  match (i,j) with
	    (i,j) when (i,j) = (x,y) ->   begin
	                                  rf := !rf + 5*r;
	                                  gf := !gf + 5*g;
	                                  bf := !bf + 5*b
	                                  end

	    |(_,_) ->                     begin
	                                  rf := !rf + r;
	                                  gf := !gf + g;
	                                  bf := !bf + b;
	                                  end

        (*if (i = x) && (j = y) then
	  rf := !rf + 5*r;
	  gf := !gf + 5*g;
	  bf := !bf + 5*b;
        else 	
          rf := !rf + r;
	  gf := !gf + g;
	  bf := !bf + b;*)         
    done;
  done;     
   (borne(!rf),borne(!gf),borne(!bf))

(* *)
let applyScrubFilter img dst = 
  let (w,h) = get_dims img in
  for i = 0 to w-1 do
    for j = 0 to h-1 do
      let color = scrubMatrixMult img i j in
      Sdlvideo.put_pixel_color dst i j color
    done
  done


