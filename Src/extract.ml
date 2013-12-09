let scannedCorner = ref [] (* List which contains scanned pixels *)
let imgList = ref []
let resultList = ref []
let alphabetList = ref []

(* Tests if a line only has white pixels *)
let test_empty_line img h w0 wmax =
  let rec f img h w  =
    if w = wmax then 
      true
    else
      match Sdlvideo.get_pixel_color img w h with
        | (x,_,_) when x >= 127 -> f img h (w+1)
        | _ -> false
  in f img h w0
		
(* Tests if a column only has white pixels *)
let test_empty_column img w h0 hmax =
  let rec f img h w =
    if h = hmax then
      true
    else 
      match Sdlvideo.get_pixel_color img w h with
        | (x,_,_) when x >= 127 -> f img (h+1) w
        | _ -> false
  in f img h0 w
		
(* Sets the line h all in grey *)
let set_grey_line img h w0 wmax =
    for x = w0 to wmax do
      Sdlvideo.put_pixel_color img x h (127,127,127)
    done

(* Sets the column w all in grey *)
let set_grey_column img w h0 hmax =
  for x = h0 to hmax do
    Sdlvideo.put_pixel_color img w x (127,127,127)
  done

(* Detects the empty lines and replace them by grey color *)
let trace_lines img w0 wmax h0 hmax =
  let result = ref false in
  for h = h0 to hmax do
    if test_empty_line img h w0 wmax then
      begin
	set_grey_line img h w0 wmax;
	result := true;
      end
  done;
  !result
    
(* Detects the empty columns and replace them by grey color *)    
let trace_lines_column img h0 hmax w0 wmax =
  let result = ref false in
  for w = w0 to wmax do
    if test_empty_column img w h0 hmax then
      begin
	set_grey_column img w h0 hmax;
	result := true;
      end
  done;
  !result

(* Return the width and height of the current white block *)
let getDimension img i j = 
  let (width, height) = Function.get_dims img  and x = ref i and y = ref j in
  while (!x < width && (Sdlvideo.get_pixel_color img !x j <> (127,127,127))) do
    x := !x + 1;
  done;
  while (!y < height && (Sdlvideo.get_pixel_color img i !y <> (127,127,127))) do
    y := !y + 1;
  done;
  (!x, !y)

(* Return the topleft corner of the current white block *)
let getLeftCorner img i j = 
  let x = ref i and y = ref j in
  while (!x > 0 && (Sdlvideo.get_pixel_color img !x j <> (127,127,127))) do
    x := !x - 1;
  done;
  while (!y > 0 && (Sdlvideo.get_pixel_color img i !y <> (127,127,127))) do
    y := !y - 1;
  done;
  (!x + 1, !y + 1)

let print img = 
  let ite = if List.length !resultList < 50 then List.length !resultList - 1 else 49 in
  for x = 0 to ite do
    let buffer = List.nth !resultList x in let (w,h) = (Array.length buffer, Array.length buffer.(0)) in
    let result = Sdlvideo.create_RGB_surface_format img [] w h in
    for i = 0 to w - 1 do
      for j = 0 to h - 1 do
	Sdlvideo.put_pixel_color result i j buffer.(i).(j);
      done;
    done;
    Sdlvideo.save_BMP result ((string_of_int x)^".bmp");
  done

let redimensionner img =
  let taille = 16 in 
  while List.length !imgList > 0 do
    let image = ref (List.hd !imgList) in
    let (w,h) = (Array.length !image, Array.length !image.(0)) in 
    let trueImg = ref (Array.create_matrix (max w h) (max w h) (255,255,255)) in
    let decalagex  = if h > w then (h-w) else 0 and decalagey = if w > h then (w-h) else 0 in
 
    for i = 0 to w -1 do
	for j = 0 to h -1 do
	  !trueImg.(i+decalagex / 2).(j+decalagey / 2) <- !image.(i).(j);
	done;
    done;

    image := !trueImg;
    trueImg := (Array.create_matrix taille taille (255,255,255));

    let rx = (float_of_int taille /. float_of_int (max w h)) and ry = (float_of_int taille /. float_of_int (max w h)) in
    for i = 0 to taille -1 do
	for j = 0 to taille - 1 do
	    !trueImg.(i).(j) <- !image.( int_of_float (float_of_int i /. rx) ).( int_of_float (float_of_int j /. ry) );
	done;
    done;

    imgList := List.tl !imgList;
    if((Function.get_dims img) = (1100,16)) then
      alphabetList := !trueImg::!alphabetList
    else
      resultList := !trueImg::!resultList;
  done;
   if((Function.get_dims img) = (1100,16)) then
     alphabetList := List.tl !alphabetList

let charDetection img =
scannedCorner := []; (* List which contains scanned pixels *)
imgList := [];

  let (width, height) = Function.get_dims img and i = ref 0 and j = ref 0 in
  while !j < height do
    while !i < width do
      let (r,g,b) = Sdlvideo.get_pixel_color img !i !j in
      if (r = 255 && g = 255 && b = 255) then
	begin
	  (* Split the blocs caracters *)
	  let (cornerI, cornerJ) = getLeftCorner img !i !j in
	  let result = ref (not (List.exists (function (x,y) -> x = cornerI && y = cornerJ ) !scannedCorner)) in
	 
	  while !result do
	    let (w, h) = getDimension img cornerI cornerJ in
	    let resultLines = trace_lines img cornerI (w - 1) cornerJ (h - 1) and resultColumns = trace_lines_column img cornerJ (h - 1) cornerI (w - 1) in
	    result := (resultLines || resultColumns);
	  done;
	 
	  scannedCorner := (cornerI,cornerJ)::!scannedCorner;
	end;
      i := !i + 1;
    done;
    i := 0;
    j := !j + 1;
  done;
  Printf.printf "Recupération des caractères...\n%!";
  scannedCorner := [];
  let compteur = ref 0 and maxite = 200 in
  let alphabet = ((Function.get_dims img) = (1100,16)) in
if alphabet then
  for i = 0 to width - 1 do
  for j = 0 to height - 1 do
       if ( (Sdlvideo.get_pixel_color img i j) <> (127,127,127)) then
	 begin
	  
	   let (cornerI, cornerJ) = getLeftCorner img i j in
	   if (not (List.exists (function (x,y) -> x = cornerI && y = cornerJ ) !scannedCorner)) then	     
	     let (w, h) = getDimension img cornerI cornerJ in
	     let buffer = Array.create_matrix (w - i + 1) (h - j + 1) (255,255,255) in 
	     if !compteur < maxite then
	       begin
		 for i2 = i to w - 1 do
		   for j2 = j to h - 1 do
		     buffer.(i2 - i).(j2 - j) <- (Sdlvideo.get_pixel_color img i2 j2);
		   done;
		 done;
		 (*Sdlvideo.save_BMP imagedeouf ((string_of_int !compteur)^".bmp") ;*)
		 imgList := buffer::!imgList;
		 (*compteur := !compteur + 1;*)
	       end;
	     scannedCorner := (cornerI, cornerJ)::!scannedCorner;
	 end;
    done;
  done
else
    for j = 0 to height - 1 do
    for i = 0 to width - 1 do
       if ( (Sdlvideo.get_pixel_color img i j) <> (127,127,127)) then
	 begin
	  
	   let (cornerI, cornerJ) = getLeftCorner img i j in
	   if (not (List.exists (function (x,y) -> x = cornerI && y = cornerJ ) !scannedCorner)) then	     
	     let (w, h) = getDimension img cornerI cornerJ in
	     let buffer = Array.create_matrix (w - i + 1) (h - j + 1) (255,255,255) in 
	     if !compteur < maxite then
	       begin
		 for i2 = i to w - 1 do
		   for j2 = j to h - 1 do
		     buffer.(i2 - i).(j2 - j) <- (Sdlvideo.get_pixel_color img i2 j2);
		   done;
		 done;
		 (*Sdlvideo.save_BMP imagedeouf ((string_of_int !compteur)^".bmp") ;*)
		 imgList := buffer::!imgList;
		 (*compteur := !compteur + 1;*)
	       end;
	     scannedCorner := (cornerI, cornerJ)::!scannedCorner;
	 end;
    done;
    done;
  Sdlvideo.save_BMP img "gris.bmp";
  redimensionner img;
  Printf.printf "resultList contient %i images à traiter\n" (List.length !resultList);
  Printf.printf "alphabetList contient %i images à traiter\n" (List.length !alphabetList);
  print img
