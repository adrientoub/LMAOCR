let scanned2 = ref (Array.create_matrix 1 1 false) (* Tableau qui contient les pixels scannés *)

(* Tests if a line only has white pixels *)
let test_empty_line img h w0 wmax =
  let rec f img h w  =
    if w = wmax then 
      true
    else
      match Sdlvideo.get_pixel_color img w h with
        | (x,_,_) when x >= 127 -> f img h (w+1) (* SI c'est du blanc ou du gris on relance *)
        | _ -> false (* Sinon on est tombé sur une lettre, donc la ligne n'est pas vide *)
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
let trace_lines img w0 wmax =
  let (_,height) = Function.get_dims img in
  for h = 0 to height - 1 do
    if test_empty_line img h w0 wmax then
      set_grey_line img h w0 wmax
  done
    
(* Detects the empty columns and replace them by grey color *)    
let trace_lines_column img h0 hmax =
  let (width, _) = Function.get_dims img in
  for w = 0 to width - 1 do
    if test_empty_column img w h0 hmax then
      set_grey_column img w h0 hmax
  done

let findWhite img = 
  let (width, height) = Function.get_dims img and i = ref 0 and j = ref 0 and compteur = ref 0 and nbitemax = 30 in
  while !j < height && !compteur < nbitemax do
    while !i < width && !compteur < nbitemax do
      let (r,g,b) = Sdlvideo.get_pixel_color img !i !j in
      if (r = 255 && g = 255 && b = 255) then
	begin
	  (* On determine la largeur et la longueur du bloc *)
	  let x = ref !i and y = ref !j in
	  while (!x < width && Sdlvideo.get_pixel_color img !x !j = (255,255,255)) do
	    x := !x + 1;
	  done;
	  while (!y < height && Sdlvideo.get_pixel_color img !i !y = (255,255,255)) do
	    y := !y + 1;
	  done;
	  (*for ni = !i to (!x-1) do
	    for nj = !j to (!y-1) do
	      !scanned2.(!i).(!j) <- true;
	      Printf.printf "Cherjiojiojition...\n%!";
	    done;
	  done;*)
	  trace_lines img !i !x;
	  trace_lines_column img !j !y;
	  (* Printf.printf "Position : %i %i w,h : %i %i\n" !i !j !x !y; *)
	  i := !x;
	  j := !y;
	  compteur := !compteur + 1;
	end;
      i := !i + 1;
    done;
    i := 0;
    j := !j + 1;
  done

let charDetection img = 
    let (width, height) = Function.get_dims img in
    trace_lines img 0 (width-1);
    trace_lines_column img 0 (height-1);
    scanned2 := Array.create_matrix width height false;
    findWhite img
