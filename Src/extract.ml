(* Tests if a line only has white pixels *)
let test_empty_line img h w0 wmax =
  let rec f img h w  =
    if w = wmax then 
      true
    else
      match Sdlvideo.get_pixel_color img w h with
        | (x,_,_) when x >= 127 -> f img h (w+1) (* SI c'est du blanc ou du gris on relance *)
        | _ -> false (* SInon on est tombé sur une lettre, donc la ligne n'est pas vide *)
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
  let (width, height) = Function.get_dims img in
  for j = height - 1 to 0 do
    for i = width - 1 to 0 do
      let (r,g,b) = Sdlvideo.get_pixel_color img i j in
      if (r = 255 && g = 255 & b = 255) then
	Printf.printf "lol";
    done;
  done

let charDetection img = 
    let (width, height) = Function.get_dims img in
    trace_lines img 0 (width-1);
    trace_lines_column img 0 (height-1)

(* Extract all the lines that aren't grey. This function return the 
  list of int*int intervals where there is a line. The list returned is
  backwards. Use a List.rev list to have list in order

let extract_lines img =
  let (_, height) = Function.get_dims img in 
  let rec extr img h l =
    if h < height then
      match Sdlvideo.get_pixel_color img 0 h with
        | (127,127,127) -> let lis =
                   (match l with
                   | [] -> []
                   |(e,-1)::li -> (e,h-1) :: li
                   |(e,f)::li -> l) in extr img (h+1) lis 
        | _ -> let lis = 
                 (match l with
                  | [] -> (h,-1) :: l
                  |(e,-1)::li -> l
                  |(e,f)::li -> (h,-1) ::l) in extr img (h+1) lis
    else
      match l with
        | [] -> []
        | (e,-1)::li -> (e,h-1)::li 
        | (e,f)::li -> l
  in
  extr img 0 []

 Extract all the columns that aren't grey. This function return the 
  list of int*int intervals where there is a line. The list returned is
  backwards. Use a List.rev list to have list in order 
let extract_lines_column img =
  let (width, _) = Function.get_dims img in 
  let rec extr img w l =
    if w < width then
      match Sdlvideo.get_pixel_color img w 0 with
        | (127,127,127) -> let lis =
                 (match l with
                  | [] -> []
                  |(e,-1)::li -> (e,w-1) :: li
                  |(e,f)::li -> l) in extr img (w+1) lis 
        | _ -> let lis = 
                (match l with
                  | [] -> (w,-1) :: l
                  |(e,-1)::li -> l
                  |(e,f)::li -> (w,-1) ::l) in extr img (w+1) lis
    else
      match l with
        | [] -> []
        | (e,-1)::li -> (e,w-1)::li 
        | (e,f)::li -> l
  in
  extr img 0 []*)
