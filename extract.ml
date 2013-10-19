(* Tests if a line only has white pixels *)
let test_empty_line img h =
        let width = img.Image.w in
        let rec f img h w cpt =
          if w = width then 
            true
          else
            match Image.get_pixel_safe img (w,h) with
              | x when x > 127 -> f img h (w+1) cpt
              | _ -> if cpt = 4 then false else f img h (w+1) (cpt+1)
        in f img h 0 0
		
(* Tests if a column only has white pixels *)
let test_empty_column img w =
        let height = img.Image.h in
        let rec f img h w cpt =
          if h = height then
            true
          else 
            match Image.get_pixel_safe img (w,h) with
              | x when x > 127 -> f img (h+1) w cpt
              | _ -> if cpt = 7 then false else f img (h+1) w (cpt+1)
        in f img 0 w 0
		
(* Tests if a part of a column only has white pixels *)
(* let test_empty_column2 beg en img w =
        let rec f img h w =
          if h = en+1 then
            true
          else 
            match Image.get_pixel_safe img (w,h) with
              | x when x > 127 -> true && f img (h+1) w
              | _ -> false
        in f img beg w 
*)
		
(* Sets the line h all in grey *)
let set_grey_line img h =
        for w = 0 to img.Image.w - 1 do
                Image.set_pixel_safe img (w,h) 127
        done

(* Sets the column w all in grey *)
let set_grey_column img w =
        for h = 0 to img.Image.h - 1 do
                Image.set_pixel_safe img (w,h) 127
        done

(* Sets a part of the column w all in grey *)
(* let set_grey_column2 beg en img w =
        for h = beg to en do
                Image.set_pixel_safe img (w,h) 127
        done
*)

(* Detects the empty lines and replace them by grey color *)
let trace_lines img =
        for h = 0 to img.Image.h - 1 do
                if test_empty_line img h then
                        set_grey_line img h
        done;
        img
    
(* Detects the empty columns and replace them by grey color *)    
let trace_lines_column img =
        for w = 0 to img.Image.w - 1 do
                if test_empty_column img w then
                        set_grey_column img w
        done;
        img

(* let trace_lines_column2 beg en img =
        for w = 0 to img.Image.w - 1 do
                if test_empty_column2 beg en img w then
                        set_grey_column2 beg en img w
        done;
        img
*)

(* Extract all the lines that aren't grey. This function return the 
  list of int*int intervals where there is a line. The list returned is
  backwards. Use a List.rev list to have list in order *)
let extract_lines img =
  let rec extr img h l =
    if h < img.Image.h then
      match Image.get_pixel_safe img (0,h) with
        | 127 -> let lis =
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

(* Extract all the columns that aren't grey. This function return the 
  list of int*int intervals where there is a line. The list returned is
  backwards. Use a List.rev list to have list in order *)
let extract_lines_column img =
  let rec extr img w l =
    if w < img.Image.w then
      match Image.get_pixel_safe img (w,0) with
        | 127 -> let lis =
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
  extr img 0 []