let pi = 3.141592654
let scanned = ref (Array.create_matrix 1 1 false); (* Tableau qui contien les pixels scannés *)

type point =
{
  mutable x: int;
  mutable y: int;
}

type pixel = 
{
  mutable p: point;
  mutable r: float array;
}

(* Get dimensions (width, heigth) of an image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

let deleteFirst l = match l with
    [] -> []
  | e::l -> l

let initPoint x0 y0 = 
  {
    x = x0;
    y = y0;
  }

let initPixel x0 y0  = 
begin
  let p = { p = { x = x0; y = y0 }; r = Array.init 360 (function n -> 0.)} in 
  let xf = float_of_int p.p.x and yf = float_of_int p.p.y in
     for i = 0 to 359 do
      p.r.(i) <- xf *. cos (float_of_int i *. pi /. 180.) +. yf *. sin (float_of_int i *. pi /. 180. )
     done; p
end

let getVoisins img x y rayon =
    let (w, h) = get_dims img and voisins = ref [] in
    for j = y - rayon to y + rayon do
       for i = x - rayon to x + rayon do
	 if (i >= 0 && i < w && j >= 0 && j < h) then
	   let (r, g, b) = Sdlvideo.get_pixel_color img i j in
	   if (r = 0 && g = 0 && b = 0 && !scanned.(i).(j) = false && (i = x && j = y) = false) then (* wtf i et j *)
	     begin
	       Printf.printf "Trouve\n";
	       voisins := (initPoint i j)::(!voisins);
	     end;
       done;
    done;
    !voisins

 let rec scanLetter img x y =
    !scanned.(x).(y) <- true;
   Printf.printf "Debut\n";
    let voisins = ref (getVoisins img x y 1) and result = ref [] in
    for i = 0 to List.length !voisins - 1 do
      result := (!result)@(scanLetter img (List.nth !voisins i).x (List.nth !voisins i).y)
    done;
    result := (!result)@[(initPoint x y)];
    (*print_int (List.length !result);*)
    Printf.printf "Fin\n";
    !result

 let getMiddlePoint l = 
   let point = ref (initPoint 0 0) in
     for i = 0 to List.length l - 1 do
	 !point.x <- !point.x + (List.nth l i).x;
	 !point.y <- !point.y + (List.nth l i).y;
     done;
       !point.x <- !point.x / List.length l;
       !point.y <- !point.y / List.length l;
       !point

let transformToPoints img output =
    let (w, h) = get_dims img
    and
	fini = ref false and 
	lastPixel = ref [] and pixelsNoirs = ref [] and
	i = ref 0 and j = ref 0  in
    begin
      Rotate.toWhite output;
      Sdlvideo.save_BMP img "bin.bmp";
      scanned := (Array.create_matrix w h false); (* Initialise le tableau de scan *)
(*  Detecte le premier pixel noir et le stock dans lastPixel *)
      while (!j < h && !fini = false) do
	while (!i < w && !fini = false) do
	  let (r, g, b) = Sdlvideo.get_pixel_color img !i !j in
	  if r = 0 && g = 0 && b = 0 then
	    begin
	      if(List.length (scanLetter img !i !j) > 1) then
		begin
		  (*Printf.printf "Premier pixel trouvé : %s %s \n" (string_of_int !i) (string_of_int !j);*)
		  fini := true;
		  lastPixel := (initPoint !i !j)::(!lastPixel);
		end
	    end;
	  i := !i + 1;
	done;
	i := 0;
	j := !j + 1;
      done;

      while List.length !lastPixel > 0 do
 (* Detecte la lettre et dessine un point au centre de celle ci *)
	let pointsDeLaLettre = (scanLetter img (List.nth !lastPixel 0).x (List.nth !lastPixel 0).y)
	in let moy = (getMiddlePoint pointsDeLaLettre) in
	   begin
	     Printf.printf "tour";
	     pixelsNoirs := moy::(!pixelsNoirs);
	     Sdlvideo.put_pixel_color output moy.x moy.y (0,0,0);
	     Printf.printf "osef d'en dessous\n";
	     let nextLetter = ref (getVoisins img  (List.nth !lastPixel 0).x (List.nth !lastPixel 0).y 50) in
	     if (List.length !nextLetter > 0) then
	       lastPixel := (!lastPixel)@[(List.nth !nextLetter 0)];
	     lastPixel := deleteFirst !lastPixel;
	   end;
      done;
      print_int (List.length !pixelsNoirs);
      Printf.printf " lettres trouvés et remplacés par des points\n";
      Sdlvideo.save_BMP output "points1.bmp";

      let moyFinal1 = ref (initPoint 0 0) and moyFinal2 = ref (initPoint 0 0) in
      begin
	(* Ajout des premiers pixels *)
	for i = 0 to (List.length !pixelsNoirs) / 2 - 1 do
	  !moyFinal1.x <- !moyFinal1.x + (List.nth !pixelsNoirs i).x;
	  !moyFinal1.y <- !moyFinal1.y + (List.nth !pixelsNoirs i).y;
	done;
	(* Division des premiers pixels *)
	if (List.length !pixelsNoirs / 2) > 0 then
	  begin
	    !moyFinal1.x <- !moyFinal1.x / ( (List.length !pixelsNoirs) / 2);
	    !moyFinal1.y <- !moyFinal1.y / ( (List.length !pixelsNoirs) / 2);
	  end;
	(* Ajout des seconds pixels buuuuug*)
	for i = (List.length !pixelsNoirs) / 2 to (List.length !pixelsNoirs) - 1 do
	  !moyFinal2.x <- !moyFinal2.x + (List.nth !pixelsNoirs i).x;
	  !moyFinal2.y <- !moyFinal2.y + (List.nth !pixelsNoirs i).y;
	done;
	(* Division des seconds pixels *)
	if (List.length !pixelsNoirs - List.length !pixelsNoirs / 2) > 0 then
	  begin
	    !moyFinal1.x <- !moyFinal1.x / (List.length !pixelsNoirs - List.length !pixelsNoirs / 2);
	    !moyFinal1.y <- !moyFinal1.y / (List.length !pixelsNoirs - List.length !pixelsNoirs / 2);
	  end
      end

    end 
