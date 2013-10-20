let pi = 3.141592654
let scanned = ref (Array.create_matrix 1 1 false); (* Tableau qui contient les pixels scannés *)

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

let getMedian l = 
  if ((List.length l mod 2) = 0) then
    ((List.nth l (List.length l / 2)) +. (List.nth l (List.length l / 2 - 1))) /. 2.
  else
    (List.nth l (List.length l / 2))    

let compareTo a b =
  if (a < b) then
    (-1)
  else
    1

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

(* Renvoie les pixels noirs non scannés dans un rayon de [rayon] *)
let getVoisins img x y rayon =
    let (w, h) = get_dims img and voisins = ref [] in
    for j = y - rayon to y + rayon do
       for i = x - rayon to x + rayon do
	 if (i >= 0 && i < w && j >= 0 && j < h) then
	   let (r, g, b) = Sdlvideo.get_pixel_color img i j in
	   if (r = 0 && g = 0 && b = 0 && !scanned.(i).(j) = false && (i = x && j = y) = false) then (* wtf i et j *)
	     begin
	       voisins := (!voisins)@[(initPoint i j)];
	     end;
       done;
    done;
    !voisins

(* Renvoie tous les pixels noirs liés au point [i] [j] *)
 let rec scanLetter img x y =
   !scanned.(x).(y) <- true;
    let voisins = ref (getVoisins img x y 1) and result = ref [] in
    for i = 0 to List.length !voisins - 1 do
      result := (!result)@(scanLetter img (List.nth !voisins i).x (List.nth !voisins i).y)
    done;
    result := (!result)@[(initPoint x y)];
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

 let distance a c =  sqrt( float_of_int ((a.x - c.x) * (a.x - c.x) + (a.y - c.y) * (a.y - c.y) ))

 let getNearest l c = 
    let predicate a b = 
    if (distance a c) > (distance b c) then
      1
    else
      -1 in (List.nth (List.sort predicate l) 0)

(* Renvoie la distance approximative jusqu'a la prochaine lettre de la ligne *)
 let getRayon l center = 
   let rayon = ref 0. in
   for i = 0 to (List.length l) - 1 do
     let distance = sqrt( float_of_int ((List.nth l i).x - center.x) *. float_of_int ((List.nth l i).x - center.x) +. float_of_int ((List.nth l i).y - center.y) *. float_of_int ((List.nth l i).y - center.y)) in
     if(distance > !rayon) then
       rayon := distance +. 1.;
   done;
   int_of_float (!rayon *. 2. *. 1.5) 
  
(* Renvoie l'angle entre p1 et p2 *)
let getAngle img p1 p2 =
  let (_,h) = get_dims img in
  let p1x = float_of_int p1.x and p1y = float_of_int (h - p1.y) and p2x = float_of_int p2.x and p2y = float_of_int (h - p2.y) in
  if (p1x < p2x) then
    atan ( (p2y -. p1y) /. (p2x -. p1x)) *. 180. /. pi
  else
    atan ( (p1y -. p2y) /. (p1x -. p2x)) *. 180. /. pi
          

(* Transforme chaque lettre de la premiere ligne en point centré, pour pouvoir ensuite calculer l'angle *)
let transformToPoints img output =
    let (w, h) = get_dims img
    and 
	lastPixel = ref [] and pixelsNoirs = ref [] and test = 1 and
	i = ref 0 and j = ref 0 and finalList = ref [] in
    begin
      Sdlvideo.save_BMP img "bin.bmp";
      Rotate.toWhite output;
      (* Initialise le tableau de scan *)
      scanned := (Array.create_matrix w h false);
(*  Detecte le premier pixel noir et le stock dans lastPixel *)
      while (!j < h && (List.length !finalList) < 200) do
	while (!i < w && (List.length !finalList) < 200) do
	  let (r, g, b) = Sdlvideo.get_pixel_color img !i !j in
	  if r = 0 && g = 0 && b = 0 && !scanned.(!i).(!j) = false then
		begin
		  lastPixel := (initPoint !i !j)::(!lastPixel);
		  (* Tant que l'on rencontre une lettre *)
		  while List.length !lastPixel > 0 do
		    (* Detecte la lettre et dessine un point au centre de celle ci *)
		    let pointsDeLaLettre = (scanLetter img (List.nth !lastPixel 0).x (List.nth !lastPixel 0).y)
		    in let moy = (getMiddlePoint pointsDeLaLettre) in
		       begin
			 if (List.length pointsDeLaLettre > 1) then
			   begin
			     pixelsNoirs := moy::(!pixelsNoirs);
			     if (List.length !finalList = test) then
			       Sdlvideo.put_pixel_color output moy.x moy.y (0,0,0);
			     let nextLetter = (getVoisins img  moy.x moy.y (30)) in
			     if (List.length nextLetter > 0) then
			       let lePlusPres = getNearest nextLetter moy in
			       lastPixel := (!lastPixel)@[lePlusPres];
			   end
			 else
			   begin
			   end;

			 lastPixel := deleteFirst !lastPixel;
		       end;
		  done;
		  if (List.length !pixelsNoirs > 1) then
		    begin
		      (* Test *)
		      if (List.length !finalList = test) then
			begin
			  print_int (List.length !pixelsNoirs);
			  Printf.printf " lettres trouvés et remplacés par des points.\n";
			end;

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
			(* Ajout des seconds pixels *)
			for i = (List.length !pixelsNoirs) / 2 to (List.length !pixelsNoirs) - 1 do
			  !moyFinal2.x <- !moyFinal2.x + (List.nth !pixelsNoirs i).x;
			  !moyFinal2.y <- !moyFinal2.y + (List.nth !pixelsNoirs i).y;
			done;
			(* Division des seconds pixels *)
			if (List.length !pixelsNoirs - List.length !pixelsNoirs / 2) > 0 then
			  begin
			    !moyFinal2.x <- !moyFinal2.x / (List.length !pixelsNoirs - List.length !pixelsNoirs / 2);
			    !moyFinal2.y <- !moyFinal2.y / (List.length !pixelsNoirs - List.length !pixelsNoirs / 2);
			  end;
			(* Ajout de l'angle a la liste *)
			
			let angle = getAngle img !moyFinal1 !moyFinal2 in
			finalList := angle::(!finalList);
			(* Test *)
			if ((*List.length !finalList = test + 1 ||*) angle > 50.) then
			  begin
			    Printf.printf "Angle de la ligne : %s\n" (string_of_float angle);
			    Sdlvideo.save_BMP output ("rendu/rendu" ^ (string_of_int (List.length !finalList - 1)) ^ ".bmp");
			  end;

		      end
		    end;
		end;
	  pixelsNoirs := [];
	  lastPixel := [];
	  i := !i + 1;
	done;
	i := 0;
	j := !j + 1;
      done;
       Sdlvideo.save_BMP output "rendu.bmp";
      finalList := (List.sort compareTo !finalList);
      let angle = getMedian !finalList in
      Printf.printf "Nombre de lignes : %i\n" (List.length !finalList);
      angle
    end;
