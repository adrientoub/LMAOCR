let pi = 3.141592654
let scanned = ref (Array.create_matrix 1 1 false); (* Tableau qui contient les pixels scannés *)

type point =
{
  mutable x: int;
  mutable y: int;
}

let deleteFirst l = match l with
    [] -> []
  | e::l -> l

let getMedian l = 
  if ((List.length l mod 2) = 0) then
    ((List.nth l (List.length l / 2)) +.
    (List.nth l (List.length l / 2 - 1))) /. 2.
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

(* Renvoie les pixels noirs non scannés dans un rayon de [rayon] *)
let getVoisins img x y rayon =
    let (w, h) = Function.get_dims img and voisins = ref [] in
    for j = y - rayon to y + rayon do
       for i = x - rayon to x + rayon do
	 if (i >= 0 && i < w && j >= 0 && j < h) then
	   let (r, g, b) = Sdlvideo.get_pixel_color img i j in
	   if (r = 0 && g = 0 && b = 0 && !scanned.(i).(j) = false &&
	       (i = x && j = y) = false) then
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
    result :=
      !result@(scanLetter img (List.nth !voisins i).x (List.nth !voisins i).y)
  done;
  result := !result@[(initPoint x y)];
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

 let distance a c =  sqrt( float_of_int ((a.x - c.x) * (a.x - c.x)
				       + (a.y - c.y) * (a.y - c.y) ))

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
     let distance = distance (List.nth l i) center in
     if(distance > !rayon) then
       rayon := distance +. 1.;
   done;
   int_of_float (!rayon *. 2.)
  
(* Renvoie l'angle entre p1 et p2 *)
let getAngle img p1 p2 =
  let (_,h) = Function.get_dims img in
  let p1x = float_of_int p1.x and p1y = float_of_int (h - p1.y) and
      p2x = float_of_int p2.x and p2y = float_of_int (h - p2.y) in
  if (p1x < p2x) then
    atan ( (p2y -. p1y) /. (p2x -. p1x)) *. 180. /. pi
  else
    atan ( (p1y -. p2y) /. (p1x -. p2x)) *. 180. /. pi
          

(* Transforme chaque lettre de la premiere ligne en point centré, pour pouvoir ensuite calculer l'angle *)
let transformToPoints img output =
try
  begin
    let (w, h) = Function.get_dims img
    and
	lastPixel = ref [] and blackPixels = ref [] and test = false and
	i = ref 0 and j = ref 0 and finalList = ref [] and medRayon = ref [] in
    begin
      Function.toWhite output;
      Sdlvideo.save_BMP img "bin.bmp";
      (* Initialise le tableau de scan *)
      scanned := (Array.create_matrix w h false);
(*  Detecte le premier pixel noir et le stock dans lastPixel *)
      while (!j < h && (List.length !finalList) < 100) do
	while (!i < w && (List.length !finalList) < 100) do
	  let (r, g, b) = Sdlvideo.get_pixel_color img !i !j in
	  if r = 0 && g = 0 && b = 0 && !scanned.(!i).(!j) = false then
		begin
		  lastPixel := (initPoint !i !j)::(!lastPixel);
		  if (test) then
		    Function.toWhite output;
		  (* Tant que l'on rencontre une lettre *)
		  while List.length !lastPixel > 0 do
		    (* Detecte la lettre et dessine un point au centre *)
		    let letterPoints = scanLetter img
		      (List.nth !lastPixel 0).x (List.nth !lastPixel 0).y
		    in let moy = (getMiddlePoint letterPoints) in
		       begin
			 if (List.length letterPoints > 20) then
			   begin
			     blackPixels := moy::(!blackPixels);
			     if (test && List.length !finalList < 50) then
			   Sdlvideo.put_pixel_color output moy.x moy.y (0,0,0);
      	 medRayon := !medRayon@[float_of_int (getRayon letterPoints moy)];
			     medRayon := List.sort compareTo !medRayon;
			     let nextLetter =
			     (getVoisins img  moy.x moy.y
				(int_of_float (getMedian !medRayon))) in
			     if (List.length nextLetter > 0) then
			       let lePlusPres = getNearest nextLetter moy in
			       lastPixel := (!lastPixel)@[lePlusPres];
			   end;

			 lastPixel := deleteFirst !lastPixel;
		       end;
		  done;
		  (* Si on detecte plus d'une lettre on l'ajoute a la liste *)
		  if (List.length !blackPixels > 1 ) then
		      let moyFinal1 = ref (initPoint 0 0) and
			  moyFinal2 = ref (initPoint 0 0) in
		      begin
			(* Ajout des premiers pixels *)
			for i = 0 to (List.length !blackPixels) / 2 - 1 do
			  !moyFinal1.x <- !moyFinal1.x
			  + (List.nth !blackPixels i).x;
			  !moyFinal1.y <- !moyFinal1.y
			  + (List.nth !blackPixels i).y;
			done;
			(* Division des premiers pixels *)
			if (List.length !blackPixels / 2) > 0 then
			  begin
			    !moyFinal1.x <- !moyFinal1.x
			    / ( (List.length !blackPixels) / 2);
			    !moyFinal1.y <- !moyFinal1.y
			    / ( (List.length !blackPixels) / 2);
			  end;
			(* Ajout des seconds pixels *)
			for i = (List.length !blackPixels) / 2
			  to (List.length !blackPixels) - 1 do
			  !moyFinal2.x <- !moyFinal2.x
			  + (List.nth !blackPixels i).x;
			  !moyFinal2.y <- !moyFinal2.y
			  + (List.nth !blackPixels i).y;
			done;
			(* Division des seconds pixels *)
			if (List.length !blackPixels
			    - List.length !blackPixels / 2) > 0 then
			  begin
			    !moyFinal2.x <- !moyFinal2.x
			    / (List.length !blackPixels
			       - List.length !blackPixels / 2);
			    !moyFinal2.y <- !moyFinal2.y
			    / (List.length !blackPixels
			       - List.length !blackPixels / 2);
			  end;

			let angle = getAngle img !moyFinal1 !moyFinal2 in
			finalList := angle::(!finalList);
			(* Test *)
			if (test && List.length !finalList < 20) then
			  begin
			    Printf.printf "Angle de la ligne %i: %f\n"
			      (List.length !finalList - 1) angle;
			    Sdlvideo.save_BMP output
			      ("rendu/rendu" ^
			(string_of_int (List.length !finalList - 1))
			       ^ ".bmp");
			  end;
		      end;
		end;
	  blackPixels := [];
	  lastPixel := [];
	  i := !i + 1;
	done;
	i := 0;
	j := !j + 1;
      done;
      finalList := (List.sort compareTo !finalList);
      let angle = getMedian !finalList in
      Printf.printf "Nombre de lignes : %i\n" (List.length !finalList);
      angle
    end;
  end;
with
  _->failwith "Error : Black picture"
