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
    let (w, h) = get_dims img and voisins = ref [] and (r, g, b) = Sdlvideo.get_pixel_color img x y in
    for j = y - rayon to y + rayon do
       for i = x - rayon to x + rayon do
	 if (i >= 0 && i < w && j >= 0 && j < h && r = 0 && g = 0 && b = 0 && !scanned.(i).(j) = false && i <> x && j <> y) then
	   voisins := (initPoint i j)::(!voisins);
       done;
    done; !voisins

 let rec scanLetter img x y =
  begin
    !scanned.(x).(y) <- true;
    let voisins = ref (getVoisins img x y 1) in
    for i = 0 to List.length !voisins - 1 do
      voisins := (!voisins)@(scanLetter img (List.nth !voisins i).x (List.nth !voisins i).y)
    done;
    voisins := (initPoint x y)::(!voisins);
    !voisins;
  end

 let getMiddlePoint l = 
   let point = ref (initPoint 0 0) in
   begin
     for i = 0 to List.length l - 1 do
       begin
	 !point.x <- !point.x + (List.nth l i).x;
	 !point.y <- !point.y + (List.nth l i).y;
       end;
     done;
       !point.x <- !point.x / List.length l;
       !point.y <- !point.y / List.length l;
   end; !point

let transformToPoints img = 
    let (w, h) = get_dims img in
    let output = Sdlvideo.create_RGB_surface_format img [] w h and
	fini = ref false and 
	lastPixel = ref [] and pixelsNoirs = ref [] and
	i = ref 0 and j = ref 0 in
    begin
      scanned := (Array.create_matrix w h false); (* Initialise le tableau de scan *)
(*  Detecte le premier pixel noir et le stock dans lastPixel *)
      while (!j < h && !fini = false) do
	while (!i < w && !fini = false) do
	  let (r, g, b) = Sdlvideo.get_pixel_color img !i !j in
	  if r = 0 && g = 0 && b = 0 then
	    begin
	      fini := false;
	      lastPixel := [(initPoint !i !j)]@(!lastPixel);
	    end;
	  i := !i + 1
	done;
	j := !j + 1
      done;

      while List.length !lastPixel > 0 do
(* (* Detecte la lettre et dessine un point au centre de celle ci *)
    let pointsDeLaLettre = ref ( scanLetter img (List.nth !lastPixel 0).x (List.nth !lastPixel 0).x)
    in moy = (getMiddlePoint (!pointsDeLaLettre)) in
    begin
      pixelsNoirs := moy::(!pixelsNoirs);
      Sdlvideo.set_pixel_color img moy.x moy.y;
    end;*)
()
      done;
    end 
