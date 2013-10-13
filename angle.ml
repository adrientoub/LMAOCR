let pi = 3.141592654

type point =
{
  x: int;
  y: int;
}

type pixel = 
{
  x: int;
  y: int;
  r: float array;
}

(* Get dimensions (width, heigth) of an image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

let initPixel x0 y0  = 
begin
  let p = { x = x0; y = y0; r = Array.init 360 (function n -> 0.)} in 
  let xf = float_of_int p.x and yf = float_of_int p.y in
     for i = 0 to 359 do
      p.r.(i) <- xf *. cos (float_of_int i *. pi /. 180.) +. yf *. sin (float_of_int i *. pi /. 180. )
     done; p
end

let transformToPoints img = 
    let (w, h) = get_dims img in
    let output = Sdlvideo.create_RGB_surface_format img [] w h and
	fini = ref false and 
	lastPixel = [] and pixelsNoirs = [] and
	i = ref 0 and j = ref 0 in
    while (!j < h && !fini = false) do
      while (!i < w && !fini = false) do
	let (r, g, b) = Sdlvideo.get_pixel_color img !i !j in
	if r = 0 && g = 0 && b = 0 then
	  fini := true;
	i := !i + 1
      done;
	j := !j + 1
    done;
