(* 
Ce programme est un OCR développé par l'équipe HipsterEgg dans le cadre d'un projet de deuxième année à l'EPITA
*)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)
 
(* initialisation de SDL *)
let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end
 
(* attendre une touche ... *)
let rec wait_key () =
  let e = Sdlevent.wait_event () in
    match e with
    Sdlevent.KEYDOWN _ -> ()
      | _ -> wait_key ()
 
(*
  show img dst
  affiche la surface img sur la surface de destination dst (normalement l'écran)
*)
let show img dst =
  let d = Sdlvideo.display_format img in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst

(* obtient le niveau du gris dans l'image *)
let level (r, g, b) = 
  let rf = float_of_int r and gf = float_of_int g and bf = float_of_int b in
(0.3 *. rf +. 0.59 *. gf +. 0.11 *. bf)/. 255.

(* transforme une couleur en gris *)
let color2grey (r, g, b) = 
  let grey = int_of_float (level (r, g, b) *. 255.) in
  (grey, grey, grey)

(* transforme une image en niveau de gris *)
let image2grey src dst =
  let (w, h) = get_dims src in 
  for i = 0 to w do
    for j = 0 to h do 
      let color = Sdlvideo.get_pixel_color src i j in 
      Sdlvideo.put_pixel_color dst i j (color2grey color)
    done 
  done

(* passe une image en noir et blanc
Le résultat n'est pas très joli *)
let image2bnw src dst =
  let (w, h) = get_dims src in 
  for i = 0 to w do
    for j = 0 to h do
      let color = Sdlvideo.get_pixel_color src i j in 
      let lvl = level color in 
      let newColor = if lvl < 0.5 then 0 else 255 in 
      Sdlvideo.put_pixel_color dst i j (newColor, newColor, newColor)
    done 
  done

let showHelp () = 
  Printf.printf "Pour lancer l'ocr utilisez les paramètres suivants : \n
--help : afficher l'aide
./lmaocr [nom du fichier] : lancer l'ocr avec l'image en paramètre
./lmaocr [nom du fichier] [angle] : lancer l'ocr avec l'angle défini en paramètre\n";
  exit 0

(* main *)
let main () =
  begin
    (* Nous voulons en argument le nom du fichier *)
    if (Array.length (Sys.argv) < 2) || ((compare Sys.argv.(1) "--help") = 0) then
      showHelp ();
	
    (* détection de l'angle en ligne de commande *)
    let angle = 
    if Array.length (Sys.argv) = 3 then 
      float_of_string (Sys.argv.(2))
    else 
      5. in
    (* Initialisation de SDL *)
    sdl_init ();
    (* Chargement d'une image *)
    let img = Sdlloader.load_image Sys.argv.(1) in
    (* On récupère les dimensions *)
    let (w,h) = get_dims img in
    let bnwImage = Sdlvideo.create_RGB_surface_format img [] w h in 
    image2bnw img bnwImage;
    let finalImage = Sdlvideo.create_RGB_surface_format img [] w h in
    Rotate.toWhite finalImage;
    Rotate.rotate bnwImage finalImage angle;
    (* On crée la surface d'affichage en doublebuffering de la taille exacte de l'image *)
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
    (* on affiche l'image *)
    show finalImage display;
      (* on attend une touche *)
      wait_key ();
      (* on quitte *)
      exit 0
  end
 
let _ = main ()
