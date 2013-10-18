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

let showHelp () = 
  Printf.printf "Pour lancer l'ocr utilisez les paramètres suivants : \n
--help : afficher l'aide
./lmaocr [nom du fichier] : lancer l'ocr avec l'image en paramètre
./lmaocr [nom du fichier] [angle] : lancer l'ocr avec l'angle défini en paramètre
./lmaocr [nom du fichier] [-o [sauvegarde]] : sauvegarde le rendu dans l'image sauvegarde\n";
  exit 0

let saveImage finalImage = 
  let length = Array.length (Sys.argv) in
  for i = 3 to length-1 do
    if (compare Sys.argv.(i) "-o") = 0 then 
      if (length >= i+1) then 
	  Sdlvideo.save_BMP finalImage Sys.argv.(i+1);
 done
 
(* main *)

let main () =
  begin
    (* Get image file name *)
    if (Array.length (Sys.argv) < 2) || ((compare Sys.argv.(1) "--help") = 0) then
      showHelp ();

    (* get the rotation's angle *)
    let angle = 
    if Array.length (Sys.argv) >= 3 then 
      float_of_string (Sys.argv.(2))
    else 
      0. in

    (* Initialisation of SDL *)
    Printf.printf "Initialisation\n";
    sdl_init ();

    (* Loading *)
    Printf.printf "Loading image\n";
    let src = Sdlloader.load_image Sys.argv.(1) in
    Printf.printf "Image loaded\n";

    (* Apply filter against noise (currently a relaxed median filter) *)
    Printf.printf "Applying anti-noise filters\n";
    let (w,h) = get_dims src in
    let filteredImage = Sdlvideo.create_RGB_surface_format src [] w h in      
    Filters.applyRelaxedFilterMedianGrey src filteredImage;
    Printf.printf "Anti-noise filters applied\n";
    
    (* Make a copy of the filtered image for the rotation *)
    let filteredImageCopy = Sdlvideo.create_RGB_surface_format filteredImage [] w h in
    
    (* Binarize the filtered image using Ostu's method for setting the threshold *)
    Printf.printf "Binarization...\n";
    let binarizedImage = Sdlvideo.create_RGB_surface_format filteredImage [] w h in
    Binarization.binarization filteredImage binarizedImage;
    Printf.printf "Binarization done\n";
      
    (* Detect the angle using Hough transform *)
    Printf.printf "Angle detecting...\n";
    let points = Sdlvideo.create_RGB_surface_format binarizedImage [] w h in
    Angle.transformToPoints binarizedImage points;
    Printf.printf "Angle found!\n";
    
    (* Rotation using Bilinear interpolation after the rotation is done *)  
    Printf.printf "Rotating...\n";
    let rotatedImage = Sdlvideo.create_RGB_surface_format filteredImageCopy [] w h in
    Function.toWhite rotatedImage;
    Rotate.rotateWeighted filteredImageCopy rotatedImage angle;
    Printf.printf "Rotation done\n";
    
    
    (* Binarize the rotated image (again using Otsu's method) *)
    let pretreatedImage = Sdlvideo.create_RGB_surface_format rotatedImage [] w h in
    Binarization.binarization rotatedImage pretreatedImage;
    Printf.printf "Pretreatement done bitches\n";    
        
    (* Create the display surface in doublebuffering with the image size *)
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in  
    show pretreatedImage display;   
    saveImage pretreatedImage;
      (* on attend une touche *)
      wait_key ();
      (* on quitte *)
      exit 0
  end

(* Old main *)
(*
let main () =
  begin
    (* Nous voulons en argument le nom du fichier *)
    if (Array.length (Sys.argv) < 2) || ((compare Sys.argv.(1) "--help") = 0) then
      showHelp ();

    (* détection de l'angle en ligne de commande *)
    let angle = 
    if Array.length (Sys.argv) >= 3 then 
      float_of_string (Sys.argv.(2))
    else 
      0. in
    (* Initialisation de SDL *)
    sdl_init ();
    (* Chargement d'une image *)
    let img = Sdlloader.load_image Sys.argv.(1) in
    (* On récupère les dimensions *)
    let (w,h) = get_dims img in
    let binarizedImage = Sdlvideo.create_RGB_surface_format img [] w h in
    Binarization.binarization img binarizedImage;
    (* Creer un fichier qui a remplacé les lettre par des points *)
    let points = Sdlvideo.create_RGB_surface_format img [] w h in
    Angle.transformToPoints binarizedImage points; 
    let finalImage = Sdlvideo.create_RGB_surface_format img [] w h in
    Rotate.toWhite finalImage;
    Rotate.rotateWeighted binarizedImage finalImage angle; 
    (* On crée la surface d'affichage en doublebuffering de la taille exacte de l'image *)
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in  
    show finalImage display;   
    saveImage finalImage;
      (* on attend une touche *)
      wait_key ();
      (* on quitte *)
      exit 0
  end
*)

let _ = main ()
