(* 
Ce programme est un OCR développé par l'équipe HipsterEgg
dans le cadre d'un projet de deuxième année à l'EPITA
*)
 
(* initialisation de SDL *)
let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end

(*
  show img dst
  affiche la surface img sur la surface de destination dst
*)
let show img dst =
  let d = Sdlvideo.display_format img in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst

let showHelp () =
  Printf.printf "Pour lancer l'ocr utilisez les paramètres suivants : \n
--help : afficher l'aide
-help  : afficher l'aide
./lmaocr [nom du fichier] : lancer l'ocr avec l'image en paramètre
./lmaocr [nom du fichier] [angle] : lance l'ocr avec l'angle défini en paramètre
./lmaocr [nom du fichier] [-o [sauvegarde]] : save the image \n";
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
    if (Array.length (Sys.argv) < 2) || ((compare Sys.argv.(1) "--help") = 0)
      || ((compare Sys.argv.(1) "-help") = 0) then
      showHelp ();

    (* get the rotation's angle *)
    let angle = ref 0. in

    (* get the filter to use *)
    let filter = ref (
      if (Array.length (Sys.argv) > 2) then
	(Sys.argv.(2))
      else
	"") in	    

    (* Initialisation of SDL *)
    Printf.printf "Initialisation\n%!";
    sdl_init ();

    (* Loading *)
    Printf.printf "Loading image\n%!";
    let src = Sdlloader.load_image Sys.argv.(1) in
    Printf.printf "Image loaded\n%!";     

    let (w,h) = Function.get_dims src in 
    
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
    show src display;
    Function.wait_key ();
    
    (* Apply filter against noise (currently a relaxed median filter) *)
    Printf.printf "Applying anti-noise filters \n %!"; 
    let filteredImage = Sdlvideo.create_RGB_surface_format src [] w h in
      if (!filter = "m") then
	begin
	  Printf.printf "Median Filter \n %!";
	  Filters.applyRelaxedFilterMedianGrey src filteredImage;
	  Binarization.opening filteredImage filteredImage;
	  show filteredImage display;
	  Function.wait_key ();
	end
      else if (!filter = "c") then
	begin
	  Printf.printf "Passe Bande filter \n %!";
	  Filters.applyPasseBandeFilter src filteredImage;
	end
      else
	Printf.printf "No filter %!";
	Function.copyImg src filteredImage;
   
    Printf.printf "Anti-noise filters applied\n%!";

    (* Make a copy of the filtered image for the rotation *)
    let filteredImageCopy =
    Sdlvideo.create_RGB_surface_format filteredImage [] w h in
    Function.copyImg filteredImage filteredImageCopy;

    (*Binarize the filtered image using Ostu method for setting the threshold*)
    Printf.printf "Binarization...\n%!";
    let binarizedImage =
      Sdlvideo.create_RGB_surface_format filteredImage [] w h in
    Binarization.binarizationOtsu filteredImage binarizedImage;
    Printf.printf "Binarization done\n%!";

    show binarizedImage display;
    Function.wait_key ();
    (* Detect the angle using Hough transform *)
    Printf.printf "Angle detecting...\n%!";
    let points = Sdlvideo.create_RGB_surface_format binarizedImage [] w h in
    begin
      angle := Angle.transformToPoints binarizedImage points;
    Printf.printf "Angle found! %f\n%!" !angle;
    end;

    (* Rotation using Bilinear interpolation after the rotation is done *)
    Printf.printf "Rotating...\n%!";
    let rotatedImage = Sdlvideo.create_RGB_surface_format filteredImageCopy [] w h in
    Function.toWhite rotatedImage;
    Rotate.rotateWeighted filteredImageCopy rotatedImage !angle;    
    Printf.printf "Rotation done\n%!";

    show rotatedImage display;
    Function.wait_key ();

    (* Binarize the rotated image (again using Otsu's method) *)
    let pretreatedImage =
      Sdlvideo.create_RGB_surface_format rotatedImage [] w h in
    Binarization.binarizationOtsu rotatedImage pretreatedImage;

    show pretreatedImage display;
    Function.wait_key ();

    (* Detect the chacacter from the rotated and filter image *)
    Printf.printf "Character detection...\n%!";
    Extract.charDetection pretreatedImage;
    show pretreatedImage display;
    saveImage pretreatedImage;
    Printf.printf "Character detection done\n%!";

    Printf.printf "Pretreatement done\n%!";

    (* Create the display surface in doublebuffering with the image size *)


    (* on attend une touche *)
    Function.wait_key ();

    (* on quitte *)
    exit 0
  end

let _ = main ()
