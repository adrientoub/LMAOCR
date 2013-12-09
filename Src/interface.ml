let gtk_init = GMain.init ()
    
let img_path = ref "init"
  
let step1_clicked = ref false 
  
(* string textbox *)
let texte = ref "" 

let about () =
  let dialog = 
    GWindow.about_dialog 
      ~name:"LMAOCR" 
      ~authors:["HipstersEgg : \n
Adrien Toubiana 
Damien Pradier 
Thomas Diot
Olivier Do"]
      ~version:"1.2"
      ~website:"http://www.lmaocr.hostoi.com"
      ~position:`CENTER_ON_PARENT
      ~destroy_with_parent:true () in    
  ignore (dialog#connect#response 
            ~callback:(fun _ -> dialog#show ()));
    ignore(dialog#run ())
      
let network = Network.create_default () 

(* ---------------- Lance l'interface --------------- *)
  
let start_interface () =
  begin
  
    let _ = GMain.init () in
    
    (* Init window *)
    let window = GWindow.window 
      ~title:"LMAOCR v1.2"
      ~position:`CENTER
      ~resizable:false
      ~width:1200 
      ~height:650
      ~border_width:10 () in
    ignore(window#connect#destroy ~callback:GMain.Main.quit);
    
  (*------------------ INTERFACE ----------------------*)
    
    let big_box = GPack.hbox
      ~border_width:0
      ~packing:window#add() in
    
    let frame = GBin.frame
      ~label:"Display"
      ~width:650
      ~border_width:5
      ~packing:big_box#add() in
    
  (* ------------------- IMAGE -----------------------*)
    
    let picture_area  = GMisc.image    
      ~file:"init.png"
      ~packing:frame#add () in
    
    let right_box = GPack.vbox
      ~width:600
      ~border_width:5
      ~packing:big_box#add() in    
    
    let box_2D = GPack.vbox
      ~height:130
      ~packing:right_box#add() in
    
    
(* ----------------------  MENU  ----------------------- *)
    
    let frame_menu = GBin.frame
      ~label:"Menu"
      ~height:50
      ~packing:box_2D#add() in

    let hbox = GPack.hbox
      ~border_width:5
      ~packing:frame_menu#add() in
    
    let menu_box = GPack.button_box `HORIZONTAL
      ~layout:`SPREAD
      ~spacing:10
      ~packing:(hbox#pack ~expand:false) () in
    
    let copy_ref path = 
      img_path := path in
    
    (* OPEN FILE FILTER *)
    let checkImg () = GFile.filter
      ~name:"Pictures"
      ~patterns:[ "*.bmp" ; "*.jpg" ; "*.png"] () in   
    
    
  (* -------------------- OCR FONCTIONS ---------------------- *) 
    let sdl_init () =
      begin
	Sdl.init [`EVERYTHING];
	Sdlevent.enable_events Sdlevent.all_events_mask;
      end
    in
    
    let imageapercu () = 
      let pixbufvide = GdkPixbuf.create
	~width:600
	~height:600
	() in
      
      let pixbuf = GdkPixbuf.from_file("img/image_tmp.bmp") in
      GdkPixbuf.scale 
        ~dest: pixbufvide
        ~scale_x: (600. /. (float (GdkPixbuf.get_width pixbuf)))
        ~scale_y: (600. /. (float (GdkPixbuf.get_height pixbuf)))
        pixbuf;
      GdkPixbuf.save 
        ~filename:("img/image_apercu.bmp") 
        ~typ:"bmp"
        pixbufvide;
    in

      let binarize () =
	begin
	  sdl_init ();
	  let src = Sdlloader.load_image (!img_path) in
	  let (w,h) = Function.get_dims src in 
	  let binarizedImage =
	    Sdlvideo.create_RGB_surface_format src [] w h in
	  Binarization.binarizationOtsu src binarizedImage;      
	  Sdlvideo.save_BMP binarizedImage "img/image_tmp.bmp";
	end;
	imageapercu ();
	picture_area#set_file "img/image_apercu.bmp";
	copy_ref "img/image_tmp.bmp" in    
      
      (* let filter_median () =
	begin
	  sdl_init ();
	  let filterType = ref "m" in
	  let src = Sdlloader.load_image (!img_path) in  
	  let (w,h) = Function.get_dims src in
	  let filteredImage = Sdlvideo.create_RGB_surface_format src [] w h in
	  if (!filterType = "m") then
	    begin
	      Filters.applyRelaxedFilterMedianGrey src filteredImage;
	    end
	  else if (!filterType = "c") then
	    begin
	      Filters.applyPasseBandeFilter src filteredImage;
	    end
	  else
	    Function.copyImg src filteredImage;
	  Sdlvideo.save_BMP filteredImage "img/image_tmp.bmp";
	end;
	imageapercu ();
	picture_area#set_file "img/image_apercu.bmp";
	copy_ref "img/image_tmp.bmp" in
      *)

      let filter_median () =
	begin
	  sdl_init ();	  
	  let src = Sdlloader.load_image (!img_path) in  
	  let (w,h) = Function.get_dims src in
	  let filteredImage = Sdlvideo.create_RGB_surface_format src [] w h in
	  Filters.applyRelaxedFilterMedianGrey src filteredImage;
	  Sdlvideo.save_BMP filteredImage "img/image_tmp.bmp";
	end;
	imageapercu ();
	picture_area#set_file "img/image_apercu.bmp";
	copy_ref "img/image_tmp.bmp" in
      
      let filter_linear () =
	begin
	  sdl_init ();	  
	  let src = Sdlloader.load_image (!img_path) in  
	  let (w,h) = Function.get_dims src in
	  let filteredImage = Sdlvideo.create_RGB_surface_format src [] w h in
	  Filters.applyPasseBandeFilter src filteredImage;
	  Sdlvideo.save_BMP filteredImage "img/image_tmp.bmp";
	end;
	imageapercu ();
	picture_area#set_file "img/image_apercu.bmp";
	copy_ref "img/image_tmp.bmp" in
      
      let rotation () =
	begin
	  sdl_init ();
	  let angle = ref 0. in
	  let src = Sdlloader.load_image (!img_path) in 
	  let (w,h) = Function.get_dims src in
	  let points = Sdlvideo.create_RGB_surface_format src [] w h in
	  begin
	    angle := Angle.transformToPoints src points;
	  end; 
	  
	  let rotatedImage = Sdlvideo.create_RGB_surface_format src [] w h in
	  Function.toWhite rotatedImage;
	  Rotate.rotateWeighted src rotatedImage !angle;    
	  Sdlvideo.save_BMP rotatedImage "img/image_tmp.bmp";
	end;
	imageapercu ();
	picture_area#set_file "img/image_apercu.bmp";
	copy_ref "img/image_tmp.bmp" in
      
      let detect () = 
	begin
	  sdl_init ();
	  let src = Sdlloader.load_image (!img_path) in
	  Extract.charDetection src;   
	  Sdlvideo.save_BMP src "img/image_tmp.bmp";
	end;
	imageapercu ();
	picture_area#set_file "img/image_apercu.bmp";
	copy_ref "img/image_tmp.bmp" in    
    
      let all_pre () = 
	begin
	  sdl_init ();
	  let angle = ref 0. 
	  and filterType = ref "m" in
	  let src = Sdlloader.load_image (!img_path) in
	  let (w,h) = Function.get_dims src in 
	  
	  let filteredImage = Sdlvideo.create_RGB_surface_format src [] w h in
	  if (!filterType = "m") then
	    begin
	      Filters.applyRelaxedFilterMedianGrey src filteredImage;
	    end
	  else if (!filterType = "c") then
	    begin
	      Filters.applyPasseBandeFilter src filteredImage;
	    end
	  else
	    Function.copyImg src filteredImage;
	  
	  let filteredImageCopy =
	    Sdlvideo.create_RGB_surface_format filteredImage [] w h in
	  Function.copyImg filteredImage filteredImageCopy;
	  
	  let binarizedImage =
	    Sdlvideo.create_RGB_surface_format filteredImage [] w h in
	  Binarization.binarizationOtsu filteredImage binarizedImage;
	  
	  let points = 
	    Sdlvideo.create_RGB_surface_format binarizedImage [] w h in
	  angle := Angle.transformToPoints binarizedImage points;
	  
	  let rotatedImage = 
	   Sdlvideo.create_RGB_surface_format filteredImageCopy [] w h in
	  Function.toWhite rotatedImage;
	  Rotate.rotateWeighted filteredImageCopy rotatedImage !angle;  
	  
	  let pretreatedImage =
	    Sdlvideo.create_RGB_surface_format rotatedImage [] w h in
	  Binarization.binarizationOtsu rotatedImage pretreatedImage;
	  
	  Sdlvideo.save_BMP pretreatedImage "img/image_tmp.bmp";
	end;
	
	imageapercu ();
	picture_area#set_file "img/image_apercu.bmp";
	copy_ref "img/image_tmp.bmp" in

		 
      
    (* -------------------  PRETREATMENT  ---------------------- *)
      
      let pre_processing_frame = GBin.frame
	~label:"Image Processing"
	~height:50
	~packing:box_2D#add() in

      let hbox = GPack.hbox
      ~border_width:0
      ~packing:pre_processing_frame#add() in
      
      let box1 = GPack.vbox
	~border_width:0
	~packing:hbox#add() in
      
      let box2 = GPack.vbox
	~border_width:0
	~packing:hbox#add() in
      
      let bbox1 = GPack.button_box `HORIZONTAL
	~layout:`SPREAD
	~height: 50
	~spacing:10
	~packing:(box1#pack ~expand:false) () in
      
      let filter_button_median = GButton.button 
	~label:"Median filter"
	~packing:bbox1#add () in
      ignore (filter_button_median#connect#clicked ~callback:(filter_median););

      let filter_button_linear = GButton.button 
	~label:"Linear filter"
	~packing:bbox1#add () in
      ignore (filter_button_linear#connect#clicked ~callback:(filter_linear););      
      
      let bbox2 = GPack.button_box `HORIZONTAL
	~layout:`SPREAD
	~height: 50
	~spacing:10
	~packing:(box2#pack ~expand:false) () in
      
      let binarisation_button = GButton.button
	~label:"Binarization" 
	~packing:bbox2#add () in
      ignore(binarisation_button#connect#clicked ~callback:(binarize););
      
      let rotate_button = GButton.button
      ~label:"Rotation" 
      ~packing:bbox2#add () in
      ignore(rotate_button#connect#clicked ~callback:(rotation););
      
      let all_pre_button = GButton.button
	~label:"All"
	~packing:bbox2#add () in
      ignore(all_pre_button#connect#clicked ~callback:(all_pre););
      
      
    (* ---------------------- TEXT RECONISATION ---------------------- *)
      
      let box_3D = GPack.vbox
	~height:125
	~packing:right_box#add() in
      
      let reconisation_frame = GBin.frame
	~label:"Texte recognition"
	~height:70
	~packing:box_3D#add() in
      
      let hbox2 = GPack.hbox
	~border_width:5
	~packing:reconisation_frame#add() in
      
      let box1 = GPack.vbox
	~border_width:0
	~packing:hbox2#add() in
      
      let bbox1 = GPack.button_box `HORIZONTAL
	~layout:`SPREAD
	~height: 50
	~spacing:10
      ~packing:(box1#pack ~expand:false) () in
      
      let detection_button = GButton.button 
	~label:"Detection"
	~packing:bbox1#add () in
      ignore (detection_button#connect#clicked ~callback:(detect););
      
    (*---------------TEXT TREATMENT--------------- *)
      
      let frame_textetraitment = GBin.frame
	~width:50
	~label:"Texte processing"
	~packing:box_3D#add() in
      
      let hboxtraitement = GPack.hbox
	~border_width:5
	~packing:frame_textetraitment#add() in
      
      let boxtraitement = GPack.vbox
	~border_width:0
	~packing:hboxtraitement#add() in
      
      let boxtreat = GPack.button_box `HORIZONTAL
	~layout:`SPREAD
	~height: 50
	~spacing:10
	~packing:(boxtraitement#pack ~expand:false) () in
      
    (* ---------------------- TEXT BOX ----------------------- *)
      
      let box_3D2 = GPack.hbox
	~border_width:0
	~height:300
	~packing:right_box#add() in
      
      let frame_texte = GBin.frame
	~width:300
	~label:"Texte"
	~packing:box_3D2#add() in
      
      let textbox = GText.view
	~wrap_mode:`WORD 
	~packing:frame_texte#add () in  
      textbox#misc#modify_font_by_name "Monospace 10";
      let s = ""
      in textbox#buffer#set_text(s);


      let recognize_text () = 
	let alphabet = Sdlloader.load_image "Alphabet.jpg" in
	let (w,h) = Function.get_dims alphabet in
	let binarizedAlphabet =
	  Sdlvideo.create_RGB_surface_format alphabet [] w h in
	Binarization.binarizationOtsu alphabet binarizedAlphabet;
	Extract.charDetection binarizedAlphabet;
	Network.learn_alphabet network;
	let texte = Network.read_string network in
	let fichierTexte = open_out "texte.txt" in
	output_string fichierTexte texte;
	close_out fichierTexte;
	textbox#buffer#set_text(texte);
      in

      let network_button = GButton.button 
	~label:"Recognize"
	~packing:boxtreat#add () in 
      ignore (network_button#connect#clicked ~callback:(recognize_text););
      
      
      (* -------------------- SAVE BUTTON ---------------------- *)
      
      let savetext () = 
	let och = open_out "Result_by_LMAOCR.txt" in
	output_string och (textbox#buffer#get_text ());
	close_out och  in
      
      let save_button = GButton.button 
	~label:"Save"
	~stock: `SAVE
	~packing:boxtreat#add () in ignore (save_button#connect#clicked
					      ~callback:(savetext));
      ignore(GMisc.image ~stock:`SAVE ~packing:save_button#set_image ());
      
    (* -------------------- OPEN BUTTON ---------------------- *)
      let touch_picture btn () = 
	Gaux.may copy_ref btn#filename;
	let image1 = Sdlloader.load_image (!img_path) in
	Sdlvideo.save_BMP image1 "img/image_tmp.bmp"; 
	imageapercu();   
	picture_area#set_file ("img/image_apercu.bmp");
	step1_clicked := false in 
      
      let open_b = GFile.chooser_button
	~action: `OPEN
	~width:100
	~height:25
	~packing:(menu_box#add) () in
      open_b#set_filter (checkImg ());
      ignore(open_b#connect#selection_changed (touch_picture open_b););
      
      let reset () = 
	picture_area#set_file "init.png";
	copy_ref "init.png";
	step1_clicked:= false;
	Extract.resultList := [];
	textbox#buffer#set_text("");
	
	img_path := "" in
      
      let reset_button = GButton.button 
	~label:"Reset" 
	~stock:`CLEAR
	~packing:menu_box#add () in ignore (reset_button#connect#clicked 
					      ~callback:(reset));
      ignore(GMisc.image ~stock:`CLEAR ~packing:reset_button#set_image ());    
      
      let about_button = GButton.button 
	~label:"About" 
	~stock: `ABOUT
	~packing:menu_box#add ()in ignore (about_button#connect#clicked
	~callback:(about));
      ignore(GMisc.image ~stock:`ABOUT ~packing:about_button#set_image ());     
      
   (*   let close_button = GButton.button 
	~label:"Close" 
	~stock: `CLOSE
	~packing:menu_box#add () in ignore (close_button#connect#clicked
					      ~callback:(GMain.quit));
      ignore(GMisc.image ~stock:`CLOSE ~packing:close_button#set_image ()); *)
      
      window#show();
      
      GMain.Main.main ()
	
  end

