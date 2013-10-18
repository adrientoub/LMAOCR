(* fichier permettant la binairisation de l'image *)

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
  let (w, h) = Rotate.get_dims src in 
  for i = 0 to w-1 do
    for j = 0 to h-1 do 
      let color = Sdlvideo.get_pixel_color src i j in 
      Sdlvideo.put_pixel_color dst i j (color2grey color)
    done 
  done

(* passe une image en noir et blanc
Le résultat n'est pas très joli *)
let image2bnw src dst =
  let (w, h) = Rotate.get_dims src in 
  for i = 0 to w do
    for j = 0 to h do
      let color = Sdlvideo.get_pixel_color src i j in 
      let lvl = level color in 
      let newColor = if lvl < 0.5 then 0 else 255 in 
      Sdlvideo.put_pixel_color dst i j (newColor, newColor, newColor)
    done 
  done

(* binarization using average level of img's pixel as treshold *)
let binarization src dst = 
  let (w,h) = Rotate.get_dims src in
  let greyImage = Sdlvideo.create_RGB_surface_format src [] w h in
  image2grey src greyImage;
  let filteredImage = Sdlvideo.create_RGB_surface_format src [] w h in
  let v = ref 0 in
  Filters.applyRelaxedFilterMedianGrey greyImage filteredImage;
  for x=0 to w-1 do
    for y=0 to h-1 do
      let (color,_,_) = Sdlvideo.get_pixel_color filteredImage x y in 
      v := !v + color;
    done;
  done;
  let seuil = int_of_float (((float_of_int !v)) /. (float_of_int (w*h))) in
  for j=0 to w-1 do
    for k=0 to h-1 do
      let (color,_,_) = Sdlvideo.get_pixel_color filteredImage j k in
      if color <= seuil then
	Sdlvideo.put_pixel_color dst j k (0,0,0)
      else 
	Sdlvideo.put_pixel_color dst j k (255,255,255)
    done
  done

(* ------------------ Otsu stuff ------------------ *)

(* histo *) 
let getHistogramme img = 
  begin
    let (w,h) = Function.get_dims img
    and histo = Array.init 256 (fun x -> 0) in
    for i = 0 to w-1 do
      for j = 0 to h-1 do
	let greyColor = int_of_float (Function.level (Sdlvideo.get_pixel_color img i j) *. 255.) in
	histo.(greyColor) <- histo.(greyColor) + 1;
      done;
    done;
    histo
  end

(* The histogramme will contains the probability of occurrence of level in the image *)
let equalizationHisto histo n = 
  for i = 0 to 256 do
    histo.(i) <- histo.(i) / n
  done

(* Calcule the within class variance for a given pixel in a equalized histogramme *)
let variance histo tresh numPixel =
  begin
    (* Background shit *)
    let bNumPixel = ref 0.
    and bWeight = ref  0.
    and bMean = ref 0.
    and bVariance = ref 0. in  
    for i = 0 to tresh do 
      bWeight := !bWeight +. float_of_int histo.(i);
      bMean := !bMean +. float_of_int (i * histo.(i));
      bNumPixel := !bNumPixel +. 1.;
    done;
    bWeight := !bWeight /. float_of_int numPixel;
    bMean := !bMean /. !bNumPixel;
    for i = 0 to tresh do
      bVariance := !bVariance +. ((((float_of_int i) -. !bMean)**2.) *. (float_of_int histo.(i)));
    done;
    bVariance := !bVariance /. !bNumPixel;
    (* Foreground shit *)
    let fNumPixel = ref 0.
    and fWeight = ref  0.
    and fMean = ref 0.
    and fVariance = ref 0. in  
    for i = tresh+1 to 256 do 
      fWeight := !fWeight +. float_of_int histo.(i);
      fMean := !fMean +. float_of_int (i * histo.(i));
      fNumPixel := !fNumPixel +. 1.;
    done;
    fWeight := !fWeight /. float_of_int numPixel;
    fMean := !fMean /. !fNumPixel;
    for i = tresh+1 to 256 do
      fVariance := !fVariance +. ((((float_of_int i) -. !fMean)**2.) *. (float_of_int histo.(i)));
    done;
    fVariance := !fVariance /. !fNumPixel;
    let intraClasseVariance  = !bWeight *. !bVariance +. !fWeight *. !fVariance in
    intraClasseVariance;
    end	

let minVariance histo numPixel = 
  begin
    let minVar = ref 0. 
    and treshold = ref 0 in
    for i = 0 to 256 do
      let var = variance histo i numPixel in
      if var < !minVar then
	minVar := var;
        treshold := i;      
    done;
    !treshold;
    end

(* binarization using Otsu's method, src must be greyscaled *)
let binarizationOtsu src dst = 
  let (w,h) = Function.get_dims src 
  and histo = getHistogramme src in
  let numPixel = w * h in
  equalizationHisto histo numPixel;
  let treshold = minVariance histo numPixel in
  for i = 0 to w - 1 do
    for j = 0 to h - 1 do
      let (color,_,_) = Sdlvideo.get_pixel_color src i j in
      if color <= treshold then
	Sdlvideo.put_pixel_color dst i j (0,0,0)
      else
	Sdlvideo.put_pixel_color dst i j (255,255,255)
    done
  done
