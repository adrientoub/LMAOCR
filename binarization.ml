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
	let greyColor = truncate (Function.level (Sdlvideo.get_pixel_color img i j) *. 255.) in
	histo.(greyColor) <- histo.(greyColor) + 1;
      done;
    done;
    histo
  end

(* Calcule the within class variance for a given pixel in a equalized histogramme *)
let otsuThreshold histo numPixel =
  begin    
    let bSum = ref 0.
    and bWeight = ref 0. 
    and bMean = ref 0.
    and fWeight = ref  0.
    and fMean = ref 0.
    and treshold = ref 0
    and maxVariance = ref 0.
    and currentVariance = ref 0.
    and sum = ref 0. in
    for i = 1 to 255 do
      sum := !sum +. float_of_int (i * histo.(i))
    done;
    let i = ref 0 in
    while !i <= 255 do
      bWeight := !bWeight +. float_of_int histo.(!i);
      if (!bWeight <> 0.) then
	begin
	  fWeight := numPixel -. !bWeight;
	  if (!fWeight = 0.) then
	    i := 256
	  else 
	    begin
	      bSum := !bSum +. float_of_int !i *. float_of_int histo.(!i);
	      bMean := !bSum /. !bWeight;
	      fMean := (!sum -. !bSum) /. !fWeight;
	      currentVariance := !bWeight *. !fWeight *. ((!bMean -. !fMean)**2.);
	      if !currentVariance > !maxVariance then
		begin
		  maxVariance := !currentVariance;
		  treshold := !i;
		end;
	    end; 
	end;
	  i := !i +1;
    done;
    !treshold;
    end	

(* binarization using Otsu's method, src must be greyscaled *)
let binarizationOtsu src dst = 
  let (w,h) = Function.get_dims src 
  and histo = getHistogramme src in
  let nbVal = ref 0 in
  for i=0 to 255 do
    nbVal := histo.(i) + !nbVal;
  done;
  let numPixel = w * h in
  let treshold = otsuThreshold histo (float_of_int numPixel) in
  Printf.printf "%i" treshold;
  for i = 0 to w-1 do
    for j = 0 to h-1 do
      let (color,_,_) = Sdlvideo.get_pixel_color src i j in
      if color < treshold then
	Sdlvideo.put_pixel_color dst i j (0,0,0)
      else
	Sdlvideo.put_pixel_color dst i j (255,255,255)
    done
  done
