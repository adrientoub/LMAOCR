(* fichier permettant la binairisation de l'image *)

(* transforme une couleur en gris *)
let color2grey (r, g, b) = 
  let grey = int_of_float (Function.level (r, g, b) *. 255.) in
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
      let lvl = Function.level color in 
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
    and treshold2 = ref 0
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
		  treshold2 := !i;
                end;
              if !currentVariance = !maxVariance then
		treshold2 := !i;
            end;
        end;
          i := !i +1;
    done;
    ((!treshold + !treshold2) / 2)
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
  for i = 0 to w-1 do
    for j = 0 to h-1 do
      let (color,_,_) = Sdlvideo.get_pixel_color src i j in
      if color < treshold then
	Sdlvideo.put_pixel_color dst i j (0,0,0)
      else
	Sdlvideo.put_pixel_color dst i j (255,255,255)
    done
  done

  (* -------------- Opening ----------- *)

let toBit img =
  begin
  let (w,h) = Function.get_dims img in
  let imgBit = Array.create_matrix w h 0 in
  for i = 0 to w - 1 do
    for j = 0 to h - 1 do
      let (color,_,_) = Sdlvideo.get_pixel_color img i j in
      if color = 0 then
	imgBit.(i).(j) <- 1
    done;
  done;
  imgBit;
  end

let bitToImg imgBit dst =
  begin
    let (w,h) = (Array.length imgBit, Array.length imgBit.(0)) in
    for i = 0 to w - 1 do
      for j = 0 to h - 1 do
	if imgBit.(i).(j) = 1 then
	  Sdlvideo.put_pixel_color dst i j (0,0,0)
	else
	  Sdlvideo.put_pixel_color dst i j (255,255,255)
      done
    done
end

(* Dilatation *)
let dilTab = Array.create_matrix 3 3 1

(* Only 3*3 for now *)
let dilatation imgBit dilTab =
  let w = Array.length imgBit and h = Array.length imgBit.(0)
 (* and wTab = Array.length dilTab and hTab = Array.length dilTab.(0) *)
  and statut = ref 0 in
  for i = 0 to w - 1 do
    for j = 0 to h - 1 do
      for i2 = i - 1 to i + 1 do
	for j2 = j - 1 to j + 1 do
	  if (i2 >= 0) && (j2 >= 0) && (i2 < 1) && (j2 < 1) then
	    statut := !statut lor (imgBit.(i2).(j2) land dilTab.(i2 + 1).(j2 + 1));
        done;
      done;
      imgBit.(i).(j) <- !statut;
    done
  done


(* Erosion *)
let eroTab = Array.create_matrix 3 3 0

(* Only 3*3 for now *)
let erosion imgBit eroTab =
  let w = Array.length imgBit and h = Array.length imgBit.(0)
  (* and wTab = Array.length eroTab and hTab = Array.length eroTab.(0) *)
  and statut = ref 1 in
  for i = 0 to w - 1 do
    for j = 0 to h - 1 do
      for i2 = i - 1 to i + 1 do
	for j2 = j - 1 to j + 1 do
	   if (i2 >= 0) && (j2 >= 0) && (i2 < 1) && (j2 < 1) then
	     statut := !statut land ((lnot imgBit.(i2).(j2)) land
					(lnot eroTab.(i2 + 1).(j2 + 1)));
        done;
      done;
      imgBit.(i).(j) <- lnot !statut;
    done
  done

(* Opening *)
let opening src dst =
  begin
    Printf.printf "loading";
    let imgBit = toBit src in
    Printf.printf "loaded";
    erosion imgBit eroTab;
    Printf.printf "erosion";
    dilatation imgBit dilTab;
    Printf.printf "dila";
    bitToImg imgBit dst;
  end
