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
  for i = 0 to w do
    for j = 0 to h do 
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
