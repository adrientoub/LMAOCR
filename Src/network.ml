(* le type neuron qui définit chaque neurone de notre réseau *)
type neuron = 
{
  mutable c: char; (* le caractère *)
  w : float array; (* un tableau de float qui contient des valeurs d'activations *)
}

type t =
{
  alphabet: string;
  size: int;
  nb_input: int;
  input: float array;
  neurons: neuron array;
}

(* la fonction sigmoide qui permet de gérer l'activation du réseau *)
let sigmoid x = 
  1. /. (1. +. exp (-.x))

(* la fonction qui permet de créer un réseau *)
let create input s = 
  let size = String.length s in
  let network = 
    { 
      alphabet = s;
      size = size;
      nb_input = input;
      input = Array.make input 0.;
      neurons = Array.init size 
	(fun i -> {
	    c = s.[i];
	    w = Array.make input 0.;
	  })
    }
  in 
  network

(* Constructeur par défaut du réseau de neurones *)
let create_default () = 
  create 256 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"

(* Sauvegarde le réseau de neurones network dans un fichier file *)
let save network file = 
  let f = open_out_bin file in (* Ouvre un fichier en écriture binaire *)
  Marshal.to_channel f network []; (* Marshal permet d'avoir une sérialization automatique *)
  close_out f

(* Charge le réseau de neurones à partir d'un fichier file *)
let load file = 
  let f = open_in_bin file in (* ouvre en lecture binaire un fichier *)
  let network = (Marshal.from_channel f : t) in
  close_in f;
  network

let sum network neuron = 
  let s = ref 0. in
  for i = 1 to network.nb_input - 1 do
    s := !s +. network.neurons.(neuron).w.(i) *. network.input.(i)
  done;
  !s -. 0.5

let output network neuron =
  sigmoid (sum network neuron)

(* ne fonctionne que sur des images en niveau de gris et renvoie un flotant entre 0 et 1 *)
let get_color image x y = 
  let (c,_,_) = image.(x).(y) in
  (float c) /. 255.

let get_dims_mat image =
  (Array.length image, Array.length image.(0))

let watch network image = 
  let (w,h) = get_dims_mat image in
  for x = 0 to w - 1 do
    for y = 0 to h - 1 do
      network.input.(y * w + x) <- get_color image x  y
    done
  done

let learn network c rate = 
  for i = 0 to network.size - 1 do
    let n = network.neurons.(i) in
    let yk = if n.c = c then 1. else 0. and
	sk = output network i in
    for j = 0 to network.nb_input - 1 do
      n.w.(j) <- n.w.(j) +. rate *. (yk -. sk) *. network.input.(j)
    done
  done

(* copie une image *)
let copy image = 
  let (w, h) = get_dims_mat image in
  let nimage = Array.create_matrix w h 0 in
  for x = 0 to w - 1 do
    for y = 0 to h - 1 do
      let color = image.(x).(y) in
      nimage.(x).(y) <- color
    done
  done;
  nimage

(* Apprend l'alphabet au network *)
let learn_alphabet network =
  let rate = 0.1 in
  let rec rl = function
    | (0, _) | (_, []) -> ()
    | (i, h::t) ->
      watch network h;
      learn network (network.alphabet.[network.size-i]) rate;
      rl (i-1, t)
  in
  let lc = !Extract.alphabetList in
  for i = 0 to 200 do
    rl (network.size, lc)
  done;
  save network "network.ocr"

let read_char network = 
  let c = ref 'x' in
  let max = ref 0. in
  for i = 0 to network.size - 1 do
    let nc = network.neurons.(i).c in
    let out = output network i in
    if out > !max then
      begin
	max := out;
	c := nc
      end
  done;
  !c

(*let read_string network image = 
  let rec rr b = function
    | [] -> b
    | h::t -> 
      watch network c;
      rr (b ^ (Char.escaped read_char network)) t in
  rr "" !resultList*)
