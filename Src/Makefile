# LMAOCR
 
OCAML=ocamlopt
OCAMLFLAGS= -I +sdl -I +site-lib/sdl -I lablgtk2 -I +site-lib/lablgtk2
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa lablgtk.cmxa 
 
tpsdl: main.ml
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o lmaocr function.ml t_matrix.ml rotate.ml filters.ml angle.ml binarization.ml extract.ml network.ml interface.ml main.ml 
 
clean::
	rm -f *~ *.o *.core *.cm? lmaocr
 
# FIN

