# LMAOCR
 
OCAML=ocamlopt
OCAMLFLAGS= -I +sdl -I +site-lib/sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa
 
tpsdl: main.ml
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o lmaocr t_matrix.ml rotate.ml filters.ml angle.ml binarization.ml main.ml 
 
clean::
	rm -f *~ *.o *.cm? lmaocr
 
# FIN
