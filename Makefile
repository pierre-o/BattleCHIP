.PHONY: all clean mrproper

all: battlechip

battlechip: *.ml
	ocamlopt -o battlechip \
		-I +sdl bigarray.cmxa sdl.cmxa sdlmixer.cmxa \
		font.ml \
		util.ml \
		audio.ml \
		input.ml \
		memory.ml \
		time.ml \
		video.ml \
		cpu.ml \
		main.ml

clean:
	rm -f *.cmi *.cmx *.o

mrproper: clean
	rm -f battlechip
