.PHONY: all clean mrproper

EXE = battlechip

all: $(EXE)

$(EXE): *.ml
	ocamlopt -o $(EXE) \
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
	rm -f $(EXE)
