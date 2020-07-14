.PHONY: build part1

FLAGS = +m --html --dest out ++style static/manual-racket.css ++style static/manual-fonts.css --style static/manual-style.css
SCRIBBLE = raco scribble

build: part1

part1:
	mkdir -p out
	$(SCRIBBLE) $(FLAGS) part1.rkt
