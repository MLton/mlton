mlton = mlton

all: test

.PHONY: test
test: world
	save-world @MLton load-world world --

world: save-world
	save-world

save-world: save-world.sml
	$(mlton) save-world.sml

.PHONY: clean
clean:
	../../../bin/clean
