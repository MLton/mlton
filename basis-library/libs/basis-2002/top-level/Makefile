GEN = generate-overloads

overloads.sml: $(GEN).sml
	mlton $(GEN).sml
	$(GEN) >overloads.sml

.PHONY: clean
clean:
	../../../../bin/clean
