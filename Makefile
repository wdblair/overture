all: demo

control.dats: control.plu
	./preludec -node main -print_ats ./$^ > $@

.phony: demo

.phony: preludec

demo: preludec control.dats
	patsopt --constraint-export -tc -d control.dats | patsolve

preludec:
	cd prelude-src && ./configure && make
	cp prelude-src/main.native ./preludec
