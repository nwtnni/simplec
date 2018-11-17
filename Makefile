TARGETS=simplec.exe

.PHONY: all build clean install

all: build link

build:
	dune build

link: $(TARGETS)

%.exe:
	if [ ! -f $@ ]; then ln -s _build/default/bin/$@ . ; fi

install:
	dune install

clean:
	rm -rf _build *.install $(TARGETS)
