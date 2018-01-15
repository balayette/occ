.PHONY: build clean

build:
	jbuilder build src/compiler.exe
	ln -fs $(shell pwd)/_build/default/src/compiler.exe $(shell pwd)/compiler.exe

clean:
	jbuilder clean
	rm -f ./compiler.exe
