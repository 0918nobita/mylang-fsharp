build: hello

hello: hello.s
	gcc -o hello hello.s

hello.s: hello.bc
	llc hello.bc

hello.bc: hello.ll
	llvm-as hello.ll

.PHONY: clean
clean:
	rm -f hello hello.bc hello.s
