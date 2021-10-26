%.out: %.o
	gcc -g -m64 -o $@ main.c $<

%.o: %.s
	nasm -f elf64 -o $@ $<

%.s: %.boa
	stack build
	stack run -- prog.boa --dump-asm > $@
.PHONY: clean
clean:
	rm -f *.o *.s 
