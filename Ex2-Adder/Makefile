%.out: %.o
	gcc -g -m64 -o $@ main.c $<

%.o: %.s
	nasm -f elf64 -o $@ $<

%.s:
	stack build
	stack exec Ex2-Adder-exe > $@ < example.add
clean:
	rm *.o *.s 
