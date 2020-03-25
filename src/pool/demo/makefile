GDB_ENABLE=-g -O0
MEMORY=../pool-memory.c
INCLUDE=-I ../
OBJECT=string.o \
	   user.o \
	   int.o

.PHONY: clean time

all: a

a: $(OBJECT) main.c
	gcc $(GDB_ENABLE) -o a main.c $(OBJECT)

int.o: $(MEMORY) int.h int.c
	gcc $(GDB_ENABLE) -o int.o -c int.c $(INCLUDE)

user.o: $(MEMORY) user.h user.c
	gcc $(GDB_ENABLE) -o user.o -c user.c $(INCLUDE)

string.o: $(MEMORY) string.h string.c
	gcc $(GDB_ENABLE) -o string.o -c string.c $(INCLUDE)

mem:
	valgrind ./a -v

time:
	sh ./time

gdb:
	vim +"packadd termdebug" +"Termdebug ./a" debug

clean:
	rm ./*.o a
