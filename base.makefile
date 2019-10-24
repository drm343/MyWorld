COMPILER ?= gcc
STD ?=-std=gnu11 -fms-extensions
DEBUG=-DDEBUG
INCLUDE_DEP += -I ../base

.PHONY: clean pc

%.o: %.c
	$(COMPILER) $(DEBUG) $(INCLUDE_DEP) -o $@ -c $(STD) -lm $(basename $@).c

clean:
	rm *.o

pc: $(PC)
	cp $< $(PC_PATH)
