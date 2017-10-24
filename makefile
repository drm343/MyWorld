APP_NAME=rogue

INDENT_NUMBER=4
CASE_INDENT_NUMBER=4

# Compiler and Standard
COMPILER=gcc

 # C11 only allow anonymous struct or union without a tag.
 # Enable ms-extensions for named struct.
C11=-std=gnu11 -fms-extensions
STD=$(C11)

 # Source and Include dir defined.
CURREND=`pwd`
PACKAGE=$(CURREND)/../AppDir
BIN=$(CURREND)/bin
OBJ=$(CURREND)/obj
SRC=$(CURREND)/src
INCLUDE=-I $(CURREND)/include -I $(CURREND)/intern

# Compile flags
CFLAGS=-lSDL2 -lSDL2_ttf -L/usr/lib64 -lz -lconfig
LFLAGS=-Werror -L$(OBJ) -lmy_world

LIB_MY_WORLD=$(OBJ)/libmy_world.a

# Use DEBUG=-DDEBUG will enable debug message.
#DEBUG=-DDEBUG
DEBUG=


CHECK_DIR := $(OBJ) $(BIN) $(CURREND)/static/fonts


AUTO_BUILD_DEP := $(OBJ)/point.o \
	$(OBJ)/point-use_self.o \
	$(OBJ)/two_point.o \
	$(OBJ)/rectangle.o \
	$(OBJ)/rectangle-use_self.o \
	$(OBJ)/helper_function-strings.o \
	$(OBJ)/strings-instance.o \
	$(OBJ)/history_array.o \
	$(OBJ)/graphic.o \
	$(OBJ)/character.o \
	$(OBJ)/character-status.o \
	$(OBJ)/status_list.o \
	$(OBJ)/status_pool.o \
	$(OBJ)/map_system.o \
	$(OBJ)/graphic-camera.o \
	$(OBJ)/graphic-message.o \
	$(OBJ)/character_pool.o \
	$(OBJ)/character_pool-use_self.o


LIBSTRINGS := $(OBJ)/block.o \
	$(OBJ)/strings.o

DEP := $(LIBSTRINGS) $(AUTO_BUILD_DEP)


AUTO_BUILD_TOOLS := $(BIN)/gen_list \
	$(BIN)/gen_pool \
	$(BIN)/gen_cycle-base_type

TOOLS := $(AUTO_BUILD_TOOLS)


.PHONY: clean doc examples strings app test indent
app: $(CHECK_DIR) $(TOOLS) $(LIB_MY_WORLD) indent
	$(COMPILER) $(DEBUG) $(STD) $(INCLUDE) $(SRC)/main.c $(CFLAGS) $(LFLAGS) -o $(BIN)/$(APP_NAME)
	@echo "build app done"


all: $(CHECK_DIR) $(TOOLS) $(LIB_MY_WORLD) doc
	@echo "build all done"


%(TOOLS): $(AUTO_BUILD_TOOLS)


$(AUTO_BUILD_TOOLS):
	gcc tools/$(basename $(notdir $@)).c -lconfig -o $@
	$@


indent:
	find $(CURREND)/include -name '*.h' -exec indent -kr -i$(INDENT_NUMBER) -cli$(CASE_INDENT_NUMBER) -nut {} \;
	find $(CURREND)/src -name '*.c' -exec indent -kr -i$(INDENT_NUMBER) -cli$(CASE_INDENT_NUMBER) -nut {} \;
	git checkout $(SRC)/helper_function-strings.c
	find $(CURREND)/include -name *~ -exec rm {} \;
	find $(CURREND)/src -name *~ -exec rm {} \;


strings: $(CHECK_DIR)
	$(COMPILER) $(DEBUG) $(STD) $(INCLUDE) intern/block.c $(CFLAGS) $(LFLAGS) -c -o $(OBJ)/block.o
	$(COMPILER) $(DEBUG) $(STD) $(INCLUDE) intern/strings.c $(CFLAGS) $(LFLAGS) -c -o $(OBJ)/strings.o
	ar cr $(OBJ)/libstrings.a $(OBJ)/block.o $(OBJ)/strings.o
	ranlib $(OBJ)/libstrings.a
	$(COMPILER) $(DEBUG) $(STD) $(INCLUDE) examples/check_strings.c $(CFLAGS) -L $(OBJ) -lstrings -o $(BIN)/app


examples:
	$(COMPILER) $(DEBUG) $(STD) $(INCLUDE) examples/container.c $(CFLAGS) $(LFLAGS) -o $(BIN)/app


build: all $(PACKAGE)
	rsync -avh $(CURREND)/static $(PACKAGE)
	rsync -avh $(CURREND)/config $(PACKAGE)
	rsync -avh $(CURREND)/bin $(PACKAGE)

$(PACKAGE):
	mkdir -p $(PACKAGE)

$(LIB_MY_WORLD): $(DEP)
	ar cr $(LIB_MY_WORLD) $(DEP)
	ranlib $(LIB_MY_WORLD)

$(AUTO_BUILD_DEP):
	$(COMPILER) $(DEBUG) -o $@ -c $(STD) $(INCLUDE) $(SRC)/$(basename $(notdir $@)).c

$(LIBSTRINGS):
	$(COMPILER) $(DEBUG) -o $@ -c $(STD) $(INCLUDE) $(CFLAGS) $(LFLAGS) intern/$(basename $(notdir $@)).c


$(CHECK_DIR):
	mkdir -p $@

clean:
	rm -rf bin obj docs/*

doc:
	-rm -rf docs/*
	@doxygen && cd docs && mv html/* . && rm -rf html
	touch docs/.nojekyll
