APP_NAME=rogue

INDENT_NUMBER=4
CASE_INDENT_NUMBER=4

COMPILER=gcc
#COMPILER=clang-5.0-32 -ferror-limit=1

CURREND=`pwd`
PACKAGE=$(CURREND)/../AppDir
BIN=$(CURREND)/bin
OBJ=$(CURREND)/obj
SRC=$(CURREND)/src
STD=-std=gnu11
INCLUDE=-I $(CURREND)/include -I $(CURREND)/intern -I /usr/lib64/gcc/x86_64-slackware-linux/7.2.0/include -I `gnustep-config --variable=GNUSTEP_SYSTEM_HEADERS`
CFLAGS=-lSDL2 -lSDL2_ttf -L/usr/lib64 -lz -lconfig
LFLAGS=-L$(OBJ) -lmy_world
OBJC_FLAGS=-Werror -L `gnustep-config --variable=GNUSTEP_SYSTEM_LIBRARIES` -fconstant-string-class=NSConstantString -lobjc -lgnustep-base

LIB_MY_WORLD=$(OBJ)/libmy_world.a

#DEBUG=-DDEBUG
DEBUG=


CHECK_DIR := $(OBJ) $(BIN) $(CURREND)/static/fonts


AUTO_BUILD_DEP := $(OBJ)/point.o \
	$(OBJ)/point-use_self.o \
	$(OBJ)/rectangle.o \
	$(OBJ)/rectangle-use_self.o \
	$(OBJ)/helper_function-strings.o \
	$(OBJ)/strings-instance.m \
	$(OBJ)/graphic.o \
	$(OBJ)/character.o \
	$(OBJ)/character-status.o \
	$(OBJ)/status_list.o \
	$(OBJ)/character-skill.o \
	$(OBJ)/map_system.o \
	$(OBJ)/graphic-camera.o \
	$(OBJ)/graphic-message.o \
	$(OBJ)/character_pool.o \
	$(OBJ)/character_pool-use_self.o


LIBSTRINGS := $(OBJ)/block.o \
	$(OBJ)/strings.o

DEP := $(LIBSTRINGS) $(AUTO_BUILD_DEP)


AUTO_BUILD_TOOLS := $(BIN)/gen_list \
	$(BIN)/gen_pool

TOOLS := $(AUTO_BUILD_TOOLS)


.PHONY: clean doc examples strings app test indent
app: $(CHECK_DIR) $(TOOLS) $(LIB_MY_WORLD)
	$(COMPILER) $(DEBUG) $(STD) $(INCLUDE) $(SRC)/main.m $(CFLAGS) $(LFLAGS) $(OBJC_FLAGS) -o $(BIN)/$(APP_NAME)
	@echo "build app done"


all: $(CHECK_DIR) $(TOOLS) $(LIB_MY_WORLD) doc
	@echo "build all done"


%(TOOLS): $(AUTO_BUILD_TOOLS)


$(AUTO_BUILD_TOOLS):
	gcc tools/$(basename $(notdir $@)).c -lconfig -o $@
	$@


indent:
	find $(CURREND)/include -name '*.h' -exec indent -kr -i$(INDENT_NUMBER) -cli$(CASE_INDENT_NUMBER) -nut {} \;


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
	$(COMPILER) $(DEBUG) -o $@ -c $(STD) $(INCLUDE) $(OBJC_FLAGS) $(SRC)/$(basename $(notdir $@)).m

$(LIBSTRINGS):
	$(COMPILER) $(DEBUG) -o $@ -c $(STD) $(INCLUDE) $(CFLAGS) $(LFLAGS) intern/$(basename $(notdir $@)).c


$(CHECK_DIR):
	mkdir -p $@

clean:
	rm -rf bin obj

doc:
	-rm -rf docs/*
	@doxygen && cd docs && mv html/* . && rm -rf html
	touch docs/.nojekyll
