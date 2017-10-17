APP_NAME=rogue

PLATFORM=-DLINUX_VERSION_FUNCTION=1
#PLATFORM=-DMAXOS_VERSION_FUNCTION=1
#PLATFORM=-DWINDOS_VERSION_FUNCTION=1

COMPILER=gcc
#COMPILER=clang-5.0-32 -ferror-limit=1

CURREND=`pwd`
PACKAGE=$(CURREND)/../AppDir
BIN=$(CURREND)/bin
OBJ=$(CURREND)/obj
SRC=$(CURREND)/src
INCLUDE=-I $(CURREND)/include -I /usr/lib64/gcc/x86_64-slackware-linux/7.2.0/include -I `gnustep-config --variable=GNUSTEP_SYSTEM_HEADERS`
CFLAGS=-lSDL2 -lSDL2_ttf -L/usr/lib64 -lz -lconfig
LFLAGS=-L$(OBJ) -lmy_world
OBJC_FLAGS=-L `gnustep-config --variable=GNUSTEP_SYSTEM_LIBRARIES` -fconstant-string-class=NSConstantString -lobjc -lgnustep-base

LIB_MY_WORLD=$(OBJ)/libmy_world.a

#DEBUG=-g
DEBUG=


CHECK_DIR := $(OBJ) $(BIN) $(CURREND)/static/fonts


AUTO_BUILD_DEP := $(OBJ)/object_point.o \
	$(OBJ)/string_helper.o \
	$(OBJ)/graphic.o \
	$(OBJ)/character.o \
	$(OBJ)/character-status.o \
	$(OBJ)/status_list.o \
	$(OBJ)/character-skill.o \
	$(OBJ)/map_system.o \
	$(OBJ)/graphic-camera.o \
	$(OBJ)/graphic-message.o \
	$(OBJ)/character-pool.o


DEP := $(AUTO_BUILD_DEP)


.PHONY: clean doc gen_list examples strings app
all: $(CHECK_DIR) gen_list $(LIB_MY_WORLD) doc


app: $(CHECK_DIR) gen_list $(LIB_MY_WORLD)
	$(COMPILER) $(DEBUG) -std=c11 $(INCLUDE) $(SRC)/main.m $(CFLAGS) $(LFLAGS) $(OBJC_FLAGS) -o $(BIN)/$(APP_NAME)


gen_list: 
	gcc tools/gen_list.c -lconfig -o $(BIN)/gen_list
	$(BIN)/gen_list


test:
	$(COMPILER) $(DEBUG) -std=c11 a.c -c -o $(OBJ)/a.o
	ar cr $(OBJ)/liba.a $(OBJ)/a.o
	ranlib $(OBJ)/liba.a
	$(COMPILER) $(DEBUG) -std=c11 -I intern hello.c -L $(OBJ) -la -o $(BIN)/app


strings: $(CHECK_DIR)
	$(COMPILER) $(DEBUG) -std=c11 $(INCLUDE) -I intern intern/block.c $(CFLAGS) $(LFLAGS) -c -o $(OBJ)/block.o
	$(COMPILER) $(DEBUG) -std=c11 $(INCLUDE) -I intern intern/strings.c $(CFLAGS) $(LFLAGS) -c -o $(OBJ)/strings.o
	ar cr $(OBJ)/libstrings.a $(OBJ)/block.o $(OBJ)/strings.o
	ranlib $(OBJ)/libstrings.a
	$(COMPILER) $(DEBUG) -std=c11 $(INCLUDE) -I intern examples/check_strings.c $(CFLAGS) -L $(OBJ) -lstrings -o $(BIN)/app


examples:
	$(COMPILER) $(DEBUG) -std=c11 $(INCLUDE) examples/container.c $(CFLAGS) $(LFLAGS) -o $(BIN)/app


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
	$(COMPILER) $(DEBUG) -o $@ -c -std=c11 $(INCLUDE) $(OBJC_FLAGS) $(SRC)/$(basename $(notdir $@)).m

$(CHECK_DIR):
	mkdir -p $@

clean:
	rm -rf bin obj

doc:
	-rm -rf docs/*
	@doxygen && cd docs && mv html/* . && rm -rf html
