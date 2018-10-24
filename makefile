APP_NAME=myworld

INDENT_NUMBER=4
CASE_INDENT_NUMBER=4

# Compiler and Standard
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S), FreeBSD)
	INDENT=gindent
	INCLUDE=-I /usr/local/include -I $(CURREND)/include -I$(CURREND)/submodules/mulklib
else
	INDENT=indent
	INCLUDE=-I $(CURREND)/include -I$(CURREND)/submodules/mulklib
endif
INDENT_STYLE=-kr
COMPILER=gcc-7


# C11 only allow anonymous struct or union without a tag.
# Enable ms-extensions for named struct.
STD=-std=gnu11 -fms-extensions

# Source and Include dir defined.
CURREND=`pwd`
PACKAGE=$(CURREND)/../AppDir
BIN=$(CURREND)/bin
OBJ=$(CURREND)/obj
SRC=$(CURREND)/src

# Compile flags
CFLAGS=-lSDL2 -lSDL2_ttf -L/usr/lib64 -lz -lconfig
LFLAGS=-Werror -L$(OBJ) -lmy_world ./submodules/mulklib/libmulklib.a -lm

LIB_MY_WORLD=$(OBJ)/libmy_world.a

# Use DEBUG=-DDEBUG will enable debug message.
DEBUG=-DDEBUG
#DEBUG=


CHECK_DIR := $(OBJ) $(BIN) $(CURREND)/static/fonts

AUTO_BUILD_DEP := $(OBJ)/point.o \
	$(OBJ)/rectangle.o \
	$(OBJ)/String.o \
	$(OBJ)/style.o \
	$(OBJ)/status.o \
	$(OBJ)/status_pool.o \
	$(OBJ)/character.o \
	$(OBJ)/character_list.o \
	$(OBJ)/character_pool.o \
	$(OBJ)/character_factory.o \
	$(OBJ)/Morph.o \
	$(OBJ)/Morph-SDL2.o \
	$(OBJ)/Morph-Message.o \
	$(OBJ)/room.o \
	$(OBJ)/room_pool.o \
	$(OBJ)/room_tree.o \
	$(OBJ)/map_system.o \
	$(OBJ)/game_status.o \
	$(OBJ)/graphic-camera.o \
	$(OBJ)/graphic-message.o


DEP := $(AUTO_BUILD_DEP)


AUTO_BUILD_TOOLS := $(BIN)/gen_list \
	$(BIN)/gen_pool \
	$(BIN)/gen_normal_tree

TOOLS := $(AUTO_BUILD_TOOLS)


.PHONY: clean doc examples strings app test indent
app: $(CHECK_DIR) indent $(TOOLS) $(LIB_MY_WORLD)
	$(COMPILER) $(DEBUG) $(STD) $(INCLUDE) $(SRC)/app/main.c $(CFLAGS) $(LFLAGS) -o $(BIN)/$(APP_NAME)
	@echo "build app done"


all: $(CHECK_DIR) $(TOOLS) $(LIB_MY_WORLD) doc
	@echo "build all done"


%(TOOLS): $(AUTO_BUILD_TOOLS)


$(AUTO_BUILD_TOOLS):
	$(COMPILER) $(INCLUDE) tools/$(basename $(notdir $@)).c -L/usr/local/lib -lconfig -o $@
	$@


indent:
	find $(CURREND)/include -name '*.h' -exec $(INDENT) $(INDENT_STYLE) -i$(INDENT_NUMBER) -cli$(CASE_INDENT_NUMBER) -nut {} \;
	find $(CURREND)/src -name '*.c' ! -name 'String.c' -exec $(INDENT) $(INDENT_STYLE) -i$(INDENT_NUMBER) -cli$(CASE_INDENT_NUMBER) -nut {} \;
	find $(CURREND)/include -name *~ -exec rm {} \;
	find $(CURREND)/src -name *~ -exec rm {} \;


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
	$(COMPILER) $(DEBUG) -o $@ -c $(STD) -lm $(INCLUDE) $(SRC)/$(basename $(notdir $@)).c


$(CHECK_DIR):
	mkdir -p $@

clean:
	rm -rf bin obj docs/*

doc:
	-rm -rf docs/*
	@doxygen && cd docs && mv html/* . && rm -rf html
	touch docs/.nojekyll
