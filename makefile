APP_NAME=myworld

INDENT_NUMBER=4
CASE_INDENT_NUMBER=4

# Compiler and Standard
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S), FreeBSD)
	INDENT=gindent
	INCLUDE=-I /usr/local/include -I $(CURREND)/include -I $(CURREND)/submodules/intern -I$(CURREND)/submodules/MyWorldStep/include -I$(CURREND)/submodules/CoreFoundation/CoreFoundation/include
else
	INDENT=indent
	INCLUDE=-I $(CURREND)/include -I $(CURREND)/submodules/intern -I$(CURREND)/submodules/MyWorldStep/include -I$(CURREND)/submodules/CoreFoundation/CoreFoundation/include
endif
INDENT_STYLE=-kr
COMPILER=gcc7
#COMPILER=clang50

# C11 only allow anonymous struct or union without a tag.
# Enable ms-extensions for named struct.
STD=-std=c11 -fms-extensions

# Source and Include dir defined.
CURREND=`pwd`
PACKAGE=$(CURREND)/../AppDir
BIN=$(CURREND)/bin
OBJ=$(CURREND)/obj
SRC=$(CURREND)/src

# Compile flags
ifeq ($(COMPILER), clang50)
	CFLAGS=-Wl,-lSDL2 -Wl,-lSDL2_ttf -Wl,-L/usr/lib64 -Wl,-lz -Wl,-lconfig
else
	CFLAGS=-lSDL2 -lSDL2_ttf -L/usr/lib64 -lz -lconfig
endif
LFLAGS=-Werror -L$(OBJ) -lm -lmy_world -L$(CURREND)/submodules/MyWorldStep/Build/tmp -lMWStep ./submodules/CoreFoundation/Build/Release/Products/amd64/libCoreFoundation.a -luuid -lobjc

LIB_MY_WORLD=$(OBJ)/libmy_world.a

# Use DEBUG=-DDEBUG will enable debug message.
DEBUG=-DDEBUG
#DEBUG=


CHECK_DIR := $(OBJ) $(BIN) $(CURREND)/static/fonts


AUTO_BUILD_DEP := $(OBJ)/point.o \
	$(OBJ)/two_point.o \
	$(OBJ)/rectangle.o \
	$(OBJ)/rectangle-use_self.o \
	$(OBJ)/helper_function-strings.o \
	$(OBJ)/strings-instance.o \
	$(OBJ)/history_array.o \
	$(OBJ)/style.o \
	$(OBJ)/status.o \
	$(OBJ)/status_pool.o \
	$(OBJ)/character.o \
	$(OBJ)/character_list.o \
	$(OBJ)/character_pool.o \
	$(OBJ)/character_factory.o \
	$(OBJ)/room.o \
	$(OBJ)/room_pool.o \
	$(OBJ)/room_tree.o \
	$(OBJ)/map_system.o \
	$(OBJ)/game_status.o \
	$(OBJ)/graphic-camera.o \
	$(OBJ)/graphic-message.o


CF := submodules/CoreFoundation/Build/Release/Products/amd64

LIBSTRINGS := $(OBJ)/block.o \
	$(OBJ)/strings.o

DEP := $(LIBSTRINGS) $(AUTO_BUILD_DEP)


AUTO_BUILD_TOOLS := $(BIN)/gen_list \
	$(BIN)/gen_pool \
	$(BIN)/gen_cycle-base_type \
	$(BIN)/gen_normal_tree

TOOLS := $(AUTO_BUILD_TOOLS)


.PHONY: clean doc examples strings CoreFoundation MyWorldStep app test indent
app: $(CHECK_DIR) indent $(TOOLS) $(LIB_MY_WORLD)
	$(COMPILER) $(DEBUG) $(STD) $(INCLUDE) $(SRC)/app/main.m $(CFLAGS) $(LFLAGS) -o $(BIN)/$(APP_NAME)
	@echo "build app done"


all: $(CHECK_DIR) $(TOOLS) $(LIB_MY_WORLD) doc
	@echo "build all done"


%(TOOLS): $(AUTO_BUILD_TOOLS)


$(AUTO_BUILD_TOOLS):
	$(COMPILER) $(INCLUDE) tools/$(basename $(notdir $@)).c -L/usr/local/lib -lconfig -o $@
	$@


indent:
	find $(CURREND)/include -name '*.h' -exec $(INDENT) $(INDENT_STYLE) -i$(INDENT_NUMBER) -cli$(CASE_INDENT_NUMBER) -nut {} \;
	find $(CURREND)/src -name '*.c' -exec $(INDENT) $(INDENT_STYLE) -i$(INDENT_NUMBER) -cli$(CASE_INDENT_NUMBER) -nut {} \;
	git checkout $(SRC)/helper_function-strings.c
	find $(CURREND)/include -name *~ -exec rm {} \;
	find $(CURREND)/src -name *~ -exec rm {} \;

submodules/intern/config.h:
	cd submodules/intern && cmake -G 'Unix Makefiles' -Wno-dev


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

$(LIBSTRINGS): submodules/intern/config.h $(CHECK_DIR)
	$(COMPILER) $(DEBUG) -o $@ -c $(STD) $(INCLUDE) submodules/intern/$(basename $(notdir $@)).c


CoreFoundation:
	cd submodules/CoreFoundation && gmake

MyWorldStep:
	cd submodules/MyWorldStep && gmake clean && gmake


$(CHECK_DIR):
	mkdir -p $@

clean:
	rm -rf bin obj docs/*

doc:
	-rm -rf docs/*
	@doxygen && cd docs && mv html/* . && rm -rf html
	touch docs/.nojekyll
