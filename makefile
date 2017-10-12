APP_NAME=rogue

PLATFORM=-DLINUX_VERSION_FUNCTION=1
#PLATFORM=-DMAXOS_VERSION_FUNCTION=1
#PLATFORM=-DWINDOS_VERSION_FUNCTION=1

COMPILER=gcc

CURREND=`pwd`
PACKAGE=$(CURREND)/../AppDir
BIN=$(CURREND)/bin
OBJ=$(CURREND)/obj
SRC=$(CURREND)/src
INCLUDE=-I $(CURREND)/include -I /usr/lib64/gcc/x86_64-slackware-linux/7.2.0/include -I `gnustep-config --variable=GNUSTEP_SYSTEM_HEADERS`
CFLAGS=-lSDL2 -lSDL2_ttf -L/usr/lib64 -lz -lconfig
LFLAGS=-L$(OBJ) -lmy_world -L `gnustep-config --variable=GNUSTEP_SYSTEM_LIBRARIES` -fconstant-string-class=NSConstantString -lobjc -lgnustep-base

LIB_MY_WORLD=$(OBJ)/libmy_world.a

#DEBUG=-g
DEBUG=


CHECK_DIR := $(OBJ) $(BIN) $(CURREND)/static/fonts


AUTO_BUILD_DEP := $(OBJ)/base_type.o \
	$(OBJ)/string_helper.o \
	$(OBJ)/string_pool.o \
	$(OBJ)/graphic.o \
	$(OBJ)/character.o \
	$(OBJ)/character-status.o \
	$(OBJ)/character-skill.o \
	$(OBJ)/character-npc.o \
	$(OBJ)/graphic-camera.o \
	$(OBJ)/graphic-message.o \
	$(OBJ)/character-pool.o


DEP := $(AUTO_BUILD_DEP)


.PHONY: clean html app
all: $(CHECK_DIR) $(LIB_MY_WORLD)
	$(COMPILER) $(DEBUG) -std=c11 $(INCLUDE) $(SRC)/main.m $(CFLAGS) $(LFLAGS) -o $(BIN)/$(APP_NAME)


app: $(LIB_MY_WORLD)


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
	$(COMPILER) $(DEBUG) -o $@ -c -std=c11 $(INCLUDE) $(SRC)/$(basename $(notdir $@)).m


$(CHECK_DIR):
	mkdir -p $@

clean:
	rm -rf bin obj

html:
	cd $(CURREND) && htags -g -F && htags -Ffnsa
