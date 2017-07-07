APP_NAME=rogue

PLATFORM=-DLINUX_VERSION_FUNCTION=1
#PLATFORM=-DMAXOS_VERSION_FUNCTION=1
#PLATFORM=-DWINDOS_VERSION_FUNCTION=1

CURREND=`pwd`
PACKAGE=$(CURREND)/../AppDir
BIN=$(CURREND)/bin
OBJ=$(CURREND)/obj
SRC=$(CURREND)/src
INCLUDE=-I$(CURREND)/include
CFLAGS=-lSDL2 -lSDL2_ttf -L/usr/lib64 -lz
LFLAGS=-L$(OBJ) -lmy_world

LIB_MY_WORLD=$(OBJ)/libmy_world.a

#DEBUG=-g
DEBUG=


DEP := $(OBJ)/base_type.o \
  $(OBJ)/string_pool.o \
 	$(OBJ)/graphic.o \
 	$(OBJ)/character-ability.o \
 	$(OBJ)/character-pool.o \
 	$(OBJ)/mpc.o


all: $(LIB_MY_WORLD)
	gcc $(DEBUG) $(PLATFORM) -o $(OBJ)/main.o -c -std=c11 $(INCLUDE) $(SRC)/main.c
	gcc $(DEBUG) $(PLATFORM) $(OBJ)/main.o -s $(CFLAGS) $(LFLAGS) -o $(BIN)/$(APP_NAME)
	@sed -ie "s/APP_NAME=.*/APP_NAME=$(APP_NAME)/" $(CURREND)/app && rm $(CURREND)/appe && echo "sed done"

build: all $(PACKAGE)
	rsync -avh $(CURREND)/static $(PACKAGE)
	rsync -avh $(CURREND)/config $(PACKAGE)
	rsync -avh $(CURREND)/bin $(PACKAGE)

$(PACKAGE):
	mkdir -p $(PACKAGE)

$(LIB_MY_WORLD): $(DEP)
	ar cr $(LIB_MY_WORLD) $(DEP)
	ranlib $(LIB_MY_WORLD)

$(DEP):
	gcc $(DEBUG) -o $@ -c -std=c11 $(INCLUDE) $(SRC)/$(basename $(notdir $@)).c

clean:
	rm bin/*
	rm obj/*
