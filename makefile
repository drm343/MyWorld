APP_NAME=myworld

INDENT_NUMBER=4
CASE_INDENT_NUMBER=4

# Compiler and Standard
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S), FreeBSD)
	INDENT=gindent
	INCLUDE=-I /usr/local/include -I $(CURREND)/include
else
	INDENT=indent
	INCLUDE=-I $(CURREND)/include
endif
INDENT_STYLE=-kr
COMPILER=gcc-7
GPP_COMPILER=gpp


# C11 only allow anonymous struct or union without a tag.
# Enable ms-extensions for named struct.
STD=-std=gnu11 -fms-extensions

# Source and Include dir defined.
CURREND=`pwd`
SRC=$(CURREND)/src
CONFIG=$(CURREND)/config
OBJ=$(CURREND)/obj
DOCS=$(CURREND)/docs
FONTS=$(CURREND)/static/fonts
GPP_SRC=$(CURREND)/gpp_src

PACKAGE=$(CURREND)/BUILD
BIN=$(PACKAGE)/bin
USER=$(PACKAGE)/USER


# Compile flags
CFLAGS=-lSDL2 -lSDL2_ttf -L/usr/lib64 -lz -lconfig
LFLAGS=-Werror -L$(OBJ) -lmy_world -lm -luuid

LIB_MY_WORLD=$(OBJ)/libmy_world.a

# Use DEBUG=-DDEBUG will enable debug message.
DEBUG=-DDEBUG
#DEBUG=


CHECK_DIR := $(OBJ) $(USER) $(BIN) $(FONTS)

#$(OBJ)/room_tree.o
AUTO_BUILD_DEP := \
	$(OBJ)/BaseClass.o \
	$(OBJ)/point.o \
	$(OBJ)/rectangle.o \
	$(OBJ)/String.o \
	$(OBJ)/style.o \
	$(OBJ)/status.o \
	$(OBJ)/Status_Pool.o \
	$(OBJ)/character.o \
	$(OBJ)/List.o \
	$(OBJ)/List-Iterator.o \
	$(OBJ)/List-Faction.o \
	$(OBJ)/List-Faction_Group.o \
	$(OBJ)/Character_Pool.o \
	$(OBJ)/character_factory.o \
	$(OBJ)/Morph.o \
	$(OBJ)/Morph-SDL2.o \
	$(OBJ)/Morph-SubWindow.o \
	$(OBJ)/Morph-Message.o \
	$(OBJ)/room.o \
	$(OBJ)/Room_Pool.o \
	$(OBJ)/map_system.o \
	$(OBJ)/game_status.o \
	$(OBJ)/graphic-camera.o \
	$(OBJ)/graphic-message.o


DEP := $(AUTO_BUILD_DEP)


AUTO_BUILD_C_SOURCE := \
	$(SRC)/Character_Pool.c \
	$(SRC)/Room_Pool.c \
	$(SRC)/Status_Pool.c \
	$(SRC)/List-Faction.c \
	$(SRC)/character.c \
	$(SRC)/character_factory.c \
	$(SRC)/game_status.c \
	$(SRC)/graphic-camera.c \
	$(SRC)/graphic-message.c \
	$(SRC)/main.c \
	$(SRC)/map_system.c \
	$(SRC)/status.c \
	$(SRC)/style.c

SOURCE := $(AUTO_BUILD_C_SOURCE)
INCLUDE_DIR=$(CURREND)/include

AUTO_BUILD_C_HEADER := \
	$(INCLUDE_DIR)/Character_Pool.h \
	$(INCLUDE_DIR)/Room_Pool.h \
	$(INCLUDE_DIR)/Status_Pool.h \
	$(INCLUDE_DIR)/status.h \
	$(INCLUDE_DIR)/style.h \
	$(INCLUDE_DIR)/character.h \
	$(INCLUDE_DIR)/character_factory.h \
	$(INCLUDE_DIR)/game_status.h \
	$(INCLUDE_DIR)/graphic-camera.h \
	$(INCLUDE_DIR)/graphic-message.h \
	$(INCLUDE_DIR)/main.h \
	$(INCLUDE_DIR)/map_system.h

HEADER := $(AUTO_BUILD_C_HEADER)


.PHONY: clean doc examples strings source app test indent
app: $(CHECK_DIR) indent $(LIB_MY_WORLD)
	$(COMPILER) $(DEBUG) $(STD) $(INCLUDE) $(SRC)/main.c $(CFLAGS) $(LFLAGS) -o $(BIN)/$(APP_NAME)
	@cp -r $(CONFIG) $(USER)
	@cp -r $(FONTS) $(USER)
	@echo "build app done"

source: $(HEADER) $(SOURCE)

$(AUTO_BUILD_C_SOURCE):
	$(GPP_COMPILER) -o $@ $(GPP_SRC)/$(basename $(notdir $@)).gpp

$(AUTO_BUILD_C_HEADER):
	$(GPP_COMPILER) -o $@ $(GPP_SRC)/$(basename $(notdir $@)).h.gpp


indent:
	find $(CURREND)/include -name '*.h' -exec $(INDENT) $(INDENT_STYLE) -i$(INDENT_NUMBER) -cli$(CASE_INDENT_NUMBER) -nut {} \;
	find $(CURREND)/src -name '*.c' ! -name 'String.c' -exec $(INDENT) $(INDENT_STYLE) -i$(INDENT_NUMBER) -cli$(CASE_INDENT_NUMBER) -nut {} \;
	find $(CURREND)/include -name *~ -exec rm {} \;
	find $(CURREND)/src -name *~ -exec rm {} \;


examples:
	$(COMPILER) $(DEBUG) $(STD) $(INCLUDE) examples/container.c $(CFLAGS) $(LFLAGS) -o $(BIN)/app


$(LIB_MY_WORLD): $(DEP)
	ar cr $(LIB_MY_WORLD) $(DEP)
	ranlib $(LIB_MY_WORLD)


$(AUTO_BUILD_DEP):
	$(COMPILER) $(DEBUG) -o $@ -c $(STD) -lm $(INCLUDE) $(SRC)/$(basename $(notdir $@)).c


$(CHECK_DIR):
	mkdir -p $@

clean:
	@cp $(DOCS)/CNAME .
	rm -rf $(BIN)/* $(OBJ)/*
	@cp CNAME $(DOCS)/CNAME

doc:
	@cp $(DOCS)/CNAME .
	-rm -rf $(DOCS)/*
	@doxygen && cd $(DOCS) && mv html/* . && rm -rf html
	@cp CNAME $(DOCS)/CNAME
	touch $(DOCS)/.nojekyll

test:
	$(BIN)/$(APP_NAME)
