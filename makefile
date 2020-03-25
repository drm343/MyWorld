APP_NAME=myworld

INDENT_NUMBER=4
CASE_INDENT_NUMBER=4

PKG_CONFIG_PATH := $(PKG_CONFIG_PATH):./pkgconfig

# Compiler and Standard
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S), FreeBSD)
	INDENT=gindent
	INCLUDE=-I /usr/local/include
else
	INDENT=indent
endif
INDENT_STYLE=-kr
COMPILER=gcc


# C11 only allow anonymous struct or union without a tag.
# Enable ms-extensions for named struct.
STD=-std=gnu11 -fms-extensions

# Source and Include dir defined.
CURREND=`pwd`
SRC=$(CURREND)/src
CONFIG=$(CURREND)/config
DOCS=$(CURREND)/docs
FONTS=$(CURREND)/static/fonts
PKGCONFIG=$(CURREND)/pkgconfig

PACKAGE=$(CURREND)/BUILD
BIN=$(PACKAGE)/bin
USER=$(PACKAGE)/USER


# Compile flags
CFLAGS=-lSDL2 -lSDL2_ttf -L/usr/lib64 -lz -lconfig
LFLAGS=-Werror -L$(PACKAGE) -lmy_world -lm -luuid

LIB_MY_WORLD=$(PACKAGE)/libmy_world.a

# Use DEBUG=-DDEBUG will enable debug message.
DEBUG=-DDEBUG
#DEBUG=


CHECK_DIR := $(USER) $(BIN) $(FONTS) $(PKGCONFIG)

DEP := base morph room character list graphic application
INCLUDE += $(foreach i, $(DEP), `pkg-config $i --cflags`)


.PHONY: clean doc app test indent pc
app: $(CHECK_DIR) $(LIB_MY_WORLD)
	$(COMPILER) $(DEBUG) $(STD) $(INCLUDE) $(SRC)/main.c $(CFLAGS) $(LFLAGS) -o $(BIN)/$(APP_NAME)
	@cp -r $(CONFIG) $(USER)
	@cp -r $(FONTS) $(USER)
	@echo "build app done"


pc:
	@$(foreach i, $(DEP), make -C $(SRC)/$(i) PC_PATH="$(CURREND)/pkgconfig" pc;)


indent:
	find $(CURREND)/src -name '*.h' -exec $(INDENT) $(INDENT_STYLE) -i$(INDENT_NUMBER) -cli$(CASE_INDENT_NUMBER) -nut {} \;
	find $(CURREND)/src -name '*.c' ! -name 'String.c' -exec $(INDENT) $(INDENT_STYLE) -i$(INDENT_NUMBER) -cli$(CASE_INDENT_NUMBER) -nut {} \;
	find $(CURREND)/src -name *~ -exec rm {} \;


$(LIB_MY_WORLD):
	$(foreach i, $(DEP), make -C $(SRC)/$(i);)
	ar cr $(LIB_MY_WORLD) $(foreach i,$(DEP), `pkg-config --libs $i`)
	ranlib $(LIB_MY_WORLD)


$(CHECK_DIR):
	mkdir -p $@


clean:
	@cp $(DOCS)/CNAME .
	rm -rf $(BIN)/*
	@cp CNAME $(DOCS)/CNAME
	$(foreach i, $(DEP), make -C $(SRC)/$(i) clean;)

doc:
	@cp $(DOCS)/CNAME .
	-rm -rf $(DOCS)/*
	@doxygen && cd $(DOCS) && mv html/* . && rm -rf html
	@cp CNAME $(DOCS)/CNAME
	touch $(DOCS)/.nojekyll

test:
	$(BIN)/$(APP_NAME)
