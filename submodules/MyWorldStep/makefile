TEST_DIR=Test
OBJ_DIR=Build/tmp
BIN_DIR=Build/release

CC=gcc7 -std=c11


.PHONY: test CHECK_DIRS_DEP CHECK_DEPS clean


all: CHECK_DIRS_DEP CHECK_DEPS


TESTS := $(BIN_DIR)/test-string $(BIN_DIR)/test-string-array
test: $(TESTS)
$(TESTS):
	$(CC) -I include -I ../CoreFoundation/CoreFoundation/include -luuid -lobjc $(TEST_DIR)/$(notdir $@).m -L $(OBJ_DIR) -lMWStep ../CoreFoundation/Build/Release/Products/amd64/libCoreFoundation.a -o $(BIN_DIR)/$(notdir $@)
$(BIN_DIR)/test-string-array:
	$(CC) -I include -I ../mulklib -I ../CoreFoundation/CoreFoundation/include -luuid -lobjc $(TEST_DIR)/$(notdir $@).m -L $(OBJ_DIR) -lMWStep ../CoreFoundation/Build/Release/Products/amd64/libCoreFoundation.a ../mulklib/libmulklib.a -o $(BIN_DIR)/$(notdir $@)


DEPS := $(OBJ_DIR)/MWObject.o $(OBJ_DIR)/MWString.o $(OBJ_DIR)/MWMutableString.o
CHECK_DEPS: $(DEPS)
	ar cr $(OBJ_DIR)/libMWStep.a $(DEPS)
	ranlib $(OBJ_DIR)/libMWStep.a
$(DEPS):
	$(CC) -c -I include -I ../CoreFoundation/CoreFoundation/include -lobjc Source/$(basename $(notdir $@)).m -o $@


CHECK_DIRS_DEP: $(OBJ_DIR) $(BIN_DIR) $(TEST_DIR)
CHECK_DIRS := $(OBJ_DIR) $(BIN_DIR) $(TEST_DIR)
$(CHECK_DIRS):
	mkdir -p $(OBJ_DIR) $(BIN_DIR) $(TEST_DIR)

clean:
	rm -r $(OBJ_DIR)/* $(BIN_DIR)/*
