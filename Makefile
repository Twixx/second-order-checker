.PHONY: all clean game_maker
# Prevent make to remove secondary files
.SECONDARY:

GMC_DIR := gmc

CHECKERS_DIR := checkers
CHECKERS := $(wildcard $(CHECKERS_DIR)/*)

# Makefile to run inside the games
MAKEFILE := $(shell realpath $(GMC_DIR)/Makefile.game)
DEFAULT_DIR := $(shell realpath $(GMC_DIR)/game_src)

FILE_NAMES := ast.ml checker.ml unify.ml
GEN_FILES := $(addprefix src/, $(FILE_NAMES))

all: $(addsuffix /main.native, $(CHECKERS))

game_maker:
	$(MAKE) -C $(GMC_DIR)

%/main.native: $(addprefix %/, $(GEN_FILES)) %/default
	$(MAKE) -f $(MAKEFILE) -C $*

%/src:
	mkdir -p $@

%/default:
	ln -sf $(DEFAULT_DIR) $@

$(addprefix %/, $(GEN_FILES)): game_maker %/src
	cd $(GMC_DIR) &&\
	./main.native $(realpath $*/$(notdir $*).gm) $(realpath $*/src)

clean:
	$(MAKE) -C $(GMC_DIR) clean
	$(foreach c, $(CHECKERS), $(MAKE) -f $(MAKEFILE) -C $(c) clean)
	$(foreach c, $(CHECKERS), rm -f $(c)/default; rm -rf $(c)/src)

