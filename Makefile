.PHONY: all clean
# Prevent make to remove secondary files
.SECONDARY:

GMC_DIR := gmc

CHECKERS_DIR := checkers
CHECKERS := $(wildcard $(CHECKERS_DIR)/*)

# Makefile to run inside the games
MAKEFILE := $(realpath $(GMC_DIR)/Makefile.game)
DEFAULT_DIR := $(realpath $(GMC_DIR)/game_src)

FILE_NAMES := ast.ml checker.ml unify.ml
GEN_FILES := $(addprefix src/,$(FILE_NAMES))
SRC_FILES := local_parser.mly
CHECKER_TARGET := main.native

all: $(addsuffix /$(CHECKER_TARGET),$(CHECKERS)) game_maker

game_maker:
	$(MAKE) -C $(GMC_DIR)

%/$(CHECKER_TARGET): $(addprefix %/,$(GEN_FILES) default) $(MAKEFILE)
	$(MAKE) -f $(MAKEFILE) -C $*

%/src:
	mkdir -p $@

%/default:
	ln -sf $(DEFAULT_DIR) $@

REL_DIR != realpath --relative-to=$(GMC_DIR) $(CHECKERS_DIR)
.SECONDEXPANSION:
$(addprefix $(CHECKERS_DIR)/%/,$(GEN_FILES)): game_maker $(addprefix $(CHECKERS_DIR)/$$*/,src $(SRC_FILES) $$*.gm)
	cd $(GMC_DIR) &&\
	./main.native $(addprefix $(REL_DIR)/$*/,$*.gm src)

#Check existance of source files
$(addprefix %/,$(SRC_FILES)) %.gm:
	ls $@

clean:
	$(MAKE) -C $(GMC_DIR) clean
	$(foreach c,$(CHECKERS),$(MAKE) -f $(MAKEFILE) -C $(c) clean && rm -f $(c)/default && rm -rf $(c)/src;)

