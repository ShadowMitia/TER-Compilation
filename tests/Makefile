
#SRC_PASS=$(shell find . -name "*pass.c" -printf '%f\n')
#SRC_FAIL=$(shell find . -name "*fail.c" -printf '%f\n')
#SRC=$(shell find -name "*.c" ! -name "*fail.c" ! -name "*pass.c" -printf '%f\n')
SRC_ALL := $(wildcard *.c)
SRC_EXE := $(SRC_ALL:.c=.exe)

COMPILER := ../minic
CC := gcc

.SECONDARY:

all: build
	echo $(SRC_ALL)

.PHONY: build

build: $(SRC_EXE)

%.s:%.c
	@echo "Compiling" $^
	@$(COMPILER) $^

%.exe:%.s
	@echo "Generating" $@
	$(CC) -o $@ $^

clean:
	rm -f *.s
	rm -f *.exe

# 	@echo -e "\n \n"
# 	@echo -e "\n C \n"
# 	@echo -e "\n \n"
#	 @echo -e "\n[ Tests qui ne passent pas! ]\n"
#	 @for i in $(shell find tests/ -name *fail.c); do \
#	 	echo -n "Testing on " \"$$i\"; \
#	 	./$(EXEC) -type-only $$i; \
#	 	if [[ ("$$?" -ne 0 && "$$i" = *".fail."* ) \
#	 	|| ( "$$?" -eq 0 && "$$i" = *".pass."* ) ]]; \
#	 	then echo "	[PASSED]"; else echo "	[FAILED]"; fi \
#	 done
#	 @echo -e "\n[ Tests qui passent ]\n"
#	 @for i in $(shell find tests/ -name *pass.c); do \
#	 	echo -n "Testing on " \"$$i\"; \
#	 	./$(EXEC) -type-only $$i; \
#	 	if [[ ("$$?" -ne 0 && "$$i" = *".fail."* ) \
#	 	|| ( "$$?" -eq 0 && "$$i" = *".pass."* ) ]]; \
#	 	then echo "	[PASSED]"; else echo "	[FAILED]"; fi \
#	 done;
#	 @echo -e "\n[ Tests de génération de codes ]\n";
