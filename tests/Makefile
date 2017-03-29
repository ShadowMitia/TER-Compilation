
#SRC_PASS=$(shell find . -name "*pass.c" -printf '%f\n')
#SRC_FAIL=$(shell find . -name "*fail.c" -printf '%f\n')
#SRC=$(shell find -name "*.c" ! -name "*fail.c" ! -name "*pass.c" -printf '%f\n')
SRC_ALL := $(wildcard *.c)
SRC_PASS := $(filter *.pass.c, $(wildcard *.c))
SRC_FAIL := $(filter *.fail.c, $(SRC_ALL))
COMPILER := ../minic
CC := gcc
OBJ_PASS := $(SRC_PASS:%.c = %.s)

all:
	@echo "Compiling test"
#	$(COMPILER) -type-only $(SRC_PASS)
#	$(COMPILER) -type-only $(SRC_FAIL)
#	$(COMPILER) $(SRC)
	echo $(SRC_ALL)
	echo $(SRC_PASS)
	echo $(OBJ_PASS)
	@echo "Done"

-include $(OBJ_PASS)

%.s:%.c
	@echo "Compiling"
	$(COMPILER) $^

%.exe:%.s
	@echo "Compile GCC"
	$(CC) -o $@ $^

#%.pass.s: %.pass.c
#	@echo "Compiling things pass"
#	$(COMPILER) -type-only $<

# %.fail.s: %.fail.c
# 	@echo "Compiling things fail"
# 	$(COMPILER) -type-only $<

#%.s: %.c
#	@echo "Compiling things"
#	$(COMPILER) $<

clean:
	rm -f *.s

# gen_c:
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
