
SRC_ALL := $(wildcard *.c)
EXE := $(SRC_ALL:.c=.exe)
INLINE_EXE := $(SRC_ALL:.c=.inline.exe)
COMPILER_FLAGS =

CHECKLIST  = copy_string.c  d02.c fact_rec.c itoa.c main2.c mandelbrot_d.c rec.c test01.c  test03.c d01.c fact_imp.c  gcd.c main1.c  mandelbrot.c mult.c test02.c

COMPILER := ../minic
CC := gcc

# Pour garder les .s
.SECONDARY:

all:
	$(MAKE) test
	$(MAKE) test-inline

test-inline: COMPILER_FLAGS += -inline
test-inline: $(INLINE_EXE)

.PHONY: build check clean test-inline

test: $(EXE)

%.s:%.c
	@echo "Compiling" $^
	$(COMPILER) $(COMPILER_FLAGS) $^

%.exe:%.s
	@echo "Generating" $@
	$(CC) -o $@ $^


%.inline.exe:%.s
	@echo "Generating" $@
	$(CC) -o $@ $^

#check:
#	@$(foreach p, $(wildcard *.exe), \
#	echo "Testing on" $p; \
#	./$p > $p.tmp; \
#	diff -s $p.tmp $(basename $(basename $p)).out > /dev/null; \
#	echo diff -s $p.tmp $(basename $(basename $p)).out > /dev/null; \
#	$(if $(filter "1", diff -s $p.tmp $(basename $(basename $p)).out ), echo "PASSED", echo "FAILED"); \
#	)


clean:
	rm -f *.s
	rm -f *.exe
	rm -f *.inline.exe
