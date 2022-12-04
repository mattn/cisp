TARGET = cisp

SRCS = \
	cisp.c \
	parser.c \
	util.c

OBJS = $(subst .c,.o,$(SRCS))

CC=gcc
CFLAGS = -O3 -funroll-loops -Wall -Wextra -Wwrite-strings -Wformat=2 -Werror

ifeq ($(OS),Windows_NT)
BIN := $(TARGET).exe
LIBS = -O3 -funroll-loops -ldl -Wl,--export-all-symbols -Wl,--out-implib,libcisp.a
else
BIN = $(TARGET)
LIBS = -O3 -funroll-loops -ldl -Wl,--export-dynamic
endif

.SUFFIXES: .c .o

all : $(BIN)

$(BIN) : $(OBJS)
	$(CC) -o $@ $(OBJS) $(LIBS)

lib$(TARGET).a : $(OBJS)
	@echo EXPORTS > cisp.def
	@grep -h '^EXPORT' *.h | sed -n 's/^EXPORT .* \(\w\+\)(.*/\t\1/p' >> cisp.def
	@dlltool --dllname cisp.exe --input-def cisp.def --output-lib libcisp.a

.c.o :
	$(CC) -c $(CFLAGS) -I. $< -o $@

clean :
	rm -f *.o $(BIN)

debug : $(OBJS)
	$(CC) -g -o cisp -pg $(OBJS) $(LIBS)

test : $(BIN)
	@./t/run-test.sh

prof1 : $(BIN)
	$(CC) -o $(BIN) -pg cisp.c
	./$(BIN) example/tak.lisp
	gprof $(BIN) gmon.out -p | less

prof2 : $(BIN)
	$(CC) -o $(BIN) -pg cisp.c
	./$(BIN) example/fib.lisp
	gprof $(BIN) gmon.out -p | less

valgrind : $(BIN)
	ls t/*.lisp | grep -v error\. | xargs -n 1 valgrind --leak-check=full ./$(BIN) > valgrind.log 2>&1

cisp.o : cisp.h parser.h util.h
parser.o : parser.h cisp.h
util.o : util.h cisp.h
