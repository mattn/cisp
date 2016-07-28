SRCS = \
	cisp.c \
	parser.c \
	util.c

OBJS = $(subst .c,.o,$(SRCS))

CC=gcc
CFLAGS = -g -O2 -funroll-loops -Wall -Wextra -Wwrite-strings -Wformat=2 -Werror
LIBS = -g -O2 -funroll-loops
TARGET = cisp
ifeq ($(OS),Windows_NT)
TARGET := $(TARGET).exe
endif

.SUFFIXES: .c .o

all : $(TARGET)

$(TARGET) : $(OBJS)
	$(CC) -o $@ $(OBJS) $(LIBS)

.c.o :
	$(CC) -c $(CFLAGS) -I. $< -o $@

clean :
	rm -f *.o $(TARGET)

debug :
	$(CC) -g -o cisp -pg cisp.c

test : $(TARGET)
	@./t/run-test.sh

prof1 : $(TARGET)
	$(CC) -o $(TARGET) -pg cisp.c
	./$(TARGET) example/tak.lisp
	gprof $(TARGET) gmon.out -p | less

prof2 : $(TARGET)
	$(CC) -o $(TARGET) -pg cisp.c
	./$(TARGET) example/fib.lisp
	gprof $(TARGET) gmon.out -p | less

valgrind : $(TARGET)
	ls t/*.lisp | grep -v error\. | xargs -n 1 valgrind --leak-check=full ./$(TARGET) > valgrind.log 2>&1

cisp.o : cisp.h parser.h util.h
parser.o : parser.h cisp.h
util.o : util.h cisp.h
