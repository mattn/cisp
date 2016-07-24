SRCS = \
	cisp.c

OBJS = $(subst .c,.o,$(SRCS))

CFLAGS = -g -O2 -funroll-loops -Wall -Werror -Werror=unused-result -pedantic
LIBS = -g -O2 -funroll-loops
TARGET = cisp
ifeq ($(OS),Windows_NT)
TARGET := $(TARGET).exe
endif

.SUFFIXES: .c .o

all : $(TARGET)

$(TARGET) : $(OBJS)
	gcc -o $@ $(OBJS) $(LIBS)

.c.o :
	gcc -c $(CFLAGS) -I. $< -o $@

clean :
	rm -f *.o $(TARGET)

debug :
	gcc -g -o cisp -pg cisp.c

test : $(TARGET)
	@./t/run-test.sh

prof1 : $(TARGET)
	gcc -o $(TARGET) -pg cisp.c
	./$(TARGET) example/tak.lisp
	gprof $(TARGET) gmon.out -p | less

prof2 : $(TARGET)
	gcc -o $(TARGET) -pg cisp.c
	./$(TARGET) example/fib.lisp
	gprof $(TARGET) gmon.out -p | less

valgrind : $(TARGET)
	ls t/*.lisp | grep -v error\. | xargs -n 1 valgrind --leak-check=full ./$(TARGET) > valgrind.log 2>&1

