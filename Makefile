SRCS = \
	cisp.c

OBJS = $(subst .c,.o,$(SRCS))

CFLAGS = -g -O2 -funroll-loops -Wall -Werror -Werror=unused-result
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

test : $(TARGET)
	@./t/run-test.sh

debug :
	gcc -g -o cisp -pg cisp.c

prof :
	gcc -o $(TARGET) -pg cisp.c
	./$(TARGET) example/tak.lisp
	gprof $(TARGET) gmon.out -p | less
