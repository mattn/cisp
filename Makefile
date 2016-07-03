SRCS = \
	cisp.c

OBJS = $(subst .c,.o,$(SRCS))

CFLAGS = -O2 -funroll-loops -Wall -Werror -Werror=unused-result
LIBS = -O2 -funroll-loops
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
	gcc -o cisp -pg cisp.c
	./cisp example/tak.l
	gprof $(TARGET) gmon.out -p | less
