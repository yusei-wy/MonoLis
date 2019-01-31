CFLAGS=-Wall -std=c11
SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

mlis: $(OBJS)

$(OBJS): mlis.h

test: mlis
	./mlis --test

clean:
	rm -f mlic *.o *~
