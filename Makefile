#Interpret jazyka IFJ2011
#Autori:
#Michal Risa (xrisam01)

CC=gcc
CFLAGS=-std=c99 -pedantic -Wall -Wextra #-g -DLOG #-DNDEBUG
BIN=test
OBJS=main.o scanner.o parser.o ial.o list.o interpret.o
MAINH=parser.h scanner.h ial.h list.h interpret.h

.PHONY: $(BIN) clean

all: $(BIN)

main.o: main.c $(MAINH)
scanner.o: scanner.c $(MAINH)
parser.o: parser.c $(MAINH)
ial.o: ial.c $(MAINH)
list.o: list.c $(MAINH)
interpret.o: interpret.c $(MAINH)

$(BIN): $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o $(BIN) -lm

clean:
	rm *.o $(BIN)

