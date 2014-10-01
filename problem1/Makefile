CC=ghc

OBJECTS=main.o

problem1: $(OBJECTS)
	$(CC) -o $@ $(OBJECTS)

%.o: %.hs
	$(CC) -c -O $<

clean:
	rm *.o *.hi problem1
