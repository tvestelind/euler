CC=ghc
LDFLAGS=
CCFLAGS=-hidir build
BIN := problem3

SRC := $(wildcard src/*.hs)
OBJ := $(patsubst src/%.hs, build/%.o, $(SRC))
EXEC := $(patsubst %, build/%, $(BIN))

all: build $(EXEC)

$(EXEC): $(OBJ)
	$(CC) $(LDFLAGS) -o $@ $^

build/%.o: src/%.hs
	$(CC) $(CCFLAGS) -c -o $@ $<

build:
	mkdir build

run: all
	$(EXEC)

clean:
	rm -rf build
