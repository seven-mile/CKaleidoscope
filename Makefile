CC = g++

LLVM_CONF = llvm-config
LLVM_MODULES = all

CXXFLAGS = `$(LLVM_CONF) --cxxflags`
LDFLAGS = `$(LLVM_CONF) --ldflags`
LIBS = `$(LLVM_CONF) --libs $(LLVM_MODULES)`

all: clean main.o
	$(CC) main.o $(LDFLAGS) $(LIBS) -o main

main.o:
	find -name '*.cpp' -print0 | xargs -0 $(CC) -o main.o -c --std=c++17 -g -O3

clean:
	rm main.o -f
