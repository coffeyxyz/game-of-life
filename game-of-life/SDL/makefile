# Copyright (C) 2020 Robert Coffey
# Released under the GPLv2 license

CC=clang
CFLAGS=-I./deps/rtb/src -Weverything
LIBS=`sdl2-config --cflags --libs`

SRCS=$(wildcard src/**.c)
HEDS=$(wildcard src/**.h)
OBJS=$(SRCS:%.c=%.o)
PROG=game-of-life

all: $(PROG)
	$(CC) $(CFLAGS) $(OBJS) -o $(PROG) $(LIBS)
$(PROG): $(OBJS)
$(OBJS): $(HEDS)
$(OBJS): $(SRCS)

.PHONY: clean
clean:
	rm -rf $(OBJS)

.PHONY: uninstall
uninstall:
	rm -rf $(OBJS) $(PROG)
