CC=/opt/parallax/bin/propeller-elf-gcc
RM = rm -Rf

LDFLAGS =

SRCROOT = src/c/src
INCROOT = src/c/include
TSTROOT = test/c

OBJROOT = build
BINROOT = bin

VPATH=$(SRCROOT):$(INCROOT):$(TSTROOT):$(OBJROOT)

.PHONY: all clean test_clean

all: stst.elf

runtime.o: runtime.c runtime.h
	$(CC) -c -I$(INCROOT) -std=c99  -o $(OBJROOT)/$(@) $(<)

stst.o: stst.s
	$(CC) -c -o $(OBJROOT)/$(@) $(<)

stst.elf: runtime.o stst.o
	$(CC) $(OBJROOT)/stst.o $(OBJROOT)/runtime.o  -o $(BINROOT)/$(@)

run: all
	/opt/parallax/bin/propeller-load -Dreset=dtr -I /opt/parallax/propeller-load bin/stst.elf -r -q -t

clean test_clean:
	$(RM) $(OBJROOT)/*.s
	$(RM) $(OBJROOT)/*.o
	$(RM) $(BINROOT)/*.elf
	$(RM) stst.out

.SILENT: stst.o stst.elf test_clean