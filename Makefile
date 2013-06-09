CC=/opt/parallax/bin/propeller-elf-gcc
RM = rm -Rf

LDFLAGS =

CFLAGS = -mfcache -fno-exceptions -m32bit-doubles -std=c99

SRCROOT = src/c/src
INCROOT = src/c/include
TSTROOT = test/c

OBJROOT = build
BINROOT = bin

VPATH=$(SRCROOT):$(INCROOT):$(TSTROOT):$(OBJROOT)

.PHONY: all clean test_clean

all: stst.elf

runtime.o: runtime.c runtime.h
	$(CC) $(CFLAGS) -c -I$(INCROOT) -o $(OBJROOT)/$(@) $(<) -Os $(MEMORY)

stst.o: stst.s
	$(CC) -c -o $(OBJROOT)/$(@) $(<) $(MEMORY)

stst.elf: runtime.o stst.o
	$(CC) $(OBJROOT)/stst.o $(OBJROOT)/runtime.o  -o $(BINROOT)/$(@) $(MEMORY)

run:
	/opt/parallax/bin/propeller-load -Dreset=dtr -I /opt/parallax/propeller-load bin/stst.elf -r -q -t

hub: MEMORY=-mlmm
hub: all
hubrun: hub run

cog: MEMORY=-mcog
cog: all
cogrun: cog run 

clean test_clean:
	$(RM) $(OBJROOT)/*.s
	$(RM) $(OBJROOT)/*.o
	$(RM) $(BINROOT)/*.elf
	$(RM) stst.out

.SILENT: stst.o stst.elf test_clean