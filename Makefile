CC=/opt/parallax/bin/propeller-elf-gcc
RM = rm -Rf

LDFLAGS =

SRCROOT = src/c/src
INCROOT = src/c/include
TSTROOT = test/c

OBJROOT = build
BINROOT = bin

SIMPLETOOLS = /opt/parallax/Workspace/Learn/Simple\ Libraries/Utility/libsimpletools
CMM = /opt/parallax/Workspace/Learn/Simple\ Libraries/Utility/libsimpletools/cmm/

VPATH=$(SRCROOT):$(INCROOT):$(TSTROOT):$(OBJROOT)

all: stst.elf

runtime.o: runtime.c runtime.h
	$(CC) -c -I$(SIMPLETOOLS)  -I$(INCROOT) -std=c99  -o $(OBJROOT)/$(@) $(<)

stst.o: stst.s
	$(CC) -c -o $(OBJROOT)/$(@) $(<)

stst.elf: runtime.o stst.o
	$(CC) -L$(CMM) $(OBJROOT)/stst.o $(OBJROOT)/runtime.o  -o $(BINROOT)/$(@) -ltiny -lsimpletools

run:
	/opt/parallax/bin/propeller-load -Dreset=dtr -I /opt/parallax/propeller-load/ -b QUICKSTART -p /dev/cu.usbserial-A800HF2M bin/stst.elf -r -q -t

clean test_clean:
	$(RM) $(OBJROOT)/*.s
	$(RM) $(OBJROOT)/*.o
	$(RM) $(BINROOT)/*.elf
	$(RM) stst.out
