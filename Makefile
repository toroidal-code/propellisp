CC=/opt/parallax/bin/propeller-elf-gcc

all: runtime

runtime: stst.o
	$(CC) -I . -L . -I /opt/parallax/Workspace/Learn/Simple\ Libraries/Utility/libsimpletools -L /opt/parallax/Workspace/Learn/Simple\ Libraries/Utility/libsimpletools/cmm/ -o build/run.elf -std=c99 src/c/runtime.c build/stst.o -ltiny -lsimpletools
stst.o: 
	$(CC) -c build/stst.s -o build/stst.o

clean:
	rm -f stst.o run.elf

run: runtime
	/opt/parallax/bin/propeller-load -Dreset=dtr -I /opt/parallax/propeller-load/ -b QUICKSTART -p /dev/cu.usbserial-A800HF2M build/run.elf -r -t