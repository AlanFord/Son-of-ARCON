## The name of your project (without the .c or .for)
TARGET = sonofARCON

# Object files: will find all .f/.for/.c/.h files in current directory
#  and in LIBDIR.  If you have any other (sub-)directories with code,
#  you can add them in to SOURCES below in the wildcard statement.
SOURCES=$(wildcard *.for)
OBJECTS=$(SOURCES:.for=.o)

FFLAGS = -Os -g -Wall
CC = gfortran

## Explicit pattern rules:
##  To make .o files from .c files 
%.o: %.for $(HEADERS) Makefile
	 $(CC) $(FFLAGS) -c -o $@ $<;

$(TARGET): $(OBJECTS)
	$(CC) $(LDFLAGS) $(TARGET_ARCH) $^ $(LDLIBS) -o $@

## These targets don't have files named after them
.PHONY: all clean

all: $(TARGET)

clean:
	rm -f $(TARGET) *.elf *.hex *.obj *.o *.d *.eep *.lst *.lss *.sym *.map *~ *.eeprom
