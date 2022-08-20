TARGET = libtext.a

REGEX_SRC ?= text-regex-tre.c

SRC = array.c \
	buffer.c \
	libutf.c \
	text.c \
	text-common.c \
	text-io.c \
	text-iterator.c \
	text-motions.c \
	text-objects.c \
	text-util.c \
	$(REGEX_SRC)

OBJ = $(SRC:.c=.o)

CFLAGS = -DCONFIG_TRE -Itre/include
CFLAGS_STD ?= -std=c99 -D_POSIX_C_SOURCE=200809L -D_XOPEN_SOURCE=700 -DNDEBUG
CFLAGS_LIBC = -DHAVE_MEMRCHR=1
LDFLAGS_STD ?= -lc

$(TARGET): $(OBJ) libtre
	@echo "[Link (Static)]"
	@ar crs $(TARGET) $(OBJ) $(wildcard tre/lib/*.o)

libtre:
	$(MAKE) -C tre/

.c.o:
	@echo [Compile] $<
	@$(CC) -c $(CFLAGS) $(CFLAGS_STD) $(CFLAGS_LIBC) $(CFLAGS_EXTRA) $< -o $@

clean:
	$(MAKE) -C tre/ clean
	rm -f $(TARGET) $(OBJ) *~ core tags *.bak Makefile.bak libgeniePi.*
