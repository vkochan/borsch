TARGET = libtext.a

REGEX_SRC ?= text-regex.c

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

CFLAGS_STD ?= -std=c99 -D_POSIX_C_SOURCE=200809L -D_XOPEN_SOURCE=700 -DNDEBUG
CFLAGS_LIBC = -DHAVE_MEMRCHR=1
LDFLAGS_STD ?= -lc

$(TARGET): $(OBJ)
	@echo "[Link (Static)]"
	@ar rcs $(TARGET) $(OBJ)

.c.o:
	@echo [Compile] $<
	@$(CC) -c $(CFLAGS) $(CFLAGS_STD) $(CFLAGS_LIBC) $(CFLAGS_EXTRA) $< -o $@

clean:
	rm -f $(TARGET) $(OBJ) *~ core tags *.bak Makefile.bak libgeniePi.*
