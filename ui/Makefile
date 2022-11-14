TARGET = libui.a

SRC = core.c term.c

OBJ = $(SRC:.c=.o)

CFLAGS = -I.. -DCONFIG_MOUSE
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
