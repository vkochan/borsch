TARGET = libui.a

PKG_CONFIG = pkg-config

SRC = core.c term.c x.c

OBJ = $(SRC:.c=.o)

X11_INC = /usr/X11R6/include
X11_INCS = -I$(X11_INC) \
       `$(PKG_CONFIG) --cflags fontconfig` \
       `$(PKG_CONFIG) --cflags freetype2`

CFLAGS = -I.. -DCONFIG_MOUSE $(X11_INCS)
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
