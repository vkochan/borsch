# Customize below to fit your system

PROGNAME = borsch

PREFIX ?= /usr
MANPREFIX = ${PREFIX}/share/man
# specify your systems terminfo directory
# leave empty to install into your home folder
TERMINFO := ${DESTDIR}${PREFIX}/share/terminfo
LIB_PREFIX = ${PREFIX}/lib/${PROGNAME}

INCS = -I.
LIBS = -lc -lutil -lncursesw
CPPFLAGS = -D_POSIX_C_SOURCE=200809L -D_XOPEN_SOURCE=700 -D_XOPEN_SOURCE_EXTENDED
CFLAGS += -std=c99 ${INCS} -DNDEBUG ${CPPFLAGS}

CC ?= cc
