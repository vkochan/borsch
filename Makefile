include config.mk

SCHEME_LIST = scheme chez chez-scheme
$(foreach scm,$(SCHEME_LIST),$(if $(SCHEME),,$(eval SCHEME := $(shell which $(scm)))))
$(if $(SCHEME),,$(error No scheme program found))

PKG_CONFIG = pkg-config

X11_LIB = /usr/X11R6/lib
X11_LIBS = -L$(X11_LIB) -lrt -lX11 -lutil -lXft \
       `$(PKG_CONFIG) --libs fontconfig` \
       `$(PKG_CONFIG) --libs freetype2`

SRCS = xstr.c \
      process.c \
      main.c \
      vt.c \
      array.c \
      window.c \
      view.c \
      buffer.c \
      keymap.c \
      event.c \
      timer.c \
      syntax.c \
      style.c

SCH_VERSION := $(shell echo "(scheme-version)" | $(SCHEME) -q | sed -e 's|"||g' | cut -d ' ' -f4)
SCH_MACHINE := $(shell echo "(machine-type)"   | $(SCHEME) -q)
SCH_PREFIX ?= /usr
SCH_PATH = $(SCH_PREFIX)/lib/csv$(SCH_VERSION)/$(SCH_MACHINE)
LIBS += -lpthread -luuid -ldl -lm
BIN += ${PROGNAME}-eval
SRCS += scheme.c

LDFLAGS += -L ./text -L ./ui
LIBS += -ltext -lui -ltree-sitter $(X11_LIBS)

SRCS += syntax/c/parser.c \
       syntax/devicetree/parser.c \
       syntax/diff/parser.c \
       syntax/scheme/parser.c \
       syntax/make/parser.c \
       syntax/org/scanner.c \
       syntax/org/parser.c

CFLAGS += -I$(SCH_PATH) \
   -DPROGNAME='"${PROGNAME}"' \
   -DLIB_PATH='"'"${DESTDIR}${LIB_PREFIX}"'"'

OBJS += ${SRCS:.c=.o}

BIN += ${PROGNAME}
MANUALS = ${PROGNAME}.1

VERSION = $(shell git describe --always --dirty 2>/dev/null || echo "0.15-git")
CFLAGS += -DVERSION=\"${VERSION}\"

ifeq ($(DEBUG),1)
CFLAGS += -UNDEBUG -O0 -g -ggdb -Wall -Wextra -Wno-unused-parameter
endif

.PHONY: libtext libui ${PROGNAME}.boot

all: ${PROGNAME} ${PROGNAME}.boot

${PROGNAME}.boot:
	cat mkboot.ss | $(SCHEME) -q

${PROGNAME}: ${OBJS} libui libtext
	${CC} ${CFLAGS} ${LDFLAGS} $(SCH_PATH)/kernel.o ${OBJS} ${LIBS} -o $@

%.o: %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

libtext:
	$(MAKE) -C text/

libui:
	$(MAKE) -C ui/

man:
	@for m in ${MANUALS}; do \
		echo "Generating $$m"; \
		sed -e "s/VERSION/${VERSION}/" "$$m" | mandoc -W warning -T utf8 -T xhtml -O man=%N.%S.html -O style=mandoc.css 1> "$$m.html" || true; \
	done

debug: clean
	@$(MAKE) CFLAGS='${DEBUG_CFLAGS}'

clean:
	@echo cleaning
	@$(MAKE) -C text/ clean
	@$(MAKE) -C ui/ clean
	@rm -f ${PROGNAME}.boot
	@rm -f ${PROGNAME} 
	@rm -f ${OBJS}

dist: clean
	@echo creating dist tarball
	@git archive --prefix=${PROGNAME}-${VERSION}/ -o ${PROGNAME}-${VERSION}.tar.gz HEAD

install: all
	@mkdir -p ${DESTDIR}${PREFIX}/bin
	@for b in ${BIN}; do \
		echo "installing ${DESTDIR}${PREFIX}/bin/$$b"; \
		cp -f "$$b" "${DESTDIR}${PREFIX}/bin" && \
		chmod 755 "${DESTDIR}${PREFIX}/bin/$$b"; \
	done
	@cp -f ${PROGNAME}.boot ${SCH_PATH}
	@echo installing manual page to ${DESTDIR}${MANPREFIX}/man1
	@mkdir -p ${DESTDIR}${MANPREFIX}/man1
	@for m in ${MANUALS}; do \
		sed -e "s/VERSION/${VERSION}/" < "$$m" >  "${DESTDIR}${MANPREFIX}/man1/$$m" && \
		chmod 644 "${DESTDIR}${MANPREFIX}/man1/$$m"; \
	done
	@echo installing terminfo description
	@TERMINFO=${TERMINFO} tic -s ${PROGNAME}.info

uninstall:
	@for b in ${BIN}; do \
		echo "removing ${DESTDIR}${PREFIX}/bin/$$b"; \
		rm -f "${DESTDIR}${PREFIX}/bin/$$b"; \
	done
	@rm -f ${SCH_PATH}/${PROGNAME}.boot
	@echo removing manual page from ${DESTDIR}${MANPREFIX}/man1
	@rm -f ${DESTDIR}${MANPREFIX}/man1/${PROGNAME}.1

.PHONY: all clean dist install uninstall debug
