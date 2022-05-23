include config.mk

SRC = main.c vt.c array.c view.c buffer.c keymap.c event.c timer.c syntax.c

SCH_VERSION := $(shell echo "(scheme-version)" | scheme -q | sed -e 's|"||g' | cut -d ' ' -f4)
SCH_MACHINE := $(shell echo "(machine-type)" | scheme -q)
SCH_PREFIX ?= /usr
SCH_PATH = $(SCH_PREFIX)/lib/csv$(SCH_VERSION)/$(SCH_MACHINE)
LIBS += -lpthread -luuid -ldl -lm
OBJS += $(SCH_PATH)/kernel.o
BIN += ${PROGNAME}-eval
SRC += scheme.c

SCH_SCRIPTS = main.ss

LDFLAGS += -L ./text -L ./ui
LIBS += -ltext -lui -ltree-sitter

SRC += parser/c/parser.c

CFLAGS += -I$(SCH_PATH) \
   -DPROGNAME='"${PROGNAME}"' \
   -DLIB_PATH='"'"${DESTDIR}${LIB_PREFIX}"'"'

define install_scheme
	@echo installing scheme scripts
	@mkdir -p ${DESTDIR}${LIB_PREFIX}
	@for s in $$(find scheme/ -type f | sed -e 's|scheme/||'); do \
		echo "installing ${DESTDIR}${LIB_PREFIX}/$$s"; \
		install -D -m 0644 "scheme/$$s" "${DESTDIR}${LIB_PREFIX}/$$s"; \
	done
endef

define uninstall_scheme
	@for s in $$(find scheme/ -type f | sed -e 's|scheme/||'); do \
		echo "removing ${DESTDIR}${LIB_PREFIX}/$$s"; \
		rm -rf "${DESTDIR}${LIB_PREFIX}/$$s"; \
	done
endef

BIN += ${PROGNAME}
MANUALS = ${PROGNAME}.1

VERSION = $(shell git describe --always --dirty 2>/dev/null || echo "0.15-git")
CFLAGS += -DVERSION=\"${VERSION}\"

ifeq ($(DEBUG),1)
CFLAGS += -UNDEBUG -O0 -g -ggdb -Wall -Wextra -Wno-unused-parameter
endif

all: ${PROGNAME}

${PROGNAME}: libui.a libtext.a config.mk *.c *.h
	${CC} ${CFLAGS} ${SRC} ${LDFLAGS} ${OBJS} ${LIBS} -o $@

libtext.a:
	$(MAKE) -C text/

libui.a:
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
	@rm -f ${PROGNAME} 

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
	$(install_scheme)
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
	$(uninstall_scheme)
	@echo removing manual page from ${DESTDIR}${MANPREFIX}/man1
	@rm -f ${DESTDIR}${MANPREFIX}/man1/${PROGNAME}.1

.PHONY: all clean dist install uninstall debug
