include config.mk

SRCS = main.c \
      vt.c \
      array.c \
      view.c \
      buffer.c \
      keymap.c \
      event.c \
      timer.c \
      syntax.c

SCH_VERSION := $(shell ./scripts/get_scheme_version.sh)
SCH_MACHINE := $(shell ./scripts/get_scheme_machine.sh)
SCH_PREFIX ?= /usr
SCH_PATH = $(SCH_PREFIX)/lib/csv$(SCH_VERSION)/$(SCH_MACHINE)
LIBS += -lpthread -luuid -ldl -lm
BIN += ${PROGNAME}-eval
SRCS += scheme.c

SCH_SCRIPTS = main.ss

LDFLAGS += -L ./text -L ./ui
LIBS += -ltext -lui -ltree-sitter

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

.PHONY: libtext libui

all: ${PROGNAME}

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
