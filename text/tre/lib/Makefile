TARGET = libtre.a

SOURCES =			\
	tre-ast.c		\
	tre-compile.c		\
	tre-match-backtrack.c	\
	tre-match-parallel.c	\
	tre-mem.c		\
	tre-parse.c		\
	tre-stack.c		\
	regcomp.c		\
	regexec.c		\
	regerror.c

SOURCES += tre-match-approx.c

OBJECTS = $(SOURCES:.c=.o)

CFLAGS += -DHAVE_CONFIG_H -I../

$(TARGET): $(OBJECTS)
	@echo "[Link (Static)]"
	@ar rcs $(TARGET) $(OBJECTS)

.c.o:
	@echo [Compile] $<
	@$(CC) -c $(CFLAGS) $(CFLAGS_STD) $(CFLAGS_LIBC) $(CFLAGS_EXTRA) $< -o $@

clean:
	rm -f $(TARGET) $(OBJECTS)
