#ifndef XSTR_H
#define XSTR_H

typedef struct xstr {
	char   *str;
	unsigned short len;
	unsigned short cap;
	unsigned char flags;
} xstr_t;

xstr_t xstr(char *s);

char *xstr_cptr(xstr_t s);

xstr_t xstr_dup(xstr_t s);

xstr_t xstr_cat(xstr_t s1, xstr_t s2);

xstr_t xstr_join(xstr_t s1, xstr_t js, xstr_t s2);

size_t xstr_len(xstr_t s);

bool xstr_is_empty(xstr_t s);

bool xstr_has_prefix(xstr_t s, xstr_t p);

bool xstr_eq(xstr_t s1, xstr_t s2);

xstr_t xstr_tok(xstr_t *s, char delim);

void xstr_del(xstr_t s);

#endif /* XSTR_H */
