#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include "xstr.h"

#define BIT(x) (1UL << (x))

#define XMEM_BIT_ALLOC	BIT(0)

#define max(a,b)             \
	({                           \
	 __typeof__ (a) _a = (a); \
	 __typeof__ (b) _b = (b); \
	 _a > _b ? _a : _b;       \
	 })

#define min(a,b)             \
	({                           \
	 __typeof__ (a) _a = (a); \
	 __typeof__ (b) _b = (b); \
	 _a < _b ? _a : _b;       \
	 })

xstr_t xstr(char *s)
{
	xstr_t ret = {0};

	if (s) {
		ret.len = strlen(s);
		ret.cap = ret.len;
		ret.str = s;
	}

	return ret;
}

char *xstr_cptr(xstr_t s)
{
	return s.str;
}

xstr_t xstr_dup(xstr_t s)
{
	xstr_t ret;

	ret.str = strndup(s.str, s.len);
	ret.flags = XMEM_BIT_ALLOC;
	ret.len = s.len;
	ret.cap = s.len;

	return ret;
}

xstr_t xstr_cat(xstr_t s1, xstr_t s2)
{
	xstr_t ret;

	if (!s1.len)
		return s2;
	if (!s2.len)
		return s1;

	ret.len = s1.len + s2.len;
	ret.str = calloc(ret.len + 1, sizeof(*ret.str));
	ret.flags = XMEM_BIT_ALLOC;
	ret.cap = ret.len;

	strncpy(ret.str, s1.str, s1.len);
	strncpy(ret.str + s1.len, s2.str, s2.len);

	return ret;
}

xstr_t xstr_join(xstr_t s1, xstr_t js, xstr_t s2)
{
	xstr_t ret;

	ret.len = s1.len + js.len + s2.len;
	ret.str = calloc(ret.len + 1, sizeof(*ret.str));
	ret.flags = XMEM_BIT_ALLOC;
	ret.cap = ret.len;

	strncpy(ret.str, s1.str, s1.len);
	strncpy(ret.str + s1.len, js.str, js.len);
	strncpy(ret.str + s1.len + js.len, s2.str, s2.len);

	return ret;
}

size_t xstr_len(xstr_t s)
{
	return s.len;
}

bool xstr_is_empty(xstr_t s)
{
	return s.len == 0;
}

bool xstr_has_prefix(xstr_t s, xstr_t p)
{
	char *s_cptr = s.str;
	char *p_cptr = p.str;
	int i;

	if (p.len > s.len)
		return false;

	for (i = 0; i < p.len; i++) {
		if (*s_cptr != *p_cptr)
			return false;
	}

	return i == p.len;
}

bool xstr_eq(xstr_t s1, xstr_t s2)
{
	if (s1.len != s2.len)
		return false;

	return strncmp(s1.str, s2.str, min(s1.len, s2.len)) == 0;
}

xstr_t xstr_tok(xstr_t *s, char delim)
{
	char *start, *end;
	xstr_t ret = {0};

	if (s->len == 0)
		return ret;

	for (start = s->str; start != s->str + s->len && *start == delim; start++)
		;
	for (end = start; end != s->str + s->len && *end != delim; end++)
		;

	ret.len = end - start;
	ret.str = start;
	ret.cap = ret.len;

	s->len -= end - s->str;
	s->str = end;

	return ret;
}

void xstr_clear(xstr_t *s)
{
	s->str[0] = '\0';
	s->len = 0;
}

void xstr_copy(xstr_t *s, xstr_t from)
{
	if ((s->flags & XMEM_BIT_ALLOC) && s->cap >= from.len) {
		memcpy(s->str, from.str, from.len);
		s->str[from.len] = '\0';
	} else {
		xstr_t dup = xstr_dup(from);

		xstr_del(*s);
		*s = dup;
	}
}

void xstr_del(xstr_t s)
{
	if (s.flags & XMEM_BIT_ALLOC)
		free(s.str);
}
