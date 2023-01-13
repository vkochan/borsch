#ifndef STYLE_H
#define STYLE_H

typedef struct {
	bool expand;
	const char *name;
	short fg;
	short bg;
	int attr;
	int id;
} Style;

int style_init(void);

void style_cleanup(void);

Style *style_new(void);

int style_add(Style *style);

int style_update(int id, Style *update);

Style *style_get_by_id(int id);

Style *style_get_by_name(const char *name);

#endif /* STYLE_H */
