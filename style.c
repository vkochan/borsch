#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "array.h"
#include "style.h"
#include "ui/ui.h"

static Array style_array;

int style_init(void)
{
	Style default_style = {
		.fg = UI_TEXT_COLOR_WHITE,
		.bg = UI_TEXT_COLOR_BLACK,
		.name = "default",
	};

	array_init_sized(&style_array, sizeof(Style));
	style_add(&default_style);
}

void style_cleanup(void)
{
	array_release(&style_array);
}

Style *style_new(void)
{
	Style *style;

	style = calloc(1, sizeof(*style));
	if (!style)
		return NULL;

	style->id = -1;
	style->fg = -1;
	style->bg = -1;

	return style;
}

int style_add(Style *style)
{
	if (style->name && style->name[0] && style_get_by_name(style->name)) {
		return -1;
	}

	if (array_add(&style_array, style)) {
		int id = array_length(&style_array) - 1;
		Style *added = array_get(&style_array, id);
		
		added->id = id;
		return id;
	} else {
		return -1;
	}
}

int style_update(int id, Style *update)
{
	Style *style;

	style = style_get_by_id(id);
	if (!style) {
		return -1;
	}

	*style = *update;
	style->id = id;

	return 0;
}

Style *style_get_by_id(int id)
{
	return array_get(&style_array, id);
}

Style *style_get_by_name(const char *name)
{
	if (!name || !name[0])
		return NULL;

	for (int i = 0; i < array_length(&style_array); i++) {
		Style *style = array_get(&style_array, i);

		if (strcmp(style->name, name) == 0)
			return style;
	}

	return NULL;
}
