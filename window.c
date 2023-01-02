#include <stdbool.h>

static bool layout_needs_arrange = false;

bool layout_is_changed(void)
{
	return layout_needs_arrange;
}

void layout_changed(bool changed)
{
	layout_needs_arrange = changed;
}
