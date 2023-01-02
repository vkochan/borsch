#include <stdbool.h>

static bool layout_needs_arrange = false;
static unsigned int curr_tab;

bool layout_is_changed(void)
{
	return layout_needs_arrange;
}

void layout_changed(bool changed)
{
	layout_needs_arrange = changed;
}

int tab_current_id_get(void)
{
	return curr_tab;
}

void tab_current_id_set(int tab)
{
	curr_tab = tab;
}
