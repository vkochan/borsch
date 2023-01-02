#include <unistd.h>
#include <stdlib.h>
#include <stdbool.h>

#include "window.h"

static bool layout_needs_arrange = false;
static unsigned int curr_tab;
static Tab tabs[MAXTABS + 1];

/* by default the first layout entry is used */
static Layout layouts[] = {
	{ "[]=", NULL },
	{ "+++", NULL },
	{ "TTT", NULL },
	{ "[ ]", NULL },
};

Layout *layout_get(int id)
{
	return &layouts[id];
}

bool layout_is_changed(void)
{
	return layout_needs_arrange;
}

void layout_changed(bool changed)
{
	layout_needs_arrange = changed;
}

void layout_set_func(int id, void (*arrange)(unsigned int, unsigned int, unsigned int, unsigned int))
{
	layout_get(id)->arrange = arrange;
}

Tab *tab_get(int tab)
{
	return &tabs[tab];
}

int tab_current_id_get(void)
{
	return curr_tab;
}

void tab_current_id_set(int tab)
{
	curr_tab = tab;
}

Frame *frame_get(int fid)
{
	return tabs[fid].f;
}

Frame *frame_current(void)
{
	return frame_get(tab_current_id_get());
}

void tabs_init(void) {
	int i;

	tab_current_id_set(1);
	for(i=0; i <= MAXTABS; i++) {
		tabs[i].f = calloc(1, sizeof(Frame));
		tabs[i].f->nmaster = NMASTER;
		tabs[i].f->mfact = MFACT;
		tabs[i].f->layout = layouts;
		tabs[i].f->layout_prev = layouts;
		tabs[i].f->msticky = false;
		tabs[i].f->name = NULL;
		tabs[i].f->cwd = calloc(CWD_MAX, 1);
		getcwd(tabs[i].f->cwd, CWD_MAX);
	}
}
