#include <unistd.h>
#include <stdlib.h>
#include <stdbool.h>

#include "window.h"

static bool layout_needs_arrange = false;
static unsigned int curr_tab;
static Tab tabs[MAXTABS + 1];

/* by default the first layout entry is used */
static Layout layouts[] = {
	{ LAYOUT_TILED,     "[]=", NULL },
	{ LAYOUT_GRID,      "+++", NULL },
	{ LAYOUT_BSTACK,    "TTT", NULL },
	{ LAYOUT_MAXIMIZED, "[ ]", NULL },
};

Layout *layout_get(int id)
{
	return &layouts[id];
}

Layout *layout_current(void)
{
	return frame_current()->layout;
}

bool layout_is_changed(void)
{
	return layout_needs_arrange;
}

void layout_changed(bool changed)
{
	layout_needs_arrange = changed;
}

void layout_set_arrange(int id, void (*arrange)(unsigned int, unsigned int, unsigned int, unsigned int))
{
	layout_get(id)->arrange = arrange;
}

bool layout_is_arrange(int id)
{
	return layout_current()->id == id;
}

layout_t layout_current_get(int tab)
{
	return tab_get(tab)->f->layout->id;
}

int layout_current_set(int tab, layout_t lay)
{
	if (window_popup_get())
		return -1;

	frame_current()->layout_prev = frame_current()->layout;
	frame_current()->layout = layout_get(lay);
	layout_changed(true);
}

int layout_current_nmaster(void)
{
	return frame_current()->nmaster;
}

float layout_current_fmaster(void)
{
	return frame_current()->mfact;
}

int layout_nmaster_get(int tab)
{
	return tab_get(tab)->f->nmaster;
}

int layout_nmaster_set(int tab, int n)
{
	if (window_popup_get() || layout_is_arrange(LAYOUT_MAXIMIZED) || layout_is_arrange(LAYOUT_GRID))
		return -1;

	tab_get(tab)->f->nmaster = n;
	layout_changed(true);

	return 0;
}

float layout_fmaster_get(int tab)
{
	return tab_get(tab)->f->mfact;
}

int layout_fmaster_set(int tab, float mfact)
{
	if (window_popup_get() || layout_is_arrange(LAYOUT_MAXIMIZED) || layout_is_arrange(LAYOUT_GRID))
		return -1;

	tab_get(tab)->f->mfact = mfact;
	layout_changed(true);

	return 0;
}

bool layout_sticky_get(int tab)
{
	return tab_get(tab)->f->msticky;
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

int frame_current_id(void)
{
	return tab_current_id_get();
}

Frame *frame_current(void)
{
	return frame_get(tab_current_id_get());
}

int frame_current_set(int tab)
{
	tab_current_id_set(tab);
	layout_changed(true);
}

const char *frame_name_get(int tab)
{
	if (tab_get(tab)->f->name && strlen(tab_get(tab)->f->name)) {
		return tab_get(tab)->f->name;
	} else {
		return NULL;
	}
}

int frame_name_set(int tab, char *name)
{
	free(tab_get(tab)->f->name);
	tab_get(tab)->f->name = NULL;

	if (name && strlen(name))
		tab_get(tab)->f->name = strdup(name);
}

char *frame_cwd_get(int tab)
{
	return tab_get(tab)->f->cwd;
}

int frame_cwd_set(int tab, char *cwd)
{
	strncpy(tab_get(tab)->f->cwd, cwd, CWD_MAX - 1);
	return 0;
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

void tabs_cleanup(void)
{
	for(int i = 0; i <= MAXTABS; i++) {
		free(tab_get(i)->f->name);
		free(tab_get(i)->f->cwd);
		free(tab_get(i)->f);
	}
}

Window *window_popup_get(void)
{
	return frame_current()->popup;
}

void *window_popup_set(Window *p)
{
	frame_current()->popup = p;
}
