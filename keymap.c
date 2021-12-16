#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sys/param.h>

#define MAX_KEYS 3

#define ALT	27
#if defined CTRL && defined _AIX
  #undef CTRL
#endif
#ifndef CTRL
  #define CTRL(k)   ((k) & 0x1F)
#endif
#define CTRL_ALT(k) ((k) + (129 - 'a'))

typedef unsigned int KeyCombo[MAX_KEYS];

typedef struct KeyMap KeyMap;

typedef struct KeyBinding {
	struct KeyBinding *next;
	KeyCombo keys;
	void (*act)(void);
	int len;
	KeyMap *map;
} KeyBinding;

typedef struct KeyMap {
	KeyBinding *kbd_list;
	char name[64];
	KeyMap *parent;
	KeyMap *next;
	int ref_count;
	int kid;
} KeyMap;

static KeyMap *map_list;
static int map_id_gen;

void keymap_free(KeyMap *map);

static int keymap_parse(KeyBinding *kbd, char *key)
{
	char *tok_ptr = key;
	char tmp[60] = {0};
	char *tok;
	int i;

	strncpy(tmp, key, sizeof(tmp)-1);

	tok = strtok_r(tmp, " ", &tok_ptr);

	for (i = 0; i < MAX_KEYS && tok; i++) {
		if (strlen(tok) == 3 && tok[0] == 'C' && tok[1] == '-') {
			kbd->keys[i] = CTRL(tok[2]);
		} else if (strlen(tok) == 3 && tok[0] == 'M' && tok[1] == '-') {
			kbd->keys[i++] = ALT;
			kbd->keys[i] = tok[2];
		} else if (strcmp(tok, "<Space>") == 0) {
			kbd->keys[i] = ' ';
		} else if (strcmp(tok, "<Enter>") == 0) {
			kbd->keys[i] = '\r';
		} else if (strcmp(tok, "<Tab>") == 0) {
			kbd->keys[i] = '\t';
		} else {
			kbd->keys[i] = tok[0];
		}

		tok = strtok_r(NULL, " ", &tok_ptr);
	}

	kbd->len = i;
	return 0;
}

KeyBinding *keymap_find(KeyMap *map, char *key)
{
	KeyBinding *it = NULL, kbd;
	int i;

	keymap_parse(&kbd, key);

	for (it = map->kbd_list; it; it = it->next) {
		int len = MIN(it->len, MAX_KEYS);

		if (it->len != kbd.len)
			continue;

		for (i = 0; i < len; i++)
			if (it->keys[i] == kbd.keys[i] && i == len - 1)
				return it;
	}

	return NULL;
}

/* TODO: optimize to store found map for next matching to do not run over all
 *       parents */
KeyBinding *keymap_match(KeyMap *map, int *key, int len)
{
	KeyBinding *it;
	int i;

	len = MIN(len, MAX_KEYS);

	for (it = map->kbd_list; it; it = it->next) {
		for (i = 0; i < len; i++) {
			if (it->keys[i] == key[i] && i == len - 1)
				return it;
		}
	}

	if (map->parent)
		return keymap_match(map->parent, key, len);

	return NULL;
}

bool keymap_kbd_is_map(KeyBinding *kbd)
{
	return kbd->map != NULL;
}

KeyMap *keymap_kbd_map_get(KeyBinding *kbd)
{
	return kbd->map;
}

int keymap_kbd_len(KeyBinding *kbd)
{
	return kbd->len;
}

void keymap_kbd_action(KeyBinding *kbd)
{
	kbd->act();
}

int keymap_bind(KeyMap *map, char *key, void (*act_cb)(void), KeyMap *act_map)
{
	KeyBinding *kbd;

	kbd = keymap_find(map, key);
	if (!kbd) {
		kbd = calloc(1, sizeof(*kbd));

		kbd->next = map->kbd_list;
		map->kbd_list = kbd;

		keymap_parse(kbd, key);
	}

	kbd->map = act_map;
	kbd->act = act_cb;

	return 0;
}

int keymap_unbind(KeyMap *map, char *key)
{
	KeyBinding *kbd, *prev;

	kbd = keymap_find(map, key);
	if (kbd) {
		for (prev = map->kbd_list; prev; prev = prev->next) {
			if (prev->next == kbd) {
				prev->next = kbd->next;
				break;
			}
		}

		free(kbd);
	}

	return 0;
}

void keymap_ref_get(KeyMap *map)
{
	map->ref_count++;
}

void keymap_ref_put(KeyMap *map)
{
	if (map->ref_count == 1) {
		keymap_free(map);
		return;
	} else if (map->ref_count > 1) {
		map->ref_count--;
	}
}

KeyMap *keymap_new(KeyMap *parent)
{
	KeyMap *map;

	map = calloc(1, sizeof(*map));
	if (!map)
		return NULL;

	map->kid = ++map_id_gen;
	map->parent = parent;
	map->next = map_list;
	map->ref_count = 1;

	if (parent)
		keymap_ref_get(parent);

	map_list = map;

	return map;
}

void keymap_free(KeyMap *map)
{
	KeyBinding *kbd;
	KeyMap *prev;

	if (map->ref_count && --map->ref_count)
		return;

	for (prev = map_list; prev; prev = prev->next) {
		if (prev->next == map) {
			prev->next = map->next;
			break;
		}
	}

	kbd = map->kbd_list;
	while (kbd) {
		KeyBinding *next = kbd->next;

		free(kbd);
		kbd = next;
	}

	if (map->parent)
		keymap_ref_put(map->parent);

	free(map);
}

KeyMap *keymap_by_id(int kid)
{
	KeyMap *it;

	for (it = map_list; it; it = it->next) {
		if (it->kid == kid)
			return it;
	}

	return NULL;
}

int keymap_id_get(KeyMap *map)
{
	return map->kid;
}

KeyMap *keymap_parent_get(KeyMap *map)
{
	return map->parent;
}

void keymap_parent_set(KeyMap *map, KeyMap *parent)
{
	if (parent)
		keymap_ref_get(parent);

	if (parent != map->parent && map->parent)
		keymap_ref_put(map->parent);

	map->parent = parent;
}
