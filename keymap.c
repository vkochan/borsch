#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sys/param.h>

#include <curses.h>

#include "keymap.h"

#define MAX_KEYS 3

static void* (*symb_resolver)(keymap_symb_t type, char *name);

typedef struct KeyMap KeyMap;

typedef struct KeyBinding {
	struct KeyBinding *next;
	KeyCode keys[MAX_KEYS];
	void (*act)(void);
	int len;
	char map_name[64];
	KeyMap *map;
} KeyBinding;

typedef struct KeyMap {
	KeyBinding *kbd_list;
	char parent_name[64];
	KeyMap *parent;
	KeyMap *prev;
	KeyMap *next;
	char name[64];
	int ref_count;
	int kid;
} KeyMap;

static KeyMap map_list;
static int map_id_gen;

void keymap_free(KeyMap *map);

KeyMap *keymap_by_name(char *name)
{
	KeyMap *map = NULL;

	if (name && strlen(name) && symb_resolver) {
		void *v = symb_resolver(KEYMAP_SYMB_MAP, name);
		int id;

		if (!v)
			return NULL;

		id = (int)v;
		if (id > 0)
			map = keymap_by_id(id);
		if (map)
			return map;
	}

	return NULL;
}

static int __keymap_code(char *key)
{
	if (strcmp(key, "<Space>") == 0) {
		return ' ';
	} else if (strcmp(key, "<Enter>") == 0) {
		return '\r';
	} else if (strcmp(key, "<Tab>") == 0) {
		return '\t';
	} else if (strcmp(key, "<Backspace>") == 0) {
		/* TODO: make it term specific and remove curses.h inclusion */
		return KEY_BACKSPACE;
	} else if (strcmp(key, "<Esc>") == 0) {
		return 27;
	} else {
		return key[0];
	}
}

static int keymap_parse(KeyBinding *kbd, char *key)
{
	char *tok_ptr = key;
	char tmp[60] = {0};
	char *tok;
	int i;

	strncpy(tmp, key, sizeof(tmp)-1);

	tok = strtok_r(tmp, " ", &tok_ptr);

	for (i = 0; i < MAX_KEYS && tok; i++) {
		if (strlen(tok) >= 3 && tok[0] == 'C' && tok[1] == '-') {
			kbd->keys[i].flags = KEY_MOD_F_CTL;
			kbd->keys[i].code = __keymap_code(&tok[2]);
		} else if (strlen(tok) >= 3 && tok[0] == 'M' && tok[1] == '-') {
			kbd->keys[i].flags = KEY_MOD_F_ALT;
			kbd->keys[i].code = __keymap_code(&tok[2]);
		} else {
			kbd->keys[i].code = __keymap_code(tok);;
		}

		tok = strtok_r(NULL, " ", &tok_ptr);
	}

	kbd->len = i;
	return 0;
}

static bool keycode_equal(KeyCode *k1, KeyCode *k2)
{
	return k1->code == k2->code && k1->flags == k2->flags;
}

KeyBinding *keymap_find(KeyMap *map, char *key)
{
	KeyBinding *it = NULL, kbd;
	int i;

	keymap_parse(&kbd, key);

	for (it = map->kbd_list; it; it = it->next) {
		int len = MIN(it->len, MAX_KEYS);
		int match = 0;

		if (it->len != kbd.len)
			continue;

		for (i = 0; i < len; i++)
			if (keycode_equal(&it->keys[i], &kbd.keys[i]))
				match++;

		if (match == kbd.len)
			return it;
	}

	return NULL;
}

/* TODO: optimize to store found map for next matching to do not run over all
 *       parents */
KeyBinding *keymap_match(KeyMap *map, KeyCode *keys, int len)
{
	KeyBinding *it;
	KeyMap *parent;
	int i, m;

	len = MIN(len, MAX_KEYS);

	for (it = map->kbd_list; it; it = it->next) {
		for (m = i = 0; i < len; i++) {
			if (keycode_equal(&it->keys[i], &keys[i]))
				m++;
		}

		if (m == len)
			return it;
	}

	parent = keymap_parent_get(map);
	if (parent)
		return keymap_match(parent, keys, len);

	return NULL;
}

bool keymap_kbd_is_map(KeyBinding *kbd)
{
	return kbd->map || kbd->map_name[0];
}

KeyMap *keymap_kbd_map_get(KeyBinding *kbd)
{
	if (kbd->map)
		return kbd->map;

	kbd->map = keymap_by_name(kbd->map_name);
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

int keymap_bind(KeyMap *map, char *key, void (*act_cb)(void), char *act_map)
{
	KeyBinding *kbd;

	kbd = keymap_find(map, key);
	if (!kbd) {
		kbd = calloc(1, sizeof(*kbd));

		kbd->next = map->kbd_list;
		map->kbd_list = kbd;

		keymap_parse(kbd, key);
	}

	if (act_map && strlen(act_map))
		strncpy(kbd->map_name, act_map, sizeof(kbd->map_name));
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

static void keymap_insert(KeyMap *head, KeyMap *map)
{
	map->next = head->next;
	map->prev = head;

	if (head->next)
		head->next->prev = map;
	head->next = map;
}

static void keymap_remove(KeyMap *map)
{
	if (map->prev)
		map->prev->next = map->next;
	if (map->next)
		map->next->prev = map->prev;
}

KeyMap *keymap_new(KeyMap *parent)
{
	KeyMap *map;

	map = calloc(1, sizeof(*map));
	if (!map)
		return NULL;

	map->kid = ++map_id_gen;
	map->parent = parent;
	map->ref_count = 1;

	if (parent)
		keymap_ref_get(parent);

	keymap_insert(&map_list, map);
	return map;
}

void keymap_free(KeyMap *map)
{
	KeyBinding *kbd;

	if (map->ref_count && --map->ref_count)
		return;

	keymap_remove(map);

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

	for (it = map_list.next; it; it = it->next) {
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
	if (map->parent)
		return map->parent;

	map->parent = keymap_by_name(map->parent_name);
	if (map->parent)
		keymap_ref_get(map->parent);

	return map->parent;
}

void keymap_parent_set(KeyMap *map, KeyMap *parent)
{
	if (map->parent)
		keymap_ref_put(map->parent);

	map->parent = parent;

	if (parent)
		keymap_ref_get(parent);
}

void keymap_parent_name_set(KeyMap *map, char *name)
{
	if (strcmp(map->parent_name, name) == 0)
		return;

	if (name)
		strncpy(map->parent_name, name, sizeof(map->parent_name));

	if (map->parent)
		keymap_ref_put(map->parent);

	map->parent = NULL;
}

void keymap_symb_resolver_set(void * (*resolv)(keymap_symb_t type, char *name))
{
	symb_resolver = resolv;
}
