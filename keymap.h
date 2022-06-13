#ifndef KEYMAP_H
#define KEYMAP_H

#include <stdbool.h>

typedef enum {
	KEYMAP_SYMB_FUNC,
	KEYMAP_SYMB_MAP,
} keymap_symb_t;

typedef struct KeyBinding KeyBinding;
typedef struct KeyMap KeyMap;

#define KEY_MOD_F_ALT		1
#define KEY_MOD_F_CTL		2

typedef struct {
	int flags;
	int code;
} KeyCode;

void keymap_ref_get(KeyMap *map);
void keymap_ref_put(KeyMap *map);
KeyMap *keymap_new(KeyMap *parent);
void keymap_free(KeyMap *map);
KeyBinding *keymap_find(KeyMap *map, char *key);
KeyBinding *keymap_match(KeyMap *map, KeyCode *keys, int len);
bool keymap_kbd_is_map(KeyBinding *kbd);
KeyMap *keymap_kbd_map_get(KeyBinding *kbd);
int keymap_kbd_len(KeyBinding *kbd);
void keymap_kbd_action(KeyBinding *kbd);
int keymap_bind(KeyMap *map, char *key, void (*act_cb)(void), char *act_map);
int keymap_unbind(KeyMap *map, char *key);
KeyMap *keymap_by_name(char *name);
KeyMap *keymap_by_id(int id);
int keymap_id_get(KeyMap *map);
KeyMap *keymap_parent_get(KeyMap *map);
void keymap_parent_set(KeyMap *map, KeyMap *pmap);
void keymap_parent_name_set(KeyMap *map, char *name);
void keymap_symb_resolver_set(void * (*resolv)(keymap_symb_t type, char *name));

#endif /* KEYMAP_H */
