#ifndef KEYMAP_H
#define KEYMAP_H

#include <stdbool.h>

typedef struct KeyBinding KeyBinding;
typedef struct KeyMap KeyMap;

KeyMap *keymap_new(KeyMap *parent);
void keymap_free(KeyMap *map);
KeyBinding *keymap_find(KeyMap *map, char *key);
KeyBinding *keymap_match(KeyMap *map, int *keys, int len);
bool keymap_kbd_is_map(KeyBinding *kbd);
KeyMap *keymap_kbd_map_get(KeyBinding *kbd);
int keymap_kbd_len(KeyBinding *kbd);
void keymap_kbd_action(KeyBinding *kbd);
int keymap_bind(KeyMap *map, char *key, void (*act_cb)(void), KeyMap *act_map);
int keymap_unbind(KeyMap *map, char *key);
KeyMap *keymap_by_id(int id);
KeyMap *keymap_parent_get(KeyMap *map);
void keymap_parent_set(KeyMap *map, KeyMap *parent);

#endif /* KEYMAP_H */
