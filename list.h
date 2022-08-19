#ifndef LIST_H
#define LIST_H

#define container_of(ptr, type, member) ({	\
		void *__mptr = (void *)(ptr);	\
		((type *)(__mptr - offsetof(type, member))); })

typedef struct list_item {
	struct list_item *prev;
	struct list_item *next;
} list_t;

static inline void list_init(list_t *list)
{
	list->prev = list->next = list;
}

static inline bool list_empty(list_t *list)
{
	return list->next == list;
}

static inline void __list_add(list_t *new, list_t *prev, list_t *next)
{
	next->prev = new;
	new->next = next;
	new->prev = prev;
	prev->next = new;
}

static inline void list_add_first(list_t *new, list_t *head)
{
	__list_add(new, head, head->next);
}

static inline void list_add_tail(list_t *new, list_t *head)
{
	__list_add(new, head->prev, head);
}

static inline void __list_del(list_t *prev, list_t *next)
{
	next->prev = prev;
	prev->next = next;
}

static inline void list_del(list_t *entry)
{
	__list_del(entry->prev, entry->next);
}

#define list_entry(ptr, type, member) \
	container_of(ptr, type, member)

#define list_first_entry(ptr, type, member) \
	list_entry((ptr)->next, type, member)

#define list_next_entry(pos, member) \
	list_entry((pos)->member.next, typeof(*(pos)), member)

#define list_entry_is_head(pos, head, member)				\
	(&pos->member == (head))

#define list_for_each(pos, head) \
	for (pos = (head)->next; pos != (head); pos = pos->next)

#define list_for_each_entry(pos, head, member)				\
	for (pos = list_first_entry(head, typeof(*pos), member);	\
	     !list_entry_is_head(pos, head, member);			\
	     pos = list_next_entry(pos, member))

#define list_for_each_entry_safe(pos, n, head, member)		\
	for (pos = list_first_entry(head, typeof(*pos), member), \
	     n = list_next_entry(pos, member); \
	     &pos->member != (head); \
	     pos = n, n = list_next_entry(n, member))

#endif /* LIST_H */
