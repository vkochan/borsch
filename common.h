#ifndef COMMON_H
#define COMMON_H

typedef enum {
	EVT_KEY_PRESS    = 100,
	EVT_TEXT_INSERT  = 101,
	EVT_PROC_EXIT    = 200,
	EVT_VTERM_FILTER = 300,
} event_id_t;

typedef struct {
	event_id_t   eid;
	int          oid;
	char	     *str;
	size_t	     len;
} event_t;

#define LENGTH(arr) (sizeof(arr) / sizeof((arr)[0]))
#define MAX(x, y)   ((x) > (y) ? (x) : (y))
#define MIN(x, y)   ((x) < (y) ? (x) : (y))

#endif /* COMMON_H */
