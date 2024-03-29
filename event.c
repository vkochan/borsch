#include <errno.h>
#include <signal.h>
#include <stdlib.h>
#include <stdbool.h>
#include <time.h>
#include <sys/param.h>
#include <sys/select.h>

#include <stdio.h>

#define TIMEDIFF(t1, t2)	((t1.tv_sec-t2.tv_sec)*1000 + \
				(t1.tv_nsec-t2.tv_nsec)/1E6)

typedef struct event_fd {
	struct event_fd *prev, *next;
	void (*fn)(int fd, void *arg);
	void *arg;
	int fd;
} event_fd_t;

static event_fd_t event_fd_list;

static double minlatency = 8;
static double maxlatency = 33;

static double (*event_update_timeout_cb)(double timeout, void *arg);
static bool (*event_pre_handler_cb)(void *arg);

static void event_fd_list_insert(event_fd_t *list, event_fd_t *evt)
{
	evt->next = list->next;
	evt->prev = list;

	if (list->next)
		list->next->prev = evt;

	list->next = evt;
}

static void event_fd_list_delete(event_fd_t *evt)
{
	evt->prev->next = evt->next;

	if (evt->next)
		evt->next->prev = evt->prev;
}

void event_update_timeout_cb_set(double (*cb)(double timeout, void *arg))
{
	event_update_timeout_cb = cb;
}

void event_pre_handler_cb_set(bool (*cb)(void *arg))
{
	event_pre_handler_cb = cb;
}

int event_fd_handler_register(int fd, void (*fn)(int fd, void *arg), void *arg)
{
	event_fd_t *evt;

	evt = malloc(sizeof(*evt));
	if (!evt)
		return -1;

	evt->arg = arg;
	evt->fn = fn;
	evt->fd = fd;

	event_fd_list_insert(&event_fd_list, evt);

	return 0;
}

void event_fd_handler_unregister(int fd)
{
	event_fd_t *evt = event_fd_list.next;;
	int i;

	while (evt) {
		if (evt->fd == fd) {
			event_fd_list_delete(evt);
			free(evt);
			return;
		}
		evt = evt->next;
	}
}

int event_process(void *arg)
{
	struct timespec seltv, *tv, now, lastblink, trigger;
	sigset_t emptyset;
	double timeout = -1;
	event_fd_t *evt;
	int r, nfds = 0;
	fd_set rd;
	int idle;

	for (idle = 0;;) {
		bool ev = false;
		FD_ZERO(&rd);

		sigemptyset(&emptyset);

		for (evt = event_fd_list.next; evt; evt = evt->next) {
			FD_SET(evt->fd, &rd);
			nfds = MAX(nfds, evt->fd);
		}

		if (event_update_timeout_cb)
			timeout = event_update_timeout_cb(timeout, arg);

		seltv.tv_sec = timeout / 1E3;
		seltv.tv_nsec = 1E6 * (timeout - 1E3 * seltv.tv_sec);
		tv = timeout >= 0 ? &seltv : NULL;

		r = pselect(nfds + 1, &rd, NULL, NULL, tv, &emptyset);
		if (r < 0) {
			if (errno == EINTR)
				continue;
		}
		clock_gettime(CLOCK_MONOTONIC, &now);

		if (event_pre_handler_cb)
			ev = event_pre_handler_cb(arg);

		evt = event_fd_list.next;
		while (evt) {
			event_fd_t *next = evt->next;

			if (FD_ISSET(evt->fd, &rd) && evt->fn) {
				evt->fn(evt->fd, evt->arg);
			}
			evt = next;
		}

		/*
		 * To reduce flicker and tearing, when new content or event
		 * triggers drawing, we first wait a bit to ensure we got
		 * everything, and if nothing new arrives - we draw.
		 * We start with trying to wait minlatency ms. If more content
		 * arrives sooner, we retry with shorter and shorter periods,
		 * and eventually draw even without idle after maxlatency ms.
		 * Typically this results in low latency while interacting,
		 * maximum latency intervals during `cat huge.txt`, and perfect
		 * sync with periodic updates from animations/key-repeats/etc.
		 */
		if (r || ev) {
			if (!idle) {
				trigger = now;
				idle = 1;
			}
			timeout = (maxlatency - TIMEDIFF(now, trigger)) \
			          / maxlatency * minlatency;
			if (timeout > 0)
				continue;  /* we have time, try to find idle */
		}

		/* idle detected or maxlatency exhausted -> draw */
		return r;
	}
}
