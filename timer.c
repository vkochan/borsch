#include <sys/timerfd.h>
#include <inttypes.h>
#include <time.h>
#include <unistd.h>
#include <stdlib.h>

#include "event.h"

typedef struct timer_entry {
	struct timer_entry *prev, *next;
	void (*cb)(void *arg);
	void *arg;
	int fd;
} timer_entry_t;

static timer_entry_t timer_list;

static void timer_list_insert(timer_entry_t *head, timer_entry_t *entry)
{
	entry->next = head->next;
	entry->prev = head;

	if (head->next)
		head->prev = entry;
	head->next = entry;
}

static void timer_list_remove(timer_entry_t *entry)
{
	if (entry->prev)
		entry->prev->next = entry->next;
	if (entry->next)
		entry->next->prev = entry->prev;
}

static void timer_fd_handler(int fd, void *arg)
{
	timer_entry_t *timer = arg;
	uint64_t exp;

	timer->cb(timer->arg);
	read(fd, &exp, sizeof(exp));
}

static timer_entry_t *timer_entry_by_fd(int fd)
{
	timer_entry_t *timer;

	for (timer = timer_list.next; timer; timer = timer->next)
	{
		if (timer->fd == fd)
			return timer;
	}

	return NULL;
}

int timer_add(void (*cb)(void *ctx), void *arg)
{
	timer_entry_t *timer;
	int err;
	int fd;

	timer = calloc(1, sizeof(*timer));
	if (!timer)
		return -1;

	fd = timerfd_create(CLOCK_REALTIME, TFD_NONBLOCK|TFD_CLOEXEC);
	if (fd == -1) {
		free(timer);
		return -1;
	}

	timer->arg = arg;
	timer->cb = cb;
	timer->fd = fd;

	err = event_fd_handler_register(fd, timer_fd_handler, timer);
	if (err)
		return -1;

	timer_list_insert(&timer_list, timer);

	return fd;
}

void timer_del(int fd)
{
	timer_entry_t *timer = timer_entry_by_fd(fd);

	if (timer) {
		event_fd_handler_unregister(fd);
		timer_list_remove(timer);
		free(timer);
	}
}

void timer_interval_set(int fd, unsigned long ms)
{
	struct itimerspec spec;
	struct timespec now;
        long nsec;
        time_t sec;
	int err;

	sec = (time_t) ((ms)/1000);
	nsec = 1000000*ms-sec*1000000000;

	spec.it_value.tv_nsec = nsec;
	spec.it_value.tv_sec = sec;
	spec.it_interval.tv_nsec = nsec;
	spec.it_interval.tv_sec = sec;

	err = clock_gettime(CLOCK_REALTIME, &now);
	if (!err) {
		spec.it_value.tv_sec += now.tv_sec;
		timerfd_settime(fd, TFD_TIMER_ABSTIME, &spec, NULL);
	}
}

void timer_time_set(int fd, unsigned long sec, unsigned long nsec)
{
	struct itimerspec spec =
	{
		{ 0, 0 },
		{ sec, nsec }
	};

	timerfd_settime(fd, 0, &spec, NULL);
}
