#ifndef TIMER_H
#define TIMER_H

int timer_add(void (*cb)(void *ctx), void *arg);
void timer_del(int fd);
void timer_interval_set(int fd, unsigned long ms);
void timer_time_set(int fd, unsigned long sec, unsigned long nsec);

#endif /* TIMER_H */
