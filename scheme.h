#ifndef SCHEME_H
#define SCHEME_H

#include "api.h"

int scheme_init(const char *);
void scheme_uninit(void);
int scheme_event_handle(event_t evt);
int scheme_eval_file(const char *scm_in, const char *out);
void *scheme_env_alloc(void);
void scheme_env_free(void *env);

#endif /* SCHEME_H */
