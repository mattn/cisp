#ifndef _UTIL_H_
#define _UTIL_H_

EXPORT void load_libs(ENV *env);
EXPORT void buf_init(BUFFER *b);
EXPORT void buf_append(BUFFER *b, const char *s);
EXPORT void buf_free(BUFFER *b);

#endif /* _UTIL_H_ */

/* vim:set et sw=2 cino=>2,\:0: */
