#ifndef util_h
#define util_h

EXPORT void load_libs(ENV *env);
EXPORT void buf_init(BUFFER *b);
EXPORT void buf_append(BUFFER *b, const char *s);
EXPORT void buf_free(BUFFER *b);
EXPORT const char *intern(const char *s);

#endif /* util_h */

/* vim:set et sw=2 cino=>2,\:0: */
