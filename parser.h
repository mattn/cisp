#ifndef _PARSER_H_
#define _PARSER_H_

#define PARSE_ANY 0
#define PARSE_BQUOTE 1

EXPORT NODE* parse_paren(SCANNER *s, int mode);
EXPORT NODE* parse_any(SCANNER *s, int mode);
EXPORT NODE* invalid_token(SCANNER *s);
EXPORT int s_reset(SCANNER *s);
EXPORT long s_pos(SCANNER *s);
EXPORT int s_eof(SCANNER *s);
EXPORT int s_peek(SCANNER *s);
EXPORT int s_getc(SCANNER *s);
EXPORT void skip_white(SCANNER *s);
EXPORT void s_file_init(SCANNER *s, FILE* v);

#endif /* _PARSER_H_ */

/* vim:set et sw=2 cino=>2,\:0: */
