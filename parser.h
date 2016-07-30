#ifndef parser_h
#define parser_h

enum PARSE_MODE {
  PARSE_ANY, PARSE_BQUOTE,
};

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

#endif /* parser_h */

/* vim:set et sw=2 cino=>2,\:0: */
