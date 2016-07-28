#ifndef _PARSER_H_
#define _PARSER_H_

#define PARSE_ANY 0
#define PARSE_BQUOTE 1

NODE* parse_paren(SCANNER *s, int mode);
NODE* parse_any(SCANNER *s, int mode);
NODE* invalid_token(SCANNER *s);
int s_reset(SCANNER *s);
long s_pos(SCANNER *s);
int s_eof(SCANNER *s);
int s_peek(SCANNER *s);
int s_getc(SCANNER *s);
void skip_white(SCANNER *s);
void s_file_init(SCANNER *s, FILE* v);

#endif /* _PARSER_H_ */
