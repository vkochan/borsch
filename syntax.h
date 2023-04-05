#ifndef SYNTAX_H
#define SYNTAX_H

typedef struct SyntaxParser SyntaxParser;

typedef struct
{
	int id;
	const char *name;
	void *data;
} SyntaxCapture;

typedef struct
{
	const char * (* chunk_read)(void *payload, uint32_t index, uint32_t *size);
	size_t       (* text_read)(void *payload, size_t pos, size_t len, char *buf);
	void 	*payload;
} SyntaxInput;

int syntax_init(void);
void syntax_cleanup(void);

int syntax_lang_rule_add(const char *lang_name, int type, const char *match, void *arg, int (*bind)(SyntaxCapture *cap, void *arg));
void syntax_lang_rule_remove(const char *lang_name, int type, const char *match);
void syntax_lang_rules_clear(const char *lang_name, int type);

SyntaxParser *syntax_parser_new(const char *lang_name);

void syntax_parser_delete(SyntaxParser *parser);

void syntax_parser_input_set(SyntaxParser *parser, SyntaxInput *input);

int syntax_parser_parse(SyntaxParser *parser);

int syntax_parser_edit(SyntaxParser *parser, size_t pos, size_t len);

void syntax_parser_rules_walk(
	SyntaxParser *parser, int type, size_t start, size_t end, void *arg,
	int (*cb) (SyntaxParser *parser,
		   int type, size_t start, size_t end,
		   void *data, void *arg)
);

#endif /* PARSER_H */
