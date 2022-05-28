#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include <tree_sitter/api.h>

typedef struct
{
	int type;
	const char *match;
	TSQuery *query;
	void *data;
} SyntaxRule;

typedef struct
{
	char *name;
	TSLanguage *(*get)();
	SyntaxRule *rules;
	int n_rules;
} SyntaxLang;

typedef struct SyntaxParser
{
	const char * (* read)(void *payload, uint32_t index, uint32_t *size);
	void *payload;
	TSParser *ts_parser;
	TSQueryCursor *cursor;
	uint32_t n_changes;
	TSRange *changes;
	TSTree *tree;
	SyntaxLang *lang;
} SyntaxParser;

TSLanguage *tree_sitter_c();
TSLanguage *tree_sitter_devicetree();
TSLanguage *tree_sitter_diff();
TSLanguage *tree_sitter_scheme();
TSLanguage *tree_sitter_make();
TSLanguage *tree_sitter_org();

static SyntaxLang langs[] = {
	{ "c", tree_sitter_c, },
	{ "dts", tree_sitter_devicetree, },
	{ "diff", tree_sitter_diff, },
	{ "scheme", tree_sitter_scheme, },
	{ "gnumake", tree_sitter_make, },
	{ "org", tree_sitter_org, },
	{ NULL, NULL, NULL, 0 }
};

static SyntaxLang *get_lang_by_name(const char *name)
{
	SyntaxLang *iter;

	for (iter = langs; iter; iter++) {
		if (strcmp(iter->name, name) == 0)
			return iter;
	}

	return NULL;
}

int syntax_lang_rule_add(const char *lang_name, int type, const char *match, void *data)
{
	TSQueryError error_type;
	uint32_t error_offset;
	TSQuery *query;
	SyntaxLang *lang;
	SyntaxRule *rule;

	lang = get_lang_by_name(lang_name);
	if (!lang)
		return -1;

	query = ts_query_new(lang->get(), match, strlen(match),
  			     &error_offset, &error_type);
	if (!query) {
		char *errmsg = "unknown";

		switch (error_type) {
		case TSQueryErrorSyntax: errmsg = "syntax"; break;
		case TSQueryErrorNodeType: errmsg = "node type"; break;
		case TSQueryErrorField: errmsg = "field"; break;
		case TSQueryErrorCapture: errmsg = "capture"; break;
		case TSQueryErrorStructure: errmsg = "structure"; break;
		case TSQueryErrorLanguage: errmsg = "language"; break;
		}

		/* TODO: return errmsg back to the caller */
		fprintf(stderr, "Invalid syntax query [%s]: (%s)\n", match, errmsg);
		return -2;
	}

	lang->n_rules++;
	lang->rules = realloc(lang->rules, sizeof(*rule) * lang->n_rules);
	if (!lang->rules) {
		ts_query_delete(query);
		return -1;
	}

	rule = &lang->rules[lang->n_rules-1];
	memset(rule, 0, sizeof(*rule));
	rule->match = match;
	rule->query = query;
	rule->type = type;
	rule->data = data;

	return 0;
}

void syntax_lang_rule_remove(const char *lang_name, int type, const char *match)
{
	SyntaxRule *rule;
	SyntaxLang *lang;
	int i;

	lang = get_lang_by_name(lang_name);
	if (!lang)
		return;

	for (i = 0; i < lang->n_rules; i++) {
		rule = &lang->rules[i];

		if (rule->type == type && strcmp(rule->match, match) == 0) {
			ts_query_delete(rule->query);
			/* TODO: pass a data destructor */
			free(rule->data);
			*rule = lang->rules[lang->n_rules-1];
			lang->n_rules--;
			lang->rules = realloc(lang->rules, sizeof(*rule) * lang->n_rules);
			if (!lang->rules && lang->n_rules) {
				fprintf(stderr, "Failed to re-allocate syntax rules\n");
				return;
			}

			return;
		}
	}
}

void syntax_lang_rules_clear(const char *lang_name, int type)
{
	SyntaxRule *rule;
	SyntaxLang *lang;
	int i;

	lang = get_lang_by_name(lang_name);
	if (!lang)
		return;

	for (i = 0; i < lang->n_rules; i++) {
		rule = &lang->rules[i];

		ts_query_delete(rule->query);
		/* TODO: pass a data destructor */
		free(rule->data);
	}
	lang->n_rules = 0;
	free(lang->rules);
}

SyntaxParser *syntax_parser_new(const char *lang_name)
{
	TSParser *ts_parser;
	SyntaxParser *parser;
	SyntaxLang *lang;

	lang = get_lang_by_name(lang_name);
	if (!lang)
		return NULL;

	parser = calloc(1, sizeof(*parser));
	if (!parser)
		return -1;

	ts_parser = ts_parser_new();
	if (!ts_parser)
		return NULL;

	if (ts_parser_set_language(ts_parser, lang->get())) {
		parser->cursor = ts_query_cursor_new();
		if (!parser->cursor)
			goto err_qry_cursor;

		parser->ts_parser = ts_parser;
		parser->lang = lang;
		return parser;
	}

err_qry_cursor:
	ts_parser_delete(ts_parser);
	free(parser);
	return NULL;
}

void syntax_parser_delete(SyntaxParser *parser)
{
	if (parser) {
		if (parser->cursor)
			ts_query_cursor_delete(parser->cursor);
		ts_tree_delete(parser->tree);
		ts_parser_delete(parser->ts_parser);
		free(parser->changes);
	}
}

void syntax_parser_input_set(SyntaxParser *parser, void *payload,
			     const char * (* read)(void *payload,
						   uint32_t index,
						   uint32_t *size))
{
	parser->payload = payload;
	parser->read = read;
}

static const char *syntax_parser_read(void *payload, uint32_t byte_index, TSPoint position,
				      uint32_t *bytes_read)
{
	SyntaxParser *parser = payload;

	return parser->read(parser->payload, byte_index, bytes_read);
}

int syntax_parser_parse(SyntaxParser *parser)
{
	TSInput input = {
		.encoding = TSInputEncodingUTF8,
		.read = syntax_parser_read,
		.payload = parser,
	};
	TSTree *old_tree = NULL;
	TSRange *new_changes = NULL;
	uint32_t n_changes;
	TSTree *new_tree;

	/* TODO: add support of tree edit */
	/* old_tree = parser->tree; */

	new_tree = ts_parser_parse(parser->ts_parser, old_tree, input);
	if (!new_tree) {
		return -1;
	}

	if (old_tree)
		new_changes = ts_tree_get_changed_ranges(old_tree, new_tree, &n_changes);

	free(parser->changes);
	ts_tree_delete(parser->tree);

	parser->n_changes = n_changes;
	parser->changes = new_changes;
	parser->tree = new_tree;

	return 0;
}

int syntax_parser_edit(SyntaxParser *parser, size_t pos, size_t len)
{
	/* TODO: implement editing */
	return syntax_parser_parse(parser);
}

void syntax_parser_rules_walk(
	SyntaxParser *parser, int type, size_t start, size_t end, void *arg,
	int (*cb) (SyntaxParser *parser,
		   int type, size_t start, size_t end,
		   void *data, void *arg)
)
{
	TSQueryCursor *cursor;;
	TSNode root_node;
	SyntaxLang *lang;
	int i;

	if (!parser)
		return;

	root_node = ts_tree_root_node(parser->tree);
	cursor = parser->cursor;
	lang = parser->lang;

	ts_query_cursor_set_byte_range(cursor, start, end);

	for (i = 0; i < lang->n_rules; i++) {
		SyntaxRule *rule = &lang->rules[i];

		if (rule->type == type || type == -1) {
			TSQueryMatch match;

			ts_query_cursor_exec(cursor, rule->query, root_node);

			while (ts_query_cursor_next_match(cursor, &match)) {
				int cap_count = match.capture_count;
				int c;

				for (c = 0; c < cap_count; c++) {
					const TSQueryCapture *cap = &match.captures[c];
					uint32_t node_start = ts_node_start_byte(cap->node);
					uint32_t node_end = ts_node_end_byte(cap->node);

					cb(parser, type, node_start, node_end-1, rule->data, arg);
				}
			}
		}
	}
}

int syntax_init(void)
{
}

void syntax_cleanup(void)
{
	SyntaxLang *iter;

	for (iter = langs; iter && iter->name; iter++) {
		syntax_lang_rules_clear(iter->name, -1);
	}
}
