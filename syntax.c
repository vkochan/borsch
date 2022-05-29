#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <regex.h>

#include <tree_sitter/api.h>

#include "syntax.h"
#include "array.h"
#include "util.h"

#define SYNTAX_FUNC_MAX_CAPBUF_SIZE	256
#define SYNTAX_FUNC_ARG_COUNT		2

typedef enum
{
	SyntaxFuncTypeNone,
	SyntaxFuncTypeMatch,
	SyntaxFuncTypeEqual,
} SyntaxFuncType;

typedef enum
{
	SyntaxFuncArgTypeNone,
	SyntaxFuncArgTypeString,
	SyntaxFuncArgTypeCapture,
} SyntaxFuncArgType;

typedef enum
{
	SyntaxFuncReturnTypeSuccess,
	SyntaxFuncReturnTypeFail,
} SyntaxFuncReturnType;

typedef struct _SyntaxFuncArg
{
	SyntaxFuncArgType arg_type;
	int arg_id;
	bool is_compregex;
	regex_t regex;
} SyntaxFuncArg;

typedef struct
{
	int type;
	const char *match;
	Array func_array;
	TSQuery *query;
	void *data;
} SyntaxRule;

typedef struct _SyntaxFunc
{
	SyntaxFuncReturnType (*call)(struct _SyntaxFunc *func, SyntaxRule *rule, SyntaxParser *parser, const TSQueryMatch *match);
	int 		     (*init)(struct _SyntaxFunc *func, SyntaxRule *rule);
	void 		     (*cleanup)(struct _SyntaxFunc *func);
	Array args;
} SyntaxFunc;

typedef struct
{
	char *name;
	TSLanguage *(*get)();
	Array rules;
} SyntaxLang;

typedef struct SyntaxParser
{
	SyntaxInput input;
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

static const char *__syntax_func_arg_string_get(SyntaxFuncArg *arg, SyntaxRule *rule)
{
	uint32_t length;

	return ts_query_string_value_for_id(rule->query, arg->arg_id, &length);
}

static size_t __syntax_func_arg_capture_get(SyntaxFuncArg *arg, SyntaxParser *parser, const TSQueryMatch *match,
					    size_t max_size, char *str)
{
	int count = match->capture_count;
	int i;

	for (i = 0; i < count; i++) {
		const TSQueryCapture *cap = &match->captures[i];

		if (cap->index == arg->arg_id) {
			uint32_t node_start = ts_node_start_byte(cap->node);
			uint32_t node_end = ts_node_end_byte(cap->node);
			size_t len = MIN(max_size, node_end - node_start);

			return parser->input.text_read(parser->input.payload, node_start, len, str);
		}
	}

	return 0;
}

static int __syntax_func_match_init(SyntaxFunc *func, SyntaxRule *rule)
{
	int i;

	for (i = 0; i < array_length(&func->args); i++) {
		SyntaxFuncArg *arg = array_get(&func->args, i);

		arg->is_compregex = false;

		if (arg->arg_type == SyntaxFuncArgTypeString) {
			const char *pattern = __syntax_func_arg_string_get(arg, rule);
			int flags = REG_NOSUB | REG_EXTENDED | REG_NEWLINE;
			int ret;

			ret = regcomp(&arg->regex, pattern, flags);
			if (ret != 0) {
				fprintf(stderr, "XXX failed to compile: %s\n", pattern);
				return -1;
			}
			arg->is_compregex = true;
		}
	}

	return 0;
}

static void __syntax_func_match_cleanup(SyntaxFunc *func)
{
	for (int i = 0; i < array_length(&func->args); i++) {
		SyntaxFuncArg *arg = array_get(&func->args, i);

		if (arg->is_compregex)
			regfree(&arg->regex);
	}
}

static SyntaxFuncReturnType __syntax_func_match_call(SyntaxFunc *func, SyntaxRule *rule, SyntaxParser *parser, const TSQueryMatch *match)
{
	char capbuf[SYNTAX_FUNC_MAX_CAPBUF_SIZE];
	regex_t *regex = NULL;
	char *str = NULL;
	int i;

	for (i = 0; i < SYNTAX_FUNC_ARG_COUNT; i++) {
		SyntaxFuncArg *arg = array_get(&func->args, i);

		if (arg->arg_type == SyntaxFuncArgTypeString) {
			regex = &arg->regex;
		} else if (arg->arg_type == SyntaxFuncArgTypeCapture) {
			size_t len;

			len = __syntax_func_arg_capture_get(arg, parser, match, SYNTAX_FUNC_MAX_CAPBUF_SIZE-1, capbuf);
			if (!len)
				return SyntaxFuncReturnTypeFail;
			capbuf[len] = '\0';
			str = capbuf;
		} else {
			return SyntaxFuncReturnTypeFail;
		}
	}

	if (regexec(regex, str, 0, NULL, 0) != REG_NOMATCH)
		return SyntaxFuncReturnTypeSuccess;

	return SyntaxFuncReturnTypeFail;
}

static SyntaxFuncReturnType __syntax_func_eq_call(SyntaxFunc *func, SyntaxRule *rule,
						  SyntaxParser *parser, const TSQueryMatch *match)
{
	char capbuf[SYNTAX_FUNC_MAX_CAPBUF_SIZE];
	const char *str[SYNTAX_FUNC_ARG_COUNT];
	int i;

	for (i = 0; i < SYNTAX_FUNC_ARG_COUNT; i++) {
		SyntaxFuncArg *arg = array_get(&func->args, i);

		if (arg->arg_type == SyntaxFuncArgTypeString) {
			str[i] = __syntax_func_arg_string_get(arg, rule);
		} else if (arg->arg_type == SyntaxFuncArgTypeCapture) {
			size_t len;

			len = __syntax_func_arg_capture_get(arg, parser, match, SYNTAX_FUNC_MAX_CAPBUF_SIZE-1, capbuf);
			if (!len)
				return SyntaxFuncReturnTypeFail;
			capbuf[len] = '\0';
			str[i] = capbuf;
		} else {
			return SyntaxFuncReturnTypeFail;
		}
	}

	if (strcmp(str[0], str[1]) == 0)
		return SyntaxFuncReturnTypeSuccess;

	return SyntaxFuncReturnTypeFail;
}

static int __syntax_func_name_resolve(SyntaxFunc *func, const char *name)
{
	if (strcmp(name, "match?") == 0) {
		func->call =__syntax_func_match_call;
		func->init = __syntax_func_match_init;
		func->cleanup = __syntax_func_match_cleanup;
	} else if (strcmp(name, "eq?") == 0) {
		func->call = __syntax_func_eq_call;
	} else {
		return -1;
	}

	return 0;
}

static int __syntax_func_reset(SyntaxFunc *func)
{
	memset(func, 0, sizeof(*func));
	array_init_sized(&func->args, sizeof(SyntaxFuncArg));
}

static void __syntax_func_delete(SyntaxFunc *func)
{
	if (func->cleanup)
		func->cleanup(func);
	array_release(&func->args);
}

static int __syntax_rule_parse_func(SyntaxRule *rule)
{
	const TSQueryPredicateStep *preds;
	uint32_t pred_length;
	uint32_t length;
	const char *str_value;
	SyntaxFuncArg arg;
	SyntaxFunc func;
	int err;
	int i;

	preds = ts_query_predicates_for_pattern(rule->query, 0, &pred_length);

	__syntax_func_reset(&func);

	for (i = 0; i < pred_length; i++) {
		const TSQueryPredicateStep *pred = &preds[i];

		switch (pred->type)
		{
		case TSQueryPredicateStepTypeDone:
			/* TODO: current limitation */
			if (array_length(&func.args) != SYNTAX_FUNC_ARG_COUNT) {
				fprintf(stderr, "Invalid number of syntax func arguments\n");
				return -1;
			}
			if (!func.call) {
				fprintf(stderr, "Missing function in syntax predicate\n");
				return -1;
			}

			array_add(&rule->func_array, &func);
			__syntax_func_reset(&func);
			break;

		case TSQueryPredicateStepTypeCapture:
			arg.arg_type = SyntaxFuncArgTypeCapture;
			arg.arg_id = pred->value_id;
			array_add(&func.args, &arg);
			break;

		case TSQueryPredicateStepTypeString:
			str_value = ts_query_string_value_for_id(rule->query, pred->value_id, &length);
			err = __syntax_func_name_resolve(&func, str_value);
			if (!err) {
				continue;
			} else {
				arg.arg_type = SyntaxFuncArgTypeString;
				arg.arg_id = pred->value_id;
				array_add(&func.args, &arg);
			}
			break;
		}
	}

	for (i = 0; i < array_length(&rule->func_array); i++) {
		SyntaxFunc *func = array_get(&rule->func_array, i);

		if (func->init) {
			err = func->init(func, rule);
			if (err) {
				return -1;
			}
		}
	}

	return 0;
}

static SyntaxFuncReturnType __syntax_rule_call_func(SyntaxRule *rule, SyntaxParser *parser, const TSQueryMatch *match)
{
	for (int i = 0; i < array_length(&rule->func_array); i++) {
		SyntaxFunc *func = array_get(&rule->func_array, i);
		SyntaxFuncReturnType ret = SyntaxFuncReturnTypeSuccess;

		ret = func->call(func, rule, parser, match);
		if (ret != SyntaxFuncReturnTypeSuccess)
			return ret;
	}

	return SyntaxFuncReturnTypeSuccess;
}

static void __syntax_rule_delete(SyntaxRule *rule)
{
	size_t i;

	for (i = 0; i < array_length(&rule->func_array); i++) {
		__syntax_func_delete(array_get(&rule->func_array, i));
	}
	array_release(&rule->func_array);
	ts_query_delete(rule->query);
	free(rule->data);
}

int syntax_lang_rule_add(const char *lang_name, int type, const char *match, void *data)
{
	TSQueryError error_type;
	uint32_t error_offset;
	TSQuery *query;
	SyntaxRule rule = {0};
	SyntaxLang *lang;
	int err;

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

	array_init_sized(&rule.func_array, sizeof(SyntaxFunc));
	rule.match = match;
	rule.query = query;
	rule.type = type;
	rule.data = data;

	err = __syntax_rule_parse_func(&rule);
	if (err) {
		__syntax_rule_delete(&rule);
		return -1;
	}

	array_add(&lang->rules, &rule);
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

	for (i = 0; i < array_length(&lang->rules); i++) {
		rule = array_get(&lang->rules, i);

		if (strcmp(rule->match, match) == 0) {
			__syntax_rule_delete(rule);
			array_remove(&lang->rules, i);
			return;
		}
	}
}

void syntax_lang_rules_clear(const char *lang_name, int type)
{
	SyntaxLang *lang;
	int i;

	lang = get_lang_by_name(lang_name);
	if (!lang)
		return;

	for (i = 0; i < array_length(&lang->rules); i++) {
		__syntax_rule_delete(array_get(&lang->rules, i));
	}

	array_clear(&lang->rules);
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
		return NULL;;

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

void syntax_parser_input_set(SyntaxParser *parser, SyntaxInput *input)
{
	parser->input = *input;
}

static const char *syntax_parser_read(void *payload, uint32_t byte_index, TSPoint position,
				      uint32_t *bytes_read)
{
	SyntaxParser *parser = payload;

	return parser->input.chunk_read(parser->input.payload, byte_index, bytes_read);
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

	for (i = 0; i < array_length(&lang->rules); i++) {
		SyntaxRule *rule = array_get(&lang->rules, i);

		if (rule->type == type || type == -1) {
			TSQueryMatch match;

			ts_query_cursor_exec(cursor, rule->query, root_node);

			while (ts_query_cursor_next_match(cursor, &match)) {
				int count = match.capture_count;
				SyntaxFuncReturnType func_return;
				int i;

				func_return = __syntax_rule_call_func(rule, parser, &match);
				if (func_return != SyntaxFuncReturnTypeSuccess)
					continue;

				for (i = 0; i < count; i++) {
					const TSQueryCapture *cap = &match.captures[i];
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
	SyntaxLang *iter;

	for (iter = langs; iter && iter->name; iter++) {
		array_init_sized(&iter->rules, sizeof(SyntaxRule));
	}
}

void syntax_cleanup(void)
{
	SyntaxLang *iter;

	for (iter = langs; iter && iter->name; iter++) {
		syntax_lang_rules_clear(iter->name, -1);
		array_release(&iter->rules);
	}
}
