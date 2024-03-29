#ifndef UI_STYLE
#define UI_STYLE

typedef enum {
	UI_TEXT_STYLE_NORMAL	= (1 << 0),
	UI_TEXT_STYLE_BOLD	= (1 << 1),
	UI_TEXT_STYLE_DIM	= (1 << 2),
	UI_TEXT_STYLE_ITALIC	= (1 << 3),
	UI_TEXT_STYLE_UNDERLINE	= (1 << 4),
	UI_TEXT_STYLE_BLINK	= (1 << 5),
	UI_TEXT_STYLE_REVERSE	= (1 << 6),
	UI_TEXT_STYLE_INVIS	= (1 << 7),

	UI_TEXT_STYLE_MAX = 31
} ui_text_style_t;

#define UI_TEXT_COLOR_DEFAULT		-1
#define UI_TEXT_COLOR_BLACK		0
#define UI_TEXT_COLOR_RED		1
#define UI_TEXT_COLOR_GREEN		2
#define UI_TEXT_COLOR_YELLOW		3
#define UI_TEXT_COLOR_BLUE		4
#define UI_TEXT_COLOR_MAGENTA		5
#define UI_TEXT_COLOR_CYAN		6
#define UI_TEXT_COLOR_WHITE		7
#define UI_TEXT_COLOR_BRIGHT_BLACK	8
#define UI_TEXT_COLOR_BRIGHT_RED	9
#define UI_TEXT_COLOR_BRIGHT_GREEN	10
#define UI_TEXT_COLOR_BRIGHT_YELLOW	11
#define UI_TEXT_COLOR_BRIGHT_BLUE	12
#define UI_TEXT_COLOR_BRIGHT_MAGENTA	13
#define UI_TEXT_COLOR_BRIGHT_CYAN	14
#define UI_TEXT_COLOR_BRIGHT_WHITE	15

#define UI_TEXT_SYMBOL_ULCORNER		0x250C
#define UI_TEXT_SYMBOL_LLCORNER		0x2514
#define UI_TEXT_SYMBOL_URCORNER		0x2510
#define UI_TEXT_SYMBOL_LRCORNER		0x2518
#define UI_TEXT_SYMBOL_LTEE		0x251C
#define UI_TEXT_SYMBOL_RTEE		0x2524
#define UI_TEXT_SYMBOL_BTEE		0x2534
#define UI_TEXT_SYMBOL_TTEE		0x252C
#define UI_TEXT_SYMBOL_HLINE		0x2500
#define UI_TEXT_SYMBOL_VLINE		0x2502
#define UI_TEXT_SYMBOL_PLUS		0x253C

#endif /* UI_STYLE */
