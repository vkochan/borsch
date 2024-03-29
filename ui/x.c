/* See LICENSE for license details. */
#include <errno.h>
#include <math.h>
#include <limits.h>
#include <locale.h>
#include <signal.h>
#include <sys/select.h>
#include <time.h>
#include <unistd.h>
#include <libgen.h>
#include <X11/Xatom.h>
#include <X11/Xlib.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <X11/Xft/Xft.h>
#include <X11/XKBlib.h>

#define KeyCode _KeyCode
#include "keymap.h"
#include "ui/ui.h"
#include "event.h"

/* Arbitrary sizes */
#define UTF_INVALID   0xFFFD
#define UTF_SIZ       4

/* macros */
#define MAX(a, b)		((a) < (b) ? (b) : (a))
#define LEN(a)			(sizeof(a) / sizeof(a)[0])
#define BETWEEN(x, a, b)	((a) <= (x) && (x) <= (b))
#define DIVCEIL(n, d)		(((n) + ((d) - 1)) / (d))
#define DEFAULT(a, b)		(a) = (a) ? (a) : (b)
#define ATTRCMP(a, b)		((a).style.attr != (b).style.attr || (a).style.fg != (b).style.fg || \
				(a).style.bg != (b).style.bg)

#define IS_TRUECOL(x)		(1 << 24 & (x))
#define TRUERED(x)		(((x) & 0xff0000) >> 8)
#define TRUEGREEN(x)		(((x) & 0xff00))
#define TRUEBLUE(x)		(((x) & 0xff) << 8)

enum win_mode {
	MODE_VISIBLE     = 1 << 0,
	MODE_FOCUSED     = 1 << 1,
	MODE_APPKEYPAD   = 1 << 2,
	MODE_MOUSEBTN    = 1 << 3,
	MODE_MOUSEMOTION = 1 << 4,
	MODE_REVERSE     = 1 << 5,
	MODE_KBDLOCK     = 1 << 6,
	MODE_HIDE        = 1 << 7,
	MODE_APPCURSOR   = 1 << 8,
	MODE_MOUSESGR    = 1 << 9,
	MODE_8BIT        = 1 << 10,
	MODE_BLINK       = 1 << 11,
	MODE_FBLINK      = 1 << 12,
	MODE_FOCUS       = 1 << 13,
	MODE_MOUSEX10    = 1 << 14,
	MODE_MOUSEMANY   = 1 << 15,
	MODE_BRCKTPASTE  = 1 << 16,
	MODE_NUMLOCK     = 1 << 17,
	MODE_MOUSE       = MODE_MOUSEBTN|MODE_MOUSEMOTION|MODE_MOUSEX10\
	                  |MODE_MOUSEMANY,
};

typedef XftDraw *Draw;
typedef XftColor Color;
typedef XftGlyphFontSpec GlyphFontSpec;

typedef unsigned char uchar;
typedef unsigned int uint;
typedef unsigned long ulong;
typedef unsigned short ushort;

typedef uint_least32_t Rune;

static const uchar utfbyte[UTF_SIZ + 1] = {0x80,    0, 0xC0, 0xE0, 0xF0};
static const uchar utfmask[UTF_SIZ + 1] = {0xC0, 0x80, 0xE0, 0xF0, 0xF8};
static const Rune utfmin[UTF_SIZ + 1] = {       0,    0,  0x80,  0x800,  0x10000};
static const Rune utfmax[UTF_SIZ + 1] = {0x10FFFF, 0x7F, 0x7FF, 0xFFFF, 0x10FFFF};

/*
 * Default columns and rows numbers
 */
static unsigned int cols = 120;
static unsigned int rows = 40;

static int borderpx = 2;

/* Kerning / character bounding-box multipliers */
static float cwscale = 1.0;
static float chscale = 1.0;

/* default TERM value */
char *termname = "borsch-256color";

static char *opt_title = "borsch";
static char *opt_class = NULL;
static char *opt_name  = NULL;

/* Font Ring Cache */
enum {
	FRC_NORMAL,
	FRC_ITALIC,
	FRC_BOLD,
	FRC_ITALICBOLD
};

typedef struct {
	XftFont *font;
	int flags;
	Rune unicodep;
} Fontcache;

/* Fontcache is an array now. A new font will be appended to the array. */
static Fontcache *frc = NULL;
static int frclen = 0;
static int frccap = 0;

static char *font = "Liberation Mono:pixelsize=16:antialias=true:autohint=true";
static char *usedfont = NULL;
static double usedfontsize = 0;
static double defaultfontsize = 0;


/* Font structure */
#define Font Font_
typedef struct {
	int height;
	int width;
	int ascent;
	int descent;
	int badslant;
	int badweight;
	short lbearing;
	short rbearing;
	XftFont *match;
	FcFontSet *set;
	FcPattern *pattern;
} Font;

/* Drawing Context */
typedef struct {
	Color *col;
	size_t collen;
	Font font, bfont, ifont, ibfont;
	GC gc;
} DC;

typedef struct {
	Ui ui;

	Display *dpy;
	DC dc;
	Colormap cmap;
	Window win;
	Drawable buf;
	GlyphFontSpec *specbuf; /* font spec buffer used for rendering */
	Atom xembed, wmdeletewin, netwmname, netwmiconname, netwmpid;
	struct {
		XIM xim;
		XIC xic;
		XPoint spot;
		XVaNestedList spotlist;
	} ime;
	Draw draw;
	Visual *vis;
	XSetWindowAttributes attrs;
	int scr;
	int isfixed; /* is fixed geometry? */
	int l, t; /* left and top offset */
	int gm; /* geometry mask */
	int ch; /* char height */
	int cw; /* char width  */
	int w, h; /* window width and height */
	int mode; /* window state/mode flags */
	int cursor; /* cursor style */
	bool need_resize;
} XUi;

typedef struct {
	UiWin win;
} XWin;

/*
 * Printable characters in ASCII, used to estimate the advance width
 * of single wide characters.
 */
static char ascii_printable[] =
	" !\"#$%&'()*+,-./0123456789:;<=>?"
	"@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"
	"`abcdefghijklmnopqrstuvwxyz{|}~";

/* Terminal colors (16 first used in escape sequence) */
static const char *colorname[] = {
	/* 8 normal colors */
	"black",
	"red3",
	"green3",
	"yellow3",
	"blue2",
	"magenta3",
	"cyan3",
	"gray90",

	/* 8 bright colors */
	"gray50",
	"red",
	"green",
	"yellow",
	"#5c5cff",
	"magenta",
	"cyan",
	"white",

	[255] = 0,

	/* more colors can be added after 255 to use with DefaultXX */
	"#cccccc",
	"#555555",
};

/*
 * Default colors (colorname index)
 * foreground, background, cursor, reverse cursor
 */
unsigned int defaultfg = 7;
unsigned int defaultbg = 0;
static unsigned int defaultcs = 256;
static unsigned int defaultrcs = 257;

/*
 * thickness of underline and bar cursors
 */
static unsigned int cursorthickness = 2;

/*
 * Default colour and shape of the mouse cursor
 */
static unsigned int mouseshape = XC_xterm;
static unsigned int mousefg = 7;
static unsigned int mousebg = 0;

static size_t utf8validate(Rune *u, size_t i)
{
	if (!BETWEEN(*u, utfmin[i], utfmax[i]) || BETWEEN(*u, 0xD800, 0xDFFF))
		*u = UTF_INVALID;
	for (i = 1; *u > utfmax[i]; ++i)
		;

	return i;
}

static Rune utf8decodebyte(char c, size_t *i)
{
	for (*i = 0; *i < LEN(utfmask); ++(*i))
		if (((uchar)c & utfmask[*i]) == utfbyte[*i])
			return (uchar)c & ~utfmask[*i];

	return 0;
}

static size_t utf8decode(const char *c, Rune *u, size_t clen)
{
	size_t i, j, len, type;
	Rune udecoded;

	*u = UTF_INVALID;
	if (!clen)
		return 0;
	udecoded = utf8decodebyte(c[0], &len);
	if (!BETWEEN(len, 1, UTF_SIZ))
		return 1;
	for (i = 1, j = 1; i < clen && j < len; ++i, ++j) {
		udecoded = (udecoded << 6) | utf8decodebyte(c[i], &type);
		if (type != 0)
			return j;
	}
	if (j < len)
		return 0;
	*u = udecoded;
	utf8validate(u, len);

	return len;
}

char
utf8encodebyte(Rune u, size_t i)
{
	return utfbyte[i] | (u & ~utfmask[i]);
}

size_t
utf8encode(Rune u, char *c)
{
	size_t len, i;

	len = utf8validate(&u, 0);
	if (len > UTF_SIZ)
		return 0;

	for (i = len - 1; i != 0; --i) {
		c[i] = utf8encodebyte(u, 0);
		u >>= 6;
	}
	c[0] = utf8encodebyte(u, len);

	return len;
}

int x_loadfont(XUi *xui, Font *f, FcPattern *pattern)
{
	FcPattern *configured;
	FcPattern *match;
	FcResult result;
	XGlyphInfo extents;
	int wantattr, haveattr;

	/*
	 * Manually configure instead of calling XftMatchFont
	 * so that we can use the configured pattern for
	 * "missing glyph" lookups.
	 */
	configured = FcPatternDuplicate(pattern);
	if (!configured)
		return 1;

	FcConfigSubstitute(NULL, configured, FcMatchPattern);
	XftDefaultSubstitute(xui->dpy, xui->scr, configured);

	match = FcFontMatch(NULL, configured, &result);
	if (!match) {
		FcPatternDestroy(configured);
		return 1;
	}

	if (!(f->match = XftFontOpenPattern(xui->dpy, match))) {
		FcPatternDestroy(configured);
		FcPatternDestroy(match);
		return 1;
	}

	if ((XftPatternGetInteger(pattern, "slant", 0, &wantattr) ==
	    XftResultMatch)) {
		/*
		 * Check if xft was unable to find a font with the appropriate
		 * slant but gave us one anyway. Try to mitigate.
		 */
		if ((XftPatternGetInteger(f->match->pattern, "slant", 0,
		    &haveattr) != XftResultMatch) || haveattr < wantattr) {
			f->badslant = 1;
			fputs("font slant does not match\n", stderr);
		}
	}

	if ((XftPatternGetInteger(pattern, "weight", 0, &wantattr) ==
	    XftResultMatch)) {
		if ((XftPatternGetInteger(f->match->pattern, "weight", 0,
		    &haveattr) != XftResultMatch) || haveattr != wantattr) {
			f->badweight = 1;
			fputs("font weight does not match\n", stderr);
		}
	}

	XftTextExtentsUtf8(xui->dpy, f->match,
		(const FcChar8 *) ascii_printable,
		strlen(ascii_printable), &extents);

	f->set = NULL;
	f->pattern = configured;

	f->ascent = f->match->ascent;
	f->descent = f->match->descent;
	f->lbearing = 0;
	f->rbearing = f->match->max_advance_width;

	f->height = f->ascent + f->descent;
	f->width = DIVCEIL(extents.xOff, strlen(ascii_printable));

	return 0;
}

int x_loadfonts(XUi *xui, const char *fontstr, double fontsize)
{
	FcPattern *pattern;
	double fontval;

	if (fontstr[0] == '-')
		pattern = XftXlfdParse(fontstr, False, False);
	else
		pattern = FcNameParse((const FcChar8 *)fontstr);

	if (!pattern) {
		fprintf(stderr, "can't open font %s\n", fontstr);
		return -1;
	}

	if (fontsize > 1) {
		FcPatternDel(pattern, FC_PIXEL_SIZE);
		FcPatternDel(pattern, FC_SIZE);
		FcPatternAddDouble(pattern, FC_PIXEL_SIZE, (double)fontsize);
		usedfontsize = fontsize;
	} else {
		if (FcPatternGetDouble(pattern, FC_PIXEL_SIZE, 0, &fontval) ==
				FcResultMatch) {
			usedfontsize = fontval;
		} else if (FcPatternGetDouble(pattern, FC_SIZE, 0, &fontval) ==
				FcResultMatch) {
			usedfontsize = -1;
		} else {
			/*
			 * Default font size is 12, if none given. This is to
			 * have a known usedfontsize value.
			 */
			FcPatternAddDouble(pattern, FC_PIXEL_SIZE, 12);
			usedfontsize = 12;
		}
		defaultfontsize = usedfontsize;
	}

	if (x_loadfont(xui, &xui->dc.font, pattern)) {
		fprintf(stderr, "can't open font %s\n", fontstr);
		return -1;
	}

	if (usedfontsize < 0) {
		FcPatternGetDouble(xui->dc.font.match->pattern,
		                   FC_PIXEL_SIZE, 0, &fontval);
		usedfontsize = fontval;
		if (fontsize == 0)
			defaultfontsize = fontval;
	}

	/* Setting character width and height. */
	xui->cw = ceilf(xui->dc.font.width * cwscale);
	xui->ch = ceilf(xui->dc.font.height * chscale);

	FcPatternDel(pattern, FC_SLANT);
	FcPatternAddInteger(pattern, FC_SLANT, FC_SLANT_ITALIC);
	if (x_loadfont(xui, &xui->dc.ifont, pattern)) {
		fprintf(stderr, "can't open font %s\n", fontstr);
		return -1;
	}

	FcPatternDel(pattern, FC_WEIGHT);
	FcPatternAddInteger(pattern, FC_WEIGHT, FC_WEIGHT_BOLD);
	if (x_loadfont(xui, &xui->dc.ibfont, pattern)) {
		fprintf(stderr, "can't open font %s\n", fontstr);
		return -1;
	}

	FcPatternDel(pattern, FC_SLANT);
	FcPatternAddInteger(pattern, FC_SLANT, FC_SLANT_ROMAN);
	if (x_loadfont(xui, &xui->dc.bfont, pattern)) {
		fprintf(stderr, "can't open font %s\n", fontstr);
		return -1;
	}

	FcPatternDestroy(pattern);
}

void __x_clear(XUi *xui, int x1, int y1, int x2, int y2);
void __x_resize(XUi *xui, int col, int row)
{
	XFreePixmap(xui->dpy, xui->buf);
	xui->buf = XCreatePixmap(xui->dpy, xui->win, xui->w, xui->h,
			DefaultDepth(xui->dpy, xui->scr));
	XftDrawChange(xui->draw, xui->buf);
	__x_clear(xui, 0, 0, xui->w, xui->h);

	/* resize to new width */
	xui->specbuf = realloc(xui->specbuf, col * sizeof(GlyphFontSpec));
}

void x_cresize(XUi *xui, int width, int height)
{
	int col, row;

	if (width != 0)
		xui->w = width;
	if (height != 0)
		xui->h = height;

	col = (xui->w - 2 * borderpx) / xui->cw;
	row = (xui->h - 2 * borderpx) / xui->ch;
	cols = MAX(1, col);
	rows = MAX(1, row);

	__x_resize(xui, col, row);
}

ushort sixd_to_16bit(int x)
{
	return x == 0 ? 0 : 0x3737 + 0x2828 * x;
}

int x_loadcolor(XUi *xui, int i, const char *name, Color *ncolor)
{
	XRenderColor color = { .alpha = 0xffff };

	if (!name) {
		if (BETWEEN(i, 16, 255)) { /* 256 color */
			if (i < 6*6*6+16) { /* same colors as xterm */
				color.red   = sixd_to_16bit( ((i-16)/36)%6 );
				color.green = sixd_to_16bit( ((i-16)/6) %6 );
				color.blue  = sixd_to_16bit( ((i-16)/1) %6 );
			} else { /* greyscale */
				color.red = 0x0808 + 0x0a0a * (i - (6*6*6+16));
				color.green = color.blue = color.red;
			}
			return XftColorAllocValue(xui->dpy, xui->vis,
			                          xui->cmap, &color, ncolor);
		} else
			name = colorname[i];
	}

	return XftColorAllocName(xui->dpy, xui->vis, xui->cmap, name, ncolor);
}

int x_init_colors(XUi *xui)
{
	int i;
	static int loaded;
	Color *cp;

	if (loaded) {
		for (cp = xui->dc.col; cp < &xui->dc.col[xui->dc.collen]; ++cp)
			XftColorFree(xui->dpy, xui->vis, xui->cmap, cp);
	} else {
		xui->dc.collen = MAX(LEN(colorname), 256);
		xui->dc.col = malloc(xui->dc.collen * sizeof(Color));
		if (!xui->dc.col) {
			fprintf(stderr, "failed to allocate memory for colors\n");
			return -1;
		}
	}

	for (i = 0; i < xui->dc.collen; i++)
		if (!x_loadcolor(xui, i, NULL, &xui->dc.col[i])) {
			if (colorname[i])
				fprintf(stderr, "could not allocate color '%s'\n", colorname[i]);
			else
				fprintf(stderr, "could not allocate color %d\n", i);
			return -1;
		}
	loaded = 1;

	return 0;
}

void x_hints(XUi *xui)
{
	XClassHint class = {opt_name ? opt_name : termname,
	                    opt_class ? opt_class : termname};
	XWMHints wm = {.flags = InputHint, .input = 1};
	XSizeHints *sizeh;

	sizeh = XAllocSizeHints();

	sizeh->flags = PSize | PResizeInc | PBaseSize | PMinSize;
	sizeh->height = xui->h;
	sizeh->width = xui->w;
	sizeh->height_inc = xui->ch;
	sizeh->width_inc = xui->cw;
	sizeh->base_height = 2 * borderpx;
	sizeh->base_width = 2 * borderpx;
	sizeh->min_height = xui->ch + 2 * borderpx;
	sizeh->min_width = xui->cw + 2 * borderpx;
	if (xui->isfixed) {
		sizeh->flags |= PMaxSize;
		sizeh->min_width = sizeh->max_width = xui->w;
		sizeh->min_height = sizeh->max_height = xui->h;
	}

	XSetWMProperties(xui->dpy, xui->win, NULL, NULL, NULL, 0, sizeh, &wm,
			&class);
	XFree(sizeh);
}

void x_settitle(XUi *xui, char *p)
{
	XTextProperty prop;
	DEFAULT(p, opt_title);

	Xutf8TextListToTextProperty(xui->dpy, &p, 1, XUTF8StringStyle,
			&prop);
	XSetWMName(xui->dpy, xui->win, &prop);
	XSetTextProperty(xui->dpy, xui->win, &prop, xui->netwmname);
	XFree(prop.value);
}

void x_finishdraw(XUi *xui)
{
	XCopyArea(xui->dpy, xui->buf, xui->win, xui->dc.gc, 0, 0, xui->w, xui->h, 0, 0);
	XSetForeground(xui->dpy, xui->dc.gc, xui->dc.col[defaultbg].pixel);
}

static UiWin *x_window_new(Ui *ui, View *view)
{
	XWin *xwin;

	xwin = calloc(1, sizeof(*xwin));

	ui_window_text_fg_set(&xwin->win, defaultfg);
	ui_window_text_bg_set(&xwin->win, defaultbg);

	return (UiWin*)xwin;
}

static void x_window_free(UiWin *win)
{
	XWin *xwin = (XWin*)win;

	free(xwin);
}

bool x_handle_events(int fd, void *arg)
{
	XUi *xui = arg;
	KeySym key_sym;
	XEvent ev;
	char text[16];
	bool xev = false;
	
	while (XPending(xui->dpy)) {
		xev = true;
		XNextEvent(xui->dpy, &ev);
		if (XFilterEvent(&ev, None))
			continue;

		if (ev.type == ConfigureNotify) {
			if (ev.xconfigure.width != xui->w || ev.xconfigure.height == xui->h) {
				x_cresize(xui, ev.xconfigure.width, ev.xconfigure.height);
				xui->need_resize = true;
			}
		} else if (ev.type == KeyPress) {
			int len = XLookupString(&ev.xkey, text, sizeof(text), &key_sym, 0);

			if (key_sym == XK_Up) {
				strcpy(text, "\033[A");
				len = strlen(text);
			} else if (key_sym == XK_Down) {
				strcpy(text, "\033[B");
				len = strlen(text);
			} else if (key_sym == XK_Right) {
				strcpy(text, "\033[C");
				len = strlen(text);
			} else if (key_sym == XK_Left) {
				strcpy(text, "\033[D");
				len = strlen(text);
			}

			for (int i = 0; i < len; i++) {
				KeyCode key_code = {};
				Rune code = text[i];
				int flags = 0;

				if (len == 1) {
					if (ev.xkey.state & Mod1Mask) {
						flags |= KEY_MOD_F_ALT;
					} else if (code < 0x1f && code != 0xd && code != 0x9) {
						flags |= KEY_MOD_F_CTL;
						code = code + 0x60;
					}

					switch (key_sym) {
					case XK_BackSpace: flags = 0; code = 0x107; break;
					case XK_Escape: flags = 0; code = 27; break;
					}
				}

				key_code.flags = flags;
				key_code.code = code;

				if (xui->ui.event_handler_cb)
					xui->ui.event_handler_cb(&xui->ui, UiEventType_KeyPress, &key_code);
			}
		}
	}
	return xev;
}

static double x_event_update_timeout(double timeout, void *arg);
static bool x_event_pre_handler(void *arg);

static int x_init(Ui *ui)
{
	XGCValues gcvalues;
	Cursor cursor;
	Window parent;
	XColor xmousefg, xmousebg;
	pid_t thispid = getpid();
	XUi *xui = (XUi *)ui;
	int w, h;
	XEvent ev;
	int err;

	XSetLocaleModifiers("");

	if (!(xui->dpy = XOpenDisplay(NULL))) {
		fprintf(stderr, "can't open display\n");
		return -1;
	}
	xui->scr = XDefaultScreen(xui->dpy);
	xui->vis = XDefaultVisual(xui->dpy, xui->scr);

	/* font */
	if (!FcInit()) {
		fprintf(stderr, "could not init fontconfig.\n");
		return -1;
	}

	usedfont = font;
	err = x_loadfonts(xui, usedfont, 0);
	if (err)
		return -1;

	xui->cmap = XDefaultColormap(xui->dpy, xui->scr);
	err = x_init_colors(xui);
	if (err)
		return -1;

	/* adjust fixed window geometry */
	xui->w = 2 * borderpx + cols * xui->cw;
	xui->h = 2 * borderpx + rows * xui->ch;
	if (xui->gm & XNegative)
		xui->l += DisplayWidth(xui->dpy, xui->scr) - xui->w - 2;
	if (xui->gm & YNegative)
		xui->t += DisplayHeight(xui->dpy, xui->scr) - xui->h - 2;

	/* Events */
	xui->attrs.background_pixel = xui->dc.col[defaultbg].pixel;
	xui->attrs.border_pixel = xui->dc.col[defaultbg].pixel;
	xui->attrs.bit_gravity = NorthWestGravity;
	xui->attrs.event_mask = FocusChangeMask | KeyPressMask | KeyReleaseMask
		| ExposureMask | VisibilityChangeMask | StructureNotifyMask
		| ButtonMotionMask | ButtonPressMask | ButtonReleaseMask;
	xui->attrs.colormap = xui->cmap;

	parent = XRootWindow(xui->dpy, xui->scr);
	xui->win = XCreateWindow(xui->dpy, parent, xui->l, xui->t,
			xui->w, xui->h, 0, XDefaultDepth(xui->dpy, xui->scr), InputOutput,
			xui->vis, CWBackPixel | CWBorderPixel | CWBitGravity
			| CWEventMask | CWColormap, &xui->attrs);

	memset(&gcvalues, 0, sizeof(gcvalues));
	gcvalues.graphics_exposures = False;
	xui->dc.gc = XCreateGC(xui->dpy, parent, GCGraphicsExposures,
			&gcvalues);
	xui->buf = XCreatePixmap(xui->dpy, xui->win, xui->w, xui->h,
			DefaultDepth(xui->dpy, xui->scr));
	XSetForeground(xui->dpy, xui->dc.gc, xui->dc.col[defaultbg].pixel);
	XFillRectangle(xui->dpy, xui->buf, xui->dc.gc, 0, 0, xui->w, xui->h);

	/* font spec buffer */
	xui->specbuf = malloc(cols * sizeof(GlyphFontSpec));

	/* Xft rendering context */
	xui->draw = XftDrawCreate(xui->dpy, xui->buf, xui->vis, xui->cmap);

	/* white cursor, black outline */
	cursor = XCreateFontCursor(xui->dpy, mouseshape);
	XDefineCursor(xui->dpy, xui->win, cursor);

	if (XParseColor(xui->dpy, xui->cmap, colorname[mousefg], &xmousefg) == 0) {
		xmousefg.red   = 0xffff;
		xmousefg.green = 0xffff;
		xmousefg.blue  = 0xffff;
	}

	if (XParseColor(xui->dpy, xui->cmap, colorname[mousebg], &xmousebg) == 0) {
		xmousebg.red   = 0x0000;
		xmousebg.green = 0x0000;
		xmousebg.blue  = 0x0000;
	}

	XRecolorCursor(xui->dpy, cursor, &xmousefg, &xmousebg);

	xui->xembed = XInternAtom(xui->dpy, "_XEMBED", False);
	xui->wmdeletewin = XInternAtom(xui->dpy, "WM_DELETE_WINDOW", False);
	xui->netwmname = XInternAtom(xui->dpy, "_NET_WM_NAME", False);
	xui->netwmiconname = XInternAtom(xui->dpy, "_NET_WM_ICON_NAME", False);
	XSetWMProtocols(xui->dpy, xui->win, &xui->wmdeletewin, 1);

	xui->netwmpid = XInternAtom(xui->dpy, "_NET_WM_PID", False);
	XChangeProperty(xui->dpy, xui->win, xui->netwmpid, XA_CARDINAL, 32,
			PropModeReplace, (uchar *)&thispid, 1);

	xui->mode = MODE_NUMLOCK;
	x_settitle(xui, NULL);
	x_hints(xui);
	XMapWindow(xui->dpy, xui->win);
	XSync(xui->dpy, False);

	/* Waiting for window mapping */
	w = xui->w;
	h = xui->h;
	do {
		XNextEvent(xui->dpy, &ev);
		/*
		 * This XFilterEvent call is required because of XOpenIM. It
		 * does filter out the key event and some client message for
		 * the input method too.
		 */
		if (XFilterEvent(&ev, None))
			continue;
		if (ev.type == ConfigureNotify) {
			w = ev.xconfigure.width;
			h = ev.xconfigure.height;
		}
	} while (ev.type != MapNotify);

	x_cresize(xui, w, h);

	event_update_timeout_cb_set(x_event_update_timeout);
	event_pre_handler_cb_set(x_event_pre_handler);

	event_fd_handler_register(XConnectionNumber(xui->dpy), NULL, NULL);

	return 0;
}

static int x_makeglyphfontspecs(XUi *xui, XftGlyphFontSpec *specs, const Cell *cells, int len, int x, int y)
{
	float winx = borderpx + x * xui->cw, winy = borderpx + y * xui->ch, xp, yp;
	CellAttr attr, prevattr = ~0;
	Font *font = &xui->dc.font;
	int frcflags = FRC_NORMAL;
	float runewidth = xui->cw;
	Rune rune;
	FT_UInt glyphidx;
	FcResult fcres;
	FcPattern *fcpattern, *fontpattern;
	FcFontSet *fcsets[] = { NULL };
	FcCharSet *fccharset;
	int i, f, numspecs = 0;

	for (i = 0, xp = winx, yp = winy + font->ascent; i < len; ++i) {
		/* Fetch rune and mode for current glyph. */
		utf8decode(cells[i].data, &rune, cells[i].len);
		
		attr = (ulong)cells[i].style.attr;

		/* Skip dummy wide-character spacing. */
		if (attr == 0)
			continue;

		/* Determine font for glyph if different from previous glyph. */
		if (prevattr != attr) {
			prevattr = attr;
			font = &xui->dc.font;
			frcflags = FRC_NORMAL;
			runewidth = xui->cw * (/*(mode & ATTR_WIDE) ? 2.0f :*/ 1.0f);
			if ((attr & UI_TEXT_STYLE_ITALIC) && (attr & UI_TEXT_STYLE_BOLD)) {
				font = &xui->dc.ibfont;
				frcflags = FRC_ITALICBOLD;
			} else if (attr & UI_TEXT_STYLE_ITALIC) {
				font = &xui->dc.ifont;
				frcflags = FRC_ITALIC;
			} else if (attr & UI_TEXT_STYLE_BOLD) {
				font = &xui->dc.bfont;
				frcflags = FRC_BOLD;
			}
			yp = winy + font->ascent;
		}

		/* Lookup character index with default font. */
		glyphidx = XftCharIndex(xui->dpy, font->match, rune);
		if (glyphidx) {
			specs[numspecs].font = font->match;
			specs[numspecs].glyph = glyphidx;
			specs[numspecs].x = (short)xp;
			specs[numspecs].y = (short)yp;
			xp += runewidth;
			numspecs++;
			continue;
		}

		/* Fallback on font cache, search the font cache for match. */
		for (f = 0; f < frclen; f++) {
			glyphidx = XftCharIndex(xui->dpy, frc[f].font, rune);
			/* Everything correct. */
			if (glyphidx && frc[f].flags == frcflags)
				break;
			/* We got a default font for a not found glyph. */
			if (!glyphidx && frc[f].flags == frcflags
					&& frc[f].unicodep == rune) {
				break;
			}
		}

		/* Nothing was found. Use fontconfig to find matching font. */
		if (f >= frclen) {
			if (!font->set)
				font->set = FcFontSort(0, font->pattern,
				                       1, 0, &fcres);
			fcsets[0] = font->set;

			/*
			 * Nothing was found in the cache. Now use
			 * some dozen of Fontconfig calls to get the
			 * font for one single character.
			 *
			 * Xft and fontconfig are design failures.
			 */
			fcpattern = FcPatternDuplicate(font->pattern);
			fccharset = FcCharSetCreate();

			FcCharSetAddChar(fccharset, rune);
			FcPatternAddCharSet(fcpattern, FC_CHARSET,
					fccharset);
			FcPatternAddBool(fcpattern, FC_SCALABLE, 1);

			FcConfigSubstitute(0, fcpattern,
					FcMatchPattern);
			FcDefaultSubstitute(fcpattern);

			fontpattern = FcFontSetMatch(0, fcsets, 1,
					fcpattern, &fcres);

			/* Allocate memory for the new cache entry. */
			if (frclen >= frccap) {
				frccap += 16;
				frc = realloc(frc, frccap * sizeof(Fontcache));
				if (!frc) {
					fprintf(stderr, "failed to alloc font cache entry\n");
					exit(1);
				}
			}

			frc[frclen].font = XftFontOpenPattern(xui->dpy, fontpattern);
			if (!frc[frclen].font) {
				fprintf(stderr, "failed to seeking fallback font %s\n", strerror(errno));
				exit(1);
			}
			frc[frclen].flags = frcflags;
			frc[frclen].unicodep = rune;

			glyphidx = XftCharIndex(xui->dpy, frc[frclen].font, rune);

			f = frclen;
			frclen++;

			FcPatternDestroy(fcpattern);
			FcCharSetDestroy(fccharset);
		}

		specs[numspecs].font = frc[f].font;
		specs[numspecs].glyph = glyphidx;
		specs[numspecs].x = (short)xp;
		specs[numspecs].y = (short)yp;
		xp += runewidth;
		numspecs++;
	}

	return numspecs;
}

/*
 * Absolute coordinates.
 */
void __x_clear(XUi *xui, int x1, int y1, int x2, int y2)
{
	XftDrawRect(xui->draw, &xui->dc.col[defaultbg], x1, y1, x2-x1, y2-y1);
}

void x_drawglyphfontspecs(XUi *xui, const XftGlyphFontSpec *specs, Cell base, int len, int x, int y)
{
	int charlen = len * (/*(base.mode & ATTR_WIDE) ? 2 :*/ 1);
	int winx = borderpx + x * xui->cw, winy = borderpx + y * xui->ch,
	    width = charlen * xui->cw;
	Color *fg, *bg, *temp, revfg, revbg, truefg, truebg;
	XRenderColor colfg, colbg;
	CellAttr attr = base.style.attr;
	XRectangle r;

	if (base.style.fg == UI_TEXT_COLOR_DEFAULT)
		base.style.fg = defaultfg;
	if (base.style.bg == UI_TEXT_COLOR_DEFAULT)
		base.style.bg = defaultbg;

	/* Fallback on color display for attributes not supported by the font */
	if (attr & UI_TEXT_STYLE_ITALIC && attr & UI_TEXT_STYLE_BOLD) {
		if (xui->dc.ibfont.badslant || xui->dc.ibfont.badweight)
			base.style.fg = defaultfg;
	} else if ((attr & UI_TEXT_STYLE_ITALIC && xui->dc.ifont.badslant) ||
	    (attr & UI_TEXT_STYLE_BOLD && xui->dc.bfont.badweight)) {
		base.style.fg = defaultfg;
	}

	if (IS_TRUECOL(base.style.fg)) {
		colfg.alpha = 0xffff;
		colfg.red = TRUERED(base.style.fg);
		colfg.green = TRUEGREEN(base.style.fg);
		colfg.blue = TRUEBLUE(base.style.fg);
		XftColorAllocValue(xui->dpy, xui->vis, xui->cmap, &colfg, &truefg);
		fg = &truefg;
	} else {
		fg = &xui->dc.col[base.style.fg];
	}

	if (IS_TRUECOL(base.style.bg)) {
		colbg.alpha = 0xffff;
		colbg.green = TRUEGREEN(base.style.bg);
		colbg.red = TRUERED(base.style.bg);
		colbg.blue = TRUEBLUE(base.style.bg);
		XftColorAllocValue(xui->dpy, xui->vis, xui->cmap, &colbg, &truebg);
		bg = &truebg;
	} else {
		bg = &xui->dc.col[base.style.bg];
	}

	/* Intelligent cleaning up of the borders. */
	if (x == 0) {
		__x_clear(xui, 0, (y == 0)? 0 : winy, borderpx,
			winy + xui->ch +
			((winy + xui->ch >= borderpx + xui->h)? xui->h : 0));
	}
	if (winx + width >= borderpx + xui->w) {
		__x_clear(xui, winx + width, (y == 0)? 0 : winy, xui->w,
			((winy + xui->ch >= borderpx + xui->h)? xui->h : (winy + xui->ch)));
	}
	if (y == 0)
		__x_clear(xui, winx, 0, winx + width, borderpx);
	if (winy + xui->ch >= borderpx + xui->h)
		__x_clear(xui, winx, winy + xui->ch, winx + width, xui->h);

	/* Clean up the region we want to draw to. */
	XftDrawRect(xui->draw, bg, winx, winy, width, xui->ch);

	/* Set the clip region because Xft is sometimes dirty. */
	r.x = 0;
	r.y = 0;
	r.height = xui->ch;
	r.width = width;
	XftDrawSetClipRectangles(xui->draw, winx, winy, &r, 1);

	/* Render the glyphs. */
	XftDrawGlyphFontSpec(xui->draw, fg, specs, len);

	/* Render underline and strikethrough. */
	if (attr & UI_TEXT_STYLE_UNDERLINE) {
		XftDrawRect(xui->draw, fg, winx, winy + xui->dc.font.ascent + 1,
				width, 1);
	}

	#if 0
	if (attr & ATTR_STRUCK) {
		XftDrawRect(xui->draw, fg, winx, winy + 2 * xui->dc.font.ascent / 3,
				width, 1);
	}
	#endif

	/* Reset clip to none. */
	XftDrawSetClip(xui->draw, 0);
}

void x_drawglyph(XUi *xui, Cell c, int x, int y)
{
	int numspecs;
	XftGlyphFontSpec spec;

	numspecs = x_makeglyphfontspecs(xui, &spec, &c, 1, x, y);
	x_drawglyphfontspecs(xui, &spec, c, numspecs, x, y);
}

void xdrawline(XUi *xui, Line *line, int x1, int y1, int x2)
{
	int i, x, ox, numspecs;
	Cell base, new;
	XftGlyphFontSpec *specs = xui->specbuf;

	numspecs = x_makeglyphfontspecs(xui, specs, &line->cells[x1], x2 - x1, x1, y1);
	i = ox = 0;
	for (x = x1; x < x2 && i < numspecs; x++) {
		new = line->cells[x];
		if (new.style.attr == 0)
			continue;
		#if 0
		if (selected(x, y1))
			new.style.attr ^= ATTR_REVERSE;
		#endif
		if (i > 0 && ATTRCMP(base, new)) {
			x_drawglyphfontspecs(xui, specs, base, i, ox, y1);
			specs += i;
			numspecs -= i;
			i = 0;
		}
		if (i == 0) {
			ox = x;
			base = new;
		}
		i++;
	}
	if (i > 0)
		x_drawglyphfontspecs(xui, specs, base, i, ox, y1);
}

static void x_free(Ui *ui)
{
	free(ui);
}

static int x_height_get(Ui *ui)
{
	return rows;
}

static int x_width_get(Ui *ui)
{
	return cols;
}

static bool x_check_resize(Ui *ui)
{
	XUi *xui = (XUi *)ui;
	bool need_resize = xui->need_resize;

	xui->need_resize = false;
	return need_resize;
}

static void x_draw_wchar(Ui *ui, int x, int y, wchar_t ch, short fg, short bg, ui_text_style_t style)
{
	XUi *xui = (XUi *)ui;
	Cell c = {};

	c.style.attr = style;
	c.style.fg = fg;
	c.style.bg = bg;
	c.len = utf8encode(ch, c.data);

	x_drawglyph(xui, c, x, y);
}

static void x_draw_cell(Ui *ui, int x, int y, Cell *c)
{
	XUi *xui = (XUi *)ui;

	x_drawglyph(xui, *c, x, y);
}

static void x_clear(Ui *ui)
{
	XUi *xui = (XUi *)ui;
	__x_clear(xui, 0, 0, xui->w, xui->h);
}

static void x_update(Ui *ui)
{
	XUi *xui = (XUi *)ui;
	x_finishdraw(xui);
}

static double x_event_update_timeout(double timeout, void *arg)
{
	XUi *xui = (XUi *)arg;

	if (XPending(xui->dpy))
		return 0;  /* existing events might not set xfd */
	return timeout;
}

static bool x_event_pre_handler(void *arg)
{
	XUi *xui = (XUi *)arg;

	return x_handle_events(0, xui);
}

static void x_event_process(Ui *ui)
{
	XUi *xui = (XUi *)ui;

	ui_update(ui);
	event_process(xui);
	XFlush(xui->dpy);
}

Ui *ui_x_new(void)
{
	XUi *xui;

	xui = calloc(1, sizeof(*xui));

	xui->ui.init = x_init;
	xui->ui.free = x_free;
	xui->ui.height_get = x_height_get;
	xui->ui.width_get = x_width_get;
	xui->ui.resize = x_check_resize;
	xui->ui.clear = x_clear;
	xui->ui.update = x_update;
	xui->ui.event_process = x_event_process;
	xui->ui.draw_wchar = x_draw_wchar;
	xui->ui.draw_cell = x_draw_cell;
	xui->ui.window_new = x_window_new;
	xui->ui.window_free = x_window_free;

	return (Ui *)xui;
}
