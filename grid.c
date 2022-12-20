static void grid(unsigned int wax, unsigned int way, unsigned int waw, unsigned int wah)
{
	unsigned int lax = wax, lay = way-1, law = waw, lah = wah;
	unsigned int i, n, nx, ny, nw, nh, aw, ah, cols, rows;

	Window *c;

	for (n = 0, c = nextvisible(windows_list()); c; c = nextvisible(c->next))
		if (!c->minimized)
			n++;
	/* grid dimensions */
	for (cols = 0; cols <= n / 2; cols++)
		if (cols * cols >= n)
			break;
	rows = (cols && (cols - 1) * cols >= n) ? cols - 1 : cols;
	/* window geoms (cell height/width) */
	nh = lah / (rows ? rows : 1);
	nw = law / (cols ? cols : 1);
	for (i = 0, c = nextvisible(windows_list()); c; c = nextvisible(c->next)) {
		if (c->minimized)
			continue;
		/* if there are less windows in the last row than normal adjust the
		 * split rate to fill the empty space */
		if (rows > 1 && i == (rows * cols) - cols && (n - i) <= (n % cols))
			nw = law / (n - i);
		nx = (i % cols) * nw + lax;
		ny = (i / cols) * nh + lay;
		/* adjust height/width of last row/column's windows */
		ah = (i >= cols * (rows - 1)) ? lah - nh * rows : 0;
		/* special case if there are less windows in the last row */
		if (rows > 1 && i == n - 1 && (n - i) < (n % cols))
			/* (n % cols) == number of windows in the last row */
			aw = law - nw * (n % cols);
		else
			aw = ((i + 1) % cols == 0) ? law - nw * cols : 0;
		if (i % cols) {
			ui_draw_char_vert(ui, nx, ny, ACS_VLINE, nh + ah);
			/* if we are on the first row, or on the last one and there are fewer windows
			 * than normal whose border does not match the line above, print a top tree char
			 * otherwise a plus sign. */
			if (i <= cols
			    || (i >= rows * cols - cols && n % cols
				&& (cols - (n % cols)) % 2))
				ui_draw_char(ui, nx, ny, ACS_TTEE, 1);
			else
				ui_draw_char(ui, nx, ny, ACS_PLUS, 1);
			nx++, aw--;
		}
		resize(c, nx, ny+(way-lay), nw + aw, nh + ah);
		i++;
	}
}
