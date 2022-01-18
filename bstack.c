static void bstack(unsigned int wax, unsigned int way, unsigned int waw, unsigned int wah)
{
	unsigned int lax = wax, lay = way-1, law = waw, lah = wah;
	unsigned int i, n, nx, ny, nw, nh, m, mw, mh, tw;

	Window *c;

	for (n = 0, c = nextvisible(windows); c; c = nextvisible(c->next))
		if (!c->minimized)
			n++;

	m  = MAX(1, MIN(n, getnmaster()));
	mh = n == m ? lah : getmfact() * lah;
	mw = law / m;
	tw = n == m ? 0 : law / (n - m);
	nx = lax;
	ny = lay;

	for (i = 0, c = nextvisible(windows); c; c = nextvisible(c->next)) {
		if (c->minimized)
			continue;
		if (i < m) {	/* master */
			if (i > 0) {
				ui_draw_char_vert(ui, nx, ny, ACS_VLINE, nh);
				ui_draw_char(ui, nx, ny, ACS_TTEE, 1);
				nx++;
			}
			nh = mh;
			nw = (i < m - 1) ? mw : (lax + law) - nx;
		} else {	/* tile window */
			if (i == m) {
				nx = lax;
				ny += mh;
				nh = (lay + lah) - ny;
			}
			if (i > m) {
				ui_draw_char_vert(ui, nx, ny, ACS_VLINE, nh);
				ui_draw_char(ui, nx, ny, ACS_TTEE, 1);
				nx++;
			}
			nw = (i < n - 1) ? tw : (lax + law) - nx;
		}
		resize(c, nx, ny+(way-lay), nw, nh);
		nx += nw;
		i++;
	}

	/* Fill in nmaster intersections */
	if (n > m) {
		nx = lax;
		for (i = 0; i < m; i++) {
			if (i > 0) {
				ui_draw_char(ui, nx, ny, ACS_PLUS, 1);
				nx++;
			}
			nw = (i < m - 1) ? mw : (lax + law) - nx;
			nx += nw;
		}
	}
}
