static void tile(void)
{
	unsigned int lax = wax, lay = way-1, law = waw, lah = wah;
	unsigned int i, n, nx, ny, nw, nh, m, mw, mh, th;
	Client *c;

	for (n = 0, c = nextvisible(clients); c; c = nextvisible(c->next))
		if (!c->minimized)
			n++;

	m  = MAX(1, MIN(n, getnmaster()));
	mw = n == m ? waw : getmfact() * waw;
	mh = wah / m;
	th = n == m ? 0 : wah / (n - m);
	nx = lax;
	ny = lay;

	for (i = 0, c = nextvisible(clients); c; c = nextvisible(c->next)) {
		if (c->minimized)
			continue;
		if (i < m) {	/* master */
			nw = mw;
			nh = (i < m - 1) ? mh : (lay + wah) - ny;
		} else {	/* tile window */
			if (i == m) {
				ny = lay;
				nx += mw;
				ui_draw_char_vert(ui, nx, ny, ACS_VLINE, wah);
				ui_draw_char(ui, nx, ny, ACS_TTEE, 1);
				nx++;
				nw = waw - mw -1;
			}
			nh = (i < n - 1) ? th : (lay + wah) - ny;
			if (i > m)
				ui_draw_char(ui, nx - 1, ny, ACS_LTEE, 1);
		}
		resize(c, nx, ny+(way-lay), nw, nh);
		ny += nh;
		i++;
	}

	/* Fill in nmaster intersections */
	if (n > m) {
		ny = lay + mh;
		for (i = 1; i < m; i++) {
			ui_draw_char(ui, nx - 1, ny, ((ny - 1) % th ? ACS_RTEE : ACS_PLUS), 1);
			ny += mh;
		}
	}
}
