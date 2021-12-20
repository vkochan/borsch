static void fullscreen(void)
{
	for (Window *c = nextvisible(windows); c; c = nextvisible(c->next))
		resize(c, wax, way, waw, wah);
}
