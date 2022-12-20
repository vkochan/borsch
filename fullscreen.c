static void fullscreen(unsigned int wax, unsigned int way, unsigned int waw, unsigned int wah)
{
	for (Window *c = nextvisible(windows_list()); c; c = nextvisible(c->next))
		resize(c, wax, way, waw, wah);
}
