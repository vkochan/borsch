static void fullscreen(unsigned int wax, unsigned int way, unsigned int waw, unsigned int wah)
{
	for (Window *c = windows_list(); c; c = c->next)
		resize(c, wax, way, waw, wah);
}
