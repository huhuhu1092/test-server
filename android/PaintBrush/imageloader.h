#ifndef IMAGELOADER_H
#define IMAGELOADER_H
typedef struct Image
{
	int x;
	int y;
	int width;
	int height;
	int bpp; //bytes per pixel
	int rowstride;
    unsigned char* data;
	Image()
	{
		x = y = width = height = bpp = rowstride = 0;
		data = 0;
	}
};
extern Image load(const char* filename);
extern void save(Image image, const char* filename);
#endif