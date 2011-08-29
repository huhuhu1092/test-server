#include "gimpressionist.h"
#include "random.h"
#include "imageloader.h"
#include "ppmtool.h"
#include <stdio.h>
#include <string>
#include <math.h>
static char filename[] = "c:\\paintbrush.jpg";
static ppm_t         infile =  {0, 0, NULL};
void grabarea (Image drawable)
{
  Image  src_rgn;
  ppm_t        *p;
  gint          x1, y1, x2, y2;
  gint          x, y;
  gint          width, height;
  gint          row, col;
  gint          rowstride;

  //gimp_drawable_mask_bounds (drawable->drawable_id, &x1, &y1, &x2, &y2);
  x1 = drawable.x;
  y1 = drawable.y;
  x2 = x1 + drawable.width;
  y2= y1 + drawable.height;
  width  = x2 - x1;
  height = y2 - y1;

  ppm_new (&infile, width, height);
  p = &infile;

  rowstride = p->width * 3;

  src_rgn.x = x1;
  src_rgn.y = y1;
  src_rgn.width = width;
  src_rgn.height = height;
  src_rgn.bpp = drawable.bpp;
  src_rgn.data = drawable.data;
  src_rgn.rowstride = drawable.rowstride;
  const guchar *src = src_rgn.data;

  switch (src_rgn.bpp)
    {
    case 1:
      for (y = 0, row = src_rgn.y - y1; y < src_rgn.height; y++, row++)
        {
          const guchar *s      = src;
          guchar       *tmprow = p->col + row * rowstride;

          for (x = 0, col = src_rgn.x - x1; x < src_rgn.width; x++, col++)
            {
              gint k = col * 3;

              tmprow[k + 0] = s[0];
              tmprow[k + 1] = s[0];
              tmprow[k + 2] = s[0];

              s++;
            }

          src += src_rgn.rowstride;
        }
      break;

    case 2:
      for (y = 0, row = src_rgn.y - y1; y < src_rgn.height; y++, row++)
        {
          const guchar *s       = src;
          guchar       *tmprow  = p->col + row * rowstride;

          for (x = 0, col = src_rgn.x - x1; x < src_rgn.width; x++, col++)
            {
              gint k = col * 3;

              tmprow[k + 0] = s[0];
              tmprow[k + 1] = s[0];
              tmprow[k + 2] = s[0];

              s += 2;
            }

          src += src_rgn.rowstride;
        }
      break;

    case 3:
      col = src_rgn.x - x1;

      for (y = 0, row = (src_rgn.y - y1); y < src_rgn.height; y++, row++)
        {
          memcpy (p->col + row * rowstride + col * 3, src, src_rgn.width * 3);

          src += src_rgn.rowstride;
        }
      break;

    case 4:
      for (y = 0, row = src_rgn.y - y1; y < src_rgn.height; y++, row++)
        {
          const guchar *s       = src;
          guchar       *tmprow  = p->col + row * rowstride;
          for (x = 0, col = src_rgn.x - x1; x < src_rgn.width; x++, col++)
            {
              gint k = col * 3;

              tmprow[k + 0] = s[0];
              tmprow[k + 1] = s[1];
              tmprow[k + 2] = s[2];

              s += 4;
            }

          src += src_rgn.rowstride;
        }
      break;
	default:
		break;
    }
}
static void setppm()
{
	std::string brush = std::string("c:\\backup\\Brushes\\arrow01.pgm");
	std::string paper = std::string("c:\\backup\\Paper\\bricks.pgm");
	memset(pcvals.selected_brush, 0, sizeof(pcvals.selected_brush));
	strncpy(pcvals.selected_brush, brush.c_str(), sizeof(pcvals.selected_brush) - 1);
	memset(pcvals.selected_paper, 0 , sizeof(pcvals.selected_paper));
	strncpy(pcvals.selected_paper, paper.c_str(), sizeof(pcvals.selected_paper)  -1);
}
static void init()
{
	setDefaultPcvals();
	setppm();
	pcvals.size_first = 47;
	pcvals.size_last = 151;
	pcvals.size_num = 8;
	pcvals.size_type = 0;
	pcvals.orient_num = 6;
	pcvals.orient_first = 35;
	pcvals.orient_last = 96;
	pcvals.orient_type = 4;
	//pcvals.general_background_type = BG_TYPE_KEEP_ORIGINAL;
}
int main(int argc , char** argv)
{
	double nx = fabs ((double)(0 - 191 / 2.0));
	random_generator = g_rand_new ();
	init();
    Image image = load(filename);
    grabarea(image);
	repaint(&infile, NULL);
	Image outImage;
	outImage.bpp = 3;
	outImage.width = infile.width;
	outImage.height = infile.height;
	outImage.data = infile.col;
	save(outImage, "c:\\testoutput.jpg");
	//save(image, "c:\\testoutput.png");
	getchar();
	return 0;
}