/* GIMP - The GNU Image Manipulation Program
 * Copyright (C) 1995 Spencer Kimball and Peter Mattis
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */


#include "gimpressionist.h"
#include <stdio.h>
#include <math.h>
REPAINTCALLBACK_FUN repaintCallBack = 0;
int tmpWidth = 0;
int tmpHeight = 0;
/*
 * The default values for the application, to be initialized at startup.
 * */
static gimpressionist_vals_t defaultpcvals = {
  4,
  0.0,
  60.0,
  0,
  12.0,
  20.0,
  20.0,
  1.0,
  1,
  0.1,
  0.0,
  30.0,
  0,
  0,
  "defaultbrush.pgm",
  "defaultpaper.pgm",
  {0,0,0,1.0},
  1,
  0,
  { { 0.5, 0.5, 0.0, 0.0, 1.0, 1.0, 0 } },
  1,
  0,
  0.0,
  0.0,
  1.0,
  0,
  0,
  0,
  0,
  0,
  20.0,
  1,
  10.0,
  20.0,
  0,
  0.1,

  { { 0.5, 0.5, 50.0, 1.0 } },
  1,
  1.0,
  0,

  10,
  4,

  0, 0.0
};
gimpressionist_vals_t  pcvals;
void setDefaultPcvals()
{
	pcvals = defaultpcvals;
}
double dist (double x, double y, double end_x, double end_y)
{
  double dx = end_x - x;
  double dy = end_y - y;
  return sqrt (dx * dx + dy * dy);
}
double
getsiz_proto (double x, double y, int n, smvector_t *vec,
              double smstrexp, int voronoi)
{
  int    i;
  double sum, ssum, dst;
  int    first = 0, last;

  if ((x < 0.0) || (x > 1.0))
    g_warning ("HUH? x = %f\n",x);

#if 0
  if (from == 0)
    {
      n = numsmvect;
      vec = smvector;
      smstrexp = gtk_adjustment_get_value (GTK_ADJUSTMENT (smstrexpadjust));
      voronoi = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (size_voronoi));
    }
  else
    {
      n = pcvals.num_size_vectors;
      vec = pcvals.size_vectors;
      smstrexp = pcvals.size_strength_exponent;
      voronoi = pcvals.size_voronoi;
    }
#endif

  if (voronoi)
    {
      gdouble bestdist = -1.0;
      for (i = 0; i < n; i++)
         {
           dst = dist (x, y, vec[i].x, vec[i].y);
           if ((bestdist < 0.0) || (dst < bestdist))
             {
               bestdist = dst;
               first = i;
             }
         }
      last = first+1;
    }
  else
    {
      first = 0;
      last = n;
    }

  sum = ssum = 0.0;
  for (i = first; i < last; i++)
    {
      gdouble s = vec[i].str;

      dst = dist (x,y,vec[i].x,vec[i].y);
      dst = pow (dst, smstrexp);
      if (dst < 0.0001)
        dst = 0.0001;
      s = s / dst;

      sum += vec[i].siz * s;
      ssum += 1.0/dst;
  }
  sum = sum / ssum / 100.0;
  return CLAMP (sum, 0.0, 1.0);
}
#define P_VAL(item, fmt) g_printerr(#item "= %" #fmt "\n", val->item)
void print_val(gimpressionist_vals_t* val)
{
    P_VAL(orient_num, d);
    P_VAL(orient_first, f);
    P_VAL(orient_last, f);
    P_VAL(orient_type, d);
    P_VAL(brush_relief, f);
    P_VAL(brush_scale, f);
    P_VAL(brush_density, f);
    P_VAL(brushgamma, f);
    P_VAL(general_background_type, d);
    P_VAL(general_dark_edge, f);
    P_VAL(paper_relief, f);
    P_VAL(paper_scale, f);
    P_VAL(paper_invert, d);
    P_VAL(run, d);
    P_VAL(selected_brush, s);
    P_VAL(selected_paper, s);
    P_VAL(general_paint_edges, d);
    P_VAL(place_type, d);
    P_VAL(num_orient_vectors, d);
    P_VAL(placement_center, d);
    P_VAL(brush_aspect, f);
    P_VAL(orient_angle_offset, f);
    P_VAL(orient_strength_exponent, f);
    P_VAL(general_tileable, d);
    P_VAL(paper_overlay, d);
    P_VAL(orient_voronoi, d);
    P_VAL(color_brushes, d);
    P_VAL(general_drop_shadow, d);
    P_VAL(general_shadow_darkness, f);
    P_VAL(size_num, d);
    P_VAL(size_first, f);
    P_VAL(size_last, f);
    P_VAL(size_type, d);
    P_VAL(devthresh, f);

    P_VAL(num_size_vectors, d);
    P_VAL(size_strength_exponent, f);
    P_VAL(size_voronoi, d);

    P_VAL(general_shadow_depth, d);
    P_VAL(general_shadow_blur, d);

    P_VAL(color_type, d);
    P_VAL(color_noise, f); 
}