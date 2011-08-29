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

#ifndef __GIMPRESSIONIST_H
#define __GIMPRESSIONIST_H

#include "type.h"
#include "ppmtool.h"
#ifdef __cplusplus
extern "C" {
#endif
/* Defines */

#define PLUG_IN_PROC    "plug-in-gimpressionist"
#define PLUG_IN_VERSION "v1.0, November 2003"
#define PLUG_IN_BINARY  "gimpressionist"

#define PREVIEWSIZE     150
#define MAXORIENTVECT   50
#define MAXSIZEVECT     50

/* Type declaration and definitions */

typedef struct vector
{
  double x, y;
  double dir;
  double dx, dy;
  double str;
  int    type;
} vector_t;

typedef struct smvector
{
  double x, y;
  double siz;
  double str;
} smvector_t;

typedef struct
{
  int        orient_num;
  double     orient_first;
  double     orient_last;
  int        orient_type;
  double     brush_relief;
  double     brush_scale;
  double     brush_density;
  double     brushgamma;
  int        general_background_type;
  double     general_dark_edge;
  double     paper_relief;
  double     paper_scale;
  int        paper_invert;
  int        run;
  char       selected_brush[200];
  char       selected_paper[200];
  GimpRGB    color;
  int        general_paint_edges;
  int        place_type;
  vector_t   orient_vectors[MAXORIENTVECT];
  int        num_orient_vectors;
  int        placement_center;
  double     brush_aspect;
  double     orient_angle_offset;
  double     orient_strength_exponent;
  int        general_tileable;
  int        paper_overlay;
  int        orient_voronoi;
  int        color_brushes;
  int        general_drop_shadow;
  double     general_shadow_darkness;
  int        size_num;
  double     size_first;
  double     size_last;
  int        size_type;
  double     devthresh;

  smvector_t size_vectors[MAXSIZEVECT];
  int        num_size_vectors;
  double     size_strength_exponent;
  int        size_voronoi;

  int        general_shadow_depth;
  int        general_shadow_blur;

  int        color_type;
  double     color_noise;
} gimpressionist_vals_t;

/* Enumerations */

enum GENERAL_BG_TYPE_ENUM
{
    BG_TYPE_SOLID = 0,
    BG_TYPE_KEEP_ORIGINAL = 1,
    BG_TYPE_FROM_PAPER = 2,
    BG_TYPE_TRANSPARENT = 3,
};

enum PRESETS_LIST_COLUMN_ENUM
{
  PRESETS_LIST_COLUMN_FILENAME = 0,
  PRESETS_LIST_COLUMN_OBJECT_NAME = 1,
};
enum PLACEMENT_TYPE_ENUM
{
    PLACEMENT_TYPE_RANDOM = 0,
    PLACEMENT_TYPE_EVEN_DIST = 1,
};
enum SIZE_TYPE_ENUM
{
    SIZE_TYPE_VALUE = 0,
    SIZE_TYPE_RADIUS = 1,
    SIZE_TYPE_RANDOM = 2,
    SIZE_TYPE_RADIAL = 3,
    SIZE_TYPE_FLOWING = 4,
    SIZE_TYPE_HUE = 5,
    SIZE_TYPE_ADAPTIVE = 6,
    SIZE_TYPE_MANUAL = 7,
};
enum ORIENTATION_ENUM
{
    ORIENTATION_VALUE = 0,
    ORIENTATION_RADIUS = 1,
    ORIENTATION_RANDOM = 2,
    ORIENTATION_RADIAL = 3,
    ORIENTATION_FLOWING = 4,
    ORIENTATION_HUE = 5,
    ORIENTATION_ADAPTIVE = 6,
    ORIENTATION_MANUAL = 7,
};
/*
 * x == -2 && y == -2 indicate the list is at end
 * x == -1 && y == -1 indicate there has no brush piece in list
 * */
typedef struct
{
    ppm_t data;
    int x;
    int y;
} BrushPiece;
/* Globals */

extern gimpressionist_vals_t pcvals;
double getsiz_proto (double x, double y, int n, smvector_t *vec,
                     double smstrexp, int voronoi);
extern double dist (double x, double y, double end_x, double end_y);
extern void repaint (ppm_t *p, ppm_t *a);
extern void setDefaultPcvals();
extern void print_val(gimpressionist_vals_t* val);
extern BrushPiece getNextBrushPiece();
extern void addBrushPiece(const BrushPiece& bp);
extern int isRepaintEnd();
#ifdef __cplusplus
}
#endif
#endif /* #ifndef __GIMPRESSIONIST_H */

