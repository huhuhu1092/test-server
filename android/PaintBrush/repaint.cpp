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
#include <stdlib.h>
#include <string.h>
#include <list>
#include <math.h>
#include "gimpressionist.h"
#include "ppmtool.h"
#include "random.h"
#include "SE_Mutex.h"
#include "assert.h"
#include <algorithm>
#ifdef WIN32
#include "imageloader.h"
#endif
#ifdef MACOS
#include "PGMDataReader.h"
#endif
#ifdef WIN32
#else
//extern "C"
//{
#include <sys/time.h>
//}
#endif
REPAINTCALLBACK_FUN repaintCallBack = 0;
int tmpWidth = 0;
int tmpHeight = 0;
int gBrushMaxWidth = 0;
int gBrushMaxHeight = 0;
int gImageWidth = 0;
int gImageHeight = 0;
double gRunningTime = 0;
#ifdef WIN32
#else
struct timeval gStartTime;
#endif
ppm_t gBackground = {0, 0, NULL};
ppm_t gBackgroundBack = {0, 0, NULL};
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
    0.001,
    
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
double getsiz_proto (double x, double y, int n, smvector_t *vec,
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
void changeBackground()
{
    ppm_copy(&gBackgroundBack, &gBackground);
}
static void clearBackground()
{
    ppm_kill(&gBackground);
    ppm_kill(&gBackgroundBack);
    gBrushMaxWidth = 0;
    gBrushMaxHeight = 0;
    gImageWidth = 0;
    gImageHeight = 0;
    tmpWidth = 0;
    tmpHeight = 0;
}
static ppm_t createBackground(gimpressionist_vals_t runningvals, int width, int height)
{
    int x, y;
    ppm_t tmp;
    ppm_t paper_ppm;
    float scale = runningvals.paper_scale / 100.0;
    ppm_new (&tmp, width, height);
    ppm_load (runningvals.selected_paper, &paper_ppm);
    resize (&paper_ppm, paper_ppm.width * scale, paper_ppm.height * scale);
    if (runningvals.paper_invert)
        ppm_apply_gamma (&paper_ppm, -1.0, 1, 1, 1);
    for (x = 0; x < tmp.width; x++)
    {
        int rx = x % paper_ppm.width;
        
        for (y = 0; y < tmp.height; y++)
        {
            int ry = y % paper_ppm.height;
            memcpy (&tmp.col[y * tmp.width * 3 + x * 3],
                    &paper_ppm.col[ry*paper_ppm.width*3+rx*3],
                    3);
        }
    }
    ppm_kill(&paper_ppm);
    return tmp;
}
void startTime()
{
#ifdef WIN32
#else
    gettimeofday(&gStartTime, NULL);
    gRunningTime = 0;
#endif
}
void endTime()
{
#ifdef WIN32
#else
    struct timeval endTime;
    int endms, startms;
    gettimeofday(&endTime, NULL);
    endms = endTime.tv_sec * 1000 + endTime.tv_usec / 1000;
    startms = gStartTime.tv_sec * 1000 + endTime.tv_usec / 1000;
    gRunningTime = (endms - startms) / 1000.0f;
#endif
}
double getTime()
{
    return gRunningTime;
}
void clearTime()
{
    gRunningTime = 0;
}

/////////////////////////////////////////////////
static gboolean img_has_alpha = 0;
static gint brush_from_file = 2;
static ppm_t brushppm  = {0, 0, NULL};

static gimpressionist_vals_t runningvals;
static std::list<BrushPiece> gBrushPieceList;
static SE_Mutex gBrushPieceMutex;
static volatile int isRepaintEnd = 0;
static SE_Mutex gIsRepaintEnd;
static volatile int gBrushPaintRunning = 1;
static SE_Mutex gBrushPaintRunningMutex;
void terminateBrushPaint()
{
    gBrushPaintRunningMutex.lock();
    gBrushPaintRunning = 0;
    gBrushPaintRunningMutex.unlock();
}
static int isBrushPaint()
{
    int ret = 0;
    gBrushPaintRunningMutex.lock();
    ret = gBrushPaintRunning ;
    gBrushPaintRunningMutex.unlock();
    return ret;
}
void startBrushPaint()
{
    gBrushPaintRunningMutex.lock();
    gBrushPaintRunning = 1;
    gBrushPaintRunningMutex.unlock();
}
void setIsRepaintEnd(int v)
{
    gIsRepaintEnd.lock();
    isRepaintEnd = v;
    gIsRepaintEnd.unlock();
}
int hasRepaintEnd()
{
    volatile int ret;
    gIsRepaintEnd.lock();
    ret = isRepaintEnd;
    gIsRepaintEnd.unlock();
    return ret;
}
void clearBrushPiece()
{
    gBrushPieceMutex.lock();
    std::list<BrushPiece>::iterator it;
    for(it = gBrushPieceList.begin() ; it != gBrushPieceList.end(); it++)
    {
        BrushPiece bp = *it;
        ppm_kill(&bp.data);
        ppm_kill(&bp.alpha);
    }
	gBrushPieceList.clear();
	gBrushPieceMutex.unlock();
}
BrushPiece getNextBrushPiece()
{
    BrushPiece bp;
    int tmpRepaintEnd = hasRepaintEnd();
    gBrushPieceMutex.lock();
    if(!gBrushPieceList.empty())
    {
        bp = gBrushPieceList.front();
        gBrushPieceList.pop_front();
    }
    else
    {
        if(!tmpRepaintEnd)
	    {
            bp.x = bp.y = -1;
   	    }
	    else
	    {
	        bp.x = bp.y = -2;
	    }
    }
    gBrushPieceMutex.unlock();
    return bp;
}
void addBrushPiece(BrushPiece bp)
{
    gBrushPieceMutex.lock();
    gBrushPieceList.push_back(bp);
    gBrushPieceMutex.unlock();
}
///////////////////////////////////////////////////////////////////////////
static void gimp_rgb_get_uchar (const GimpRGB *rgb,
                    guchar        *r,
                    guchar        *g,
                    guchar        *b)
{
    if(rgb == NULL)
	    return;
    if (r) *r = ROUND (CLAMP (rgb->r, 0.0, 1.0) * 255.0);
    if (g) *g = ROUND (CLAMP (rgb->g, 0.0, 1.0) * 255.0);
    if (b) *b = ROUND (CLAMP (rgb->b, 0.0, 1.0) * 255.0);
}
static gboolean file_is_color (const char *fn)
{
    return fn && strstr (fn, ".ppm");
}
static void set_colorbrushes (const gchar *fn)
{
    pcvals.color_brushes = file_is_color (fn);
}
static size_t g_strlcpy (gchar       *dest,
           const gchar *src,
           size_t        dest_size)
{
    gchar *d = dest;
    const gchar *s = src;
    size_t n = dest_size;

    if(!dest)
        return 0;
    if(!src)
	    return 0; 

    /* Copy as many bytes as will fit */
    if (n != 0 && --n != 0)
    do
    {
        register gchar c = *s++;

        *d++ = c;
        if (c == 0)
            break;
    }
    while (--n != 0);

    /* If not enough room in dest, add NUL and traverse rest of src */
    if (n == 0)
    {
        if (dest_size != 0)
            *d = 0;
        while (*s++)
            ;
    }

    return s - src - 1;  /* count does not include NUL */
}

static void brush_reload (const gchar *fn, ppm_t       *p)
{
    static char  lastfn[256] = "";
    static ppm_t cache       = {0, 0, NULL};

    if (fn == NULL)
    {
        ppm_kill (&cache);
        lastfn[0] = '\0';
        return;
    }
    g_printerr("## brush_reload fn = %s ###\n", fn);
    if (strcmp (fn, lastfn))
    {
        g_strlcpy (lastfn, fn, sizeof (lastfn));
        g_printerr("## lastfn = %s ##\n", lastfn);
        ppm_kill (&cache);
        ppm_load (fn, &cache);
    }
    ppm_copy (&cache, p);
    set_colorbrushes (fn);
}
static void brush_get(const char* ppmName, ppm_t* p)
{
    ppm_load(ppmName, p);
}
static void brush_get_selected (ppm_t *p)
{
    if (brush_from_file)
    {
        g_printerr("#####brush_get_selected ####");
        brush_reload (pcvals.selected_brush, p);
	    /*
        //debug
	    Image image;
	    image.width = p->width;
	    image.height = p->height;
	    image.bpp = 3;
	    image.data = p->col;
	    save(image, "c:\\testbrush.jpg");
	    //endi
        */
    }
    else
        ppm_copy (&brushppm, p);
}

static double get_direction (double x, double y, int from)
{
    gint      i;
    gint      n;
    gint      voronoi;
    gdouble   sum, dx, dy, dst;
    vector_t *vec;
    gdouble   angoff, strexp;
    gint      first = 0, last;

    
    n = pcvals.num_orient_vectors;
    vec = pcvals.orient_vectors;
    angoff = pcvals.orient_angle_offset;
    strexp = pcvals.orient_strength_exponent;
    voronoi = pcvals.orient_voronoi;
  

    if (voronoi)
    {
        gdouble bestdist = -1.0;

        for (i = 0; i < n; i++)
        {
            dst = dist(x,y,vec[i].x,vec[i].y);

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

    dx = dy = 0.0;
    sum = 0.0;
    for (i = first; i < last; i++)
    {
        gdouble s = vec[i].str;
        gdouble tx = 0.0, ty = 0.0;

        if (vec[i].type == 0)
        {
            tx = vec[i].dx;
            ty = vec[i].dy;
        }
        else if (vec[i].type == 1)
        {
            gdouble a = atan2 (vec[i].dy, vec[i].dx);

            a -= atan2 (y-vec[i].y, x-vec[i].x);
            tx = sin (a + G_PI_2);
            ty = cos (a + G_PI_2);
        }
        else if (vec[i].type == 2)
        {
            gdouble a = atan2 (vec[i].dy, vec[i].dx);

            a += atan2 (y-vec[i].y, x-vec[i].x);
            tx = sin (a + G_PI_2);
            ty = cos (a + G_PI_2);
        }
        else if (vec[i].type == 3)
        {
            gdouble a = atan2 (vec[i].dy, vec[i].dx);

            a -= atan2 (y-vec[i].y, x-vec[i].x)*2;
            tx = sin (a + G_PI_2);
            ty = cos (a + G_PI_2);
        }

        dst = dist (x,y,vec[i].x,vec[i].y);
        dst = pow (dst, strexp);

        if (dst < 0.0001)
            dst = 0.0001;
        s = s / dst;

        dx += tx * s;
        dy += ty * s;
        sum += s;
    }
    dx = dx / sum;
    dy = dy / sum;

    return 90 - (gimp_rad_to_deg (atan2 (dy, dx)) + angoff);
}
static double get_siz_from_pcvals (double x, double y)
{
    return getsiz_proto (x, y, pcvals.num_size_vectors, pcvals.size_vectors,
                       pcvals.size_strength_exponent, pcvals.size_voronoi);
}

static int get_pixel_value (double dir)
{
    while (dir < 0.0)
        dir += 360.0;
    while (dir >= 360.0)
        dir -= 360.0;
    return dir * 255.0 / 360.0;
}

static void prepare_brush (ppm_t *p)
{
    int x, y;
    int rowstride = p->width * 3;

    for (y = 0; y< p->height; y++)
    {
        for (x = 0; x < p->width; x++)
        {
            p->col[y * rowstride + x * 3 + 1] = 0;
        }
    }

    for (y = 1; y< p->height; y++)
    {
        for (x = 1; x < p->width; x++)
        {
            int v = p->col[y * rowstride + x * 3] -
                  p->col[(y - 1) * rowstride + (x - 1) * 3];
            if (v < 0)
                v = 0;
            p->col[y * rowstride + x * 3 + 1] = v;
        }
    }
}

static double sum_brush (ppm_t *p)
{
    double sum = 0;
    int i;

    for (i = 0; i < p->width*3*p->height; i += 3)
        sum += p->col[i];
    return sum;
}

/* TODO : Use r = rgb[0]; g = rgb[1] ; b = rgb[2]; instead of
 * the direct references here.
 * */
static int get_hue (guchar *rgb)
{
    double h, v, temp, diff;
    /* TODO : There seems to be some typoes in the comments here.
     * Ask vidar what he meant.
     * */
    if ((rgb[0] == rgb[1]) && (rgb[0] == rgb[2])) /* Gray */
        return 0;
    v = (rgb[0] > rgb[1] ? rgb[0] : rgb[1]);     /* v = strste verdi */
    if (rgb[2] > v)
        v = rgb[2];
    temp = (rgb[0] > rgb[1] ? rgb[1] : rgb[0] ); /* temp = minste */
    if (rgb[2] < temp)
        temp = rgb[2];
    diff = v - temp;

    if (v == rgb[0])
        h = ((double)rgb[1] - rgb[2]) / diff;
    else if(v == rgb[1])
        h = ((double)rgb[2] - rgb[0]) / diff + 2;
    else /* v == rgb[2] */
        h = ((double)rgb[0] - rgb[1]) / diff + 4;
    if(h < 0) h += 6;
    return h * 255.0 / 6.0;
}
template <typename T> bool g_list_nth(std::list<T>& data, int index, T& outData)
{
	typename std::list<T>::iterator it = data.begin();
	int i = 0;
	while(it != data.end() && i < index)
	{
		it++;
		i++;
	}
	if(it != data.end())
	{
		outData = *it;
		return true;
	}
	else
		return false;
}
static bool brushOnEdge(ppm_t* edgeDetectionMap, int brushWidth, int brushHeight, int tx, int ty)
{
    int x, y;
    double r, g , b;
    bool found = false;
    int bottom = edgeDetectionMap->height;
    int right = edgeDetectionMap->width;
    for (y = 0; y < brushHeight; y++)
    {
        if((ty + y) < bottom)
        {
            guchar *row = edgeDetectionMap->col + (ty + y) * edgeDetectionMap->width * 3;
            for (x = 0; x < brushWidth; x++)
            {
                if((tx + x) < right)
                {
                    int    k = (tx + x) * 3;
                    r = row[k+0];
                    g = row[k+1];
                    b = row[k+2];
                    //double gray = (r + b + g) / 3;
                    //if(gray <= 255.0 && gray > 191)
                    if((r <= 255 && r >= 200) || 
                       (g <= 255 && g >= 200) ||
                       (b <= 255 && b >= 200))
                    {
                        found = true;
                        //LOGI("## gray = %f ##\n", gray);
                        break;
                    }
                }
                else 
                {
                    break;
                }
            }
            if(found)
                break;
        }
        else
        {
            break;
        }
    }
    return found;

}
static int choose_best_brush (ppm_t* edgeDetectionMap, ppm_t *p, ppm_t *a, int tx, int ty,
                   ppm_t *brushes, int num_brushes,
                   double *brushes_sum, int start, int step)
{
    double dev, thissum;
    double bestdev = 0.0;
    double r, g, b;
    long    best = -1;
    int    x, y, h;
    long   i;
    //GList *brlist = NULL;
    std::list<long> brlist;
    for (i = start; i < num_brushes; i += step)
    {
        ppm_t *brush = &brushes[i];
#if 0
        thissum = 0.0;
#endif
        thissum = brushes_sum[i];

        /* TODO: Pointer-arithmeticize this code */
        r = g = b = 0.0;
        for (y = 0; y < brush->height; y++)
        {
            guchar *row = p->col + (ty + y) * p->width * 3;

            for (x = 0; x < brush->width; x++)
            {
                int    k = (tx + x) * 3;
                double v;

                if ((h = brush->col[(y * brush->width * 3) + x * 3]))
                {
#if 0
                    thissum += h;
#endif
                    v = h / 255.0;
                    r += row[k+0] * v;
                    g += row[k+1] * v;
                    b += row[k+2] * v;
                }
            }
        }
        r = r * 255.0 / thissum;
        g = g * 255.0 / thissum;
        b = b * 255.0 / thissum;

        dev = 0.0;
        for (y = 0; y < brush->height; y++)
        {
            guchar *row = p->col + (ty + y) * p->width * 3;

            for (x = 0; x < brush->width; x++)
            {
                int    k = (tx + x) * 3;
                double v;

                if ((h = brush->col[(y * brush->width * 3) + x * 3]))
                {
                    v = h / 255.0;
                    dev += abs (row[k+0] - r) * v;
                    dev += abs (row[k+1] - g) * v;
                    dev += abs (row[k+2] - b) * v;
                    if (img_has_alpha)
                        dev += a->col[(ty + y) * a->width * 3 + (tx + x) * 3] * v;
                }
            }
        }
        dev /= thissum;
        double dist = fabs(dev - bestdev);
        //LOGI("## brush dis = %f ##\n", dist);
        if ((best == -1) || (dev < bestdev))
        //if (best == -1)
        {
            brlist.clear();
        }
        //LOGI("## dev = %f ##\n", dev);
        if (dev <= bestdev || best < 0)
        //if(( dev < bestdev) || best < 0)
        {
            best = i;
            bestdev = dev;
            brlist.push_back(i);
        }
        if (dev < runningvals.devthresh)
            break;
    } 

    if (brlist.empty())
    {
        LOGI("What!? No brushes?!\n");
        return 0;
    }
    
    //TODO: need change
    i = g_rand_int_range (random_generator, 0, brlist.size());
    //LOGI("## best brush size = %lu, i = %lu  ##\n", brlist.size(), i);
    bool ret = g_list_nth<long> (brlist,i, best);
    if(ret == 0)
	    LOGI("error best value\n");
    //TODO: end
    return best;
     
    //i = g_rand_int_range(random_generator, 0, num_brushes);
    //return i;
}
void apply_brush_area (ppm_t *brush,
             ppm_t *shadow,
             ppm_t *p, ppm_t *a,
             int tx, int ty, int r, int g, int b)
{
    ppm_t  tmp;
    ppm_t  atmp;
    double v, h;
    int    x, y;
    double edgedarken = 1.0 - runningvals.general_dark_edge;
    double relief = runningvals.brush_relief / 100.0;
    int    shadowdepth = pcvals.general_shadow_depth;
    int    shadowblur = pcvals.general_shadow_blur;
    
    atmp.col = 0;
    atmp.width = 0;
    
    tmp = *p;
    if (img_has_alpha)
        atmp = *a;
    
    if (shadow)
    {
        int sx = tx + shadowdepth - shadowblur * 2;
        int sy = ty + shadowdepth - shadowblur * 2;
        
        for (y = 0; y < shadow->height; y++)
        {
            guchar *row, *arow = NULL;
            
            if ((sy + y) < 0)
                continue;
            if ((sy + y) >= tmp.height)
                break;
            row = tmp.col + (sy + y) * tmp.width * 3;
            
            if (img_has_alpha)
                arow = atmp.col + (sy + y) * atmp.width * 3;
            
            for (x = 0; x < shadow->width; x++)
            {
                int k = (sx + x) * 3;
                
                if ((sx + x) < 0)
                    continue;
                if ((sx + x) >= tmp.width)
                    break;
                
                h = shadow->col[y * shadow->width * 3 + x * 3 + 2];
                
                if (!h)
                    continue;
                v = 1.0 - (h / 255.0 * runningvals.general_shadow_darkness / 100.0);
                
                row[k+0] *= v;
                row[k+1] *= v;
                row[k+2] *= v;
                if (img_has_alpha)
                    arow[k] *= v;
            }
        }
    }
    
    for (y = 0; y < brush->height; y++)
    {
        if((ty + y) < 0)
            continue;
        if((ty + y) >= tmp.height)
            continue;
        
        guchar *row = tmp.col + (ty + y) * tmp.width * 3;
        guchar *arow = NULL;
        
        if (img_has_alpha)
            arow = atmp.col + (ty + y) * atmp.width * 3;
        
        for (x = 0; x < brush->width; x++)
        {
            if((tx + x) < 0)
                continue;
            if((tx + x) >= tmp.width)
                continue;
            int k = (tx + x) * 3;
            h = brush->col[y * brush->width * 3 + x * 3];
            
            if (!h)
            { 
                continue;
            }
            
            if (runningvals.color_brushes)
            {
                v = 1.0 - brush->col[y * brush->width * 3 + x * 3 + 2] / 255.0;
                row[k+0] *= v;
                row[k+1] *= v;
                row[k+2] *= v;
                if (img_has_alpha)
                    arow[(tx + x) * 3] *= v;
            }
            v = (1.0 - h / 255.0) * edgedarken;
            row[k+0] *= v;
            row[k+1] *= v;
            row[k+2] *= v;
            if(img_has_alpha) arow[k] *= v;
            
            v = h / 255.0;
            row[k+0] += r * v;
            row[k+1] += g * v;
            row[k+2] += b * v;
        }
    }
    
    if (relief > 0.001)
    {
        for (y = 1; y < brush->height; y++)
        {
            guchar *row = tmp.col + (ty + y) * tmp.width * 3;
            
            for (x = 1; x < brush->width; x++)
            {
                int k = (tx + x) * 3;
                h = brush->col[y * brush->width * 3 + x * 3 + 1] * relief;
                if (h < 0.001)
                    continue;
                if (h > 255) h = 255;
                row[k+0] = (row[k+0] * (255-h) + 255 * h) / 255;
                row[k+1] = (row[k+1] * (255-h) + 255 * h) / 255;
                row[k+2] = (row[k+2] * (255-h) + 255 * h) / 255;
            }
        }
    }
}

static void apply_brush (ppm_t *brush,
             ppm_t *shadow,
             ppm_t *p, ppm_t *a,
             int tx, int ty, int r, int g, int b)
{
    ppm_t  tmp;
    ppm_t  atmp;
    double v, h;
    int    x, y;
    double edgedarken = 1.0 - runningvals.general_dark_edge;
    double relief = runningvals.brush_relief / 100.0;
    int    shadowdepth = pcvals.general_shadow_depth;
    int    shadowblur = pcvals.general_shadow_blur;

    atmp.col = 0;
    atmp.width = 0;

    tmp = *p;
    if (img_has_alpha)
        atmp = *a;

    if (shadow)
    {
        int sx = tx + shadowdepth - shadowblur * 2;
        int sy = ty + shadowdepth - shadowblur * 2;

        for (y = 0; y < shadow->height; y++)
        {
            guchar *row, *arow = NULL;

            if ((sy + y) < 0)
                continue;
            if ((sy + y) >= tmp.height)
                break;
            row = tmp.col + (sy + y) * tmp.width * 3;

            if (img_has_alpha)
                arow = atmp.col + (sy + y) * atmp.width * 3;

            for (x = 0; x < shadow->width; x++)
            {
                int k = (sx + x) * 3;

                if ((sx + x) < 0)
                    continue;
                if ((sx + x) >= tmp.width)
                    break;

                h = shadow->col[y * shadow->width * 3 + x * 3 + 2];

                if (!h)
                    continue;
                v = 1.0 - (h / 255.0 * runningvals.general_shadow_darkness / 100.0);

                row[k+0] *= v;
                row[k+1] *= v;
                row[k+2] *= v;
                if (img_has_alpha)
                    arow[k] *= v;
            }
        }
    }

    for (y = 0; y < brush->height; y++)
    {
        
        guchar *row = tmp.col + (ty + y) * tmp.width * 3;
        guchar *arow = NULL;

        if (img_has_alpha)
            arow = atmp.col + (ty + y) * atmp.width * 3;

        for (x = 0; x < brush->width; x++)
        {
            int k = (tx + x) * 3;
            h = brush->col[y * brush->width * 3 + x * 3];

            if (!h)
            { 
                continue;
	        }

            if (runningvals.color_brushes)
            {
                v = 1.0 - brush->col[y * brush->width * 3 + x * 3 + 2] / 255.0;
                row[k+0] *= v;
                row[k+1] *= v;
                row[k+2] *= v;
                if (img_has_alpha)
                    arow[(tx + x) * 3] *= v;
            }
            v = (1.0 - h / 255.0) * edgedarken;
            row[k+0] *= v;
            row[k+1] *= v;
            row[k+2] *= v;
            if(img_has_alpha) arow[k] *= v;

            v = h / 255.0;
            row[k+0] += r * v;
            row[k+1] += g * v;
            row[k+2] += b * v;
        }
    }

    if (relief > 0.001)
    {
        for (y = 1; y < brush->height; y++)
        {
            guchar *row = tmp.col + (ty + y) * tmp.width * 3;

            for (x = 1; x < brush->width; x++)
            {
                int k = (tx + x) * 3;
                h = brush->col[y * brush->width * 3 + x * 3 + 1] * relief;
                if (h < 0.001)
                    continue;
                if (h > 255) h = 255;
                row[k+0] = (row[k+0] * (255-h) + 255 * h) / 255;
                row[k+1] = (row[k+1] * (255-h) + 255 * h) / 255;
                row[k+2] = (row[k+2] * (255-h) + 255 * h) / 255;
            }
        }
    }
}

////////////////////
struct BrushProperty
{
    int tx;
    int ty;
    int r, g, b;
    ppm_t* brush;
    ppm_t* destBrush;
    ppm_t* shadow;
};
static std::list<BrushProperty> gBrushProperties;
class _BrushPropertyComp
{
public:
	bool operator()(const BrushProperty& left, const BrushProperty& right)
	{
		float leftGray = left.r * 0.2126f + left.g * 0.7152f + left.b * 0.0722f;
		float rightGray = right.r * 0.2126f + right.g * 0.7152f + right.b * 0.0722f;
		if(leftGray < rightGray)
			return true;
		else
			return false;
	}
};
static void createAlpha(ppm_t* brush, ppm_t* alpha)
{
    int brushrowstride = brush->width * 3;
    int alpharowstride = alpha->width;
    int x, y;
    for(y = 0 ; y < brush->height ; y++)
    {
        guchar* row = brush->col + y * brushrowstride;
        guchar* alpharow = alpha->col + y * alpharowstride;
        for(x = 0 ; x < brush->width ; x++)
        {
            guchar* src = row + x * 3;
            guchar* dst = alpharow + x;
            if(src[0] == 0)
                dst[0] = 0;
            else
                dst[0] = 255;
        }
    }
}
#define ROW_NUM 10
#define COL_NUM 10
struct GeoPaintArea
{
    float left, right, top, bottom;
    int hasPainted;
    std::list<BrushProperty> bp;
    GeoPaintArea()
    {
        left = right = top = bottom = 0;
        hasPainted = 0;
    }
};
struct GeoPaintCanvas
{
    GeoPaintArea pa[ROW_NUM][COL_NUM];
};
struct ImageHueDistribute
{
    float minAngle;
    float maxAngle;
    int num;
};
static int firstHue, secondHue, thirdHue;
static ImageHueDistribute gImageHueDistribute[] = 
{
    {0, 60, 0},
    {60, 120, 0},
    {120, 180, 0},
    {180, 240, 0},
    {240, 300, 0},
    {300, 360, 0}
};
//return the hue angle
static float rgbToHueAngle(guchar *rgb)
{
    float r = rgb[0] / 255.0f;
    float g = rgb[1] / 255.0f;
    float b = rgb[2] / 255.0f;
    //float v = r > g ? r : g;
    float max = std::max(r, std::max(g, b));
    float min = std::min(r, std::min(g, b));
    /*
    if(v < b)
        max = b;
    v = r < g ? r : g;
    float min = v;
    if(v > b)
        v = v;
     */
    float delta = max - min;
    if(max == min)
        return 0;
    float h = 0;
    if(g > b)
    {
        h = (max - r + g - min + b - min) * 60.0f/ delta;
    }
    else 
    {
        h = 360 - (max - r + g - min + b - min) * 60 / delta;    
    }
    if(h < 0)
    {
        h = 360 + h;
    }
    return h;
}
static int getImageHueDistributeIndex(float hAngle)
{
    int count = sizeof(gImageHueDistribute) / sizeof(ImageHueDistribute);
    for(int i = 0 ; i < count ; i++)
    {
        if(gImageHueDistribute[i].minAngle <= hAngle && hAngle <= gImageHueDistribute[i].maxAngle)
            return i;
    }
    return -1;
}
static void clearImageHueDistribute()
{
    int count = sizeof(gImageHueDistribute) / sizeof(ImageHueDistribute);
    for(int i = 0 ; i < count ; i++)
    {
        gImageHueDistribute[i].num = 0;
    }
}

static void findFirstThreeHue(int& first, int& second, int& third)
{
    int count = sizeof(gImageHueDistribute) / sizeof(ImageHueDistribute);
    int max = 0;
    int minNum = INT_MIN;
    std::list<int> hasFindIndex;
    for(int i = 0 ;i < count ; i++)
    {
        std::list<int>::iterator it = find(hasFindIndex.begin(), hasFindIndex.end(), i);
        if(gImageHueDistribute[i].num >  minNum && it == hasFindIndex.end())
        {
            max = i;
            minNum = gImageHueDistribute[i].num;
        }
    }
    hasFindIndex.push_back(max);
    first = max;
    minNum = INT_MIN;
    max = 0;
    for(int i = 0 ;i < count ; i++)
    {
        std::list<int>::iterator it = find(hasFindIndex.begin(), hasFindIndex.end(), i);
        if(gImageHueDistribute[i].num >  minNum && it == hasFindIndex.end())
        {
            max = i;
            minNum = gImageHueDistribute[i].num;
        }
    }
    hasFindIndex.push_back(max);
    second = max;
    max = 0;
    minNum = INT_MIN;
    for(int i = 0 ;i < count ; i++)
    {
        std::list<int>::iterator it = find(hasFindIndex.begin(), hasFindIndex.end(), i);
        if(gImageHueDistribute[i].num >  minNum && it == hasFindIndex.end())
        {
            max = i;
            minNum = gImageHueDistribute[i].num;
        }
    }
    third = max;
    
    /*
    for(int i = 1 ; i < count ; i++)
    {
        if(gImageHueDistribute[max].num < gImageHueDistribute[i].num)
        {
            max = i;
        }
    }
    first = max;
    ImageHueDistribute secondHueDistribute[5];
    int j = 0;
    for(int i = 0 ; i < 6 ; i++)
    {
        if(i != max)
        {
            secondHueDistribute[j++] = gImageHueDistribute[i];
        }
    }
    max = 0;
    for(int i = 1 ; i < 5 ; i++)
    {
        if(secondHueDistribute[max].num < secondHueDistribute[i].num)
            max = i;
    }
    second = max;
    ImageHueDistribute thirdHueDistribute[4];
    j = 0 ; 
    for(int i = 0 ;i < 5 ; i++)
    {
        if(i != max)
        {
            thirdHueDistribute[j++] = secondHueDistribute[i];
        }
    }
    max = 0;
    for(int i = 1 ; i < 4 ; i++)
    {
        if(thirdHueDistribute[max].num < thirdHueDistribute[i].num)
            max = i;
    }
    third = max;
    assert(gImageHueDistribute[first].num >= gImageHueDistribute[second].num && gImageHueDistribute[second].num >= gImageHueDistribute[third].num);
     */
    LOGI("## firstHue = %d, secondHue = %d, thirdHue = %d ##\n", first, second, third);
}
static void createImageHueDistribute(ppm_t* image)
{
    clearImageHueDistribute();
    for(int i = 0 ;i < image->height ; i++)
    {
        guchar* src = image->col + i * image->width * 3;
        for(int j = 0 ; j < image->width ; j++)
        {
            float h = rgbToHueAngle(src + j * 3);
            int index = getImageHueDistributeIndex(h);
            assert(index != -1);
            gImageHueDistribute[index].num++;
        }
    }
}
#define QUAD_TREE_DEPTH 3
struct QuadRect
{
    float left, right, top, bottom;
    QuadRect()
    {
        left = right = top = bottom = 0;
    }
    QuadRect(float l, float r, float t, float b)
    {
        left = l;
        right = r;
        top = t;
        bottom = b;
    }
};
struct QuadTreeNode
{
    QuadTreeNode* child[4];
    QuadRect rect;
    std::list<BrushProperty> bpList;
    QuadTreeNode()
    {
        for(int i = 0 ; i < 4  ; i++)
            child[i] = NULL;
    }
};

static QuadTreeNode* rootQuadTree = NULL;
static QuadTreeNode* createQuadTree(float left, float right, float top, float bottom, int depth)
{
    QuadTreeNode* qtn = new QuadTreeNode;
    qtn->rect = QuadRect(left, right, top, bottom);
    if(depth == QUAD_TREE_DEPTH)
        return qtn;
    QuadRect rects[4];
    float midx = left + (right - left) / 2;
    float midy = top + (bottom - top) / 2;
    
    rects[0].left = left;
    rects[0].right = midx;
    rects[0].top = top;
    rects[0].bottom = midy;
    rects[1].left = left;
    rects[1].right = midx;
    rects[1].top = midy;
    rects[1].bottom = bottom;

    rects[2].left = midx;
    rects[2].right = right;
    rects[2].top = top;
    rects[2].bottom = midy;

    rects[3].left = midx;
    rects[3].right = right;
    rects[3].top = midy;
    rects[3].bottom = bottom;
    for(int i = 0 ; i < 4 ; i++)
    {
        qtn->child[i] = createQuadTree(rects[i].left, rects[i].right, rects[i].top, rects[i].bottom, depth + 1);
    }
    return qtn;
}
static void releaseQuadTree(QuadTreeNode* quadTree)
{
    if(quadTree == NULL)
        return;
    for(int i = 0 ; i < 4 ; i++)
    {
        releaseQuadTree(quadTree->child[i]);
    }
    delete quadTree;
}
static int pointInRect(float x, float y , QuadRect& rect)
{
    if(x >= rect.left && x <= rect.right && y >= rect.top && y <= rect.bottom)
        return 1;
    else
        return 0;
}
static std::list<BrushProperty> outputBrushProperty(QuadTreeNode* quadTree)
{
    if(quadTree == NULL)
    {
        std::list<BrushProperty> bp;
        return bp;
    }
    else 
    {
        std::list<BrushProperty> outList;
        std::list<BrushProperty>::iterator it;
        for(int i = 0 ;i < 4 ; i++)
        {
            std::list<BrushProperty> bp = outputBrushProperty(quadTree->child[i]);
            for(it = bp.begin() ; it != bp.end(); it++)
            {
                outList.push_back(*it);
            }
        }
        if(quadTree->child[0] == NULL)
        {
            std::list<BrushProperty> currentBp1;;
            std::list<BrushProperty> currentBp2;
            std::list<BrushProperty> currentBp3;
            std::list<BrushProperty> currentBp4;
            for(it = quadTree->bpList.begin() ; it != quadTree->bpList.end() ; it++)
            {
                //outList.push_back(*it);
                guchar rgb[3];
                rgb[0] = it->r;
                rgb[1] = it->g;
                rgb[2] = it->b;
                float h = rgbToHueAngle(rgb);
                if(h >= gImageHueDistribute[firstHue].minAngle && h < gImageHueDistribute[firstHue].maxAngle)
                {
                    currentBp1.push_back(*it);
                }
                else if(h >= gImageHueDistribute[secondHue].minAngle && h < gImageHueDistribute[secondHue].maxAngle)
                {
                    currentBp2.push_back(*it);
                }
                else if(h >= gImageHueDistribute[thirdHue].minAngle && h < gImageHueDistribute[thirdHue].maxAngle)
                {
                    currentBp3.push_back(*it);
                }
                else 
                {
                    currentBp4.push_back(*it);    
                }
                
            }
            for(it = currentBp1.begin() ; it != currentBp1.end() ; it++)
            {
                outList.push_back(*it);
            }
            for(it = currentBp2.begin() ; it != currentBp2.end() ; it++)
            {
                outList.push_back(*it);
            }
            for(it = currentBp3.begin() ; it != currentBp3.end() ; it++)
            {
                outList.push_back(*it);
            }
            for(it = currentBp4.begin() ; it != currentBp4.end(); it++)
            {
                outList.push_back(*it);
            }
        }
        else 
        {
            assert(quadTree->bpList.size() == 0);
        }
        return outList;
    }
}
static void clearQuadTree(QuadTreeNode* quadTree)
{
    if(quadTree == NULL)
    {
        return;
    }
    else
    {
        quadTree->bpList.clear();
        for(int i = 0 ;i < 4 ; i++)
        {
            clearQuadTree(quadTree->child[i]);    
        }
    }
}
/*
 1: add ok
 0: add failed
 */
static int addBrushPropertyToQuadTree(BrushProperty bp, QuadTreeNode* quadTree)
{
    if(quadTree->child[0] == NULL)
    {
        if(pointInRect(bp.tx, bp.ty, quadTree->rect))
        {
            quadTree->bpList.push_back(bp);
            return 1;
        }
        else
            return 0;
    }
    else
    {
        if(pointInRect(bp.tx, bp.ty, quadTree->rect))
        {
            for(int i = 0 ; i < 4 ; i++)
            {
                int found = addBrushPropertyToQuadTree(bp, quadTree->child[i]);
                if(found)
                    return 1;
            }
            assert(0);
        }
        else
            return 0;
    };
}
#define GRAY_NUM 5
struct GrayPaintArea
{
    std::list<BrushProperty> bp;
};
GrayPaintArea grayPaintArray[GRAY_NUM];
static void clearGrayPaintArea()
{
    for(int i = 0 ; i < GRAY_NUM ; i++)
    {
        grayPaintArray[i].bp.clear();
    }
}
static void addBrushPropertyToGrayPaintArea(BrushProperty bp)
{
    float gray = bp.r * 0.2126f + bp.g * 0.7152f + bp.b * 0.0722f;
    float graySpan = 256.0f / GRAY_NUM;
    if(gray >= 0 && gray < graySpan)
    {
        grayPaintArray[0].bp.push_back(bp);
    }
    else if(gray >= graySpan && gray < 2 * graySpan)
    {
        grayPaintArray[1].bp.push_back(bp);
    }
    else if(gray >= 2 * graySpan && gray < 3 * graySpan)
    {
        grayPaintArray[2].bp.push_back(bp);
    }
    else if(gray >= 3 * graySpan && gray < 4 * graySpan)
    {
        grayPaintArray[3].bp.push_back(bp);
    }
    else {
        grayPaintArray[4].bp.push_back(bp);
    }
}
static void addBrushPropertyToGeoPaintCanvas(BrushProperty bp, GeoPaintCanvas* canvas)
{
    
    
}
static void separatePaintArea()
{
    gBrushProperties.clear();
    for(int i = 0 ; i < GRAY_NUM ; i++)
    {
        std::list<BrushProperty>::iterator it;
        for(it = grayPaintArray[i].bp.begin() ; it != grayPaintArray[i].bp.end() ; it++)
        {
            addBrushPropertyToQuadTree(*it, rootQuadTree);
        }
        std::list<BrushProperty> outList = outputBrushProperty(rootQuadTree);
        for(it = outList.begin() ; it != outList.end() ; it++)
        {
            gBrushProperties.push_back(*it);
        }
        clearQuadTree(rootQuadTree);
    }
}
void testQuadTree()
{
    rootQuadTree = createQuadTree(0, 1024, 0, 768, 0);
    releaseQuadTree(rootQuadTree);
}
////////////////////////
//#define BRUSH_NUM 3
void repaint (ppm_t *p, ppm_t *a, RepaintData rd)
{
    int         x, y;
    int         tx = 0, ty = 0;
    ppm_t       tmp = {0, 0, NULL};
    ppm_t       atmp = {0, 0, NULL};
    int         r, g, b, n, h, i, j, on, sn;
    int         num_brushes, maxbrushwidth, maxbrushheight;
    guchar      back[3] = {0, 0, 0};
    ppm_t      *brushes, *shadows;
    ppm_t      *destBrushes;
    ppm_t      *brush, *shadow = NULL;
    double     *brushes_sum;
    int         cx, cy, maxdist;
    double      scale, relief, startangle, anglespan, density, bgamma;
    double*      scales;//[BRUSH_NUM];
    double      thissum;
    int         max_progress;
    ppm_t       paper_ppm = {0, 0, NULL};
    ppm_t       dirmap = {0, 0, NULL};
    ppm_t       sizmap = {0, 0, NULL};
    int        *xpos = NULL, *ypos = NULL;
    int         step = 1;
    int         progstep;
    int destWidth ;
    int destHeight;
    int brushIndex;
    SE_BrushSet brushSet;
    SS_PausePoint* currentPausePoint = NULL;
    SS_Canvas* currentCanvas = NULL;
    static int  running = 0;
    ppm_t edgeDetectionMap = {0, 0, NULL};
    
    int dropshadow = pcvals.general_drop_shadow;
    int shadowblur = pcvals.general_shadow_blur;
  
    g_printerr("####running = %d ###\n", running);
    if (running)
        return;
    
    currentPausePoint = SS_GetCurrentPausePoint();
    if(rootQuadTree == NULL)
    {
        rootQuadTree = createQuadTree(0, 1024, 0, 768, 0);
    }
    createImageHueDistribute(p);
    findFirstThreeHue(firstHue, secondHue, thirdHue);
    
    currentCanvas = SS_GetCurrentCanvas();
    running++;
    SS_GetDestSize(&destWidth, &destHeight);
    SS_GetSettingBrush(&brushSet);
    int BRUSH_NUM = brushSet.brush.size();
    scales = new double[BRUSH_NUM];
    runningvals = pcvals;
    print_val(&runningvals);
    gImageWidth = p->width;
    gImageHeight = p->height;
    /*
    if(ppm_empty(&gBackground))
    {
        gBackground = createBackground(runningvals, destWidth, destHeight);
        tmp = createBackground(runningvals, p->width, p->height);
        
    }
    else
    {
        tmp = createBackground(runningvals, p->width, p->height);
    }
     */
    //tmp.col = NULL;
    //tmp.width = p->width;
    //tmp.height = p->height;
    LOGI("## gImageWidth = %d, gImageHeight = %d ##\n", gImageWidth, gImageHeight);
    //gBackground = createBackground(runningvals, destWidth, destHeight);
    /* Shouldn't be necessary, but... */
    if (img_has_alpha)
    {
        if ((p->width != a->width) || (p->height != a->height))
        {
            g_printerr ("Huh? Image size != alpha size?\n");
            return;
        }
    }
    if(rd.calculateOnEdge)
    {
        edgeDetectionMap = edgeDetection(p);
    }
    num_brushes = runningvals.orient_num * runningvals.size_num;
    startangle = runningvals.orient_first;
    anglespan = runningvals.orient_last;

    density = runningvals.brush_density;

    if (runningvals.place_type == PLACEMENT_TYPE_EVEN_DIST)
        density /= 3.0;

    bgamma = runningvals.brushgamma;

    brushes = (ppm_t*)g_malloc (num_brushes * sizeof (ppm_t));
    destBrushes = (ppm_t*)g_malloc(num_brushes * sizeof(ppm_t));
    brushes_sum = (double*)g_malloc (num_brushes * sizeof (double));

    if (dropshadow)
        shadows = (ppm_t*)g_malloc (num_brushes * sizeof (ppm_t));
    else
        shadows = NULL;
    for(brushIndex = 0 ; brushIndex < BRUSH_NUM ; brushIndex++)
    {
        brushes[brushIndex].col = NULL;
        destBrushes[brushIndex].col = NULL;
        int k = g_rand_int_range(random_generator, 0, BRUSH_NUM);
        brush_get(brushSet.brush[k].c_str(), &brushes[brushIndex]);
        ppm_copy(&brushes[brushIndex], &destBrushes[brushIndex]);
    }
    scale = runningvals.size_last / std::max (brushes[0].width, brushes[0].height);
    for(brushIndex = 0 ; brushIndex < BRUSH_NUM ; brushIndex++)
    {
        scales[brushIndex] = runningvals.size_last / std::max (brushes[brushIndex].width, brushes[brushIndex].height);
    }
    if (bgamma != 1.0)
        ppm_apply_gamma (&brushes[0], 1.0 / bgamma, 1,1,1);
    
    for(brushIndex = 0 ; brushIndex < BRUSH_NUM ; brushIndex++)
    {
        if(brushes[brushIndex].height < 5)
        {
            ppm_t tmp = {0, 0, NULL};
            ppm_copy(&brushes[brushIndex], &tmp);
            autocrop(&tmp, 0);
            resize(&tmp, ceil(brushes[brushIndex].width * scales[brushIndex]), ceil(brushes[brushIndex].height * scales[brushIndex]));
        }
        else
            resize (&brushes[brushIndex], ceil(brushes[brushIndex].width * scales[brushIndex]), ceil(brushes[brushIndex].height * scales[brushIndex]));
    }
    
    for(brushIndex = 0 ; brushIndex < BRUSH_NUM ; brushIndex++)
    {
        i = 1 + sqrtf (brushes[brushIndex].width  * brushes[brushIndex].width +
                brushes[brushIndex].height * brushes[brushIndex].height);
        ppm_pad (&brushes[brushIndex], i-brushes[brushIndex].width, i-brushes[brushIndex].width,
           i - brushes[brushIndex].height, i - brushes[brushIndex].height, back);
        
        i = 1 + sqrtf(destBrushes[brushIndex].width * destBrushes[brushIndex].width +
                     destBrushes[brushIndex].height * destBrushes[brushIndex].height);
        ppm_pad(&destBrushes[brushIndex], i - destBrushes[brushIndex].width, i - destBrushes[brushIndex].width, i - destBrushes[brushIndex].height, i - destBrushes[brushIndex].height, back);
    }
    delete[] scales;
    assert(num_brushes > BRUSH_NUM);
    for (i = BRUSH_NUM; i < num_brushes; i++)
    {
        brushes[i].col = NULL;
        destBrushes[i].col = NULL;
        brushIndex = g_rand_int_range (random_generator, 0, BRUSH_NUM);
        //LOGE("## brushIndex = %d ##\n", brushIndex);
        ppm_copy (&brushes[brushIndex], &brushes[i]);
        ppm_copy(&destBrushes[brushIndex], &destBrushes[i]);
    }

    for (i = 0; i < runningvals.size_num; i++)
    {
        double sv;
        if (runningvals.size_num > 1)
            sv = i / (runningvals.size_num - 1.0);
        else 
            sv = 1.0;
        for (j = 0; j < runningvals.orient_num; j++)
        {
            float times;
            h = j + i * runningvals.orient_num;
            //ppm_copy(&brushes[h], &destBrushes[h]);
            free_rotate (&brushes[h],
                       startangle + j * anglespan / runningvals.orient_num);
            free_rotate(&destBrushes[h], startangle + j * anglespan / runningvals.orient_num); 
            rescale (&brushes[h],
                   ( sv      * runningvals.size_first +
                    (1.0-sv) * runningvals.size_last    ) / runningvals.size_last);
            autocrop (&brushes[h],1);
            times = ((float)destWidth) / gImageWidth;
            double first , last;
            first = runningvals.size_first * times;
            last = runningvals.size_last * times;
            //rescale(&destBrushes[h], (sv * first + (1.0 - sv) * last) / last);
            autocrop(&destBrushes[h], 1);
            resize(&destBrushes[h], brushes[h].width * times, brushes[h].height * times);
            //autocrop(&destBrushes[h], 1);
            //SS_SaveBrush("brush", h, brushes[h]);
            //SS_SaveBrush("destbrush", h, destBrushes[h]);
        }
        //SS_Pause(currentPausePoint);
    }

    /* Brush-debugging */
#if 0
    for (i = 0; i < num_brushes; i++)
    {
        char tmp[1000];
        g_snprintf (tmp, sizeof (tmp), "/tmp/_brush%03d.ppm", i);
        ppm_save (&brushes[i], tmp);
    }
#endif
    int edgeBrushNum = 0;
    for (i = 0; i < num_brushes; i++)
    {
        if (!runningvals.color_brushes)
        {
            prepare_brush (&brushes[i]);
            prepare_brush(&destBrushes[i]);
        }
        brushes_sum[i] = sum_brush (&brushes[i]);
        //LOGE("## brush sum %d : %f ##\n", i, brushes_sum[i]);
        //SS_Pause(currentPausePoint);
    }

    brush = &brushes[0];
    thissum = brushes_sum[0];

    maxbrushwidth = maxbrushheight = 0;
    for (i = 0; i < num_brushes; i++)
    {
        if (brushes[i].width > maxbrushwidth)
            maxbrushwidth = brushes[i].width;
        if (brushes[i].height > maxbrushheight)
            maxbrushheight = brushes[i].height;
    }

    for (i = 0; i < num_brushes; i++)
    {
        int xp, yp;
        guchar blk[3] = {0, 0, 0};

        xp = maxbrushwidth - brushes[i].width;
        yp = maxbrushheight - brushes[i].height;
        if (xp || yp)
        {
            float times = ((float)destWidth) / gImageWidth;
            int left = xp / 2;
            int right = xp - xp / 2;
            int top = yp / 2;
            int bottom = yp - yp / 2;
            ppm_pad (&brushes[i], xp / 2, xp - xp / 2, yp / 2, yp - yp / 2, blk);
            ppm_pad(&destBrushes[i], left * times, right * times, top * times, bottom * times, blk);
        }
        //SS_SaveBrush("padbrush", i, brushes[i]);
        //SS_SaveBrush("paddestbrush", i, destBrushes[i]);
        //SS_Pause(currentPausePoint);
    }

    if (dropshadow)
    {
        for (i = 0; i < num_brushes; i++)
        {
            shadows[i].col = NULL;
            ppm_copy (&brushes[i], &shadows[i]);
            ppm_apply_gamma (&shadows[i], 0, 1,1,0);
            ppm_pad (&shadows[i], shadowblur*2, shadowblur*2,
                   shadowblur*2, shadowblur*2, back);
            for (j = 0; j < shadowblur; j++)
                blur (&shadows[i], 2, 2);
#if 0
            autocrop (&shadows[i],1);
#endif
        }
#if 0
        maxbrushwidth += shadowdepth*3;
        maxbrushheight += shadowdepth*3;
#endif
    }

    /* For extra annoying debugging :-) */
#if 0
    ppm_save (brushes, "/tmp/__brush.ppm");
    if (shadows) ppm_save (shadows, "/tmp/__shadow.ppm");
    system ("xv /tmp/__brush.ppm & xv /tmp/__shadow.ppm & ");
#endif

    if (runningvals.general_paint_edges)
    {
        edgepad (p, maxbrushwidth, maxbrushwidth,
               maxbrushheight, maxbrushheight);
        if (img_has_alpha)
            edgepad (a, maxbrushwidth, maxbrushwidth,
                 maxbrushheight, maxbrushheight);
    }

    if (img_has_alpha)
    {
        /* Initially fully transparent */
        if (runningvals.general_background_type == BG_TYPE_TRANSPARENT)
        {
            guchar tmpcol[3] = {255, 255, 255};

            ppm_new (&atmp, a->width, a->height);
            fill (&atmp, tmpcol);
        }
        else
        {
            ppm_copy (a, &atmp);
        }
    }
/*
    if (runningvals.general_background_type == BG_TYPE_SOLID)
    {
        guchar tmpcol[3];

        ppm_new (&tmp, p->width, p->height);
        //TODO : change code
        gimp_rgb_get_uchar (&runningvals.color,
                          &tmpcol[0], &tmpcol[1], &tmpcol[2]);
        //TODO : end
        fill (&tmp, tmpcol);
    }
    else if (runningvals.general_background_type == BG_TYPE_KEEP_ORIGINAL)
    {
        ppm_copy (p, &tmp);
    }
    else
    {
#if 0
        if(!ppm_empty(&gBackground))
        {
            ppm_kill(&gBackground);
        }
        tmp = createBackground(runningvals, p->width, p->height);
        ppm_copy(&tmp, &gBackground);
        
#else
        if(ppm_empty(&gBackground))
        {
            gBackground = createBackground(runningvals, destWidth, destHeight);
            tmp = createBackground(runningvals, p->width, p->height);

        }
        else
        {
            tmp = createBackground(runningvals, p->width, p->height);
        }
#endif
    }
 */
    /*
    if(ppm_empty(&gBackground))
    {
        gBackground = createBackground(runningvals, destWidth, destHeight);
        tmp = createBackground(runningvals, p->width, p->height);
        
    }
    else
    {
        tmp = createBackground(runningvals, p->width, p->height);
    }
     */
    tmp.width = p->width;
    tmp.height = p->height;
    tmpWidth = tmp.width;
    tmpHeight = tmp.height;
    gBrushMaxWidth = maxbrushwidth;
    gBrushMaxHeight = maxbrushheight;
    LOGI("## gBrushMaxWidth = %d, gBrushMaxHeight = %d ##\n", gBrushMaxWidth, gBrushMaxHeight);
    
#ifdef MACOS
    static int startupDrawing = 1;
    if(startupDrawing)
    {
        //SS_SetCanvasBackground(currentCanvas, gBackground);
        startupDrawing = 0;
    }
#else
    
    if(repaintCallBack)
	{
	    (*repaintCallBack)("background", "initok");
  	}
#endif
    cx = p->width / 2;
    cy = p->height / 2;
    maxdist = sqrtf (cx * cx + cy * cy);

    switch (runningvals.orient_type)
    { 
    case ORIENTATION_VALUE:
        ppm_new (&dirmap, p->width, p->height);
        for (y = 0; y < dirmap.height; y++)
        {
            guchar *dstrow = &dirmap.col[y * dirmap.width * 3];
            guchar *srcrow = &p->col[y * p->width * 3];
            for (x = 0; x < dirmap.width; x++)
            {
                dstrow[x * 3] =
                (srcrow[x * 3] + srcrow[x * 3 + 1] + srcrow[x * 3 + 2]) / 3;
            }
        }
        break;

    case ORIENTATION_RADIUS:
        ppm_new (&dirmap, p->width, p->height);
        for (y = 0; y < dirmap.height; y++)
        {
            guchar *dstrow = &dirmap.col[y * dirmap.width * 3];
            double ysqr = (cy - y) * (cy - y);

            for (x = 0; x < dirmap.width; x++)
            {
                dstrow[x*3] = sqrt ((cx - x) * (cx - x) + ysqr) * 255 / maxdist;
            }
        }
        break;

    case ORIENTATION_RADIAL:
        ppm_new (&dirmap, p->width, p->height);
        for (y = 0; y < dirmap.height; y++)
        {
            guchar *dstrow = &dirmap.col[y * dirmap.width * 3];

            for (x = 0; x < dirmap.width; x++)
            {
                dstrow[x * 3] = (G_PI + atan2f (cy - y, cx - x)) *
                              255.0 / (G_PI * 2);
            }
        }
        break;

    case ORIENTATION_FLOWING:
        ppm_new (&dirmap, p->width / 6 + 5, p->height / 6 + 5);
        mkgrayplasma (&dirmap, 15);
        blur (&dirmap, 2, 2);
        blur (&dirmap, 2, 2);
        resize (&dirmap, p->width, p->height);
        blur (&dirmap, 2, 2);
        if (runningvals.general_paint_edges)
            edgepad (&dirmap, maxbrushwidth, maxbrushheight,
                 maxbrushwidth, maxbrushheight);
        break;

    case ORIENTATION_HUE:
        ppm_new (&dirmap, p->width, p->height);
        for (y = 0; y < dirmap.height; y++)
        {
            guchar *dstrow = &dirmap.col[y * dirmap.width * 3];
            guchar *srcrow = &p->col[y * p->width * 3];

            for (x = 0; x < dirmap.width; x++)
            {
                dstrow[x * 3] = get_hue (&srcrow[x * 3]);
            }
        }
        break;

    case ORIENTATION_ADAPTIVE:
    {
        guchar tmpcol[3] = {0, 0, 0};

        ppm_new (&dirmap, p->width, p->height);
        fill (&dirmap, tmpcol);
    }
    break;

    case ORIENTATION_MANUAL:
        ppm_new (&dirmap, p->width-maxbrushwidth*2, p->height-maxbrushheight*2);
        for (y = 0; y < dirmap.height; y++)
        {
            guchar *dstrow = &dirmap.col[y * dirmap.width * 3];
            double tmpy = y / (double)dirmap.height;
            for (x = 0; x < dirmap.width; x++)
            {
                dstrow[x * 3] = get_pixel_value(90 -
                                              get_direction(x /
                                                            (double)dirmap.width,
                                                            tmpy, 1));
            }
        }
        edgepad (&dirmap,
               maxbrushwidth, maxbrushwidth,
               maxbrushheight, maxbrushheight);
    break;
    }

    if (runningvals.size_type == SIZE_TYPE_VALUE)
    {
        ppm_new (&sizmap, p->width, p->height);
        for (y = 0; y < sizmap.height; y++)
        {
            guchar *dstrow = &sizmap.col[y * sizmap.width * 3];
            guchar *srcrow = &p->col[y * p->width * 3];

            for (x = 0; x < sizmap.width; x++)
            {
                dstrow[x * 3] =
                (srcrow[x * 3] + srcrow[x * 3 + 1] + srcrow[x * 3 + 2]) / 3;
            }
        }
    }
    else if (runningvals.size_type == SIZE_TYPE_RADIUS)
    {
        ppm_new (&sizmap, p->width, p->height);
        for (y = 0; y < sizmap.height; y++)
        {
            guchar *dstrow = &sizmap.col[y * sizmap.width * 3];
            double ysqr = (cy - y) * (cy - y);

            for (x = 0; x < sizmap.width; x++)
            {
                dstrow[x * 3] =
                sqrt ((cx - x) * (cx - x) + ysqr) * 255 / maxdist;
            }
        }
    }
    else if (runningvals.size_type == SIZE_TYPE_RADIAL)
    {
        ppm_new (&sizmap, p->width, p->height);
        for (y = 0; y < sizmap.height; y++)
        {
            guchar *dstrow = &sizmap.col[y * sizmap.width * 3];

            for (x = 0; x < sizmap.width; x++)
            {
                dstrow[x * 3] = (G_PI + atan2f (cy - y, cx - x)) *
                              255.0 / (G_PI * 2);
            }
        }
    }
    else if (runningvals.size_type == SIZE_TYPE_FLOWING)
    {
        ppm_new (&sizmap, p->width / 6 + 5, p->height / 6 + 5);
        mkgrayplasma (&sizmap, 15);
        blur (&sizmap, 2, 2);
        blur (&sizmap, 2, 2);
        resize (&sizmap, p->width, p->height);
        blur (&sizmap, 2, 2);
        if (runningvals.general_paint_edges)
            edgepad (&sizmap,
                 maxbrushwidth, maxbrushheight,
                 maxbrushwidth, maxbrushheight);
    }
    else if (runningvals.size_type == SIZE_TYPE_HUE)
    {
        ppm_new (&sizmap, p->width, p->height);
        for (y = 0; y < sizmap.height; y++)
        {
            guchar *dstrow = &sizmap.col[y * sizmap.width * 3];
            guchar *srcrow = &p->col[y * p->width * 3];

            for (x = 0; x < sizmap.width; x++)
            {
                dstrow[ x * 3] = get_hue (&srcrow[x * 3]);
            }
        }
    }
    else if (runningvals.size_type == SIZE_TYPE_ADAPTIVE)
    {
        guchar tmpcol[3] = {0, 0, 0};

        ppm_new (&sizmap, p->width, p->height);
        fill (&sizmap, tmpcol);
    }
    else if (runningvals.size_type == SIZE_TYPE_MANUAL)
    {
        ppm_new (&sizmap,
               p->width-maxbrushwidth * 2,
               p->height-maxbrushheight * 2);

        for (y = 0; y < sizmap.height; y++)
        {
            guchar *dstrow = &sizmap.col[y * sizmap.width * 3];
            double tmpy = y / (double)sizmap.height;

            for (x = 0; x < sizmap.width; x++)
            {
                dstrow[x * 3] = 255 * (1.0 - get_siz_from_pcvals (x / (double)sizmap.width, tmpy));
            }
        }
        edgepad (&sizmap,
               maxbrushwidth, maxbrushwidth,
               maxbrushheight, maxbrushheight);
    }
#if 0
    ppm_save(&sizmap, "/tmp/_sizmap.ppm");
#endif
    if (runningvals.place_type == PLACEMENT_TYPE_RANDOM)
    {
        i = tmp.width * tmp.height / (maxbrushwidth * maxbrushheight);
        i *= density;
    }
    else if (runningvals.place_type == PLACEMENT_TYPE_EVEN_DIST)
    {
        i = (int)(tmp.width * density / maxbrushwidth) *
          (int)(tmp.height * density / maxbrushheight);
        step = i;
#if 0
      g_printerr("step=%d i=%d\n", step, i);
#endif
    }

    if (i < 1)
        i = 1;

    max_progress = i;
    progstep = max_progress / 30;
    if (progstep < 10)
    progstep = 10;

    if (runningvals.place_type == PLACEMENT_TYPE_EVEN_DIST)
    {
        int j;

        xpos = g_new (int, i);
        ypos = g_new (int, i);
        for (j = 0; j < i; j++)
        {
            int factor = (int)(tmp.width * density / maxbrushwidth + 0.5);

            if (factor < 1)
                factor = 1;
            xpos[j] = maxbrushwidth/2 + (j % factor) * maxbrushwidth / density;
            ypos[j] = maxbrushheight/2 + (j / factor) * maxbrushheight / density;
        }
        for (j = 0; j < i; j++)
        {
            int a, b;
            //TODO : change code
            a = g_rand_int_range (random_generator, 0, i);
            //TODO : end
            b = xpos[j]; xpos[j] = xpos[a]; xpos[a] = b;
            b = ypos[j]; ypos[j] = ypos[a]; ypos[a] = b;
        }
    }
    //start calculate brush
    clearQuadTree(rootQuadTree);
    clearGrayPaintArea();
    for (; i && isBrushPaint(); i--)
    {
        //SS_Pause(currentPausePoint);
        if (runningvals.place_type == PLACEMENT_TYPE_RANDOM)
        {
            tx = g_rand_int_range (random_generator, maxbrushwidth / 2,
                                 tmp.width - maxbrushwidth / 2);
            ty = g_rand_int_range (random_generator, maxbrushheight / 2,
                                 tmp.height - maxbrushheight / 2);
        }
        else if (runningvals.place_type == PLACEMENT_TYPE_EVEN_DIST)
        {
            tx = xpos[i - 1];
            ty = ypos[i - 1];
        }
        if (runningvals.placement_center)
        {
            double z = g_rand_double_range (random_generator, 0, 0.75);
            tx = tx * (1.0 - z) + tmp.width / 2 * z;
            ty = ty * (1.0 - z) + tmp.height / 2 * z;
        }

        if ((tx < maxbrushwidth / 2)             ||
          (ty < maxbrushwidth / 2)             ||
          (tx + maxbrushwidth / 2 >= p->width) ||
          (ty + maxbrushheight / 2 >= p->height))
        {
#if 0
            LOGI("Internal Error; invalid coords: (%d,%d) i=%d\n", tx, ty, i);
#endif
            continue;
        }

        if (img_has_alpha)
        {
            if (a->col[ty * a->width * 3 + tx * 3] > 128)
                continue;
        }

        n = sn = on = 0;

        switch (runningvals.orient_type)
        {
        case ORIENTATION_RANDOM:
            on = g_rand_int_range (random_generator, 0, runningvals.orient_num);
            break;

        case ORIENTATION_VALUE:
        case ORIENTATION_RADIUS:
        case ORIENTATION_RADIAL:
        case ORIENTATION_FLOWING:
        case ORIENTATION_HUE:
        case ORIENTATION_MANUAL:
            on = runningvals.orient_num *
                 dirmap.col[ty * dirmap.width * 3 + tx * 3] / 256;
            break;

        case ORIENTATION_ADAPTIVE:
            break; /* Handled below */

        default:
            LOGI ("Internal error; Unknown orientationtype\n");
            on = 0;
          break;
        }

        switch (runningvals.size_type)
        {
        case SIZE_TYPE_RANDOM:
            sn = g_rand_int_range (random_generator, 0, runningvals.size_num);
          break;

        case SIZE_TYPE_VALUE:
        case SIZE_TYPE_RADIUS:
        case SIZE_TYPE_RADIAL:
        case SIZE_TYPE_FLOWING:
        case SIZE_TYPE_HUE:
        case SIZE_TYPE_MANUAL:
            sn = runningvals.size_num * sizmap.col[ty*sizmap.width*3+tx*3] / 256;
            break;

        case SIZE_TYPE_ADAPTIVE:
            break; /* Handled below */

        default:
            LOGI ("Internal error; Unknown size_type\n");
            sn = 0;
            break;
        }
        bool calculateEdge = rd.calculateOnEdge;
        /* Handle Adaptive selections */
        bool onEdge = !calculateEdge;
        if(!onEdge)
        {
            onEdge = brushOnEdge(&edgeDetectionMap, maxbrushwidth, maxbrushheight, tx - maxbrushwidth / 2, ty - maxbrushheight / 2);
            if(onEdge)
            {
                edgeBrushNum++;
            }
        }
        if(calculateEdge == true && onEdge == true)
        {
            //LOGI("## edge tx = %d, ty = %d ##\n", tx, ty);
        }
        if (runningvals.orient_type == ORIENTATION_ADAPTIVE)
        {
            
            if (runningvals.size_type == SIZE_TYPE_ADAPTIVE)
            {

                if(onEdge)
                {
                    n = choose_best_brush (&edgeDetectionMap, p, a, tx-maxbrushwidth/2,
                                  ty-maxbrushheight/2, brushes,
                                  num_brushes, brushes_sum, 0, 1);
                }
                else 
                {
                    n = -1;
                }
            }
            else
            {
                int st = sn * runningvals.orient_num;
                if(onEdge)
                {
                    n = choose_best_brush (&edgeDetectionMap, p, a, tx-maxbrushwidth/2,
                                    ty-maxbrushheight/2, brushes,
                                    st+runningvals.orient_num, brushes_sum,
                                    st, 1);
                }
                else 
                {
                    n = -1;
                }
            }
        }
        else
        {
            if(onEdge)
            {
                if (runningvals.size_type == SIZE_TYPE_ADAPTIVE)
                {
                    n = choose_best_brush (&edgeDetectionMap, p, a, tx-maxbrushwidth/2,
                                    ty-maxbrushheight/2, brushes,
                                    num_brushes, brushes_sum,
                                    on, runningvals.orient_num);
                }
                else
                    n = sn * runningvals.orient_num + on;
            }
            else 
            {
                n = -1;
            }
        }
        /* Should never happen, but hey... */
        if (n < 0)
        {
            //change for edge detection
            //n = 0;
            continue;
        }
        else if (n >= num_brushes)
            n = num_brushes - 1;

        tx -= maxbrushwidth/2;
        ty -= maxbrushheight/2;

        brush = &brushes[n];
        if (dropshadow)
            shadow = &shadows[n];
        thissum = brushes_sum[n];

        /* Calculate color - avg. of in-brush pixels */
        if (runningvals.color_type == 0)
        {
            r = g = b = 0;
            for (y = 0; y < brush->height; y++)
            {
                guchar *row = &p->col[(ty + y) * p->width * 3];

                for (x = 0; x < brush->width; x++)
                {
                    int k = (tx + x) * 3;
                    double v;

                    if ((h = brush->col[y * brush->width * 3 + x * 3]))
                    {
                        v = h / 255.0;
                        r += row[k+0] * v;
                        g += row[k+1] * v;
                        b += row[k+2] * v;
                    }
                }
            }
            if(thissum != 0)
            {
                 r = r * 255.0 / thissum;
                 g = g * 255.0 / thissum;
                 b = b * 255.0 / thissum;
            }
        }
        else if (runningvals.color_type == 1)
        {
            guchar *pixel;

            y = ty + (brush->height / 2);
            x = tx + (brush->width / 2);
            pixel = &p->col[y*p->width * 3 + x * 3];
            r = pixel[0];
            g = pixel[1];
            b = pixel[2];
        }
        else
        {
            /* No such color_type! */
            r = g = b = 0;
        }
        if (runningvals.color_noise > 0.0)
        {
            double v = runningvals.color_noise;
#define BOUNDS(a) (((a) < 0) ? (a) : ((a) > 255) ? 255 : (a))
#define MYASSIGN(a) \
    { \
        a = a + g_rand_double_range (random_generator, -v/2.0, v/2.0); \
        a = BOUNDS(a) ;       \
    }
            MYASSIGN (r);
            MYASSIGN (g);
            MYASSIGN (b);
#undef BOUNDS
#undef MYASSIGN
        }
        //debug for change
        //apply_brush (brush, shadow, &tmp, &atmp, tx,ty, r,g,b);
	    BrushProperty bp;
	    bp.brush = brush;
        bp.destBrush = &destBrushes[n];
	    bp.b = b;
	    bp.g = g;
	    bp.r = r;
	    bp.shadow = shadow;
	    bp.tx = tx;
	    bp.ty = ty;
	    //gBrushProperties.push_back(bp);
        addBrushPropertyToGrayPaintArea(bp);
        //end
        if (runningvals.general_tileable && runningvals.general_paint_edges)
        {
            int orig_width = tmp.width - 2 * maxbrushwidth;
            int orig_height = tmp.height - 2 * maxbrushheight;
            int dox = 0, doy = 0;

            if (tx < maxbrushwidth)
            {
                apply_brush (brush, shadow, &tmp, &atmp, tx+orig_width,ty, r,g,b);
                dox = -1;
            }
            else if (tx > orig_width)
            {
                apply_brush (brush, shadow, &tmp, &atmp, tx-orig_width,ty, r,g,b);
                dox = 1;
            }
            if (ty < maxbrushheight)
            {
                apply_brush (brush, shadow, &tmp, &atmp, tx,ty+orig_height, r,g,b);
                doy = 1;
            }
            else if (ty > orig_height)
            {
                apply_brush (brush, shadow, &tmp, &atmp, tx,ty-orig_height, r,g,b);
                doy = -1;
            }
            if (doy)
            {
                if (dox < 0)
                    apply_brush (brush, shadow, &tmp, &atmp,
                             tx+orig_width, ty + doy * orig_height, r, g, b);
                if (dox > 0)
                    apply_brush (brush, shadow, &tmp, &atmp,
                              tx-orig_width, ty + doy * orig_height, r, g, b);
            }
        }
    }
    LOGI("### edge brush num = %d ##\n", edgeBrushNum);
  	//debug for change
	//apply_brush (brush, shadow, &tmp, &atmp, tx,ty, r,g,b);
    
    tmpWidth = tmp.width;
    tmpHeight = tmp.height;
    
    LOGI("############# start create brush piece w = %d, h = %d ##############\n", tmpWidth, tmpHeight);
    /*
    //gBrushProperties.sort(_BrushPropertyComp());
     */
    //use separate function to sort property
    //clearQuadTree(rootQuadTree);
    //clearGrayPaintArea();
	std::list<BrushProperty>::iterator it;
    //for(it = gBrushProperties.begin() ; it != gBrushProperties.end(); it++)
    //{
    //    addBrushPropertyToGrayPaintArea(*it);
    //}
    separatePaintArea();
    //end
    LOGI("## set pause point before ##");
    SS_SetComputationPausePoint(currentPausePoint);
    LOGI("## set pause point end ");
#ifdef MACOS
    //SE_startBrushPaint();
#else
	if(repaintCallBack)
	{
        LOGI("## call repaint callback ##\n");
		(*repaintCallBack)("apply_brush", "start");
        LOGI("## call repaint callback end ##\n");
	}
#endif
    int drawing_speed = SS_GetDrawingSpeed();
    int drawing_index = 0;
    brushIndex = 0;
    SS_BrushList* brushList = NULL;
    if(gBrushProperties.size() > 0)
        brushList = SS_BrushListCreate();
    int sequence = 0;
    SS_BrushListPool* brushListPool = SS_GetBrushListPool();
	for(it = gBrushProperties.begin(); it != gBrushProperties.end() && isBrushPaint(); it++)
	{
		BrushProperty bp = *it;
        //SS_Pause(currentPausePoint);
#if 1
		//apply_brush (bp.brush, bp.shadow, &tmp, &atmp, bp.tx,bp.ty, bp.r,bp.g,bp.b);
        float startRealPicX = maxbrushwidth;
        float startRealPicY = maxbrushheight;
        float bpx = (float)bp.tx - startRealPicX;
        float bpy = (float)bp.ty - startRealPicY;
        float bpright = bpx + (float)bp.brush->width;
        float bpbottom = bpy + (float)bp.brush->height;
        float newtx = bpx * destWidth / gImageWidth;
        float newty = bpy * destHeight / gImageHeight;
        float newright = bpright * destWidth / gImageWidth;
        float newbottom = bpbottom * destHeight / gImageHeight;
        int neww = newright - newtx;
        int newh = newbottom - newty;
        /*
        ppm_t newbrush;
        newbrush.width = 0;
        newbrush.height = 0;
        newbrush.col = NULL;
        ppm_copy(bp.brush, &newbrush);
        resize(&newbrush, neww, newh);
        ppm_kill(&newbrush);
        SE_SaveBrush("drawingnewbrush", brushIndex, newbrush);
        brushIndex++;
         */
        //apply_brush_area(bp.destBrush, bp.shadow, &gBackground,NULL, newtx, newty, bp.r, bp.g, bp.b);
        
        BrushPiece brushPiece;
        brushPiece.data.col = NULL;
		brushPiece.x = newtx;
		brushPiece.y = newty;
		brushPiece.w = tmp.width;
		brushPiece.h = tmp.height;
        brushPiece.mbw = maxbrushwidth;
        brushPiece.mbh = maxbrushheight;
        brushPiece.r = bp.r;
        brushPiece.g = bp.g;
        brushPiece.b = bp.b;
        brushPiece.last_piece = 0;
        ppm_copy(bp.destBrush, &brushPiece.data);
        SS_AddBrushPiece(brushList, brushPiece);
        if(drawing_index == (drawing_speed - 1))
        {
            SS_AddBrushList(brushListPool, brushList);
            
            //SS_MyRect* rectArray = NULL;
            //int rectCount = 0;
            //SS_GetBrushListRects(brushList, &rectArray, &rectCount);
            //SS_DrawInMainThread(rectArray, rectCount);
            
            drawing_index = 0;
            brushList = SS_BrushListCreate();
        }
        else
            drawing_index++;
        
#else
        
        apply_brush (bp.brush, bp.shadow, &tmp, &atmp, bp.tx,bp.ty, bp.r,bp.g,bp.b);
		BrushPiece brushPiece;
		brushPiece.x = bp.tx;
		brushPiece.y = bp.ty;
		brushPiece.w = tmp.width;
		brushPiece.h = tmp.height;
        brushPiece.mbw = maxbrushwidth;
        brushPiece.mbh = maxbrushheight;
        brushPiece.last_piece = 0;
	    ppm_new(&brushPiece.data, bp.brush->width, bp.brush->height);
        ppm_new_alpha(&brushPiece.alpha, bp.brush->width, bp.brush->height);
	    ppm_copy_xy(&tmp, &brushPiece.data, bp.tx, bp.ty, bp.brush->width, bp.brush->height, 0, 0);    	
        createAlpha(bp.brush, &brushPiece.alpha);
        //addBrushPiece(brushPiece);
        SE_startBrushPaintInMainQueue(brushPiece);
#endif 
        
	}
    if(brushList)
    {
        SS_AddBrushList(brushListPool, brushList);
        
        //SS_MyRect* rectArray = NULL;
        //int rectCount = 0;
        //SS_GetBrushListRects(brushList, &rectArray, &rectCount);
        //SS_DrawInMainThread(rectArray, rectCount);
        
        //SS_DrawBrushList(brushList, sequence);
        //SE_BrushListRelease(brushList);
    }
    gBrushProperties.clear();
    if(!isBrushPaint())
    {
        clearBrushPiece(); 
    }
    for (i = 0; i < num_brushes; i++)
    {
        ppm_kill (&brushes[i]);
        ppm_kill(&destBrushes[i]);
    }
    g_free (brushes);
    g_free(destBrushes);
    g_free (shadows);
    g_free (brushes_sum);

    g_free (xpos);
    g_free (ypos);
    /*
    if (runningvals.general_paint_edges)
    {
        crop (&tmp,
            maxbrushwidth, maxbrushheight,
            p->width - maxbrushwidth, p->height - maxbrushheight);
        if (img_has_alpha)
            crop (&atmp,
               maxbrushwidth, maxbrushheight,
              atmp.width - maxbrushwidth, atmp.height - maxbrushheight);
    }
*/
    ppm_kill (p);
    p->width = tmp.width;
    p->height = tmp.height;
    p->col = tmp.col;

    if (img_has_alpha)
    {
        ppm_kill (a);
        a->width = atmp.width;
        a->height = atmp.height;
        a->col = atmp.col;
    }

    relief = runningvals.paper_relief / 100.0;
    if (relief > 0.001)
    {
        scale = runningvals.paper_scale / 100.0;

        if (PPM_IS_INITED (&paper_ppm))
        {
            tmp = paper_ppm;
            paper_ppm.col = NULL;
        }
        else
        {
            tmp.col = NULL;
            ppm_load (runningvals.selected_paper, &tmp);
            resize (&tmp, tmp.width * scale, tmp.height * scale);
            if (runningvals.paper_invert)
                ppm_apply_gamma (&tmp, -1.0, 1,1,1);
        }
        for (x = 0; x < p->width; x++)
        {
            double h, v;
            int    px = x % tmp.width, py;

            for (y = 0; y < p->height; y++)
            {
                int k = y * p->width * 3 + x * 3;

                py = y % tmp.height;
                if (runningvals.paper_overlay)
                    h = (tmp.col[py * tmp.width * 3 + px * 3]-128) * relief;
                else
                    h = (tmp.col[py * tmp.width * 3 + px * 3] -
                     (int)tmp.col[((py + 1) % tmp.height) * tmp.width * 3 +
                                  ((px + 1) % tmp.width) * 3]) /
                     -2.0 * relief;
                if (h <= 0.0)
                {
                    v = 1.0 + h/128.0;
                    if (v < 0.0)
                        v = 0.0;
                    else if (v > 1.0)
                        v = 1.0;
                    p->col[k+0] *= v;
                    p->col[k+1] *= v;
                    p->col[k+2] *= v;
                }
                else
                {
                    v = h/128.0;
                    if (v < 0.0)
                        v = 0.0;
                    else if (v > 1.0)
                        v = 1.0;
                    p->col[k+0] = p->col[k+0] * (1.0-v) + 255 * v;
                    p->col[k+1] = p->col[k+1] * (1.0-v) + 255 * v;
                    p->col[k+2] = p->col[k+2] * (1.0-v) + 255 * v;
                }
            }
        }
        ppm_kill (&tmp);
    }

    ppm_kill (&paper_ppm);
    ppm_kill (&dirmap);
    ppm_kill (&sizmap);
    if(rd.calculateOnEdge)
        ppm_kill(&edgeDetectionMap);
    running = 0;
}
