#ifndef TYPE_H
#define TYPE_H
typedef char gchar;
typedef unsigned char guchar;
typedef double gdouble;
typedef unsigned int guint32;
typedef int gint32
#ifdef WIN32
typedef __int64 gint64;
typedef unsigned __int64 guint64;
#endif
typedef struct _GimpRGB GimpRGB;
struct _GimpRGB
{
    gdouble r, g, b, a;
};
#define g_warning printf
#define g_printerer printf
#define g_malloc malloc
#define g_free free
#endif
