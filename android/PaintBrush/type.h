#ifndef TYPE_H
#define TYPE_H
#include <stdlib.h>
#include <stdio.h>
#ifdef WIN32
#include <windows.h>
#endif
#ifdef ANDROID
#include <android/log.h>
#endif
#ifdef __cplusplus
extern "C" {
#endif
typedef char gchar;
typedef unsigned char guchar;
typedef double gdouble;
typedef unsigned int guint32;
typedef int gint32;
typedef unsigned short guint16;
typedef gint32 gint;
typedef guint32 guint;
typedef long glong;
typedef int gboolean;
#ifdef WIN32
typedef __int64 gint64;
typedef unsigned __int64 guint64;
#endif
typedef struct _GimpRGB GimpRGB;
struct _GimpRGB
{
    gdouble r, g, b, a;
};
#ifdef ANDROID
#define  LOG_TAG    "libhello-jni"
#define  LOGI(...)  __android_log_print(ANDROID_LOG_INFO,LOG_TAG,__VA_ARGS__)
#define  LOGE(...)  __android_log_print(ANDROID_LOG_ERROR,LOG_TAG,__VA_ARGS__)
#define g_warning LOGI
#define g_printerer LOGI
#define g_printerr LOGI
#define g_message LOGI
#else
#define g_warning printf
#define g_printerer printf
#define g_printerr printf
#define g_message printf
#define LOGI printf
#define LOGE printf
#endif
#define g_malloc malloc
#define g_free free
#define g_new(struct_type, n_structs) (struct_type*)g_malloc(sizeof(struct_type) * n_structs)
#define  G_PI 3.1415926
#define  G_PI_2 3.1415926 * 2
#define CLAMP(x,l,u) ((x)<(l)?(l):((x)>(u)?(u):(x)))
#define ROUND(x) ((int) ((x) + 0.5))
#define gimp_rad_to_deg(angle) ((angle) * 360.0 / (2.0 * G_PI))
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define CLAMP_UP_TO(x, max) (CLAMP((x),(0),(max-1)))
#ifdef __cplusplus
}
#endif
#endif
