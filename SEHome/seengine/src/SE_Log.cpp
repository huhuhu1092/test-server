#include "SE_Log.h"
#include "SE_Common.h"
#include <stdarg.h>
#if defined(WIN32)
#include <windows.h>
#endif
#ifndef ANDROID
void LOGE(const char* fmt, ...)
{
    va_list ap;
    char buf[4096];
    va_start(ap, fmt);
    memset(buf, 0, 4096);
    vsnprintf(buf, 4096, fmt, ap);
    buf[4096 - 1] = '\0';
    va_end(ap);
#if defined(WIN32)
	OutputDebugString(buf);
#else
    fprintf(stderr, "%s", buf);
#endif
    exit(-1);
}
void LOGI(const char* fmt, ...)
{
    va_list ap;
    char buf[4096];
    va_start(ap, fmt);
    memset(buf, 0, 4096);
    vsnprintf(buf, 4096, fmt, ap);
    buf[4096 - 1] = '\0';
    va_end(ap);
#if defined(WIN32)
	OutputDebugString(buf);
#else
    fprintf(stderr, "%s", buf);
#endif
}
void LOGVA(const char* fmt, va_list ap)
{
    char buf[4096];
    memset(buf, 0, 4096);
    vsnprintf(buf, 4096, fmt, ap);
    buf[4096 - 1] = '\0';
#if defined(WIN32)
	OutputDebugString(buf);
#else
    fprintf(stderr, "%s", buf);
#endif
}
#endif
