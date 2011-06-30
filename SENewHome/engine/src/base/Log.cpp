#include "base/Log.h"
#include "base/Type.h"
#include <stdarg.h>
#include <wchar.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#if defined(WIN32)
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>
#endif
#ifndef ANDROID
static void outputString(const char* buf, int size)
{
#if defined(WIN32)
	wchar_t wbuf[512];
	memset(wbuf, 0, 512 * sizeof(wchar_t));
    _snwprintf(wbuf, 512, L"%S", buf);
	OutputDebugString(wbuf);
#else
    fprintf(stderr, "%s", buf);
#endif
}
void LOGE(const char* fmt, ...)
{
    va_list ap;
    int8_t buf[4096];
    va_start(ap, fmt);
    memset(buf, 0, 4096);
    vsnprintf(buf, 4096, fmt, ap);
    buf[4096 - 1] = '\0';
    va_end(ap);
	outputString(buf, strlen(buf));
    exit(-1);
}
void LOGI(const char* fmt, ...)
{
    va_list ap;
    int8_t buf[4096];
    va_start(ap, fmt);
    memset(buf, 0, 4096);
    vsnprintf(buf, 4096, fmt, ap);
    buf[4096 - 1] = '\0';
    va_end(ap);
    outputString(buf, strlen(buf));
}
void LOGVA(const char* fmt, va_list ap)
{
    int8_t buf[4096];
    memset(buf, 0, 4096);
    vsnprintf(buf, 4096, fmt, ap);
    buf[4096 - 1] = '\0';
    outputString(buf, strlen(buf));
}
#endif
