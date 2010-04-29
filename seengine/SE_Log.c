#include "SE_Log.h"
#include "SE_Common.h"
#include <stdarg.h>
void LOGE(const char* fmt, ...)
{
    va_list ap;
    char buf[4096];
    va_start(ap, fmt);
    memset(buf, 0, 4096);
    vsnprintf(buf, 4096, fmt, ap);
    buf[4096 - 1] = '\0';
    va_end(ap);
    fprintf(stderr, "%s", buf);
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
    fprintf(stderr, "%s", buf);
}
void LOGVA(const char* fmt, va_list ap)
{
    char buf[4096];
    memset(buf, 0, 4096);
    vsnprintf(buf, 4096, fmt, ap);
    buf[4096 - 1] = '\0';
    fprintf(stderr, "%s", buf);
}
