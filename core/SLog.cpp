#include "SLog.h"
#include <stdio.h>
#include <stdarg.h>
static void output(const char* format, va_list ap)
{
    static const int BUFSIZE = 2048;
    char buffer[BUFSIZE];
    vsnprintf(buffer, BUFSIZE - 1, format, ap);
    buffer[BUFSIZE - 1] = 0;
    fprintf(stderr, "%s", buffer);
}
void SLog::msg(const char* format, ...)
{
    va_list ap;
    va_start(ap, format);
    output(format, ap);
}
