#ifndef SE_LOG_H
#define SE_LOG_H
#include <stdarg.h>
#ifdef __cplusplus
extern "C" {
#endif
extern void LOGE(const char* fmt, ...);
extern void LOGI(const char* fmt, ...);
extern void LOGVA(const char* fmt, va_list ap);
#ifdef __cplusplus
}
#endif
#endif
