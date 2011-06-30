#ifndef OMS_LOG_H
#define OMS_LOG_H
#include <stdarg.h>
#ifdef __cplusplus
extern "C" {
#endif
#ifdef ANDROID
#include <cutils/log.h>
#else
extern void LOGE(const char* fmt, ...);
extern void LOGI(const char* fmt, ...);
extern void LOGVA(const char* fmt, va_list ap);
#endif
#ifdef __cplusplus
}
#endif
#endif
