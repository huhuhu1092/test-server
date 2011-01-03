#ifndef SE_CHECKXML_H
#define SE_CHECKXML_H
#if defined(WIN32)
#include <windows.h>
#include <tchar.h>
extern void checkXml();
#else
extern void checkXml(const char* inputDir, const char* outputDir);
#endif
#endif
