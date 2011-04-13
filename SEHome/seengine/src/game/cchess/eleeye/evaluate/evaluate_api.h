#ifndef EVALUATE_API_H
#define EVALUATE_API_H

#if _WIN32
#include <windows.h>
struct PositionStruct;
struct PositionEvalStruct;
extern "C" void WINAPI PreEvaluate(PositionStruct *lppos, PreEvalStruct *lpPreEval);
extern "C" int WINAPI Evaluate(const PositionStruct *lppos, int vlAlpha, int vlBeta);
extern "C" const char *WINAPI GetEngineName(void);
#else
#define WINAPI
extern "C" void WINAPI PreEvaluate(PositionStruct *lppos, PreEvalStruct *lpPreEval);
extern "C" int Evaluate(const PositionStruct *lppos, int vlAlpha, int vlBeta);
extern "C" const char *GetEngineName(void);

#endif

#endif
