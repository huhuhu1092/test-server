#ifndef STYPE_H
#define STYPE_H
#if !defined(WIN32)
#include <stdint.h>
#include <linux/stddef.h>
#else
typedef unsigned char uint8_t;
typedef signed char int8_t;
typedef unsigned short uint16_t;
typedef signed short int16_t;
typedef unsigned int uint32_t;
typedef signed int int32_t;
#endif
#endif
