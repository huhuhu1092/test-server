//
//  PGMDataReader.h
//  TestImageView
//
//  Created by 陈勇 on 11-10-7.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//
#ifndef __PGMDATAREADER_H
#define __PGMDATAREADER_H

#ifdef __cplusplus
extern "C" {
#endif
#include "gimpressionist.h"
struct ppm;

extern void getPgmData(const char* fn, const char** outData, int* outLen);
extern void SE_setBackground(struct ppm* background);
extern void SE_startBrushPaint();
extern void SE_startBrushPaintInMainQueue(BrushPiece inputbp);
#ifdef __cplusplus
}
#endif
#endif