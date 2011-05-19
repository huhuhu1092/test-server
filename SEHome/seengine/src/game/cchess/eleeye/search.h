/*
search.h/search.cpp - Source Code for ElephantEye, Part VIII

ElephantEye - a Chinese Chess Program (UCCI Engine)
Designed by Morning Yellow, Version: 3.15, Last Modified: Jul. 2008
Copyright (C) 2004-2008 www.elephantbase.net

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include "../base/base.h"
#include "../base/rc4prng.h"
#include "ucci.h"
#include "pregen.h"
#include "position.h"

#ifndef SEARCH_H
#define SEARCH_H

#ifdef _WIN32
  #include <windows.h>
#else
  #define WINAPI
#endif

// ����ģʽ
const int GO_MODE_INFINITY = 0;
const int GO_MODE_NODES = 1;
const int GO_MODE_TIMER = 2;

// ����ǰ�����õ�ȫ�ֱ�����ָ����������
struct SearchStruct {
  PositionStruct pos;                // �д������ľ���
  bool bQuit, bPonder, bDraw;        // �Ƿ��յ��˳�ָ���̨˼��ģʽ�����ģʽ
  bool bBatch, bDebug;               // �Ƿ�������ģʽ�͵���ģʽ
  bool bUseHash, bUseBook;           // �Ƿ�ʹ���û���ü��Ϳ��ֿ�
  bool bNullMove, bKnowledge;        // �Ƿ���Ųü���ʹ�þ�������֪ʶ
  bool bAlwaysCheck;                 // �Ƿ�ֻ����(������ɱ�ž�)
  bool bIdle;                        // �Ƿ����
  RC4Struct rc4Random;               // �����
  int nGoMode, nNodes, nCountMask;   // ����ģʽ���������
  int nProperTimer, nMaxTimer;       // �ƻ�ʹ��ʱ��
  int nRandomMask, nBanMoves;        // ���������λ�ͽ�����
  uint16_t wmvBanList[MAX_MOVE_NUM]; // �����б�
  char szBookFile[1024];             // ���ֿ�
  const char *(WINAPI *GetEngineName)(void);                     // ����������������API����ָ��
  void (WINAPI *PreEvaluate)(PositionStruct *, PreEvalStruct *); // ����Ԥ����API����ָ��
  int (WINAPI *Evaluate)(const PositionStruct *, int, int);      // ��������API����ָ��
};

extern SearchStruct Search;

// UCCI���湹�����
void BuildPos(PositionStruct &pos, const UcciCommStruct &UcciComm);

// UCCI֧�� - ���Ҷ�ӽ��ľ�����Ϣ
void PopLeaf(PositionStruct &pos);

// ��������������
void SearchMain(int nDepth);

#endif
