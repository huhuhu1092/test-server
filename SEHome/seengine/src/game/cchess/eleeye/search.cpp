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

#include <stdio.h>
#include "../base/base2.h"
#include "ucci.h"
#include "pregen.h"
#include "position.h"
#include "hash.h"
#include "book.h"
#include "movesort.h"
#include "search.h"
#include "../SE_Pipe.h"
const int IID_DEPTH = 2;         // �ڲ�������������
const int SMP_DEPTH = 6;         // �������������
const int UNCHANGED_DEPTH = 4;   // δ�ı�����ŷ������

const int DROPDOWN_VALUE = 20;   // ���ķ�ֵ
const int RESIGN_VALUE = 300;    // ����ķ�ֵ
const int DRAW_OFFER_VALUE = 40; // ��͵ķ�ֵ

SearchStruct Search;

// ������Ϣ���Ƿ�װ��ģ���ڲ���
static struct {
  int64_t llTime;                     // ��ʱ��
  bool bStop, bPonderStop;            // ��ֹ�źźͺ�̨˼����Ϊ����ֹ�ź�
  bool bPopPv, bPopCurrMove;          // �Ƿ����pv��currmove
  int nPopDepth, vlPopValue;          // �������Ⱥͷ�ֵ
  int nAllNodes, nMainNodes;          // �ܽ���������������Ľ����
  int nUnchanged;                     // δ�ı�����ŷ������
  uint16_t wmvPvLine[MAX_MOVE_NUM];   // ��Ҫ����·���ϵ��ŷ��б�
  uint16_t wmvKiller[LIMIT_DEPTH][2]; // ɱ���ŷ���
  MoveSortStruct MoveSort;            // �������ŷ�����
} Search2;

void BuildPos(PositionStruct &pos, const UcciCommStruct &UcciComm) {
  int i, mv;
  pos.FromFen(UcciComm.szFenStr);
  for (i = 0; i < UcciComm.nMoveNum; i ++) {
    mv = COORD_MOVE(UcciComm.lpdwMovesCoord[i]);
    if (mv == 0) {
      break;
    }
    if (pos.ucpcSquares[SRC(mv)] == 0) {
      break;
    }
    pos.MakeMove(mv);
  }
}

// �ж�����
static bool Interrupt(void) {
  UcciCommStruct UcciComm;
  PositionStruct posProbe;
  if (Search.bIdle) {
    Idle();
  }
  switch (Search.nGoMode) {
  case GO_MODE_NODES:
    if (Search2.nAllNodes > Search.nNodes) {
      Search2.bStop = true;
      return true;
    }
    break;
  case GO_MODE_TIMER:
    if (!Search.bPonder && (int) (GetTime() - Search2.llTime) > Search.nMaxTimer) {
      Search2.bStop = true;
      return true;
    }
    break;
  default:
    break;
  }
  if (Search.bBatch) {
    return false;
  }

  // �������������ģʽ����ô�ȵ���UCCI���ͳ������ж��Ƿ���ֹ
  switch (BusyLine(UcciComm, Search.bDebug)) {
  case UCCI_COMM_ISREADY:
    // "isready"ָ��ʵ����û������
    //printf("readyok\n");
    //fflush(stdout);
	  pipeOutputWrite("readyok\n");
    return false;
  case UCCI_COMM_PONDERHIT:
    // "ponderhit"ָ��������ʱ���ܣ����"SearchMain()"������Ϊ�Ѿ��������㹻��ʱ�䣬 ��ô������ֹ�ź�
    if (Search2.bPonderStop) {
      Search2.bStop = true;
      return true;
    } else {
      Search.bPonder = false;
      return false;
    }
  case UCCI_COMM_PONDERHIT_DRAW:
    // "ponderhit draw"ָ��������ʱ���ܣ���������ͱ�־
    Search.bDraw = true;
    if (Search2.bPonderStop) {
      Search2.bStop = true;
      return true;
    } else {
      Search.bPonder = false;
      return false;
    }
  case UCCI_COMM_STOP:
    // "stop"ָ�����ֹ�ź�
    Search2.bStop = true;
    return true;
  case UCCI_COMM_PROBE:
    // "probe"ָ�����Hash����Ϣ
    BuildPos(posProbe, UcciComm);
    PopHash(posProbe);
    return false;
  case UCCI_COMM_QUIT:
    // "quit"ָ����˳��ź�
    Search.bQuit = Search2.bStop = true;
    return true;
  default:
    return false;
  }
}

// �����Ҫ����
static void PopPvLine(int nDepth = 0, int vl = 0) {
  uint16_t *lpwmv;
  uint32_t dwMoveStr;
  // �����δ�ﵽ��Ҫ�������ȣ���ô��¼����Ⱥͷ�ֵ���Ժ������
  if (nDepth > 0 && !Search2.bPopPv && !Search.bDebug) {
    Search2.nPopDepth = nDepth;
    Search2.vlPopValue = vl;
    return;
  }
  // ���ʱ������������
  //printf("info time %d nodes %d\n", (int) (GetTime() - Search2.llTime), Search2.nAllNodes);
  //fflush(stdout);
  pipeOutputWrite("info time %d nodes %d\n", (int) (GetTime() - Search2.llTime), Search2.nAllNodes);
  if (nDepth == 0) {
    // ��������������������������Ѿ����������ô���������
    if (Search2.nPopDepth == 0) {
      return;
    }
    // ��ȡ��ǰû���������Ⱥͷ�ֵ
    nDepth = Search2.nPopDepth;
    vl = Search2.vlPopValue;
  } else {
    // �ﵽ��Ҫ�������ȣ���ô�Ժ󲻱������
    Search2.nPopDepth = Search2.vlPopValue = 0;
  }
  //printf("info depth %d score %d pv", nDepth, vl);
  pipeOutputWrite("info depth %d score %d pv", nDepth, vl);
  lpwmv = Search2.wmvPvLine;
  while (*lpwmv != 0) {
    dwMoveStr = MOVE_COORD(*lpwmv);
    //printf(" %.4s", (const char *) &dwMoveStr);
	pipeOutputWrite(" %.4s", (const char *) &dwMoveStr);
    lpwmv ++;
  }
  //printf("\n");
  //fflush(stdout);
  pipeOutputWrite("\n");
}

// �޺��ü�
static int HarmlessPruning(const PositionStruct &pos, int vlBeta) {
  int vl, vlRep;

  // 1. ɱ�岽���ü���
  vl = pos.nDistance - MATE_VALUE;
  if (vl >= vlBeta) {
    return vl;
  }

  // 2. ����ü���
  if (pos.IsDraw()) {
    return 0; // ��ȫ��������ﲻ��"pos.DrawValue()";
  }

  // 3. �ظ��ü���
  vlRep = pos.RepStatus();
  if (vlRep > 0) {
    return pos.RepValue(vlRep);
  }

  return -MATE_VALUE;
}

// �����;������ۺ���
inline int Evaluate(const PositionStruct &pos, int vlAlpha, int vlBeta) {
  int vl;
  vl = Search.bKnowledge ? Search.Evaluate(&pos, vlAlpha, vlBeta) : pos.Material();
  return vl == pos.DrawValue() ? vl - 1 : vl;
}

// ��̬��������
static int SearchQuiesc(PositionStruct &pos, int vlAlpha, int vlBeta) {
  int vlBest, vl, mv;
  bool bInCheck;
  MoveSortStruct MoveSort;  
  // ��̬�������̰������¼������裺
  Search2.nAllNodes ++;

  // 1. �޺��ü���
  vl = HarmlessPruning(pos, vlBeta);
  if (vl > -MATE_VALUE) {
    return vl;
  }

#ifdef HASH_QUIESC
  // 3. �û��ü���
  vl = ProbeHashQ(pos, vlAlpha, vlBeta);
  if (Search.bUseHash && vl > -MATE_VALUE) {
    return vl;
  }
#endif

  // 3-a. ����ɱ����(���û������������һ��û�����Է����򷵻�ʤ����ֵ)��
  if (Search.bAlwaysCheck && pos.LastMove().ChkChs <= 0 &&
      pos.rbsList[pos.nMoveNum - 2].mvs.ChkChs <= 0) {
    return MATE_VALUE - pos.nDistance;
  }

  // 4. �ﵽ������ȣ�ֱ�ӷ�������ֵ��
  if (pos.nDistance == LIMIT_DEPTH) {
    return Evaluate(pos, vlAlpha, vlBeta);
  }
  __ASSERT(Search.pos.nDistance < LIMIT_DEPTH);

  // 5. ��ʼ����
  vlBest = -MATE_VALUE;
  bInCheck = (pos.LastMove().ChkChs > 0);

  // 6. ���ڱ������ľ��棬����ȫ���ŷ���
  if (bInCheck) {
    MoveSort.InitAll(pos);
  } else {

    // 7. ����δ�������ľ��棬�������ŷ�ǰ���ȳ��Կ���(��������)�����Ծ��������ۣ�
    vl = Evaluate(pos, vlAlpha, vlBeta);
    __ASSERT_BOUND(1 - WIN_VALUE, vl, WIN_VALUE - 1);
    __ASSERT(vl > vlBest);
    if (vl >= vlBeta) {
#ifdef HASH_QUIESC
      RecordHashQ(pos, vl, MATE_VALUE);
#endif
      return vl;
    }
    vlBest = vl;
    vlAlpha = MAX(vl, vlAlpha);

    // 8. ����δ�������ľ��棬���ɲ��������г����ŷ�(MVV(LVA)����)��
    MoveSort.InitQuiesc(pos);
  }

  // 9. ��Alpha-Beta�㷨������Щ�ŷ���
  while ((mv = MoveSort.NextQuiesc(bInCheck)) != 0) {
    __ASSERT(bInCheck || pos.ucpcSquares[DST(mv)] > 0);
    if (pos.MakeMove(mv)) {
      vl = -SearchQuiesc(pos, -vlBeta, -vlAlpha);
      pos.UndoMakeMove();
      if (vl > vlBest) {
        if (vl >= vlBeta) {
#ifdef HASH_QUIESC
          if (vl > -WIN_VALUE && vl < WIN_VALUE) {
            RecordHashQ(pos, vl, MATE_VALUE);
          }
#endif
          return vl;
        }
        vlBest = vl;
        vlAlpha = MAX(vl, vlAlpha);
      }
    }
  }

  // 10. ���ط�ֵ��
  if (vlBest == -MATE_VALUE) {
    __ASSERT(pos.IsMate());
    return pos.nDistance - MATE_VALUE;
  } else {
#ifdef HASH_QUIESC
    if (vlBest > -WIN_VALUE && vlBest < WIN_VALUE) {
      RecordHashQ(pos, vlBest > vlAlpha ? vlBest : -MATE_VALUE, vlBest);
    }
#endif
    return vlBest;
  }
}

// UCCI֧�� - ���Ҷ�ӽ��ľ�����Ϣ
void PopLeaf(PositionStruct &pos) {
  int vl;
  Search2.nAllNodes = 0;
  vl = SearchQuiesc(pos, -MATE_VALUE, MATE_VALUE);
  //printf("pophash lowerbound %d depth 0 upperbound %d depth 0\n", vl, vl);
  //fflush(stdout);
  pipeOutputWrite("pophash lowerbound %d depth 0 upperbound %d depth 0\n", vl, vl);
}

const bool NO_NULL = true; // "SearchCut()"�Ĳ������Ƿ��ֹ���Ųü�

// �㴰����ȫ��������
static int SearchCut(int vlBeta, int nDepth, bool bNoNull = false) {
  int nNewDepth, vlBest, vl;
  int mvHash, mv, mvEvade;
  MoveSortStruct MoveSort;
  // ��ȫ�������̰������¼������裺

  // 1. ��Ҷ�ӽ�㴦���þ�̬������
  if (nDepth <= 0) {
    __ASSERT(nDepth >= -NULL_DEPTH);
    return SearchQuiesc(Search.pos, vlBeta - 1, vlBeta);
  }
  Search2.nAllNodes ++;

  // 2. �޺��ü���
  vl = HarmlessPruning(Search.pos, vlBeta);
  if (vl > -MATE_VALUE) {
    return vl;
  }

  // 3. �û��ü���
  vl = ProbeHash(Search.pos, vlBeta - 1, vlBeta, nDepth, bNoNull, mvHash);
  if (Search.bUseHash && vl > -MATE_VALUE) {
    return vl;
  }

  // 3-a. ����ɱ����(���û������������һ��û�����Է����򷵻�ʤ����ֵ)��
  if (Search.bAlwaysCheck && Search.pos.LastMove().ChkChs <= 0 &&
      Search.pos.rbsList[Search.pos.nMoveNum - 2].mvs.ChkChs <= 0) {
    return MATE_VALUE - Search.pos.nDistance;
  }

  // 4. �ﵽ������ȣ�ֱ�ӷ�������ֵ��
  if (Search.pos.nDistance == LIMIT_DEPTH) {
    return Evaluate(Search.pos, vlBeta - 1, vlBeta);
  }
  __ASSERT(Search.pos.nDistance < LIMIT_DEPTH);

  // 5. �жϵ��ã�
  Search2.nMainNodes ++;
  vlBest = -MATE_VALUE;
  if ((Search2.nMainNodes & Search.nCountMask) == 0 && Interrupt()) {
    return vlBest;
  }

  // 6. ���Կ��Ųü���
  if (Search.bNullMove && !bNoNull && Search.pos.LastMove().ChkChs <= 0 && Search.pos.NullOkay()) {
    Search.pos.NullMove();
    vl = -SearchCut(1 - vlBeta, nDepth - NULL_DEPTH - 1, NO_NULL);
    Search.pos.UndoNullMove();
    if (vl >= vlBeta) {
      if (Search.pos.NullSafe()) {
        // a. ������Ųü��������飬��ô��¼�������Ϊ(NULL_DEPTH + 1)��
        RecordHash(Search.pos, HASH_BETA, vl, MAX(nDepth, NULL_DEPTH + 1), 0);
        return vl;
      } else if (SearchCut(vlBeta, nDepth - NULL_DEPTH, NO_NULL) >= vlBeta) {
        // b. ������Ųü������飬��ô��¼�������Ϊ(NULL_DEPTH)��
        RecordHash(Search.pos, HASH_BETA, vl, MAX(nDepth, NULL_DEPTH), 0);
        return vl;
      }
    }
  }

  // 7. ��ʼ����
  if (Search.pos.LastMove().ChkChs > 0) {
    // ����ǽ������棬��ô��������Ӧ���ŷ���
    mvEvade = MoveSort.InitEvade(Search.pos, mvHash, Search2.wmvKiller[Search.pos.nDistance]);
  } else {
    // ������ǽ������棬��ôʹ���������ŷ��б���
    MoveSort.InitFull(Search.pos, mvHash, Search2.wmvKiller[Search.pos.nDistance]);
    mvEvade = 0;
  }

  // 8. ����"MoveSortStruct::NextFull()"���̵��ŷ�˳����һ������
  while ((mv = MoveSort.NextFull(Search.pos)) != 0) {
    if (Search.pos.MakeMove(mv)) {

      // 9. ����ѡ�������죻
      nNewDepth = (Search.pos.LastMove().ChkChs > 0 || mvEvade != 0 ? nDepth : nDepth - 1);

      // 10. �㴰��������
      vl = -SearchCut(1 - vlBeta, nNewDepth);
      Search.pos.UndoMakeMove();
      if (Search2.bStop) {
        return vlBest;
      }

      // 11. �ض��ж���
      if (vl > vlBest) {
        vlBest = vl;
        if (vl >= vlBeta) {
          RecordHash(Search.pos, HASH_BETA, vlBest, nDepth, mv);
          if (!MoveSort.GoodCap(Search.pos, mv)) {
            SetBestMove(mv, nDepth, Search2.wmvKiller[Search.pos.nDistance]);
          }
          return vlBest;
        }
      }
    }
  }

  // 12. ���ضϴ�ʩ��
  if (vlBest == -MATE_VALUE) {
    __ASSERT(Search.pos.IsMate());
    return Search.pos.nDistance - MATE_VALUE;
  } else {
    RecordHash(Search.pos, HASH_ALPHA, vlBest, nDepth, mvEvade);
    return vlBest;
  }
}

// ������Ҫ����
static void AppendPvLine(uint16_t *lpwmvDst, uint16_t mv, const uint16_t *lpwmvSrc) {
  *lpwmvDst = mv;
  lpwmvDst ++;
  while (*lpwmvSrc != 0) {
    *lpwmvDst = *lpwmvSrc;
    lpwmvSrc ++;
    lpwmvDst ++;
  }
  *lpwmvDst = 0;
}

/* ��Ҫ������ȫ�������̣����㴰����ȫ���������������¼��㣺
 *
 * 1. �����ڲ���������������
 * 2. ��ʹ���и���Ӱ��Ĳü���
 * 3. Alpha-Beta�߽��ж����ӣ�
 * 4. PV���Ҫ��ȡ��Ҫ������
 * 5. ����PV��㴦������ŷ��������
 */
static int SearchPV(int vlAlpha, int vlBeta, int nDepth, uint16_t *lpwmvPvLine) {
  int nNewDepth, nHashFlag, vlBest, vl;
  int mvBest, mvHash, mv, mvEvade;
  MoveSortStruct MoveSort;
  uint16_t wmvPvLine[LIMIT_DEPTH];
  // ��ȫ�������̰������¼������裺

  // 1. ��Ҷ�ӽ�㴦���þ�̬������
  *lpwmvPvLine = 0;
  if (nDepth <= 0) {
    __ASSERT(nDepth >= -NULL_DEPTH);
    return SearchQuiesc(Search.pos, vlAlpha, vlBeta);
  }
  Search2.nAllNodes ++;

  // 2. �޺��ü���
  vl = HarmlessPruning(Search.pos, vlBeta);
  if (vl > -MATE_VALUE) {
    return vl;
  }

  // 3. �û��ü���
  vl = ProbeHash(Search.pos, vlAlpha, vlBeta, nDepth, NO_NULL, mvHash);
  if (Search.bUseHash && vl > -MATE_VALUE) {
    // ����PV��㲻�����û��ü������Բ��ᷢ��PV·���жϵ����
    return vl;
  }

  // 3-a. ����ɱ����(���û������������һ��û�����Է����򷵻�ʤ����ֵ)��
  if (Search.bAlwaysCheck && Search.pos.LastMove().ChkChs <= 0 &&
      Search.pos.rbsList[Search.pos.nMoveNum - 2].mvs.ChkChs <= 0) {
    return MATE_VALUE - Search.pos.nDistance;
  }

  // 4. �ﵽ������ȣ�ֱ�ӷ�������ֵ��
  __ASSERT(Search.pos.nDistance > 0);
  if (Search.pos.nDistance == LIMIT_DEPTH) {
    return Evaluate(Search.pos, vlAlpha, vlBeta);
  }
  __ASSERT(Search.pos.nDistance < LIMIT_DEPTH);

  // 5. �жϵ��ã�
  Search2.nMainNodes ++;
  vlBest = -MATE_VALUE;
  if ((Search2.nMainNodes & Search.nCountMask) == 0 && Interrupt()) {
    return vlBest;
  }

  // 6. �ڲ���������������
  if (nDepth > IID_DEPTH && mvHash == 0) {
    __ASSERT(nDepth / 2 <= nDepth - IID_DEPTH);
    vl = SearchPV(vlAlpha, vlBeta, nDepth / 2, wmvPvLine);
    if (vl <= vlAlpha) {
      vl = SearchPV(-MATE_VALUE, vlBeta, nDepth / 2, wmvPvLine);
    }
    if (Search2.bStop) {
      return vlBest;
    }
    mvHash = wmvPvLine[0];
  }

  // 7. ��ʼ����
  mvBest = 0;
  nHashFlag = HASH_ALPHA;
  if (Search.pos.LastMove().ChkChs > 0) {
    // ����ǽ������棬��ô��������Ӧ���ŷ���
    mvEvade = MoveSort.InitEvade(Search.pos, mvHash, Search2.wmvKiller[Search.pos.nDistance]);
  } else {
    // ������ǽ������棬��ôʹ���������ŷ��б���
    MoveSort.InitFull(Search.pos, mvHash, Search2.wmvKiller[Search.pos.nDistance]);
    mvEvade = 0;
  }

  // 8. ����"MoveSortStruct::NextFull()"���̵��ŷ�˳����һ������
  while ((mv = MoveSort.NextFull(Search.pos)) != 0) {
    if (Search.pos.MakeMove(mv)) {

      // 9. ����ѡ�������죻
      nNewDepth = (Search.pos.LastMove().ChkChs > 0 || mvEvade != 0 ? nDepth : nDepth - 1);

      // 10. ��Ҫ����������
      if (vlBest == -MATE_VALUE) {
        vl = -SearchPV(-vlBeta, -vlAlpha, nNewDepth, wmvPvLine);
      } else {
        vl = -SearchCut(-vlAlpha, nNewDepth);
        if (vl > vlAlpha && vl < vlBeta) {
          vl = -SearchPV(-vlBeta, -vlAlpha, nNewDepth, wmvPvLine);
        }
      }
      Search.pos.UndoMakeMove();
      if (Search2.bStop) {
        return vlBest;
      }

      // 11. Alpha-Beta�߽��ж���
      if (vl > vlBest) {
        vlBest = vl;
        if (vl >= vlBeta) {
          mvBest = mv;
          nHashFlag = HASH_BETA;
          break;
        }
        if (vl > vlAlpha) {
          vlAlpha = vl;
          mvBest = mv;
          nHashFlag = HASH_PV;
          AppendPvLine(lpwmvPvLine, mv, wmvPvLine);
        }
      }
    }
  }

  // 12. �����û�������ʷ����ɱ���ŷ�����
  if (vlBest == -MATE_VALUE) {
    __ASSERT(Search.pos.IsMate());
    return Search.pos.nDistance - MATE_VALUE;
  } else {
    RecordHash(Search.pos, nHashFlag, vlBest, nDepth, mvEvade == 0 ? mvBest : mvEvade);
    if (mvBest != 0 && !MoveSort.GoodCap(Search.pos, mvBest)) {
      SetBestMove(mvBest, nDepth, Search2.wmvKiller[Search.pos.nDistance]);
    }
    return vlBest;
  }
}

/* ������������̣�����ȫ���������������¼��㣺
 *
 * 1. ʡ���޺��ü�(Ҳ����ȡ�û����ŷ�)��
 * 2. ��ʹ�ÿ��Ųü���
 * 3. ѡ��������ֻʹ�ý������죻
 * 4. ���˵���ֹ�ŷ���
 * 5. ����������ŷ�ʱҪ���ܶദ��(������¼��Ҫ��������������)��
 * 6. ��������ʷ����ɱ���ŷ�����
 */
static int SearchRoot(int nDepth) {
  int nNewDepth, vlBest, vl, mv;
  uint32_t dwMoveStr;
  uint16_t wmvPvLine[LIMIT_DEPTH];
  // ������������̰������¼������裺

  // 1. ��ʼ��
  vlBest = -MATE_VALUE;
  Search2.MoveSort.ResetRoot();

  // 2. ��һ����ÿ���ŷ�(Ҫ���˽�ֹ�ŷ�)
  while ((mv = Search2.MoveSort.NextRoot()) != 0) {
    if (Search.pos.MakeMove(mv)) {
      if (Search2.bPopCurrMove || Search.bDebug) {
        dwMoveStr = MOVE_COORD(mv);
        //printf("info currmove %.4s\n", (const char *) &dwMoveStr);
        //fflush(stdout);
		pipeOutputWrite("info currmove %.4s\n", (const char *) &dwMoveStr);
      }

      // 3. ����ѡ��������(ֻ���ǽ�������)
      nNewDepth = (Search.pos.LastMove().ChkChs > 0 ? nDepth : nDepth - 1);

      // 4. ��Ҫ��������
      if (vlBest == -MATE_VALUE) {
        vl = -SearchPV(-MATE_VALUE, MATE_VALUE, nNewDepth, wmvPvLine);
        __ASSERT(vl > vlBest);
      } else {
        vl = -SearchCut(-vlBest, nNewDepth);
        if (vl > vlBest) { // ���ﲻ��Ҫ" && vl < MATE_VALUE"��
          vl = -SearchPV(-MATE_VALUE, -vlBest, nNewDepth, wmvPvLine);
        }
      }
      Search.pos.UndoMakeMove();
      if (Search2.bStop) {
        return vlBest;
      }

      // 5. Alpha-Beta�߽��ж�("vlBest"������"SearchPV()"�е�"vlAlpha")
      if (vl > vlBest) {

        // 6. �����������һ�ŷ�����ô"δ�ı�����ŷ�"�ļ�������1����������
        Search2.nUnchanged = (vlBest == -MATE_VALUE ? Search2.nUnchanged + 1 : 0);
        vlBest = vl;

        // 7. ����������ŷ�ʱ��¼��Ҫ����
        AppendPvLine(Search2.wmvPvLine, mv, wmvPvLine);
        PopPvLine(nDepth, vl);

        // 8. ���Ҫ��������ԣ���AlphaֵҪ���������������������ɱ��ʱ�����������
        if (vlBest > -WIN_VALUE && vlBest < WIN_VALUE) {
          vlBest += (Search.rc4Random.NextLong() & Search.nRandomMask) -
              (Search.rc4Random.NextLong() & Search.nRandomMask);
          vlBest = (vlBest == Search.pos.DrawValue() ? vlBest - 1 : vlBest);
        }

        // 9. ���¸�����ŷ��б�
        Search2.MoveSort.UpdateRoot(mv);
      }
    }
  }
  return vlBest;
}

// Ψһ�ŷ�������ElephantEye�������ϵ�һ����ɫ�������ж�����ĳ����Ƚ��е������Ƿ��ҵ���Ψһ�ŷ���
// ��ԭ���ǰ��ҵ�������ŷ���ɽ�ֹ�ŷ���Ȼ����(-WIN_VALUE, 1 - WIN_VALUE)�Ĵ�������������
// ����ͳ��߽���˵�������ŷ�������ɱ��
static bool SearchUnique(int vlBeta, int nDepth) {
  int vl, mv;
  Search2.MoveSort.ResetRoot(ROOT_UNIQUE);
  // ������һ���ŷ�
  while ((mv = Search2.MoveSort.NextRoot()) != 0) {
    if (Search.pos.MakeMove(mv)) {
      vl = -SearchCut(1 - vlBeta, Search.pos.LastMove().ChkChs > 0 ? nDepth : nDepth - 1);
      Search.pos.UndoMakeMove();
      if (Search2.bStop || vl >= vlBeta) {
        return false;
      }
    }
  }
  return true;
}

// ����������
void SearchMain(int nDepth) {
  int i, vl, vlLast, nBookMoves;
  int nCurrTimer, nLimitTimer;
  bool bUnique;
  uint32_t dwMoveStr;
  MoveStruct mvsBook[MAX_GEN_MOVES];
  // ���������̰������¼������裺

  // 1. ����������ֱ�ӷ���
  if (Search.pos.IsDraw() || Search.pos.RepStatus(3) > 0) {
	  //printf("nobestmove\n");
	  //fflush(stdout);
	  pipeOutputWrite("nobestmove\n");
    return;    
  }

  // 2. �ӿ��ֿ��������ŷ�
  if (Search.bUseBook) {
    // a. ��ȡ���ֿ��е������߷�
    nBookMoves = GetBookMoves(Search.pos, Search.szBookFile, mvsBook);
    if (nBookMoves > 0) {
      vl = 0;
      for (i = 0; i < nBookMoves; i ++) {
        vl += mvsBook[i].wvl;
        dwMoveStr = MOVE_COORD(mvsBook[i].wmv);
        //printf("info depth 0 score %d pv %.4s\n", mvsBook[i].wvl, (const char *) &dwMoveStr);
        //fflush(stdout);
		pipeOutputWrite("info depth 0 score %d pv %.4s\n", mvsBook[i].wvl, (const char *) &dwMoveStr);
      }
      // b. ����Ȩ�����ѡ��һ���߷�
      vl = Search.rc4Random.NextLong() % (uint32_t) vl;
      for (i = 0; i < nBookMoves; i ++) {
        vl -= mvsBook[i].wvl;
        if (vl < 0) {
          break;
        }
      }
      __ASSERT(vl < 0);
      __ASSERT(i < nBookMoves);
      // c. ������ֿ��е��ŷ�����ѭ�����棬��ô��������ŷ�
      Search.pos.MakeMove(mvsBook[i].wmv);
      if (Search.pos.RepStatus(3) == 0) {
        dwMoveStr = MOVE_COORD(mvsBook[i].wmv);
        //printf("bestmove %.4s", (const char *) &dwMoveStr);
		pipeOutputWrite("bestmove %.4s", (const char *) &dwMoveStr);
        // d. ������̨˼�����ŷ�(���ֿ��е�һ����Ȩ�����ĺ����ŷ�)
        nBookMoves = GetBookMoves(Search.pos, Search.szBookFile, mvsBook);
        Search.pos.UndoMakeMove();
        if (nBookMoves > 0) {
          dwMoveStr = MOVE_COORD(mvsBook[0].wmv);
          //printf(" ponder %.4s", (const char *) &dwMoveStr);
		  pipeOutputWrite(" ponder %.4s", (const char *) &dwMoveStr);
        }
        //printf("\n");
        //fflush(stdout);
		pipeOutputWrite("\n");
        return;
      }
      Search.pos.UndoMakeMove();
    }
  }

  // 3. ������Ϊ���򷵻ؾ�̬����ֵ
  if (nDepth == 0) {
	  //printf("info depth 0 score %d\n", SearchQuiesc(Search.pos, -MATE_VALUE, MATE_VALUE));
	  //fflush(stdout);
	  //printf("nobestmove\n");
	  //fflush(stdout);
	  pipeOutputWrite("info depth 0 score %d\n", SearchQuiesc(Search.pos, -MATE_VALUE, MATE_VALUE));
	  pipeOutputWrite("nobestmove\n");
    return;
  }

  // 4. ���ɸ�����ÿ���ŷ�
  Search2.MoveSort.InitRoot(Search.pos, Search.nBanMoves, Search.wmvBanList);

  // 5. ��ʼ��ʱ��ͼ�����
  Search2.bStop = Search2.bPonderStop = Search2.bPopPv = Search2.bPopCurrMove = false;
  Search2.nPopDepth = Search2.vlPopValue = 0;
  Search2.nAllNodes = Search2.nMainNodes = Search2.nUnchanged = 0;
  Search2.wmvPvLine[0] = 0;
  ClearKiller(Search2.wmvKiller);
  ClearHistory();
  ClearHash();
  // ���� ClearHash() ��Ҫ����һ��ʱ�䣬���Լ�ʱ�����Ժ�ʼ�ȽϺ���
  Search2.llTime = GetTime();
  vlLast = 0;
  // �������10�غ������ŷ�����ô����������ͣ��Ժ�ÿ��8�غ����һ��
  if (Search.pos.nMoveNum > 5 && ((Search.pos.nMoveNum - 4) / 2) % 8 == 0) {
    Search.bDraw = true;
  }
  bUnique = false;
  nCurrTimer = 0;

  // 6. ��������������
  for (i = 1; i <= nDepth; i ++) {
    // ��Ҫ�����Ҫ����ʱ����һ��"info depth n"�ǲ������
    if (Search2.bPopPv || Search.bDebug) {
		//printf("info depth %d\n", i);
		//fflush(stdout);
		pipeOutputWrite("info depth %d\n", i);
    }

    // 7. ����������ʱ��������Ƿ���Ҫ�����Ҫ�����͵�ǰ˼�����ŷ�
    Search2.bPopPv = (nCurrTimer > 300);
    Search2.bPopCurrMove = (nCurrTimer > 3000);

    // 8. ���������
    vl = SearchRoot(i);
    if (Search2.bStop) {
      if (vl > -MATE_VALUE) {
        vlLast = vl; // ������vlLast�������ж������Ͷ����������Ҫ�������һ��ֵ
      }
      break; // û����������"vl"�ǿɿ�ֵ
    }

    nCurrTimer = (int) (GetTime() - Search2.llTime);
    // 9. �������ʱ�䳬���ʵ�ʱ�ޣ�����ֹ����
    if (Search.nGoMode == GO_MODE_TIMER) {
      // a. ���û��ʹ�ÿ��Ųü�����ô�ʵ�ʱ�޼���(��Ϊ��֦���Ӽӱ���)
      nLimitTimer = (Search.bNullMove ? Search.nProperTimer : Search.nProperTimer / 2);
      // b. �����ǰ����ֵû�����ǰһ��ܶ࣬��ô�ʵ�ʱ�޼���
      nLimitTimer = (vl + DROPDOWN_VALUE >= vlLast ? nLimitTimer / 2 : nLimitTimer);
      // c. �������ŷ��������û�б仯����ô�ʵ�ʱ�޼���
      nLimitTimer = (Search2.nUnchanged >= UNCHANGED_DEPTH ? nLimitTimer / 2 : nLimitTimer);
      if (nCurrTimer > nLimitTimer) {
        if (Search.bPonder) {        
          Search2.bPonderStop = true; // ������ں�̨˼��ģʽ����ôֻ���ں�̨˼�����к�������ֹ������
        } else {
          vlLast = vl;
          break; // �����Ƿ�������"vlLast"���Ѹ���
        }
      }
    }
    vlLast = vl;

    // 10. ������ɱ������ֹ����
    if (vlLast > WIN_VALUE || vlLast < -WIN_VALUE) {
      break;
    }

    // 11. ��Ψһ�ŷ�������ֹ����
    if (SearchUnique(1 - WIN_VALUE, i)) {
      bUnique = true;
      break;
    }
  }

  // 12. �������ŷ��������Ӧ��(��Ϊ��̨˼���Ĳ²��ŷ�)
  if (Search2.wmvPvLine[0] != 0) {
    PopPvLine();
    dwMoveStr = MOVE_COORD(Search2.wmvPvLine[0]);
    //printf("bestmove %.4s", (const char *) &dwMoveStr);
	pipeOutputWrite("bestmove %.4s", (const char *) &dwMoveStr);
    if (Search2.wmvPvLine[1] != 0) {
      dwMoveStr = MOVE_COORD(Search2.wmvPvLine[1]);
      //printf(" ponder %.4s", (const char *) &dwMoveStr);
	  pipeOutputWrite(" ponder %.4s", (const char *) &dwMoveStr);
    }

    // 13. �ж��Ƿ��������ͣ����Ǿ���Ψһ�ŷ�����Ĳ��ʺ���������(��Ϊ������Ȳ���)
    if (!bUnique) {
      if (vlLast > -WIN_VALUE && vlLast < -RESIGN_VALUE) {
		  //printf(" resign");
		  pipeOutputWrite(" resign");
      } else if (Search.bDraw && !Search.pos.NullSafe() && vlLast < DRAW_OFFER_VALUE * 2) {
		  //printf(" draw");
		  pipeOutputWrite(" draw");
      }
    }
  } else {
	  //printf("nobestmove");
	  pipeOutputWrite("nobestmove");
  }
  //printf("\n");
  //fflush(stdout);
  pipeOutputWrite("\n");
}