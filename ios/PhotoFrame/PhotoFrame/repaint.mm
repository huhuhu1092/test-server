/* GIMP - The GNU Image Manipulation Program
 * Copyright (C) 1995 Spencer Kimball and Peter Mattis
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */
#include <stdlib.h>
#include <string.h>
#include <list>
#include <map>
#include <math.h>
#include "gimpressionist.h"
#include "ppmtool.h"
#include "random.h"
#include "SE_Mutex.h"

#include "assert.h"
#import "SEUtil.h"
#include <algorithm>
#ifdef WIN32
#include "imageloader.h"
#endif
#ifdef MACOS
#include "PGMDataReader.h"
#endif
#ifdef WIN32
#else
//extern "C"
//{
#include <sys/time.h>
//}
#endif
////////
int gMaxBrushWidth = 0;
int gMaxBrushHeight = 0;
typedef std::list<ppm_t> DrawingBrushList;
///////////////
struct BrushPoint
{
    int tx;
    int ty;
    BrushPoint(int x , int y)
    {
        tx = x;
        ty = y;
    }
    BrushPoint()
    {
        tx = 0;
        ty = 0;
    }
};
struct BrushBlock
{
    int tx, ty;
    int angle;
    int width, height;
    bool valid;
    BrushBlock()    
    {
        tx = ty = 0;
        angle = 0;
        width = height = 0;
        valid = false;
    }
    BrushBlock(int x , int y, int a, int w, int h)
    {
        tx = x;
        ty = y;
        angle = a;
        width = w;
        height = h;
        valid = false;
    }
};
struct BrushPassData
{
    int imageWidth;
    int imageHeight;
    int brushWidth;
    int brushHeight;
    int brushSize;
    int brushOrient;
};

//static int gImageWidthList[10];
//static int gImageHeightList[10];
//static int gBrushWidthList[10];
//static int gBrushHeightList[10];
static BrushPassData gBrushPassData[10];
typedef std::vector<BrushBlock> BrushBlockRow;
typedef std::vector<BrushBlockRow> BrushBlockMatrix;
static std::vector<BrushBlockMatrix> gBrushBlockMatrixVector(10);
static void setAngleToBrushPointVector(std::vector<BrushBlock>& brushBlockList, int tx, int ty, int angle)
{
    for(int i = 0 ; i < brushBlockList.size() ; i++)
    {
        if(brushBlockList[i].tx == tx && brushBlockList[i].ty == ty)
        {
            //NSLog(@"set angle = %d", angle);
            brushBlockList[i].angle = angle;
        }
    }
}
static void createBrushPointMatrix(std::vector<BrushBlock>& brushPoints, int index)
{
    BrushBlockMatrix& bbm = gBrushBlockMatrixVector[index];
    std::list<BrushBlock> brushPointList;
    std::list<BrushBlockRow> brushBlockRowList;
    std::vector<BrushBlock>::iterator itPrev = brushPoints.begin();
    
    for(std::vector<BrushBlock>::iterator it = brushPoints.begin();
        it != brushPoints.end();
        it++)
    {
        if(itPrev->ty == it->ty)
        {
            brushPointList.push_back(*it);
            itPrev = it;
        }
        else
        {
            BrushBlockRow bbr(brushPointList.size());
            NSLog(@"brush block row size = %ld", brushPointList.size());
            std::copy(brushPointList.begin(), brushPointList.end(), bbr.begin());
            brushBlockRowList.push_back(bbr);
            itPrev = it;
            brushPointList.clear();
        }
    }
    bbm.resize(brushBlockRowList.size());
    std::copy(brushBlockRowList.begin(), brushBlockRowList.end(), bbm.begin());
    //for test
    for(int i = 0 ; i < bbm.size() ; i++)
    {
        BrushBlockRow& row = bbm[i];
        for(int j = 0 ; j < row.size() - 1 ; j++)
        {
            BrushBlock& b1 = row[j];
            BrushBlock& b2 = row[j + 1];
            assert(b1.ty == b2.ty);
            assert(b1.tx <= b2.tx);
        }
    }
    for(int i = 0 ; i < bbm.size() - 1;  i++)
    {
        BrushBlockRow& r1 = bbm[i];
        BrushBlockRow& r2 = bbm[i + 1];
        for(int j = 0  ; j < r1.size() && j < r2.size() ; j++)
        {
            BrushBlock& b1 = r1[j];
            BrushBlock& b2 = r2[j];
            NSLog(@"b1.ty = %d, b2.ty = %d", b1.ty, b2.ty);
            NSLog(@"b1.tx = %d, b2.tx = %d", b1.tx, b2.tx);
            assert(b1.ty <= b2.ty);
            assert(b1.tx == b2.tx);
        }
    }
    //end
}
static int getAllBrushCount(int index)
{
    BrushBlockMatrix& bbm = gBrushBlockMatrixVector[index];
    int sum = 0;
    for(int i = 0 ; i < bbm.size() ; i++)
    {
        BrushBlockRow& row = bbm[i];
        sum += row.size();
    }
    return sum;
}
static bool isUseCacheBrushBlock(int index)
{
    return index == 3 || index == 5 || index == 7 || index == 9;
}
static BrushBlock getBrushBlock(int blockIndex, int index)
{
    BrushBlockMatrix& bbm = gBrushBlockMatrixVector[blockIndex];
    int size = bbm.size();
    int rowSize = bbm[0].size(); 
    /*
    NSLog(@"bbm size = %d", size);
    for(int i = 0 ; i < size ; i++)
    {
        BrushBlockRow& row = bbm[i];
        for(int j = 0 ; j < row.size() ; j++)
        {
            NSLog(@"## row angle = %d #", row[j].angle);
            NSLog(@"## row tx = %d, ty = %d", row[j].tx, row[j].ty);
        }
    }
     */
    int row = index / rowSize;
    int col = index % rowSize;
    BrushBlockRow& brushRow = bbm[row];
    return brushRow[col];
}
static void createBrushBlockMatrix(int from, int to, BrushPassData& fromBrushPassData, BrushPassData& toBrushPassData)
{
    BrushBlockMatrix& fromBbm = gBrushBlockMatrixVector[from];
    BrushBlockMatrix& toBbm = gBrushBlockMatrixVector[to];
    toBbm.clear();
    toBbm.resize(fromBbm.size() * 2);
    float wRatio = toBrushPassData.imageWidth / (float)fromBrushPassData.imageWidth;
    float hRatio = toBrushPassData.imageHeight / (float)fromBrushPassData.imageHeight;
    NSLog(@"fromImageWidth = %d, fromImageHeight = %d, toImageWidth = %d, toImageHeight = %d", fromBrushPassData.imageWidth, fromBrushPassData.imageHeight, toBrushPassData.imageWidth, toBrushPassData.imageHeight);
    for(int i = 0 ; i < toBbm.size() ; i++)
    {
        if((i % 2) == 0)
        {
            BrushBlockRow& srcBbr1 = fromBbm[i / 2];
            BrushBlockRow& dstBbr1 = toBbm[i];
            dstBbr1.resize(srcBbr1.size() * 2);
            for(int j = 0 ; j < dstBbr1.size() ; j++)
            {
                if(j % 2 == 0)
                {
                    dstBbr1[j] = srcBbr1[j / 2];
                    dstBbr1[j].tx = (dstBbr1[j].tx - fromBrushPassData.brushWidth) * wRatio;
                    dstBbr1[j].ty = (dstBbr1[j].ty - fromBrushPassData.brushHeight)* hRatio;
                    dstBbr1[j].valid = true;
                    //NSLog(@"dst angle = %d", dstBbr1[j].angle);
                }
            }
            for(int j = 0 ; j < dstBbr1.size() ; j++)
            {
                if((j % 2) == 1)
                {
                    int k = (j - 1);
                    int n = (j + 1);
                    if(n < dstBbr1.size())
                    {
                        dstBbr1[j].tx = (dstBbr1[k].tx + dstBbr1[n].tx) / 2;
                        dstBbr1[j].ty = dstBbr1[k].ty;
                        dstBbr1[j].angle = (dstBbr1[k].angle + dstBbr1[n].angle) / 2;
                        //NSLog(@"src col1 angle = %d, src col2 angle = %d ", dstBbr1[k].angle, dstBbr1[n].angle);
                        //NSLog(@"dst angle = %d", dstBbr1[j].angle);
                        dstBbr1[j].width = dstBbr1[k].width;
                        dstBbr1[j].height = dstBbr1[k].height;
                        dstBbr1[j].valid = true;
                    }
                    else
                    {
                        dstBbr1[j].valid = false;    
                    }
                }
            }
        }
    }
    for(int i = 0 ; i < toBbm.size() ; i++)
    {
        if((i % 2) == 1)
        {
            int k = i - 1;
            int n = i + 1;
            if(n < toBbm.size())
            {
                BrushBlockRow& dstRow = toBbm[i];
                BrushBlockRow& srcRow1 = toBbm[k];
                BrushBlockRow& srcRow2 = toBbm[n];
                
                int size = srcRow1.size() > srcRow2.size() ? srcRow2.size() : srcRow1.size();
                //NSLog(@"row1 size = %ld, row2 size = %ld", srcRow1.size(), srcRow2.size());
                dstRow.resize(size);
                for(int j = 0 ; j < dstRow.size() ; j++)
                {
                    if((j % 2) == 0)
                    {
                        dstRow[j].tx = srcRow1[j].tx;
                        dstRow[j].ty = (srcRow1[j].ty + srcRow2[j].ty) / 2;
                        dstRow[j].angle = (srcRow1[j].angle + srcRow2[j].angle) / 2;
                        if(srcRow1[j].angle > 360 || srcRow2[j].angle > 360)
                        {
                            NSLog(@" error ");
                        }
                        //NSLog(@"src row1 angle = %d, src row2 angle = %d", srcRow1[j].angle, srcRow2[j].angle);
                        dstRow[j].valid = true;
                        //NSLog(@"dst angle = %d", dstRow[j].angle);
                    }
                    else {
                        dstRow[j].valid = false;
                    }
                }
            }
        }
    }
}

//////////////////////////////
static double gCosTable[361];
static double gSinTable[361];
class SETimePrinter
{
private:
    struct timeval start;
public:
    SETimePrinter()
    {
        reset();
    }
    void reset()
    {
        gettimeofday(&start, NULL);
    }
    void printTime(const char* str)
    {
        struct timeval end;
        gettimeofday(&end, NULL);
        long ret = (end.tv_sec - start.tv_sec) * 1000000 + (end.tv_usec - start.tv_usec);
        NSLog(@"%s time : %ld", str, ret);
    }
};
static double sum_brush (ppm_t *p);
enum POINT_TYPE {POSITIVE, NEGTIVE, OTHER_POS, POINT_TYPE_NUM};
struct BrushMapData
{
    typedef std::vector<POINT_TYPE> LineVector;
    std::vector<LineVector> data;
};
//typedef std::map<int, BrushMapData> BrushMapDataMap;
struct BrushMapDataMap
{
    BrushMapData data[721];
};
struct MyBrushData
{
    ppm_t brush;
    ppm_t rawBrush;
    double angle;
    double sum;
    double anglefirst;
    double anglespan;
    int startx, endx;
    int starty, endy;
    MyBrushData(ppm_t b, double a)
    {
        brush = b;
        angle = a;
        rawBrush.width = 0;
        rawBrush.height = 0;
        rawBrush.col = NULL;
        anglefirst = 0;
        anglespan = 0;
        startx = 0;
        endx = 0;
        starty = 0;
        endy = 0;
        //sum = sum_brush(&brush);
    }
    MyBrushData()
    {
        brush.width = 0;
        brush.height = 0;
        brush.col = NULL;
        rawBrush.width = 0;
        rawBrush.height = 0;
        rawBrush.col = NULL;
        angle = 0;
        anglefirst = 0;
        anglespan = 0;
        sum = 0;
    }
};
////
////////////
REPAINTCALLBACK_FUN repaintCallBack = 0;
int tmpWidth = 0;
int tmpHeight = 0;
int gBrushMaxWidth = 0;
int gBrushMaxHeight = 0;
int gImageWidth = 0;
int gImageHeight = 0;
double gRunningTime = 0;
#ifdef WIN32
#else
struct timeval gStartTime;
#endif

//////////////////////
#define PART_NUM  4
struct BrushProperty
{
    struct p1_t
    {
        ppm_t* brush;
        ppm_t* destBrush;
        int tx;
        int ty;
        unsigned char r, g, b;
    };
    struct p2_t
    {
        bool precise;
        int startx, starty, endx, endy;
    };
    union
    {
        p1_t p1;
        p2_t p2;
    };
};

///////////
#define QUAD_TREE_DEPTH 3
struct QuadRect
{
    float left, right, top, bottom;
    QuadRect()
    {
        left = right = top = bottom = 0;
    }
    QuadRect(float l, float r, float t, float b)
    {
        left = l;
        right = r;
        top = t;
        bottom = b;
    }
};
struct QuadTreeNode
{
    QuadTreeNode* child[4];
    QuadRect rect;
    std::list<BrushProperty> bpList;
    QuadTreeNode()
    {
        for(int i = 0 ; i < 4  ; i++)
            child[i] = NULL;
    }
};

static QuadTreeNode* rootQuadTree = NULL;
////////////////////
struct BrushRotateData
{
    BOOL end;
    std::list<ppm_t> brushes;
    BrushRotateData()
    {
        end = NO;
    }
};
struct BrushRotateThreadData
{
    NSCondition* condition;
    BrushRotateData data[PART_NUM];
    int num;
    BrushRotateThreadData()
    {
        num = PART_NUM;
        condition = [[NSCondition alloc] init];
    }
    ~BrushRotateThreadData()
    {
        [condition release];
    }
};
struct BrushRotatePropData
{
    int h;
    int i, j;
    double sv;
    int brushIndex;
};
/////////////////////////////
struct BrushCreateData
{
    BOOL end;
    std::list<BrushProperty> brushPropertyList;
    BrushCreateData()
    {
        end = NO;
    }
};
struct BrushCreatePropData
{
    int tx, ty;
    int sn, on;
};
struct BrushCreateThreadData
{
    NSCondition* condition;
    BrushCreateData data[PART_NUM];
    int num;
    BrushCreateThreadData()
    {
        num = PART_NUM;
        condition = [[NSCondition alloc] init];
    }
    ~BrushCreateThreadData()
    {
        [condition release];
    }
};
////////////////////
struct BrushInsertToArea
{
    BOOL end;
    QuadTreeNode* root;
    BrushInsertToArea()
    {
        end = NO;
        root = NULL;
    }
};
struct BrushInsertToAreaThreadData
{
    BrushInsertToArea data[PART_NUM];
    int num;
}
;
////////////////////////////
static BrushRotateThreadData brushRotateThreadData;
static BrushCreateThreadData brushCreateThreadData;
static BrushInsertToAreaThreadData brushInsertToAreaThreadData;
//////////////////////////
#define GRAY_NUM 5
struct GrayPaintArea
{
    std::list<BrushProperty> bp;
};
GrayPaintArea grayPaintArray[GRAY_NUM];
struct GraySpan
{
    float start;
    float end;
};
static std::vector<GraySpan> gGraySpanVector;
static void createGraySpan(ppm_t* p)
{
    gGraySpanVector.clear();
    gGraySpanVector.resize(GRAY_NUM);
    std::list<float> grayList;
    for(int i = 0 ; i < p->height ; i++)
    {
        for(int j = 0 ; j < p->width;  j++)
        {
            guchar* bits = p->col + i * p->width * 3 + j * 3;
            float gray = bits[0] * 0.2126f + bits[1] * 0.7152f + bits[2] * 0.0722f;
            grayList.push_back(gray);
        }
    }
    grayList.sort();
    std::vector<float> v(grayList.size());
    std::copy(grayList.begin(), grayList.end(),v.begin());
    //int step = (v.size() - 1) / GRAY_NUM;
    float step = 255.0f / GRAY_NUM;
    float start = 0;
    GraySpan gs;
    gs.end = start + step;//v[v.size() - 1];
    gs.start = start;//v[(v.size() - 1) * 4 / 5];
    gGraySpanVector[0] = gs;
    
    gs.end = start + 2 * step;//v[(v.size() - 1) * 4 / 5];
    gs.start = start + step;//v[(v.size() - 1) * 3 / 5];
    gGraySpanVector[1] = gs;
    
    gs.end = start + 3 * step;//v[(v.size() - 1) * 3 / 5];
    gs.start = start + 2 * step;//v[(v.size() - 1) * 2 / 5];
    gGraySpanVector[2] = gs;
    
    gs.end = start + 4 * step;//v[(v.size() - 1) * 2 / 5];
    gs.start = start + 3 * step;//v[(v.size() - 1) / 5];
    gGraySpanVector[3] = gs;
    
    gs.end = start + 5 * step;//v[(v.size() - 1) / 5];
    gs.start = start + 4 * step;//0;
    gGraySpanVector[4] = gs;
    for(int i = 0 ; i < GRAY_NUM ; i++)
    {
        NSLog(@"## gray span = %f, %f", gGraySpanVector[i].start, gGraySpanVector[i].end);
    }
    /*
    for(int i = 0 ; i < GRAY_NUM; i++)
    {
        GraySpan gs;
        gs.end = v[v.size() - 1];
        gs.start = v[v.size() - 1 - i * step];
        gGraySpanVector[i] = gs;
    }
     */
    
}
/////////////////////////////////////////
static BOOL isAllEnd()
{
    BOOL allEnd = YES;
    for(int i = 0 ; i < brushRotateThreadData.num ; i++)
    {
        if(brushRotateThreadData.data[i].end == NO)
        {
            allEnd = NO;
            break;
        }
    }
    return allEnd;
}
static void setBrushData(int index)
{
    [brushRotateThreadData.condition lock];
    brushRotateThreadData.data[index].end = YES;
    //brushRotateThreadData.data[index].brushes = brushList;
    if(isAllEnd())
    {
        NSLog(@"## all brush rotate end ##");
        [brushRotateThreadData.condition signal];
    }
    [brushRotateThreadData.condition unlock];
}
static void checkToContinue()
{
    [brushRotateThreadData.condition lock];
    while(isAllEnd() == NO)
    {
        [brushRotateThreadData.condition wait];
    }
    [brushRotateThreadData.condition unlock];
    for(int i = 0 ; i < PART_NUM ; i++)
    {
        brushRotateThreadData.data[i].end = NO;
    }
}
@interface BrushRotateThreadFunctor : NSObject
{
@public
    ppm_t* brushes;
    ppm_t* destBrushes;
    ppm_t* newBrushes;
    ppm_t* newDestBrushes;
    int startIndex;
    int endIndex;
    gimpressionist_vals_t runningvals;
    int BRUSH_NUM;
    int destWidth;
    int selfIndex;
    std::list<BrushRotatePropData>* brushPropData;
}
- (void) createBrush;
@end
@implementation BrushRotateThreadFunctor
- (id) init
{
    self = [super init];
    if(self)
    {
        brushPropData = new std::list<BrushRotatePropData>;
    }
    return self;
}
- (void)dealloc
{
    NSLog(@"## brushRotateThreadFunctor delloc ##\n");
    delete brushPropData;
    [super dealloc];
}
- (void) createBrush
{
    double startangle = runningvals.orient_first;
    double anglespan = runningvals.orient_last;
    double ns = std::max(startangle, anglespan);
    double ls = std::min(startangle, anglespan);
    startangle = ls;
    anglespan = ns;
    std::list<BrushRotatePropData>::iterator it;
    
    for (it = brushPropData->begin(); it !=  brushPropData->end(); it++)
    {
        int i = it->i;
        int j = it->j;
        int h = it->h;
        int brushIndex = it->brushIndex;
        double sv = it->sv;
        float times;
        //int h = j + i * runningvals.orient_num;
        //NSLog(@"## h = %d ##\n", h);
        //ppm_copy(&brushes[h], &destBrushes[h]);
        if(h < 3)
        {
            free_rotate (&brushes[h],
                         startangle + j * anglespan / runningvals.orient_num);
            autocrop (&brushes[h],1);
            rescale (&brushes[h],
                     ( sv      * runningvals.size_first +
                      (1.0-sv) * runningvals.size_last    ) / runningvals.size_last);
            
            times = ((float)destWidth) / gImageWidth;
            //double first , last;
            //first = runningvals.size_first * times;
            //last = runningvals.size_last * times;
            //for test
            //rescale(&destBrushes[h], (sv * first + (1.0 - sv) * last) / last);
            //end
            free_rotate(&destBrushes[h], startangle + j * anglespan / runningvals.orient_num);
            autocrop(&destBrushes[h], 1);
            resize(&destBrushes[h], brushes[h].width * times, brushes[h].height * times);
        }
        else
        {
            brushes[h].col = NULL;
            destBrushes[h].col = NULL;
            //int brushIndex = g_rand_int_range (random_generator, 0, BRUSH_NUM);
            //NSLog(@"## brushIndex = %d ##\n", brushIndex);
            brushes[h] = free_rotate_return(&newBrushes[brushIndex],
                                            startangle + j * anglespan / runningvals.orient_num);
            autocrop (&brushes[h],1);
            rescale (&brushes[h],
                     ( sv      * runningvals.size_first +
                      (1.0-sv) * runningvals.size_last    ) / runningvals.size_last);
            
            times = ((float)destWidth) / gImageWidth;
            //double first , last;
            //first = runningvals.size_first * times;
            //last = runningvals.size_last * times;
            
            destBrushes[h] = free_rotate_return(&newDestBrushes[brushIndex], startangle + j * anglespan / runningvals.orient_num);
            autocrop(&destBrushes[h], 1);
            resize(&destBrushes[h], brushes[h].width * times, brushes[h].height * times);
            
        }
        //autocrop(&destBrushes[h], 1);
        //SS_SaveBrush("brush", h, brushes[h]);
        //SS_SaveBrush("destbrush", h, destBrushes[h]);
    }
    //SS_Pause(currentPausePoint);

    NSLog(@"## part %d compute end ##\n", selfIndex);
    setBrushData(selfIndex);
}
@end
//////////////////
static BOOL isAllBrushCreate()
{
    BOOL ret = YES;
    for(int i = 0 ; i < brushCreateThreadData.num ; i++)
    {
        if(brushCreateThreadData.data[i].end == NO)
        {
            ret = NO;
            break;
        }
    }
    return ret;
}
static void setBrushData(int index, std::list<BrushProperty>& bpList)
{
    [brushCreateThreadData.condition lock];
    LOGI("## bpList size = %lu ##\n", bpList.size());
    std::list<BrushProperty>::iterator it;
    for(it = bpList.begin() ; it != bpList.end() ; it++)
    {
        brushCreateThreadData.data[index].brushPropertyList.push_back(*it);
    }
    brushCreateThreadData.data[index].end = YES;
    if(isAllBrushCreate())
    {
        [brushCreateThreadData.condition signal];
    }
    [brushCreateThreadData.condition unlock];
}
static void addBrushPropertyToGrayPaintArea(BrushProperty bp, bool mostConcise, RepaintData& rd);
static void checkAllBrushCreate()
{
    /*
    [brushCreateThreadData.condition lock];
    while(isAllBrushCreate() == NO)
    {
        [brushCreateThreadData.condition wait];
    }

    [brushCreateThreadData.condition unlock];
    std::list<BrushProperty>::iterator it;
    for(int k = 0 ; k < PART_NUM ; k++)
    {
        NSLog(@"## brush create data %d, size = %lu", k, brushCreateThreadData.data[k].brushPropertyList.size());
        for(it = brushCreateThreadData.data[k].brushPropertyList.begin() ;
            it != brushCreateThreadData.data[k].brushPropertyList.end();
            it++)
        {
            addBrushPropertyToGrayPaintArea(*it, false, rd);
        }
    }
    for(int i = 0 ; i < GRAY_NUM ; i++)
    {
        unsigned int count = grayPaintArray[i].bp.size();
        //SS_AddLog("## gray %d num : %lu ##", i, count);
    }
    for(int i = 0 ; i < PART_NUM ; i++)
    {
        brushCreateThreadData.data[i].end = NO;
        brushCreateThreadData.data[i].brushPropertyList.clear();
    }
     */
}
@interface BrushCreateThreadFunctor : NSObject
{
@public
    //int tx, ty;
    //int sn, on;
    std::list<BrushCreatePropData>* brushDataList;
    bool calculateEdge;
    int maxbrushwidth, maxbrushheight;
    ppm_t* brushes;
    ppm_t* p;
    ppm_t* a;
    ppm_t edgeDetectionMap;
    gimpressionist_vals_t runningvals;
    double* brushes_sum;
    int num_brushes;
    ppm_t* destBrushes;
    int selfIndex;
}
- (void) createBrushes;
@end
static bool brushOnEdge(ppm_t* edgeDetectionMap, int brushWidth, int brushHeight, int tx, int ty, float& outRadian);
static int choose_best_brush (ppm_t* edgeDetectionMap, ppm_t *p, ppm_t *a, int tx, int ty,
                              ppm_t *brushes, int num_brushes,
                              double *brushes_sum, int start, int step);
@implementation BrushCreateThreadFunctor
- (id) init
{
    self = [super init];
    if(self)
    {
        brushDataList = new std::list<BrushCreatePropData>;
    }
    return self;
}
- (void) dealloc
{
    NSLog(@"## brush create thread dealloc ##\n");
    delete brushDataList;
    [super dealloc];
}
- (void) createBrushes
{
    /* Handle Adaptive selections */
    bool onEdge = !calculateEdge;
    std::list<BrushCreatePropData>::iterator it;
    int edgeBrushNum = 0;
    std::list<BrushProperty> bpList;
    NSLog(@"## brushDataList size = %lu ##", brushDataList->size());
    for(it = brushDataList->begin() ; it != brushDataList->end() ; it++)
    {
        int tx = it->tx;
        int ty = it->ty;
        int sn = it->sn;
        int on = it->on;
        //NSLog(@"# tx = %d, ty = %d ##\n", tx, ty);
        int n = 0;
        float outRadian = 0;
        if(!onEdge)
        {
            onEdge = brushOnEdge(&edgeDetectionMap, maxbrushwidth, maxbrushheight, tx - maxbrushwidth / 2, ty - maxbrushheight / 2, outRadian);
            if(onEdge)
            {
                edgeBrushNum++;
            }
        }

        if (runningvals.orient_type == ORIENTATION_ADAPTIVE)
        {
            
            if (runningvals.size_type == SIZE_TYPE_ADAPTIVE)
            {
                
                if(onEdge)
                {
                    n = choose_best_brush (&edgeDetectionMap, p, a, tx-maxbrushwidth/2,
                                           ty-maxbrushheight/2, brushes,
                                           num_brushes, brushes_sum, 0, 1);
                }
                else 
                {
                    n = -1;
                }
            }
            else
            {
                int st = sn * runningvals.orient_num;
                if(onEdge)
                {
                    n = choose_best_brush (&edgeDetectionMap, p, a, tx-maxbrushwidth/2,
                                           ty-maxbrushheight/2, brushes,
                                           st+runningvals.orient_num, brushes_sum,
                                           st, 1);
                    /*
                     n = choose_best_brush (&edgeDetectionMap, p, a, tx-maxbrushwidth/2,
                     ty-maxbrushheight/2, brushes,
                     st+runningvals.orient_num, brushes_sum,
                     st, 10);
                     */
                }
                else 
                {
                    n = -1;
                }
            }
        }
        else
        {
            if(onEdge)
            {
                if (runningvals.size_type == SIZE_TYPE_ADAPTIVE)
                {
                    n = choose_best_brush (&edgeDetectionMap, p, a, tx-maxbrushwidth/2,
                                           ty-maxbrushheight/2, brushes,
                                           num_brushes, brushes_sum,
                                           on, runningvals.orient_num);
                }
                else
                    n = sn * runningvals.orient_num + on;
            }
            else 
            {
                n = -1;
            }
        }
        /* Should never happen, but hey... */
        if (n < 0)
        {
            //change for edge detection
            //n = 0;
            continue;
        }
        else if (n >= num_brushes)
            n = num_brushes - 1;
        
        tx -= maxbrushwidth/2;
        ty -= maxbrushheight/2;
        
        ppm_t* brush = &brushes[n];
        double thissum = brushes_sum[n];
        int r,g, b, h;
        int x, y;
        /* Calculate color - avg. of in-brush pixels */
        if (runningvals.color_type == 0)
        {
            r = g = b = 0;
            for (y = 0; y < brush->height; y++)
            {
                guchar *row = &p->col[(ty + y) * p->width * 3];
                
                for (x = 0; x < brush->width; x++)
                {
                    int k = (tx + x) * 3;
                    double v;
                    
                    if ((h = brush->col[y * brush->width * 3 + x * 3]))
                    {
                        v = h / 255.0;
                        r += row[k+0] * v;
                        g += row[k+1] * v;
                        b += row[k+2] * v;
                    }
                }
            }
            if(thissum != 0)
            {
                r = r * 255.0 / thissum;
                g = g * 255.0 / thissum;
                b = b * 255.0 / thissum;
            }
        }
        else if (runningvals.color_type == 1)
        {
            guchar *pixel;
            
            y = ty + (brush->height / 2);
            x = tx + (brush->width / 2);
            pixel = &p->col[y*p->width * 3 + x * 3];
            r = pixel[0];
            g = pixel[1];
            b = pixel[2];
        }
        else
        {
            /* No such color_type! */
            r = g = b = 0;
        }
        if (runningvals.color_noise > 0.0)
        {
            double v = runningvals.color_noise;
    #define BOUNDS(a) (((a) < 0) ? (a) : ((a) > 255) ? 255 : (a))
    #define MYASSIGN(a) \
    { \
    a = a + g_rand_double_range (random_generator, -v/2.0, v/2.0); \
    a = BOUNDS(a) ;       \
    }
            MYASSIGN (r);
            MYASSIGN (g);
            MYASSIGN (b);
    #undef BOUNDS
    #undef MYASSIGN
        }
        //debug for change
        //apply_brush (brush, shadow, &tmp, &atmp, tx,ty, r,g,b);
        BrushProperty bp;
        bp.p1.brush = brush;
        bp.p1.destBrush = &destBrushes[n];
        bp.p1.b = b;
        bp.p1.g = g;
        bp.p1.r = r;
        //bp.p1.shadow = NULL;
        bp.p1.tx = tx;
        bp.p1.ty = ty;
        bpList.push_back(bp);
    }
    NSLog(@"## create index %d OK #\n", selfIndex);
    setBrushData(selfIndex, bpList);
}

@end
////////////////////////////////////
ppm_t gBackground = {0, 0, NULL};
ppm_t gBackgroundBack = {0, 0, NULL};
/*
 * The default values for the application, to be initialized at startup.
 * */
static gimpressionist_vals_t defaultpcvals = {
    4,
    0.0,
    60.0,
    0,
    12.0,
    20.0,
    20.0,
    1.0,
    1,
    0.1,
    0.0,
    30.0,
    0,
    0,
    "defaultbrush.pgm",
    "defaultpaper.pgm",
    {0,0,0,1.0},
    1,
    0,
    { { 0.5, 0.5, 0.0, 0.0, 1.0, 1.0, 0 } },
    1,
    0,
    0.0,
    0.0,
    1.0,
    0,
    0,
    0,
    0,
    0,
    20.0,
    1,
    10.0,
    20.0,
    0,
    0.001,
    
    { { 0.5, 0.5, 50.0, 1.0 } },
    1,
    1.0,
    0,
    
    10,
    4,
    
    0, 0.0
};
gimpressionist_vals_t  pcvals;
void setDefaultPcvals()
{
	pcvals = defaultpcvals;
}
double dist (double x, double y, double end_x, double end_y)
{
    double dx = end_x - x;
    double dy = end_y - y;
    return sqrt (dx * dx + dy * dy);
}
double getsiz_proto (double x, double y, int n, smvector_t *vec,
                     double smstrexp, int voronoi)
{
    int    i;
    double sum, ssum, dst;
    int    first = 0, last;
    
    if ((x < 0.0) || (x > 1.0))
        g_warning ("HUH? x = %f\n",x);
    
#if 0
    if (from == 0)
    {
        n = numsmvect;
        vec = smvector;
        smstrexp = gtk_adjustment_get_value (GTK_ADJUSTMENT (smstrexpadjust));
        voronoi = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (size_voronoi));
    }
    else
    {
        n = pcvals.num_size_vectors;
        vec = pcvals.size_vectors;
        smstrexp = pcvals.size_strength_exponent;
        voronoi = pcvals.size_voronoi;
    }
#endif
    
    if (voronoi)
    {
        gdouble bestdist = -1.0;
        for (i = 0; i < n; i++)
        {
            dst = dist (x, y, vec[i].x, vec[i].y);
            if ((bestdist < 0.0) || (dst < bestdist))
            {
                bestdist = dst;
                first = i;
            }
        }
        last = first+1;
    }
    else
    {
        first = 0;
        last = n;
    }
    
    sum = ssum = 0.0;
    for (i = first; i < last; i++)
    {
        gdouble s = vec[i].str;
        
        dst = dist (x,y,vec[i].x,vec[i].y);
        dst = pow (dst, smstrexp);
        if (dst < 0.0001)
            dst = 0.0001;
        s = s / dst;
        
        sum += vec[i].siz * s;
        ssum += 1.0/dst;
    }
    sum = sum / ssum / 100.0;
    return CLAMP (sum, 0.0, 1.0);
}
#define P_VAL(item, fmt) g_printerr(#item "= %" #fmt "\n", val->item)
void print_val(gimpressionist_vals_t* val)
{
    P_VAL(orient_num, d);
    P_VAL(orient_first, f);
    P_VAL(orient_last, f);
    P_VAL(orient_type, d);
    P_VAL(brush_relief, f);
    P_VAL(brush_scale, f);
    P_VAL(brush_density, f);
    P_VAL(brushgamma, f);
    P_VAL(general_background_type, d);
    P_VAL(general_dark_edge, f);
    P_VAL(paper_relief, f);
    P_VAL(paper_scale, f);
    P_VAL(paper_invert, d);
    P_VAL(run, d);
    P_VAL(selected_brush, s);
    P_VAL(selected_paper, s);
    P_VAL(general_paint_edges, d);
    P_VAL(place_type, d);
    P_VAL(num_orient_vectors, d);
    P_VAL(placement_center, d);
    P_VAL(brush_aspect, f);
    P_VAL(orient_angle_offset, f);
    P_VAL(orient_strength_exponent, f);
    P_VAL(general_tileable, d);
    P_VAL(paper_overlay, d);
    P_VAL(orient_voronoi, d);
    P_VAL(color_brushes, d);
    P_VAL(general_drop_shadow, d);
    P_VAL(general_shadow_darkness, f);
    P_VAL(size_num, d);
    P_VAL(size_first, f);
    P_VAL(size_last, f);
    P_VAL(size_type, d);
    P_VAL(devthresh, f);
    
    P_VAL(num_size_vectors, d);
    P_VAL(size_strength_exponent, f);
    P_VAL(size_voronoi, d);
    
    P_VAL(general_shadow_depth, d);
    P_VAL(general_shadow_blur, d);
    
    P_VAL(color_type, d);
    P_VAL(color_noise, f); 
}
void changeBackground()
{
    ppm_copy(&gBackgroundBack, &gBackground);
}
static void clearBackground()
{
    ppm_kill(&gBackground);
    ppm_kill(&gBackgroundBack);
    gBrushMaxWidth = 0;
    gBrushMaxHeight = 0;
    gImageWidth = 0;
    gImageHeight = 0;
    tmpWidth = 0;
    tmpHeight = 0;
}
static ppm_t createBackground(gimpressionist_vals_t runningvals, int width, int height)
{
    int x, y;
    ppm_t tmp;
    ppm_t paper_ppm;
    float scale = runningvals.paper_scale / 100.0;
    ppm_new (&tmp, width, height);
    ppm_load (runningvals.selected_paper, &paper_ppm);
    resize (&paper_ppm, paper_ppm.width * scale, paper_ppm.height * scale);
    if (runningvals.paper_invert)
        ppm_apply_gamma (&paper_ppm, -1.0, 1, 1, 1);
    for (x = 0; x < tmp.width; x++)
    {
        int rx = x % paper_ppm.width;
        
        for (y = 0; y < tmp.height; y++)
        {
            int ry = y % paper_ppm.height;
            memcpy (&tmp.col[y * tmp.width * 3 + x * 3],
                    &paper_ppm.col[ry*paper_ppm.width*3+rx*3],
                    3);
        }
    }
    ppm_kill(&paper_ppm);
    return tmp;
}
void startTime()
{
#ifdef WIN32
#else
    gettimeofday(&gStartTime, NULL);
    gRunningTime = 0;
#endif
}
void endTime()
{
#ifdef WIN32
#else
    struct timeval endTime;
    int endms, startms;
    gettimeofday(&endTime, NULL);
    endms = endTime.tv_sec * 1000 + endTime.tv_usec / 1000;
    startms = gStartTime.tv_sec * 1000 + gStartTime.tv_usec / 1000;
    gRunningTime = (endms - startms) / 1000.0f;
#endif
}
double getTime()
{
    return gRunningTime;
}
void clearTime()
{
    gRunningTime = 0;
}

/////////////////////////////////////////////////
static gboolean img_has_alpha = 0;
static gint brush_from_file = 2;
static ppm_t brushppm  = {0, 0, NULL};

static gimpressionist_vals_t runningvals;
static std::list<BrushPiece> gBrushPieceList;
static SE_Mutex gBrushPieceMutex;
static volatile int isRepaintEnd = 0;
static SE_Mutex gIsRepaintEnd;
static volatile int gBrushPaintRunning = 1;
static SE_Mutex gBrushPaintRunningMutex;
void terminateBrushPaint()
{
    gBrushPaintRunningMutex.lock();
    gBrushPaintRunning = 0;
    gBrushPaintRunningMutex.unlock();
}
static int isBrushPaint()
{
    int ret = 0;
    gBrushPaintRunningMutex.lock();
    ret = gBrushPaintRunning ;
    gBrushPaintRunningMutex.unlock();
    return ret;
}
void startBrushPaint()
{
    gBrushPaintRunningMutex.lock();
    gBrushPaintRunning = 1;
    gBrushPaintRunningMutex.unlock();
}
void setIsRepaintEnd(int v)
{
    gIsRepaintEnd.lock();
    isRepaintEnd = v;
    gIsRepaintEnd.unlock();
}
int hasRepaintEnd()
{
    volatile int ret;
    gIsRepaintEnd.lock();
    ret = isRepaintEnd;
    gIsRepaintEnd.unlock();
    return ret;
}
void clearBrushPiece()
{
    gBrushPieceMutex.lock();
    std::list<BrushPiece>::iterator it;
    for(it = gBrushPieceList.begin() ; it != gBrushPieceList.end(); it++)
    {
        BrushPiece bp = *it;
        //ppm_kill(&bp.data);
        //ppm_kill(&bp.alpha);
    }
	gBrushPieceList.clear();
	gBrushPieceMutex.unlock();
}
BrushPiece getNextBrushPiece()
{
    BrushPiece bp;
    int tmpRepaintEnd = hasRepaintEnd();
    gBrushPieceMutex.lock();
    if(!gBrushPieceList.empty())
    {
        bp = gBrushPieceList.front();
        gBrushPieceList.pop_front();
    }
    else
    {
        if(!tmpRepaintEnd)
	    {
            bp.p1.x = bp.p1.y = -1;
   	    }
	    else
	    {
	        bp.p1.x = bp.p1.y = -2;
	    }
    }
    gBrushPieceMutex.unlock();
    return bp;
}
void addBrushPiece(BrushPiece bp)
{
    gBrushPieceMutex.lock();
    gBrushPieceList.push_back(bp);
    gBrushPieceMutex.unlock();
}
///////////////////////////////////////////////////////////////////////////
static void gimp_rgb_get_uchar (const GimpRGB *rgb,
                    guchar        *r,
                    guchar        *g,
                    guchar        *b)
{
    if(rgb == NULL)
	    return;
    if (r) *r = ROUND (CLAMP (rgb->r, 0.0, 1.0) * 255.0);
    if (g) *g = ROUND (CLAMP (rgb->g, 0.0, 1.0) * 255.0);
    if (b) *b = ROUND (CLAMP (rgb->b, 0.0, 1.0) * 255.0);
}
static gboolean file_is_color (const char *fn)
{
    return fn && strstr (fn, ".ppm");
}
static void set_colorbrushes (const gchar *fn)
{
    pcvals.color_brushes = file_is_color (fn);
}
static size_t g_strlcpy (gchar       *dest,
           const gchar *src,
           size_t        dest_size)
{
    gchar *d = dest;
    const gchar *s = src;
    size_t n = dest_size;

    if(!dest)
        return 0;
    if(!src)
	    return 0; 

    /* Copy as many bytes as will fit */
    if (n != 0 && --n != 0)
    do
    {
        register gchar c = *s++;

        *d++ = c;
        if (c == 0)
            break;
    }
    while (--n != 0);

    /* If not enough room in dest, add NUL and traverse rest of src */
    if (n == 0)
    {
        if (dest_size != 0)
            *d = 0;
        while (*s++)
            ;
    }

    return s - src - 1;  /* count does not include NUL */
}

static void brush_reload (const gchar *fn, ppm_t       *p)
{
    static char  lastfn[256] = "";
    static ppm_t cache       = {0, 0, NULL};

    if (fn == NULL)
    {
        ppm_kill (&cache);
        lastfn[0] = '\0';
        return;
    }
    g_printerr("## brush_reload fn = %s ###\n", fn);
    if (strcmp (fn, lastfn))
    {
        g_strlcpy (lastfn, fn, sizeof (lastfn));
        g_printerr("## lastfn = %s ##\n", lastfn);
        ppm_kill (&cache);
        ppm_load (fn, &cache);
    }
    ppm_copy (&cache, p);
    set_colorbrushes (fn);
}
static void brush_get(const char* ppmName, ppm_t* p)
{
    ppm_load(ppmName, p);
}
static void brush_get_selected (ppm_t *p)
{
    if (brush_from_file)
    {
        g_printerr("#####brush_get_selected ####");
        brush_reload (pcvals.selected_brush, p);
	    /*
        //debug
	    Image image;
	    image.width = p->width;
	    image.height = p->height;
	    image.bpp = 3;
	    image.data = p->col;
	    save(image, "c:\\testbrush.jpg");
	    //endi
        */
    }
    else
        ppm_copy (&brushppm, p);
}

static double get_direction (double x, double y, int from)
{
    gint      i;
    gint      n;
    gint      voronoi;
    gdouble   sum, dx, dy, dst;
    vector_t *vec;
    gdouble   angoff, strexp;
    gint      first = 0, last;

    
    n = pcvals.num_orient_vectors;
    vec = pcvals.orient_vectors;
    angoff = pcvals.orient_angle_offset;
    strexp = pcvals.orient_strength_exponent;
    voronoi = pcvals.orient_voronoi;
  

    if (voronoi)
    {
        gdouble bestdist = -1.0;

        for (i = 0; i < n; i++)
        {
            dst = dist(x,y,vec[i].x,vec[i].y);

            if ((bestdist < 0.0) || (dst < bestdist))
            {
                bestdist = dst;
                first = i;
            }
        }
        last = first+1;
    }
    else
    {
        first = 0;
        last = n;
    }

    dx = dy = 0.0;
    sum = 0.0;
    for (i = first; i < last; i++)
    {
        gdouble s = vec[i].str;
        gdouble tx = 0.0, ty = 0.0;

        if (vec[i].type == 0)
        {
            tx = vec[i].dx;
            ty = vec[i].dy;
        }
        else if (vec[i].type == 1)
        {
            gdouble a = atan2 (vec[i].dy, vec[i].dx);

            a -= atan2 (y-vec[i].y, x-vec[i].x);
            tx = sin (a + G_PI_2);
            ty = cos (a + G_PI_2);
        }
        else if (vec[i].type == 2)
        {
            gdouble a = atan2 (vec[i].dy, vec[i].dx);

            a += atan2 (y-vec[i].y, x-vec[i].x);
            tx = sin (a + G_PI_2);
            ty = cos (a + G_PI_2);
        }
        else if (vec[i].type == 3)
        {
            gdouble a = atan2 (vec[i].dy, vec[i].dx);

            a -= atan2 (y-vec[i].y, x-vec[i].x)*2;
            tx = sin (a + G_PI_2);
            ty = cos (a + G_PI_2);
        }

        dst = dist (x,y,vec[i].x,vec[i].y);
        dst = pow (dst, strexp);

        if (dst < 0.0001)
            dst = 0.0001;
        s = s / dst;

        dx += tx * s;
        dy += ty * s;
        sum += s;
    }
    dx = dx / sum;
    dy = dy / sum;

    return 90 - (gimp_rad_to_deg (atan2 (dy, dx)) + angoff);
}
static double get_siz_from_pcvals (double x, double y)
{
    return getsiz_proto (x, y, pcvals.num_size_vectors, pcvals.size_vectors,
                       pcvals.size_strength_exponent, pcvals.size_voronoi);
}

static int get_pixel_value (double dir)
{
    while (dir < 0.0)
        dir += 360.0;
    while (dir >= 360.0)
        dir -= 360.0;
    return dir * 255.0 / 360.0;
}

static void prepare_brush (ppm_t *p)
{
    /*
    int x, y;
    int rowstride = p->width * 3;

    for (y = 0; y< p->height; y++)
    {
        for (x = 0; x < p->width; x++)
        {
            p->col[y * rowstride + x * 3 + 1] = 0;
        }
    }

    for (y = 1; y< p->height; y++)
    {
        for (x = 1; x < p->width; x++)
        {
            int v = p->col[y * rowstride + x * 3] -
                  p->col[(y - 1) * rowstride + (x - 1) * 3];
            if (v < 0)
                v = 0;
            p->col[y * rowstride + x * 3 + 1] = v;
        }
    }
     */
}

static double sum_brush (ppm_t *p)
{
    double sum = 0;
    int i;

    for (i = 0; i < p->width*3*p->height; i += 3)
        sum += p->col[i];
    return sum;
}

/* TODO : Use r = rgb[0]; g = rgb[1] ; b = rgb[2]; instead of
 * the direct references here.
 * */
static int get_hue (guchar *rgb)
{
    double h, v, temp, diff;
    /* TODO : There seems to be some typoes in the comments here.
     * Ask vidar what he meant.
     * */
    if ((rgb[0] == rgb[1]) && (rgb[0] == rgb[2])) /* Gray */
        return 0;
    v = (rgb[0] > rgb[1] ? rgb[0] : rgb[1]);     /* v = strste verdi */
    if (rgb[2] > v)
        v = rgb[2];
    temp = (rgb[0] > rgb[1] ? rgb[1] : rgb[0] ); /* temp = minste */
    if (rgb[2] < temp)
        temp = rgb[2];
    diff = v - temp;

    if (v == rgb[0])
        h = ((double)rgb[1] - rgb[2]) / diff;
    else if(v == rgb[1])
        h = ((double)rgb[2] - rgb[0]) / diff + 2;
    else /* v == rgb[2] */
        h = ((double)rgb[0] - rgb[1]) / diff + 4;
    if(h < 0) h += 6;
    return h * 255.0 / 6.0;
}
template <typename T> bool g_list_nth(std::list<T>& data, int index, T& outData)
{
	typename std::list<T>::iterator it = data.begin();
	int i = 0;
	while(it != data.end() && i < index)
	{
		it++;
		i++;
	}
	if(it != data.end())
	{
		outData = *it;
		return true;
	}
	else
		return false;
}
struct EdgePoint
{
    int x, y;
    EdgePoint()
    {
        x = y = 0;
    }
    EdgePoint(int x,int y)
    {
        this->x = x;
        this->y = y;
    }
};
static float createBrushAngle(std::list<EdgePoint>& edgePointList, int brushWidth, int brushHeight)
{
    if(edgePointList.size() == 1)
        return 0;
    int maxX = INT_MIN;
    int minX = INT_MAX;
    int maxY = INT_MIN;
    int minY = INT_MAX;
    std::list<EdgePoint>::iterator it;
    std::list<EdgePoint>::iterator minIt = edgePointList.end();
    std::list<EdgePoint>::iterator maxIt = edgePointList.end();
    for(it = edgePointList.begin() ; it != edgePointList.end() ; it++)
    {
        if(it->x > maxX)
        {
            maxX = it->x;
            maxIt = it;
        }
        if(it->x < minX)
        {
            minX = it->x;
            minIt = it;
        }
    }
    if(minIt == edgePointList.end() || maxIt == edgePointList.end())
    {
        return 0;
    }
    if(minX == maxX)
        return 3.1415926 / 2;
    float slope = (maxIt->y - minIt->y) / (maxX - minX);
    float theta = atanf(slope);
    if(theta < 0)
    {
        theta += 3.1415926;
    }
    return theta;
}
static int choose_brush_by_radian(double* brushAngles, int num_brushes, double outRadian)
{
    double angle = outRadian * 180.0 / 3.1415926;
    int ret = 0;
    double delta = 360.0f;
    for(int i = 0 ; i < num_brushes ; i++)
    {
        double d = fabs(brushAngles[i] - angle);
        if(d < delta)
        {
            delta = d;
            ret = i;
        }
    }
    return ret;
}
static bool brushOnEdge(ppm_t* edgeDetectionMap, int brushWidth, int brushHeight, int tx, int ty, float& outRadian)
{
    int x, y;
    double r, g , b;
    bool found = false;
    int bottom = edgeDetectionMap->height;
    int right = edgeDetectionMap->width;
    std::list<EdgePoint> edgePointList;
    tx -= brushWidth;
    ty -= brushHeight;
    for (y = 0; y < brushHeight; y++)
    {
        if((ty + y) < bottom && (ty + y) >= 0)
        {
            guchar *row = edgeDetectionMap->col + (ty + y) * edgeDetectionMap->width * 3;
            for (x = 0; x < brushWidth; x++)
            {
                if((tx + x) < right && (tx + x) >= 0)
                {
                    int    k = (tx + x) * 3;
                    r = row[k+0];
                    g = row[k+1];
                    b = row[k+2];
                    //double gray = (r + b + g) / 3;
                    //if(gray <= 255.0 && gray > 191)
                    /*
                    if((r <= 255 && r >= 200) || 
                       (g <= 255 && g >= 200) ||
                       (b <= 255 && b >= 200))
                     */
                    if(r == 255)
                    {
                        //edgePointList.push_back(EdgePoint(x, y));
                        found = true;
                        //LOGI("## gray = %f ##\n", gray);
                        break;
                    }
                }
                //else 
                //{
                //    break;
                //}
            }
            if(found)
                break;
        }
    }
    //outRadian = createBrushAngle(edgePointList, brushWidth, brushHeight);
    //found = edgePointList.size() > 0;
    return found;

}
static double calculateBrushDev(ppm_t* p , ppm_t* a ,ppm_t* brush, int tx, int ty, double thissum)
{
    double dev = 0;
    int r, g, b;
    int x, y, h;
    r = g = b = 0.0;
    for (y = 0; y < brush->height; y++)
    {
        if((ty + y) < 0)
            continue;
        if((ty + y) >= p->height)
            continue;
        
        guchar *row = p->col + (ty + y) * p->width * 3;
        
        for (x = 0; x < brush->width; x++)
        {
            if((tx + x ) < 0)
                continue;
            if((tx + x) >= p->width)
                continue;
            int    k = (tx + x) * 3;
            double v;
            
            if ((h = brush->col[(y * brush->width * 3) + x * 3]))
            {
                v = h / 255.0;
                r += row[k+0] * v;
                g += row[k+1] * v;
                b += row[k+2] * v;
            }
        }
    }
    r = r * 255.0 / thissum;
    g = g * 255.0 / thissum;
    b = b * 255.0 / thissum;
    
    dev = 0.0;
    for (y = 0; y < brush->height; y++)
    {
        if((ty + y) < 0)
            continue;
        if((ty + y) >= p->height)
            continue;
        

        guchar *row = p->col + (ty + y) * p->width * 3;
        
        for (x = 0; x < brush->width; x++)
        {
            if((tx + x ) < 0)
                continue;
            if((tx + x) >= p->width)
                continue;
            int    k = (tx + x) * 3;
            double v;
            
            if ((h = brush->col[(y * brush->width * 3) + x * 3]))
            {
                v = h / 255.0;
                dev += abs (row[k+0] - r) * v;
                dev += abs (row[k+1] - g) * v;
                dev += abs (row[k+2] - b) * v;
                if (img_has_alpha)
                    dev += a->col[(ty + y) * a->width * 3 + (tx + x) * 3] * v;
            }
        }
    }
    dev /= thissum;
    return dev;    
}
static ppm_t rotateBrush(ppm_t* srcBrush, float angle)
{
    ppm_t tmp = {0, 0, NULL};
    ppm_copy(srcBrush, &tmp);
    free_rotate(&tmp, angle);
    //autocrop (&tmp,1);
    return tmp;
}
struct PointRGB
{
    int x, y;
    int rgb;
};
static bool pointInBrushArea(int x, int y, std::list<PointRGB>& list, int& rgb)
{
    for(std::list<PointRGB>::iterator it = list.begin(); it != list.end(); it++)
    {
        if(x == it->x && y == it->y)
        {
            rgb = it->rgb;
            return true;
        }
    }
    return false;
}
static bool pointInCircle(float brushWidth, float brushHeight, float x, float y)
{
    float w = brushWidth > brushHeight ? brushHeight : brushWidth;
    float circlex = w / 2;
    float circley = w / 2;
    float radius = w * w / 4.0;
    float dist = (x - circlex) * (x - circlex) + (y - circley) * (y - circley);
    if(dist > radius)
        return  false;
    else 
    {
        return true;
    }
}
struct MyRGB
{
    double r, g , b;
    MyRGB()
    {
        r = g = b = 0;
    }
    MyRGB(double r, double g, double b)
    {
        this->r  = r;
        this->g  = g;
        this->b = b;
    }
};
static  MyRGB calculateColor(ppm_t* p, ppm_t* brush, int tx, int ty, int x, int y, int w, int h, double c , double s, bool& canAdd)
{
    int nx = (int)((x * c - y * s + (1 - c) * w / 2 + s * h /2) + 0.5);
    int ny = (int)((x * s + y * c + (1 - c) * h / 2 - w * s / 2) + 0.5);
    if((ty + ny) >= p->height)
    {
        canAdd = false;
        return MyRGB();
    }
    if((ty + ny) < 0)
    {
        canAdd = false;
        return MyRGB();
    }
    if((tx + nx ) < 0)
    {
        canAdd = false;
        return MyRGB();
    }
    if((tx + nx ) >= p->width)
    {
        canAdd = false;
        return MyRGB();
    }
    if(!pointInCircle(brush->width, brush->height, nx, ny))
    {
        canAdd = false;
        return MyRGB();
    }
    /*
    if(nx < 0)
        return 0;
    if(nx >= brush->width)
        return 0;
    if(ny < 0)
        return 0;
    if(ny >= brush->height)
        return 0;
     */
    guchar* data = p->col + (ty + ny) * p->width * 3 + (tx + nx) * 3;
    //int rgb = data[0] + data[1] + data[2];
    canAdd = true;
    return MyRGB(data[0], data[1], data[2]);
    //return rgb;
}
static std::vector<double> calculateArea(double r)
{
    double circleArea = 2 * r * r * 3.1415926;
    double thirtyAngleArea = circleArea * 30 / 360;
    double bigAngleArea = circleArea * 120 / 360;
    double base = sqrt(r * r - r * r / 4);
    double triArea = base * r / 2;
    
    double firstArea = bigAngleArea - triArea;
    double secondArea = triArea + 2 * thirtyAngleArea;
    double thirdArea = secondArea;
    double fourArea = firstArea;
    
    double allArea = firstArea + secondArea + thirdArea + fourArea;
    double ret = fabs(allArea - circleArea);
    assert(ret < 0.001);
    std::vector<double> retV(4);
    retV[0] = firstArea;
    retV[1] = secondArea;
    retV[2] = thirdArea;
    retV[3] = fourArea;
    return retV;
}
static double getRgbValue1(MyRGB* rgbArea)
{
    double f1 = fabs(rgbArea[0].r + rgbArea[1].r - rgbArea[2].r - rgbArea[3].r) + fabs(rgbArea[0].g + rgbArea[1].g - rgbArea[2].g - rgbArea[3].g) + fabs(rgbArea[0].b + rgbArea[1].b - rgbArea[2].b - rgbArea[3].b);
    return f1;
}
static double getRgbValue2(MyRGB* rgbArea)
{
    double f1 = fabs(rgbArea[0].r - rgbArea[1].r) + fabs(rgbArea[2].r - rgbArea[3].r) + fabs(rgbArea[0].g - rgbArea[1].g) + fabs(rgbArea[2].g - rgbArea[3].g) + fabs(rgbArea[0].b - rgbArea[1].b) + fabs(rgbArea[2].b - rgbArea[3].b);
    return f1;
}
static double getRgbValue3(MyRGB* rgbArea)
{
    double f1 = fabs(rgbArea[0].r - rgbArea[1].r) + fabs(rgbArea[0].g - rgbArea[1].g) + fabs(rgbArea[0].b - rgbArea[1].b);
    return f1;
}

static POINT_TYPE pointInPosition(double brushWidth, double brushHeight, int x, int y, double m, double angle)
{
    if(!pointInCircle(brushWidth, brushHeight, x, y))
    {
        return OTHER_POS;    
    }
    double w = brushWidth > brushHeight ? brushHeight : brushWidth;
    double radius = w / 2;
    if(angle == 90 || angle == -90 || angle == 270)
    {
        if(x < radius)
            return NEGTIVE;
        else if(x > radius)
            return POSITIVE;
        else 
        {
            return OTHER_POS;
        }
    }
    double b = radius - m * radius;
    double result = y - m * x - b;
    if(result > 0)
    {
        return POSITIVE;
    }
    else if(result < 0)
    {
        return NEGTIVE;
    }
    else 
    {
        return OTHER_POS;
    }
}
static POINT_TYPE pointInPosition(BrushMapDataMap& brushMapData, int x, int y, double angle)
{
    /*
    int realAngle = -1800;
    if(angle > 180)
        angle = angle - 180;
    int srcAngle = (int)(angle * 10);
    while (realAngle >= -1800 && realAngle <= 1800 && (realAngle - srcAngle) >= 5) {
        realAngle += 5;
    }
    BrushMapDataMap::iterator it = brushMapData.find(realAngle);
    assert(it != brushMapData.end());
    BrushMapData& bmd = brushMapData[realAngle];
    return bmd.data[y][x];
     */
}
static double calculateDifference(ppm_t*p, ppm_t* brush, int tx, int ty, double angle, BrushMapDataMap& brushMapData)
{
    MyRGB rgb[POINT_TYPE_NUM];
    int pixelCount[POINT_TYPE_NUM];
    for(int i = 0 ; i < POINT_TYPE_NUM ; i++)
    {
        pixelCount[i] = 0;
    }
    int index = (int)angle + 360;
    //NSLog(@"angle index = %d", index);
    assert(index >= 0 && index < 721);
    BrushMapData& bmd = brushMapData.data[index];
    for(int y = 0 ; y < gBrushMaxHeight ; y ++)
    {
        if(ty + y < 0)
            continue;
        if(ty + y >= p->height)
            break;
        for(int x = 0 ; x < gBrushMaxWidth ; x ++)
        {
            if(tx + x < 0)
                continue;
            if(tx + x >= p->width)
                break;
            POINT_TYPE pType = bmd.data[y][x];
            int r = 0 , g = 0 , b = 0;
            if(pType != OTHER_POS)
            {
                guchar* data = p->col + (ty + y) * p->width * 3 + (tx + x) * 3;
                r = data[0];
                g = data[1];
                b = data[2];
                rgb[pType].r += r;
                rgb[pType].g += g;
                rgb[pType].b += b;
                pixelCount[pType]++;
            }
            else
            {
                pixelCount[pType]++;    
            }
        }
    }

    /*
    BrushMapDataMap::iterator it = brushMapData.find((int)(angle));
    if(it != brushMapData.end())
    {
        BrushMapData& bmd = it->second;
        for(int y = 0 ; y < brush->height ; y += 2)
        {
            if(ty + y < 0)
                continue;
            if(ty + y >= p->height)
                break;
            for(int x = 0 ; x < brush->width ; x += 2)
            {
                if(tx + x < 0)
                    continue;
                if(tx + x >= p->width)
                    break;
                POINT_TYPE pType = bmd.data[y][x];
                int r = 0 , g = 0 , b = 0;
                if(pType != OTHER_POS)
                {
                    guchar* data = p->col + (ty + y) * p->width * 3 + (tx + x) * 3;
                    r = data[0];
                    g = data[1];
                    b = data[2];
                    rgb[pType].r += r;
                    rgb[pType].g += g;
                    rgb[pType].b += b;
                    pixelCount[pType]++;
                }
                else
                {
                    pixelCount[pType]++;    
                }
            }
        }

    }
    else
    {
        double m = 0;
        if(angle != 90 && angle != -90 && angle != 270)
            m = tan(angle * 3.1415926 / 180);
        BrushMapData bmp;
        bmp.data.resize(brush->height);
        for(int i = 0 ; i < brush->height ; i++)
        {
            bmp.data[i].resize(brush->width);
        }
        for(int y = 0 ; y < brush->height ; y += 2)
        {
            //if(ty + y < 0)
            //    continue;
            //if(ty + y >= p->height)
            //    break;
            for(int x = 0 ; x < brush->width ; x += 2)
            {
                //if(tx + x < 0)
                //    continue;
                //if(tx + x >= p->width)
                //    break;
                POINT_TYPE pType = pointInPosition(brush->width, brush->height,  x, y, m, angle);
                int r = 0 , g = 0 , b = 0;
                bmp.data[y][x] = pType;
                if(pType != OTHER_POS)
                {
                    if(ty + y >= 0 && ty + y < p->height && tx + x >= 0 && tx + x < p->width)
                    {
                    
                        guchar* data = p->col + (ty + y) * p->width * 3 + (tx + x) * 3;
                        r = data[0];
                        g = data[1];
                        b = data[2];
                        rgb[pType].r += r;
                        rgb[pType].g += g;
                        rgb[pType].b += b;
                        pixelCount[pType]++;
                    }
                }
                else
                {
                    pixelCount[pType]++;    
                }
            }
        }
        int keyAngle = (int)(angle);
        brushMapData[keyAngle] = bmp;
    }
     */
    double f = fabs(rgb[POSITIVE].r - rgb[NEGTIVE].r) + fabs(rgb[POSITIVE].g - rgb[NEGTIVE].g) + fabs(rgb[POSITIVE].b - rgb[POSITIVE].b);
    return f;
}
//static int gPass = 0;

static double calculateRgbBeneathBrush(ppm_t* p, ppm_t* brush, int startx, int starty, int endx, int endy, int tx, int ty, double angle, double (*getRgbValue) (MyRGB* pRgbArea))
{
    /*
    double f = angle * 3.1415926 / 180.0;
    std::list<PointRGB> pointRgbList;
    std::list<int> resultList;
    for (int y = 0; y < brush->height; y++)
    {
        if((ty + y) < 0)
            continue;
        if((ty + y) >= p->height)
            continue;
        for (int x = 0; x < brush->width; x++)
        {
            double r, d;
            int nx = fabs ((double)(x - brush->width / 2.0));
            int ny = fabs ((double)(y - brush->height / 2.0));
            r = sqrt (nx * nx + ny * ny);
            
            d = atan2 ((y - p->height / 2.0), (x - p->width / 2.0));
            
            nx = (brush->width / 2.0 + cos (d - f) * r);
            ny = (brush->height / 2.0 + sin (d - f) * r);
            if((tx + x) >= p->width)
                continue;
            if((tx + x ) < 0)
                continue;
            if(nx >=0 && nx < brush->width && ny >= 0 && ny < brush->height)
            {
                guchar* rgb = p->col + (ty + y) * p->width * 3 + (tx + x) * 3;
                PointRGB prgb;
                prgb.x = nx;
                prgb.y = ny;
                prgb.rgb = rgb[0] + rgb[1] + rgb[2];
                pointRgbList.push_back(prgb);
            }
        }
    }
    int size = pointRgbList.size();
    NSLog(@"size = %d", size);
    for(std::list<PointRGB>::iterator it = pointRgbList.begin(); it != pointRgbList.end() ; it++)
    {
        NSLog(@"x = %d, y = %d, rgb = %d", it->x, it->y, it->rgb);
    }
    int midy = starty + (endy - starty) / 2;
    for(int x = startx ; x <= endx ; x++)
    {
        int rgb1 = 0;
        int rgb2 = 0;
        for(int y = starty ; y <= midy ; y++)
        {
            int rgb = 0;
            bool bIn = pointInBrushArea(x, y, pointRgbList, rgb);
            if(bIn)
            {
                NSLog(@"(x, y) = (%d, %d)", x, y);
                rgb1 += rgb;
            }
        }
        for(int y = midy ; y < endy ; y++)
        {
            int rgb = 0;
            bool bIn = pointInBrushArea(x, y, pointRgbList, rgb);
            if(bIn)
            {
                NSLog(@"(x, y) = (%d, %d)", x, y);
                rgb2 += rgb;
            }
        }
        resultList.push_back(abs(rgb1 - rgb2));
    }
    int sum = 0;
    for(std::list<int>::iterator it = resultList.begin();
        it != resultList.end();
        it++)
    {
        sum += *it;
    }
    return sum;
     */
    double radian = angle * 3.1415926 / 180;
    double c = cos(radian);
    double s = sin(radian);
    int w = brush->width;
    int h = brush->height;
    std::list<double> resultList;
    
    //std::vector<double> areas = calculateArea(brush->width / 2);
    const int AREA_NUM = 2;
    MyRGB rgbArea[AREA_NUM];
    int pixelCounts[AREA_NUM];
    for(int i = 0 ; i < AREA_NUM ; i ++)
    {
        pixelCounts[i] = 0;
    }
    //for(int x = startx ; x < endx ;  x++)
    for(int x = 0 ; x < brush->width ; x ++)
    {
        MyRGB rgb[AREA_NUM];
        float step = (brush->height -1 )/ (float)AREA_NUM;
        //float step = (endy - starty) / (float) AREA_NUM;
        for(int i = 0 ; i < AREA_NUM; i++)
        {
            int tmpStarty = step * i;
            int tmpEndy = step * (i + 1);
            int count = 0;
            //NSLog(@"### piece num = %d ", (tmpEndy - tmpStarty + 1));
            //for(int y = tmpStarty ; y <= tmpEndy ; y += 2)
            for(int y = tmpStarty ; y <= tmpEndy ; y++)
            {
                bool canAdd = false;
                MyRGB color = calculateColor(p, brush,tx, ty, x, y, w, h, c, s, canAdd);
                rgb[i].r += color.r;
                rgb[i].g += color.g;
                rgb[i].b += color.b;
                if(canAdd)
                {
                    //if(gPass == 3)
                    //    NSLog(@"## color = %f , %f, %f ###", color.r, color.g, color.b);
                    count++;
                    pixelCounts[i]++;
                }
            }
            
            //if(count > 0)
            {
                rgbArea[i].r += rgb[i].r;// / count;
                rgbArea[i].g += rgb[i].g;// / count;
                rgbArea[i].b += rgb[i].b;// / count;
            } 
            
        }

        
        for(int i = 0 ; i < (AREA_NUM - 1) ; i++)
        {
            resultList.push_back(fabs(rgbArea[i].r - rgb[i + 1].r) + fabs(rgb[i].g - rgb[i + 1].g) + fabs(rgb[i].b - rgb[i + 1].b));
        }
        
    }
    
     double sum = 0;
     for(std::list<double>::iterator it = resultList.begin();
         it != resultList.end();
         it++)
     {
         sum += *it;
     }
     
     
    /*
    for(int i = 0 ; i < AREA_NUM ; i++)
    {
        rgbArea[i].r /= pixelCounts[i];
        rgbArea[i].g /= pixelCounts[i];
        rgbArea[i].b /= pixelCounts[i];
    }
    double sum = 0;
    sum = (*getRgbValue3)(rgbArea);
     */
    return sum;
}
static void calculateBrushBound(ppm_t* brush, int& startx, int& starty, int& endx, int& endy)
{
    bool found = false;
    startx = brush->width;
    starty = brush->height;
    endx = -1;
    endy = -1;
    for(int y = 0 ; y < brush->height ; y++)
    {
        guchar* data = brush->col + y * brush->width * 3;
        for(int x = 0 ; x < brush->width ; x++)
        {
            guchar* rgb = data + x * 3;
            if(rgb[0] > 10)
            {
                starty = y;
                found = true;
                break;
            }
        }
        if(found)
            break;
    }
    assert(starty != brush->height);
    found = false;
    for(int y = brush->height - 1 ; y >= 0 ; y--)
    {
        guchar* data = brush->col + y * brush->width * 3;
        for(int x = 0  ; x < brush->width ; x++)
        {
            guchar* rgb = data + x * 3;
            if(rgb[0] > 10)
            {
                endy = y;
                found = true;
                break;
            }
        }
        if(found)
            break;
    }
    assert(endy != -1);
    found = false;
    for(int x = 0 ; x < brush->width ; x++)
    {
        for(int y = 0 ; y < brush->height ; y++)
        {
            guchar* rgb = brush->col + y * brush->width * 3 + x * 3;
            if(rgb[0] > 10)
            {
                startx = x;
                found = true;
                break;
            }
        }
        if(found)
            break;
    }
    assert(startx != brush->width);
    found = false;
    for(int x = brush->width - 1 ; x >= 0 ; x--)
    {
        for(int y = 0 ; y < brush->height ; y++)
        {
            guchar* rgb = brush->col + y * brush->width * 3 + x * 3;
            if(rgb[0] > 10)
            {
                endx = x;
                found = true;
                break;
            }
        }
        if(found)
            break;
    }
    assert(endx != -1);
}
static ppm_t calculateRotateBrush(ppm_t* p , double angle)
{
    int startx , starty, endx, endy;
    calculateBrushBound(p, startx, starty, endx, endy);
    ppm_t tmp = {0, 0, NULL};
    ppm_new(&tmp, p->width, p->height);
    double radian = angle * 3.1415926 * 2 / 360.0;
    double c = cos(radian);
    double s = sin(radian);
    int w = p->width;
    int h = p->height;
    int    rowstride = p->width * 3;
    for(int y = starty ; y <= endy ; y++)
    {
        for(int x = startx ; x <= endx ; x++)
        {
            int nx = (int)((x * c - y * s + (1 - c) * w / 2 + s * h /2) + 0.5);
            int ny = (int)((x * s + y * c + (1 - c) * h / 2 - w * s / 2) + 0.5);
            //get_rgb (p, x, y, tmp.col + ny * rowstride + nx * 3);
            guchar* rgb = tmp.col + ny * tmp.width * 3 + nx * 3;
            rgb[0] = 255;
            rgb[1] = 0;
            rgb[2] = 0;
        }
    }
    return tmp;
}
static void setBrushBoundColor(ppm_t* brush)
{
    int startx, starty, endx, endy;
    calculateBrushBound(brush, startx, starty, endx, endy);
    for(int y = starty ; y <= endy ; y++)
    {
        guchar* data = brush->col + y * brush->width * 3;
        for(int x = startx ; x <= endx ; x++)
        {
            guchar* gdb = data + x * 3;
            gdb[0] = 255;
            gdb[1] = 0;
            gdb[2] = 0;
        }
    }
}

struct BrushAngleData
{
    double angle;
    double dev;
    BrushAngleData(double a, double d)
    {
        angle = a;
        dev = d;
    }
};
static bool compareAngleBrushData(BrushAngleData& first, BrushAngleData & second)
{
    if(first.dev < second.dev)
        return true;
    else 
    {
        return false;
    }
}
static double restrictAngleIn360(double angle)
{
    while (angle > 360)
    {
        angle -= 360;
    }
    return angle;
}
struct MySum
{
    double sum;
    double angle;
};
static bool mySumCompare(MySum& first, MySum& second)
{
    if(first.sum < second.sum)
        return true;
    else {
        return false;
    }
}
static MyBrushData getBrushDataByAngle(double angle, std::vector<MyBrushData>& angleList);
static double choose_best_brush_angle(ppm_t*p, ppm_t* a, int tx, int ty, std::vector<MyBrushData>& brushDataList, bool& goodAngle, BrushMapDataMap& brushMapData)
{
    double startAngle = 0;
    double endAngle = 180;
    int sliceNum = 6;
    double step = (endAngle - startAngle) / sliceNum;
    int times = 2;
    int timeIndex = 0;
    double bestAngle = 0;
    double dev = 1000;
    double destAngle = 0;
    std::vector<MyBrushData>::iterator it = brushDataList.begin();
    MyBrushData md = *it;
    double sum = INT_MAX;
    int startx = md.startx, starty = md.starty, endx = md.endx, endy = md.endy;
    //SS_SaveBrush("SampleBrush", 0, md.brush);
    //calculateBrushBound(&md.brush, startx, starty, endx, endy);
    std::list<MySum> angleList;
    /*
    for(int angle = 0 ; angle <= 180 ; angle += 5)
    {
        double s = calculateRgbBeneathBrush(p, &md.brush, startx, starty, endx, endy, tx, ty, angle, &getRgbValue1);
        MySum ms;
        ms.sum = s;
        ms.angle = angle;
        angleList.push_back(ms);
    }
    angleList.sort(&mySumCompare);
    std::list<MySum> resultList;
    for(std::list<MySum>::iterator it = angleList.begin() ;
        it != angleList.end() ;)
    {
        MySum first = *it;
        it++;
        MySum second = *it;
        if(first.sum == second.sum && first.sum != 0.0)
        {
            NSLog(@"first sum = %f, second sum = %f", first.sum, second.sum);
            resultList.push_back(first);
            resultList.push_back(second);
            goodAngle = false;
            break;
        }
        else
        {
            resultList.push_back(first);
            goodAngle = true;
            break;
        }
    }
    assert(resultList.size() > 0);
    if(resultList.size() == 1)
    {
        std::list<MySum>::iterator it = resultList.begin();
        MySum first = *it;
        destAngle = first.angle + 90;
    }
    else
    {
        NSLog(@"### second algorithm #####");
        sum = INT_MAX;
        for(std::list<MySum>::iterator it = resultList.begin();
            it != resultList.end(); it++)
        {
            MySum t = *it;
            double s = calculateRgbBeneathBrush(p, &md.brush, startx, starty, endx, endy, tx, ty, t.angle, &getRgbValue2);
            if(s < sum)
            {
                sum = s;
                destAngle = t.angle + 90;
            }
        }
    }
    return destAngle;
     */
    /*
    for(int angle = 0 ; angle <= 180 ; angle += 10)
    {
        MyBrushData bd = getBrushDataByAngle(angle, brushDataList);
        ppm_t tmp = bd.brush;
        double brush_sum = bd.sum;
        double d = calculateBrushDev(p, a, &tmp, tx, ty, brush_sum);
        if(d < dev)
        {
            dev = d;
            destAngle = angle;
        }
    }
     */
    /*
    for(int i = 0 ; i < runningvals.orient_num ; i++)
    {
        double angle = md.anglefirst + i * md.anglespan / (runningvals.orient_num - 1);
        //NSLog(@"choose angle = %f", angle);
        //angle = restrictAngleIn360(angle);
        MyBrushData bd = getBrushDataByAngle(angle, brushDataList);
        ppm_t tmp = bd.brush;
        double brush_sum = bd.sum;
        double d = calculateBrushDev(p, a, &tmp, tx, ty, brush_sum);
        if(d < dev)
        {
            dev = d;
            destAngle = angle;
        }
    }
    return destAngle;
     */
    
    while (timeIndex < times)
    {
        std::list<BrushAngleData> angleDataList;
        //angleDataList.push_back(BrushAngleData(startAngle, 0));
        for(int i = 0 ; i <= sliceNum ; i++)
        {
            double angle = startAngle + step * i;
            //NSLog(@"angle = %f", angle);
            angleDataList.push_back(BrushAngleData(angle, 0));
        }
        std::list<BrushAngleData>::iterator angleIt;
        for(angleIt = angleDataList.begin() ; angleIt != angleDataList.end() ; angleIt++)
        {
            //double s = calculateRgbBeneathBrush(p, &md.brush, startx, starty, endx, endy, tx, ty, angleIt->angle, &getRgbValue1);
            double s = calculateDifference(p, &md.brush, tx, ty, angleIt->angle, brushMapData);
            angleIt->dev = s;
        }
        angleDataList.sort(&compareAngleBrushData);
        /*
        NSLog(@"angleDataList size = %ld", angleDataList.size());
        for(std::list<BrushAngleData>::iterator it = angleDataList.begin();
            it != angleDataList.end();
            it++)
        {
            NSLog(@"angle = %f, dev = %f", it->angle, it->dev);
        }
         */
        angleIt = angleDataList.begin();
        BrushAngleData first = *angleIt;
        bestAngle = first.angle;
        startAngle = first.angle - step / 2;
        endAngle = first.angle + step / 2;
        startAngle = std::min(startAngle, endAngle);
        endAngle = std::max(startAngle, endAngle);
        step = (endAngle - startAngle) / sliceNum;
        timeIndex++;    
    }
    return bestAngle + 90;
}

static int choose_best_brush (ppm_t* edgeDetectionMap, ppm_t *p, ppm_t *a, int tx, int ty,
                   ppm_t *brushes, int num_brushes,
                   double *brushes_sum, int start, int step)
{
    double dev, thissum;
    double bestdev = 0.0;
    double r, g, b;
    long    best = -1;
    int    x, y, h;
    long   i;
    //GList *brlist = NULL;
    std::list<long> brlist;
    for (i = start; i < num_brushes; i += step)
    {
        ppm_t *brush = &brushes[i];
#if 0
        thissum = 0.0;
#endif
        thissum = brushes_sum[i];

        /* TODO: Pointer-arithmeticize this code */
        r = g = b = 0.0;
        for (y = 0; y < brush->height; y++)
        //for (y = 0; y < brush->height; y += 3)
        {
            guchar *row = p->col + (ty + y) * p->width * 3;

            for (x = 0; x < brush->width; x++)
            //for (x = 0; x < brush->width; x += 3)
            {
                int    k = (tx + x) * 3;
                double v;

                if ((h = brush->col[(y * brush->width * 3) + x * 3]))
                {
#if 0
                    thissum += h;
#endif
                    v = h / 255.0;
                    r += row[k+0] * v;
                    g += row[k+1] * v;
                    b += row[k+2] * v;
                }
            }
        }
        r = r * 255.0 / thissum;
        g = g * 255.0 / thissum;
        b = b * 255.0 / thissum;

        dev = 0.0;
        for (y = 0; y < brush->height; y++)
        //for (y = 0; y < brush->height; y += 3)
        {
            guchar *row = p->col + (ty + y) * p->width * 3;

            for (x = 0; x < brush->width; x++)
            //for (x = 0; x < brush->width; x += 3)
            {
                int    k = (tx + x) * 3;
                double v;

                if ((h = brush->col[(y * brush->width * 3) + x * 3]))
                {
                    v = h / 255.0;
                    dev += abs (row[k+0] - r) * v;
                    dev += abs (row[k+1] - g) * v;
                    dev += abs (row[k+2] - b) * v;
                    if (img_has_alpha)
                        dev += a->col[(ty + y) * a->width * 3 + (tx + x) * 3] * v;
                }
            }
        }
        dev /= thissum;
        double dist = fabs(dev - bestdev);
        //LOGI("## brush dis = %f ##\n", dist);
        if ((best == -1) || (dev < bestdev))
        //if (best == -1)
        {
            brlist.clear();
        }
        //LOGI("## dev = %f ##\n", dev);
        if (dev <= bestdev || best < 0)
        //if(( dev < bestdev) || best < 0)
        {
            best = i;
            bestdev = dev;
            brlist.push_back(i);
        }
        if (dev < runningvals.devthresh)
            break;
    } 

    if (brlist.empty())
    {
        LOGI("What!? No brushes?!\n");
        return 0;
    }
    /*
    //TODO: need change
    i = g_rand_int_range (random_generator, 0, brlist.size());
    //LOGI("## best brush size = %lu, i = %lu  ##\n", brlist.size(), i);
    bool ret = g_list_nth<long> (brlist,i, best);
    if(ret == 0)
	    LOGI("error best value\n");
    //TODO: end
     */
    return best;
     
    //i = g_rand_int_range(random_generator, 0, num_brushes);
    //return i;
}
static bool savebrush = false;
void apply_brush_area (ppm_t *brush,
             ppm_t *shadow,
             ppm_t *p, ppm_t *a,
             int tx, int ty, int r, int g, int b, int transparent)
{
    ppm_t  tmp;
    ppm_t  atmp;
    double v, h;
    int    x, y;
    double edgedarken = 1.0 - runningvals.general_dark_edge;
    double relief = runningvals.brush_relief / 100.0;
    int    shadowdepth = pcvals.general_shadow_depth;
    int    shadowblur = pcvals.general_shadow_blur;
    
    atmp.col = 0;
    atmp.width = 0;
    /*
    if(savebrush == false)
    {
        savebrush = true;
        unsigned char* data = (unsigned char* )malloc(brush->width * brush->height * 4);
        size_t bytesPerRow = brush->width * 4;
        for(int i = 0 ; i < brush->height ; i++)
        {
            for(int j = 0 ; j < brush->width; j++)
            {
                data[i * bytesPerRow + j * 4] = r;
                data[i * bytesPerRow + j * 4 + 1] = g;
                data[i * bytesPerRow + j * 4 + 2] = b;
                data[i * bytesPerRow + j * 4 + 3] = brush->col[i * brush->width * 3 + j * 3];
            }
        }
        CFDataRef dstData = CFDataCreate(NULL, data, bytesPerRow * brush->height);
        free(data);
        CGDataProviderRef dstDataProvider = CGDataProviderCreateWithCFData(dstData);
        CFRelease(dstData);
        CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
        CGImageRef dstImageRef = CGImageCreate(brush->width, brush->height, 8, 32, bytesPerRow, colorSpace, kCGImageAlphaLast, dstDataProvider, NULL, TRUE, kCGRenderingIntentDefault);
        CGDataProviderRelease(dstDataProvider);
        UIImage* uiImage = [UIImage imageWithCGImage:dstImageRef];
        CGImageRelease(dstImageRef);
        CGColorSpaceRelease(colorSpace);
        [SEUtil savePNGImageToDocument:uiImage withName:@"brushWithColor"];
    }
     */
    tmp = *p;
    if (img_has_alpha)
        atmp = *a;
    
    if (shadow)
    {
        int sx = tx + shadowdepth - shadowblur * 2;
        int sy = ty + shadowdepth - shadowblur * 2;
        
        for (y = 0; y < shadow->height; y++)
        {
            guchar *row, *arow = NULL;
            
            if ((sy + y) < 0)
                continue;
            if ((sy + y) >= tmp.height)
                break;
            row = tmp.col + (sy + y) * tmp.width * 3;
            
            if (img_has_alpha)
                arow = atmp.col + (sy + y) * atmp.width * 3;
            
            for (x = 0; x < shadow->width; x++)
            {
                int k = (sx + x) * 3;
                
                if ((sx + x) < 0)
                    continue;
                if ((sx + x) >= tmp.width)
                    break;
                
                h = shadow->col[y * shadow->width * 3 + x * 3 + 2];
                
                if (!h)
                    continue;
                v = 1.0 - (h / 255.0 * runningvals.general_shadow_darkness / 100.0);
                
                row[k+0] *= v;
                row[k+1] *= v;
                row[k+2] *= v;
                if (img_has_alpha)
                    arow[k] *= v;
            }
        }
    }
    
    for (y = 0; y < brush->height; y++)
    {
        if((ty + y) < 0)
            continue;
        if((ty + y) >= tmp.height)
            continue;
        
        guchar *row = tmp.col + (ty + y) * tmp.width * 3;
        guchar *arow = NULL;
        
        if (img_has_alpha)
            arow = atmp.col + (ty + y) * atmp.width * 3;
        for (x = 0; x < brush->width; x++)
        {
            if((tx + x) < 0)
                continue;
            if((tx + x) >= tmp.width)
                continue;

            int k = (tx + x) * 3;
            //for test
            if(y == 0 || x == 0 || y == (brush->height -1) || x == (brush->width - 1))
            {
                //row[k] = 255;
                //row[k + 1] = 0;
                //row[k + 2] = 0;
            }
            //end
            h = brush->col[y * brush->width * 3 + x * 3];
            
            if (!h)
            { 
                //row[k+0] = 0;
                //row[k+1] = 0;
                //row[k+2] = 0;
                continue;
            }
            /*
            if (runningvals.color_brushes)
            {
                v = 1.0 - brush->col[y * brush->width * 3 + x * 3 + 2] / 255.0;
                row[k+0] *= v;
                row[k+1] *= v;
                row[k+2] *= v;
                if (img_has_alpha)
                    arow[(tx + x) * 3] *= v;
            }
             */
            float alpha = h / 255.0;
            float totalValue = alpha;// * transparent/ 100;
            v = 1 - totalValue;
            int srcR = row[k];
            int srcG = row[k + 1];
            int srcB = row[k + 2];
            row[k+0] = srcR + (r - srcR) * alpha;
            row[k+1] = srcG + (g - srcG) * alpha;
            row[k+2] = srcB + (b - srcB) * alpha;
            
            /*
            v = totalValue;
            row[k+0] += r * v;
            row[k+1] += g * v;
            row[k+2] += b * v;
             */
            //for test
            if(y == 0 || x == 0 || y == (brush->height -1) || x == (brush->width - 1))
            {
                //row[k] = 255;
                //row[k + 1] = 0;
                //row[k + 2] = 0;
            }
            //end
        }
    }
    
    if (relief > 0.001)
    {
        for (y = 1; y < brush->height; y++)
        {
            guchar *row = tmp.col + (ty + y) * tmp.width * 3;
            
            for (x = 1; x < brush->width; x++)
            {
                int k = (tx + x) * 3;
                h = brush->col[y * brush->width * 3 + x * 3 + 1] * relief;
                if (h < 0.001)
                    continue;
                if (h > 255) h = 255;
                row[k+0] = (row[k+0] * (255-h) + 255 * h) / 255;
                row[k+1] = (row[k+1] * (255-h) + 255 * h) / 255;
                row[k+2] = (row[k+2] * (255-h) + 255 * h) / 255;
            }
        }
    }
}

static void apply_brush (ppm_t *brush,
             ppm_t *shadow,
             ppm_t *p, ppm_t *a,
             int tx, int ty, int r, int g, int b)
{
    ppm_t  tmp;
    ppm_t  atmp;
    double v, h;
    int    x, y;
    double edgedarken = 1.0 - runningvals.general_dark_edge;
    double relief = runningvals.brush_relief / 100.0;
    int    shadowdepth = pcvals.general_shadow_depth;
    int    shadowblur = pcvals.general_shadow_blur;

    atmp.col = 0;
    atmp.width = 0;

    tmp = *p;
    if (img_has_alpha)
        atmp = *a;

    if (shadow)
    {
        int sx = tx + shadowdepth - shadowblur * 2;
        int sy = ty + shadowdepth - shadowblur * 2;

        for (y = 0; y < shadow->height; y++)
        {
            guchar *row, *arow = NULL;

            if ((sy + y) < 0)
                continue;
            if ((sy + y) >= tmp.height)
                break;
            row = tmp.col + (sy + y) * tmp.width * 3;

            if (img_has_alpha)
                arow = atmp.col + (sy + y) * atmp.width * 3;

            for (x = 0; x < shadow->width; x++)
            {
                int k = (sx + x) * 3;

                if ((sx + x) < 0)
                    continue;
                if ((sx + x) >= tmp.width)
                    break;

                h = shadow->col[y * shadow->width * 3 + x * 3 + 2];

                if (!h)
                    continue;
                v = 1.0 - (h / 255.0 * runningvals.general_shadow_darkness / 100.0);

                row[k+0] *= v;
                row[k+1] *= v;
                row[k+2] *= v;
                if (img_has_alpha)
                    arow[k] *= v;
            }
        }
    }

    for (y = 0; y < brush->height; y++)
    {
        
        guchar *row = tmp.col + (ty + y) * tmp.width * 3;
        guchar *arow = NULL;

        if (img_has_alpha)
            arow = atmp.col + (ty + y) * atmp.width * 3;

        for (x = 0; x < brush->width; x++)
        {
            int k = (tx + x) * 3;
            h = brush->col[y * brush->width * 3 + x * 3];

            if (!h)
            { 
                continue;
	        }

            if (runningvals.color_brushes)
            {
                v = 1.0 - brush->col[y * brush->width * 3 + x * 3 + 2] / 255.0;
                row[k+0] *= v;
                row[k+1] *= v;
                row[k+2] *= v;
                if (img_has_alpha)
                    arow[(tx + x) * 3] *= v;
            }
            v = (1.0 - h / 255.0) * edgedarken;
            row[k+0] *= v;
            row[k+1] *= v;
            row[k+2] *= v;
            if(img_has_alpha) arow[k] *= v;

            v = h / 255.0;
            row[k+0] += r * v;
            row[k+1] += g * v;
            row[k+2] += b * v;
        }
    }

    if (relief > 0.001)
    {
        for (y = 1; y < brush->height; y++)
        {
            guchar *row = tmp.col + (ty + y) * tmp.width * 3;

            for (x = 1; x < brush->width; x++)
            {
                int k = (tx + x) * 3;
                h = brush->col[y * brush->width * 3 + x * 3 + 1] * relief;
                if (h < 0.001)
                    continue;
                if (h > 255) h = 255;
                row[k+0] = (row[k+0] * (255-h) + 255 * h) / 255;
                row[k+1] = (row[k+1] * (255-h) + 255 * h) / 255;
                row[k+2] = (row[k+2] * (255-h) + 255 * h) / 255;
            }
        }
    }
}

////////////////////
//static std::list<BrushProperty> gBrushProperties;
static std::list<BrushProperty> gMostPreciseBrushPiecelist;
class _BrushPropertyComp
{
public:
	bool operator()(const BrushProperty& left, const BrushProperty& right)
	{
        /*
		float leftGray = left.r * 0.2126f + left.g * 0.7152f + left.b * 0.0722f;
		float rightGray = right.r * 0.2126f + right.g * 0.7152f + right.b * 0.0722f;
		if(leftGray < rightGray)
			return true;
		else
			return false;
         */
	}
};
static void createAlpha(ppm_t* brush, ppm_t* alpha)
{
    int brushrowstride = brush->width * 3;
    int alpharowstride = alpha->width;
    int x, y;
    for(y = 0 ; y < brush->height ; y++)
    {
        guchar* row = brush->col + y * brushrowstride;
        guchar* alpharow = alpha->col + y * alpharowstride;
        for(x = 0 ; x < brush->width ; x++)
        {
            guchar* src = row + x * 3;
            guchar* dst = alpharow + x;
            if(src[0] == 0)
                dst[0] = 0;
            else
                dst[0] = 255;
        }
    }
}
#define ROW_NUM 10
#define COL_NUM 10
struct GeoPaintArea
{
    float left, right, top, bottom;
    int hasPainted;
    std::list<BrushProperty> bp;
    GeoPaintArea()
    {
        left = right = top = bottom = 0;
        hasPainted = 0;
    }
};
struct GeoPaintCanvas
{
    GeoPaintArea pa[ROW_NUM][COL_NUM];
};
struct ImageHueDistribute
{
    float minAngle;
    float maxAngle;
    int num;
};
static int firstHue, secondHue, thirdHue;
static ImageHueDistribute gImageHueDistribute[] = 
{
    {0, 60, 0},
    {60, 120, 0},
    {120, 180, 0},
    {180, 240, 0},
    {240, 300, 0},
    {300, 360, 0}
};
//return the hue angle
static float rgbToHueAngle(guchar *rgb)
{
    float r = rgb[0] / 255.0f;
    float g = rgb[1] / 255.0f;
    float b = rgb[2] / 255.0f;
    //float v = r > g ? r : g;
    float max = std::max(r, std::max(g, b));
    float min = std::min(r, std::min(g, b));
    /*
    if(v < b)
        max = b;
    v = r < g ? r : g;
    float min = v;
    if(v > b)
        v = v;
     */
    float delta = max - min;
    if(max == min)
        return 0;
    float h = 0;
    if(g > b)
    {
        h = (max - r + g - min + b - min) * 60.0f/ delta;
    }
    else 
    {
        h = 360 - (max - r + g - min + b - min) * 60 / delta;    
    }
    if(h < 0)
    {
        h = 360 + h;
    }
    return h;
}
static int getImageHueDistributeIndex(float hAngle)
{
    int count = sizeof(gImageHueDistribute) / sizeof(ImageHueDistribute);
    for(int i = 0 ; i < count ; i++)
    {
        if(gImageHueDistribute[i].minAngle <= hAngle && hAngle <= gImageHueDistribute[i].maxAngle)
            return i;
    }
    return -1;
}
static void clearImageHueDistribute()
{
    int count = sizeof(gImageHueDistribute) / sizeof(ImageHueDistribute);
    for(int i = 0 ; i < count ; i++)
    {
        gImageHueDistribute[i].num = 0;
    }
}

static void findFirstThreeHue(int& first, int& second, int& third)
{
    int count = sizeof(gImageHueDistribute) / sizeof(ImageHueDistribute);
    int max = 0;
    int minNum = INT_MIN;
    std::list<int> hasFindIndex;
    for(int i = 0 ;i < count ; i++)
    {
        std::list<int>::iterator it = find(hasFindIndex.begin(), hasFindIndex.end(), i);
        if(gImageHueDistribute[i].num >  minNum && it == hasFindIndex.end())
        {
            max = i;
            minNum = gImageHueDistribute[i].num;
        }
    }
    hasFindIndex.push_back(max);
    first = max;
    minNum = INT_MIN;
    max = 0;
    for(int i = 0 ;i < count ; i++)
    {
        std::list<int>::iterator it = find(hasFindIndex.begin(), hasFindIndex.end(), i);
        if(gImageHueDistribute[i].num >  minNum && it == hasFindIndex.end())
        {
            max = i;
            minNum = gImageHueDistribute[i].num;
        }
    }
    hasFindIndex.push_back(max);
    second = max;
    max = 0;
    minNum = INT_MIN;
    for(int i = 0 ;i < count ; i++)
    {
        std::list<int>::iterator it = find(hasFindIndex.begin(), hasFindIndex.end(), i);
        if(gImageHueDistribute[i].num >  minNum && it == hasFindIndex.end())
        {
            max = i;
            minNum = gImageHueDistribute[i].num;
        }
    }
    third = max;
    
    /*
    for(int i = 1 ; i < count ; i++)
    {
        if(gImageHueDistribute[max].num < gImageHueDistribute[i].num)
        {
            max = i;
        }
    }
    first = max;
    ImageHueDistribute secondHueDistribute[5];
    int j = 0;
    for(int i = 0 ; i < 6 ; i++)
    {
        if(i != max)
        {
            secondHueDistribute[j++] = gImageHueDistribute[i];
        }
    }
    max = 0;
    for(int i = 1 ; i < 5 ; i++)
    {
        if(secondHueDistribute[max].num < secondHueDistribute[i].num)
            max = i;
    }
    second = max;
    ImageHueDistribute thirdHueDistribute[4];
    j = 0 ; 
    for(int i = 0 ;i < 5 ; i++)
    {
        if(i != max)
        {
            thirdHueDistribute[j++] = secondHueDistribute[i];
        }
    }
    max = 0;
    for(int i = 1 ; i < 4 ; i++)
    {
        if(thirdHueDistribute[max].num < thirdHueDistribute[i].num)
            max = i;
    }
    third = max;
    assert(gImageHueDistribute[first].num >= gImageHueDistribute[second].num && gImageHueDistribute[second].num >= gImageHueDistribute[third].num);
     */
    LOGI("## firstHue = %d, secondHue = %d, thirdHue = %d ##\n", first, second, third);
}
static void createImageHueDistribute(ppm_t* image)
{
    clearImageHueDistribute();
    for(int i = 0 ;i < image->height ; i++)
    {
        guchar* src = image->col + i * image->width * 3;
        for(int j = 0 ; j < image->width ; j++)
        {
            float h = rgbToHueAngle(src + j * 3);
            int index = getImageHueDistributeIndex(h);
            assert(index != -1);
            gImageHueDistribute[index].num++;
        }
    }
}

static QuadTreeNode* createQuadTree(float left, float right, float top, float bottom, int depth)
{
    QuadTreeNode* qtn = new QuadTreeNode;
    qtn->rect = QuadRect(left, right, top, bottom);
    if(depth == QUAD_TREE_DEPTH)
        return qtn;
    QuadRect rects[4];
    float midx = left + (right - left) / 2;
    float midy = top + (bottom - top) / 2;
    
    rects[0].left = left;
    rects[0].right = midx;
    rects[0].top = top;
    rects[0].bottom = midy;
    rects[1].left = left;
    rects[1].right = midx;
    rects[1].top = midy;
    rects[1].bottom = bottom;

    rects[2].left = midx;
    rects[2].right = right;
    rects[2].top = top;
    rects[2].bottom = midy;

    rects[3].left = midx;
    rects[3].right = right;
    rects[3].top = midy;
    rects[3].bottom = bottom;
    for(int i = 0 ; i < 4 ; i++)
    {
        qtn->child[i] = createQuadTree(rects[i].left, rects[i].right, rects[i].top, rects[i].bottom, depth + 1);
    }
    return qtn;
}
static void releaseQuadTree(QuadTreeNode* quadTree)
{
    if(quadTree == NULL)
        return;
    for(int i = 0 ; i < 4 ; i++)
    {
        releaseQuadTree(quadTree->child[i]);
    }
    delete quadTree;
}
static int pointInRect(float x, float y , QuadRect& rect)
{
    if(x >= rect.left && x <= rect.right && y >= rect.top && y <= rect.bottom)
        return 1;
    else
        return 0;
}
static std::list<BrushProperty> outputBrushProperty(QuadTreeNode* quadTree, RepaintData& rd)
{
    if(quadTree == NULL)
    {
        std::list<BrushProperty> bp;
        return bp;
    }
    else 
    {
        std::list<BrushProperty> outList;
        std::list<BrushProperty>::iterator it;
        for(int i = 0 ;i < 4 ; i++)
        {
            std::list<BrushProperty> bp = outputBrushProperty(quadTree->child[i], rd);
            for(it = bp.begin() ; it != bp.end(); it++)
            {
                outList.push_back(*it);
            }
        }
        if(quadTree->child[0] == NULL)
        {
            if(rd.mostConcisePass)
            {
                for(it = quadTree->bpList.begin() ; it != quadTree->bpList.end() ; it++)
                {
                    outList.push_back(*it);
                }
            }
            else
            {
                std::list<BrushProperty> currentBp1;;
                std::list<BrushProperty> currentBp2;
                std::list<BrushProperty> currentBp3;
                std::list<BrushProperty> currentBp4;
                for(it = quadTree->bpList.begin() ; it != quadTree->bpList.end() ; it++)
                {
                    //outList.push_back(*it);
                    guchar rgb[3];
                    rgb[0] = it->p1.r;
                    rgb[1] = it->p1.g;
                    rgb[2] = it->p1.b;
                    float h = rgbToHueAngle(rgb);
                    if(h >= gImageHueDistribute[firstHue].minAngle && h < gImageHueDistribute[firstHue].maxAngle)
                    {
                        currentBp1.push_back(*it);
                    }
                    else if(h >= gImageHueDistribute[secondHue].minAngle && h < gImageHueDistribute[secondHue].maxAngle)
                    {
                        currentBp2.push_back(*it);
                    }
                    else if(h >= gImageHueDistribute[thirdHue].minAngle && h < gImageHueDistribute[thirdHue].maxAngle)
                    {
                        currentBp3.push_back(*it);
                    }
                    else 
                    {
                        currentBp4.push_back(*it);    
                    }
                    
                }
                for(it = currentBp1.begin() ; it != currentBp1.end() ; it++)
                {
                    outList.push_back(*it);
                }
                for(it = currentBp2.begin() ; it != currentBp2.end() ; it++)
                {
                    outList.push_back(*it);
                }
                for(it = currentBp3.begin() ; it != currentBp3.end() ; it++)
                {
                    outList.push_back(*it);
                }
                for(it = currentBp4.begin() ; it != currentBp4.end(); it++)
                {
                    outList.push_back(*it);
                }
            }
        }
        else 
        {
            assert(quadTree->bpList.size() == 0);
        }
        return outList;
    }
}
static void clearQuadTree(QuadTreeNode* quadTree)
{
    if(quadTree == NULL)
    {
        return;
    }
    else
    {
        quadTree->bpList.clear();
        for(int i = 0 ;i < 4 ; i++)
        {
            clearQuadTree(quadTree->child[i]);    
        }
    }
}
/*
 1: add ok
 0: add failed
 */
static int addBrushPropertyToQuadTree(BrushProperty bp, QuadTreeNode* quadTree, BOOL mostConcise)
{
    if(quadTree->child[0] == NULL)
    {
        if(mostConcise)
        {
            if(pointInRect(bp.p2.startx, bp.p2.starty, quadTree->rect))
            {
                quadTree->bpList.push_back(bp);
                return 1;
            }
            else
                return 0;
        }
        else
        {
            if(pointInRect(bp.p1.tx, bp.p1.ty, quadTree->rect))
            {
                quadTree->bpList.push_back(bp);
                return 1;
            }
            else
                return 0;
        }
    }
    else
    {
        if(mostConcise)
        {
            if(pointInRect(bp.p2.startx, bp.p2.starty, quadTree->rect))
            {
                for(int i = 0 ; i < 4 ; i++)
                {
                    int found = addBrushPropertyToQuadTree(bp, quadTree->child[i], mostConcise);
                    if(found)
                        return 1;
                }
                assert(0);
            }
            else
                return 0;
        }
        else
        {
            if(pointInRect(bp.p1.tx, bp.p1.ty, quadTree->rect))
            {
                for(int i = 0 ; i < 4 ; i++)
                {
                    int found = addBrushPropertyToQuadTree(bp, quadTree->child[i], mostConcise);
                    if(found)
                        return 1;
                }
                assert(0);
            }
            else
                return 0;
        }
    };
}

static void clearGrayPaintArea()
{
    for(int i = 0 ; i < GRAY_NUM ; i++)
    {
        grayPaintArray[i].bp.clear();
    }
}
static void addBrushPropertyToGrayPaintArea(BrushProperty bp, bool mostConsise, RepaintData& rd)
{
    if(mostConsise == false)
    {
        int i;
        float gray = gray = bp.p1.r * 0.2126f + bp.p1.g * 0.7152f + bp.p1.b * 0.0722f;
        for(i = 0 ; i < GRAY_NUM ; i++)
        {
            if(gray >= gGraySpanVector[i].start && gray <= gGraySpanVector[i].end)
            {
                grayPaintArray[i].bp.push_back(bp);
                break;
            }
        }
        if(i == GRAY_NUM)
            NSLog(@"bp gray error");
    }
    else
    {
        /*
        int everyGrayNum = rd.allBrushCount / GRAY_NUM;
        if(rd.currentGrayBrushCount < everyGrayNum)
        {
            rd.currentGrayBrushCount++;
        }
        else
        {
            rd.currentGrayIndex++;
            rd.currentGrayBrushCount = 0;
            if(rd.currentGrayIndex >= GRAY_NUM)
            {
                rd.currentGrayIndex = GRAY_NUM - 1;
                rd.currentGrayBrushCount = 0;
            }
        }
         */
        rd.currentGrayIndex += 1;
        if(rd.currentGrayIndex >= GRAY_NUM) 
        {
            rd.currentGrayIndex = 0;
            
        }
        grayPaintArray[rd.currentGrayIndex].bp.push_back(bp);
    }
    /*
    float graySpan = 256.0f / GRAY_NUM;
    if(gray >= 0 && gray < graySpan)
    {
        grayPaintArray[0].bp.push_back(bp);
    }
    else if(gray >= graySpan && gray < 2 * graySpan)
    {
        grayPaintArray[1].bp.push_back(bp);
    }
    else if(gray >= 2 * graySpan && gray < 3 * graySpan)
    {
        grayPaintArray[2].bp.push_back(bp);
    }
    else if(gray >= 3 * graySpan && gray < 4 * graySpan)
    {
        grayPaintArray[3].bp.push_back(bp);
    }
    else {
        grayPaintArray[4].bp.push_back(bp);
    }
     */
}

static bool brushInBrushDrawingList(std::list<ppm_t>& drawingBrushList, ppm_t* brush)
{
    std::list<ppm_t>::iterator it;
    for(it = drawingBrushList.begin(); it != drawingBrushList.end() ; it++)
    {
        if(it->col == brush->col)
            return true;
    }
    return false;
}
static void outputBrushPiece(std::list<BrushProperty>& brushProperties, int maxbrushwidth, int maxbrushheight, float destWidth, float destHeight, float gImageWidth, float gImageHeight, ppm_t tmp, int drawing_speed, SS_AtomicCounter* currentStatusPoint, RepaintData rd, DrawingBrushList& drawingBrushList)
{
    int drawing_index = 0;
    SS_BrushList* brushList = NULL;
    //if(gBrushProperties.size() > 0)
    if(brushProperties.size() > 0)
        brushList = SS_BrushListCreate();
    std::list<BrushProperty>::iterator it;
    SS_BrushListPool* brushListPool = SS_GetBrushListPool();
    for(it = brushProperties.begin(); it != brushProperties.end() && SS_GetAtomicCounterValue(currentStatusPoint); it++)
    {
        BrushProperty bp = *it;
        float startRealPicX = maxbrushwidth;
        float startRealPicY = maxbrushheight;
        float bpx = (float)bp.p1.tx - startRealPicX;
        float bpy = (float)bp.p1.ty - startRealPicY;
        float bpright = bpx + (float)bp.p1.brush->width;
        float bpbottom = bpy + (float)bp.p1.brush->height;
        float newtx = bpx * destWidth / gImageWidth;
        float newty = bpy * destHeight / gImageHeight;
        float newright = bpright * destWidth / gImageWidth;
        float newbottom = bpbottom * destHeight / gImageHeight;
        int neww = newright - newtx;
        int newh = newbottom - newty;

        BrushPiece brushPiece;
        //brushPiece.data.col = NULL;
        brushPiece.p1.destData.col = NULL;
        brushPiece.precise = false;
        brushPiece.p1.x = newtx;
        brushPiece.p1.y = newty;
        //brushPiece.w = tmp.width;
        //brushPiece.h = tmp.height;
        //brushPiece.mbw = maxbrushwidth;
        //brushPiece.mbh = maxbrushheight;
        brushPiece.p1.r = bp.p1.r;
        brushPiece.p1.g = bp.p1.g;
        brushPiece.p1.b = bp.p1.b;
        //NSLog(@"## out list bp = %d, %d, %d", brushPiece.r, brushPiece.g, brushPiece.b);
        brushPiece.last_piece = 0;
        brushPiece.p1.destData = *bp.p1.destBrush;
        SS_AddBrushPiece(brushList, brushPiece);
        SS_AddBrush(brushListPool, brushPiece.p1.destData, rd.pass);
        if(brushInBrushDrawingList(drawingBrushList, &brushPiece.p1.destData) == false)
        {
            drawingBrushList.push_back(brushPiece.p1.destData);
        }
        if(drawing_index == (drawing_speed - 1))
        {
            SS_AddBrushList(brushListPool, brushList);
            drawing_index = 0;
            brushList = SS_BrushListCreate();
        }
        else
        {
            drawing_index++;
        }
        
    }
    if(brushList && SS_GetAtomicCounterValue(currentStatusPoint))
    {
        SS_AddBrushList(brushListPool, brushList);
    }
    else
    {
        if(brushList)
        {
            SS_BrushListRelease(brushList);
        }
        
    }
    //brushProperties.clear();
    /*
    if(!isBrushPaint())
    {
        clearBrushPiece(); 
    }
     */
}

static void separatePaintArea(int maxbrushwidth, int maxbrushheight, float destWidth, float destHeight, float gImageWidth, float gImageHeight, ppm_t tmp, int drawing_speed, RepaintData rd, SS_AtomicCounter* currentStatusPoint, DrawingBrushList& drawingBrushList)
{
    //gBrushProperties.clear();
    for(int i = 0 ; i < GRAY_NUM && SS_GetAtomicCounterValue(currentStatusPoint); i++)
    {
        std::list<BrushProperty>::iterator it;
        for(it = grayPaintArray[i].bp.begin() ; it != grayPaintArray[i].bp.end() && SS_GetAtomicCounterValue(currentStatusPoint); it++)
        {
            addBrushPropertyToQuadTree(*it, rootQuadTree, rd.mostConcisePass);
        }
        grayPaintArray[i].bp.clear();
        std::list<BrushProperty> outList = outputBrushProperty(rootQuadTree, rd);
        clearQuadTree(rootQuadTree);
        NSLog(@"### outList = %ld ###", outList.size());
        //SS_SetSecondNum(outList.size());
        if(rd.mostConcisePass)
        {
            int drawing_speed = 500;
            int count = 0;
            SS_BrushList* brushList = SS_BrushListCreate(true);
            SS_BrushListPool* brushListPool = SS_GetBrushListPool();
            std::list<BrushProperty>::iterator it;
            rd.brushNum = outList.size();
            for(it = outList.begin(); 
                it != outList.end() && SS_GetAtomicCounterValue(currentStatusPoint);
                it++)
                //for(it = gBrushProperties.begin() ; it != gBrushProperties.end() ; it++)
            {
                BrushProperty bpp = *it;
                BrushPiece bp;
                bp.precise = true;
                bp.last_piece = 0;
                bp.p2.startx = bpp.p2.startx - maxbrushwidth / 2;
                bp.p2.starty = bpp.p2.starty - maxbrushheight / 2;
                bp.p2.endx = bp.p2.startx + maxbrushwidth;
                bp.p2.endy = bp.p2.starty + maxbrushheight;
                /*
                if(bp.p2.startx > 768)
                {
                    LOGI("startx = %d, starty = %d", bp.p2.startx, bp.p2.starty);
                }
                 */
                SS_AddBrushPiece(brushList, bp);
                count++;
                if(count == drawing_speed)
                {
                    //NSLog(@"add brush list");
                    count = 0;
                    SS_AddBrushList(brushListPool, brushList);
                    brushList = SS_BrushListCreate(true);
                }
                /*
                rd.brushNum--;
                if(rd.brushNum % 10000 == 0)
                {
                    SS_SetSecondNum(rd.brushNum);
                }
                 */
            }
            if(brushList && SS_GetAtomicCounterValue(currentStatusPoint))
            {
                SS_AddBrushList(brushListPool, brushList);
            }
        }
        else
        {
            outputBrushPiece(outList, maxbrushwidth, maxbrushheight, destWidth, destHeight, gImageWidth, gImageHeight, tmp, drawing_speed, currentStatusPoint, rd, drawingBrushList);
        }
        outList.clear();
    }
    for(int i = 0 ; i < GRAY_NUM ; i++)
    {
        grayPaintArray[i].bp.clear();
    }
}

void testQuadTree()
{
    rootQuadTree = createQuadTree(0, 1024, 0, 768, 0);
    releaseQuadTree(rootQuadTree);
}
////////////////////////
//#define BRUSH_NUM 3
static MyBrushData getBrushDataByAngle(double angle, std::list<MyBrushData>& angleList)
{
    for(std::list<MyBrushData>::iterator it = angleList.begin();
        it != angleList.end();
        it++)
    {
        if(fabs(it->angle - angle) < 1)
            return *it;
    }
    assert(0);
    ppm_t p = {0, 0, NULL};
    return MyBrushData(p, 0);
}
static MyBrushData getBrushDataByAngle(double angle, std::vector<MyBrushData>& angleList)
{
    /*
    for(std::list<MyBrushData>::iterator it = angleList.begin();
        it != angleList.end();
        it++)
    {
        if(fabs(it->angle - angle) < 1)
            return *it;
    }
    assert(0);
    ppm_t p = {0, 0, NULL};
    return MyBrushData(p, 0);
    */
    int index = (int)angle;
    return angleList[index];
}
static ppm_t* getBrushByAngleForList(double angle, std::list<MyBrushData>& angleList)
{
    for(std::list<MyBrushData>::iterator it = angleList.begin();
        it != angleList.end();
        it++)
    {
        if(fabs(it->angle - angle) < 1)
            return &it->brush;
    }
    return NULL;
}
static ppm_t* getBrushByAngle(double angle, std::vector<MyBrushData>& angleList)
{
    /*
    for(std::list<MyBrushData>::iterator it = angleList.begin();
        it != angleList.end();
        it++)
    {
        if(fabs(it->angle - angle) < 1)
            return &it->brush;
    }
     */
    int index = (int)angle;
    return &angleList[index].brush;
    //return NULL;
}
static ppm_t* getRawBrushByAngleForList(double angle, std::list<MyBrushData>& angleList)
{
    for(std::list<MyBrushData>::iterator it = angleList.begin();
        it != angleList.end();
        it++)
    {
        if(fabs(it->angle - angle) < 1)
            return &it->rawBrush;
    }
    return NULL;    
}
static ppm_t* getRawBrushByAngle(double angle, std::vector<MyBrushData>& angleList)
{
    /*
    for(std::list<MyBrushData>::iterator it = angleList.begin();
        it != angleList.end();
        it++)
    {
        if(fabs(it->angle - angle) < 1)
            return &it->rawBrush;
    }
    return NULL;
     */
    int index = (int)angle;
    return &angleList[index].rawBrush;
}



static void createBrushMap(int brushWidth, int brushHeight, double angle, BrushMapData* bmd)
{
    double m = 0;
    if(angle != 90 && angle != -90 && angle != 270)
        m = tan(angle * 3.1415926 / 180);
    bmd->data.clear();
    bmd->data.resize(brushHeight);
    for(int i = 0 ; i < brushHeight ; i++)
    {
        bmd->data[i].resize(brushWidth);
    }
    for(int y = 0 ; y < brushHeight ; y++)
    {
        for(int x = 0 ; x < brushWidth ; x++)
        {
            POINT_TYPE pType = pointInPosition(brushWidth, brushHeight, x, y, m, angle);
            bmd->data[y][x] = pType;
        }
    }
}
static void createAllBrushMap(int brushWidth, int brushHeight, BrushMapDataMap& brushDataMap)
{
    for(int angle = -360 ; angle <= 360 ; angle++)
    {
        int index = angle + 360;
        createBrushMap(brushWidth, brushHeight, angle, &brushDataMap.data[index]);
    }
}
static bool brushPointCompare(const BrushBlock& first, const BrushBlock& second)
{
    if(first.ty < second.ty)
        return true;
    if(first.ty > second.ty)
        return false;
    if(first.tx < second.tx)
        return true;
    else 
    {
        return false;
    }
}
static void createBrushes(ppm_t* p, int pass, std::vector<MyBrushData>& brushDataList, std::list<MyBrushData>& destBrushDataList, ppm_t* newDestBrushes, int destWidth, int gImageWidth)
{
    BrushBlockMatrix& bbm = gBrushBlockMatrixVector[pass];
    BrushPassData bpd = gBrushPassData[pass];
    int size = bbm.size();
    for(int i = 0 ; i < size; i++)
    {
        BrushBlockRow& row = bbm[i];
        for(int j = 0 ; j < row.size() ; j++)
        {
            BrushBlock bb = row[j];
            if(bb.valid == false)
                continue;
            int tx = bb.tx;
            int ty = bb.ty;
            if ((tx < bpd.brushWidth / 2)             ||
                (ty < bpd.brushHeight / 2)             ||
                (tx + bpd.brushWidth / 2 >= p->width) ||
                (ty + bpd.brushHeight / 2 >= p->height))
            {
#if 0
                LOGI("Internal Error; invalid coords: (%d,%d) i=%d\n", tx, ty, i);
#endif
                continue;
            }
            int angle = bb.angle;
            ppm_t* brush = NULL;
            ppm_t* rawBrush = NULL;
            brush = getBrushByAngle(angle, brushDataList);
            rawBrush = getRawBrushByAngle(angle, brushDataList);
            MyBrushData bd = getBrushDataByAngle(angle, brushDataList);
            int thissum = bd.sum;//sum_brush(brush);
            int x, y;
            double r, g, b;
            int h;
            if (runningvals.color_type == 0)
            {
                r = g = b = 0;
                for (y = 0; y < brush->height; y ++)
                {
                    guchar *row = &p->col[(ty + y) * p->width * 3];
                    
                    for (x = 0; x < brush->width; x ++)
                    {
                        int k = (tx + x) * 3;
                        double v;
                        
                        if ((h = brush->col[y * brush->width * 3 + x * 3]))
                        {
                            v = h / 255.0;
                            r += row[k+0] * v;
                            g += row[k+1] * v;
                            b += row[k+2] * v;
                            
                        }
                    }
                }
                //assert( thissum > 0);
                if(thissum != 0)
                {
                    //if(rd.pass == 0)
                    {
                        r = r * 255.0 / thissum;
                        g = g * 255.0 / thissum;
                        b = b * 255.0 / thissum;
                    }
                    /*
                     else
                     {
                     r = 255;
                     g = 0;
                     b = 0;
                     }
                     */
                }
                else
                {
                    y = ty + (brush->height / 2);
                    x = tx + (brush->width / 2);
                    guchar* pixel = &p->col[y*p->width * 3 + x * 3];
                    r = pixel[0];
                    g = pixel[1];
                    b = pixel[2];
                }
            }
            else if (runningvals.color_type == 1)
            {
                guchar *pixel;
                
                y = ty + (brush->height / 2);
                x = tx + (brush->width / 2);
                pixel = &p->col[y*p->width * 3 + x * 3];
                guchar* brushPixel = &brush->col[brush->width * 3 * brush->height / 2 + 3 * brush->width / 2];
                if(brushPixel[0] != 0)
                {
                    double v = brushPixel[0] / 255.0;
                    r = pixel[0] * v;
                    g = pixel[1] * v;
                    b = pixel[2] * v;
                }
                else
                {
                    r = pixel[0];
                    g = pixel[1];
                    b = pixel[2];
                }
            }
            else
            {
                /* No such color_type! */
                r = g = b = 0;
            }
            BrushProperty bp;
            bp.p1.brush = brush;
            //SETimePrinter tp3;
            ppm_t* currentDestBrush = getBrushByAngleForList(angle, destBrushDataList);
            if(currentDestBrush == NULL)
            {
                float times = ((float)destWidth) / gImageWidth;
                //float adjust = g_rand_double_range(random_generator, 1, runningvals.size_last / runningvals.size_first);
                //NSLog(@"adjust = %f", adjust);
                float tmpWidth = rawBrush->width;//brush->width;
                float tmpHeight = rawBrush->height;//brush->height;
                //float tmpWidth = maxbrushwidth;
                //float tmpHeight = maxbrushheight;
                //NSLog(@"tmpWidth = %f, tmpHeight = %f", tmpWidth, tmpHeight);
                //double first , last;
                ppm_t tmpDestBrush = {0, 0, NULL};
                tmpDestBrush = free_rotate_return(&newDestBrushes[0], angle);
                //SS_SaveBrush("lastBrush", 0, tmpDestBrush);
                //autocrop(&tmpDestBrush, 1);
                resize(&tmpDestBrush, tmpWidth * times, tmpHeight * times);
                prepare_brush(&tmpDestBrush);
                MyBrushData mbd(tmpDestBrush, angle);
                destBrushDataList.push_back(mbd);
                currentDestBrush = getBrushByAngleForList(angle, destBrushDataList);
                /*
                 static bool save = false;
                 if(!save)
                 {
                 save = true;
                 SS_SaveBrush("mybrush", 0, newDestBrushes[0]);
                 }
                 SS_SaveBrush("brush", destBrushDataList.size(), tmpDestBrush);
                 */
            }
            bp.p1.destBrush = currentDestBrush;
            bp.p1.b = b;
            bp.p1.g = g;
            bp.p1.r = r;
            //bp.p1.shadow = NULL;
            bp.p1.tx = tx;
            bp.p1.ty = ty;
            //gBrushProperties.push_back(bp);
            //addBrushPropertyToGrayPaintArea(bp, false, rd);
            //end
        }
    }
    
}

static void createMipmap(ppm_t srcImage, std::list<MyBrushData>& brushDataList)
{
    int width = srcImage.width;
    int height = srcImage.height;
    int minW = width > height ? height : width;
    while (minW > 10) 
    {
        if(width != srcImage.width || height != srcImage.height)
        {
            ppm_t newImage = resize_return(&srcImage, width, height);
            MyBrushData md;
            md.brush = newImage;
            md.startx = 0;
            md.starty = 0;
            md.endx = newImage.width;
            md.endy = newImage.height;
            brushDataList.push_back(md);
        }
        else
        {
            ppm_t newImage = {0, 0, NULL};
            ppm_copy(&srcImage, &newImage);
            MyBrushData md;
            md.brush = newImage;
            md.startx = 0;
            md.starty = 0;
            md.endx = newImage.width;
            md.endy = newImage.height;
            brushDataList.push_back(md);
        }
        width /= 2;
        height /= 2;
        minW = width > height ? height : width;
    }
}
static void padBrushes(ppm_t* brush)
{
    guchar      back[3] = {0, 0, 0};
    autocrop(brush, 1);
    float width = brush->width;
    float height = brush->height;
    float brushLen = 1 + sqrtf(width  * width / 4 + height * height / 4);
    float left = brushLen - width / 2 ;
    float right = brushLen - width / 2;
    float top = brushLen - height / 2;
    float bottom = brushLen - height / 2;
    ppm_pad(brush, left, right, top, bottom, back);
}
static ppm_t getBestBrush(std::list<MyBrushData>& brushList, float dstWidth, float dstHeight)
{
    std::list<MyBrushData>::iterator it;
    std::list<MyBrushData>::iterator outIt = brushList.end();
    std::list<MyBrushData>::iterator firstIt = brushList.begin();
    std::list<MyBrushData>::iterator lastIt = brushList.end();
    lastIt--;
    float firstWidth = firstIt->endx - firstIt->startx;
    float lastWidth = lastIt->endx - lastIt->startx;
    if(dstWidth >= firstWidth)
    {
        return firstIt->brush;
    }
    if(dstWidth <= lastWidth)
    {
        return lastIt->brush;
    }
    for(it = brushList.begin(); it != brushList.end() ; it++)
    {
        float width = it->endx - it->startx;
        if(dstWidth > width)
        {
            outIt = it;
            outIt--;
            break;
        }
    }
    assert(outIt != brushList.end());
    return outIt->brush;
}
static void doubleBrushValue(ppm_t* brush, float t)
{
    if(t < 1)
        t = 1;
    //int sum = 0;
    for(int i = 0 ; i < brush->height ; i++)
    {
        for(int j = 0 ; j < brush->width ; j++)
        {
            float r = brush->col[i * brush->width * 3 + j * 3];
            float g = brush->col[i * brush->width * 3 + j * 3 + 1];
            float b = brush->col[i * brush->width * 3 + j * 3 + 2];
            r *= t;
            g *= t;
            b *= t;
            //LOGI("r = %d, g = %d, b = %d", r, g, b);
            if(r > 255)
                r = 255;
            if(g > 255)
                g = 255;
            if(b > 255)
                b = 255;
            brush->col[i * brush->width * 3 + j * 3] = r ;
            brush->col[i * brush->width * 3 + j * 3 + 1] = r;
            brush->col[i * brush->width * 3 + j * 3 + 2] = r;
            //sum += r + g + b;
        }
    }
}
void repaint3 (ppm_t *p, ppm_t *a, RepaintData rd)
{
    int         x, y;
    int         tx = 0, ty = 0;
    ppm_t       tmp = {0, 0, NULL};
    ppm_t       atmp = {0, 0, NULL};
    int         n, h, i, j, on, sn;
    double      r,g, b;
    int         num_brushes, maxbrushwidth, maxbrushheight;
    guchar      back[3] = {0, 0, 0};
    ppm_t      *brushes, *shadows;
    ppm_t      *destBrushes;
    ppm_t      *brush, *shadow = NULL;
    double     *brushes_sum;
    int         cx, cy, maxdist;
    double      scale, relief, startangle, anglespan, density, bgamma;
    double*      scales;//[BRUSH_NUM];
    double*     destAngles;
    int*     destBrushIndex;
    double      thissum;
    ppm_t       paper_ppm = {0, 0, NULL};
    ppm_t       dirmap = {0, 0, NULL};
    ppm_t       sizmap = {0, 0, NULL};
    int        *xpos = NULL, *ypos = NULL;
    int         step = 1;
    int destWidth ;
    int destHeight;
    int brushIndex;
    SE_BrushSet brushSet;
    std::vector<BrushPassData> brushPassDataList;
    std::list<ppm_t> destBrushesForDrawing;
    SS_PausePoint* currentPausePoint = NULL;
    SS_AtomicCounter* currentStatusPoint = NULL;
    SS_Canvas* currentCanvas = NULL;
    static int  running = 0;
    ppm_t edgeDetectionMap = {0, 0, NULL};
    //std::list<MyBrushData> brushDataList;
    std::vector<MyBrushData> brushDataList;
    std::vector<MyBrushData> rawBrushDataList;
    //std::vector<MyBrushData> mipmapBrushList;
    std::list<MyBrushData> destBrushDataList;
    std::vector<BrushBlock> brushPointVector;
    std::vector<std::list<MyBrushData> > mipmapBrushes;
    std::vector<ppm_t> blurBrushes;
    int currentStatusPointValue = 1;
    //std::list<MyBrushData> rawBrushDataList;
    BrushMapDataMap brushMapDataMap;
    std::vector<MyBrushData>::iterator tmpRawBrushIt;
    int dropshadow = pcvals.general_drop_shadow;
    int shadowblur = pcvals.general_shadow_blur;
    int ALL_ANGLE = 360;
    //SETimePrinter brushRotateTimer;
    int edgeBrushNum = 0;
    int maxFixBrushWidth = 0;
    int maxFixBrushHeight = 0;
    bool sendByNum = false;
    int allCount = 0;
    BOOL bSetStage2 = NO;
    int brushSizeComp = rd.brushSizeComp;
    g_printerr("####running = %d ###\n", running);
    if (running)
        return;
    LOGI("### edge detect start = %d ##\n", rd.edgeDetectionStart);
    
    int propertySize = sizeof(BrushProperty);
    LOGI("property size = %d\n", propertySize);
    /*
    if(rd.pass > 6)
    {
        rd.calculateOnEdge = true;
    }
    */
    //SS_ShowLoadingView();
    //rd.calculateOnEdge = false;
    currentPausePoint = SS_GetCurrentPausePoint();
    currentStatusPoint = SS_GetCurrentStatusPoint();
    currentStatusPointValue = SS_GetAtomicCounterValue(currentStatusPoint);
    if(currentStatusPointValue == 0)
        return;
    if(rootQuadTree == NULL)
    {
        rootQuadTree = createQuadTree(0, 1024, 0, 768, 0);
    }
    createImageHueDistribute(p);
    findFirstThreeHue(firstHue, secondHue, thirdHue);
    
    currentCanvas = SS_GetCurrentCanvas();
    running++;
    SS_GetDestSize(&destWidth, &destHeight);
    SS_GetSettingBrush(&brushSet);
    int BRUSH_NUM = brushSet.brush.size();
    scales = new double[BRUSH_NUM];
    ppm_t* newBrushes = (ppm_t*)g_malloc(sizeof(ppm_t) * BRUSH_NUM);
    memset(newBrushes, 0, sizeof(ppm_t) * BRUSH_NUM);
    ppm_t* newDestBrushes = (ppm_t*)g_malloc(sizeof(ppm_t) * BRUSH_NUM);
    memset(newDestBrushes, 0, sizeof(ppm_t) * BRUSH_NUM);
    double* newBrushSum = (double*)g_malloc(sizeof(double) * BRUSH_NUM);
    memset(newBrushSum, 0, sizeof(double) * BRUSH_NUM);
    blurBrushes.resize(BRUSH_NUM);
    runningvals = pcvals;
    //print_val(&runningvals);
    gImageWidth = p->width;
    gImageHeight = p->height;
    for(int k = 0  ; k < blurBrushes.size() ; k++)
    {
        blurBrushes[k].width = 0;
        blurBrushes[k].height = 0;
        blurBrushes[k].col = NULL;
    }
    LOGI("## gImageWidth = %d, gImageHeight = %d ##\n", gImageWidth, gImageHeight);
    //SS_AddLog("## gImageWidth = %d, gImageHeight = %d ##\n", gImageWidth, gImageHeight);
    if(rd.calculateOnEdge)
    {
        startTime();
        LOGI("### edge detect start = %d, end = %d ####\n", rd.edgeDetectionStart, rd.edgeDetectionEnd);
        edgeDetectionMap = edgeDetection(p, rd.edgeDetectionStart, rd.edgeDetectionEnd);
        endTime();
        LOGI("## create edge detection time : %f ##\n", gRunningTime);
        //SS_AddLog("## create edge detection time : %f ##\n", gRunningTime);
        NSLog(@"edge detection width = %d, height = %d", edgeDetectionMap.width, edgeDetectionMap.height);
        //SS_SaveBrush("edgedetctionMap", rd.pass, edgeDetectionMap);
    }
    if(gGraySpanVector.size() == 0)
    {
        createGraySpan(p);
    }
    num_brushes = runningvals.orient_num * runningvals.size_num;
    startangle = runningvals.orient_first;
    anglespan = runningvals.orient_last;//runningvals.orient_last - runningvals.orient_first;//runningvals.orient_last;
    
    density = runningvals.brush_density;
    NSLog(@"#### density = %f ###", density);
    if (runningvals.place_type == PLACEMENT_TYPE_EVEN_DIST)
    {
        density /= 3.0;
        if(density < 1)
            density = 1;
    }
    else
    {
        //for debg
        density /= 2.0;
        if(density < 1)
            density = 1;
        //end
    }
    bgamma = runningvals.brushgamma;
    
    brushes = (ppm_t*)g_malloc (num_brushes * sizeof (ppm_t));
    destBrushes = (ppm_t*)g_malloc(num_brushes * sizeof(ppm_t));
    brushes_sum = (double*)g_malloc (num_brushes * sizeof (double));
    destAngles = (double*)g_malloc(num_brushes *sizeof(double));
    destBrushIndex = (int*)g_malloc(num_brushes * sizeof(int));
    memset(brushes, 0, sizeof(ppm_t) * num_brushes);
    memset(destBrushes, 0, sizeof(ppm_t) * num_brushes);
    memset(brushes_sum, 0, sizeof(double) * num_brushes);
    memset(destAngles, 0, sizeof(double) * num_brushes);
    memset(destBrushIndex, 0, sizeof(int) * num_brushes);
    mipmapBrushes.resize(BRUSH_NUM);
    for(int i = 0 ; i < num_brushes ; i++)
    {
        destBrushes[i].width = 0;
        destBrushes[i].height = 0;
        destBrushes[i].col = NULL;
    }
    /*
    for(int angle = 0 ; angle <= 360 ; angle++)
    {
        double radian = angle * 3.1415926 / 180;
        gCosTable[angle] = cos(radian);
        gSinTable[angle] = sin(radian);
    }
     */
    if (dropshadow)
        shadows = (ppm_t*)g_malloc (num_brushes * sizeof (ppm_t));
    else
        shadows = NULL;
    startTime();
    //float ratio = 0;
    float brushWidth = 0;
    float brushHeight = 0;
    rawBrushDataList.resize(BRUSH_NUM);
    // read brush from file and set to rawBrushList, brushes, destBrushes
    for(brushIndex = 0 ; brushIndex < BRUSH_NUM && SS_GetAtomicCounterValue(currentStatusPoint); brushIndex++)
    {
        brushes[brushIndex].col = NULL;
        destBrushes[brushIndex].col = NULL;
        int k = brushIndex;//g_rand_int_range(random_generator, 0, BRUSH_NUM);
        brush_get(brushSet.brush[k].c_str(), &brushes[brushIndex]);
        //SS_SaveBrush("brush", brushIndex, brushes[brushIndex]);
        ppm_t newBrush = {0, 0, NULL};
        if(rd.blurRadiusH > 0 && rd.blurRadiusV > 0)
        {
            int paddx = rd.blurRadiusH * 1;
            int paddy = rd.blurRadiusV * 1;
            guchar tmpBg[] = {0, 0, 0};
            ppm_t origBrush = ppm_pad_return(&brushes[brushIndex], paddx, paddx, paddy, paddy, tmpBg);
            //ppm_pad(&brushes[brushIndex], paddx, paddx, paddy, paddy, tmpBg);
            newBrush = [SEUtil blurBrush:origBrush horiz:rd.blurRadiusH vert:rd.blurRadiusV];
            int valueForIndex = (brushIndex + 100) * rd.pass;
            //SS_SaveBrush("startblurBrush", valueForIndex, newBrush);
            ppm_kill(&brushes[brushIndex]);
            LOGI("rd pass = %d, times = %d, ratio = %f", rd.pass, rd.totalTimes, rd.blurRatio);
            float ratio = rd.pass / (float) (rd.totalTimes - 1);
            float multiplyValue = 1 + (2.5 - 1) * rd.blurRatio;
            doubleBrushValue(&newBrush, multiplyValue * (1 - ratio));
            autocrop(&newBrush, 0);
            brushes[brushIndex].width = 0;
            brushes[brushIndex].height = 0;
            brushes[brushIndex].col = NULL;
            ppm_copy(&newBrush , &brushes[brushIndex]);
            blurBrushes[brushIndex] = newBrush;
            ppm_kill(&origBrush);
        }
        if(rd.blurRadiusH > 0 && rd.blurRadiusV > 0)
        {
            ppm_copy(&blurBrushes[brushIndex], &destBrushes[brushIndex]);
        }
        else
        {
            ppm_copy(&brushes[brushIndex], &destBrushes[brushIndex]);
        }
        
        ppm_t tmp = {0, 0 , NULL};
        ppm_copy(&brushes[brushIndex], &tmp);
        MyBrushData md(tmp, 0);
        rawBrushDataList[brushIndex] = md;
        if(rd.blurRadiusH > 0 && rd.blurRadiusV > 0)
        {
            createMipmap(blurBrushes[brushIndex], mipmapBrushes[brushIndex]);
        }
        else
        {
            createMipmap(brushes[brushIndex], mipmapBrushes[brushIndex]);
        }
        
        NSLog(@"raw brush width = %d, height = %d", brushes[brushIndex].width, brushes[brushIndex].height);
        //SS_SaveBrush("rawbrush", brushIndex, destBrushes[brushIndex]);
    }
    for(int i = 0 ; i < blurBrushes.size() ; i++)
    {
        ppm_t p = blurBrushes[i];
        ppm_kill(&p);
    }
    endTime();
    currentStatusPointValue = SS_GetAtomicCounterValue(currentStatusPoint);
    if(currentStatusPointValue == 0)
        goto endlabel;

    LOGI("## brush load time = %f ##\n", gRunningTime);
    //scale = runningvals.size_first / std::max (brushes[0].width, brushes[0].height);
    for(brushIndex = 0 ; brushIndex < BRUSH_NUM && SS_GetAtomicCounterValue(currentStatusPoint) ; brushIndex++)
    {
        scales[brushIndex] = runningvals.size_last / std::max (brushes[brushIndex].width, brushes[brushIndex].height);
    }
    if (bgamma != 1.0)
        ppm_apply_gamma (&brushes[0], 1.0 / bgamma, 1,1,1);
    startTime();
    //resize brush to the size expected
    for(brushIndex = 0 ; brushIndex < BRUSH_NUM && SS_GetAtomicCounterValue(currentStatusPoint); brushIndex++)
    {
        if(brushes[brushIndex].height > 5)
        {
            resize (&brushes[brushIndex], ceil(brushes[brushIndex].width * scales[brushIndex]), ceil(brushes[brushIndex].height * scales[brushIndex]));
        }
        if(brushIndex == 0)
        {
            brushWidth = brushes[brushIndex].width;
            brushHeight = brushes[brushIndex].height;
            NSLog(@"brush0 width = %f, height = %f", brushWidth, brushHeight);
        }
        NSLog(@"resize brush width = %d, height = %d", brushes[brushIndex].width, brushes[brushIndex].height);
    }
    currentStatusPointValue = SS_GetAtomicCounterValue(currentStatusPoint);
    if(currentStatusPointValue == 0)
        goto endlabel;
    // resize rawBrushDataList and padding to make width and height be same
    for(brushIndex = 0 , tmpRawBrushIt = rawBrushDataList.begin(); brushIndex < BRUSH_NUM && tmpRawBrushIt != rawBrushDataList.end() && SS_GetAtomicCounterValue(currentStatusPoint); brushIndex++, tmpRawBrushIt++)
    {
        //ppm_t* tmpbrush = &rawBrushDataList[brushIndex].brush;
        ppm_t* tmpbrush = &tmpRawBrushIt->brush;
        if(tmpbrush->width > 5)
        {
            double tmpScale = runningvals.size_last / std::max(tmpbrush->width, tmpbrush->height);
            resize(tmpbrush, ceil(tmpbrush->width * tmpScale), ceil(tmpbrush->height * tmpScale));
        }
        autocrop(tmpbrush, 1);
        float brushLen = 1 + sqrtf(tmpbrush->width  * tmpbrush->width / 4 + tmpbrush->height * tmpbrush->height / 4);
        float left = brushLen - tmpbrush->width / 2 ;
        float right = brushLen - tmpbrush->width / 2;
        float top = brushLen - tmpbrush->height / 2;
        float bottom = brushLen - tmpbrush->height / 2;
        //ppm_pad(tmpbrush, brushLen - tmpbrush->width, brushLen - tmpbrush->width, brushLen - tmpbrush->height, brushLen - tmpbrush->height, back);
        ppm_pad(tmpbrush, left, right, top, bottom, back);
        NSLog(@"raw brush width = %d, height = %d", tmpbrush->width, tmpbrush->height);
        brushWidth = tmpbrush->width;
        brushHeight = tmpbrush->height;
        //calculateBrushBound(tmpbrush, tmpRawBrushIt->startx, tmpRawBrushIt->starty, tmpRawBrushIt->endx, tmpRawBrushIt->endy);
        /*
        int y = tmpbrush->height / 2;
        
        for(int xx = 0 ; xx < tmpbrush->width ; xx++)
        {
            guchar* data = tmpbrush->col +  y * tmpbrush->width * 3 + xx * 3;
            data[0] = 255;
            data[1] = 0;
            data[2] = 0;
        }
         */
        
        //SS_SaveBrush("newRawBrush", brushIndex, *tmpbrush);
        //setBrushBoundColor(tmpbrush);
        //SS_SaveBrush("newRawMidBrushRed", brushIndex, *tmpbrush);
        //ppm_t myTmp = calculateRotateBrush(tmpbrush, 90);
        //SS_SaveBrush("newRawMidBrush", brushIndex, myTmp);
        //myTmp = calculateRotateBrush(tmpbrush, 180);
        //SS_SaveBrush("newRawMidBrush180", brushIndex , myTmp);
        
    }
    currentStatusPointValue = SS_GetAtomicCounterValue(currentStatusPoint);
    if(currentStatusPointValue == 0)
        goto endlabel;
    endTime();
    LOGI("## brush scale time : %f ##\n", gRunningTime);
    startTime();
    if(rd.pass == 0)
    {
        for(brushIndex = 0 ; brushIndex < BRUSH_NUM && SS_GetAtomicCounterValue(currentStatusPoint); brushIndex++)
        {
            
            i = 1 + sqrtf (brushes[brushIndex].width  * brushes[brushIndex].width +
                           brushes[brushIndex].height * brushes[brushIndex].height);
            ppm_pad (&brushes[brushIndex], i-brushes[brushIndex].width, i-brushes[brushIndex].width,
                     i - brushes[brushIndex].height, i - brushes[brushIndex].height, back);
            
            i = 1 + sqrtf(destBrushes[brushIndex].width * destBrushes[brushIndex].width +
                          destBrushes[brushIndex].height * destBrushes[brushIndex].height);
            ppm_pad(&destBrushes[brushIndex], i - destBrushes[brushIndex].width, i - destBrushes[brushIndex].width, i - destBrushes[brushIndex].height, i - destBrushes[brushIndex].height, back);
            //SS_SaveBrush("padbrush", brushIndex, brushes[brushIndex]);
            NSLog(@"padding brush width = %d, height = %d", brushes[brushIndex].width, brushes[brushIndex].height);
        }
    }
    else
    {
        for(brushIndex = 0 ; brushIndex < BRUSH_NUM && SS_GetAtomicCounterValue(currentStatusPoint); brushIndex++)
        {
            autocrop(&brushes[brushIndex], 1);
            float brushLen = 1 + sqrtf(brushes[brushIndex].width  * brushes[brushIndex].width / 4 + brushes[brushIndex].height * brushes[brushIndex].height / 4);
            float left = brushLen - brushes[brushIndex].width / 2 ;
            float right = brushLen - brushes[brushIndex].width / 2;
            float top = brushLen - brushes[brushIndex].height / 2;
            float bottom = brushLen - brushes[brushIndex].height / 2;
            ppm_pad(&brushes[brushIndex], left, right, top, bottom, back);
            
            autocrop(&destBrushes[brushIndex], 1);
            brushLen = 1 + sqrtf(destBrushes[brushIndex].width  * destBrushes[brushIndex].width / 4 + destBrushes[brushIndex].height * destBrushes[brushIndex].height / 4);
            left = brushLen - destBrushes[brushIndex].width / 2 ;
            right = brushLen - destBrushes[brushIndex].width / 2;
            top = brushLen - destBrushes[brushIndex].height / 2;
            bottom = brushLen - destBrushes[brushIndex].height / 2;
            ppm_pad(&destBrushes[brushIndex], left, right, top, bottom, back);
        }
    }
    endTime();
    currentStatusPointValue = SS_GetAtomicCounterValue(currentStatusPoint);
    if(currentStatusPointValue == 0)
        goto endlabel;
    LOGI("## brush pad time : %f ##\n", gRunningTime);
    SS_SetLoadingStage(0);
    assert(num_brushes > BRUSH_NUM);
    for(brushIndex = 0 ; brushIndex < BRUSH_NUM && SS_GetAtomicCounterValue(currentStatusPoint); brushIndex++)
    {
        std::list<MyBrushData>& brushDataList = mipmapBrushes[brushIndex];
        std::list<MyBrushData>::iterator it;
        for(it = brushDataList.begin() ; it != brushDataList.end() ; it++)
        {
            padBrushes(&it->brush);
        }
    }
    currentStatusPointValue = SS_GetAtomicCounterValue(currentStatusPoint);
    if(currentStatusPointValue == 0)
        goto endlabel;
    for(int kk = 0 ; kk < BRUSH_NUM && SS_GetAtomicCounterValue(currentStatusPoint); kk++)
    {
        newBrushes[kk].col = NULL;
        newDestBrushes[kk].col = NULL;
        ppm_copy(&brushes[kk], &newBrushes[kk]);
        ppm_copy(&destBrushes[kk], &newDestBrushes[kk]);
        NSLog(@"new brush width = %d, height = %d", newBrushes[kk].width, newBrushes[kk].height);
        NSLog(@"new dest brush width = %d, height = %d", newDestBrushes[kk].width, newDestBrushes[kk].height);
        //SS_SaveBrush("dest_orig_brush", kk, newDestBrushes[kk]);
    }
    currentStatusPointValue = SS_GetAtomicCounterValue(currentStatusPoint);
    if(currentStatusPointValue == 0)
        goto endlabel;
    startTime();
    //create 180 angles brush
    //SETimePrinter timePP;
    brushDataList.resize(ALL_ANGLE + 1);
    for(int kk = 0 ; kk <= ALL_ANGLE && SS_GetAtomicCounterValue(currentStatusPoint); kk++)
    {
        ppm_t tmp = rotateBrush(&brushes[0], kk);
        //rescale(&tmp, runningvals.size_last / runningvals.size_first);
        MyBrushData md(tmp, kk);
        md.anglefirst = runningvals.orient_first;
        md.anglespan = runningvals.orient_last;
        //brushDataList.push_back(md);
        brushDataList[kk] = md;
         //SS_SaveBrush("brush180_", kk, tmp);
    }
    currentStatusPointValue = SS_GetAtomicCounterValue(currentStatusPoint);
    if(currentStatusPointValue == 0)
        goto endlabel;

    brushPassDataList.resize(runningvals.size_num * runningvals.orient_num);
    //if(rd.pass == 0)
    {
        for (i = 0; i < runningvals.size_num && SS_GetAtomicCounterValue(currentStatusPoint); i++)
        {
            double sv;
            if (runningvals.size_num > 1)
                sv = i / (runningvals.size_num - 1.0);
            else 
                sv = 1.0;
            for (j = 0; j < runningvals.orient_num && SS_GetAtomicCounterValue(currentStatusPoint); j++)
            {
                float times;
                h = j + i * runningvals.orient_num;
                //ppm_copy(&brushes[h], &destBrushes[h]);
                if(h < BRUSH_NUM)
                {
                    free_rotate (&brushes[h],
                                 startangle + j * anglespan / runningvals.orient_num);
                    autocrop (&brushes[h],1);
                    rescale (&brushes[h],
                             ( sv      * runningvals.size_first +
                              (1.0-sv) * runningvals.size_last    ) / runningvals.size_last);
                    destAngles[h] = startangle + j * anglespan / runningvals.orient_num;
                    destBrushIndex[h] = h;
                    times = ((float)destWidth) / gImageWidth;
                    double first , last;
                    first = runningvals.size_first * times;
                    last = runningvals.size_last * times;
                    free_rotate(&destBrushes[h], startangle + j * anglespan / runningvals.orient_num);
                    autocrop(&destBrushes[h], 1);
                    resize(&destBrushes[h], brushes[h].width * times, brushes[h].height * times);
                    //SS_SaveBrush("brush", h, brushes[h]);
                    //ppm_t blurBrush = [SEUtil blurBrush:brushes[h]];
                    //SS_SaveBrush("blurBrush", h, blurBrush);
                    //SS_SaveBrush("dest_brush_0_3_", h, destBrushes[h]);
                }
                else
                {
                    brushes[h].col = NULL;
                    destBrushes[h].col = NULL;
                    brushIndex = g_rand_int_range (random_generator, 0, BRUSH_NUM);
                    destBrushIndex[h] = brushIndex;
                    
                    brushes[h] = free_rotate_return(&newBrushes[brushIndex],
                                                    startangle + j * anglespan / runningvals.orient_num);
                    autocrop (&brushes[h],1);
                    rescale (&brushes[h],
                             ( sv      * runningvals.size_first +
                              (1.0-sv) * runningvals.size_last    ) / runningvals.size_last);
                    destAngles[h] = startangle + j * anglespan / runningvals.orient_num;
                    if(rd.pass == 0)
                    {
                        times = ((float)destWidth) / gImageWidth;
                        double first , last;
                        first = runningvals.size_first * times;
                        last = runningvals.size_last * times;
                        
                        destBrushes[h] = free_rotate_return(&newDestBrushes[brushIndex], startangle + j * anglespan / runningvals.orient_num);
                        autocrop(&destBrushes[h], 1);
                        resize(&destBrushes[h], brushes[h].width * times, brushes[h].height * times);
                    }
                }
                NSLog(@"after rotate brush width = %d, height = %d", brushes[h].width, brushes[h].height);
                //SS_SaveBrush("brush", h, brushes[h]);
                //ppm_t blurBrush = [SEUtil blurBrush:brushes[h]];
                //SS_SaveBrush("blurBrush", h, blurBrush);
                //SS_SaveBrush("destbrush", h, destBrushes[h]);
            }
        }
            //SS_Pause(currentPausePoint);
    }
    currentStatusPointValue = SS_GetAtomicCounterValue(currentStatusPoint);
    if(currentStatusPointValue == 0)
        goto endlabel;

    endTime();
    LOGI("## brush rotate time : %f ##\n", gRunningTime);
    //SS_AddLog("## brush rotate time : %f ###\n", gRunningTime);
    
    startTime();
    for (i = 0; i < num_brushes && SS_GetAtomicCounterValue(currentStatusPoint); i++)
        //for(int i = 0 ; i < BRUSH_NUM ; i++)
    {
        if (!runningvals.color_brushes)
        {
            prepare_brush (&brushes[i]);
            prepare_brush(&destBrushes[i]);
        }
        brushes_sum[i] = sum_brush (&brushes[i]);
        //LOGE("## brush sum %d : %f ##\n", i, brushes_sum[i]);
        //SS_Pause(currentPausePoint);
    }
    currentStatusPointValue = SS_GetAtomicCounterValue(currentStatusPoint);
    if(currentStatusPointValue == 0)
        goto endlabel;

    for(int kk = 0 ; kk < BRUSH_NUM && SS_GetAtomicCounterValue(currentStatusPoint); kk++)
    {
        if(!runningvals.color_brushes)
        {
            prepare_brush(&newBrushes[kk]);
        }
        newBrushSum[kk] = sum_brush(&newBrushes[kk]);
        NSLog(@"newBrushSum = %f", newBrushSum[kk]);
    }
    currentStatusPointValue = SS_GetAtomicCounterValue(currentStatusPoint);
    if(currentStatusPointValue == 0)
        goto endlabel;

    endTime();
    LOGI("## brush sum time : %f ##\n", gRunningTime);
    brush = &brushes[0];
    thissum = brushes_sum[0];
    
    maxbrushwidth = maxbrushheight = 0;
    startTime();
    for (i = 0; i < num_brushes; i++)
    {
        if (brushes[i].width > maxbrushwidth)
            maxbrushwidth = brushes[i].width;
        if (brushes[i].height > maxbrushheight)
            maxbrushheight = brushes[i].height;
    }
    endTime();
    currentStatusPointValue = SS_GetAtomicCounterValue(currentStatusPoint);
    if(currentStatusPointValue == 0)
        goto endlabel;

    LOGI("## max brush time : %f ##\n", gRunningTime);
    startTime();
    for (i = 0; i < num_brushes && SS_GetAtomicCounterValue(currentStatusPoint); i++)
        //for(int i = 0 ; i < BRUSH_NUM ; i++)
    {
        int xp, yp;
        guchar blk[3] = {0, 0, 0};
        
        xp = maxbrushwidth - brushes[i].width;
        yp = maxbrushheight - brushes[i].height;
        if (xp || yp)
        {
            float times = ((float)destWidth) / gImageWidth;
            int left = xp / 2;
            int right = xp - xp / 2;
            int top = yp / 2;
            int bottom = yp - yp / 2;
            ppm_pad (&brushes[i], xp / 2, xp - xp / 2, yp / 2, yp - yp / 2, blk);
            ppm_pad(&destBrushes[i], left * times, right * times, top * times, bottom * times, blk);
        }
        //SS_SaveBrush("padbrush", i, brushes[i]);
        //SS_SaveBrush("paddestbrush", i, destBrushes[i]);
        //SS_Pause(currentPausePoint);
    }
    currentStatusPointValue = SS_GetAtomicCounterValue(currentStatusPoint);
    if(currentStatusPointValue == 0)
        goto endlabel;

    for(std::vector<MyBrushData>::iterator it = brushDataList.begin();
        it != brushDataList.end();
        it++)
    {
        prepare_brush(&it->brush);
        it->sum = sum_brush(&it->brush);
    }
    currentStatusPointValue = SS_GetAtomicCounterValue(currentStatusPoint);
    if(currentStatusPointValue == 0)
        goto endlabel;

    //for(std::list<MyBrushData>::iterator it = brushDataList.begin(); 
    //    it != brushDataList.end();
    //    it++)
    for(std::vector<MyBrushData>::iterator it = rawBrushDataList.begin(); 
        it != rawBrushDataList.end();
        it++)
    {
        if(it->brush.width > maxFixBrushWidth)
            maxFixBrushWidth = it->brush.width;
        if(it->brush.height > maxFixBrushHeight)
            maxFixBrushHeight = it->brush.height;
    }
    NSLog(@"maxFixBrushWidth = %d, maxFixBrushHeight = %d", maxFixBrushWidth, maxFixBrushHeight);
    if(rd.pass != 0)
    {
        maxbrushwidth = maxFixBrushWidth;
        maxbrushheight = maxFixBrushHeight;
    }
    currentStatusPointValue = SS_GetAtomicCounterValue(currentStatusPoint);
    if(currentStatusPointValue == 0)
        goto endlabel;

    for(std::vector<MyBrushData>::iterator it = brushDataList.begin();
        it != brushDataList.end() && SS_GetAtomicCounterValue(currentStatusPoint);
        it++)
    {
        int xp = maxFixBrushWidth - it->brush.width;
        int yp = maxFixBrushHeight - it->brush.height;
        //NSLog(@"xp = %d, yp = %d ", xp, yp);
        ppm_copy(&it->brush, &it->rawBrush);
        if(xp || yp)
        {
            ppm_pad(&it->brush, xp / 2, xp - xp / 2, yp / 2, yp - yp / 2, back);
            ppm_pad(&it->rawBrush, xp / 2, xp - xp / 2, yp / 2, yp - yp / 2, back);
        }
    }
    endTime();
    LOGI("## brush pad to max time: %f ##\n", gRunningTime);    
    currentStatusPointValue = SS_GetAtomicCounterValue(currentStatusPoint);
    if(currentStatusPointValue == 0)
        goto endlabel;

    createAllBrushMap(maxbrushwidth, maxbrushheight, brushMapDataMap);
    //brushRotateTimer.printTime("brush line rotateTime");
    if (runningvals.general_paint_edges)
    {
        edgepad (p, maxbrushwidth, maxbrushwidth,
                 maxbrushheight, maxbrushheight);
        if (img_has_alpha)
            edgepad (a, maxbrushwidth, maxbrushwidth,
                     maxbrushheight, maxbrushheight);
    }
    //endTime();
    //LOGI("### brush initial time : %f ####\n", gRunningTime);
    tmp.width = p->width;
    tmp.height = p->height;
    tmpWidth = tmp.width;
    tmpHeight = tmp.height;
    gBrushMaxWidth = maxbrushwidth;
    gBrushMaxHeight = maxbrushheight;
    
    if(rd.pass != 0)
    {
        assert(maxbrushwidth == maxFixBrushWidth);
        assert(maxbrushheight == maxFixBrushHeight);
    }
    LOGI("## gBrushMaxWidth = %d, gBrushMaxHeight = %d ##\n", gBrushMaxWidth, gBrushMaxHeight);
    
#ifdef MACOS
    static int startupDrawing = 1;
    if(startupDrawing)
    {
        //SS_SetCanvasBackground(currentCanvas, gBackground);
        startupDrawing = 0;
    }
#else
    
    if(repaintCallBack)
	{
	    (*repaintCallBack)("background", "initok");
  	}
#endif
    cx = p->width / 2;
    cy = p->height / 2;
    maxdist = sqrtf (cx * cx + cy * cy);
    startTime();
    switch (runningvals.orient_type)
    { 
        case ORIENTATION_VALUE:
            ppm_new (&dirmap, p->width, p->height);
            for (y = 0; y < dirmap.height && SS_GetAtomicCounterValue(currentStatusPoint); y++)
            {
                guchar *dstrow = &dirmap.col[y * dirmap.width * 3];
                guchar *srcrow = &p->col[y * p->width * 3];
                for (x = 0; x < dirmap.width; x++)
                {
                    dstrow[x * 3] =
                    (srcrow[x * 3] + srcrow[x * 3 + 1] + srcrow[x * 3 + 2]) / 3;
                }
            }
            break;
            
        case ORIENTATION_RADIUS:
            ppm_new (&dirmap, p->width, p->height);
            for (y = 0; y < dirmap.height && SS_GetAtomicCounterValue(currentStatusPoint); y++)
            {
                guchar *dstrow = &dirmap.col[y * dirmap.width * 3];
                double ysqr = (cy - y) * (cy - y);
                
                for (x = 0; x < dirmap.width; x++)
                {
                    dstrow[x*3] = sqrt ((cx - x) * (cx - x) + ysqr) * 255 / maxdist;
                }
            }
            break;
            
        case ORIENTATION_RADIAL:
            ppm_new (&dirmap, p->width, p->height);
            for (y = 0; y < dirmap.height && SS_GetAtomicCounterValue(currentStatusPoint); y++)
            {
                guchar *dstrow = &dirmap.col[y * dirmap.width * 3];
                
                for (x = 0; x < dirmap.width; x++)
                {
                    dstrow[x * 3] = (G_PI + atan2f (cy - y, cx - x)) *
                    255.0 / (G_PI * 2);
                }
            }
            break;
            
        case ORIENTATION_FLOWING:
            ppm_new (&dirmap, p->width / 6 + 5, p->height / 6 + 5);
            mkgrayplasma (&dirmap, 15);
            blur (&dirmap, 2, 2);
            blur (&dirmap, 2, 2);
            resize (&dirmap, p->width, p->height);
            blur (&dirmap, 2, 2);
            if (runningvals.general_paint_edges)
                edgepad (&dirmap, maxbrushwidth, maxbrushheight,
                         maxbrushwidth, maxbrushheight);
            break;
            
        case ORIENTATION_HUE:
            ppm_new (&dirmap, p->width, p->height);
            for (y = 0; y < dirmap.height && SS_GetAtomicCounterValue(currentStatusPoint); y++)
            {
                guchar *dstrow = &dirmap.col[y * dirmap.width * 3];
                guchar *srcrow = &p->col[y * p->width * 3];
                
                for (x = 0; x < dirmap.width; x++)
                {
                    dstrow[x * 3] = get_hue (&srcrow[x * 3]);
                }
            }
            break;
            
        case ORIENTATION_ADAPTIVE:
        {
            guchar tmpcol[3] = {0, 0, 0};
            
            ppm_new (&dirmap, p->width, p->height);
            fill (&dirmap, tmpcol);
        }
            break;
            
        case ORIENTATION_MANUAL:
            ppm_new (&dirmap, p->width-maxbrushwidth*2, p->height-maxbrushheight*2);
            for (y = 0; y < dirmap.height && SS_GetAtomicCounterValue(currentStatusPoint); y++)
            {
                guchar *dstrow = &dirmap.col[y * dirmap.width * 3];
                double tmpy = y / (double)dirmap.height;
                for (x = 0; x < dirmap.width; x++)
                {
                    dstrow[x * 3] = get_pixel_value(90 -
                                                    get_direction(x /
                                                                  (double)dirmap.width,
                                                                  tmpy, 1));
                }
            }
            edgepad (&dirmap,
                     maxbrushwidth, maxbrushwidth,
                     maxbrushheight, maxbrushheight);
            break;
    }
    
    if (runningvals.size_type == SIZE_TYPE_VALUE)
    {
        ppm_new (&sizmap, p->width, p->height);
        for (y = 0; y < sizmap.height && SS_GetAtomicCounterValue(currentStatusPoint); y++)
        {
            guchar *dstrow = &sizmap.col[y * sizmap.width * 3];
            guchar *srcrow = &p->col[y * p->width * 3];
            
            for (x = 0; x < sizmap.width; x++)
            {
                dstrow[x * 3] =
                (srcrow[x * 3] + srcrow[x * 3 + 1] + srcrow[x * 3 + 2]) / 3;
            }
        }
    }
    else if (runningvals.size_type == SIZE_TYPE_RADIUS)
    {
        ppm_new (&sizmap, p->width, p->height);
        for (y = 0; y < sizmap.height && SS_GetAtomicCounterValue(currentStatusPoint); y++)
        {
            guchar *dstrow = &sizmap.col[y * sizmap.width * 3];
            double ysqr = (cy - y) * (cy - y);
            
            for (x = 0; x < sizmap.width; x++)
            {
                dstrow[x * 3] =
                sqrt ((cx - x) * (cx - x) + ysqr) * 255 / maxdist;
            }
        }
    }
    else if (runningvals.size_type == SIZE_TYPE_RADIAL)
    {
        ppm_new (&sizmap, p->width, p->height);
        for (y = 0; y < sizmap.height && SS_GetAtomicCounterValue(currentStatusPoint); y++)
        {
            guchar *dstrow = &sizmap.col[y * sizmap.width * 3];
            
            for (x = 0; x < sizmap.width; x++)
            {
                dstrow[x * 3] = (G_PI + atan2f (cy - y, cx - x)) *
                255.0 / (G_PI * 2);
            }
        }
    }
    else if (runningvals.size_type == SIZE_TYPE_FLOWING)
    {
        ppm_new (&sizmap, p->width / 6 + 5, p->height / 6 + 5);
        mkgrayplasma (&sizmap, 15);
        blur (&sizmap, 2, 2);
        blur (&sizmap, 2, 2);
        resize (&sizmap, p->width, p->height);
        blur (&sizmap, 2, 2);
        if (runningvals.general_paint_edges)
            edgepad (&sizmap,
                     maxbrushwidth, maxbrushheight,
                     maxbrushwidth, maxbrushheight);
    }
    else if (runningvals.size_type == SIZE_TYPE_HUE)
    {
        ppm_new (&sizmap, p->width, p->height);
        for (y = 0; y < sizmap.height; y++)
        {
            guchar *dstrow = &sizmap.col[y * sizmap.width * 3];
            guchar *srcrow = &p->col[y * p->width * 3];
            
            for (x = 0; x < sizmap.width; x++)
            {
                dstrow[ x * 3] = get_hue (&srcrow[x * 3]);
            }
        }
    }
    else if (runningvals.size_type == SIZE_TYPE_ADAPTIVE)
    {
        guchar tmpcol[3] = {0, 0, 0};
        
        ppm_new (&sizmap, p->width, p->height);
        fill (&sizmap, tmpcol);
    }
    else if (runningvals.size_type == SIZE_TYPE_MANUAL)
    {
        ppm_new (&sizmap,
                 p->width-maxbrushwidth * 2,
                 p->height-maxbrushheight * 2);
        
        for (y = 0; y < sizmap.height && SS_GetAtomicCounterValue(currentStatusPoint); y++)
        {
            guchar *dstrow = &sizmap.col[y * sizmap.width * 3];
            double tmpy = y / (double)sizmap.height;
            
            for (x = 0; x < sizmap.width; x++)
            {
                dstrow[x * 3] = 255 * (1.0 - get_siz_from_pcvals (x / (double)sizmap.width, tmpy));
            }
        }
        edgepad (&sizmap,
                 maxbrushwidth, maxbrushwidth,
                 maxbrushheight, maxbrushheight);
    }
#if 0
    ppm_save(&sizmap, "/tmp/_sizmap.ppm");
#endif
    if (runningvals.place_type == PLACEMENT_TYPE_RANDOM)
    {
        i = tmp.width * tmp.height / (maxbrushwidth * maxbrushheight);
        i *= density;
    }
    else if (runningvals.place_type == PLACEMENT_TYPE_EVEN_DIST)
    {
        i = (int)(tmp.width * density / maxbrushwidth) *
        (int)(tmp.height * density / maxbrushheight);
        step = i;
#if 0
        g_printerr("step=%d i=%d\n", step, i);
#endif
    }
    
    if (i < 1)
        i = 1;
    
    if (runningvals.place_type == PLACEMENT_TYPE_EVEN_DIST)
    {
        int j;
        
        xpos = g_new (int, i);
        ypos = g_new (int, i);
        for (j = 0; j < i; j++)
        {
            int factor = (int)(tmp.width * density / maxbrushwidth + 0.5);
            
            if (factor < 1)
                factor = 1;
            xpos[j] = maxbrushwidth/2 + (j % factor) * maxbrushwidth / density;
            ypos[j] = maxbrushheight/2 + (j / factor) * maxbrushheight / density;
        }
        
        for (j = 0; j < i; j++)
        {
            int a, b;
            //TODO : change code
            a = g_rand_int_range (random_generator, 0, i);
            //TODO : end
            b = xpos[j]; 
            xpos[j] = xpos[a];
            xpos[a] = b;
            b = ypos[j]; 
            ypos[j] = ypos[a];
            ypos[a] = b;
        }
        
    }
    //start calculate brush
    endTime();
    LOGI("## brush initial size and direct map time : %f ##\n", gRunningTime);
    LOGI("## total brush num = %d ##\n", i);
    //SS_AddLog(" total brush num = %d ##\n", i);
    
    //SS_SetFirstNum(i);
    
    //gPass++;
    NSLog(@"## running orient type = %d, size type = %d", runningvals.orient_type, runningvals.size_type);
    
    if(i > 1000000)
    {
        sendByNum = true;
    }
    SS_SetLoadingStage(1);
    allCount = i;
    startTime();
    //createBrushBlockMatrix(rd.pass, rd.pass + 1, );
    //SS_SetSecondNum(rd.brushNum);
    rd.allBrushCount = allCount;
    rd.currentGrayIndex = 0;
    rd.currentGrayBrushCount = 0;
    for (; i > 0 && SS_GetAtomicCounterValue(currentStatusPoint); i--)
    {
        int passedCount = allCount - i;
        float passedPercent = passedCount / (float) allCount;
        if(passedPercent >= 0.5)
        {
            if(bSetStage2 == NO)
            {
                bSetStage2 = YES;
                SS_SetLoadingStage(2);
            }
        }
        //SS_Pause(currentPausePoint);
        /*
        if(allCount < 1000)
        {
            if(i % 100 == 0)
                SS_SetSecondNum(i);
        }
        else if(allCount < 50000)
        {
            if(i % 1000 == 0)
                SS_SetSecondNum(i);
        }
        else if(allCount < 500000)
        {
            if(i % 5000 == 0)
                SS_SetSecondNum(i);
        }
        else if(allCount > 1000000)
        {
            if(i % 10000 == 0)
            {
                SS_SetSecondNum(i);
            }
        }
         */
        float left = tmp.width * 0.625;
        float right = tmp.width * 0.9589;
        float top = tmp.height * 0.4348;
        float bottom = tmp.height * 0.5546;
        
        if (runningvals.place_type == PLACEMENT_TYPE_RANDOM)
        {
            tx = g_rand_int_range (random_generator, maxbrushwidth / 2,
                                   tmp.width - maxbrushwidth / 2);
            ty = g_rand_int_range (random_generator, maxbrushheight / 2,
                                   tmp.height - maxbrushheight / 2);
            //tx = tmp.width * 3 / 4;    
            //ty = tmp.height / 2 - maxbrushheight / 4;
            //tx = tmp.width * 0.5146;
            //ty = tmp.height * 0.43489;
        }
        else if (runningvals.place_type == PLACEMENT_TYPE_EVEN_DIST)
        {
            tx = xpos[i - 1];
            ty = ypos[i - 1];
        }
        if (runningvals.placement_center)
        {
            double z = g_rand_double_range (random_generator, 0, 0.75);
            tx = tx * (1.0 - z) + tmp.width / 2 * z;
            ty = ty * (1.0 - z) + tmp.height / 2 * z;
        }
        
        if ((tx < maxbrushwidth / 2)             ||
            (ty < maxbrushwidth / 2)             ||
            (tx + maxbrushwidth / 2 >= p->width) ||
            (ty + maxbrushheight / 2 >= p->height))
        {
#if 0
            LOGI("Internal Error; invalid coords: (%d,%d) i=%d\n", tx, ty, i);
#endif
            continue;
        }
        //for test
        /*
        if(rd.pass != 0 && (tx < left || tx > right || ty < top || ty > bottom))
            continue;
         */
        //end
        if (img_has_alpha)
        {
            if (a->col[ty * a->width * 3 + tx * 3] > 128)
                continue;
        }
        if(rd.mostConcisePass)
        {
            float outRadian = 0;
            bool onEdge = brushOnEdge(&edgeDetectionMap, maxbrushwidth, maxbrushheight, tx - maxbrushwidth / 2, ty - maxbrushheight / 2, outRadian);
            if(onEdge)
            {
                //tx -= maxbrushwidth / 2;
                //ty -= maxbrushheight / 2;
                /*
                if(tx > 768)
                {
                    //LOGI("precise tx = %d, ty = %d", tx, ty);
                }
                 */
                BrushProperty bp;
                //int bpSize = sizeof(BrushProperty);
                //LOGI("## BrushProperty Size = %d ##\n", bpSize);
                bp.p2.precise = true;
                bp.p2.startx = tx;
                bp.p2.starty = ty;
                bp.p2.endx = tx + maxbrushwidth;
                bp.p2.endy = ty + maxbrushheight;
                /*
                int r = 0, g = 0, b = 0;
                int count = 0;
                for(int y = 0 ; y < maxbrushheight ; y++)
                {
                    for(int x = 0 ; x < maxbrushwidth ; x++)
                    {
                        guchar* data = p->col + y * p->width * 3 + x * 3;
                        r += data[0];
                        g += data[1];
                        b += data[2];
                        count++;
                    }
                }
                r /= count;
                g /= count;
                b /= count;
                bp.p1.r = r;
                bp.p1.g = g;
                bp.p1.b = b;
                bp.p1.tx = tx;
                bp.p1.ty = ty;
                 */
                addBrushPropertyToGrayPaintArea(bp, true, rd);
                /*
                rd.brushNum++;
                if(rd.brushNum % 10000 == 0)
                {
                    SS_SetSecondNum(rd.brushNum);
                }
                 */
                //gMostPreciseBrushPiecelist.push_back(bp);
                
            }
            
            continue;
        }
        n = sn = on = 0;
        
        switch (runningvals.orient_type)
        {
            case ORIENTATION_RANDOM:
                on = g_rand_int_range (random_generator, 0, runningvals.orient_num);
                break;
                
            case ORIENTATION_VALUE:
            case ORIENTATION_RADIUS:
            case ORIENTATION_RADIAL:
            case ORIENTATION_FLOWING:
            case ORIENTATION_HUE:
            case ORIENTATION_MANUAL:
                on = runningvals.orient_num *
                dirmap.col[ty * dirmap.width * 3 + tx * 3] / 256;
                break;
                
            case ORIENTATION_ADAPTIVE:
                break; /* Handled below */
                
            default:
                LOGI ("Internal error; Unknown orientationtype\n");
                on = 0;
                break;
        }
        
        switch (runningvals.size_type)
        {
            case SIZE_TYPE_RANDOM:
                sn = g_rand_int_range (random_generator, 0, runningvals.size_num);
                break;
                
            case SIZE_TYPE_VALUE:
            case SIZE_TYPE_RADIUS:
            case SIZE_TYPE_RADIAL:
            case SIZE_TYPE_FLOWING:
            case SIZE_TYPE_HUE:
            case SIZE_TYPE_MANUAL:
                sn = runningvals.size_num * sizmap.col[ty*sizmap.width*3+tx*3] / 256;
                break;
                
            case SIZE_TYPE_ADAPTIVE:
                break; /* Handled below */
                
            default:
                LOGI ("Internal error; Unknown size_type\n");
                sn = 0;
                break;
        }
        bool calculateEdge = rd.calculateOnEdge;
        /* Handle Adaptive selections */
        bool onEdge = !calculateEdge;
        bool goodAngle = true;
        float outRadian = 0;
        if(!onEdge)
        {
            
            onEdge = brushOnEdge(&edgeDetectionMap, maxbrushwidth, maxbrushheight, tx - maxbrushwidth / 2, ty - maxbrushheight / 2, outRadian);
            //NSLog(@"tx = %d, ty = %d, maxbrushWidth = %d, maxbrushheight = %d", tx, ty, maxbrushwidth, maxbrushheight);
            static int kkk = 10;
            if(kkk > 0)
            {
                NSLog(@"onEdge = %d, pass = %d, calculateEdge = %d", onEdge, rd.pass, rd.calculateOnEdge);
                kkk--;
            }
            //float angle = outRadian * 180 / 3.1415926;
            //NSLog(@"## angle = %f #####", angle);
            if(onEdge)
            {
                edgeBrushNum++;
            }
        }
        
        double angle = -1;
        int currentBrushIndex = -1;
        //SETimePrinter tp;
        if (runningvals.orient_type == ORIENTATION_ADAPTIVE)
        {
            if (runningvals.size_type == SIZE_TYPE_ADAPTIVE)
            {
                
                if(onEdge)
                {
                    currentBrushIndex = 0;//g_rand_int_range(random_generator, 0, BRUSH_NUM);
                    //angle = choose_best_brush_angle(p, a, tx, ty, brushDataList);
                    angle = choose_best_brush_angle(p, a, tx - maxbrushwidth / 2, ty - maxbrushheight / 2, rawBrushDataList, goodAngle, brushMapDataMap);
                    n = -2;
                    
                }
                else 
                {
                    n = -1;
                }
            }
            else
            {
                int st = sn * runningvals.orient_num;
                if(onEdge)
                {
                    currentBrushIndex = g_rand_int_range(random_generator, 0, BRUSH_NUM);
                    angle = choose_best_brush_angle(p, a, tx - maxbrushwidth / 2, ty - maxbrushheight / 2, rawBrushDataList, goodAngle,brushMapDataMap);
                    n = -2;
                    
                }
                else 
                {
                    n = -1;
                }
            }
        }
        else
        {
            if(onEdge)
            {
                if (runningvals.size_type == SIZE_TYPE_ADAPTIVE)
                {
                    angle = choose_best_brush_angle(p, a, tx - maxbrushwidth / 2, ty - maxbrushheight / 2, rawBrushDataList, goodAngle, brushMapDataMap);
                    n = -2;
                }
                else
                {
                    n = sn * runningvals.orient_num + on;
                }
            }
            else 
            {
                n = -1;
            }
        }
        //tp.printTime("## choose best brush angle: ");
        //NSLog(@"n = %d", n);
        /* Should never happen, but hey... */
        if (n == -1)
        {
            //change for edge detection
            //n = 0;
            continue;
        }
        else if (n >= num_brushes)
        {
            LOGI("## brush index exceed: %d ###\n", n);
            n = num_brushes - 1;
        }
        if(!goodAngle)
            continue;
        /*
        NSLog(@"## angle = %f ####", angle);
        if( (angle <= 80 || angle >= 120) && rd.pass != 0)
            continue;
         */
        // if(tOnEdge)
        //NSLog(@"## angle = %f ####", angle);
        //NSLog(@"## choose angle = %f ###", destAngles[n]);
        tx -= maxbrushwidth/2;
        ty -= maxbrushheight/2;
        ppm_t* rawBrush = NULL;
        //setAngleToBrushPointVector(brushPointVector, tx, ty, angle);
        if(n >= 0)
        {
            brush = &brushes[n];
        }
        else 
        {
            //SETimePrinter tp1;
            brush = getBrushByAngle(angle, brushDataList);
            rawBrush = getRawBrushByAngle(angle, brushDataList);
            //tp1.printTime("get brush by angle");
        }
        if (dropshadow)
            shadow = &shadows[n];
        if(n >= 0)
        {
            thissum = brushes_sum[n];
        }
        else
        {
            MyBrushData bd = getBrushDataByAngle(angle, brushDataList);
            thissum = bd.sum;//sum_brush(brush);
        }
        //SETimePrinter tp2;
        /* Calculate color - avg. of in-brush pixels */
        if(!rd.mostConcisePass)
        {
            //runningvals.color_type = 1;//for test
            if (runningvals.color_type == 0)
            {
                r = g = b = 0;
                int totalNum = 0;
                for (y = 0; y < brush->height; y ++)
                {
                    guchar *row = &p->col[(ty + y) * p->width * 3];
                    
                    for (x = 0; x < brush->width; x ++)
                    {
                        int k = (tx + x) * 3;
                        double v;
                        
                        if ((h = brush->col[y * brush->width * 3 + x * 3]))
                        {
                            v = h / 255.0;
                            r += row[k+0];
                            g += row[k+1];
                            b += row[k+2];
                            totalNum++;
                                
                        }
                    }
                }
                //assert( thissum > 0);
                if(totalNum != 0)
                {
                    //if(rd.pass == 0)
                    r = r / totalNum;
                    g = g / totalNum;
                    b = b / totalNum;
                    
                    //LOGI("## 1 rgb = %f, %f, %f\n", r,g ,b);
                }
                else
                {
                    y = ty + (maxbrushheight / 2);
                    x = tx + (maxbrushwidth / 2);
                    guchar* pixel = &p->col[y*p->width * 3 + x * 3];
                    r = pixel[0];
                    g = pixel[1];
                    b = pixel[2];
                    //LOGI("## 2 x = %d, y = %d\n", x, y);
                    //LOGI("## 2 rgb = %f, %f, %f\n", r,g ,b);
                }
            }
            else if (runningvals.color_type == 1)
            {
                guchar *pixel;
                
                y = ty + (maxbrushheight / 2);
                x = tx + (maxbrushwidth / 2);
                pixel = &p->col[y*p->width * 3 + x * 3];
                r = pixel[0];
                g = pixel[1];
                b = pixel[2];
                //LOGI("3 x = %d, y = %d\n", x, y);
                //LOGI("## 3 rgb = %f, %f, %f\n", r,g ,b);

                /*
                guchar* brushPixel = &brush->col[brush->width * 3 * brush->height / 2 + 3 * brush->width / 2];
                if(brushPixel[0] != 0)
                {
                    double v = brushPixel[0] / 255.0;
                    
                    //LOGI("## 3 rgb = %f, %f, %f\n", r,g ,b);
                }
                else
                {
                    r = pixel[0];
                    g = pixel[1];
                    b = pixel[2];
                    //LOGI("## 4 rgb = %f, %f, %f\n", r,g ,b);
                }
                 */
            }
            else
            {
                /* No such color_type! */
                r = g = b = 0;
                //LOGI("## 5 rgb = %f, %f, %f\n", r,g ,b);
            }
        }
        //tp2.printTime("calculate rgb");
        //debug for change
        //apply_brush (brush, shadow, &tmp, &atmp, tx,ty, r,g,b);
        //assert(r == 255 && g == 255 && b == 255);
        BrushProperty bp;
        bp.p1.brush = brush;
        //SETimePrinter tp3;
        int ss = g_rand_int_range(random_generator, -rd.adjustAngle, rd.adjustAngle);
        angle += ss;
        ppm_t* currentDestBrush = getBrushByAngleForList(angle, destBrushDataList);
        if(n == -2 && currentDestBrush == NULL)
        {
            float times = ((float)destWidth) / gImageWidth;
            float rawBrushWidth = rawBrush->width;
            float rawBrushHeight = rawBrush->height;
            float tmpWidth = brush->width;//rawBrush->width + brushSizeComp;//brush->width;
            float tmpHeight = brush->height;//rawBrush->height + brushSizeComp;//brush->height;
            ppm_t tmpDestBrush = {0, 0, NULL};
            int newBrushIndex = g_rand_int_range(random_generator, 0, BRUSH_NUM);
            std::list<MyBrushData> originBrushList = mipmapBrushes[newBrushIndex];
            float brushDestWidth = tmpWidth * times;
            float brushDestHeight = tmpHeight * times;
            ppm_t bestBrush = getBestBrush(originBrushList, brushDestWidth, brushDestHeight);
            tmpDestBrush = free_rotate_return(&bestBrush, angle);
            //SS_SaveBrush("lastBrush", 0, tmpDestBrush);
            //autocrop(&tmpDestBrush, 1);
            NSLog(@"dest brush width = %f, height = %f", tmpWidth * times, tmpHeight * times);
            resize(&tmpDestBrush, tmpWidth * times, tmpHeight * times);
            //prepare_brush(&tmpDestBrush);
            MyBrushData mbd(tmpDestBrush, angle);
            destBrushDataList.push_back(mbd);
            currentDestBrush = getBrushByAngleForList(angle, destBrushDataList);
            /*
            static bool save = false;
            if(!save)
            {
                save = true;
                SS_SaveBrush("mybrush", 0, newDestBrushes[0]);
            }
            SS_SaveBrush("brush", destBrushDataList.size(), tmpDestBrush);
             */
        }
        if(n >= 0)
        {
            bp.p1.destBrush = &destBrushes[n];
        }
        else
        {
            bp.p1.destBrush = currentDestBrush;//&destBrushes[n];
        }
        /*
        bool foundDrawing = false;
        for(std::list<ppm_t>::iterator itDrawingBrush = destBrushesForDrawing.begin();
            itDrawingBrush != destBrushesForDrawing.end();
            itDrawingBrush++)
        {
            if(itDrawingBrush->col == bp.p1.destBrush->col)
            {
                foundDrawing = true;
                break;
            }
        }
        if(!foundDrawing)
        {
            destBrushesForDrawing.push_back(*bp.p1.destBrush);
        }
         */
        bp.p1.b = b;
        bp.p1.g = g;
        bp.p1.r = r;
        //bp.p1.shadow = shadow;
        bp.p1.tx = tx;
        bp.p1.ty = ty;
        //gBrushProperties.push_back(bp);
        addBrushPropertyToGrayPaintArea(bp, false, rd);
        //end
    }
    endTime();
    LOGI("## brush create time : %f ####\n", gRunningTime);
    //SS_AddLog("## brush create time : %f ####\n", gRunningTime);
    LOGI("### edge brush num = %d ##\n", edgeBrushNum);
    //SS_AddLog("## edge brush num = %d ##\n", edgeBrushNum);
  	//debug for change
	//apply_brush (brush, shadow, &tmp, &atmp, tx,ty, r,g,b);
    
    tmpWidth = tmp.width;
    tmpHeight = tmp.height;
    /////// release the data which will not used
    for(i = 0 ; i < BRUSH_NUM ; i++)
    {
        ppm_kill(&newBrushes[i]);
        ppm_kill(&newDestBrushes[i]);
    }
    g_free(newBrushes);
    newBrushes = NULL;
    g_free(newDestBrushes);
    newDestBrushes = NULL;
    g_free(newBrushSum);
    newBrushSum = NULL;
    delete[] scales;
    scales = NULL;
    g_free(destAngles);
    destAngles = NULL;
    g_free(destBrushIndex);
    destBrushIndex = NULL;

    for(std::vector<MyBrushData>::iterator it = rawBrushDataList.begin();
        it != rawBrushDataList.end();
        it++)
    {
        ppm_kill(&it->brush);
    }
    rawBrushDataList.clear();
    for(std::vector<std::list<MyBrushData> >::iterator it = mipmapBrushes.begin();
        it != mipmapBrushes.end();
        it++)
    {
        std::list<MyBrushData>::iterator brushIt;
        for(brushIt = it->begin() ; brushIt != it->end(); brushIt++)
        {
            ppm_kill(&brushIt->brush);
        }
    }
    mipmapBrushes.clear();
    g_free (shadows);
    shadows = NULL;
    g_free (brushes_sum);
    brushes_sum = NULL;
    g_free (xpos);
    xpos = NULL;
    g_free (ypos);
    ypos = NULL;
    ppm_kill(p);
    ppm_kill(&tmp);
    ppm_kill (&paper_ppm);
    ppm_kill (&dirmap);
    ppm_kill (&sizmap);
    if(rd.calculateOnEdge)
        ppm_kill(&edgeDetectionMap);
    ////// end
    LOGI("############# start create brush piece w = %d, h = %d ##############\n", tmpWidth, tmpHeight);
    currentStatusPointValue = SS_GetAtomicCounterValue(currentStatusPoint);
    if(currentStatusPointValue == 0)
        goto endlabel;

    /*
     //gBrushProperties.sort(_BrushPropertyComp());
     */
    //use separate function to sort property
    //clearQuadTree(rootQuadTree);
    //clearGrayPaintArea();
    SS_ComputeReady();
    LOGI("## set pause point before ##\n");
    SS_SetComputationPausePoint(currentPausePoint);
    LOGI("## set pause point end ## \n");
    startTime();
    currentStatusPointValue = SS_GetAtomicCounterValue(currentStatusPoint);
    if(currentStatusPointValue == 0)
        goto endlabel;
    
    {
        int drawing_speed = 0;
        if(rd.mostConcisePass)
        {
            drawing_speed = 500;
        }
        else 
        {
            drawing_speed =  SS_GetDrawingSpeed();
        }
        if(drawing_speed <= 0)
            drawing_speed = 1;
        separatePaintArea(maxbrushwidth, maxbrushheight, destWidth,destHeight, gImageWidth, gImageHeight, tmp, drawing_speed, rd, currentStatusPoint, destBrushesForDrawing);
    }
    //clearGrayPaintArea();
    endTime();
    LOGI("## add brush to area time : %f ##\n", gRunningTime);
    //SS_AddLog("## add brush to area time : %f ##\n", gRunningTime);
    //end
#ifdef MACOS
    //SE_startBrushPaint();
#else
	if(repaintCallBack)
	{
        LOGI("## call repaint callback ##\n");
		(*repaintCallBack)("apply_brush", "start");
        LOGI("## call repaint callback end ##\n");
	}
#endif
endlabel:
    if(newBrushes != NULL)
    {
        for(i = 0 ; i < BRUSH_NUM ; i++)
        {
            ppm_kill(&newBrushes[i]);
        }
    }
    if(newDestBrushes != NULL)
    {
        for(i = 0  ; i < BRUSH_NUM ; i++)
        {
            ppm_kill(&newDestBrushes[i]);
        }
    }
    g_free(newBrushes);
    g_free(newDestBrushes);
    g_free(newBrushSum);
    delete[] scales;
    g_free(destAngles);
    g_free(destBrushIndex);
    for(std::list<MyBrushData>::iterator it = destBrushDataList.begin();
        it != destBrushDataList.end() ; it++)
    {
        bool b = brushInBrushDrawingList(destBrushesForDrawing, &it->brush);
        if(!b)
        {
            ppm_kill(&it->brush);
        }
    }
    for(std::vector<MyBrushData>::iterator it = brushDataList.begin() ;
        it != brushDataList.end();
        it++)
    {
        ppm_kill(&it->brush);
        ppm_kill(&it->rawBrush);
    }
    for(std::vector<MyBrushData>::iterator it = rawBrushDataList.begin();
        it != rawBrushDataList.end();
        it++)
    {
        ppm_kill(&it->brush);
    }
    for (i = 0; i < num_brushes; i++)
    {
        ppm_kill (&brushes[i]);
        bool b = brushInBrushDrawingList(destBrushesForDrawing, &destBrushes[i]);
        if(!b)
        {
            ppm_kill(&destBrushes[i]);
        }
    }
    for(std::vector<std::list<MyBrushData> >::iterator it = mipmapBrushes.begin();
        it != mipmapBrushes.end();
        it++)
    {
        std::list<MyBrushData>::iterator brushIt;
        for(brushIt = it->begin() ; brushIt != it->end(); brushIt++)
        {
            ppm_kill(&brushIt->brush);
        }
    }
    g_free (brushes);
    g_free (destBrushes);
    g_free (shadows);
    g_free (brushes_sum);
    
    g_free (xpos);
    g_free (ypos);
    ppm_kill (p);
    ppm_kill(&tmp);
    p->width = tmp.width;
    p->height = tmp.height;
    p->col = tmp.col;
    clearQuadTree(rootQuadTree);
    clearGrayPaintArea();
    if (img_has_alpha)
    {
        ppm_kill (a);
        a->width = atmp.width;
        a->height = atmp.height;
        a->col = atmp.col;
    }
    
    relief = runningvals.paper_relief / 100.0;
    if (relief > 0.001)
    {
        scale = runningvals.paper_scale / 100.0;
        
        if (PPM_IS_INITED (&paper_ppm))
        {
            tmp = paper_ppm;
            paper_ppm.col = NULL;
        }
        else
        {
            tmp.col = NULL;
            ppm_load (runningvals.selected_paper, &tmp);
            resize (&tmp, tmp.width * scale, tmp.height * scale);
            if (runningvals.paper_invert)
                ppm_apply_gamma (&tmp, -1.0, 1,1,1);
        }
        for (x = 0; x < p->width; x++)
        {
            double h, v;
            int    px = x % tmp.width, py;
            
            for (y = 0; y < p->height; y++)
            {
                int k = y * p->width * 3 + x * 3;
                
                py = y % tmp.height;
                if (runningvals.paper_overlay)
                    h = (tmp.col[py * tmp.width * 3 + px * 3]-128) * relief;
                else
                    h = (tmp.col[py * tmp.width * 3 + px * 3] -
                         (int)tmp.col[((py + 1) % tmp.height) * tmp.width * 3 +
                                      ((px + 1) % tmp.width) * 3]) /
                    -2.0 * relief;
                if (h <= 0.0)
                {
                    v = 1.0 + h/128.0;
                    if (v < 0.0)
                        v = 0.0;
                    else if (v > 1.0)
                        v = 1.0;
                    p->col[k+0] *= v;
                    p->col[k+1] *= v;
                    p->col[k+2] *= v;
                }
                else
                {
                    v = h/128.0;
                    if (v < 0.0)
                        v = 0.0;
                    else if (v > 1.0)
                        v = 1.0;
                    p->col[k+0] = p->col[k+0] * (1.0-v) + 255 * v;
                    p->col[k+1] = p->col[k+1] * (1.0-v) + 255 * v;
                    p->col[k+2] = p->col[k+2] * (1.0-v) + 255 * v;
                }
            }
        }
        ppm_kill (&tmp);
    }
    
    ppm_kill (&paper_ppm);
    ppm_kill (&dirmap);
    ppm_kill (&sizmap);
    if(rd.calculateOnEdge)
    {
        ppm_kill(&edgeDetectionMap);
    }
    if(rd.lastTime)
    {
        gGraySpanVector.clear();
    }
    running = 0;

}
