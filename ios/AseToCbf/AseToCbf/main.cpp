//
//  main.cpp
//  AseToCbf
//
//  Created by 陈勇 on 11-11-25.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#include <iostream>
#include "aselib.h"
#include "ObjLoader.h"
#include <OpenCL/OpenCL.h>
#include <list>
#include <string>
#include "stdio.h"
#include "SE_Log.h"
enum LOGO_STATE {NO_LOGO, HAS_LOGO, NO_CONCERN_LOGO};
struct Edge
{
    std::string pointName;
    std::string curveName;
    LOGO_STATE logoState;
    bool outGroup;
    int time;
    Edge()
    {
        logoState = NO_LOGO;
        outGroup = false;
        time = 0;
    }
};
struct Point
{
    std::string pointName;
    std::list<Edge> edges;
};
static void outputAnimationPath()
{
    Point p;
    Edge e;
    typedef std::list<Point> PointList;
    PointList mPointList;
    
    p.pointName = "S1";
    e.pointName = "L1";
    e.curveName = "LS1";
    e.time = 12;
    e.outGroup = false;
    e.logoState = NO_CONCERN_LOGO;
    p.edges.push_back(e);
    mPointList.push_back(p);
    
    p.edges.clear();
    p.pointName = "L1";
    e.pointName = "L2";
    e.curveName = "P1L";
    e.time = 12;
    e.outGroup = false;
    e.logoState = NO_CONCERN_LOGO;
    p.edges.push_back(e);
    mPointList.push_back(p);
    
    p.edges.clear();
    p.pointName = "R1";
    e.pointName = "R2";
    e.curveName = "P1R";
    e.time = 12;
    e.logoState = NO_LOGO;
    e.outGroup = false;
    p.edges.push_back(e);
    mPointList.push_back(p);
    
    p.edges.clear();
    p.pointName = "L2";
    e.pointName = "R1";
    e.curveName = "P2L_YZINVERSE";
    e.time = 12;
    e.logoState = NO_LOGO;
    e.outGroup = false;
    p.edges.push_back(e);
    
    e.pointName = "L1";
    e.curveName = "P1L";
    e.outGroup = true;
    e.logoState = NO_CONCERN_LOGO;
    e.time = 12;
    p.edges.push_back(e);
    
    e.pointName = "R1";
    e.curveName = "P2L";
    e.outGroup = true;
    e.logoState = NO_LOGO;
    e.time = 12;
    p.edges.push_back(e);
    
    e.pointName = "R1";
    e.curveName = "HP1R_XYINVERSE";
    e.logoState = HAS_LOGO;
    e.outGroup = false;
    e.time = 12;
    p.edges.push_back(e);
    mPointList.push_back(p);
    
    p.edges.clear();
    p.pointName = "R2";
    e.pointName = "R1";
    e.outGroup = true;
    e.logoState = NO_CONCERN_LOGO;
    e.curveName = "P1R";
    e.time = 12;
    p.edges.push_back(e);
    e.pointName = "L1";
    e.outGroup = true;
    e.logoState = NO_LOGO;
    e.curveName = "P2R";
    e.time = 12;
    p.edges.push_back(e);
    e.pointName = "L1";
    e.outGroup = false;
    e.logoState = NO_LOGO;
    e.curveName = "P2R_YZINVERSE";
    e.time = 12;
    p.edges.push_back(e);
    e.pointName = "L1";
    e.logoState = HAS_LOGO;
    e.outGroup = false;
    e.curveName = "HP1L_XYINVERSE";
    e.time = 12;
    p.edges.push_back(e);
    mPointList.push_back(p);
    const char* outputFile = "/Users/chenyong/backup/animation_path.txt";
    FILE* output = fopen(outputFile, "w");
    if(output == NULL)
    {
        LOGI("## write animation path error ##\n");
        return;
    }
    PointList::iterator it;
    for(it = mPointList.begin(); it != mPointList.end() ; it++)
    {
        std::list<Edge>::iterator edgeIt;
        for(edgeIt = it->edges.begin() ; edgeIt != it->edges.end() ; edgeIt++)
        {
            fprintf(output, "%s    ", it->pointName.c_str());
            fprintf(output, "%s    ", edgeIt->pointName.c_str());
            fprintf(output, "%s            ", edgeIt->curveName.c_str());
            fprintf(output, "%d    ", edgeIt->time);
            fprintf(output, "%d    ", edgeIt->outGroup);
            fprintf(output, "%d    ", edgeIt->logoState);
            fprintf(output, "\n");
        }
    }
    LOGI("## output animation path ##\n");
}
int main (int argc, const char * argv[])
{
    /*
    cl_device_id device_id;
    int gpu = 0;
    int err = clGetDeviceIDs(NULL, gpu ? CL_DEVICE_TYPE_GPU : CL_DEVICE_TYPE_CPU, 1, &device_id, NULL);
    if(err != CL_SUCCESS)
    {
        std::cout << "can not create cl device" <<"\n";
        return 1;
    }
  */
    
    const char* inputFile = "/Users/chenyong/backup/NewPhotoFrame.ASE";
    const char* complementFile = "/Users/chenyong/backup/PhotoFrameComplement.ASE";
    const char* refenceFile = "/Users/chenyong/backup/reference.ASE";
    const char* trackPointFile = "/Users/chenyong/backup/tracklist.txt";
    const char* verticalTrackPointFile = "/Users/chenyong/backup/tracklist_vertical.txt";
    const char* lookpoittrackFile = "/Users/chenyong/backup/lookingpoint.txt";
    //const char* inputFile = "/Users/chenyong/backup/NewKola.ASE";
    const char* shaderFile = "/Users/chenyong/backup/shaderdefine.ASE";
    const char* outputFile = "/Users/chenyong/backup/photoframe.cbf";
    ASE_Loader loader(0, 0);
    loader.Load(inputFile);
    loader.Load(complementFile);
    loader.Load(shaderFile);
    loader.Load(refenceFile);
    loader.end();
    loader.LoadTrackPoint(trackPointFile);
    loader.LoadVerticalTrackPoint(verticalTrackPointFile);
    loader.LoadLookPointTrack(lookpoittrackFile);
    loader.Write(outputFile);
    outputAnimationPath();
    /*
    const char* inputFile = "/Users/chenyong/backup/MyFrame.obj";
    const char* inputMltFile = "/Users/chenyong/backup/MyFrame.mtl";
    const char* outputFile = "/Users/chenyong/backup/MyFrame.cbf";
    ObjLoader loader("/Users/chenyong/backup");
    loader.load(inputFile);
    loader.load(inputMltFile);
    loader.loadShader("default_shader", "/Users/chenyong/backup/shader/default_shader.vsh", "/Users/chenyong/backup/shader/default_shader.fsh");
    loader.write(outputFile);
     */
    // insert code here...
    std::cout << "Hello, World!\n";
     
    return 0;
}

