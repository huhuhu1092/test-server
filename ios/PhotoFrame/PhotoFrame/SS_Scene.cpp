//
//  SS_Scene.cpp
//  PhotoFrame
//
//  Created by 陈勇 on 11-11-26.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#include "SS_Scene.h"
#include "SS_OpenGL.h"
#include "SE_Camera.h"
#include "SE_Vector.h"
#include "SE_Matrix.h"
#include "SS_Shader.h"
#include "SS_Model.h"
#include "PGMDataReader.h"
#include "SE_Geometry3D.h"
#include "SE_Interpolate.h"
#include "SE_Curve.h"
#include "SE_Quat.h"
#include "SE_TimeProp.h"
#include <string>
#include <math.h>

struct DisplayObjectProperty
{
    char meshName[256];
    int vertexType;
    bool bNeedPolygonOffset;
    char shaderName[256];
    SE_Scene::USAGE usage;
    int displayObjectID;
};

#define FAR_DIST 60
#define NEAR_DIST 35
#define CAMERA_FAR_DIST 80
#define CAMERA_FIELD_OF_VIEW 80
#define CAMERA_RATIO (mScreenHeight / mScreenWidth)
const static char* worldObjName[] = {"BackWall","WallFrameH_001","FrameH_001","photoH001","WallFrameV_001","FrameV_001","photoV001","thespeedsunPlatform","logo001", "floordown001"};
static DisplayObjectProperty photoFrameH[] = {
    {"BackWall", SE_Mesh::XYZ_UV, false, "fog_shader", SE_Scene::NO_USE, 1001}, 
    {"WallFrameH_001", SE_Mesh::XYZ_UV, false, "fog_shader", SE_Scene::NO_USE, 1002}, 
    {"FrameH_001", SE_Mesh::XYZ_UV,false ,"fog_shader", SE_Scene::NO_USE, 1003}, 
    {"photoH001", SE_Mesh::XYZ_UV, true,"picture_fog_shader", SE_Scene::PICTURE_PLACE, 1004}
    //{"photoH001", SE_Mesh::XYZ_UV, true, "fog_shader", SE_Scene::NO_USE, 1004}
};
static DisplayObjectProperty photoFrameV[] = {
    {"BackWall", SE_Mesh::XYZ_UV, false,"fog_shader", SE_Scene::NO_USE, 1001}, 
    {"WallFrameV_001", SE_Mesh::XYZ_UV, false,"fog_shader", SE_Scene::NO_USE, 1005}, 
    {"FrameV_001", SE_Mesh::XYZ_UV, false,"fog_shader", SE_Scene::NO_USE, 1006}, 
    {"photoV001", SE_Mesh::XYZ_UV, true,"picture_fog_shader", SE_Scene::PICTURE_PLACE, 1007}
    //{"photoV001", SE_Mesh::XYZ_UV, true, "fog_shader", SE_Scene::NO_USE, 1007}
};
static DisplayObjectProperty stationObject[] = {
    {"thespeedsunPlatform", SE_Mesh::XYZ_UV, true,"fog_shader", SE_Scene::NO_USE, 1008}, 
    {"logo001", SE_Mesh::XYZ_UV, true,"fog_shader", SE_Scene::NO_USE, 1009}};

static DisplayObjectProperty staticObject[] = {
    {"floordown001", SE_Mesh::XYZ_UV, false,"fog_shader", SE_Scene::NO_USE, 2000}
};
static const char* frameLookingPointName = "lookingPoint_frame";
static const char* startLookingPointName = "lookingPoint_start";
#define HAS_PICTURE 0
#define HAS_NO_PICTURE 1
static const char* pictureShaderName[]= {"picture_fog_shader", "fog_shader"};
static int photoFrameHObjectNum = sizeof(photoFrameH) / sizeof(DisplayObjectProperty);
static int photoFrameVObjectNum = sizeof(photoFrameV) / sizeof(DisplayObjectProperty);
static int stationObjectNum = sizeof(stationObject) / sizeof(DisplayObjectProperty);
/////////
//photo H Frame
static float uvArrayH[4][8] = {
    {0, 0.875, 0, 0.125, 1, 0.125, 1, 0.875}, //0
    {1, 0.125, 1, 0.875, 0, 0.875, 0, 0.125}, //180
    {0.875, 1, 0.125, 1, 0.125, 0, 0.875, 0},  //90, ccw
    {0.125, 0, 0.875, 0, 0.875, 1, 0.125, 1} //90, cw
    
};
//photo V Frame
static float uvArrayV[4][8] = 
{
    {0.125, 0, 0.875, 0, 0.875, 1, 0.125, 1}, //0
    {0.875, 1, 0.125, 1, 0.125, 0, 0.875, 0}, //180
    {0, 0.875, 0, 0.125, 1, 0.125, 1, 0.875}, // 90, ccw
    {1, 0.125, 1, 0.875, 0, 0.875, 0, 0.125} //90, cw
    
};
//mirror photo H Frame
static float mirrorUvArrayH[4][8] = 
{
    {1, 0.875, 1, 0.125, 0, 0.125, 0, 0.875}, //0
    {0, 0.125, 0, 0.875, 1, 0.875, 1, 0.125}, //180
    {0.875, 0, 0.125, 0, 0.125, 1, 0.875, 1}, //90, ccw
    {0.125, 1, 0.875, 1, 0.875, 0, 0.125, 0} //90, cw
};
//mirror photo V Frame
static float mirrorUvArrayV[4][8] = 
{
    {0.875, 0, 0.125, 0, 0.125, 1, 0.875, 1}, //0
    {0.125, 1, 0.875, 1, 0.875, 0, 0.125, 0}, //180
    {0, 0.125, 0, 0.875, 1, 0.875, 1, 0.125}, //90, ccw
    {1, 0.875, 1, 0.125, 0, 0.125, 0, 0.875}, //90, cw
};
/////////

static bool isWorldObj(const char* name)
{
    int count = sizeof(worldObjName) / sizeof(const char*);
    for(int i = 0 ; i < count ; i++)
    {
        if(!strcmp(worldObjName[i], name))
            return true;
    }
    return false;
}
/*
class SE_Scene::SS_Node
{
public:
    enum CULLTYPE {CW, CCW};
    std::string name;
    SE_Matrix4f m;
    int meshIndex;
    CULLTYPE cullType;
    SS_Node()
    {
        m.identity();
        meshIndex = -1;
        cullType = CCW;
    }
};
 */
///
template <typename T>
typename std::list<T>::iterator se_list_nref(std::list<T>& data, size_t n)
{
    typename std::list<T>::iterator it = data.begin();
    size_t i = 0;
    while(it != data.end() && i < n)
    {
        it++;
        i++;
    }
    return it;
}
/////////////////
//////
struct SE_Scene::CameraMoveState
{
    float currentTime;
    bool moveStart;
    SE_Scene::CurveData currentCurve;
    float currentCurveTotalTime;
    std::vector<SE_LookingPointTrackData> currentLookPointTrackList;
    //set first
    std::vector<CameraPathProperty> cameraPath;
    int curvePathIndex;
    //for curve select
    bool leaveGroup; // current location is the last location in group, next we will leave this group
    std::string startLocationName; // current location name: L1, L2, R1, R2
    std::string endLocationName;
    bool leftFinished;
    bool rightFinished;
    bool bLeavingGroup;
    bool bStartCurve;
    float mCameraForwardingDist;
    SS_ModelManager::TRACK_LIST_FOR_PHOTO_TYPE mTrackListPhotoType;
    CameraMoveState()
    {
        currentTime = 0;
        moveStart = false;
        currentCurveTotalTime = 0;
        curvePathIndex = 0;
        leaveGroup = false;
        startLocationName = "L1";
        leftFinished = false;
        rightFinished = false;
        bLeavingGroup = false;
        mCameraForwardingDist = 0;
        mTrackListPhotoType = SS_ModelManager::HH;
        bStartCurve = false;
    }
    bool isLeaveGroup()
    {
        return leftFinished && rightFinished;
    }
    void checkLeftOrRightFinish()
    {
        if(startLocationName == "L1" && endLocationName == "L2")
        {
            leftFinished = true;
        }
        if(startLocationName == "R1" && endLocationName == "R2")
        {
            rightFinished = true;
        }
    }
    void clearLeftRightFinish()
    {
        leftFinished = rightFinished = false;
    }
    //float getCurrentCurveForwardingDist();
};
///////////////
static SE_Vector3f getExactLookPoint(std::vector<SE_LookingPointTrackData>& lookPointTrackList, float photoFrameBoundY, SS_ModelManager* modelManager,float t)
{
    SE_Vector3f lookingPoint;
    for(int i = 0 ; i < lookPointTrackList.size() ; i++)
    {
        if(fabs(t - (lookPointTrackList[i].percent / 100.0f)) < 0.000001)
        {
            lookingPoint = modelManager->getFrameLookingPoint(lookPointTrackList[i].lookpointname.c_str());
            lookingPoint.y += photoFrameBoundY * (lookPointTrackList[i].frameNum - 1);
            int side = lookPointTrackList[i].side;
            if(side == 0)
            {
                lookingPoint.x = -lookingPoint.x;
            }
            return lookingPoint;
        }
    }
    return lookingPoint;
} 

/////////////////
/*
float SE_Scene::CameraMoveState::getCurrentCurveForwardingDist()
{
    SE_Vector3f point, nextPoint;
    float step = 0 * mCameraMoveState->currentCurve.curveLen;
    float nextStep = 1 * mCameraMoveState->currentCurve.curveLen;
    mCameraMoveState->currentCurve.curve->setCurrentPoint(0);
    mCameraMoveState->currentCurve.curve->setForwardingStep(step);
    mCameraMoveState->currentCurve.curve->getNextCurvePoint(point);
    
    mCameraMoveState->currentCurve.curve->setCurrentPoint(0);
    mCameraMoveState->currentCurve.curve->setForwardingStep(nextStep);
    mCameraMoveState->currentCurve.curve->getNextCurvePoint(nextPoint);
    
    SE_Vector3f lookpoint = getExactLookPoint(0);
    SE_Vector3f nextLookPoint = getExactLookPoint(1);
    if(mCameraMoveState->bLeavingGroup == true)
    {
        nextLookPoint.y += mYLength;
        nextPoint.y += mYLength;
    }
    PhotoFrameNode* firstNode = getPhotoFrameNode(point, lookpoint);
    PhotoFrameNode* nextNode = getPhotoFrameNode(nextPoint, nextLookPoint);
    SS_ModelManager::TRACK_LIST_FOR_PHOTO_TYPE trackListForPhotoType = SS_ModelManager::HH;
    if(firstNode->photoType == PHOTOH && nextNode->photoType == PHOTOH)
    {
        trackListForPhotoType = SS_ModelManager::HH;
    }
    else if(firstNode->photoType == PHOTOH && nextNode->photoType == PHOTOV)
    {
        trackListForPhotoType = SS_ModelManager::HV;
    }
    else if(firstNode->photoType == PHOTOV && nextNode->photoType == PHOTOH)
    {
        trackListForPhotoType = SS_ModelManager::VH;
    }
    else if(firstNode->photoType == PHOTOV && nextNode->photoType == PHOTOV)
    {
        trackListForPhotoType = SS_ModelManager::VV;
    }
    else 
    {
        LOGI("error photo node\n");
        assert(0);
    }
    SE_TrackPoint trackPointAdjust = mModelManager->getTrackPointAdjust(getTrackPointListType(mViewType), trackListForPhotoType, mCameraMoveState->currentCurve.name.c_str());
    std::vector<SE_TrackPoint> trackPoints = mModelManager->getTrackPoints(getTrackPointListType(mViewType), trackListForPhotoType, mCameraMoveState->currentCurve.name.c_str());
    SE_TrackPoint lastTrackPoint = trackPoints[trackPoints.size() - 1];
    SE_TrackPoint forwardingTrackPoint(lastTrackPoint.x + trackPointAdjust.x, lastTrackPoint.y + trackPointAdjust.y , lastTrackPoint.z + trackPointAdjust.z);
    SE_Vector3f forwardingPoint = mModelManager->getTrackPoint(getTrackPointListType(mViewType), forwardingTrackPoint, mStartRef);
    float dist = nextPoint.x - forwardingPoint.x;
    LOGI("## current curve = %s , track list type = %d \n", mCameraMoveState->currentCurve.name.c_str(), trackListForPhotoType);
    LOGI("## dist = %f ##\n", dist);
    return dist;
    
}
*/
/*
/////////////
class SE_Scene::Scene
{
public:
    SE_Matrix4f translate;
    std::list<SE_Scene::SS_Node*> NodesList;
    NodeList nodeList;
};
 */
////////////////////////////
static void testInterpolate()
{
    SE_Vector2f v1(0, 0);
    SE_Vector2f v2(1, 1);
    SE_Vector2f s1(-5, 0);
    SE_Vector2f s2(0, -5);
    float t = 0;
    SE_HermiteInterpolate interpolate;
    SE_Vector4f interpolateX = interpolate.interpolate(0, 1, -5, 0);
    SE_Vector4f interpolateY = interpolate.interpolate(0, 1, 0, -5);
    for(int i = 0 ; i < 10 ; i++)
    {
        float x = interpolateX.x * t * t * t + interpolateX.y * t * t + interpolateX.z * t + interpolateX.w;
        float y = interpolateY.x * t * t * t + interpolateY.y * t * t + interpolateY.z * t + interpolateY.w;
        LOGI("## x = %f, y = %f ##\n", x, y);
        t += 0.1;
    }
}
SE_Scene::SE_Scene(SS_ModelManager* modelManager, void* vn,int w, int h, int sceneCount)
{
    mViewNav = vn;
    mCamera = new SE_Camera;
    mCameraMoveState = new CameraMoveState;
    mScreenWidth = w;
    mScreenHeight = h;
    mModelManager = modelManager;
    mMiniGroupNum = 5;
    //mTrackPointsIndex = -1;
    mRenderObjectCount = 0;
    mFogPlaneMesh = NULL;
    mYLength = 0;
    mCurrentGroupIndex = 0;
    mLastPictureIndex = -1;
    mIsDrawing = false;
    mCameraStayStatic = false;
    mCameraForwardingDist = 0;
    mTrackListPhotoType = SS_ModelManager::HH;
    mTimeForRotate = false;
    mRotateTime = 0;
    mRotateT = 0;
    mInitRendeState = false;
    mCurrentFadeInFrameIndex = 0;
    //mStartPictureIndex = 0;
    //mEndPictureIndex = 0;
    /*
    ///obsolete
    mBoundY = 0;
    mSceneCount = sceneCount;
    mSceneList.resize(sceneCount);
    //end
     */
    initMeshMap();
    initCamera();
    //initFog();
    initRootMatrix();
    initBound();
    initFog();
    createPointEdges();
}
void SE_Scene::setViewType(VIEW_TYPE t)
{
    mViewType = t;
    /*
    mCamera->create(SE_Vector3f(4, -7, 4),  SE_Vector3f(1, 0, 0), SE_Vector3f(0, 0, 1), SE_Vector3f(0, -1, 0), CAMERA_FIELD_OF_VIEW, CAMERA_RATIO, 1, CAMERA_FAR_DIST);
    if(t == LANDSCAPE)
    {
        mCamera->setViewport(0, 0, mScreenWidth, mScreenHeight);
    }
    else
    {
        mCamera->setViewport(0, 0, mScreenHeight, mScreenWidth);
    }
     */
}
SE_Scene::~SE_Scene()
{
    delete mCamera;
    delete mFogPlaneMesh;
    delete mCameraMoveState;
    std::list<TrackCurveData>::iterator it;
    for(it = mCurveData.begin(); it != mCurveData.end(); it++)
    {
        for(int i = 0 ; i < TRACK_LIST_PHOTO_NUM ; i++)
        {
            delete it->curveData[i].curve;
        }
    }
}
/*
void SE_Scene::incTrackPointIndex()
{
    mTrackPointsIndex++;
    size_t totalSize = mTrackPoints1.size() + mTrackPoints2.size();
    if(mTrackPointsIndex > totalSize)
        mTrackPointsIndex = 0;
}
 */
void SE_Scene::initCamera()
{
    //mCamera->create(SE_Vector3f(4, -7, 4),  SE_Vector3f(0, 0, 1), SE_Vector3f(-1, 0, 0), SE_Vector3f(0, -1, 0), 45, mScreenHeight/ mScreenWidth, 1, 500);
    mCamera->create(SE_Vector3f(4, -7, 4),  SE_Vector3f(1, 0, 0), SE_Vector3f(0, 0, 1), SE_Vector3f(0, -1, 0), CAMERA_FIELD_OF_VIEW, CAMERA_RATIO, 1, CAMERA_FAR_DIST);
    mCamera->setViewport(0, 0, mScreenWidth, mScreenHeight);
}
void SE_Scene::initFog()
{
    /*
    SE_GeometryData geomData = SE_GeometryData::createZAlignRect(10, 10, -35);
    mModelManager->addGeometryData(geomData);
    mFogPlaneMesh = new SE_Mesh;
    mFogPlaneMesh->geomDataIndex = mModelManager->getGeometryDataNum() - 1;
     */
    mFirstFogPoint = SE_Vector3f(0, 0, 0.5 * (mPhotoFrameBoundMax.z + mPhotoFrameBoundMin.z));
    mCurrentFogPoint = mFirstFogPoint;
}
void SE_Scene::initMeshVBO()
{
    int meshnum = mModelManager->getMeshNum();
    for(int i = 0 ; i < meshnum ; i++)
    {
        SE_Mesh* mesh = mModelManager->getMesh(i);
        float* data = mesh->getDrawingVertex(SE_Mesh::XYZ_UV);
        createMeshVBO(mesh);
    }
    mFogPlaneMesh->getDrawingVertex(SE_Mesh::XYZ_COLOR);
    createMeshVBO(mFogPlaneMesh);
}
void SE_Scene::initRootMatrix()
{
    SE_Matrix4f rotateM;
    rotateM.identity();
    
    //SE_Matrix3f rotate3f;
    //rotate3f.setRotateX(90);
    //rotateM.set(rotate3f, SE_Vector3f(1, 1,1), SE_Vector3f(0, 0, 0));
    mRootNodeMatrix = rotateM;
}
void SE_Scene::initBound()
{
    SE_Vector3f min(1000, 1000, 1000);
    SE_Vector3f max(-1000, -1000, -1000);
    for(int i = 0 ; i < photoFrameVObjectNum ; i++)
    {
        DisplayObjectProperty dop = photoFrameV[i];
        SE_Mesh* mesh = mModelManager->getMesh(mMeshMap[dop.meshName]);
        SE_GeometryData* gd = mModelManager->getGeometryData(mesh->geomDataIndex);
        int vertexNum = gd->vertexNum;
        SE_Vector3f* vertexArray = gd->vertexArray;
        for(int i = 0 ; i < vertexNum ; i++)
        {
            if(vertexArray[i].x < min.x)
                min.x = vertexArray[i].x;
            if(vertexArray[i].y < min.y)
                min.y = vertexArray[i].y;
            if(vertexArray[i].z < min.z)
                min.z = vertexArray[i].z;
            if(vertexArray[i].x > max.x)
                max.x = vertexArray[i].x;
            if(vertexArray[i].y > max.y)
                max.y = vertexArray[i].y;
            if(vertexArray[i].z > max.z)
                max.z = vertexArray[i].z;
        }
    }
    mPhotoFrameBoundMax = max;
    mPhotoFrameBoundMin = min;
    
    SE_Mesh* mesh = mModelManager->getMesh(mMeshMap["floordown001"]);
    if(mesh)
    {
        SE_Vector3f min(1000, 1000, 1000);
        SE_Vector3f max(-1000, -1000, -1000);
        SE_GeometryData* gd = mModelManager->getGeometryData(mesh->geomDataIndex);
        int vertexNum = gd->vertexNum;
        SE_Vector3f* vertexArray = gd->vertexArray;
        for(int i = 0 ; i < vertexNum ; i++)
        {
            if(vertexArray[i].x < min.x)
                min.x = vertexArray[i].x;
            if(vertexArray[i].y < min.y)
                min.y = vertexArray[i].y;
            if(vertexArray[i].z < min.z)
                min.z = vertexArray[i].z;
            if(vertexArray[i].x > max.x)
                max.x = vertexArray[i].x;
            if(vertexArray[i].y > max.y)
                max.y = vertexArray[i].y;
            if(vertexArray[i].z > max.z)
                max.z = vertexArray[i].z;
        }
        mFloorDownMax = max;
        mFloorDownMin = min;
    }
    mYLength = mFloorDownMax.y - mFloorDownMin.y;
    //float tmpLen = mPhotoFrameBoundMax.y - mPhotoFrameBoundMin.y;
    SE_Vector3f referenceBoxMin = mModelManager->getReferenceBoxMin();
    SE_Vector3f referenceBoxMax = mModelManager->getReferenceBoxMax();
    SE_Vector3f referenceBoxBound = referenceBoxMax - referenceBoxMin;
    float pshift = mFloorDownMax.y - mFloorDownMin.y;
    pshift = (referenceBoxBound.y - pshift) / 2;
    SE_Vector3f startRef;
    startRef.x = referenceBoxMin.x;
    startRef.y = referenceBoxMax.y + pshift;
    startRef.z = referenceBoxMax.z;
    mStartRef = startRef;
}
void SE_Scene::initMeshMap()
{
    int meshnum = mModelManager->getMeshNum();
    for(int i = 0 ; i < meshnum ; i++)
    {
        SE_Mesh* mesh = mModelManager->getMesh(i);
        if(isWorldObj(mesh->name.c_str()))
        {
            mMeshMap[mesh->name] = i;
        }
    }
}
void SE_Scene::removeAllMeshVBO()
{
    int meshnum = mModelManager->getMeshNum();
    for(int i = 0 ; i < meshnum ; i++)
    {
        SE_Mesh* mesh = mModelManager->getMesh(i);
        mesh->removeGLBuffer();
    }
}
SE_Scene::PhotoFrameNode SE_Scene::createPhotoFrameNode(PHOTO_TYPE pt, int pictureIndex)
{
    int count = 0; 
    PhotoFrameNode pfn;
    pfn.photoType = pt;
    DisplayObjectProperty* data = NULL;
    if(pt == PHOTOV)
    {
        count = sizeof(photoFrameV) / sizeof(DisplayObjectProperty);
        data = photoFrameV;
    }
    else if(pt == PHOTOH)
    {
        count = sizeof(photoFrameH) / sizeof(DisplayObjectProperty);
        data = photoFrameH;
    }
    SE_AABB aabb(SE_Vector3f(1000, 1000, 1000), SE_Vector3f(-1000, -1000, -1000));
    for(int i = 0 ; i < count ; i++)
    {
        DisplayObjectProperty dop = data[i];
        DisplayObject obj;
        obj.meshIndex = mMeshMap[dop.meshName];
        obj.vertexType = dop.vertexType;
        obj.shaderName = dop.shaderName;
        obj.bPicturePlace = dop.usage == SE_Scene::PICTURE_PLACE;
        obj.objID = dop.displayObjectID;
        obj.bNeedPolygonOffset = dop.bNeedPolygonOffset;
        if(obj.bPicturePlace)
        {
            if(pictureIndex != -1)
            {
                obj.pictureIndex = pictureIndex;
                obj.shaderName = pictureShaderName[HAS_PICTURE];
                SE_Mesh* mesh = mModelManager->getMesh(obj.meshIndex);
                SE_GeometryData* gd = mModelManager->getGeometryData(mesh->geomDataIndex);
                int j = 0;
                for(int i = 0 ;i < gd->texVertexNum ; i++)
                {
                    obj.uv[j++] = gd->texVertexArray2[i].x;
                    obj.uv[j++] = gd->texVertexArray2[i].y;
                }
            }
            else
            {
                obj.shaderName = pictureShaderName[HAS_NO_PICTURE];
                obj.bPicturePlace = false;
            }
        }
        SE_Mesh* mesh = mModelManager->getMesh(obj.meshIndex);
        SE_AABB meshAABB = mesh->getAABB();
        aabb = aabb.unionAABB(meshAABB);
        pfn.objList.push_back(obj);
    }
    pfn.aabb = aabb;
    return pfn;
    
}
bool SE_Scene::hasStation(int groupIndex)
{
    GroupList::iterator it = se_list_nref(mGroupList, groupIndex);
    if(it->hasStationNode)
        return true;
    else
        return false;
}
SE_Scene::EdgeList SE_Scene::getEdgeList(const std::string& point)
{
    PointList::iterator it;
    for(it = mPointList.begin() ; it != mPointList.end(); it++)
    {
        if(it->pointName == point)
            return it->edges;
    }
    EdgeList el;
    return el;
}
SE_Scene::PointCurveData SE_Scene::calculatePointCurve(const std::string& point, bool leaveGroup, LOGO_STATE logoState)
{
    EdgeList edges = getEdgeList(point);
    if(edges.size() == 1)
    {
        EdgeList::iterator edgeIt = se_list_nref(edges, 0);
        return PointCurveData(edgeIt->pointName, edgeIt->curveName, edgeIt->time);
    }
    EdgeList::iterator edgeIt;
    EdgeList edgesSameLeaveGroup;
    for(edgeIt = edges.begin() ; edgeIt != edges.end() ; edgeIt++)
    {
        if(edgeIt->outGroup == leaveGroup)
        {
            edgesSameLeaveGroup.push_back(*edgeIt);
        }
    }
    if(edgesSameLeaveGroup.size() == 1)
    {
        EdgeList::iterator it = se_list_nref(edgesSameLeaveGroup, 0);
        return PointCurveData(it->pointName, it->curveName, it->time);
    }
    EdgeList edgesSameHasStation;
    for(edgeIt = edgesSameLeaveGroup.begin() ; edgeIt != edgesSameLeaveGroup.end() ; edgeIt++)
    {
        
        if(edgeIt->logoState == logoState || edgeIt->logoState == NO_CONCERN_LOGO)
        {
            edgesSameHasStation.push_back(*edgeIt);
        }
    }
    if(edgesSameHasStation.size() == 1)
    {
        EdgeList::iterator it = se_list_nref(edgesSameHasStation, 0);
        return PointCurveData(it->pointName, it->curveName, it->time);
    }
    int num = edgesSameHasStation.size();
    assert(num > 0);
    int index = rand() % num;
    EdgeList::iterator it = se_list_nref(edgesSameHasStation, index);
    return PointCurveData(it->pointName, it->curveName, it->time);
}
SE_Scene::StaticNode SE_Scene::createStaticNode(float nodey, STATIC_NODE_TYPE staticType)
{
    int count = sizeof(staticObject) / sizeof(DisplayObjectProperty);
    StaticNode sn;
    SE_AABB aabb(SE_Vector3f(1000, 1000, 1000), SE_Vector3f(-1000, -1000, -1000));
    for(int i = 0 ; i < count ; i++)
    {
        DisplayObjectProperty dop = staticObject[i];
        DisplayObject obj;
        obj.meshIndex = mMeshMap[dop.meshName];
        obj.vertexType = dop.vertexType;
        obj.shaderName = dop.shaderName;
        obj.bNeedPolygonOffset = dop.bNeedPolygonOffset;
        obj.bPicturePlace = dop.usage == SE_Scene::PICTURE_PLACE;
        obj.objID = dop.displayObjectID;
        sn.objList.push_back(obj);
        SE_Mesh* mesh = mModelManager->getMesh(obj.meshIndex);
        SE_AABB meshAABB = mesh->getAABB();
        aabb = aabb.unionAABB(meshAABB);
    }   
    sn.aabb = aabb;
    sn.type = staticType;
    sn.worldM.identity();
    //float nodey = calculateStaticNodeY(groupIndex, staticType);
    sn.worldM.setColumn(3, SE_Vector4f(0, nodey, 0, 1));
    return sn;
}
float SE_Scene::calculateStationNodeY(int groupIndex)
{
    float span = 2 * mYLength;
    float starty = groupIndex * span;
    return starty;
}
SE_Scene::StationNode SE_Scene::createStationNode(float nodey)
{
    int count = sizeof(stationObject) / sizeof(DisplayObjectProperty);
    StationNode sn;
    SE_AABB aabb(SE_Vector3f(1000, 1000, 1000), SE_Vector3f(-1000, -1000, -1000));
    for(int i = 0 ; i < count ; i++)
    {
        DisplayObjectProperty dop = stationObject[i];
        DisplayObject obj;
        obj.bNeedPolygonOffset = dop.bNeedPolygonOffset;
        obj.meshIndex = mMeshMap[dop.meshName];
        obj.vertexType = dop.vertexType;
        obj.shaderName = dop.shaderName;
        obj.bPicturePlace = dop.usage == SE_Scene::PICTURE_PLACE;
        obj.objID = dop.displayObjectID;
        sn.objList.push_back(obj);
        SE_Mesh* mesh = mModelManager->getMesh(obj.meshIndex);
        SE_AABB meshAABB = mesh->getAABB();
        aabb = aabb.unionAABB(meshAABB);
    }   
    sn.aabb = aabb;
    SE_Matrix4f base;
    base.identity();
    //float span = 2 * mYLength;
    //float starty = groupIndex * span;
    base.setColumn(0, SE_Vector4f(1, 0, 0, 0));
    base.setColumn(1, SE_Vector4f(0, 1, 0, 0));
    base.setColumn(2, SE_Vector4f(0, 0, 1, 0));
    base.setColumn(3, SE_Vector4f(0, nodey, 0, 1));
    sn.worldM = base;
    sn.worldM = mRootNodeMatrix.mul(sn.worldM);
    return sn;
}
float SE_Scene::calculateStaticNodeY(int groupIndex, STATIC_NODE_TYPE staticType)
{
    float nodey = 0;
    switch (staticType) {
        case DOWN:
            nodey = groupIndex * mYLength * 2;
            break;
        case UP:
            nodey = groupIndex * mYLength * 2 + mYLength;
            break;
        default:
            break;
    }
    return nodey;
}
float SE_Scene::calculatePhotoFrameNodeY(int groupIndex, PHOTO_FRAME_NODE_TYPE nodeType)
{
    float nodey = 0;
    switch(nodeType)
    {
    case SW:
        nodey = groupIndex * mYLength * 2;
            break;
    case NW:
        nodey = groupIndex * mYLength * 2 + mYLength;
            break;
    case SE:
        nodey = groupIndex * mYLength * 2;
            break;
    case NE:
        nodey = groupIndex * mYLength * 2 + mYLength;
            break;
    case INVALID_PHOTO_FRAME_NODE_TYPE:
            nodey = 0;
            break;
    }
    return nodey;
}
int SE_Scene::calculatePhotoFramePictureIndex(int groupIndex, PHOTO_FRAME_NODE_TYPE nodeType)
{
    int pictureNum = mPictureIDVector.size();
    int pictureIndex = -1;
    switch (nodeType) {
        case SW:
            pictureIndex = (pictureNum == 0) ? -1 : ((groupIndex * GROUP_OBJ_NUM) % pictureNum);
            break;
        case NW:
            pictureIndex = (pictureNum == 0) ? -1 : ((groupIndex * GROUP_OBJ_NUM + 1) % pictureNum);
            break;
        case SE:
            pictureIndex = (pictureNum == 0) ? -1 : ((groupIndex * GROUP_OBJ_NUM + 2) % pictureNum);
            break;
        case NE:
            pictureIndex = (pictureNum == 0) ? -1 : ((groupIndex * GROUP_OBJ_NUM + 3) % pictureNum);
            break;
        case INVALID_PHOTO_FRAME_NODE_TYPE:
            assert(0);
            pictureIndex = -1;
            break;
        default:
            break;
    }
    return pictureIndex;
}
bool SE_Scene::isPhotoFrameNodeMirror(PHOTO_FRAME_NODE_TYPE nodeType) const
{
    if(nodeType == SW || nodeType == NW)
        return true;
    else if(nodeType == SE || nodeType == NE)
        return false;
    else
    {
        assert(0);
        return false;
    }
}
void SE_Scene::createPhotoFrameNodeForNegativeSide()
{
    
}
SE_Scene::PhotoFrameNode SE_Scene::createPhotoFrameNode(float nodey, PHOTO_FRAME_NODE_TYPE nodeType, int op , int& lastPictureIndex)
{
    if(mPictureIDVector.size() > 0)
    {
        if(op == 0)
        {
            lastPictureIndex++;
            lastPictureIndex = lastPictureIndex % mPictureIDVector.size();
        }
        else 
        {
            lastPictureIndex--;
            if(lastPictureIndex < 0)
            {
                lastPictureIndex = mPictureIDVector.size() - 1;
            }
            lastPictureIndex = lastPictureIndex % mPictureIDVector.size();
        }
    }
    else 
    {
        lastPictureIndex = -1;
    }
    int pictureIndex = lastPictureIndex;//calculatePhotoFramePictureIndex(groupIndex, nodeType);
    LOGI("## create picture index = %d ##\n", pictureIndex);
    bool mirror = isPhotoFrameNodeMirror(nodeType);
    SE_Matrix3f baseM;
    baseM.identity();
    if(mirror)
    {
        baseM.set(0, 0, -1);
    }
    //float nodey = calculatePhotoFrameNodeY(groupIndex, nodeType);
    SE_Vector3f translate(0, nodey, 0);
    if(pictureIndex == -1)
    {
        LOGI("## picture index= -1 #");
        PhotoFrameNode pfn = createPhotoFrameNode(PHOTOH, pictureIndex);
        pfn.mirror = mirror;
        pfn.type = nodeType;
        pfn.worldM.set(baseM, SE_Vector3f(1, 1, 1), translate);
        pfn.worldM = mRootNodeMatrix.mul(pfn.worldM);
        pfn.pictureIndex = pictureIndex;
        return pfn;
    }
    else
    {
        PHOTO_TYPE pt = mPictureIDVector[pictureIndex].photoType;
        LOGI("## photo type = %d ##\n", pt);
        PhotoFrameNode pfn = createPhotoFrameNode(pt, pictureIndex);
        pfn.mirror = mirror;
        pfn.type = nodeType;
        pfn.worldM.set(baseM, SE_Vector3f(1, 1, 1), translate);
        pfn.worldM = mRootNodeMatrix.mul(pfn.worldM);
        pfn.pictureIndex = pictureIndex;
        return pfn;
    }
}
int SE_Scene::calculateNegativeGroup(std::vector<PictureID>& pictures)
{
    return calculateGroup(pictures, CAMERA_FAR_DIST);
}
int SE_Scene::calculateGroup(std::vector<PictureID>& pictures, float maxDist)
{
    int pictureCount = pictures.size();
    int groupNum = pictureCount / 4;
    int reminder = pictureCount % 4;
    if(reminder > 0)
        groupNum++;
    const float groupYLen = mYLength * 2;
    float totalGroupYLen = groupNum * groupYLen;
    if(totalGroupYLen < maxDist)
    {
        float dist = maxDist - totalGroupYLen;
        int gpCount = (int)ceilf(dist / groupYLen);
        groupNum += gpCount;
    }
    else
    {
        totalGroupYLen = maxDist;
        int gpCount = (int)ceilf(totalGroupYLen / groupYLen);
        groupNum = gpCount;
    }
    return groupNum;
}
int SE_Scene::calculateGroup(std::vector<PictureID>& pictures)
{
    return calculateGroup(pictures, 2 * CAMERA_FAR_DIST);
}
int SE_Scene::getGroupNum(int pictureNum)
{
    int groupNum = calculateGroup(mPictureIDVector);
    return groupNum;
}

void SE_Scene::create(std::vector<PictureID>& pictureIDs, int pictureNum)
{
    mPictureIDVector = pictureIDs;
    mGroupNum = calculateGroup(pictureIDs);
    int negativeGroupNum = calculateNegativeGroup(pictureIDs);
    std::list<int> stationIndexList;
    /*
    int sectionNum = (int)ceilf(mGroupNum / (float)mMiniGroupNum);
    for(int i = 0 ; i < sectionNum ; i++)
    {
        int groupIndex = rand() % mMiniGroupNum;
        groupIndex += i * mMiniGroupNum;
        if(groupIndex > 0)
        {
            //for test
            //groupIndex = 4;
            //
            stationIndexList.push_back(groupIndex);
        }
    }
     */
    int lastPictureIndex = -1;
    for(int i = 0 ; i < mGroupNum ; i++)
    {
        Group group;
        group.isValid = true;
        float nodey = calculatePhotoFrameNodeY(i, SW);
        group.photoFrameNode[SW] = createPhotoFrameNode(nodey, SW, 0, lastPictureIndex);
        
        nodey = calculatePhotoFrameNodeY(i, SE);
        group.photoFrameNode[SE] = createPhotoFrameNode(nodey, SE, 0, lastPictureIndex);
        
        nodey = calculatePhotoFrameNodeY(i, NW);
        group.photoFrameNode[NW] = createPhotoFrameNode(nodey, NW, 0, lastPictureIndex);
        nodey = calculatePhotoFrameNodeY(i, NE);
        group.photoFrameNode[NE] = createPhotoFrameNode(nodey, NE, 0 , lastPictureIndex);
        nodey = calculateStaticNodeY(i, DOWN);
        group.staticNode[DOWN] = createStaticNode(nodey, DOWN);
        nodey = calculateStaticNodeY(i, UP);
        group.staticNode[UP] = createStaticNode(nodey, UP);
        std::list<int>::iterator it = std::find(stationIndexList.begin(), stationIndexList.end(), i);
        if(it != stationIndexList.end())
        {
            float nodey = calculateStationNodeY(i);
            group.stationNode = createStationNode(nodey);
            group.hasStationNode = true;
        }
        mGroupList.push_back(group);
    }
    mLastPictureIndex = lastPictureIndex;
    lastPictureIndex = mPictureIDVector.size();
    int groupIndex = -1;
    for(int i = 0 ; i < negativeGroupNum ; i++)
    {
        Group group;
        group.isValid = true;
        float nodey = calculatePhotoFrameNodeY(groupIndex, NE);
        group.photoFrameNode[NE] = createPhotoFrameNode(nodey, NE, 1, lastPictureIndex);
        
        nodey = calculatePhotoFrameNodeY(groupIndex, NW);
        group.photoFrameNode[NW] = createPhotoFrameNode(nodey, NW, 1, lastPictureIndex);
        
        nodey = calculatePhotoFrameNodeY(groupIndex, SE);
        group.photoFrameNode[SE] = createPhotoFrameNode(nodey, SE, 1, lastPictureIndex);
        
        nodey = calculatePhotoFrameNodeY(groupIndex, SW);
        group.photoFrameNode[SW] = createPhotoFrameNode(nodey, SW, 1, lastPictureIndex);
        
        nodey = calculateStaticNodeY(groupIndex, DOWN);
        group.staticNode[DOWN] = createStaticNode(nodey, DOWN);
        
        nodey = calculateStaticNodeY(groupIndex, UP);
        group.staticNode[UP] = createStaticNode(nodey, UP);
        mNegativeGroupList.push_back(group);
        groupIndex--;
    }
    mCurrentLookingPhotoFrameNode.prevGroupIt = mGroupList.end();
    mCurrentLookingPhotoFrameNode.currGroupIt = mGroupList.end();
}
static bool pictureSort(SE_Scene::PhotoFrameNode& left, SE_Scene::PhotoFrameNode& right)
{
    SE_Scene::DisplayObjectList::iterator itLeft = se_list_nref(left.objList, 0);
    SE_Scene::DisplayObjectList::iterator itRight = se_list_nref(right.objList, 0);
    if(itLeft->pictureIndex < itRight->pictureIndex)
    {
        return true;
    }
    else 
    {
        return false;
    }
}
void SE_Scene::sortPictureByID()
{
    mPictureObjectList.sort(pictureSort);
    /*
    PhotoFrameNodeList::iterator it;
    LOGI("\n");
    for(it = mPictureObjectList.begin() ; it != mPictureObjectList.end() ; it++)
    {
        DisplayObjectList::iterator objIt = se_list_nref(it->objList, 0);
        LOGI(" %d ", objIt->pictureIndex);
    }
    LOGI("\n");
     */
}
static bool isPicture(SE_Scene::DisplayObject& dop)
{
    return dop.bPicturePlace;
}
void SE_Scene::seperatePictureFromPhotoFrameNode()
{
    PhotoFrameNodeList::iterator it;
    for(it = mMirrorPhotoFrameNodeList.begin() ; it != mMirrorPhotoFrameNodeList.end() ; it++)
    {
        DisplayObjectList::iterator objIt;
        for(objIt = it->objList.begin() ; objIt != it->objList.end() ; objIt++)
        {
            if(objIt->bPicturePlace)
            {
                PhotoFrameNode pfn = it->copyProperty();
                DisplayObject dlo = *objIt;
                pfn.objList.push_back(dlo);
                mPictureObjectList.push_back(pfn);
            }
        }
        it->objList.remove_if(isPicture);
        //LOGI("## obj num = %lu ##\n", it->objList.size());
    }
    for(it = mPhotoFrameNodeList.begin() ; it != mPhotoFrameNodeList.end() ; it++)
    {
        DisplayObjectList::iterator objIt;
        for(objIt = it->objList.begin() ; objIt != it->objList.end() ; objIt++)
        {
            if(objIt->bPicturePlace)
            {
                PhotoFrameNode pfn = it->copyProperty();
                pfn.objList.push_back(*objIt);
                mPictureObjectList.push_back(pfn);
            }
        }
        it->objList.remove_if(isPicture);
        //LOGI("## obj num = %lu ##\n", it->objList.size());
    }
    int count = mPictureObjectList.size();
    //LOGI("## picture num = %d ##\n", count);
}
void SE_Scene::createDisplayObjectListForRender(PhotoFrameNode& pfn)
{
    DisplayObjectList::iterator objIt;
    for(objIt = pfn.objList.begin() ; objIt != pfn.objList.end(); objIt++)
    {
        objIt->mirror = pfn.mirror;
        objIt->worldM = pfn.worldM;
        mDisplayObjectList.push_back(*objIt);
    }
}
void SE_Scene::createDisplayObjectListForRender(StaticNode& sn)
{
    DisplayObjectList::iterator objIt;
    for(objIt = sn.objList.begin(); objIt != sn.objList.end() ; objIt++)
    {
        objIt->worldM = sn.worldM;
        mDisplayObjectList.push_back(*objIt);
    }
}
void SE_Scene::createDisplayObjectListForRender(StationNode& sn)
{
    DisplayObjectList::iterator objIt;
    for(objIt = sn.objList.begin(); objIt != sn.objList.end() ; objIt++)
    {
        objIt->worldM = sn.worldM;
        mDisplayObjectList.push_back(*objIt);
    }
}
static bool compareDisplayObjectID(const SE_Scene::DisplayObject& left, const SE_Scene::DisplayObject& right)
{
    return left.objID < right.objID;
}
void SE_Scene::sortDisplayObjectList()
{
    mDisplayObjectList.sort(compareDisplayObjectID);
    /*
    LOGI("\n");
    for(DisplayObjectList::iterator it = mDisplayObjectList.begin() ; it != mDisplayObjectList.end(); it++)
    {
        LOGI("%d ", it->objID);
    }
    LOGI("\n");
     */
}
void SE_Scene::setStaticNodeDrawState(GroupList::iterator begin, GroupList::iterator end, bool needDraw)
{
    GroupList::iterator it;
    for(it = begin; it != end; it++)
    {
        //if(it->hasStationNode)
        {
            it->staticNode[0].needDraw = needDraw;
            it->staticNode[1].needDraw = needDraw;
        }
    }
}
void SE_Scene::createRenderNode(GroupList::iterator begin, GroupList::iterator end)
{
    GroupList::iterator it;
    //int num = 3;
    for(it = begin ; it != end ; it++)
    {
        for(int i = 0 ;i  < 4 ; i++)
        {
            if(isRender(&it->photoFrameNode[i]))
            {
                if(it->photoFrameNode[i].mirror)
                {
                    mMirrorPhotoFrameNodeList.push_back(it->photoFrameNode[i]);
                    createDisplayObjectListForRender(it->photoFrameNode[i]);
                }
                else
                {
                    mPhotoFrameNodeList.push_back(it->photoFrameNode[i]);
                    createDisplayObjectListForRender(it->photoFrameNode[i]);
                }
            }
        }
        if(it->staticNode[0].needDraw && isRender(&it->staticNode[0]))
        {
            mStaticNodeList.push_back(it->staticNode[0]);
            createDisplayObjectListForRender(it->staticNode[0]);
        }
        if(it->staticNode[1].needDraw && isRender(&it->staticNode[1]))
        {
            mStaticNodeList.push_back(it->staticNode[1]);
            createDisplayObjectListForRender(it->staticNode[1]);
        }
        
        if(it->hasStationNode)
        {
            if(isRender(&it->stationNode))
            {
                mStationNodeList.push_back(it->stationNode);
                createDisplayObjectListForRender(it->stationNode);
            }
        }
        
    }

}
void SE_Scene::createRenderNode()
{    
    setStaticNodeDrawState(mGroupList.begin(), mGroupList.end(), true);
    setStaticNodeDrawState(mNegativeGroupList.begin(), mNegativeGroupList.end(), true);
    createRenderNode(mGroupList.begin(), mGroupList.end());
    createRenderNode(mNegativeGroupList.begin(), mNegativeGroupList.end());
    //seperatePictureFromPhotoFrameNode();
    //sortPictureByID();
    sortDisplayObjectList();
    //LOGI("## photo frame num = %lu\n", mPhotoFrameNodeList.size());
    //LOGI("## mirror photo frame num = %lu\n", mMirrorPhotoFrameNodeList.size());
}
void SE_Scene::clearRenderNode()
{
    mDisplayObjectList.clear();
    mPictureObjectList.clear();
    mMirrorPhotoFrameNodeList.clear();
    mPhotoFrameNodeList.clear();
    mStaticNodeList.clear();
    mStationNodeList.clear();
}
//static int renderstate = 0;
void SE_Scene::renderScene()
{
    mRenderObjectCount = 0;
    double starttime = getCurrentTime();
    createRenderNode();
    double endtime = getCurrentTime();
    double span = getInterval(starttime, endtime);
    //LOGI("createRenderNode = %f\n", span);
    //glClearColor(1, 1, 1, 1);
    //checkGLError();
    //glClear(GL_COLOR_BUFFER_BIT);
    //checkGLError();
    glClearColor(1.0f, 1.0f, 1.0f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
    if(!mInitRendeState)
    {
        glEnable(GL_DEPTH_TEST);
        checkGLError();
        glDepthFunc(GL_LEQUAL);
        checkGLError();
        glDisable(GL_BLEND);
        checkGLError();
        glEnable(GL_CULL_FACE);
        checkGLError();
        //glDisable(GL_CULL_FACE);
        glCullFace(GL_BACK);
        checkGLError();
        mInitRendeState = true;
    }
    glDisable(GL_POLYGON_OFFSET_FILL);
    glFrontFace(GL_CCW);
    checkGLError();
    //glGenerateMipmap(GL_TEXTURE_2D);
    /*
    SS_Shader* shader = mModelManager->getShader("picture_fog_shader");
    shader->use();
    PhotoFrameNodeList::iterator itPicture;
    for(itPicture = mPictureObjectList.begin() ; 
        itPicture != mPictureObjectList.end();
        itPicture++)
    {
        PhotoFrameNode pfn = *itPicture;
        
        renderPhotoFrameNode(&pfn);
    }
    
    SS_Shader* shader1 = mModelManager->getShader("fog_shader");
    shader1->use();
    PhotoFrameNodeList::iterator itPhotoFrameNode;
    for(itPhotoFrameNode = mPhotoFrameNodeList.begin() ;
        itPhotoFrameNode != mPhotoFrameNodeList.end() ;
        itPhotoFrameNode++)
    {
        PhotoFrameNode pfn = *itPhotoFrameNode;
        renderPhotoFrameNode(&pfn);    
    }
    
    for(itPhotoFrameNode = mMirrorPhotoFrameNodeList.begin() ; 
        itPhotoFrameNode != mMirrorPhotoFrameNodeList.end() ;
        itPhotoFrameNode++)
    {
        PhotoFrameNode pfn = *itPhotoFrameNode;
        renderPhotoFrameNode(&pfn);
    }
    
    StaticNodeList::iterator itStaticNode;
    for(itStaticNode = mStaticNodeList.begin();
        itStaticNode != mStaticNodeList.end();
        itStaticNode++)
    {
        StaticNode sn = *itStaticNode;
        renderStaticNode(&sn);
    }
    
    StationNodeList::iterator itStationNode;
    for(itStationNode = mStationNodeList.begin() ;
        itStationNode != mStationNodeList.end() ;
        itStationNode++)
    {
        StationNode sn = *itStationNode;
        renderStationNode(&sn);
    }
     */
    starttime = getCurrentTime();
    for(DisplayObjectList::iterator it = mDisplayObjectList.begin() ;
        it != mDisplayObjectList.end();
        it++)
    {
        
        if(it->mirror)
        {
            glFrontFace(GL_CW);
        }
        else 
        {
            glFrontFace(GL_CCW);
        }
        
        if(it->bNeedPolygonOffset)
        {
            glEnable(GL_POLYGON_OFFSET_FILL);
            const float polygonOffsetFactor = -1.0f; 
            const float polygonOffsetUnits = -2.0f;
            glPolygonOffset(polygonOffsetFactor, polygonOffsetUnits);
        }
        else
        {
            glDisable(GL_POLYGON_OFFSET_FILL);
        }
        //if(it->bPicturePlace == false)
        {
            //if(it->objID == 1001 || it->objID == 1002 || it->objID == 1003 
            //   || it->objID == 1004 ||it->objID == 2000)
            renderObject(&(*it), it->worldM);
        }
    }
    if(mCurrentFadeInFrameIndex < FADE_IN_FRAMR_NUM)
    {
        mCurrentFadeInFrameIndex++;
    }
    endtime = getCurrentTime();
    span = getInterval(starttime, endtime);
    //LOGI("renderobject = %f\n", span);
    starttime = getCurrentTime();
    clearRenderNode();
    endtime = getCurrentTime();
    span = getInterval(starttime, endtime);
    //LOGI("clearRenderNode = %f\n", span);
    //LOGE("## render object count = %d ##\n", mRenderObjectCount);
    //renderFogPlane();
}
void SE_Scene::renderStaticNode(StaticNode* sn)
{
    //if(!isRender(sn))
    //    return;
    mRenderObjectCount++;
    glFrontFace(GL_CCW);
    DisplayObjectList::iterator it;
    for(it = sn->objList.begin() ; it != sn->objList.end() ; it++)
    {
        DisplayObject obj = *it;
        //if(!isRender(&obj, sn->worldM))
        //    continue;
        //SS_Shader* shader = mModelManager->getShader(obj.shaderName.c_str());
        //shader->use();
        renderObject(&obj, sn->worldM);
    }    
}
void SE_Scene::renderStationNode(StationNode* sn)
{
    //if(!isRender(sn))
    //    return;
    mRenderObjectCount++;
    glFrontFace(GL_CCW);
    DisplayObjectList::iterator it;
    for(it = sn->objList.begin() ; it != sn->objList.end() ; it++)
    {
        DisplayObject obj = *it;
        //if(!isRender(&obj, sn->worldM))
        //    continue;
        //SS_Shader* shader = mModelManager->getShader(obj.shaderName.c_str());
        //shader->use();
        renderObject(&obj, sn->worldM);
    }
}

bool SE_Scene::isRender(PhotoFrameNode* pfn)
{
    SE_Matrix4f worldM = pfn->worldM;
    SE_Vector4f t = worldM.getColumn(3);
    SE_Vector3f translate = t.xyz();
    SE_AABB aabb = pfn->aabb;
    SE_Vector3f min = aabb.getMin();
    SE_Vector3f max = aabb.getMax();
    if(pfn->mirror)
    {
        min = SE_Vector3f(-max.x, min.y, min.z);
        max = SE_Vector3f(-min.x, max.y, max.z);
        aabb.set(min, max);
    }
    aabb.move(translate);
    SE_AABBBV aabbbv(aabb);
    int cull = mCamera->cullBV(aabbbv);
    if(cull == SE_FULL_CULL)
        return false;
    else
        return true;
}
bool SE_Scene::isRender(StationNode* sn)
{
    SE_Matrix4f worldM = sn->worldM;
    SE_Vector4f t = worldM.getColumn(3);
    SE_Vector3f translate = t.xyz();
    SE_AABB aabb = sn->aabb;
    aabb.move(translate);
    SE_AABBBV aabbbv(aabb);
    int cull = mCamera->cullBV(aabbbv);
    if(cull == SE_FULL_CULL)
        return false;
    else
        return true;    
}
bool SE_Scene::isRender(StaticNode* sn)
{
    SE_Matrix4f worldM = sn->worldM;
    SE_Vector4f t = worldM.getColumn(3);
    SE_Vector3f translate = t.xyz();
    SE_AABB aabb = sn->aabb;
    aabb.move(translate);
    SE_AABBBV aabbbv(aabb);
    int cull = mCamera->cullBV(aabbbv);
    if(cull == SE_FULL_CULL)
        return false;
    else
        return true;
}
bool SE_Scene::isRender(DisplayObject* displayObject, const SE_Matrix4f& worldM)
{
    SE_Vector4f t = worldM.getColumn(3);
    SE_Vector3f translate = t.xyz();
    SE_Mesh* mesh = mModelManager->getMesh(displayObject->meshIndex);
    SE_AABB aabb = mesh->getAABB();
    aabb.move(translate);
    SE_AABBBV aabbbv(aabb);
    int cull = mCamera->cullBV(aabbbv);
    if(cull == SE_FULL_CULL)
        return false;
    else
        return true;    
}

void SE_Scene::renderPhotoFrameNode(PhotoFrameNode* pfn)
{
    mRenderObjectCount++;

    DisplayObjectList::iterator itObj;
    for(itObj = pfn->objList.begin() ; 
        itObj != pfn->objList.end(); 
        itObj++)
    {
        DisplayObject obj = *itObj;
        std::string name = getMeshName(obj.meshIndex);
        //if(!isRender(&obj, pfn->worldM))
        //    continue;
        SS_Shader* shader = mModelManager->getShader(obj.shaderName.c_str());
        //shader->use();
        if(obj.bPicturePlace)
        {
            GLint mirrorLoc = shader->getUniformLocation("u_mirror");
            checkGLError();
            if(pfn->mirror)
            {
                glUniform1i(mirrorLoc, 1);
            }
            else
            {
                glUniform1i(mirrorLoc, 0);
            }
            //obj.bConcerned = pfn->bConcerned;
        }
        renderObject(&obj, pfn->worldM);
    }
}

std::string SE_Scene::getMeshName(int index)
{
    MeshMap::iterator it;
    for(it = mMeshMap.begin() ; it != mMeshMap.end() ; it++)
    {
        if(it->second == index)
            return it->first;
    }
    return "";
}

/*
 GLint viewMLoc = shader->getUniformLocation("u_wv_matrix");
 checkGLError();
 m = mCamera->getWorldToViewMatrix().mul(worldM);
 m.getColumnSequence(matrixData);
 glUniformMatrix4fv(viewMLoc, 1, 0, matrixData);
 checkGLError();
 */
static void bindMeshVao(GLuint* vaoID, GLuint* uvVboID, float* uv, SE_Mesh* mesh, SS_Shader* shader, SE_VertexProperty vp)
{
    if(*vaoID == 0)
    {
        glGenVertexArraysOES(1, vaoID);
        glBindVertexArrayOES(*vaoID);
        glBindBuffer(GL_ARRAY_BUFFER, mesh->vboID);
        GLint positionLoc = shader->getAttribLocation("a_position");
        checkGLError();
        
        glVertexAttribPointer(positionLoc, SE_VertexProperty::XYZ_SIZE, GL_FLOAT, GL_FALSE, vp.stride, (const GLvoid*)vp.xyzOffset);
        //glVertexAttribPointer(positionLoc, SE_VertexProperty::XYZ_SIZE, GL_FLOAT, GL_FALSE, vp.stride, data);
        checkGLError();
        glEnableVertexAttribArray(positionLoc);
        checkGLError();
        GLint texCoordLoc = shader->getAttribLocation("a_tex_coord1");
        checkGLError();
        
        glVertexAttribPointer(texCoordLoc, SE_VertexProperty::UV_SIZE, GL_FLOAT, GL_FALSE, vp.stride, (const GLvoid*)(vp.uv1Offset * sizeof(float)));
        //glVertexAttribPointer(texCoordLoc, SE_VertexProperty::UV_SIZE, GL_FLOAT, GL_FALSE, vp.stride, (const GLvoid*)(data + 3));
        checkGLError();
        glEnableVertexAttribArray(texCoordLoc);
        checkGLError();
        
        if(*uvVboID == 0)
        {
            glGenBuffers(1, uvVboID);
            checkGLError();
            glBindBuffer(GL_ARRAY_BUFFER, *uvVboID);
            checkGLError();
            glBufferData(GL_ARRAY_BUFFER, sizeof(float) * 8, uv, GL_STATIC_DRAW);
            checkGLError();
        }
        glBindBuffer(GL_ARRAY_BUFFER, *uvVboID);
        checkGLError();
        GLint texCoordLoc2 = shader->getAttribLocation("a_tex_coord2");
        checkGLError();
        glEnableVertexAttribArray(texCoordLoc2);
        checkGLError();
        glVertexAttribPointer(texCoordLoc2, SE_VertexProperty::UV_SIZE, GL_FLOAT, GL_FALSE, 2 * sizeof(float)  , 0);
        checkGLError();
        glBindBuffer(GL_ARRAY_BUFFER, 0);
        checkGLError();
        glBindVertexArrayOES(0);
        checkGLError();
    }
    glBindVertexArrayOES(*vaoID);
    checkGLError();
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, mesh->indexVboID);
    checkGLError();    
}
void SE_Scene::setUV(int pictureIndex, float* uv)
{
    
}
void SE_Scene::renderObject(DisplayObject* obj, const SE_Matrix4f& worldM)
{
    SS_Shader* shader = mModelManager->getShader(obj->shaderName.c_str());
    shader->use();
    int meshIndex = obj->meshIndex;
    SE_Mesh* mesh = mModelManager->getMesh(meshIndex);
    SE_Mesh::VERTEX_TYPE vertexType = (SE_Mesh::VERTEX_TYPE)obj->vertexType;
    SE_VertexProperty vp = mesh->getVertexProperty(vertexType);
    bool isPicturePlace = obj->bPicturePlace;
    float* data = mesh->getDrawingVertex(vertexType);
    if(obj->bPicturePlace)
    {
        PHOTO_TYPE pt =  mPictureIDVector[obj->pictureIndex].photoType;
        int orientation = mPictureIDVector[obj->pictureIndex].orientation;
        /*
        float* uv = NULL;
        if(pt == PHOTOH)
        {
            if(obj->mirror)
            {
                uv = mirrorUvArrayH[orientation];
            }
            else 
            {
                uv = uvArrayH[orientation];
            }
        }
        else 
        {
            if(obj->mirror)
            {
                uv = mirrorUvArrayV[orientation];
            }
            else 
            {
                uv = uvArrayV[orientation];
            }
        }
        if(mesh->vboID == 0)
        {
            createMeshVBO(mesh);
        }
        if(obj->mirror)
        {
            bindMeshVao(&mesh->mirrorVaoIDArray[pt][orientation].vao, &mesh->mirrorVaoIDArray[pt][orientation].uvVbo, uv, mesh, shader, vp);
        }
        else
        {
            bindMeshVao(&mesh->vaoIDArray[pt][orientation].vao, &mesh->vaoIDArray[pt][orientation].uvVbo, uv, mesh, shader, vp);
        }
         */
        //new implementation
        //float uv[8];
        //setUV(obj->pictureIndex, uv);
        glBindVertexArrayOES(0);
        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
        checkGLError();
        /*
        if(mesh->vboID == 0)
        {
            createMeshVBO(mesh);
            glGenBuffers(1, &mesh->uvVboID);
            glBindBuffer(GL_ARRAY_BUFFER, mesh->uvVboID);
            glBufferData(GL_ARRAY_BUFFER, 8 * sizeof(float), NULL, GL_STATIC_DRAW);
        }
         */
        //glBindBuffer(GL_ARRAY_BUFFER, mesh->vboID);
        GLint positionLoc = shader->getAttribLocation("a_position");
        checkGLError();
        
        glVertexAttribPointer(positionLoc, SE_VertexProperty::XYZ_SIZE, GL_FLOAT, GL_FALSE, vp.stride, (const GLvoid*)data);
        checkGLError();
        glEnableVertexAttribArray(positionLoc);
        checkGLError();
        GLint texCoordLoc = shader->getAttribLocation("a_tex_coord1");
        checkGLError();
        
        glVertexAttribPointer(texCoordLoc, SE_VertexProperty::UV_SIZE, GL_FLOAT, GL_FALSE, vp.stride, (const GLvoid*)(data + 3));

        checkGLError();
        glEnableVertexAttribArray(texCoordLoc);
        checkGLError();
        /*
        GLint texCoordLoc2 = shader->getAttribLocation("a_tex_coord2");
        checkGLError();
        glVertexAttribPointer(texCoordLoc2, SE_VertexProperty::UV_SIZE, GL_FLOAT, GL_FALSE, 0 , uv);
        checkGLError();
        glEnableVertexAttribArray(texCoordLoc2);
        checkGLError();
    */
        //glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, mesh->indexVboID);
        //checkGLError();    
        //end new implementation
    }
    else
    {
        if(mesh->vboID == 0)
        {
            createMeshVBO(mesh);
        }
        if(mesh->vaoID == 0)
        {
            glGenVertexArraysOES(1, &mesh->vaoID);
            checkGLError();
            glBindVertexArrayOES(mesh->vaoID);
            checkGLError();
            glBindBuffer(GL_ARRAY_BUFFER, mesh->vboID);
            checkGLError();
            GLint positionLoc = shader->getAttribLocation("a_position");
            checkGLError();
            
            glVertexAttribPointer(positionLoc, SE_VertexProperty::XYZ_SIZE, GL_FLOAT, GL_FALSE, vp.stride, (const GLvoid*)vp.xyzOffset);
            //glVertexAttribPointer(positionLoc, SE_VertexProperty::XYZ_SIZE, GL_FLOAT, GL_FALSE, vp.stride, data);
            checkGLError();
            glEnableVertexAttribArray(positionLoc);
            checkGLError();
            GLint texCoordLoc = shader->getAttribLocation("a_tex_coord");
            checkGLError();
            
            glVertexAttribPointer(texCoordLoc, SE_VertexProperty::UV_SIZE, GL_FLOAT, GL_FALSE, vp.stride, (const GLvoid*)(vp.uv1Offset * sizeof(float)));
            //glVertexAttribPointer(texCoordLoc, SE_VertexProperty::UV_SIZE, GL_FLOAT, GL_FALSE, vp.stride, (const GLvoid*)(data + 3));
            checkGLError();
            glEnableVertexAttribArray(texCoordLoc);
            checkGLError();
            
            glBindBuffer(GL_ARRAY_BUFFER, 0);
            checkGLError();
            glBindVertexArrayOES(0);
            checkGLError();
        }
        glBindVertexArrayOES(mesh->vaoID);
        checkGLError();
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, mesh->indexVboID);
        checkGLError();
    }
    /*
    if(obj->bPicturePlace)
    {
        GLint mirrorLoc = shader->getUniformLocation("u_mirror");
        checkGLError();
        if(obj->mirror)
        {
            glUniform1i(mirrorLoc, 1);
        }
        else
        {
            glUniform1i(mirrorLoc, 0);
        }
    }
    */    
    float matrixData[16];
    SE_Matrix4f m = mCamera->getPerspectiveMatrix().mul(mCamera->getWorldToViewMatrix().mul(worldM));
    m.getColumnSequence(matrixData);                            
    GLint worldMLoc = shader->getUniformLocation("u_wvp_matrix");
    checkGLError();
    glUniformMatrix4fv(worldMLoc, 1, 0, matrixData);
    checkGLError();
    
    
    SE_Vector4f v = obj->worldM.getColumn(3);
    
    
    GLint fogPointLoc = shader->getUniformLocation("u_fogpoint");
    checkGLError();
    glUniform3f(fogPointLoc, mCurrentFogPoint.x, mCurrentFogPoint.y, mCurrentFogPoint.z);
    checkGLError();
    
    static float fogProp[4];
    fogProp[0] = 0.02;
    fogProp[1] = FAR_DIST;
    fogProp[2] = NEAR_DIST;
    fogProp[3] = v.y;
    
    GLint fogPropLoc = shader->getUniformLocation("u_fogprop");
    checkGLError();
    glUniform4f(fogPropLoc, fogProp[0], fogProp[1], fogProp[2], fogProp[3]);
    
    checkGLError();
    //GLint densityLoc = shader->getUniformLocation("u_density");
    //checkGLError();
    //glUniform1f(densityLoc, 0.05);
    
    //GLint farLoc = shader->getUniformLocation("far_dist");
    //checkGLError();
    //glUniform1f(farLoc, FAR_DIST);
    
    //GLint nearLoc = shader->getUniformLocation("near_dist");
    //checkGLError();
    //glUniform1f(nearLoc, NEAR_DIST);
    
    GLint fogColorLoc = shader->getUniformLocation("u_fog_color");
    checkGLError();
    glUniform3f(fogColorLoc, 1, 1, 1);
    
    /*
    GLint positionLoc = shader->getAttribLocation("a_position");
    checkGLError();
    
    glVertexAttribPointer(positionLoc, SE_VertexProperty::XYZ_SIZE, GL_FLOAT, GL_FALSE, vp.stride, (const GLvoid*)vp.xyzOffset);
    //glVertexAttribPointer(positionLoc, SE_VertexProperty::XYZ_SIZE, GL_FLOAT, GL_FALSE, vp.stride, data);
    checkGLError();
    glEnableVertexAttribArray(positionLoc);
    checkGLError();
     */
    if(isPicturePlace)
    {  
        /*
        GLint texCoordLoc1 = shader->getAttribLocation("a_tex_coord1");
        checkGLError();
        
        glVertexAttribPointer(texCoordLoc1, SE_VertexProperty::UV_SIZE, GL_FLOAT, GL_FALSE, vp.stride, (const GLvoid*)(vp.uv1Offset * sizeof(float)));
        //glVertexAttribPointer(texCoordLoc1, SE_VertexProperty::UV_SIZE, GL_FLOAT, GL_FALSE, vp.stride, (const GLvoid*)(data + 3));
        checkGLError();
        glEnableVertexAttribArray(texCoordLoc1);
        checkGLError();
        //GLint texCoordLoc2 = shader->getAttribLocation("a_tex_coord2");
        //checkGLError();
        //glEnableVertexAttribArray(texCoordLoc2);
        checkGLError();
         */
        /*
        glVertexAttribPointer(texCoordLoc2, SE_VertexProperty::UV_SIZE, GL_FLOAT, GL_FALSE, vp.stride, (const GLvoid*)(vp.uv2Offset * sizeof(float)));
         */
        //glVertexAttribPointer(texCoordLoc2, 2, GL_FLOAT, GL_FALSE, 0, obj->uv);
        //checkGLError();
    }
    else
    {
        /*
        GLint texCoordLoc = shader->getAttribLocation("a_tex_coord");
        checkGLError();
        
        glVertexAttribPointer(texCoordLoc, SE_VertexProperty::UV_SIZE, GL_FLOAT, GL_FALSE, vp.stride, (const GLvoid*)(vp.uv1Offset * sizeof(float)));
        //glVertexAttribPointer(texCoordLoc, SE_VertexProperty::UV_SIZE, GL_FLOAT, GL_FALSE, vp.stride, (const GLvoid*)(data + 3));
        checkGLError();
        glEnableVertexAttribArray(texCoordLoc);
        checkGLError();
         */
    }
    SE_Material* material = mModelManager->getMaterial(mesh->materialIndex);
    if(material)
    {
        if(material->materialData.texturename != "")
        {
            SE_Texture* t = mModelManager->getTexture(material->materialData.texturename.c_str());
            glPixelStorei(GL_UNPACK_ALIGNMENT,1);
            checkGLError();
            glActiveTexture(GL_TEXTURE0);
            checkGLError();
            //glEnable(GL_TEXTURE_2D);
            //checkGLError();
            if(!t)
            {
                t = new SE_Texture;
                loadTexture(material->materialData.texturename.c_str(), t);
                LOGI("## load first texture ###\n");
                checkGLError();
                mModelManager->setTexture(material->materialData.texturename.c_str(), t);
                
            }
            
            if(t && t->texture != 0)
            {
                glBindTexture(GL_TEXTURE_2D, t->texture);
                checkGLError();
            }
             
            if(isPicturePlace)
            {
                glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
                checkGLError();
                glActiveTexture(GL_TEXTURE1);
                checkGLError();
                assert(obj->pictureIndex >= 0);
                //LOGI("## obj picture index = %d ##\n", obj->pictureIndex);
                PictureID pictureID = mPictureIDVector[obj->pictureIndex];
                SE_Texture* imageTex = NULL;
                std::string textureID = pictureID.pictureName;
                imageTex = mModelManager->getFullImageTexture(textureID.c_str());
                if(imageTex == NULL)
                {
                    //mModelManager->getFullTextureCount();
                    imageTex = mModelManager->getTexture(pictureID.pictureName.c_str());
                }

                
                if(imageTex && imageTex->texture != 0)
                {
                    glBindTexture(GL_TEXTURE_2D, imageTex->texture);
                    checkGLError();
                    /*
                    GLint hasTex2Loc = shader->getUniformLocation("u_hastex2");
                    checkGLError();
                    glUniform1i(hasTex2Loc, 1);
                    checkGLError();
                     */
                }
                
                GLint texLoc1 = shader->getUniformLocation("u_texture1");
                checkGLError();
                glUniform1i(texLoc1, 0);
                checkGLError();
                
                GLint texLoc2 = shader->getUniformLocation("u_texture2");
                checkGLError();
                glUniform1i(texLoc2, 1);
                checkGLError();
                
                GLint mixColorAlphaLoc = shader->getUniformLocation("u_mixcolor_alpha");
                checkGLError();
                if(mCurrentFadeInFrameIndex < FADE_IN_FRAMR_NUM)
                {
                    glUniform1f(mixColorAlphaLoc, mCurrentFadeInFrameIndex / (float) FADE_IN_FRAMR_NUM);
                }
                else {
                    glUniform1f(mixColorAlphaLoc, 1.0f);
                }
                
                checkGLError();
                
                if(imageTex != NULL)
                {
                    //glBindBuffer(GL_ARRAY_BUFFER, mesh->uvVboID);
                    //glBufferSubData(GL_ARRAY_BUFFER, 0, 8 * sizeof(float), imageTex->uv);
                    
                    GLint texCoordLoc2 = shader->getAttribLocation("a_tex_coord2");
                    checkGLError();
                    if(obj->mirror)
                    {
                        glVertexAttribPointer(texCoordLoc2, SE_VertexProperty::UV_SIZE, GL_FLOAT, GL_FALSE, 0 , imageTex->uvMirror);
                    }
                    else 
                    {
                        glVertexAttribPointer(texCoordLoc2, SE_VertexProperty::UV_SIZE, GL_FLOAT, GL_FALSE, 0 , imageTex->uv);
                    }
                    checkGLError();
                    glEnableVertexAttribArray(texCoordLoc2);
                    checkGLError();
                }
            }
            else
            {
                GLint texLoc = shader->getUniformLocation("u_texture");
                checkGLError();
                glUniform1i(texLoc, 0);
                checkGLError();
            }
        }
        
    }
    //LOGI("mesh name = %s \n", mesh->name.c_str());
    //assert(mesh->mDrawingVertexNum != 2061);
    //if(mesh->mDrawingVertexNum != 2061)
    //glDrawArrays(GL_TRIANGLES, 0, mesh->mDrawingVertexNum);
    if(isPicturePlace)
    {
        glDrawElements(GL_TRIANGLES, mesh->getIndexBufferNum(vertexType), GL_UNSIGNED_SHORT, mesh->getIndexBuffer(vertexType));
    }
    else
    {
        glDrawElements(GL_TRIANGLES, mesh->getIndexBufferNum(vertexType), GL_UNSIGNED_SHORT, 0);
    }
    //glDrawElements(GL_TRIANGLES, mesh->getIndexBufferNum(vertexType), GL_UNSIGNED_SHORT, mesh->getIndexBuffer(vertexType));
    //glBindBuffer(GL_ARRAY_BUFFER, 0);
    //glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    checkGLError();    
}
static SS_ModelManager::VIEW_TYPE getTrackPointListType(SE_Scene::VIEW_TYPE t)
{
    if(t == SE_Scene::LANDSCAPE)
        return SS_ModelManager::LANDSCAPE;
    else {
        return SS_ModelManager::PORTRAIT;
    }
}
void SE_Scene::createCurve(const std::string& name, int composedPointNum, float time, float speed)
{
    TrackCurveData trackCurveData;
    trackCurveData.name = name;
    for(int i = 0 ; i < TRACK_LIST_PHOTO_NUM ; i++)
    {
        std::vector<SE_TrackPoint> trackPoint = mModelManager->getTrackPoints(getTrackPointListType(mViewType), (SS_ModelManager::TRACK_LIST_FOR_PHOTO_TYPE)i, name.c_str());
        assert(trackPoint.size() > 0);
        if(trackPoint.size() > 0)
        {
            SE_Curve::SamplePoint* tp = new SE_Curve::SamplePoint[trackPoint.size()];
            SE_Vector3f startRef = mStartRef;
            for(size_t j = 0 ; j < trackPoint.size() ; j++)
            {
                tp[j].point = mModelManager->getTrackPoint(getTrackPointListType(mViewType),trackPoint[j], startRef);
                LOGI("## sample point %lu = %f , %f, %f ##\n", j, tp[j].point.x, tp[j].point.y, tp[j].point.z);
            }
            SE_Curve* curve = new SE_Curve(tp, trackPoint.size(), composedPointNum);
            delete[] tp;
            curve->createCurve();
            float curveLen = curve->getTotalCurveLength();
            CurveData cd;
            cd.name = name;
            cd.curve = curve;
            cd.time = time * 1000;
            cd.curveLen = curveLen;
            cd.composePointNum = composedPointNum;
            trackCurveData.curveData[i] = cd;
        }
    }
    mCurveData.push_back(trackCurveData);
}
void SE_Scene::createYInverseCurve(const std::string& curveName, const std::string& inverseCurveName,int composedPointNum, float time, float speed)
{  
    TrackCurveData trackCurveData;
    trackCurveData.name = curveName;
    for(int i = 0 ; i < TRACK_LIST_PHOTO_NUM ; i++)
    {
        std::vector<SE_TrackPoint> trackPoint = mModelManager->getTrackPoints(getTrackPointListType(mViewType),(SS_ModelManager::TRACK_LIST_FOR_PHOTO_TYPE)i, curveName.c_str());
        assert(trackPoint.size() > 0);
        if(trackPoint.size() > 0)
        {
            std::vector<SE_TrackPoint> newTrackPoint(trackPoint.size());
            for(size_t i  = 0 ; i < trackPoint.size() ; i++)
            {
                newTrackPoint[i].x = trackPoint[i].x;
            }
            for(int i = trackPoint.size() - 1 , j = 0 ; i >= 0 ; i--, j++)
            {
                newTrackPoint[i].z = trackPoint[i].z;
                newTrackPoint[j].y = trackPoint[i].y;
            }
            SE_Curve::SamplePoint* tp = new SE_Curve::SamplePoint[newTrackPoint.size()];
            SE_Vector3f startRef = mStartRef;
            LOGI("## curve name = %s ###\n", inverseCurveName.c_str());
            for(size_t i = 0 ; i < newTrackPoint.size() ; i++)
            {
                tp[i].point = mModelManager->getTrackPoint(getTrackPointListType(mViewType),newTrackPoint[i], startRef);
                LOGI("## sample point %lu = %f , %f, %f ##\n", i, tp[i].point.x, tp[i].point.y, tp[i].point.z);
            }
            SE_Curve* curve = new SE_Curve(tp, newTrackPoint.size(), composedPointNum);
            delete[] tp;
            curve->createCurve();
            float curveLen = curve->getTotalCurveLength();
            CurveData cd;
            cd.name = inverseCurveName;
            cd.curve = curve;
            cd.time = time * 1000;
            cd.curveLen = curveLen;
            cd.composePointNum = composedPointNum;
            trackCurveData.curveData[i] =  cd;
        }
    }
    mCurveData.push_back(trackCurveData);
}
void SE_Scene::createCurve()
{
    std::vector<std::string> nameV = mModelManager->getAllTrackPointsName(getTrackPointListType(mViewType));
    for(int i = 0 ; i < nameV.size(); i++)
    {
        std::string n = nameV[i];
        LOGI("## n = %s ##\n", n.c_str());
        std::string::size_type found = n.find("HP");
        if(n != "" && found == std::string::npos)
        {
            createCurve(n, 2000, 5, 0.1);
        }
    }

}
SE_Scene::CurveData SE_Scene::getCurveData(const std::string& name, int trackListPhotoType)
{
    std::list<TrackCurveData>::iterator it;
    //LOGI("## current curve name = %s ##\n", name.c_str());
    for(it = mCurveData.begin() ; it != mCurveData.end() ; it++)
    {
        if(it->name == name)
        {
            return it->curveData[trackListPhotoType];
        }
    }
    return SE_Scene::CurveData();
}

bool SE_Scene::isNeedChangeCurve(float t)
{
    size_t size = mCameraMoveState->currentLookPointTrackList.size();
    float percent = mCameraMoveState->currentLookPointTrackList[size - 1].percent;
    if(t >= (percent / 100))
    {
        return true;
    }
    else
    {
        if(t >= 1 && t < (percent / 100))
        {
            mCameraStayStatic = true;
        }
        else 
        {
            mCameraStayStatic = false;
        }
        return false;
    }
    /*
    for(int i = 0 ; i < mCameraMoveState->currentLookPointTrackList.size() ; i++)
    {
        if(t <= (mCameraMoveState->currentLookPointTrackList[i].percent / 100.0f))
            return true;
    }
    return false;
     */
}
void SE_Scene::removeAllTexture()
{
    mModelManager->removeAllTexture();
}
void SE_Scene::removeAllShaderFromGL()
{
    mModelManager->removeAllShaderFromGL();
}
static SE_Vector3f calculateInterpolateLookingPoint(const SE_Vector3f& startLookingPoint, const SE_Vector3f& endLookingPoint, float interpolatev )
{
    float radian = radianBetweenVector(startLookingPoint, endLookingPoint);
    LOGI("## start end radian = %f ##\n", radian);
    if(fabsf(radian) > 0.001)
    {
        float t1 = sinf((1 - interpolatev) * radian) / sinf(radian);
        float t2 = sinf(interpolatev * radian) / sinf(radian);
        float z = endLookingPoint.z;
        SE_Vector3f p = startLookingPoint * t1 + endLookingPoint * t2;
        p.z = z;
        return p;
    }
    else
    {
        return startLookingPoint + (endLookingPoint - startLookingPoint) * interpolatev;
    }
}
SE_Vector3f SE_Scene::getExactLookPoint(float t)
{
    float photoFrameBoundY = mPhotoFrameBoundMax.y - mPhotoFrameBoundMin.y;
    return ::getExactLookPoint(mCameraMoveState->currentLookPointTrackList, photoFrameBoundY, mModelManager, t);
    /*
    SE_Vector3f lookingPoint;
    float photoFrameBoundY = mPhotoFrameBoundMax.y - mPhotoFrameBoundMin.y;
    for(int i = 0 ; i < mCameraMoveState->currentLookPointTrackList.size() ; i++)
    {
        if(fabs(t - (mCameraMoveState->currentLookPointTrackList[i].percent / 100.0f)) < 0.000001)
        {
            lookingPoint = mModelManager->getFrameLookingPoint(mCameraMoveState->currentLookPointTrackList[i].lookpointname.c_str());
            lookingPoint.y += photoFrameBoundY * (mCameraMoveState->currentLookPointTrackList[i].frameNum - 1);
            int side = mCameraMoveState->currentLookPointTrackList[i].side;
            if(side == 0)
            {
                lookingPoint.x = -lookingPoint.x;
            }
            return lookingPoint;
        }
    }
    return lookingPoint;
     */
} 
SE_Vector3f SE_Scene::getStartCurveLookPoint(float t)
{
    SE_Vector3f startLookingPoint;
    SE_Vector3f endLookingPoint;
    float interpolatev = 0;
    float photoFrameBoundY = mPhotoFrameBoundMax.y - mPhotoFrameBoundMin.y;
    for(int i = 0 ; i < mCameraMoveState->currentLookPointTrackList.size() ; i++)
    {
        if(fabs(t - (mCameraMoveState->currentLookPointTrackList[i].percent / 100.0f)) < 0.000001)
        {
            startLookingPoint = endLookingPoint = mModelManager->getFrameLookingPoint(mCameraMoveState->currentLookPointTrackList[i].lookpointname.c_str());
            startLookingPoint.y += photoFrameBoundY * (mCameraMoveState->currentLookPointTrackList[i].frameNum - 1);
            endLookingPoint.y += photoFrameBoundY * (mCameraMoveState->currentLookPointTrackList[i].frameNum - 1);
            int side = mCameraMoveState->currentLookPointTrackList[i].side;
            if(side == 0)
            {
                startLookingPoint.x = -startLookingPoint.x;
                endLookingPoint.x = -endLookingPoint.x;
            } 
            break;
        }
        else if(t < (mCameraMoveState->currentLookPointTrackList[i].percent / 100.0f))
        {
            float prevPercent = mCameraMoveState->currentLookPointTrackList[i - 1].percent / 100;
            float currPercent = mCameraMoveState->currentLookPointTrackList[i].percent / 100;
            int prevSide = mCameraMoveState->currentLookPointTrackList[i - 1].side;
            int currSide = mCameraMoveState->currentLookPointTrackList[i].side;
            startLookingPoint = mModelManager->getFrameLookingPoint(mCameraMoveState->currentLookPointTrackList[i - 1].lookpointname.c_str());
            endLookingPoint = mModelManager->getFrameLookingPoint(mCameraMoveState->currentLookPointTrackList[i].lookpointname.c_str());
            startLookingPoint.y += photoFrameBoundY * (mCameraMoveState->currentLookPointTrackList[i - 1].frameNum - 1);
            endLookingPoint.y += photoFrameBoundY * (mCameraMoveState->currentLookPointTrackList[i].frameNum - 1);
            if(prevSide == 0)
            {
                startLookingPoint.x = -startLookingPoint.x;
                
            }
            if(currSide == 0)
            {
                endLookingPoint.x = -endLookingPoint.x;
            }
            interpolatev = (t - prevPercent) / (currPercent - prevPercent);
            break;
        }
    }
    if(startLookingPoint.isZero() && endLookingPoint.isZero())
    {
        assert(0);
    }
    //LOGI("## interpoltev = %f ##\n", interpolatev);
    //LOGI("## start p = %f, %f, %f##\n", startLookingPoint.x, startLookingPoint.y ,startLookingPoint.z);
    //LOGI("## end p = %f, %f, %f ###\n", endLookingPoint.x, endLookingPoint.y, endLookingPoint.z);
    SE_Vector3f p = startLookingPoint + (endLookingPoint - startLookingPoint) * interpolatev;
    //SE_Vector3f p = calculateInterpolateLookingPoint(startLookingPoint, endLookingPoint, interpolatev);
    //LOGI("## look p = %f, %f, %f ##\n", p.x, p.y, p.z);
    return p;
    
}
SE_Vector3f SE_Scene::getCurrentLookPoint(float t)
{
    SE_Vector3f startLookingPoint;
    SE_Vector3f endLookingPoint;
    float interpolatev = 0;
    float photoFrameBoundY = mPhotoFrameBoundMax.y - mPhotoFrameBoundMin.y;
    for(int i = 0 ; i < mCameraMoveState->currentLookPointTrackList.size() ; i++)
    {
        if(fabs(t - (mCameraMoveState->currentLookPointTrackList[i].percent / 100.0f)) < 0.000001)
        {
            startLookingPoint = endLookingPoint = mModelManager->getFrameLookingPoint(mCameraMoveState->currentLookPointTrackList[i].lookpointname.c_str());
            startLookingPoint.y += photoFrameBoundY * (mCameraMoveState->currentLookPointTrackList[i].frameNum - 1);
            endLookingPoint.y += photoFrameBoundY * (mCameraMoveState->currentLookPointTrackList[i].frameNum - 1);
            int side = mCameraMoveState->currentLookPointTrackList[i].side;
            mCurrentLookingPhotoFrameNode.side = RIGHT_SIDE;
            if(side == 0)
            {
                startLookingPoint.x = -startLookingPoint.x;
                endLookingPoint.x = -endLookingPoint.x;
                mCurrentLookingPhotoFrameNode.side = LEFT_SIDE;
            } 
            if(mCameraStayStatic)
            {
                PHOTO_FRAME_NODE_TYPE nodeType = INVALID_PHOTO_FRAME_NODE_TYPE;
                if(mCameraMoveState->currentLookPointTrackList[i].frameNum == 1)
                {
                    if(mCurrentLookingPhotoFrameNode.side == LEFT_SIDE)
                    {
                        nodeType = SW;
                    }
                    else if(mCurrentLookingPhotoFrameNode.side == RIGHT_SIDE)
                    {
                        nodeType = SE;
                    }
                }
                else if(mCameraMoveState->currentLookPointTrackList[i].frameNum == 2)
                {
                    if(mCameraMoveState->bLeavingGroup == false)
                    {
                        if(mCurrentLookingPhotoFrameNode.side == LEFT_SIDE)
                        {
                            nodeType = NW;
                        }
                        else if(mCurrentLookingPhotoFrameNode.side == RIGHT_SIDE)
                        {
                            nodeType = NE;
                        }
                    }
                    else {
                        if(mCurrentLookingPhotoFrameNode.side == LEFT_SIDE)
                        {
                            nodeType = SW;
                        }
                        else if(mCurrentLookingPhotoFrameNode.side == RIGHT_SIDE)
                        {
                            nodeType = SE;
                        }
                    }
                }
                if(nodeType != INVALID_PHOTO_FRAME_NODE_TYPE && nodeType != mCurrentLookingPhotoFrameNode.nodeType)
                {
                    mCurrentLookingPhotoFrameNode.prevNodeType = mCurrentLookingPhotoFrameNode.nodeType;
                    mCurrentLookingPhotoFrameNode.nodeType = nodeType;
                    mCurrentLookingPhotoFrameNode.nodeChanged = true;
                }
                else {
                    mCurrentLookingPhotoFrameNode.nodeChanged = false;
                }
            }
            break;
        }
        else if(t < (mCameraMoveState->currentLookPointTrackList[i].percent / 100.0f))
        {
            float prevPercent = mCameraMoveState->currentLookPointTrackList[i - 1].percent / 100;
            float currPercent = mCameraMoveState->currentLookPointTrackList[i].percent / 100;
            int prevSide = mCameraMoveState->currentLookPointTrackList[i - 1].side;
            int currSide = mCameraMoveState->currentLookPointTrackList[i].side;
            startLookingPoint = mModelManager->getFrameLookingPoint(mCameraMoveState->currentLookPointTrackList[i - 1].lookpointname.c_str());
            endLookingPoint = mModelManager->getFrameLookingPoint(mCameraMoveState->currentLookPointTrackList[i].lookpointname.c_str());
            startLookingPoint.y += photoFrameBoundY * (mCameraMoveState->currentLookPointTrackList[i - 1].frameNum - 1);
            endLookingPoint.y += photoFrameBoundY * (mCameraMoveState->currentLookPointTrackList[i].frameNum - 1);
            mCurrentLookingPhotoFrameNode.side = RIGHT_SIDE;
            if(prevSide == 0)
            {
                startLookingPoint.x = -startLookingPoint.x;
                
            }
            if(currSide == 0)
            {
                endLookingPoint.x = -endLookingPoint.x;
                mCurrentLookingPhotoFrameNode.side = LEFT_SIDE;
            }
            interpolatev = (t - prevPercent) / (currPercent - prevPercent);
            if(mCameraStayStatic)
            {
                PHOTO_FRAME_NODE_TYPE nodeType = INVALID_PHOTO_FRAME_NODE_TYPE;
                if(mCameraMoveState->currentLookPointTrackList[i].frameNum == 1)
                {
                    if(mCurrentLookingPhotoFrameNode.side == LEFT_SIDE)
                    {
                        nodeType = SW;
                    }
                    else if(mCurrentLookingPhotoFrameNode.side == RIGHT_SIDE)
                    {
                        nodeType = SE;
                    }
                }
                else if(mCameraMoveState->currentLookPointTrackList[i].frameNum == 2)
                {
                    if(mCameraMoveState->bLeavingGroup == false)
                    {
                        if(mCurrentLookingPhotoFrameNode.side == LEFT_SIDE)
                        {
                            nodeType = NW;
                        }
                        else if(mCurrentLookingPhotoFrameNode.side == RIGHT_SIDE)
                        {
                            nodeType = NE;
                        }
                    }
                    else 
                    {
                        if(mCurrentLookingPhotoFrameNode.side == LEFT_SIDE)
                        {
                            nodeType = SW;
                        }
                        else if(mCurrentLookingPhotoFrameNode.side == RIGHT_SIDE)
                        {
                            nodeType = SE;
                        }
                    }
                }
                if(nodeType != INVALID_PHOTO_FRAME_NODE_TYPE && nodeType != mCurrentLookingPhotoFrameNode.nodeType)
                {
                    mCurrentLookingPhotoFrameNode.prevNodeType = mCurrentLookingPhotoFrameNode.nodeType;
                    mCurrentLookingPhotoFrameNode.nodeType = nodeType;
                    mCurrentLookingPhotoFrameNode.nodeChanged = true;
                }
                else {
                    mCurrentLookingPhotoFrameNode.nodeChanged = false;
                }
            }
            break;
        }
    }
    if(startLookingPoint.isZero() && endLookingPoint.isZero())
    {
        assert(0);
    }
    //LOGI("## interpoltev = %f ##\n", interpolatev);
    //LOGI("## start p = %f, %f, %f##\n", startLookingPoint.x, startLookingPoint.y ,startLookingPoint.z);
    //LOGI("## end p = %f, %f, %f ###\n", endLookingPoint.x, endLookingPoint.y, endLookingPoint.z);
    SE_Vector3f p = startLookingPoint + (endLookingPoint - startLookingPoint) * interpolatev;
    //SE_Vector3f p = calculateInterpolateLookingPoint(startLookingPoint, endLookingPoint, interpolatev);
    //LOGI("## look p = %f, %f, %f ##\n", p.x, p.y, p.z);
    return p;
}
std::string SE_Scene::removeSufix(const std::string& name, const std::string& sufix)
{
    std::string::size_type pos = name.find('_');
    if(pos == std::string::npos)
        return name;
    else
        return name.substr(0, pos);
}
void SE_Scene::addPointEdge(std::list<std::string>& edge)
{
    PointList::iterator pointIt;
    std::list<std::string>::iterator it = se_list_nref(edge, 0);
    std::string pointName1 = *it;
    it = se_list_nref(edge, 1);
    std::string pointName2 = *it;
    it = se_list_nref(edge, 2);
    std::string curveName = *it;
    it = se_list_nref(edge, 3);
    std::string time = *it;
    it = se_list_nref(edge, 4);
    std::string outGroup = *it;
    it = se_list_nref(edge, 5);
    std::string logoState = *it;
    bool found = false;
    for(pointIt = mPointList.begin() ; pointIt != mPointList.end() ; pointIt++)
    {
        if(pointIt->pointName == pointName1)
        {
            Edge e;
            e.pointName = pointName2;
            e.curveName = curveName;
            e.time = atoi(time.c_str());
            e.outGroup = atoi(outGroup.c_str());
            e.logoState = (LOGO_STATE)atoi(logoState.c_str());
            pointIt->edges.push_back(e);
            found = true;
            break;
        }
    }
    if(!found)
    {
        Point p;
        p.pointName = pointName1;
        Edge e;
        e.pointName = pointName2;
        e.curveName = curveName;
        e.time = atoi(time.c_str());
        e.outGroup = atoi(outGroup.c_str());
        e.logoState = (LOGO_STATE)atoi(logoState.c_str());
        p.edges.push_back(e);
        mPointList.push_back(p);
    }
}
void SE_Scene::createPointEdges()
{
    /*
    Point p;
    Edge e;
    
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
    */
}
void SE_Scene::setCurveData(int index)
{
    mCameraMoveState->moveStart = true;
    std::string curveName = mCameraMoveState->cameraPath[index].name;
    mCameraMoveState->currentCurve = getCurveData(curveName, SS_ModelManager::HH);
    mCameraMoveState->currentCurveTotalTime = mCameraMoveState->cameraPath[index].time * 1000;
    mCameraMoveState->currentTime = 0;
    std::vector<SE_LookingPointTrackData> lookpointTrackData = mModelManager->getLookingPointTrackDataList(curveName);
    mCameraMoveState->currentLookPointTrackList = lookpointTrackData;
}
void SE_Scene::setCurveData(const PointCurveData& pcd, int trackListPhotoType)
{
    mCameraMoveState->moveStart = true;
    mCameraMoveState->currentCurve = getCurveData(pcd.curveName, trackListPhotoType);
    mCameraMoveState->currentTime = 0;
    mRotateTime = 0;
    mTimeForRotate = false;
    mRotateT = 0;
    mCameraMoveState->currentCurveTotalTime = pcd.time * 1000;
    std::vector<SE_LookingPointTrackData> lookpointTrackData = mModelManager->getLookingPointTrackDataList(pcd.curveName);
    mCameraMoveState->currentLookPointTrackList = lookpointTrackData;
    mCameraMoveState->endLocationName = pcd.pointName;
}
void SE_Scene::printPointEdge()
{
    PointList::iterator it;
    for(it = mPointList.begin() ; it != mPointList.end(); it++)
    {
        std::list<Edge>::iterator edgeIt;
        for(edgeIt = it->edges.begin() ; edgeIt != it->edges.end(); edgeIt++)
        {
            LOGI("%s , ", it->pointName.c_str());
            LOGI("%s , ", edgeIt->pointName.c_str());
            LOGI("%s , ", edgeIt->curveName.c_str());
            LOGI("%d , ", edgeIt->time);
            LOGI("%d , ", edgeIt->outGroup);
            LOGI("%d , ", edgeIt->logoState);
            LOGI("\n");
        }
        
    }
}
static int locationNameToNodeType(const std::string name)
{
    int index = -1;
    if(name == "L1")
    {
        index = SE_Scene::SW;
    }    
    else if(name == "L2")
    {
        index = SE_Scene::NW;
    }
    else if(name == "R1")
    {
        index = SE_Scene::SE;
    }
    else if(name == "R2")
    {
        index = SE_Scene::NE;
    }
    else {
        assert(0);
    }
    return index;
}
int SE_Scene::photoFrameNodePictureIndex(PhotoFrameNode& photoFrameNode)
{
    for(DisplayObjectList::iterator it = photoFrameNode.objList.begin();
        it != photoFrameNode.objList.end() ;it++)
    {
        if(it->bPicturePlace)
        {
            return it->pictureIndex;
        }
    }
    return -1;
}
void SE_Scene::getFullImageIndex(GroupList::iterator prevGroupIt, GroupList::iterator currGroupIt, const std::string startLocationName, const std::string endLocationName, bool isLeaveGroup,int& startIndex, int& endIndex)
{
    int startNode = SW;
    if(mCameraMoveState->bStartCurve)
    {
        startNode = SW;
    }
    else
    {
        startNode = locationNameToNodeType(startLocationName);
    }
    int endNode;
    if(mCameraMoveState->bStartCurve)
    {
        endNode = NW;
    }
    else
    {
        endNode = locationNameToNodeType(endLocationName);
    }
    if(isLeaveGroup)
    {
        startIndex = photoFrameNodePictureIndex(prevGroupIt->photoFrameNode[startNode]);
        endIndex = photoFrameNodePictureIndex(currGroupIt->photoFrameNode[endNode]);
    }
    else
    {
        startIndex = photoFrameNodePictureIndex(currGroupIt->photoFrameNode[startNode]);
        endIndex = photoFrameNodePictureIndex(currGroupIt->photoFrameNode[endNode]);
    }
}
bool SE_Scene::isCameraMoveDrawing()
{
    return mIsDrawing && mCurrentFadeInFrameIndex >= FADE_IN_FRAMR_NUM;
}
void SE_Scene::updateState()
{
    if(mIsDrawing == true)
        return;
    bool startImageOK = false;
    //if(mStartFullImage.size() == 0)
    //    startImageOK = true;
    for(std::list<std::string>::iterator it = mStartFullImage.begin();
        it != mStartFullImage.end() ;it++)
    {
        std::string textureName = *it;
        SE_Texture* t = mModelManager->getFullImageTexture(textureName.c_str());
        if(t == NULL)
        {
            startImageOK = false;
            break;
        }
        else 
        {
            startImageOK = true;
        }
    }
    //// maybe move to other place
    bool thumbnailOK = false;
    if(mPictureIDVector.size() == 1)
    {
        std::string name = mPictureIDVector[0].pictureName;
        if(SS_IsDefaultSelectedImage(name))
        {
            thumbnailOK = true;
        }
    }
    if(!thumbnailOK)
    {
        int num = getSeenThumbnailImageNum();
        std::list<int> indexList = getSeenThumbnailImageIndex(num);
        //for(int i = 0 ; i < num ; i++)
        for(std::list<int>::iterator it = indexList.begin() ; it != indexList.end() ; it++)
        {
            int i = *it;
            std::string texturename = mPictureIDVector[i].pictureName;
            SE_Texture* t = mModelManager->getTexture(texturename.c_str());
            if(t == NULL)
            {
                thumbnailOK = false;
                break;
            }
            else 
            {
                thumbnailOK = true;
            }
        }
    }
    /////
    //if(startImageOK || mStartFullImage.size() == 0)
    if(startImageOK && thumbnailOK)
    {
        mStartFullImage.clear();
        mIsDrawing = true;
    }
}
SE_Scene::PHOTO_TYPE SE_Scene::getFirstPhotoFramePhotoType()
{
    GroupList::iterator first = mGroupList.begin();
    return first->photoFrameNode[SW].photoType;
}
void SE_Scene::startCameraMove()
{
    mCameraStayStatic = true;
    mCameraMoveState->bStartCurve = true;
    mFirstimeStartCurveToEnd = false;
    mStartCurveCameraVectorIndex = 0;
    createCurve();
    //setCurrentCurveProperty("P1L");
    //std::vector<SE_TrackPoint> trackPoint = mModelManager->getTrackPoints(getTrackPointListType(mViewType), SS_ModelManager::VV, "P1L");
    
    mCameraMoveState->startLocationName = "S1";
    PHOTO_TYPE pt = getFirstPhotoFramePhotoType();
    std::vector<SE_TrackPoint> trackPoint;
    if(pt == PHOTOH)
    {
        trackPoint = mModelManager->getTrackPoints(getTrackPointListType(mViewType), SS_ModelManager::HH, "LS1");
        mTrackListPhotoType = SS_ModelManager::HH;
    }
    else 
    {
        trackPoint = mModelManager->getTrackPoints(getTrackPointListType(mViewType), SS_ModelManager::VV, "LS1");
        mTrackListPhotoType = SS_ModelManager::VV;
    }
    if(mCameraMoveState->bStartCurve)
    {
        SE_Vector3f point = mModelManager->getTrackPoint(getTrackPointListType(mViewType), trackPoint[trackPoint.size() - 1], mStartRef);
        SE_TrackPoint adjust ;
        if(pt == PHOTOH)
        {
            adjust = mModelManager->getTrackPointAdjust(getTrackPointListType(mViewType), SS_ModelManager::HH, "LS1");
        }
        else
        {
            adjust = mModelManager->getTrackPointAdjust(getTrackPointListType(mViewType), SS_ModelManager::VV, "LS1");
        }
        SE_TrackPoint t = trackPoint[trackPoint.size() - 1];
        SE_Vector3f forwardPoint = mModelManager->getTrackPoint(getTrackPointListType(mViewType), SE_TrackPoint(t.x + adjust.x, t.y + adjust.y , t.z + adjust.z), mStartRef);
        mCameraForwardingDist = point.x - forwardPoint.x;
    }
    else
    {
        setCurrentCurveProperty("LS1");
    }

    assert(trackPoint.size() > 0);
    SE_Vector3f startRef = mStartRef;
    
    //SE_Vector3f point = mModelManager->getTrackPoint(getTrackPointListType(mViewType),trackPoint[0], startRef);
    CurveData curveData = getCurveData("LS1", mTrackListPhotoType);
    SE_Vector3f point;
    float step = 0;// * mCameraMoveState->currentCurve.curveLen;
    curveData.curve->setCurrentPoint(0);
    curveData.curve->setForwardingStep(step);
    curveData.curve->getNextCurvePoint(point);
    point.y -= mYLength;
    //SE_Vector3f lookingPoint = mModelManager->getFrameLookingPoint(frameLookingPointName);
    //lookingPoint.x = -lookingPoint.x;
    //point.z = lookingPoint.z; // when use LS1 this can be error
    
    SE_Vector3f lookingPoint = mModelManager->getFrameLookingPoint(startLookingPointName);
    lookingPoint.x = -lookingPoint.x;//this is according to getExactLookPoint;
    //SE_Vector3f frameLookingPoint = mModelManager->getFrameLookingPoint(frameLookingPointName);
    //lookingPoint.x = -lookingPoint.x;
    
    SE_Vector3f zAxis;
    if(mCameraMoveState->bStartCurve)
    {
        zAxis = SE_Vector3f(point.x, point.y, 0) - SE_Vector3f(lookingPoint.x, lookingPoint.y, 0);
    }
    else 
    {
        zAxis = SE_Vector3f(point.x, point.y, point.z) - SE_Vector3f(lookingPoint.x, lookingPoint.y, lookingPoint.z);
    }
    zAxis = zAxis.normalize();
    SE_Vector3f yAxis = SE_Vector3f(0, 0, 1);
    updateFogPointByCameraPoint(point);
    mCamera->create(point, zAxis, yAxis, CAMERA_FIELD_OF_VIEW, CAMERA_RATIO, 1, CAMERA_FAR_DIST);
    LOGI("#### start camera p : %f, %f, %f ###\n", point.x, point.y, point.z);
    LOGI("#### start camera z: %f, %f, %f ###\n", zAxis.x, zAxis.y, zAxis.z);
    bool bHasStation = false;
    int startPictureIndex = 0, endPictureIndex = 0;
    if(mCameraMoveState->bStartCurve)
    {
        mCurrentLookingPhotoFrameNode.currGroupIt = mGroupList.begin();
        mCurrentLookingPhotoFrameNode.prevGroupIt = mGroupList.begin();
        mCurrentLookingPhotoFrameNode.currGroupIt->photoFrameNode[SW].bConcerned = true;
        mCurrentLookingPhotoFrameNode.side = LEFT_SIDE;
    }
    else
    {
        GroupList::iterator group = locationInGroup(point.y);
        bHasStation = group->hasStationNode;
        mCurrentLookingPhotoFrameNode.currGroupIt = group;
        mCurrentLookingPhotoFrameNode.prevGroupIt = group;
        group->photoFrameNode[locationNameToNodeType(mCameraMoveState->startLocationName)].bConcerned = true;
    }
    getFullImageIndex(mCurrentLookingPhotoFrameNode.prevGroupIt, mCurrentLookingPhotoFrameNode.currGroupIt, mCameraMoveState->startLocationName, mCameraMoveState->endLocationName, mCameraMoveState->isLeaveGroup(), startPictureIndex, endPictureIndex);
    PointCurveData pcd = calculatePointCurve(mCameraMoveState->startLocationName, mCameraMoveState->isLeaveGroup(), (bHasStation ? HAS_LOGO : NO_LOGO));
    setCurveData(pcd, mTrackListPhotoType);

    
    mStartFullImage.clear();
    if(startPictureIndex != -1)
    {
        //loadFullImage(startPictureIndex);
        mStartFullImage.push_back(getFullImageTextureName(startPictureIndex));
        //loadFullImage(endPictureIndex);
        mStartFullImage.push_back(getFullImageTextureName(endPictureIndex));
        
        std::list<SE_TextureLoadInfo> textureLoadInfoList;
        PictureID pictureID = mPictureIDVector[startPictureIndex];
        std::string pictureName = pictureID.pictureName;
        std::string textureName = pictureName;
        int photoType = pictureID.photoType;
        SE_TextureLoadInfo loadInfo;
        loadInfo.pictureName = pictureName;
        loadInfo.pictureDate = pictureID.pictureDate;
        loadInfo.photoFrameOrientation = photoType;
        loadInfo.textureName = pictureName;
        textureLoadInfoList.push_back(loadInfo);
        
        pictureID = mPictureIDVector[endPictureIndex];
        pictureName = pictureID.pictureName;
        textureName = pictureName;
        photoType = pictureID.photoType;
        loadInfo.pictureName = pictureName;
        loadInfo.pictureDate = pictureID.pictureDate;
        loadInfo.photoFrameOrientation = photoType;
        loadInfo.textureName = pictureName;
        textureLoadInfoList.push_back(loadInfo);
        
        SS_LoadImageTextureListAsync(textureLoadInfoList, mModelManager, mViewNav, true);
        
        mFullImagePictureIndexList.push_back(startPictureIndex);
        mFullImagePictureIndexList.push_back(endPictureIndex);
        std::string firstImageName = mPictureIDVector[0].pictureName;
        if(mPictureIDVector.size() > 1 || !SS_IsDefaultSelectedImage(firstImageName))
        {
            int num = getSeenThumbnailImageNum();
            std::list<int> indexList = getSeenThumbnailImageIndex(num);
            num = indexList.size();
            std::vector<std::string> imageNameArray(num);
            std::vector<std::string> imageDateArray(num);
            std::vector<int> imageOrientArray(num);
            std::list<int>::iterator it;
            int k = 0;
            //for(int i = 0 ; i < imageNameArray.size() ; i++)
            for(it = indexList.begin() ; it != indexList.end() ; it++)
            {
                int i = *it;
                imageNameArray[k] = mPictureIDVector[i].pictureName;
                imageDateArray[k] = mPictureIDVector[i].pictureDate;
                imageOrientArray[k] = mPictureIDVector[i].photoType;
                k++;
            }
            SS_LoadThumbnailTextureForImageArray(mViewNav, mModelManager, imageNameArray, imageDateArray, imageOrientArray);
        }
    }
    //for debug
    //printPointEdge();
    //end
}
void SE_Scene::setCurvePath(const std::vector<CameraPathProperty>& paths)
{
    if(paths.size() == 0)
        return;
    createCurve();
    mCameraMoveState->cameraPath.clear();
    mCameraMoveState->cameraPath.resize(paths.size());
    std::copy(paths.begin(), paths.end(), mCameraMoveState->cameraPath.begin());
    mCameraMoveState->curvePathIndex = 0;
    setCurveData(0);
}
float SE_Scene::setCurrentCurveProperty(const std::string& curveName)
{
    std::vector<SE_LookingPointTrackData> lookpointTrackData = mModelManager->getLookingPointTrackDataList(curveName);
    LOGI("## current curve name = %s ##\n", curveName.c_str());
    SE_Vector3f point, nextPoint;
    std::vector<SE_TrackPoint> hhTrackPoints = mModelManager->getTrackPoints(getTrackPointListType(mViewType), SS_ModelManager::HH, curveName.c_str());
    LOGI("## look point size  =%ld, track point size = %ld \n", lookpointTrackData.size(), hhTrackPoints.size());
    
    point = mModelManager->getTrackPoint(getTrackPointListType(mViewType), hhTrackPoints[0], mStartRef);
    nextPoint = mModelManager->getTrackPoint(getTrackPointListType(mViewType), hhTrackPoints[hhTrackPoints.size() - 1], mStartRef);
    float photoFrameBoundY = mPhotoFrameBoundMax.y - mPhotoFrameBoundMin.y;
    SE_Vector3f lookpoint = ::getExactLookPoint(lookpointTrackData, photoFrameBoundY, mModelManager, 0);
    SE_Vector3f nextLookPoint = ::getExactLookPoint(lookpointTrackData, photoFrameBoundY, mModelManager, 1);
    if(mCameraMoveState->bLeavingGroup == true)
    {
        nextLookPoint.y += mYLength;
        nextPoint.y += mYLength;
        point.y += mYLength;
        lookpoint.y += mYLength;
    }
    PhotoFrameNode* firstNode = getPhotoFrameNode(point, lookpoint);
    PhotoFrameNode* nextNode = getPhotoFrameNode(nextPoint, nextLookPoint);
    SS_ModelManager::TRACK_LIST_FOR_PHOTO_TYPE trackListForPhotoType = SS_ModelManager::HH;
    if(firstNode->photoType == PHOTOH && nextNode->photoType == PHOTOH)
    {
        trackListForPhotoType = SS_ModelManager::HH;
    }
    else if(firstNode->photoType == PHOTOH && nextNode->photoType == PHOTOV)
    {
        trackListForPhotoType = SS_ModelManager::HV;
    }
    else if(firstNode->photoType == PHOTOV && nextNode->photoType == PHOTOH)
    {
        trackListForPhotoType = SS_ModelManager::VH;
    }
    else if(firstNode->photoType == PHOTOV && nextNode->photoType == PHOTOV)
    {
        trackListForPhotoType = SS_ModelManager::VV;
    }
    else 
    {
        LOGI("error photo node\n");
        assert(0);
    }
    SE_TrackPoint trackPointAdjust = mModelManager->getTrackPointAdjust(getTrackPointListType(mViewType), trackListForPhotoType, curveName.c_str());
    std::vector<SE_TrackPoint> trackPoints = mModelManager->getTrackPoints(getTrackPointListType(mViewType), trackListForPhotoType, curveName.c_str());
    SE_TrackPoint lastTrackPoint = trackPoints[trackPoints.size() - 1];
    SE_TrackPoint forwardingTrackPoint(lastTrackPoint.x + trackPointAdjust.x, lastTrackPoint.y + trackPointAdjust.y , lastTrackPoint.z + trackPointAdjust.z);
    SE_Vector3f forwardingPoint = mModelManager->getTrackPoint(getTrackPointListType(mViewType), forwardingTrackPoint, mStartRef);
    hhTrackPoints = mModelManager->getTrackPoints(getTrackPointListType(mViewType), trackListForPhotoType, curveName.c_str());
    point = mModelManager->getTrackPoint(getTrackPointListType(mViewType), hhTrackPoints[0], mStartRef);
    nextPoint = mModelManager->getTrackPoint(getTrackPointListType(mViewType), hhTrackPoints[hhTrackPoints.size() - 1], mStartRef);
    //lookpoint = ::getExactLookPoint(lookpointTrackData, photoFrameBoundY, mModelManager, 0);
    //nextLookPoint = ::getExactLookPoint(lookpointTrackData, photoFrameBoundY, mModelManager, 1);
    if(mCameraMoveState->bLeavingGroup == true)
    {
        //nextLookPoint.y += mYLength;
        nextPoint.y += mYLength;
    }
    float dist = nextPoint.x - forwardingPoint.x;
    mCameraForwardingDist = dist;
    mTrackListPhotoType = trackListForPhotoType;
    LOGI("## current curve = %s , track list type = %d \n", curveName.c_str(), trackListForPhotoType);
    LOGI("## dist = %f ##\n", dist);
    return dist;
    
}
SE_Scene::PhotoFrameNode* SE_Scene::getPhotoFrameNode(const SE_Vector3f& loc, const SE_Vector3f& lookpoint)
{
    float y = loc.y;
    GroupList::iterator it;
    SE_Vector3f dist = loc - lookpoint;
    for(it = mGroupList.begin(); it != mGroupList.end() ; it++)
    {
        PhotoFrameNode pfn = it->photoFrameNode[SW];
        SE_Matrix4f m = pfn.worldM;
        SE_Vector4f v = m.getColumn(3);
        float min = mFloorDownMin.y + v.y;
        float max = mFloorDownMax.y + v.y;//check SW and SE
        float max2 = mFloorDownMax.y + v.y + mYLength;
        if(min <= y && y <= max)
        {
            if(dist.x < 0)
            {
                return &it->photoFrameNode[SE];
            }
            else {
                return &it->photoFrameNode[SW];
            }
        }
        else if( max < y && y <= max2)
        {
            if(dist.x < 0)
            {
                return &it->photoFrameNode[NE];
            }
            else {
                return &it->photoFrameNode[NW];
            }
        }
    }
    return NULL;
}
SE_Scene::GroupList::iterator SE_Scene::locationInGroup(float y)
{
    //float dist = y - mFloorDownMin.y;
    GroupList::iterator it;
    for(it = mGroupList.begin(); it != mGroupList.end() ; it++)
    {
        PhotoFrameNode pfn = it->photoFrameNode[SW];
        SE_Matrix4f m = pfn.worldM;
        SE_Vector4f v = m.getColumn(3);
        float min = mFloorDownMin.y + v.y;
        float max = mFloorDownMax.y + v.y + mYLength;
        if(min <= y && y <= max)
            return it;
    }
    return mGroupList.end();
}
int SE_Scene::locationInGroup(const SE_Vector3f& loc)
{
    float y = loc.y - mFloorDownMin.y;
    int index = floorf(y / (2 * mYLength));
    if(index < 0)
        index = 0;
    return index;
}
void SE_Scene::updateFogPointByCameraPoint(const SE_Vector3f& cameraLoc)
{
    float y = cameraLoc.y;
    float times = y / mYLength;
    int a = floor(times);
    //mCurrentFogPoint.y = mFirstFogPoint.y + mFirstFogPoint.y * a;
    mCurrentFogPoint.y = cameraLoc.y;
    //LOGI("## fogpoint = %f , %f , %f ##\n", mCurrentFogPoint.x, mCurrentFogPoint.y ,mCurrentFogPoint.z);
}
std::string SE_Scene::getFullImageTextureName(int pictureIndex)
{
    PictureID pictureID = mPictureIDVector[pictureIndex];
    std::string pictureName = pictureID.pictureName;
    std::string textureName = pictureName;//pictureName + "_full";
    return textureName;
}
void SE_Scene::loadFullImage(int pictureIndex)
{
    PictureID pictureID = mPictureIDVector[pictureIndex];
    std::string pictureName = pictureID.pictureName;
    std::string textureName = pictureName;
    int photoType = pictureID.photoType;
    if(pictureIndex != -1)
    {
        LOGI("## ready to load = %d, %s ##\n", pictureIndex, textureName.c_str());
        SS_LoadImageTextureAsync(pictureName.c_str(), pictureID.pictureDate.c_str(), textureName.c_str(), mModelManager, mViewNav, true, photoType);
    }
}
void SE_Scene::deleteFullImageTexture()
{
    assert(mFullImagePictureIndexList.size() == 3);
    LOGI("## full image picture index = %lu ##\n", mFullImagePictureIndexList.size());
    std::list<int>::iterator it = se_list_nref(mFullImagePictureIndexList, 0);
    int index = *it;
    std::string textureName = getFullImageTextureName(index);
    it++;
    bool removeTexture = true;
    for(; it != mFullImagePictureIndexList.end() ; it++)
    {
        int i = *it;
        std::string otherTextureName = getFullImageTextureName(i);
        if(otherTextureName == textureName)
        {
            removeTexture = false;
        }
    }
    LOGI("## want to delete picture = %d ##\n", index);
    if(removeTexture)
        mModelManager->removeFullImageTexture(textureName.c_str());
    mFullImagePictureIndexList.pop_front();
}
static float bounce(float t)
{
    return t * t * 8.0f;
}
static float a(float t, float s) {
    return t * t * ((s + 1) * t - s);
}

static float o(float t, float s) {
    return t * t * ((s + 1) * t + s);
}


float SE_Scene::interpolateChange(float t)
{
    const float pi = 3.1415926;
    if(t > 1)
        t = 1;
    if(mCameraMoveState->startLocationName == "S1")
    {
        float newT = t * t;
        if(newT < 0.00001)
            return 0;
        else
            return newT;
        /*
        if(newT < 0.5)
            return newT;
        else
        {
                
        }
         */
    }
    if(1)
    {
        return t;
    }
    else if(0)
    {
        float ret = SE_Cosf((t + 1) * pi) / 2.0 + 0.5;
        return ret;
    }
    else if (0){
        
        t *= 1.1226f;
        if (t < 0.3535f) 
            return bounce(t);
        else if (t < 0.7408f) return bounce(t - 0.54719f) + 0.7f;
        else if (t < 0.9644f) return bounce(t - 0.8526f) + 0.9f;
        else return bounce(t - 1.0435f) + 0.95f;
    }
    else if(0){
        float tension = 3;
        if (t < 0.5f) return 0.5f * a(t * 2.0f, tension);
        else return 0.5f * (o(t * 2.0f - 2.0f, tension) + 2.0f);
    }

}
int SE_Scene::getPhotoFrameImageIndex(PhotoFrameNode& photoFrameNode)
{
    DisplayObjectList::iterator it;
    for(it = photoFrameNode.objList.begin() ; it != photoFrameNode.objList.end() ; it++)
    {
        if(it->pictureIndex != -1)
            return it->pictureIndex;
    }
    return -1;
}
void SE_Scene::removeGroupTexture(std::vector<int>& indexV)
{
    for(int i = 0 ; i < indexV.size() ; i++)
    {
        GroupList::iterator it;
        int index = indexV[i];
        bool found = false;
        for(it = mNegativeGroupList.begin() ; it != mNegativeGroupList.end() ; it++)
        {
            for(int j = 0 ; j  < 4 ; j++)
            {
                if(it->photoFrameNode[j].pictureIndex == index)
                {
                    found = true;
                    break;
                }
            }
            if(found)
                break;
        }
        if(found == false)
        {
            for(it = mGroupList.begin() ; it != mGroupList.end() ; it++)
            {
                for(int j = 0 ; j  < 4 ; j++)
                {
                    if(it->photoFrameNode[j].pictureIndex == index)
                    {
                        found = true;
                        break;
                    }
                }
                if(found)
                    break;
            }
        }
        if(found == false)
        {
            std::string pictureName = mPictureIDVector[index].pictureName;
            mModelManager->removeTexture(pictureName.c_str());
        }
    }
}
void SE_Scene::changeGroupWorldTranslate()
{
    bool needRemoveGroupFromGroupList = false;
    if(mNegativeGroupList.size() > 0)
    {
        Group lastGroup = mNegativeGroupList.back();
        std::vector<int> indexV(4);
        for(int i = 0 ; i < 4 ; i++)
        {
            indexV[i] = lastGroup.photoFrameNode[i].pictureIndex;
        }
        mNegativeGroupList.pop_back();
        removeGroupTexture(indexV);
    }
    else
    {
        needRemoveGroupFromGroupList = true;    
    }
    GroupList::iterator it;
    float deltaY = -2 * mYLength;
    for(it = mNegativeGroupList.begin() ; it != mNegativeGroupList.end() ; it++)
    {
        for(int i = 0 ;i < 4 ; i++)
        {
            moveTranslateY(it->photoFrameNode[i], deltaY);
        }
        moveTranslateY(it->staticNode[DOWN], deltaY);
        moveTranslateY(it->staticNode[UP], deltaY);
        if(it->hasStationNode)
        {
            moveTranslateY(it->stationNode, deltaY);
        }
    }
    for(it = mGroupList.begin() ; it != mGroupList.end() ; it++)
    {
        for(int i = 0 ;i < 4 ; i++)
        {
            moveTranslateY(it->photoFrameNode[i], deltaY);
        }
        moveTranslateY(it->staticNode[DOWN], deltaY);
        moveTranslateY(it->staticNode[UP], deltaY);
        if(it->hasStationNode)
        {
            moveTranslateY(it->stationNode, deltaY);
        }
    }
    it = se_list_nref(mGroupList, mGroupList.size() - 1);
    PhotoFrameNode pfn = it->photoFrameNode[SW];
    SE_Matrix4f m = pfn.worldM;
    SE_Vector4f v = m.getColumn(3);
    SE_Vector3f cameraLoc = mCamera->getLocation();
    mCamera->setLocation(SE_Vector3f(cameraLoc.x, cameraLoc.y + deltaY, cameraLoc.z));
    float distanceToCamera = fabsf(v.y - mYLength - cameraLoc.y);
    LOGI("## v.y = %f ##\n", v.y);
    if(v.y < CAMERA_FAR_DIST)
    {
        LOGI("## add new group , last picture index = %d ##\n", mLastPictureIndex);
        Group group;
        float nodey = v.y + 2 * mYLength;
        assert(nodey >= CAMERA_FAR_DIST);
        group.photoFrameNode[SW] = createPhotoFrameNode(nodey, SW, 0, mLastPictureIndex);
        nodey = v.y + 2 * mYLength;
        group.photoFrameNode[SE] = createPhotoFrameNode(nodey, SE, 0, mLastPictureIndex);
        nodey = v.y + 2 * mYLength + mYLength;
        group.photoFrameNode[NW] = createPhotoFrameNode(nodey, NW, 0, mLastPictureIndex);
        nodey = v.y + 2 * mYLength + mYLength;
        group.photoFrameNode[NE] = createPhotoFrameNode(nodey, NE, 0, mLastPictureIndex);
        nodey = v.y + 2 * mYLength;
        group.staticNode[DOWN] = createStaticNode(nodey, DOWN);
        nodey = v.y + 2 * mYLength + mYLength;
        group.staticNode[UP] = createStaticNode(nodey, UP);
        
        if(needRemoveGroupFromGroupList)
        {
            Group firstGroup = mGroupList.front();
            std::vector<int> indexV(4);
            for(int i = 0 ; i < 4 ; i++)
            {
                indexV[i] = firstGroup.photoFrameNode[i].pictureIndex;
                LOGI("## add index = %d ##\n", indexV[i]);
            }
            mGroupList.pop_front();
            mGroupList.push_back(group);
            removeGroupTexture(indexV);
        }
        else
        {
            mGroupList.push_back(group);    
        }
        std::list<int> indexV;
        for(int i = 0 ; i < 4 ; i++)
        {
            int index  = group.photoFrameNode[i].pictureIndex;
            std::string str = mPictureIDVector[index].pictureName;
            SE_Texture* texture = mModelManager->getTexture(str.c_str());
            if(texture == NULL)
            {
                indexV.push_back(index);
            }
        }
        int num = indexV.size();
        std::vector<std::string> imageNameArray(num);
        std::vector<std::string> imageDateArray(num);
        std::vector<int> imageOrientArray(num);

        int k = 0;
        for(std::list<int>::iterator it = indexV.begin(); it != indexV.end() ; it++)
        {
            int i = *it;
            imageNameArray[k] = mPictureIDVector[i].pictureName;
            imageDateArray[k] = mPictureIDVector[i].pictureDate;
            imageOrientArray[k] = mPictureIDVector[i].photoType;
            k++;
        }
        SS_LoadThumbnailTextureForImageArray(mViewNav, mModelManager, imageNameArray, imageDateArray, imageOrientArray);
    }
}
SE_Vector3f SE_Scene::getCurrentVector(const SE_Vector3f& startPoint, const SE_Vector3f& endPoint, const SE_Vector3f& currentPoint, float t)
{

}
static bool isPositiveAngle(const SE_Vector3f& start, const SE_Vector3f& endVector)
{
    SE_Vector3f cross = start.cross(endVector);
    if(cross.z > 0)
    {
        return true;
    }
    else 
    {
        return false;
    }
}
float SE_Scene::getForwardingDist(const char* curveName)
{
    std::vector<SE_TrackPoint> trackPoints = mModelManager->getTrackPoints(getTrackPointListType(mViewType), SS_ModelManager::HH, curveName);
    SE_TrackPoint firstTrackPoint = trackPoints[0];
    SE_TrackPoint lastTrackPoint = trackPoints[trackPoints.size() - 1];
    SE_Vector3f firstPoint = mModelManager->getTrackPoint(getTrackPointListType(mViewType), firstTrackPoint, mStartRef);
    SE_Vector3f lastPoint = mModelManager->getTrackPoint(getTrackPointListType(mViewType), lastTrackPoint, mStartRef);
    return 0;
}
float SE_Scene::getForwardingPoint(const char* curveName, SE_Vector3f& startPoint, SE_Vector3f& endPoint, float& startDist, float& endDist, bool start)
{
    std::vector<SE_TrackPoint> points = mModelManager->getTrackPoints(getTrackPointListType(mViewType), SS_ModelManager::HH, curveName);
    SE_TrackPoint first = points[0];
    SE_TrackPoint last = points[points.size() - 1];
    SE_TrackPoint newFirst = first;
    newFirst.x = 3.3;
    SE_TrackPoint newLast = last;
    if(last.x == 4)
        //newLast.x = 3.3;
        newLast.x = 4;
    else if(last.x > 4)
        //newLast.x = last.x + 0.7;
        newLast.x = last.x + 0.85;
    SE_Vector3f newStartPoint = mModelManager->getTrackPoint(getTrackPointListType(mViewType), newFirst, mStartRef);
    SE_Vector3f newEndPoint = mModelManager->getTrackPoint(getTrackPointListType(mViewType), newLast, mStartRef);
    SE_Vector3f oldStartPoint = mModelManager->getTrackPoint(getTrackPointListType(mViewType), first, mStartRef);
    SE_Vector3f oldEndPoint = mModelManager->getTrackPoint(getTrackPointListType(mViewType), last, mStartRef);
    float dist = newStartPoint.distance(oldStartPoint);
    startDist = dist;
    dist = newEndPoint.distance(oldEndPoint);
    endDist = dist;
    if(last.x > 10)
        endDist = -endDist;
    float curveLen = mCameraMoveState->currentCurve.curve->getTotalCurveLength();
    float ratio = dist / curveLen;
    float seconds = ratio * mCameraMoveState->currentCurveTotalTime;
    if(start)
        return ratio;
    else 
    {
        return 0;
    }
}
SE_Vector3f SE_Scene::getStartCurveCameraLookingVector(float t, const SE_Vector3f& currentPoint,  const SE_Vector3f& currentLookPoint)
{
    std::pair<float, float> startEndT = getSpanStartEndInterpolatePoint(t);
    //LOGI("## t = %f, start = %f, end = %f ##\n", t, startEndT.first, startEndT.second);
    assert(t >= startEndT.first && t <= startEndT.second);
    SE_Vector3f point;
    SE_Vector3f nextPoint;
    float step = startEndT.first * mCameraMoveState->currentCurve.curveLen;
    float nextStep = startEndT.second * mCameraMoveState->currentCurve.curveLen;
    mCameraMoveState->currentCurve.curve->setCurrentPoint(0);
    mCameraMoveState->currentCurve.curve->setForwardingStep(step);
    mCameraMoveState->currentCurve.curve->getNextCurvePoint(point);
    
    mCameraMoveState->currentCurve.curve->setCurrentPoint(0);
    mCameraMoveState->currentCurve.curve->setForwardingStep(nextStep);
    mCameraMoveState->currentCurve.curve->getNextCurvePoint(nextPoint);
    
    SE_Vector3f lookpoint = getExactLookPoint(startEndT.first);
    SE_Vector3f nextLookPoint = getExactLookPoint(startEndT.second);

    SE_Vector3f firstCameraVector = SE_Vector3f(point.x, point.y, 0) - SE_Vector3f(lookpoint.x, lookpoint.y, 0);
    SE_Vector3f nextCameraVector = SE_Vector3f(nextPoint.x, nextPoint.y, 0) - SE_Vector3f(nextLookPoint.x, nextLookPoint.y, 0);
    float radian = radianBetweenVector(firstCameraVector, nextCameraVector);
    float angle = radian * 180 / SE_PI;
    //LOGI("### start end angle = %d ##\n", angle);
    bool calculate = false;
    if(t == 1 && mFirstimeStartCurveToEnd == false)
    {
        mFirstimeStartCurveToEnd = true;
        calculate = true;
    }
    else if(t == 1 && mFirstimeStartCurveToEnd == true)
    {
        calculate = false;
    }

    if(startEndT.first != startEndT.second || calculate == true)
    {
        if(!(lookpoint == nextLookPoint))
        {
            bool positiveAngle = isPositiveAngle(firstCameraVector, nextCameraVector);
            float ratio = 1;
            if(startEndT.first != startEndT.second)
                ratio = (t - startEndT.first) / (startEndT.second - startEndT.first);
            if(ratio >= 1)
                ratio = 1;
            if(ratio == 1)
            {
                //LOGI("### nextCameraVector = %f, %f, %f ###\n", nextCameraVector.x, nextCameraVector.y, nextCameraVector.z);
                return nextCameraVector;
            }
            else
            {
                /*
                float newAngle = angle * ratio;
                if(positiveAngle == false)
                {
                    newAngle = -newAngle;
                }
                SE_Quat q(newAngle, SE_Vector3f(0, 0, 1));
                SE_Vector3f newV = q.map(firstCameraVector);
                //LOGI("### camera vector = %f, %f, %f ###\n", newV.x, newV.y, newV.z);
                return newV;
                 */
                SE_Vector3f interpolatev;
                interpolatev.x = sinf((1 - ratio) * radian) * firstCameraVector.x / sinf(radian) + sinf(ratio * radian) * nextCameraVector.x / sinf(radian);
                interpolatev.y = sinf((1 - ratio) * radian) * firstCameraVector.y / sinf(radian) + sinf(ratio * radian) * nextCameraVector.y / sinf(radian);
                interpolatev.z = sinf((1 - ratio) * radian) * firstCameraVector.z / sinf(radian) + sinf(ratio * radian) * nextCameraVector.z / sinf(radian);
                if(t == 0 )
                    return firstCameraVector;
                else
                    return interpolatev;
            }
        }
        else
        {
            return SE_Vector3f(currentPoint.x, currentPoint.y, 0)  - SE_Vector3f( currentLookPoint.x, currentLookPoint.y, 0);
        }
    }
    else 
    {
        //LOGI("### invalid camera x = %f, %f, %f\n", mCamera->getAxisX().x, mCamera->getAxisX().y, mCamera->getAxisX().z);
        //LOGI("### invalid camera y = %f, %f, %f\n", mCamera->getAxisY().x, mCamera->getAxisY().y, mCamera->getAxisY().z);
        //LOGI("### invalid camera z = %f, %f, %f\n", mCamera->getAxisZ().x, mCamera->getAxisZ().y, mCamera->getAxisZ().z);
        
        SE_Vector3f xAxis(0, 1, 0);
        if(mCamera->getAxisX().y < 0)
        {
            xAxis.y = -xAxis.y;
        }
        SE_Vector3f yAxis(0, 0, 1);
        SE_Vector3f zAxis = xAxis.cross(yAxis);
        mTimeForRotate = false;
        zAxis = zAxis.normalize();
        nextCameraVector = nextCameraVector.normalize();
        if(mStartCurveCameraVectorIndex == 0)
        {
            mStartCurveCameraVectorIndex++;
            zAxis = (zAxis + nextCameraVector) * 0.333;
        }
        else if(mStartCurveCameraVectorIndex == 1)
        {
            mStartCurveCameraVectorIndex++;
            zAxis = (zAxis + nextCameraVector) * 0.66;
        }
        //mCamera->create(mCamera->getLocation(), xAxis, yAxis, zAxis,CAMERA_FIELD_OF_VIEW, CAMERA_RAT
        //return mCamera->getAxisZ();
        //LOGI("### final vector = %f, %f, %f ###\n", zAxis.x, zAxis.y, zAxis.z);
        return zAxis;
    }

}
SE_Vector3f SE_Scene::getCameraLookingVector(float t, const SE_Vector3f& currentPoint, const SE_Vector3f& currentLookPoint)
{
    if(t == 1)
    {
        mTimeForRotate = true;
    }
    //if(mTimeForRotate)
    //    t = mRotateT;
    if(mCameraMoveState->bStartCurve)
    {
        return getStartCurveCameraLookingVector(t, currentPoint, currentLookPoint);
    }
    std::pair<float, float> startEndT = getSpanStartEndInterpolatePoint(t);
    //LOGI("## t = %f, start = %f, end = %f ##\n", t, startEndT.first, startEndT.second);
    assert(t >= startEndT.first && t <= startEndT.second);
    SE_Vector3f point;
    SE_Vector3f nextPoint;
    float step = startEndT.first * mCameraMoveState->currentCurve.curveLen;
    float nextStep = startEndT.second * mCameraMoveState->currentCurve.curveLen;
    mCameraMoveState->currentCurve.curve->setCurrentPoint(0);
    mCameraMoveState->currentCurve.curve->setForwardingStep(step);
    mCameraMoveState->currentCurve.curve->getNextCurvePoint(point);
    
    mCameraMoveState->currentCurve.curve->setCurrentPoint(0);
    mCameraMoveState->currentCurve.curve->setForwardingStep(nextStep);
    mCameraMoveState->currentCurve.curve->getNextCurvePoint(nextPoint);
    
    SE_Vector3f lookpoint = getExactLookPoint(startEndT.first);
    SE_Vector3f nextLookPoint = getExactLookPoint(startEndT.second);

    //LOGI("current curve = %s\n", mCameraMoveState->currentCurve.name.c_str());
    if(mCameraMoveState->currentCurve.name != "LS1")
    {
        if(fabs(startEndT.second - 1.0) < 0.00001)
        {
            nextPoint.y = nextLookPoint.y;
        }
        if(fabs(startEndT.first - 0) < 0.000001)
        {
            point.y = lookpoint.y;
        }
        point.z = lookpoint.z;
        nextPoint.z = nextLookPoint.z;
    }
    else 
    {
        point.y -= mYLength;
        nextPoint.y -= mYLength;
    }

    SE_Vector3f firstCameraVector = SE_Vector3f(point.x, point.y, 0) - SE_Vector3f(lookpoint.x, lookpoint.y, 0);
    SE_Vector3f nextCameraVector = SE_Vector3f(nextPoint.x, nextPoint.y, 0) - SE_Vector3f(nextLookPoint.x, nextLookPoint.y, 0);
    float radian = radianBetweenVector(firstCameraVector, nextCameraVector);
    float angle = radian * 180 / SE_PI;
    //LOGI("### start end angle = %d ##\n", angle);
    float unitAngle = 0.1;
    if(startEndT.first != startEndT.second)
    {
        if(!(lookpoint == nextLookPoint))
        {
            bool positiveAngle = isPositiveAngle(firstCameraVector, nextCameraVector);
            float ratio = (t - startEndT.first) / (startEndT.second - startEndT.first);
            if(ratio >= 1)
                ratio = 1;
            if(ratio == 1)
            {
                
                return nextCameraVector;
            }
            else
            {
                /*
                float newAngle = angle * ratio;
                if(positiveAngle == false)
                {
                    newAngle = -newAngle;
                }
                SE_Quat q(newAngle, SE_Vector3f(0, 0, 1));
                SE_Vector3f newV = q.map(firstCameraVector);    
                return newV;
                 */
                SE_Vector3f interpolatev;
                interpolatev.x = sinf((1 - ratio) * radian) * firstCameraVector.x / sinf(radian) + sinf(ratio * radian) * nextCameraVector.x / sinf(radian);
                interpolatev.y = sinf((1 - ratio) * radian) * firstCameraVector.y / sinf(radian) + sinf(ratio * radian) * nextCameraVector.y / sinf(radian);
                interpolatev.z = sinf((1 - ratio) * radian) * firstCameraVector.z / sinf(radian) + sinf(ratio * radian) * nextCameraVector.z / sinf(radian);
                return interpolatev;
            }
        }
        else
        {
             return SE_Vector3f(currentPoint.x, currentPoint.y, currentPoint.z)  - SE_Vector3f( currentLookPoint.x, currentLookPoint.y, currentLookPoint.z);    
        }
    }
    else 
    {
        //LOGI("### invalid camera x = %f, %f, %f\n", mCamera->getAxisX().x, mCamera->getAxisX().y, mCamera->getAxisX().z);
        //LOGI("### invalid camera y = %f, %f, %f\n", mCamera->getAxisY().x, mCamera->getAxisY().y, mCamera->getAxisY().z);
        //LOGI("### invalid camera z = %f, %f, %f\n", mCamera->getAxisZ().x, mCamera->getAxisZ().y, mCamera->getAxisZ().z);
        
        SE_Vector3f xAxis(0, 1, 0);
        if(mCamera->getAxisX().y < 0)
        {
            xAxis.y = -xAxis.y;
        }
        SE_Vector3f yAxis(0, 0, 1);
        SE_Vector3f zAxis = xAxis.cross(yAxis);
        //mCamera->create(mCamera->getLocation(), xAxis, yAxis, zAxis,CAMERA_FIELD_OF_VIEW, CAMERA_RAT
        //return mCamera->getAxisZ();
        mTimeForRotate = false;
        return zAxis;
    }
}
//t can be greater than 1
std::pair<float, float> SE_Scene::getSpanStartEndInterpolatePoint(float t)
{    
    int count = mCameraMoveState->currentLookPointTrackList.size();
    //LOGI("## looking point count = %d ##\n", count);
    if(t >= 1)
    {
        return std::pair<float, float>(1, 1);
    }
    //LOGI("## t = %f ##\n", t);
    for(int i = 0 ; i < count ; i++)
    {
        float percent = (mCameraMoveState->currentLookPointTrackList[i].percent / 100.0f);
        assert((i + 1) < count);
        float nextPercent = (mCameraMoveState->currentLookPointTrackList[i + 1].percent / 100.0f);
        if(t == percent)
        {
            return std::pair<float, float>(percent, nextPercent);
        }
        else if(t < percent)
        {
            float prevPercent = (mCameraMoveState->currentLookPointTrackList[i - 1].percent / 100.0f);
            assert((i - 1) >= 0);
            return std::pair<float, float>(prevPercent, percent);
        }
    }
    assert(0);
    return std::pair<float, float>(0, 0);
}
int SE_Scene::getSeenThumbnailImageNum()
{
    if(mPictureIDVector.size() > MIN_LOAD_THUMBNAIL_NUM)
        return MIN_LOAD_THUMBNAIL_NUM;
    else {
        return mPictureIDVector.size();
    }
}
static void addIntToList(std::list<int>& intList, int i)
{
    bool found = false;
    for(std::list<int>::iterator it = intList.begin() ; it != intList.end(); it++)
    {
        if(*it == i)
        {
            found = true;
            break;
        }
    }
    if(!found)
    {
        intList.push_back(i);
    }
}
std::list<int> SE_Scene::getSeenThumbnailImageIndex(int num)
{
    if((num % 2) != 0)
    {
        num += 1;
    }
    int n = num / 2;
    std::list<int> retList;
    if(num < 2)
        n = 1;
    GroupList::iterator it ;
    int k = 0;
    for(it = mGroupList.begin() ; it != mGroupList.end() && k < n; it++)
    {
        for(int i = 0 ; i < 4; i++)
        {
            //retList.push_back(it->photoFrameNode[i].pictureIndex);
            addIntToList(retList, it->photoFrameNode[i].pictureIndex);
            k++;
        }
    }
    k = 0;
    //GroupList::iterator it ;
    for(it = mNegativeGroupList.begin() ; it != mNegativeGroupList.end() && k < n; it++)
    {
        for(int i = 0 ; i < 4; i++)
        {
            //retList.push_back(it->photoFrameNode[i].pictureIndex);
            addIntToList(retList, it->photoFrameNode[i].pictureIndex);
            k++;
        }
    }
    for(std::list<int>::iterator it = retList.begin() ; it != retList.end() ; it++)
    {
        LOGI("## i = %d ##\n", *it);
    }
    return retList;
}
void SE_Scene::loadThumbnailImageAfterGroupChange()
{
    SE_Vector3f v = mCamera->getLocation();
    GroupList::iterator groupIt = locationInGroup(v.y);
    int groupNum = 4;
    int i = 0;
    std::list<std::string> imageNameList;
    std::list<std::string> imageDateList;
    std::list<int> imageOrientList;
    for(; groupIt != mGroupList.end() && i < groupNum; groupIt++, i++)
    {
        Group group = *groupIt;
        for(int i = 0 ; i < 4 ; i++)
        {
            PhotoFrameNode& photoFrameNode = groupIt->photoFrameNode[i];
            int pictureIndex = getPhotoFrameImageIndex(photoFrameNode);
            if(pictureIndex != -1)
            {
                std::string texturename = mPictureIDVector[pictureIndex].pictureName;
                SE_Texture* t = mModelManager->getTexture(texturename.c_str());
                if(t == NULL)
                {
                    imageNameList.push_back(mPictureIDVector[pictureIndex].pictureName);
                    imageDateList.push_back(mPictureIDVector[pictureIndex].pictureDate);
                    imageOrientList.push_back( mPictureIDVector[pictureIndex].photoType);
                }
            }
        }
    }
    std::vector<std::string> imageNameArray(imageNameList.size());
    std::vector<std::string> imageDateArray(imageDateList.size());
    std::vector<int> imageOrientArray(imageOrientList.size());
    std::copy(imageNameList.begin(), imageNameList.end(), imageNameArray.begin());
    std::copy(imageDateList.begin(), imageDateList.end(), imageDateArray.begin());
    std::copy(imageOrientList.begin(), imageOrientList.end(), imageOrientArray.begin());
    SS_LoadThumbnailTextureForImageArray(mViewNav, mModelManager, imageNameArray, imageDateArray, imageOrientArray);
}
void SE_Scene::findGroupTextureNotLoaded(GroupList::iterator currentGroup)
{
    if(mCurrentLookingPhotoFrameNode.nodeChanged == false || mCameraStayStatic == false)
        return;
    GroupList::iterator startGroup = currentGroup;
    GroupList::iterator next1 = mGroupList.end();
    GroupList::iterator next2 = mGroupList.end();
    GroupList::iterator next3 = mGroupList.end();
    GroupList::iterator prev1 = mGroupList.end();
    GroupList::iterator prev2 = mGroupList.end();
    GroupList::iterator prev3 = mGroupList.end();
    if(currentGroup != mGroupList.end())
    {
        next1 = currentGroup;
        next1++;
    }
    if(next1 != mGroupList.end())
    {
        next2 = next1;
        next2++;
    }
    if(next2 != mGroupList.end())
    {
        next3 = next2;
        next3++;
    }
    
    if(currentGroup != mGroupList.begin())
    {
        prev1 = currentGroup;
        prev1--;
    }
    if(prev1 != mGroupList.begin())
    {
        prev2 = prev1;
        prev2--;
    }
    if(prev2 != mGroupList.begin())
    {
        prev3 = prev2;
        prev3--;
    }
    GroupList::iterator itList[6] = {prev1, prev2, prev3, next1, next2, next3};
    std::list<int> indexList;
    for(int i = 0 ; i < 6 ; i++)
    {
        if(itList[i] != mGroupList.end())
        {
            for(int j = 0 ;  j < 4 ; j++)
            {
                int pictureIndex = itList[i]->photoFrameNode[j].pictureIndex;
                addIntToList(indexList, pictureIndex);
            }
        }
    }
    std::list<int> needLoadList;
    for(std::list<int>::iterator it = indexList.begin() ; it != indexList.end(); it++)
    {
        int index = *it;
        std::string str = mPictureIDVector[index].pictureName;
        SE_Texture* texture = mModelManager->getTexture(str.c_str());
        if(texture == NULL)
        {
            needLoadList.push_back(index);
        }
    }
    int num = needLoadList.size();
    LOGI("need loaded image num = %d\n", num);
    if(num == 0)
    {
        return;
    }
    std::vector<std::string> imageNameArray(num);
    std::vector<std::string> imageDateArray(num);
    std::vector<int> imageOrientArray(num);
    int k = 0;
    for(std::list<int>::iterator it = needLoadList.begin(); it != needLoadList.end() ; it++)
    {
        int i = *it;
        LOGI("## need load %d ##\n", i);
        imageNameArray[k] = mPictureIDVector[i].pictureName;
        imageDateArray[k] = mPictureIDVector[i].pictureDate;
        imageOrientArray[k] = mPictureIDVector[i].photoType;
        k++;
    }
    SS_LoadThumbnailTextureForImageArray(mViewNav, mModelManager, imageNameArray, imageDateArray, imageOrientArray);
}
void SE_Scene::updateCameraMove(float deltaTime)
{
    if(!mCameraMoveState->moveStart)
        return;
    if(mCurrentFadeInFrameIndex < FADE_IN_FRAMR_NUM)
        return;
    //if(mTimeForRotate == false)
    {
        mCameraMoveState->currentTime += deltaTime;
    }
    mRotateTime += deltaTime;
    mRotateT = mRotateTime / mCameraMoveState->currentCurveTotalTime;
    mRotateT = interpolateChange(mRotateT);
    ///
    float t = mCameraMoveState->currentTime / mCameraMoveState->currentCurveTotalTime;
    bool needChangeCurve = isNeedChangeCurve(t);
    float oldT = t;
    t = interpolateChange(t);
    
    //float nextInterpolateT = getNextInterpolatePoint(t);
    if(!needChangeCurve)
    {
        float ccDist = 0;
        if(oldT > 1)
        {
            float percent = mCameraMoveState->currentLookPointTrackList[mCameraMoveState->currentLookPointTrackList.size() - 1].percent / 100;
            float span = (percent - 1) / 2;
            float ratio = (oldT - 1) / span;
            if(ratio > 1)
                ratio = 1;
            float d = ratio * mCameraForwardingDist;
            ccDist = d;
        }
        SE_Vector3f point;
        float step = t * mCameraMoveState->currentCurve.curveLen;
        mCameraMoveState->currentCurve.curve->setCurrentPoint(0);
        mCameraMoveState->currentCurve.curve->setForwardingStep(step);
        mCameraMoveState->currentCurve.curve->getNextCurvePoint(point);
        SE_Vector3f lookpoint;
        if(mCameraMoveState->bStartCurve)
        {
            lookpoint = getStartCurveLookPoint(t);
        }
        else
        {
            lookpoint = getCurrentLookPoint(t);
        }
        if(!mCameraMoveState->bLeavingGroup)
        {
            point.y = point.y;// + mCurrentGroupIndex * 2 * mYLength;
            lookpoint.y = lookpoint.y;//+ mCurrentGroupIndex * 2 * mYLength;
        }
        else 
        {
            point.y = point.y + mYLength;//(mCurrentGroupIndex - 1) * 2 * mYLength + mYLength;
            lookpoint.y = lookpoint.y + mYLength;//(mCurrentGroupIndex -1 ) * 2 * mYLength + mYLength;
        }
        point.x -= ccDist;
        if(mCameraMoveState->currentCurve.name != "LS1")
            point.z = lookpoint.z;
        
        if(mCameraMoveState->currentCurve.name == "LS1")
        {
            
            float tmpStep =  mCameraMoveState->currentCurve.curveLen;
            SE_Vector3f lastPoint;
            mCameraMoveState->currentCurve.curve->setCurrentPoint(0);
            mCameraMoveState->currentCurve.curve->setForwardingStep(tmpStep);
            mCameraMoveState->currentCurve.curve->getNextCurvePoint(lastPoint);
            
            point.y -= mYLength;
            lookpoint.y -= mYLength;
            SE_Vector3f frameLookPoint = mModelManager->getFrameLookingPoint(frameLookingPointName);
            //LOGI("## last point = %f, %f, %f ###\n", lastPoint.x, lastPoint.y - mYLength, lastPoint.z);
            //LOGI("## look point = %f, %f, %f ###\n", frameLookPoint.x, frameLookPoint.y, frameLookPoint.z);
            if(point.z < frameLookPoint.z)
            {
                //LOGI("## point z < frame looking point z\n");
                point.z = frameLookPoint.z;
            }
        }
        GroupList::iterator groupIt = locationInGroup(point.y);
        if(groupIt != mCurrentLookingPhotoFrameNode.currGroupIt && !mCameraMoveState->bStartCurve)
        {
            LOGI("## group chage ##\n");
            mCurrentLookingPhotoFrameNode.prevGroupIt = mCurrentLookingPhotoFrameNode.currGroupIt;
            mCurrentLookingPhotoFrameNode.currGroupIt = groupIt;
        }
        updateConcernPhotoFrameNode(t);
        findGroupTextureNotLoaded(groupIt);
        SE_Vector3f newZ = getCameraLookingVector(t, point, lookpoint);
        SE_Vector3f zAxis = newZ;//point - lookpoint;
        //LOGI("## current point = %f , %f, %f ###\n", point.x, point.y, point.z);
        //LOGI("## current look point = %f , %f , %f ### \n", lookpoint.x, lookpoint.y, lookpoint.z);
        //LOGI("## current zAxis = %f, %f, %f ###\n", zAxis.x, zAxis.y, zAxis.z);
        zAxis = zAxis.normalize();
        SE_Vector3f yAxis = SE_Vector3f(0, 0, 1);
        updateFogPointByCameraPoint(point);
        //LOGI("### camera point = %f, %f, %f ##\n", point.x, point.y, point.z);
        mCamera->create(point, zAxis, yAxis, CAMERA_FIELD_OF_VIEW, CAMERA_RATIO, 1, CAMERA_FAR_DIST
                        );
    }
    else
    {
        if(mCameraMoveState->bStartCurve)
        {
            mCameraMoveState->bStartCurve = false;
        }
        if(mCameraMoveState->bLeavingGroup)
        {
            mCameraMoveState->bLeavingGroup = false;
            changeGroupWorldTranslate();
        }
        mCameraMoveState->checkLeftOrRightFinish();
        LOGI("## start location name = %s, end name = %s ##\n", mCameraMoveState->startLocationName.c_str(), mCameraMoveState->endLocationName.c_str());
        mCameraMoveState->startLocationName = mCameraMoveState->endLocationName;
        mCameraMoveState->endLocationName = "";
        /*
        SE_Vector3f loc = mCamera->getLocation();
        GroupList::iterator groupIt = locationInGroup(loc.y);
        bool hasLogo = groupIt->hasStationNode;
        PointCurveData pcd = calculatePointCurve(mCameraMoveState->startLocationName, mCameraMoveState->isLeaveGroup(), (hasLogo ? HAS_LOGO : NO_LOGO));
        LOGI("### curve = %s ###\n", pcd.curveName.c_str());
         */
        if(mCameraMoveState->isLeaveGroup())
        {
            mCameraMoveState->clearLeftRightFinish();
            mCameraMoveState->bLeavingGroup = true;
        }
        setCurrentCurveProperty(mNextCurveData.curveName);
        setCurveData(mNextCurveData, mTrackListPhotoType);
        //mCameraForwardingDist = getCurrentCurveForwardingDist();
    }
}
bool SE_Scene::isAllPhotoFrameNodeConcerned(Group& group)
{
    for(int i = 0 ; i < 4 ; i++)
    {
        if(group.photoFrameNode[i].bConcerned == false)
            return false;
    }
    return true;
}
void SE_Scene::updateStartCurvePhotoFrameNode(float t)
{
    PointCurveData pcd = calculatePointCurve("L1", false , NO_LOGO);
    mNextCurveData = pcd;
}
void SE_Scene::updateConcernPhotoFrameNode(float t)
{
    if(mCameraMoveState->bStartCurve)
    {
        updateStartCurvePhotoFrameNode(t);
        return;
    }
    if(mCurrentLookingPhotoFrameNode.nodeChanged == false || mCameraStayStatic == false)
        return;
    LOGI("### update concern point #####\n");

    mCurrentLookingPhotoFrameNode.currGroupIt->photoFrameNode[mCurrentLookingPhotoFrameNode.nodeType].bConcerned = true;
    bool willLeaveGroup = isAllPhotoFrameNodeConcerned(*mCurrentLookingPhotoFrameNode.currGroupIt);
    std::string startLocationName = mCameraMoveState->endLocationName;
    bool hasLogo = mCurrentLookingPhotoFrameNode.currGroupIt->hasStationNode;
    PointCurveData pcd = calculatePointCurve(startLocationName, willLeaveGroup , (hasLogo ? HAS_LOGO : NO_LOGO));
    std::string endLocationName = pcd.pointName;
    int startPictureIndex = 0, endPictureIndex = 0;
    GroupList::iterator nextIt = mCurrentLookingPhotoFrameNode.currGroupIt;
    if(willLeaveGroup)
    {
        LOGI("## will leave group ##\n");
        nextIt++;
        assert(nextIt != mGroupList.end());
    }
    LOGI("## startLocationName = %s, endLocationName = %s ##\n", startLocationName.c_str(), endLocationName.c_str());
    getFullImageIndex(mCurrentLookingPhotoFrameNode.currGroupIt, nextIt, startLocationName, endLocationName, willLeaveGroup, startPictureIndex, endPictureIndex);
    if(startPictureIndex != -1)
    {
        std::list<int>::iterator lastIndexIt = se_list_nref(mFullImagePictureIndexList, mFullImagePictureIndexList.size() - 1);
        int lastIndex = *lastIndexIt;
        LOGI("\n##############\n");
        for(std::list<int>::iterator it = mFullImagePictureIndexList.begin() ; it != mFullImagePictureIndexList.end() ; it++)
        {
            LOGI("%d ", *it);
        }
        LOGI("\n###############\n");
        LOGI("## startPictureIndex = %d , endPictureIndex = %d,lastIndex = %d##\n", startPictureIndex, endPictureIndex, lastIndex);
        assert(startPictureIndex == lastIndex);
        loadFullImage(endPictureIndex);
        mFullImagePictureIndexList.push_back(endPictureIndex);
        deleteFullImageTexture();
    }
    mNextCurveData = pcd;
}

/*
void SE_Scene::renderPoints()
{
    glClearColor(1.0, 1.0, 1.0, 1.0);
    glClear(GL_COLOR_BUFFER_BIT);
    renderPoints(mPointData.data, mPointData.vertexNum, SE_Vector3f(1.0, 0, 0));
    renderPoints(mDynPointData.data, mPointData.vertexNum, SE_Vector3f(0, 0, 1));
}
void SE_Scene::renderPoints(float* vertexData, int vertexNum, SE_Vector3f color)
{
    SS_Shader* shader = mModelManager->getShader("point_shader");
    shader->use();
    SE_Matrix4f m;
    float matrixData[16];
    m = mCamera->getPerspectiveMatrix();
    m.getColumnSequence(matrixData);
    GLint matrixLoc = shader->getUniformLocation("u_proj_matrix");
    glUniformMatrix4fv(matrixLoc, 1, 0, matrixData);
    
    GLint colorLoc = shader->getUniformLocation("u_color");
    glUniform3f(colorLoc, color.x, color.y, color.z);
    
    GLint pointSize = shader->getUniformLocation("u_point_size");
    glUniform1f(pointSize, 2.0);
    
    GLint positionLoc = shader->getAttribLocation("a_position");
    glEnableVertexAttribArray(positionLoc);
    glVertexAttribPointer(positionLoc, 3, GL_FLOAT, GL_FALSE, 0, vertexData);
    
    glDrawArrays(GL_POINTS, 0, vertexNum);
}
 */
/*
int SE_Scene::getMeshByName(const char* name)
{
    NodeList::iterator it ;
    for(int i = 0 ; i < mSceneCount ; i++)
    {
        Scene* scene = mSceneList[i];
        for(it = mNodeList.begin() ; it != mNodeList.end() ; it++)
        {
            //int meshNum = mModelManager->getMeshNum();
            SS_Node* node = *it;
            SE_Mesh* mesh = mModelManager->getMesh(node->meshIndex);
            if(mesh->name == name)
                return node->meshIndex;
        }
    }    
    return -1;
}
*/

void SE_Scene::move(int axis, float v)
{
    if(axis == 1)
    {
        mCamera->translateLocal(SE_Vector3f(v, 0, 0));
    }
    else if(axis == 2)
    {
        mCamera->translateLocal(SE_Vector3f(0, v, 0));
    }
    
}
void SE_Scene::moveCamera(float deltax, float deltay)
{
    //SE_Vector3f loc = mCamera->getLocation();
    //loc.x += deltax;
    //mCamera->setLocation(loc);
    SE_Rect<int> viewport = mCamera->getViewport();
    int viewportWidth = viewport.right - viewport.left;
    float ratio = -180.0f / viewportWidth;
    float angle = ratio * deltax;
    //mCamera->rotateLocal(angle, SE_AXIS_X);
    mCamera->rotateLocal(angle, SE_AXIS_Y);
    mCamera->translateLocal(SE_Vector3f(0, 0, deltay));

}
void SE_Scene::rotateCamera(float angle)
{
}
void SE_Scene::renderWireFrame(int index)
{
    glDisable(GL_DEPTH_TEST);
    SE_Mesh* mesh = mModelManager->getMesh(index);
    SS_Shader* shader = mModelManager->getShader("lineseg_shader");
    shader->use();
    float matrixData[16];
    SE_Matrix4f rotateM;
    rotateM.identity();
    SE_Matrix4f m = mCamera->getPerspectiveMatrix().mul(mCamera->getWorldToViewMatrix().mul(rotateM));
    m.getColumnSequence(matrixData);                            
    GLint mLoc = shader->getUniformLocation("u_wvp_matrix");
    glUniformMatrix4fv(mLoc, 1, 0, matrixData);
    float* data = mesh->getWireframeVertex(mCamera->getAxisZ());
    GLint positionLoc = shader->getAttribLocation("a_position");
    glEnableVertexAttribArray(positionLoc);
    glVertexAttribPointer(positionLoc, 3, GL_FLOAT, GL_FALSE, 0, data);
    GLint colorU = shader->getUniformLocation("u_color");
    glUniform3f(colorU, 0.0, 0, 1.0);
    glDrawArrays(GL_LINES, 0, mesh->mWireFrameVertexNum);
}
void SE_Scene::render(SS_Shader* shader, int index, const SE_Matrix4f& worldM)
{
    SE_Mesh* mesh = mModelManager->getMesh(index);
    float matrixData[16];
    SE_Matrix4f m = mCamera->getPerspectiveMatrix().mul(mCamera->getWorldToViewMatrix().mul(worldM));
    m.getColumnSequence(matrixData);                            
    GLint worldMLoc = shader->getUniformLocation("u_wvp_matrix");
    checkGLError();
    glUniformMatrix4fv(worldMLoc, 1, 0, matrixData);
    checkGLError();

    GLint viewMLoc = shader->getUniformLocation("u_wv_matrix");
    checkGLError();
    m = mCamera->getWorldToViewMatrix().mul(worldM);
    m.getColumnSequence(matrixData);
    glUniformMatrix4fv(viewMLoc, 1, 0, matrixData);
    checkGLError();
    
    GLint densityLoc = shader->getUniformLocation("u_density");
    checkGLError();
    glUniform1f(densityLoc, 0.05);
    
    GLint farLoc = shader->getUniformLocation("far_dist");
    checkGLError();
    glUniform1f(farLoc, -35);
    
    GLint nearLoc = shader->getUniformLocation("near_dist");
    checkGLError();
    glUniform1f(nearLoc, -10);
    
    GLint fogColorLoc = shader->getUniformLocation("u_fog_color");
    checkGLError();
    glUniform3f(fogColorLoc, 1, 1, 1);
    
    
    if(mesh->subMeshNum > 0)
    {
    }
    else
    {
        GLint positionLoc = shader->getAttribLocation("a_position");
        checkGLError();
        glEnableVertexAttribArray(positionLoc);
        checkGLError();
        glBindBuffer(GL_ARRAY_BUFFER, mesh->vboID);
        /*
        float* data = mesh->mDrawingVertex;
        //debug
        for(int i = 0 ; i < mesh->mDrawingVertexNum ; i++)
        {
            float* pos = data + i * _VertexXYZ_UV::vertex_size;
            LOGI("## pos = %f, %f, %f ##\n", pos[0], pos[1], pos[2]);
            SE_Vector4f v = m.map(SE_Vector4f(pos[0], pos[1], pos[2], 1));
            LOGI("## view pos = %f, %f, %f ##\n", v.x, v.y, v.z);
            float density = 0.5f;
            float len = v.xyz().length();
            float factor = density * len;
            factor *= factor;
            float e = 2.71828;
            factor = powf(e, -factor);
            LOGI("## factor = %f ##\n", factor);
        }
         */
        //end
        //glVertexAttribPointer(positionLoc, _VertexXYZ_UV::xyz_size, GL_FLOAT, GL_FALSE, _VertexXYZ_UV::vertex_size * sizeof(float), data + _VertexXYZ_UV::xyz_offset);
        checkGLError();
        glVertexAttribPointer(positionLoc, _VertexXYZ_UV::xyz_size, GL_FLOAT, GL_FALSE, _VertexXYZ_UV::vertex_size * sizeof(float), (const GLvoid*)_VertexXYZ_UV::xyz_offset);
        checkGLError();
        GLint texCoordLoc = shader->getAttribLocation("a_tex_coord");
        checkGLError();
        glEnableVertexAttribArray(texCoordLoc);
        checkGLError();
        //glVertexAttribPointer(texCoordLoc, _VertexXYZ_UV::uv_size, GL_FLOAT, GL_FALSE, _VertexXYZ_UV::vertex_size * sizeof(float), data + _VertexXYZ_UV::uv_offset);
        checkGLError();
        glVertexAttribPointer(texCoordLoc, _VertexXYZ_UV::uv_size, GL_FLOAT, GL_FALSE, _VertexXYZ_UV::vertex_size * sizeof(float), (const GLvoid*)(_VertexXYZ_UV::uv_offset * sizeof(float)));
        checkGLError();
        SE_Material* material = mModelManager->getMaterial(mesh->materialIndex);
        if(material)
        {
            SE_Texture* t = mModelManager->getTexture(material->materialData.texturename.c_str());
            glPixelStorei(GL_UNPACK_ALIGNMENT,1);
            checkGLError();
            glActiveTexture(GL_TEXTURE0);
            checkGLError();
           // glEnable(GL_TEXTURE_2D);
           // checkGLError();
            if(!t)
            {
                t = new SE_Texture;
                loadTexture(material->materialData.texturename.c_str(), t);
                checkGLError();
                mModelManager->setTexture(material->materialData.texturename.c_str(), t);
            }
            glBindTexture(GL_TEXTURE_2D, t->texture);
            checkGLError();
            GLint texLoc = shader->getUniformLocation("u_texture");
            checkGLError();
            GLint shadeModeLoc = shader->getUniformLocation("u_shading_mode");
            checkGLError();
            glUniform1i(texLoc, 0);
            checkGLError();
            glUniform1i(shadeModeLoc, 1);
            checkGLError();
        }
        glDrawArrays(GL_TRIANGLES, 0, mesh->mDrawingVertexNum);
        checkGLError();
    }
    
}
void SE_Scene::renderFogPlane()
{
    SS_Shader* shader = mModelManager->getShader("fog_plane_shader");
    shader->use();
    glBindBuffer(GL_ARRAY_BUFFER, mFogPlaneMesh->vboID);
    SE_Matrix4f m = mCamera->getPerspectiveMatrix();
    float matrixData[16];
    m.getColumnSequence(matrixData);
    GLint matrixLoc = shader->getUniformLocation("u_proj_matrix");
    glUniformMatrix4fv(matrixLoc, 1, 0, matrixData);
    
    GLint alphaLoc = shader->getUniformLocation("u_alpha");
    glUniform1f(alphaLoc, 0.8);
    
    GLint posLoc = shader->getAttribLocation("a_position");
    glEnableVertexAttribArray(posLoc);
    glVertexAttribPointer(posLoc, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(float), 0);
    
    GLint colorLoc = shader->getAttribLocation("a_color");
    glEnableVertexAttribArray(colorLoc);
    glVertexAttribPointer(colorLoc, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(float), (const GLvoid*)(3 * sizeof(float)));
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDrawArrays(GL_TRIANGLES, 0, mFogPlaneMesh->mDrawingVertexNum);
    glDisable(GL_BLEND);
}