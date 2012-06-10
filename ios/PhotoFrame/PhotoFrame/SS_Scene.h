//
//  SS_Scene.h
//  PhotoFrame
//
//  Created by 陈勇 on 11-11-26.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#ifndef SS_Scene_h
#define SS_Scene_h
#include <list>
#include <vector>
#include <string>
#include <map>
#include "SE_Vector.h"
#include "SE_Matrix.h"
#include "SE_Geometry3D.h"
class SE_Camera;
class SS_ModelManager;
class SS_Shader;
class SE_Matrix4f;
class SE_Mesh;
class SE_Curve;
class SE_Texture;
class SE_Scene
{
public:
    enum {GROUP_OBJ_NUM = 4};
    enum PHOTO_TYPE {PHOTOV, PHOTOH};
    enum USAGE {NO_USE, PICTURE_PLACE};
    enum PHOTO_FRAME_NODE_TYPE {SW, SE, NW, NE, INVALID_PHOTO_FRAME_NODE_TYPE};
    enum STATIC_NODE_TYPE {DOWN, UP, INVALID_STATIC_NODE_TYPE};
    enum LOGO_STATE {NO_LOGO, HAS_LOGO, NO_CONCERN_LOGO};
    enum LOOKING_POINT_TYPE {LEFT_SIDE, RIGHT_SIDE, NO_SIDE};
    enum MIRROR_TYPE {HAS_MIRROR_CW, HAS_MIRROR_CCW, NO_MIRROR};
    struct PictureID
    {
        std::string pictureName;
        std::string pictureDate;
        int orientation;
        PHOTO_TYPE photoType;
        PictureID()
        {
            orientation = 0;
            photoType = PHOTOH;
        }
    };
    SE_Scene(SS_ModelManager* modelManager, void* vn, int w, int h, int sceneCount = 1);
    ~SE_Scene();
    void create(std::vector<PictureID>& pictureIDs, int pictureNum);
    //void clear();
    void renderScene();
    //void render();
    void moveCamera(float deltax, float deltay);
    void rotateCamera(float angle);
    // axis: 1 --> x  , 2 --> y
    void move(int axis, float v);
    //for test
    //void incTrackPointIndex();
    void createPoints();
    void renderPoints();
    void renderPoints(float* vertexData, int vertexNum, SE_Vector3f color);
    void createCurveFromPoints(SE_Vector3f* points , int pointNum, int curvePointNum = 1000);
    //end
    //

    struct DisplayObject
    {
        bool bConcerned;
        int meshIndex;
        std::string shaderName;
        int vertexType;
        bool bPicturePlace;
        //MIRROR_TYPE mirror;
        bool mirror;
        int pictureIndex;
        float uv[8];
        int objID;
        SE_Matrix4f worldM;
        DisplayObject()
        {
            //mirror = NO_MIRROR;
            mirror = false;
            bConcerned = false;
            pictureIndex = -1;
            meshIndex = -1;
            vertexType = 0;
            bPicturePlace = false;
            objID = 0;
            for(int i = 0 ; i < 8 ; i++)
            {
                uv[i] = 0;
            }
        }
    };
    typedef std::list<DisplayObject> DisplayObjectList;
    struct PhotoFrameNode
    {
        DisplayObjectList objList;
        SE_Matrix4f worldM;
        bool mirror;
        SE_AABB aabb;
        PHOTO_FRAME_NODE_TYPE type;
        bool bConcerned;
        PhotoFrameNode()
        {
            mirror = false;
            bConcerned = false;
            type = INVALID_PHOTO_FRAME_NODE_TYPE;
        }
        ~PhotoFrameNode()
        {

        }
        PhotoFrameNode copyProperty()
        {
            PhotoFrameNode pfn;
            pfn.worldM = worldM;
            pfn.mirror = mirror;
            pfn.aabb = aabb;
            pfn.type = type;
            pfn.bConcerned = bConcerned;
            return pfn;
        }
    };
    struct StationNode
    {
        DisplayObjectList objList;
        SE_Matrix4f worldM;
        SE_AABB aabb;
        StationNode()
        {
        }
        ~StationNode()
        {
        }
    };
    struct StaticNode
    {
        DisplayObjectList objList;
        SE_Matrix4f worldM;
        SE_AABB aabb;
        STATIC_NODE_TYPE type;
        StaticNode()
        {
            type = INVALID_STATIC_NODE_TYPE;
        }
        ~StaticNode()
        {
        }
    };
    struct CurveData
    {
        float time;
        float curveLen;
        //float moveSpeed;// the distance every ms.
        SE_Curve* curve;
        std::string name;
        int composePointNum; // this is the point number used to make the curve, we use every line between compose point to simulate a curve, the more this number, more realistic the curve
        //std::list<SE_Vector3f> trackPoints;
        CurveData()
        {
            time = 0;
            curveLen = 0;
            curve = NULL;
            composePointNum = 0;
        }
    };
    struct CameraPathProperty
    {
        float time; // time unit if second
        std::string name;
    };
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
    struct Group
    {
        PhotoFrameNode photoFrameNode[4];
        StaticNode staticNode[2];
        StationNode stationNode;
        bool hasStationNode;
        bool isValid;
        Group()
        {
            isValid = false;
            hasStationNode = false;
        }
    };
    struct PointCurveData
    {
        std::string curveName;
        std::string pointName;
        int time; //seconds
        PointCurveData(const std::string& pointName, const std::string& curveName, int time)
        {
            this->pointName = pointName;
            this->curveName = curveName;
            this->time = time;
        }
    };
    struct LookingPhotoFrameNode
    {
        //Group prevGroup;
        PHOTO_FRAME_NODE_TYPE prevNodeType;
        //int mGroupIndex;
        std::list<Group>::iterator currGroupIt;
        std::list<Group>::iterator prevGroupIt;
        bool hasFullTexture;
        PHOTO_FRAME_NODE_TYPE nodeType;
        LOOKING_POINT_TYPE side;
        bool nodeChanged;
        //bool groupChanged;
        LookingPhotoFrameNode()
        {
            //groupChanged = false;
            nodeChanged = false;
            hasFullTexture = false;
            side = NO_SIDE;
            //mGroupIndex = 0;
            //mPrevGroupIndex = 0;
            prevNodeType = SW;
            nodeType = SW;
        }
    };
    ////////
    void startCameraMove();
    //void createTrackList();
    //void resetCameraMove();
    void setCurvePath(const std::vector<CameraPathProperty>& paths);
    void updateCameraMove(float deltaTime);
    void removeAllTexture();
    void removeAllShaderFromGL();
    void removeAllMeshVBO();
    void addPointEdge(std::list<std::string>& edge);
    //int mTrackPointsIndex;
private:
    struct CameraMoveState;
    typedef std::list<PhotoFrameNode> PhotoFrameNodeList;
    typedef std::list<StationNode> StationNodeList;
    typedef std::list<StaticNode> StaticNodeList;
    typedef std::list<Group> GroupList;
    typedef std::map<std::string, int> MeshMap;
    void initCamera();
    void initFog();
    void initMeshVBO();
    void initRootMatrix();
    void initMeshMap();
    void initBound();
    PhotoFrameNode createPhotoFrameNode(PHOTO_TYPE pt, int pictureIndex);
    //op: 0 : ++, 1 : --
    PhotoFrameNode createPhotoFrameNode(float nodey, PHOTO_FRAME_NODE_TYPE nodeType, int op , int& lastPictureIndex);
    //void placePhotoFrameNodeToWorld(PhotoFrameNode pfn, float nodey, bool mirror);
    
    StationNode createStationNode(float nodey);
    //void placeStationNodeToWorld(StationNode sn, int groupIndex);
    
    StaticNode createStaticNode(float nodey, STATIC_NODE_TYPE staticType);
    //void placeStaticNodeToWorld(StaticNode sn, float nodey);
    
    int getGroupNum(int pictureNum);
    //void setPhotoFrameNode(int pictureIndex, float nodey , bool mirror);

    void renderPhotoFrameNode(PhotoFrameNode* pfn);
    void renderStationNode(StationNode* sn);
    void renderStaticNode(StaticNode* sn);
    void renderObject(DisplayObject* obj, const SE_Matrix4f& worldM);
    std::string getMeshName(int index);
    bool isRender(PhotoFrameNode* pfn);
    bool isRender(StationNode* sn);
    bool isRender(StaticNode* sn);
    bool isRender(DisplayObject*, const SE_Matrix4f& worldM);
    void createRenderNode();
    void createRenderNode(GroupList::iterator begin, GroupList::iterator end);
    void clearRenderNode();
    void createCurve();
    // time is the the total time traverse the curve, unit is second
    // speed unit is normalized length every second
    void createCurve(const std::string& name, int composedPointNum, float time, float speed);
    
    //for camera move
    bool isNeedChangeCurve(float t);
    SE_Vector3f getCurrentLookPoint(float t);
    CurveData getCurveData(const std::string& name);
    void setCurveData(int index);
    void updateFogPointByCameraPoint(const SE_Vector3f& cameraLoc);
    void createYInverseCurve(const std::string& curveName, const std::string& inverseCurveName,int composedPointNum, float time, float speed);
    std::string removeSufix(const std::string& name, const std::string& sufix);
    int calculateGroup(std::vector<PictureID>& pictures);
    float calculatePhotoFrameNodeY(int groupIndex, PHOTO_FRAME_NODE_TYPE nodeType);
    int calculatePhotoFramePictureIndex(int groupIndex, PHOTO_FRAME_NODE_TYPE nodeType);
    float calculateStaticNodeY(int groupIndex, STATIC_NODE_TYPE staticType);
    float calculateStationNodeY(int groupIndex);
    void createPointEdges();
    bool isPhotoFrameNodeMirror(PHOTO_FRAME_NODE_TYPE nodeType) const ;
    int locationInGroup(const SE_Vector3f& loc);
    bool hasStation(int groupIndex);
    //typedef std::pair<std::string, std::string> PointCurveData;
    PointCurveData calculatePointCurve(const std::string& point, bool leaveGroup, LOGO_STATE logoState);
    typedef std::list<Edge> EdgeList;
    EdgeList getEdgeList(const std::string& point);
    typedef std::list<Point> PointList;
    void setCurveData(const PointCurveData& pcd);
    void sortPictureByID();
    void seperatePictureFromPhotoFrameNode();
    void updateConcernPhotoFrameNode();
    SE_Texture* getFullImageTexture(const std::string& imageName, const std::string& textureID);
    void deleteFullImageTexture();
    float interpolateChange(float t);
    void printPointEdge();
    void createPhotoFrameNodeForNegativeSide();
    int calculateNegativeGroup(std::vector<PictureID>& pictures);
    int calculateGroup(std::vector<PictureID>& pictures, float maxDist);
    GroupList::iterator locationInGroup(float y);
    void changeGroupWorldTranslate();
    template <typename T>
    void moveTranslateY(T& node, float y)
    {
        SE_Matrix4f m = node.worldM;
        float f = m.get(1, 3);
        m.set(1, 3, f + y);
        node.worldM = m;
    }
    void createDisplayObjectListForRender(PhotoFrameNode& pfn);
    void createDisplayObjectListForRender(StaticNode& sn);
    void createDisplayObjectListForRender(StationNode& sn);
    void sortDisplayObjectList();
    //void bindMeshVao(GLuint* vaoID, GLuint* vboID, bool mirror);
private:
    //void createNode();
    //void calculateBoundingBox(int index);
    //void createScene();
    void renderWireFrame(int index);
    void render(SS_Shader* shader, int index, const SE_Matrix4f& worldM);
    //int getMeshByName(const char* name);
    //void createBoundingBox();
    void renderFogPlane();
    void testCurve(SE_Curve* curve);
private:

    void* mViewNav;
    SE_Camera* mCamera;
    SS_ModelManager* mModelManager;
    float mScreenWidth;
    float mScreenHeight;
    MeshMap mMeshMap;
    GroupList mGroupList;
    GroupList mNegativeGroupList;
    int mLastPictureIndex;
    DisplayObjectList mDisplayObjectList;
    PhotoFrameNodeList mPhotoFrameNodeList;
    PhotoFrameNodeList mMirrorPhotoFrameNodeList;
    PhotoFrameNodeList mPictureObjectList;
    StaticNodeList mStaticNodeList;
    StationNodeList mStationNodeList;
    SE_Vector3f mPhotoFrameBoundMin, mPhotoFrameBoundMax;
    SE_Matrix4f mRootNodeMatrix;
    SE_Mesh* mFogPlaneMesh;
    int mMiniGroupNum;
    int mRenderObjectCount;
    float mYLength;
    SE_Vector3f mFirstFogPoint;
    SE_Vector3f mCurrentFogPoint;
    std::list<CurveData> mCurveData;
    SE_Vector3f mStartRef;
    CameraMoveState* mCameraMoveState;
    bool mCameraStayStatic;
    LookingPhotoFrameNode mCurrentLookingPhotoFrameNode;
    //for camera move
    SE_Curve* mCurve;
    SE_Vector3f mFloorDownMin;
    SE_Vector3f mFloorDownMax;
    std::vector<SE_Vector3f> mTrackPoints1;
    std::vector<SE_Vector3f> mTrackPoints2;
    SE_Vector3f mStartVector;
    SE_Vector3f mMidVector;
    SE_Vector3f mEndVector;
    SE_Vector3f mCameraX;
    SE_Vector3f mCameraY;
    SE_Vector3f mCameraZ;
    std::vector<SE_Vector3f> mCameraZArray;
    std::vector<SE_Vector3f> mCameraZArray2;
    std::vector<PictureID> mPictureIDVector;
    int mGroupNum;
    PointList mPointList;
    int mCurrentGroupIndex;
    //end
    //obsolete
    /*
    struct Points
    {
        float* data;
        int vertexNum;
        int floatSize;
    };
    int mSceneCount;
    
    class SS_Node;
    typedef std::list<SS_Node*> NodeList;
    NodeList mNodeList;
    class Scene;
    typedef std::vector<Scene*> SceneList;
    SceneList mSceneList;
    float mBoundY;
    SE_Vector3f mMin, mMax;
    //for test
    Points mPointData;
    Points mDynPointData;
    //end
     */
};


#endif
