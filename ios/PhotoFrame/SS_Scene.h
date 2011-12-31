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
    SE_Scene(SS_ModelManager* modelManager, int w, int h, int sceneCount = 1);
    ~SE_Scene();
    void create();
    void create(char** pictureName, int pictureNum);
    void clear();
    void renderScene();
    void render();
    void moveCamera(float deltax, float deltay);
    void rotateCamera(float angle);
    // axis: 1 --> x  , 2 --> y
    void move(int axis, float v);
    //for test
    void createPoints();
    void renderPoints();
    void renderPoints(float* vertexData, int vertexNum, SE_Vector3f color);
    void createCurveFromPoints(SE_Vector3f* points , int pointNum, int curvePointNum = 1000);
    //end
    //
    
    struct DisplayObject
    {
        int meshIndex;
        std::string shaderName;
        int vertexType;
        bool bPicturePlace;
        std::string pictureName;
        SE_Texture* pictureTexture;
        DisplayObject()
        {
            meshIndex = -1;
            vertexType = 0;
            bPicturePlace = false;
            pictureTexture = NULL;
        }
    };
    typedef std::list<DisplayObject> DisplayObjectList;
    struct PhotoFrameNode
    {
        DisplayObjectList objList;
        SE_Matrix4f worldM;
        bool mirror;
        PhotoFrameNode()
        {
            mirror = false;
        }
    };
    struct StationNode
    {
        DisplayObjectList objList;
        SE_Matrix4f worldM;
    };
    struct StaticNode
    {
        DisplayObjectList objList;
        SE_Matrix4f worldM;
    };
    void startCameraMove();
    void createTrackList();
    void resetCameraMove();
    int mTrackPointsIndex;
private:
    void initCamera();
    void initFog();
    void initMeshVBO();
    void initRootMatrix();
    void initMeshMap();
    void initBound();
    PhotoFrameNode createPhotoFrameNode(PHOTO_TYPE pt, char* pictureName);
    StationNode createStationNode();
    void placePhotoFrameNodeToWorld(PhotoFrameNode pfn, int index, bool mirror);
    void placeStationNodeToWorld(StationNode sn, int groupIndex);
    int getGroupNum(int pictureNum);
    PHOTO_TYPE getPhotoType(const char* pictureNmae);
    void setPhotoFrameNode(char** pictureName, int pictureIndex, int pictureNum, int nodeIndex,bool mirror);
    void renderPhotoFrameNode(PhotoFrameNode* pfn);
    void renderStationNode(StationNode* sn);
    void renderObject(DisplayObject* obj, const SE_Matrix4f& worldM);
    std::string getMeshName(int index);
    StaticNode createStaticNode();
    void placeStaticNodeToWorld(StaticNode sn, int nodeIndex);
    void renderStaticNode(StaticNode* sn);
private:
    void createNode();
    void calculateBoundingBox(int index);
    void createScene();
    void renderWireFrame(int index);
    void render(SS_Shader* shader, int index, const SE_Matrix4f& worldM);
    int getMeshByName(const char* name);
    void createBoundingBox();
    void renderFogPlane();
    void testCurve(SE_Curve* curve);
private:
    typedef std::list<PhotoFrameNode> PhotoFrameNodeList;
    typedef std::list<StationNode> StationNodeList;
    typedef std::list<StaticNode> StaticNodeList;
    typedef std::map<std::string, int> MeshMap;
    SE_Camera* mCamera;
    SS_ModelManager* mModelManager;
    float mScreenWidth;
    float mScreenHeight;
    MeshMap mMeshMap;
    PhotoFrameNodeList mPhotoFrameNodeList;
    PhotoFrameNodeList mMirrorPhotoFrameNodeList;
    StaticNodeList mStaticNodeList;
    StationNodeList mStationNodeList;
    SE_Vector3f mPhotoFrameBoundMin, mPhotoFrameBoundMax;
    SE_Matrix4f mRootNodeMatrix;
    SE_Mesh* mFogPlaneMesh;
    int mMiniGroupNum;
    SE_Curve* mCurve;
    //for camera move
    SE_Vector3f mFloorDownMin;
    SE_Vector3f mFloorDownMax;
    std::vector<SE_Vector3f> mTrackPoints1;
    std::vector<SE_Vector3f> mTrackPoints2;
    //int mTrackPointsIndex;
    SE_Vector3f mStartVector;
    SE_Vector3f mMidVector;
    SE_Vector3f mEndVector;
    SE_Vector3f mCameraX;
    SE_Vector3f mCameraY;
    SE_Vector3f mCameraZ;
    std::vector<SE_Vector3f> mCameraZArray;
    std::vector<SE_Vector3f> mCameraZArray2;
    //end
    //obsolete
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
};


#endif
