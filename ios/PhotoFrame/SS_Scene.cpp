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
#include <string>
#include <math.h>

struct DisplayObjectProperty
{
    char meshName[256];
    int vertexType;
    char shaderName[256];
    SE_Scene::USAGE usage;
};
const static char* worldObjName[] = {"BackWall","WallFrameH_001","FrameH_001","photoH001","WallFrameV_001","FrameV_001","photoV001","thespeedsunPlatform","logo001", "floordown001"};
static DisplayObjectProperty photoFrameH[] = {
    {"BackWall", SE_Mesh::XYZ_UV, "fog_shader", SE_Scene::NO_USE}, 
    {"WallFrameH_001", SE_Mesh::XYZ_UV, "fog_shader", SE_Scene::NO_USE}, 
    {"FrameH_001", SE_Mesh::XYZ_UV, "fog_shader", SE_Scene::NO_USE}, 
    {"photoH001", SE_Mesh::XYZ_UV1_UV2, "picture_fog_shader", SE_Scene::PICTURE_PLACE}};
static DisplayObjectProperty photoFrameV[] = {
    {"BackWall", SE_Mesh::XYZ_UV, "fog_shader", SE_Scene::NO_USE}, 
    {"WallFrameV_001", SE_Mesh::XYZ_UV, "fog_shader", SE_Scene::NO_USE}, 
    {"FrameV_001", SE_Mesh::XYZ_UV, "fog_shader", SE_Scene::NO_USE}, 
    {"photoV001", SE_Mesh::XYZ_UV1_UV2, "picture_fog_shader", SE_Scene::PICTURE_PLACE}};
static DisplayObjectProperty stationObject[] = {
    {"thespeedsunPlatform", SE_Mesh::XYZ_UV, "fog_shader", SE_Scene::NO_USE}, 
    {"logo001", SE_Mesh::XYZ_UV, "fog_shader", SE_Scene::NO_USE}};

static DisplayObjectProperty staticObject[] = {
    {"floordown001", SE_Mesh::XYZ_UV, "fog_shader", SE_Scene::NO_USE}
};
static const char* frameLookingPointName = "lookingPoint_frame";
#define HAS_PICTURE 0
#define HAS_NO_PICTURE 1
static const char* pictureShaderName[]= {"picture_fog_shader", "fog_shader"};
static int photoFrameHObjectNum = sizeof(photoFrameH) / sizeof(DisplayObjectProperty);
static int photoFrameVObjectNum = sizeof(photoFrameV) / sizeof(DisplayObjectProperty);
static int stationObjectNum = sizeof(stationObject) / sizeof(DisplayObjectProperty);
static void checkGLError()
{
    
    GLenum error = glGetError();
    if(error != GL_NO_ERROR)
    {
        LOGI("### gl error = %d ####\n", error);
        SE_ASSERT(0);
    }
    
}
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
///////
class SE_Scene::Scene
{
public:
    SE_Matrix4f translate;
    std::list<SE_Scene::SS_Node*> NodesList;
    NodeList nodeList;
};
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
SE_Scene::SE_Scene(SS_ModelManager* modelManager, int w, int h, int sceneCount)
{
    mCamera = new SE_Camera;
    mScreenWidth = w;
    mScreenHeight = h;
    mModelManager = modelManager;
    mMiniGroupNum = 5;
    mTrackPointsIndex = -1;
    ///obsolete
    mBoundY = 0;
    mSceneCount = sceneCount;
    mSceneList.resize(sceneCount);
    //end
    initMeshMap();
    initCamera();
    initFog();
    initRootMatrix();
    initBound();
}
SE_Scene::~SE_Scene()
{
    delete mCamera;
    delete mFogPlaneMesh;
}
void SE_Scene::initCamera()
{
    mCamera->create(SE_Vector3f(4, -7, 4),  SE_Vector3f(0, 0, 1), SE_Vector3f(-1, 0, 0), SE_Vector3f(0, -1, 0), 45, mScreenHeight/ mScreenWidth, 1, 500);
    mCamera->setViewport(0, 0, mScreenWidth, mScreenHeight);
}
void SE_Scene::initFog()
{
    SE_GeometryData geomData = SE_GeometryData::createZAlignRect(10, 10, -35);
    mModelManager->addGeometryData(geomData);
    mFogPlaneMesh = new SE_Mesh;
    mFogPlaneMesh->geomDataIndex = mModelManager->getGeometryDataNum() - 1;
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
    //rotate3f.setRotateY(90);
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
SE_Scene::PhotoFrameNode SE_Scene::createPhotoFrameNode(PHOTO_TYPE p, char* pictureName)
{
    int count = 0; 
    PhotoFrameNode pfn;
    DisplayObjectProperty* data = NULL;
    if(p == PHOTOV)
    {
        count = sizeof(photoFrameV) / sizeof(DisplayObjectProperty);
        data = photoFrameV;
    }
    else if(p == PHOTOH)
    {
        count = sizeof(photoFrameH) / sizeof(DisplayObjectProperty);
        data = photoFrameH;
    }
    for(int i = 0 ; i < count ; i++)
    {
        DisplayObjectProperty dop = data[i];
        DisplayObject obj;
        obj.meshIndex = mMeshMap[dop.meshName];
        obj.vertexType = dop.vertexType;
        obj.shaderName = dop.shaderName;
        obj.bPicturePlace = dop.usage == SE_Scene::PICTURE_PLACE;
        if(obj.bPicturePlace)
        {
            if(pictureName)
            {
                obj.pictureName = pictureName;
                obj.shaderName = pictureShaderName[HAS_PICTURE];
            }
            else
            {
                obj.shaderName = pictureShaderName[HAS_NO_PICTURE];
                obj.bPicturePlace = false;
            }
        }
        pfn.objList.push_back(obj);
    }
    return pfn;
}
SE_Scene::StaticNode SE_Scene::createStaticNode()
{
    int count = sizeof(staticObject) / sizeof(DisplayObjectProperty);
    StaticNode sn;
    for(int i = 0 ; i < count ; i++)
    {
        DisplayObjectProperty dop = staticObject[i];
        DisplayObject obj;
        obj.meshIndex = mMeshMap[dop.meshName];
        obj.vertexType = dop.vertexType;
        obj.shaderName = dop.shaderName;
        obj.bPicturePlace = dop.usage == SE_Scene::PICTURE_PLACE;
        sn.objList.push_back(obj);
    }   
    return sn;
}
void SE_Scene::placeStaticNodeToWorld(StaticNode sn, int nodeIndex)
{
    float photoFrameBoundY = mPhotoFrameBoundMax.y - mPhotoFrameBoundMin.y;
    sn.worldM.identity();
    sn.worldM.setColumn(3, SE_Vector4f(0, nodeIndex * photoFrameBoundY, 0, 1));
    mStaticNodeList.push_back(sn);
}
SE_Scene::StationNode SE_Scene::createStationNode()
{
    int count = sizeof(stationObject) / sizeof(DisplayObjectProperty);
    StationNode sn;
    for(int i = 0 ; i < count ; i++)
    {
        DisplayObjectProperty dop = stationObject[i];
        DisplayObject obj;
        obj.meshIndex = mMeshMap[dop.meshName];
        obj.vertexType = dop.vertexType;
        obj.shaderName = dop.shaderName;
        obj.bPicturePlace = dop.usage == SE_Scene::PICTURE_PLACE;
        sn.objList.push_back(obj);
    }   
    return sn;
}
void SE_Scene::placePhotoFrameNodeToWorld(PhotoFrameNode pfn, int index, bool mirror)
{
    SE_Matrix3f baseM;
    baseM.identity();
    if(mirror)
    {
        baseM.set(0, 0, -1);
    }
    float photoFrameBoundY = mPhotoFrameBoundMax.y - mPhotoFrameBoundMin.y;
    SE_Vector3f translate(0, index * photoFrameBoundY , 0);
    pfn.worldM.set(baseM, SE_Vector3f(1, 1, 1), translate);
    pfn.worldM = mRootNodeMatrix.mul(pfn.worldM);
    pfn.mirror = mirror;
    if(!mirror)
        mPhotoFrameNodeList.push_back(pfn);
    else
        mMirrorPhotoFrameNodeList.push_back(pfn);

}
void SE_Scene::placeStationNodeToWorld(StationNode sn, int groupIndex)
{
    SE_Matrix4f base;
    base.identity();
    float span = 2 * (mPhotoFrameBoundMax.y - mPhotoFrameBoundMin.y);
    float starty = mPhotoFrameBoundMax.y;
    base.setColumn(0, SE_Vector4f(1, 0, 0, 0));
    base.setColumn(1, SE_Vector4f(0, 1, 0, 0));
    base.setColumn(2, SE_Vector4f(0, 0, 1, 0));
    base.setColumn(3, SE_Vector4f(0, starty + groupIndex * span, 0, 1));
    sn.worldM = base;
    sn.worldM = mRootNodeMatrix.mul(sn.worldM);
    mStationNodeList.push_back(sn);
}
int SE_Scene::getGroupNum(int pictureNum)
{
    int totalNum = mMiniGroupNum * SE_Scene::GROUP_OBJ_NUM;
    if(pictureNum < totalNum)
        return 2 * mMiniGroupNum;    
    int base = pictureNum / totalNum;
    int reminder = pictureNum % totalNum;
    int groupNum = base + 1;
    if(reminder > 0)
    {
        groupNum++;
    }
    return groupNum * mMiniGroupNum;
}
SE_Scene::PHOTO_TYPE SE_Scene::getPhotoType(const char* pictureNmae)
{
    int width = 0;
    int height = 0;
    SS_GetImageSize(pictureNmae, &width, &height);
    if(width >= height)
        return PHOTOH;
    else
        return PHOTOV;
}
void SE_Scene::setPhotoFrameNode(char** pictureName, int pictureIndex, int pictureNum, int nodeIndex,bool mirror)
{
    if(pictureIndex < pictureNum)
    {
        PHOTO_TYPE pt = getPhotoType(pictureName[pictureIndex]);
        PhotoFrameNode pfn = createPhotoFrameNode(pt, pictureName[pictureIndex]);
        placePhotoFrameNodeToWorld(pfn, nodeIndex, mirror);
    }
    else
    {
        PhotoFrameNode pfn = createPhotoFrameNode(PHOTOH, NULL);
        placePhotoFrameNodeToWorld(pfn, nodeIndex, mirror);
    }
}
void SE_Scene::create(char** pictureName, int pictureNum)
{
    int groupNum = getGroupNum(pictureNum);
    int pictureIndex = 0;
    int nodeIndex = 0;
    for(int i = 0 ; i < groupNum ; i++)
    {
        setPhotoFrameNode(pictureName, pictureIndex++, pictureNum, nodeIndex, false);
        setPhotoFrameNode(pictureName, pictureIndex++, pictureNum, nodeIndex, true);
        StaticNode sn = createStaticNode();
        placeStaticNodeToWorld(sn, nodeIndex);
        nodeIndex++;
        setPhotoFrameNode(pictureName, pictureIndex++, pictureNum, nodeIndex, false);
        setPhotoFrameNode(pictureName, pictureIndex++, pictureNum, nodeIndex, true);
        sn = createStaticNode();
        placeStaticNodeToWorld(sn, nodeIndex);
        nodeIndex++;
    }
    int sectionNum = groupNum / mMiniGroupNum;
    for(int i = 0 ; i < sectionNum ; i++)
    {
        int groupIndex = 2;
        StationNode sn = createStationNode();
        placeStationNodeToWorld(sn, groupIndex + i * mMiniGroupNum);
    }
}
void SE_Scene::renderScene()
{
    glClearColor(1, 1, 1, 1);
    checkGLError();
    glClear(GL_COLOR_BUFFER_BIT);
    checkGLError();
    glClearColor(1.0f, 1.0f, 1.0f, 1.0f);
    glClear(GL_DEPTH_BUFFER_BIT);
    
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
    glFrontFace(GL_CCW);
    checkGLError();
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
    //renderFogPlane();
}
void SE_Scene::renderStaticNode(StaticNode* sn)
{
    glFrontFace(GL_CCW);
    DisplayObjectList::iterator it;
    for(it = sn->objList.begin() ; it != sn->objList.end() ; it++)
    {
        DisplayObject obj = *it;
        SS_Shader* shader = mModelManager->getShader(obj.shaderName.c_str());
        shader->use();
        renderObject(&obj, sn->worldM);
    }    
}
void SE_Scene::renderStationNode(StationNode* sn)
{
    glFrontFace(GL_CCW);
    DisplayObjectList::iterator it;
    for(it = sn->objList.begin() ; it != sn->objList.end() ; it++)
    {
        DisplayObject obj = *it;
        SS_Shader* shader = mModelManager->getShader(obj.shaderName.c_str());
        shader->use();
        renderObject(&obj, sn->worldM);
    }
}
void SE_Scene::renderPhotoFrameNode(PhotoFrameNode* pfn)
{
    if(pfn->mirror)
    {
        glFrontFace(GL_CW);
    }
    else
    {
        glFrontFace(GL_CCW);
    }
    DisplayObjectList::iterator itObj;
    for(itObj = pfn->objList.begin() ; 
        itObj != pfn->objList.end() ; 
        itObj++)
    {
        DisplayObject obj = *itObj;
        std::string name = getMeshName(obj.meshIndex);
        //if(name == "photoV001" || name == "photoH002")
        //    continue;
        
        SS_Shader* shader = mModelManager->getShader(obj.shaderName.c_str());
        shader->use();
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
void SE_Scene::renderObject(DisplayObject* obj, const SE_Matrix4f& worldM)
{
    int meshIndex = obj->meshIndex;
    SE_Mesh::VERTEX_TYPE vertexType = (SE_Mesh::VERTEX_TYPE)obj->vertexType;
    bool isPicturePlace = obj->bPicturePlace;
    SE_Mesh* mesh = mModelManager->getMesh(meshIndex);
    float* data = mesh->getDrawingVertex(vertexType);
    createMeshVBO(mesh);
    SE_VertexProperty vp = mesh->getVertexProperty(vertexType);
    glBindBuffer(GL_ARRAY_BUFFER, mesh->vboID);
    SS_Shader* shader = mModelManager->getShader(obj->shaderName.c_str());
    shader->use();
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
    glUniform1f(farLoc, -200);
    
    GLint nearLoc = shader->getUniformLocation("near_dist");
    checkGLError();
    glUniform1f(nearLoc, -100);
    
    GLint fogColorLoc = shader->getUniformLocation("u_fog_color");
    checkGLError();
    glUniform3f(fogColorLoc, 1, 1, 1);

    GLint positionLoc = shader->getAttribLocation("a_position");
    checkGLError();
    glEnableVertexAttribArray(positionLoc);
    checkGLError();
    glVertexAttribPointer(positionLoc, SE_VertexProperty::XYZ_SIZE, GL_FLOAT, GL_FALSE, vp.stride, (const GLvoid*)vp.xyzOffset);
    checkGLError();
    if(isPicturePlace)
    {  
        GLint texCoordLoc1 = shader->getAttribLocation("a_tex_coord1");
        checkGLError();
        glEnableVertexAttribArray(texCoordLoc1);
        checkGLError();
        glVertexAttribPointer(texCoordLoc1, SE_VertexProperty::UV_SIZE, GL_FLOAT, GL_FALSE, vp.stride, (const GLvoid*)(vp.uv1Offset * sizeof(float)));
        checkGLError();
    
        GLint texCoordLoc2 = shader->getAttribLocation("a_tex_coord2");
        checkGLError();
        glEnableVertexAttribArray(texCoordLoc2);
        checkGLError();
        glVertexAttribPointer(texCoordLoc2, SE_VertexProperty::UV_SIZE, GL_FLOAT, GL_FALSE, vp.stride, (const GLvoid*)(vp.uv2Offset * sizeof(float)));
        checkGLError();
    }
    else
    {
        GLint texCoordLoc = shader->getAttribLocation("a_tex_coord");
        checkGLError();
        glEnableVertexAttribArray(texCoordLoc);
        checkGLError();
        glVertexAttribPointer(texCoordLoc, SE_VertexProperty::UV_SIZE, GL_FLOAT, GL_FALSE, vp.stride, (const GLvoid*)(vp.uv1Offset * sizeof(float)));
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
                checkGLError();
                mModelManager->setTexture(material->materialData.texturename.c_str(), t);
                
            }
            glBindTexture(GL_TEXTURE_2D, t->texture);
            checkGLError();
            if(isPicturePlace)
            {
                glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
                checkGLError();
                glActiveTexture(GL_TEXTURE1);
                checkGLError();
                SE_Texture* imageTex = mModelManager->getTexture(obj->pictureName.c_str());
                if(!imageTex)
                {
                    imageTex = new SE_Texture;
                    SS_LoadTextureForImage(obj->pictureName.c_str(), imageTex);
                    checkGLError();
                    mModelManager->setTexture(obj->pictureName.c_str(), imageTex);
                }
                glBindTexture(GL_TEXTURE_2D, imageTex->texture);
                checkGLError();
                
                GLint texLoc1 = shader->getUniformLocation("u_texture1");
                checkGLError();
                glUniform1i(texLoc1, 0);
                
                GLint texLoc2 = shader->getUniformLocation("u_texture2");
                checkGLError();
                glUniform1i(texLoc2, 1);
                checkGLError();
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
    glDrawArrays(GL_TRIANGLES, 0, mesh->mDrawingVertexNum);
    checkGLError();    
}
void SE_Scene::createTrackList()
{
    const char* curveName = "P1L";
    std::vector<SE_TrackPoint> trackPoint = mModelManager->getTrackPoints(curveName);
    if(trackPoint.size() > 0)
    {
        SE_Curve::SamplePoint* tp = new SE_Curve::SamplePoint[trackPoint.size()];
        SE_Vector3f referenceBoxMin = mModelManager->getReferenceBoxMin();
        SE_Vector3f referenceBoxMax = mModelManager->getReferenceBoxMax();
        SE_Vector3f referenceBoxBound = referenceBoxMax - referenceBoxMin;
        float pshift = mFloorDownMax.y - mFloorDownMin.y;
        pshift = (referenceBoxBound.y - pshift) / 2;
        SE_Vector3f startRef;
        startRef.x = referenceBoxMin.x;
        startRef.y = referenceBoxMax.y + pshift;
        startRef.z = referenceBoxMax.z;
        for(size_t i = 0 ; i < trackPoint.size() ; i++)
        {
            tp[i].point = mModelManager->getTrackPoint(trackPoint[i], startRef);
            LOGI("## sample point %lu = %f , %f, %f ##\n", i, tp[i].point.x, tp[i].point.y, tp[i].point.z);
        }
        mCurve = new SE_Curve(tp, trackPoint.size(), 2000);
        mCurve->createCurve();
        float curveLen1 = mCurve->getSplineCurveLength(0) + mCurve->getSplineCurveLength(1);
        float curveLen2 = mCurve->getSplineCurveLength(2) + mCurve->getSplineCurveLength(3);
        const int stepsize = 10;
        std::vector<float> stepvector(stepsize);
        for(size_t i = 0 ; i < stepsize ; i++)
        {
            stepvector[i] = curveLen1 / stepsize;
        }
        mTrackPoints1 = mCurve->getCurvePoint(0, 2, stepvector);
        for(size_t i = 0 ; i < stepsize ; i++)
        {
            stepvector[i] = curveLen2 / stepsize;
        }
        mTrackPoints2 = mCurve->getCurvePoint(2, 4, stepvector);
        SE_ASSERT(mTrackPoints1.size() == (stepsize + 1));
        SE_ASSERT(mTrackPoints2.size() == (stepsize + 1));
        SE_Vector3f lookingPoint = mModelManager->getFrameLookingPoint(frameLookingPointName);
        lookingPoint = SE_Vector3f(-lookingPoint.x, lookingPoint.y, lookingPoint.z);
        mCameraZ= mTrackPoints1[0] - lookingPoint;
        mCameraZ = mCameraZ.normalize();
        mCameraY = SE_Vector3f(0, -1, 0);
        SE_VectorInterpolate vi(mCameraZ, mCameraY);
        float t = 0;
        const float tstep = 1.0f / stepsize;
        mCameraZArray.resize(stepsize + 1);
        for(int i = 0 ; i  <= stepsize ; i++)
        {
            SE_Vector3f z = vi.interpolate(t);
            t += tstep;
            mCameraZArray[i] = z;
            LOGI("camera %d = %f , %f, %f \n", i, z.x, z.y, z.z);
        }
        SE_VectorInterpolate vi2(mCameraY, mCameraZ);
        t = 0;
        mCameraZArray2.resize(stepsize + 1);
        for(int i = 0 ;i < stepsize ; i++)
        {
            SE_Vector3f z = vi2.interpolate(t);
            t += tstep;
            mCameraZArray2[i] = z;
            LOGI("camera2 %d = %f, %f, %f \n", i, z.x, z.y, z.z);
        }
        mCameraZArray2[stepsize] = mCameraZ;
        //debug
        for(size_t i = 0 ; i < mTrackPoints1.size() ; i++)
        {
            LOGI("## track point1 %lu = %f, %f, %f ##\n", i, mTrackPoints1[i].x, mTrackPoints1[i].y, mTrackPoints1[i].z);
        }
        for(size_t i = 0 ; i < mTrackPoints2.size() ; i++)
        {
            LOGI("## track point2 %lu = %f, %f, %f ##\n", i, mTrackPoints2[i].x, mTrackPoints2[i].y, mTrackPoints2[i].z);
        }
        //end
        delete[] tp;
    }
}
void SE_Scene::resetCameraMove()
{
    mTrackPointsIndex = 0;
}
void SE_Scene::startCameraMove()
{
    if(mCurve)
    {
        size_t totalSize = mTrackPoints1.size() + mTrackPoints2.size();
        if(mTrackPointsIndex < totalSize)
        {
            if(mTrackPointsIndex < mTrackPoints1.size())
            {
                SE_Vector3f z = mCameraZArray[mTrackPointsIndex];
                float radian = radianBetweenVector(mCameraZ, z);
                SE_Quat q(-SE_RadianToAngle(radian), SE_Vector3f(0, 0, 1));
                SE_Vector3f y = q.map(mCameraY);
                SE_Vector3f v = mTrackPoints1[mTrackPointsIndex];
                LOGI("## camera loc = %f, %f, %f ##\n", v.x, v.y, v.z);
                LOGI("## radian = %f ##\n", SE_RadianToAngle(radian));
                LOGI("## camera z = %f , %f, %f ##\n", z.x, z.y, z.z);
                LOGI("## camera y = %f, %f, %f ##\n", y.x, y.y, y.z);
                mCamera->create(v, z, y, 45, mScreenHeight/ mScreenWidth, 1, 500);
                //mTrackPointsIndex++;
            }
            else
            {
                int index = mTrackPointsIndex - mTrackPoints1.size();
                if(index == 0)
                {
                    mCameraY = SE_Vector3f(-1, 0, 0);
                    mCameraZ = SE_Vector3f(0, -1, 0);
                }
                SE_Vector3f z = mCameraZArray2[index];
                float radian = radianBetweenVector(mCameraZ, z);
                SE_Quat q(SE_RadianToAngle(radian), SE_Vector3f(0, 0, 1));
                SE_Vector3f y = q.map(mCameraY);
                SE_Vector3f v = mTrackPoints2[index];
                mCamera->create(v, z, y, 45, mScreenHeight/ mScreenWidth, 1, 500);
                //mTrackPointsIndex++;
            }
        }
    }
}
/////////////// obsolete /////
void SE_Scene::createCurveFromPoints(SE_Vector3f* points , int pointNum, int curvePointNum)
{
    if(pointNum < 3)
    {
        LOGI("curve key point must be greater than 3\n");
        return;
    }
    //int curvePointNum = 1000;//
    float s = 0.5;
    int curveNum = pointNum - 1;
    float tstep = 1.0 / (float)curvePointNum;
    mPointData.vertexNum = curvePointNum * curveNum + 1;
    mPointData.floatSize = mPointData.vertexNum * 3;
    mPointData.data = new float[mPointData.floatSize];
    SE_Vector3f v = points[2] - points[0];
    v.mul(s);
    SE_HermiteInterpolate hip;
    SE_Vector4f hipx = hip.interpolate(points[0].x, points[1].x, 0, v.x);
    SE_Vector4f hipy = hip.interpolate(points[0].y, points[1].y, 0, v.y);
    SE_Vector4f hipz = hip.interpolate(points[0].z, points[1].z, 0, v.z);
    float t = 0;
    int j = 0;
    for(int i = 0 ; i < curvePointNum ; i++)
    {
        mPointData.data[j++] = hipx.x * t * t * t + hipx.y * t * t + hipx.z * t + hipx.w;
        mPointData.data[j++] = hipy.x * t * t * t + hipy.y * t * t + hipy.z * t + hipy.w;
        mPointData.data[j++] = hipz.x * t * t * t + hipz.y * t * t + hipz.z * t + hipz.w;
        t += tstep;
    }
    v = points[pointNum - 1] - points[pointNum - 3];
    v.mul(0.5);
    hipx = hip.interpolate(points[pointNum - 2].x, points[pointNum - 1].x, v.x, 0);
    hipy = hip.interpolate(points[pointNum - 2].y, points[pointNum - 1].y, v.y, 0);
    hipz = hip.interpolate(points[pointNum - 2].z, points[pointNum - 1].z, v.z, 0);
    t = 0;
    for(int i = 0 ; i < curvePointNum ; i++)
    {
        mPointData.data[j++] = hipx.x * t * t * t + hipx.y * t * t + hipx.z * t + hipx.w;
        mPointData.data[j++] = hipy.x * t * t * t + hipy.y * t * t + hipy.z * t + hipy.w;
        mPointData.data[j++] = hipz.x * t * t * t + hipz.y * t * t + hipz.z * t + hipz.w;
        t += tstep;
    }
    
    SE_CardinalSplineInterpolate csInterpolate;
    for(int k = 1 ; k < (pointNum - 2) ; k++)
    {
        SE_CardinalSplineInterpolate::Interpolator ipx = csInterpolate.getInterpolator(points[k - 1].x,     points[k].x, points[k + 1].x, points[k + 2].x, s);
        SE_CardinalSplineInterpolate::Interpolator ipy = csInterpolate.getInterpolator(points[k - 1].y,     points[k].y, points[k + 1].y, points[k + 2].y, s);
        SE_CardinalSplineInterpolate::Interpolator ipz =  csInterpolate.getInterpolator(points[k - 1].z, points[k].z, points[k + 1].z, points[k + 2].z, s);
        t =  0;
        for(int i = 0 ; i < curvePointNum ; i++)
        {
            mPointData.data[j++] = ipx.calc(t);
            mPointData.data[j++] = ipy.calc(t);
            mPointData.data[j++] = ipz.calc(t);
            t += tstep;
        }
    }
    mPointData.data[j++] = points[pointNum - 1].x;
    mPointData.data[j++] = points[pointNum - 1].y;
    mPointData.data[j++] = points[pointNum - 1].z;
    SE_ASSERT(j == mPointData.floatSize);
}
void SE_Scene::testCurve(SE_Curve* curve)
{
    int splineCurveNum = curve->getSplineCurveNum();
    for(int i = 0; i < splineCurveNum ; i++)
    {
        float curveLen = curve->getSplineCurveLength(i);
        LOGI("## curveLen = %f ##\n", curveLen);
    }
    curve->startForwarding(10);
    bool b;
    SE_Vector3f out;
    std::list<SE_Vector3f> dynPointList;
    while((b = curve->getNextCurvePoint(out)))
    {
        LOGI("## point = %f, %f, %f ##\n", out.x, out.y, out.z);
        dynPointList.push_back(out);
    }
    mDynPointData.vertexNum = dynPointList.size();
    mDynPointData.floatSize = mDynPointData.vertexNum * 3;
    mDynPointData.data = new float[mDynPointData.floatSize];
    int j = 0 ; 
    std::list<SE_Vector3f>::iterator it;
    for(it = dynPointList.begin() ; it != dynPointList.end() ; it++)
    {
        SE_Vector3f v = *it;
        mDynPointData.data[j++] = v.x;
        mDynPointData.data[j++] = v.y;
        mDynPointData.data[j++] = v.z;
    }
}
void SE_Scene::createPoints()
{

    /*
    SE_Vector3f points[4];
    points[0] = SE_Vector3f(0, 0, -1000);
    points[1] = SE_Vector3f(300, 0, -1000);
    points[2] = SE_Vector3f(300, 300, -1000);
    points[3] = SE_Vector3f(0, 300, -1000);
    createCurveFromPoints(points, 4, 10);
    */
    SE_Curve::SamplePoint points[5];
    points[0].point = SE_Vector3f(259.84, -169.62, -1000);
    points[1].point = SE_Vector3f(30.28, -143.04, -1000);
    points[2].point = SE_Vector3f(24.7, -512.4, -1000);
    points[3].point = SE_Vector3f(-142.34, -491.9, -1000);
    points[4].point = SE_Vector3f(-79.66, -637.47, -1000);
    
    //createCurveFromPoints(points, 5, 10);
    SE_Curve* curve = new SE_Curve(points, 5, 2000);
    curve->createCurve();
    testCurve(curve);
    mPointData.vertexNum = curve->getVertexNum();
    mPointData.floatSize = curve->getFloatSize();
    mPointData.data = curve->getData();
    float tantheta = 1024.0f / (2 * 1000.0f);
    float theta = 2 * atanf(tantheta);
    float angle = theta * 180.0 / 3.1415926;
    
    mCamera->create(SE_Vector3f(0, 0, 1000), SE_Vector3f(1, 0, 0), SE_Vector3f(0,1,0), SE_Vector3f(0, 0, 1), angle, mScreenHeight / mScreenWidth, 1 , 2000);
    mCamera->setViewport(0, 0 , mScreenWidth, mScreenHeight);
    
}
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
void SE_Scene::create()
{
    mCamera->create(SE_Vector3f(3, -10, 0),  SE_Vector3f(1, 0, 0), SE_Vector3f(0, 0, 1), SE_Vector3f(0, -1, 0), 45, mScreenHeight/ mScreenWidth, 1, 500);
    //mCamera->create(SE_Vector3f(0, -10, 2), SE_Vector3f(0, -1, 0), SE_Vector3f(0, 0, 1), SE_Vector3f(-1, 0, 0),  45, mScreenWidth / mScreenHeight, 1, 100);
    //mCamera->create(SE_Vector3f(0, 10, 0), SE_Vector3f(1, 0, 0), SE_Vector3f(0, 1, 0), SE_Vector3f(0, 0, 1), 45, mScreenWidth / mScreenHeight, 1, 100);
    mCamera->setViewport(0, 0, mScreenWidth, mScreenHeight);
    SE_GeometryData geomData = SE_GeometryData::createZAlignRect(10, 10, -35);
    mModelManager->addGeometryData(geomData);
    mFogPlaneMesh = new SE_Mesh;
    mFogPlaneMesh->geomDataIndex = mModelManager->getGeometryDataNum() - 1;
    mFogPlaneMesh->getDrawingVertex(SE_Mesh::XYZ_COLOR);
    createMeshVBO(mFogPlaneMesh);
    createNode();
    createScene();
    int meshnum = mModelManager->getMeshNum();
    for(int i = 0 ; i < meshnum ; i++)
    {
        SE_Mesh* mesh = mModelManager->getMesh(i);
        if(mesh->name == "logo001")
        {
            LOGI("## logo ###\n");
        }
        float* data = mesh->getDrawingVertex(SE_Mesh::XYZ_UV);
        createMeshVBO(mesh);
    }
}
void SE_Scene::createScene()
{
    for(int i = 0 ; i < mSceneCount ; i++)
    {
        SE_Matrix4f m;
        m.identity();
        m.setColumn(3, SE_Vector4f(0, mBoundY * i, 0, 1));
        Scene* scene = new Scene;
        scene->translate = m;
        scene->nodeList = mNodeList;
        mSceneList[i] = scene;
    }
}
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

void SE_Scene::createBoundingBox()
{
    SE_Vector3f min(1000, 1000, 1000);
    SE_Vector3f max(-1000, -1000, -1000);

    for(int i = 0 ; i < mModelManager->getMeshNum() ; i++)
    {
        SE_Mesh* mesh = mModelManager->getMesh(i);
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
    mMin = min;
    mMax = max;
}
void SE_Scene::calculateBoundingBox(int index)
{
    SE_Mesh* mesh = mModelManager->getMesh(index);
    if(mesh->name == "WallFrameV_001")
    {
        SE_GeometryData* gd = mModelManager->getGeometryData(mesh->geomDataIndex);
        int vertexNum = gd->vertexNum;
        SE_Vector3f* vertexArray = gd->vertexArray;
        SE_Vector3f min(1000, 1000, 1000);
        SE_Vector3f max(-1000, -1000, -1000);
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
        mBoundY = max.y - min.y;
    }
}
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
    float angle = ratio * deltay;
    //mCamera->rotateLocal(angle, SE_AXIS_X);
    mCamera->rotateLocal(angle, SE_AXIS_Y);
    mCamera->translateLocal(SE_Vector3f(0, 0, -deltax));

}
void SE_Scene::rotateCamera(float angle)
{
}
void SE_Scene::createNode()
{
    int meshNum = mModelManager->getMeshNum();
    SE_Matrix4f m;
    m.setColumn(0, SE_Vector4f(-1, 0, 0, 0));
    m.setColumn(1, SE_Vector4f(0, 1, 0, 0));
    m.setColumn(2, SE_Vector4f(0, 0, 1, 0));
    m.setColumn(3, SE_Vector4f(0, 0, 0, 1));
    for(int i = 0 ; i <meshNum ; i++)
    {
        SE_Mesh* mesh = mModelManager->getMesh(i);
        SS_Node* node = NULL;
        
        if(mesh->name == "WallFrameH_001" ||
           mesh->name == "FrameH_001" ||
           mesh->name == "WallFrameV_001" ||
           mesh->name == "FrameV_001" ||
           mesh->name == "BackWall" ||
           mesh->name == "photoV001" ||
           mesh->name == "photoH001"
           )
        {
            node = new SS_Node;
            node->meshIndex = i;
            node->m = m;
            node->cullType = SE_Scene::SS_Node::CW;
        }
        if(mesh->name != "Box003" && mesh->name != "logo001")
        {
            SS_Node* baseNode = new SS_Node;
            baseNode->meshIndex = i;
            if(node != NULL)
                mNodeList.push_back(node);
            mNodeList.push_back(baseNode);
            
        }
        calculateBoundingBox(i);
    }
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
void SE_Scene::render()
{
    glClearColor(1, 1, 1, 1);
    checkGLError();
    glClear(GL_COLOR_BUFFER_BIT);
    checkGLError();
    glClearColor(1.0f, 1.0f, 1.0f, 1.0f);
    glClear(GL_DEPTH_BUFFER_BIT);
    
    glEnable(GL_DEPTH_TEST);
    checkGLError();
    glDepthFunc(GL_LEQUAL);
    glDisable(GL_BLEND);
    checkGLError();
    glEnable(GL_CULL_FACE);
    checkGLError();
    //glDisable(GL_CULL_FACE);
    glCullFace(GL_BACK);
    checkGLError();
    glFrontFace(GL_CCW);
    checkGLError();
    
    SS_Shader* shader = mModelManager->getShader("fog_shader");
    shader->use();
    for(int i = 0 ; i < mSceneList.size() ; i++)
    {
        Scene* s = mSceneList[i];
        NodeList::iterator it;
        for(it = mNodeList.begin() ; it != mNodeList.end() ; it++)
        {
            SS_Node* node = *it;
            if(node->cullType == SE_Scene::SS_Node::CW)
            {
                glFrontFace(GL_CW);
            }
            else 
            {
                glFrontFace(GL_CCW);
            }
            SE_Matrix4f rotateM;
            rotateM.identity();
            
            SE_Matrix3f rotate3f;
            rotate3f.setRotateY(90);
            rotateM.set(rotate3f, SE_Vector3f(1, 1,1), SE_Vector3f(0, 0, 0));
            rotateM = rotateM.mul(s->translate.mul(node->m));
            render(shader, node->meshIndex, rotateM);
        }
    }
    renderFogPlane();
}