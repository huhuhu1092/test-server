//
//  SS_Model.h
//  PhotoFrame
//
//  Created by 陈勇 on 11-11-26.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#ifndef SS_Model_h
#define SS_Model_h
#include "SE_Vector.h"
#include "SE_GeometryData.h"
#include <string>
#include <vector>
#include <map>
#include <list>
class SS_Shader;
typedef unsigned int GLuint;
typedef int GLint;
typedef unsigned int GLenum;
enum SE_PIXELFORMAT {SE_RGB, SE_RGBA, SE_RGB565};

struct SE_MaterialData
{
    SE_Vector3f ambient;
    SE_Vector3f diffuse;
    SE_Vector3f specular;
    std::string texturename;
};

struct SE_Material
{
    SE_MaterialData materialData;
    SE_MaterialData* subMaterialArray;
    int subMaterialNum;
} ;
struct SE_FaceList
{
    int* facets; // the index array of face in source data
    int num;
    SE_FaceList()
    {
        facets = NULL;
        num = 0;
    }
    SE_FaceList(int* faces, int num)
    {
        this->facets = faces;
        this->num = num;
    }
    ~SE_FaceList()
    {
        delete[] facets;
    }
};
struct SE_SubMesh
{
    SE_FaceList faceList;
    int subMaterialIndex;
    SE_SubMesh()
    {
        subMaterialIndex = 0;
    }
};
struct _VertexXYZ_UV
{
    _Vector3f xyz;
    _Vector2f uv;
    enum { xyz_size = 3};
    enum { uv_size = 2};
    enum { xyz_offset = 0};
    enum { uv_offset  = 3};
    enum { vertex_size = 5};
};
struct SE_VertexProperty
{
    enum {XYZ_SIZE = 3};
    enum {UV_SIZE = 2};
    enum {COLOR_SIZE = 3};
    enum {NORMAL_SIZE = 3};
    int stride;
    int xyzOffset;
    int uv1Offset;
    int uv2Offset;
    int colorOffset;
    int normalOffset;
    SE_VertexProperty()
    {
        stride = 0;
        xyzOffset = 0;
        uv1Offset = 0;
        uv2Offset = 0;
        colorOffset = 0;
        normalOffset = 0;
    }
};
struct SE_Mesh
{
    enum VERTEX_TYPE {INVALID, XYZ, XYZ_UV, XYZ_COLOR, XYZ_UV1_UV2, XYZ_UV_NORMAL};
    int geomDataIndex;// -1 indicate nothing
    int materialIndex;// -1 indicate notning
    SE_Vector3f wireframeColor;
    SE_SubMesh* subMeshArray;
    int subMeshNum;
    SE_Vector3f rotateAxis;
    float rotateAngle;
    SE_Vector3f scaleAxis;
    SE_Vector3f scale;
    SE_Vector3f translate;
    std::string name;
    
    float* mDrawingVertex;
    VERTEX_TYPE mCurrVertexType;
    int mDrawingVertexNum;
    int mFloatSize;
    
    float* mWireFrameVertex;
    int mWireFrameVertexNum;
    VERTEX_TYPE mWireFrameVertexType;
    GLuint vboID;
    SE_Mesh()
    {
        geomDataIndex = -1;
        materialIndex = -1;
        subMeshArray = NULL;
        subMeshNum = 0;
        mDrawingVertex = NULL;
        mCurrVertexType = INVALID;
        mDrawingVertexNum = 0;
        vboID = 0;
        
        mWireFrameVertex = NULL;
        mWireFrameVertexNum = 0;
        mWireFrameVertexType = XYZ;
        
        mFloatSize = 0;
    }
    ~SE_Mesh()
    {
        delete[] subMeshArray;
    }
    float* getDrawingVertex(VERTEX_TYPE type);
    float* getWireframeVertex(const SE_Vector3f& cameraZ);
    SE_VertexProperty getVertexProperty(VERTEX_TYPE vt);
};

struct SE_ImageData
{
    int width;
    int height;
    int pixelFormat;
    int bytesPerRow;
    char* data;
    SE_ImageData()
    {
        width= height = 0;
        pixelFormat = 0;
        bytesPerRow = 0;
        data = NULL;
    }
    ~SE_ImageData()
    {
        delete[] data;
    }
};

struct SE_Texture
{
    std::string texturename;
    GLint texture;
    GLint wrapS;
    GLint wrapT;
    GLint filterMin;
    GLint filterMag;
    unsigned int width;
    unsigned int height;
    unsigned int realWidth;
    unsigned int realHeight;
    SE_Texture()
    {
        texture = -1;
        wrapS = 0;
        wrapT = 0;
        filterMag = 0;
        filterMin = 0;
        width = height = realWidth  = realHeight = 0;
    }
    ~SE_Texture();
};
struct SE_TrackPoint
{
    int x, y, z;
    SE_TrackPoint()
    {
        x = y = z = 0;
    }
    SE_TrackPoint(int x, int y, int z)
    {
        this->x = x;
        this->y = y;
        this->z = z;
    }
};
struct SE_TrackPointList
{
    std::string name;
    std::vector<SE_TrackPoint> points;
};
struct SE_TrackPointData
{
    int xlen, ylen, zlen;
    std::vector<SE_TrackPointList> trackPointList;
};
struct SE_LookingPoint
{
    std::string name;
    SE_Vector3f point;
    SE_LookingPoint()
    {}
    SE_LookingPoint(const std::string& name, const SE_Vector3f& point)
    {
        this->name = name;
        this->point = point;
    }
};
class SS_ModelManager
{
public:
    SS_ModelManager();
    ~SS_ModelManager();
    void loadModel(const char* filename);
    SE_Mesh* getMesh(const char* meshName);
    int getMeshNum();
    SE_Mesh* getMesh(int index);
    SS_Shader* getShader(const char* shaderName);
    SE_GeometryData* getGeometryData(int index);
    size_t getGeometryDataNum();
    SE_Material* getMaterial(int index);
    SE_Texture* getTexture(const char* texturename);
    void setTexture(const char* texturename, SE_Texture* texture);
    void removeTexture(const char* texturename);
    void addGeometryData(const SE_GeometryData& geomData);
    std::vector<SE_TrackPoint> getTrackPoints(const char* name);
    SE_Vector3f getTrackPoint(int x, int y, int z, const SE_Vector3f& start);
    SE_Vector3f getTrackPoint(const SE_TrackPoint& tp, const SE_Vector3f& start);
    SE_Vector3f getFrameLookingPoint(const char* name);
    SE_Vector3f getReferenceBoxMin() const
    {
        return mReferenceBoxMin;
    }
    SE_Vector3f getReferenceBoxMax() const
    {
        return mReferenceBoxMax;
    }
private:
    void process(const char* data, int currPos, int dataLen);
private:
    typedef std::vector<SE_GeometryData> GeometryDataArray;
    typedef std::vector<SE_Material> MaterialDataArray;
    typedef std::map<std::string, SE_ImageData> ImageDataMap;
    typedef std::vector<SE_Mesh> MeshArray;
    typedef std::vector<SS_Shader*> ShaderArray;
    typedef std::map<std::string, SE_Texture*> TextureList;
    typedef std::vector<SE_LookingPoint> LookingPointList;
    GeometryDataArray mGeometryDataArray;
    MaterialDataArray mMaterialArray;
    ImageDataMap mImageDataMap;
    MeshArray mMeshArray;
    ShaderArray mShaderArray;
    TextureList mTextureList;
    SE_Vector3f mReferenceBoxMin;
    SE_Vector3f mReferenceBoxMax;
    SE_TrackPointData mTrackPointData;
    LookingPointList mLookingPointList;
};


#endif
