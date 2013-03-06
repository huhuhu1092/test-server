//
//  ObjLoader.h
//  AseToCbf
//
//  Created by 陈勇 on 11-12-4.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#ifndef ObjLoader_h
#define ObjLoader_h
#include "SE_Vector.h"
#include "SE_GeometryData.h"
#include "SE_ResourceManager.h"
#include <list>
#include <map>
#include <string>
#include <vector>
class ObjLoader
{
public:
    enum {LINE_SIZE = 1024};
    enum SHADER_TYPE {VERTEX_SHADER, FRAGMENT_SHADER};
    struct Face
    {
        int v0, v1, v2;
        int vt0, vt1, vt2;
        int vn0, vn1, vn2;
    };
    struct Mesh
    {
        std::string materialName;
        std::string geometryName;
    };
    ObjLoader(const char* dirName);
    void load(const char* name);
    void loadShader(const char* shaderName, const char* vertexShaderFile, const char* fragmentShaderFile);
    void write(const char* name);
    ~ObjLoader();
private:
    bool getLine(char* buffer, int size);
    void process();
    std::vector<std::string> getTokens(char* buffer, size_t size);
    void handleTokenes(std::vector<std::string>);
    void readMaterial(const std::string& materialFileName);
    void fillGeometryData();
    void clearCurrentData();
private:
    struct Material
    {
        std::string name;
        SE_Material* m;
    };
    struct Geometry
    {
        std::string name;
        SE_GeometryData* geo;
    };
    struct Shader
    {
        char* vertexData;
        size_t vertexDataLen;
        char* fragmentData;
        size_t fragmentDataLen;
        std::string name;
    };
    std::string mDirName;
    size_t mIndex;
    size_t mLen;
    char* mData;
    bool mInited;
    std::list<Geometry> mGeometryDataList;
    SE_GeometryData* mCurrentGeometryData;
    SE_Material* mCurrentMaterial;
    std::list<Material> mMaterialList;
    std::list<Mesh*> mMeshList;
    std::list<Shader> mShaderList;
    Mesh* mCurrentMesh;
    std::string mCurrentMeshName;
    
    std::list<SE_Vector3f> mCurrentVertexList;
    std::list<SE_Vector3f> mCurrentNormalList;
    std::list<SE_Vector3f> mCurrentTexVertexList;
    std::list<Face> mCurrentFaceList;
};


#endif
