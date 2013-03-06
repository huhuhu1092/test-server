//
//  ObjLoader.cpp
//  AseToCbf
//
//  Created by 陈勇 on 11-12-4.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#include "ObjLoader.h"
#include <stdio.h>
#include "SE_Log.h"
#include "SE_String.h"
#include "SE_Common.h"
#include "SE_Utils.h"
static const int MAGIC = 0xCFCFCFCF;
static const int VERSION = 0x01;
static const int COORDINATE = 0x00;
static const int ENDIAN = 0x00; /** 0: little endian, 1: big endian*/


static const short MATERIAL_ID = 0x0002;
static const short GEOMOBJECT_ID = 0x0003;
static const short CAMERA_ID = 0x0004;
static const short SUB_MATERIAL_ID = 0x0005;
static const short MESH_ID = 0x0006;
static const short SHADER_ID = 0x0007;
static void writeString(const char* str, int len, FILE* fout)
{
    fwrite(&len, 1, sizeof(int), fout);
    fwrite(str, 1, len, fout);
}
static void writeString(const char* str, FILE* fout, bool hasNullEnd = false)
{
    size_t len = strlen(str);
    if(len > 0)
    {
        char buf[256];
        memset(buf, 0, 256);
        strncpy(buf, str, 255);
        LOGI("### string = %s ####\n", buf);
        len = strlen(buf);
        if(hasNullEnd)
        {
            len = len + 1;
            fwrite(&len, sizeof(int), 1, fout);
            fwrite(buf, 1, len , fout);
        }
        else
        {
            fwrite(&len, sizeof(int), 1, fout);
            fwrite(buf, 1, len, fout);
        }
    }
    else
    {
        fwrite(&len, sizeof(int), 1, fout);
    }
}
static size_t getFileLen(FILE* fp)
{
	size_t		pos;
	size_t		end;
    
	pos = ftell (fp);
	fseek (fp, 0, SEEK_END);
	end = ftell (fp);
	fseek (fp, pos, SEEK_SET);
    
	return end;
    
}
ObjLoader::ObjLoader(const char* dirName)
{
    mIndex = 0;
    mLen = 0;
    mData = NULL;
    mCurrentGeometryData = NULL;
    mInited = false;
    mDirName = dirName;
}
ObjLoader::~ObjLoader()
{}
void ObjLoader::load(const char *name)
{
    FILE* fp = fopen(name, "rb");
    if(!fp)
    {
        LOGI("can not open file : %s\n", name);
        return;
    }
    size_t fileLen = getFileLen(fp);
    mData = new char[fileLen];
    mLen = fileLen;
    mIndex = 0;
    if(!mData)
    {
        LOGI("can not malloc memory\n");
        mInited = false;
        return;
    }
    size_t ret = fread(mData, sizeof(char), mLen, fp);
    if(ferror(fp))
    {
        LOGI("read file error\n");
        return;
    }
    fclose(fp);
    mInited = true;
    process();
    delete[] mData;
    mLen = 0;
    mIndex = 0;
}
static bool isWhiteSpace(char c)
{
    return c == ' ' || c == '\r' || c == '\t';
}
std::vector<std::string> ObjLoader::getTokens(char* buffer, size_t size)
{
    size_t index = 0;
    size_t start = 0;
    std::list<std::string> ret;
    while(index < size)
    {
        while((index < size) && isWhiteSpace(buffer[index]))
        {
            index++;
        }
        if(index >= size)
            break;
        start = index;
        while((index < size) && buffer[index] != ' ' && buffer[index] != '\r')
            index++;
        std::string str(buffer + start, index - start);
        ret.push_back(str);
    }
    std::vector<std::string> strV;
    strV.resize(ret.size());
    if(ret.size() > 0)
        std::copy(ret.begin(), ret.end(), strV.begin());
    return strV;
}
bool ObjLoader::getLine(char *buffer, int size)
{
    memset(buffer, 0 , size);
    size_t currPos = mIndex;
    while(mIndex < mLen && mData[mIndex] != '\n')
        mIndex++;
    if(mIndex >= mLen)
        return false;
    size_t len = mIndex - currPos;
    if(len > 0)
    {
        if(len >= size)
        {
            LOGI("## read line exceed the max size of buffer ##\n");
            return false;
        }
        for(size_t i = 0 ; i < len ; i++)
            buffer[i] = mData[currPos + i];
    }
    mIndex++;
    return true;
}
void ObjLoader::readMaterial(const std::string& materialFileName)
{
    
}
static std::vector<int> getFaceVertexIndex(const std::string& token)
{
    size_t start = 0;
    size_t i = 0;
    size_t len = token.size();
    std::list<int> indexList;
    const char* data = token.c_str();
    while(i <= len)
    {
        if(i == len || token[i] == '/')
        {
            char buf[64];
            memset(buf, 0, 10);
            if(data[start] == '/')
            {
                start++;
            }
            if(i - start > 0)
            {
                strncpy(buf, data + start, i - start);
                indexList.push_back(atoi(buf));
            }
            else
            {
                SE_ASSERT(0);
            }
            start = i;
        }
        i++;
    }
    std::vector<int> ret(indexList.size());
    std::copy(indexList.begin(), indexList.end(), ret.begin());
    return ret;
}
void ObjLoader::fillGeometryData()
{
    if(mCurrentVertexList.size() == 0)
        return;
    mCurrentGeometryData->vertexNum = (int)mCurrentVertexList.size();
    mCurrentGeometryData->vertexArray = new SE_Vector3f[mCurrentGeometryData->vertexNum];
    mCurrentGeometryData->ownVertexArray = 1;
    std::list<SE_Vector3f>::iterator it;
    size_t j = 0;
    for(it = mCurrentVertexList.begin() ; it != mCurrentVertexList.end() ; it++)
    {
        mCurrentGeometryData->vertexArray[j] = *it;
        j++;
    }
    mCurrentGeometryData->texVertexNum = (int)mCurrentTexVertexList.size();
    mCurrentGeometryData->texVertexArray = new SE_Vector3f[mCurrentGeometryData->texVertexNum];
    mCurrentGeometryData->ownTexVertexArray = 1;
    j = 0;
    for(it = mCurrentTexVertexList.begin() ; it != mCurrentTexVertexList.end() ; it++)
    {
        mCurrentGeometryData->texVertexArray[j].x = (*it).x;
        mCurrentGeometryData->texVertexArray[j].y = (*it).y;
        j++;
    }
    mCurrentGeometryData->normalNum = (int)mCurrentNormalList.size();
    mCurrentGeometryData->normalArray = new SE_Vector3f[mCurrentGeometryData->normalNum];
    mCurrentGeometryData->ownNormalArray = 1;
    j = 0;
    for(it = mCurrentNormalList.begin() ; it!= mCurrentNormalList.end() ; it++)
    {
        mCurrentGeometryData->normalArray[j] = *it;
        j++;
    }
    mCurrentGeometryData->faceNum = (int)mCurrentFaceList.size();
    mCurrentGeometryData->faceArray = new SE_Face[mCurrentGeometryData->faceNum];
    mCurrentGeometryData->texFaceNum = (int)mCurrentFaceList.size();
    mCurrentGeometryData->texFaceArray = new SE_Face[mCurrentGeometryData->texFaceNum];
    mCurrentGeometryData->ownFaceArray = 1;
    mCurrentGeometryData->ownTexFaceArray = 1;
    j = 0;
    std::list<Face>::iterator itFace;
    for(itFace = mCurrentFaceList.begin() ; itFace != mCurrentFaceList.end() ; itFace++)
    {
        Face f = *itFace;
        mCurrentGeometryData->faceArray[j].v0 = f.v0;
        mCurrentGeometryData->faceArray[j].v1 = f.v1;
        mCurrentGeometryData->faceArray[j].v2 = f.v2;
        mCurrentGeometryData->texFaceArray[j].v0 = f.vt0;
        mCurrentGeometryData->texFaceArray[j].v1 = f.vt1;
        mCurrentGeometryData->texFaceArray[j].v2 = f.vt2;
        if(f.v0 > 10000 || f.v1 > 10000 || f.v2 > 10000 || f.vt0 > 10000 || f.vt1 > 10000)
            SE_ASSERT(0);
        j++;
    }
    
}
void ObjLoader::clearCurrentData()
{
    mCurrentVertexList.clear();
    mCurrentTexVertexList.clear();
    mCurrentFaceList.clear();
    mCurrentNormalList.clear();
    
}
void ObjLoader::handleTokenes(std::vector<std::string> tokenes)
{
    if(tokenes.size() == 3 && tokenes[0] == "#" && tokenes[1] == "object")
    {
        fillGeometryData();
        clearCurrentData();
        mCurrentMeshName = tokenes[2];
        mCurrentGeometryData = new SE_GeometryData;
        mCurrentGeometryData->type = SE_TRIANGLES;
        mCurrentGeometryData->numMapChannel = 0;
        mCurrentGeometryData->mapChannelArray = NULL;
        mCurrentGeometryData->vertexNum = 0;
        mCurrentGeometryData->vertexArray = NULL;
        mCurrentGeometryData->ownVertexArray = 0;
        mCurrentGeometryData->faceNum = 0;
        mCurrentGeometryData->faceArray = NULL;
        mCurrentGeometryData->ownFaceArray = 0;
        mCurrentGeometryData->texVertexNum = 0;
        mCurrentGeometryData->texVertexArray = NULL;
        mCurrentGeometryData->ownTexVertexArray = 0;
        mCurrentGeometryData->texFaceNum = 0;
        mCurrentGeometryData->texFaceArray = NULL;
        mCurrentGeometryData->ownTexFaceArray = NULL;
        mCurrentGeometryData->colorNum = 0;
        mCurrentGeometryData->colorArray = NULL;
        mCurrentGeometryData->ownColorArray = 0;
        mCurrentGeometryData->normalNum = 0;
        mCurrentGeometryData->normalArray = NULL;
        mCurrentGeometryData->ownNormalArray = 0;
        Geometry geom;
        geom.name = mCurrentMeshName;
        geom.geo = mCurrentGeometryData;
        mGeometryDataList.push_back(geom);
        
    }
    else if(tokenes[0] == "v")
    {
        SE_Vector3f v;
        v.x = atof(tokenes[1].c_str());
        v.y = atof(tokenes[2].c_str());
        v.z = atof(tokenes[3].c_str());
        float tmp = v.y;
        v.y = -v.z;
        v.z = tmp;
        mCurrentVertexList.push_back(v);
    }
    else if(tokenes[0] == "vn")
    {
        SE_Vector3f v;
        v.x = atof(tokenes[1].c_str());
        v.y = atof(tokenes[2].c_str());
        v.z = atof(tokenes[3].c_str());
        
        mCurrentNormalList.push_back(v);        
    }
    else if(tokenes[0] == "vt")
    {
        SE_Vector3f v;
        v.x = atof(tokenes[1].c_str());
        v.y = atof(tokenes[2].c_str());
        v.z = atof(tokenes[3].c_str());
        
        mCurrentTexVertexList.push_back(v);
    }
    else if(tokenes[0] == "f")
    {
        std::vector<int> v0 = getFaceVertexIndex(tokenes[1]);
        std::vector<int> v1 = getFaceVertexIndex(tokenes[2]);
        std::vector<int> v2 = getFaceVertexIndex(tokenes[3]);
        Face f;
        f.v0 = v0[0] - 1;
        f.v1 = v1[0] - 1; 
        f.v2 = v2[0] - 1;
        
        f.vt0 = v0[1] - 1;
        f.vt1 = v1[1] - 1;
        f.vt2 = v2[1] - 1;
        
        f.vn0 = v0[2] - 1;
        f.vn1 = v1[2] - 1;
        f.vn2 = v2[2] - 1 ;
        mCurrentFaceList.push_back(f);
    }
    else if(tokenes[0] == "g")
    {
        std::string geoName = tokenes[1];
        mCurrentMesh = new Mesh;
        mCurrentMesh->geometryName = geoName;
        mMeshList.push_back(mCurrentMesh);
    }
    else if(tokenes[0] == "usemtl")
    {
        std::string mtlName = tokenes[1];
        mCurrentMesh->materialName = mtlName;
    }
    else if(tokenes[0] == "newmtl")
    {
        std::string mtlName = tokenes[1];
        SE_Material* m = new SE_Material;
        m->subMaterialNum = 0;
        m->subMaterialArray = NULL;
        Material mm;
        mm.name = mtlName;
        mm.m = m;
        mMaterialList.push_back(mm);
        mCurrentMaterial = m;
    }
    else if(tokenes[0] == "Ka")
    {
        mCurrentMaterial->materialData.ambient.x = atof(tokenes[1].c_str());
        mCurrentMaterial->materialData.ambient.y = atof(tokenes[2].c_str());
        mCurrentMaterial->materialData.ambient.z = atof(tokenes[3].c_str());
    }
    else if(tokenes[0] == "Kd")
    {
        mCurrentMaterial->materialData.diffuse.x = atof(tokenes[1].c_str());
        mCurrentMaterial->materialData.diffuse.y = atof(tokenes[2].c_str());
        mCurrentMaterial->materialData.diffuse.z = atof(tokenes[3].c_str());
    }
    else if(tokenes[0] == "map_Kd")
    {
        const char* startData = tokenes[1].c_str();
        size_t len = strlen(tokenes[1].c_str());
        if(len == 0)
        {
            SE_String_Init(&mCurrentMaterial->materialData.texturename, "");
            return;
        }
        size_t i = len - 1;
        size_t pos = i;
        while(i > 0)
        {
            if(startData[i] == '\\')
            {
                pos = i + 1;
                break;
            }
            i--;
        }
        if(i == 0)
        {
            if(startData[i] == '\\')
                pos = 1;
            else
                pos = 0;
        }
        if(pos < len)
        {
            std::string str = tokenes[1].substr(pos);
            SE_String_Init(&mCurrentMaterial->materialData.texturename, str.c_str());
        }
        else
        {
            SE_String_Init(&mCurrentMaterial->materialData.texturename, "");
        }
        
    }
}
template <typename IteratorT>
int getItemIndex(IteratorT begin, IteratorT end, const std::string& name)
{
    int index = 0;
    IteratorT start = begin;
    for(; start != end; start++)
    {
        if(start->name == name)
            return index;
        else
            index++;
    }
    return -1;
}
void ObjLoader::write(const char* name)
{
    FILE* fout = fopen(name, "wb");
    if(!fout || !mInited)
    {
        LOGI("write file : %s error \n ", name);
        return;
    }
    fwrite(&MAGIC, sizeof(int), 1, fout);
    fwrite(&VERSION, sizeof(int), 1, fout);
    fwrite(&COORDINATE, sizeof(int), 1, fout);
    fwrite(&ENDIAN, sizeof(int), 1, fout);
    size_t materialNum = mMaterialList.size();
    size_t geomDataNum = mGeometryDataList.size();
    size_t meshNum = mMeshList.size();
    fwrite(&materialNum, sizeof(int), 1, fout);
    fwrite(&geomDataNum, sizeof(int), 1, fout);
    fwrite(&meshNum, sizeof(int), 1, fout);
    std::list<Material>::iterator itMaterial;
    for(itMaterial = mMaterialList.begin() ; itMaterial != mMaterialList.end() ; itMaterial++)
    {
        LOGI("...write material\n");
        fwrite(&MATERIAL_ID, sizeof(short), 1, fout);
        SE_Material* m = itMaterial->m;
        writeString(SE_String_GetData(&m->materialData.texturename), fout);
        fwrite(&m->materialData.ambient.x, sizeof(float), 1, fout);
        fwrite(&m->materialData.ambient.y, sizeof(float), 1, fout);
        fwrite(&m->materialData.ambient.z, sizeof(float), 1, fout);
        
        fwrite(&m->materialData.diffuse.x, sizeof(float), 1, fout);
        fwrite(&m->materialData.diffuse.y, sizeof(float), 1, fout);
        fwrite(&m->materialData.diffuse.z, sizeof(float), 1, fout);
        
        fwrite(&m->materialData.specular.x, sizeof(float), 1, fout);
        fwrite(&m->materialData.specular.y, sizeof(float), 1, fout);
        fwrite(&m->materialData.specular.z, sizeof(float), 1, fout);
        fwrite(&m->subMaterialNum , sizeof(int), 1, fout);        
    }
    std::list<Geometry>::iterator itGeometry;
    for(itGeometry = mGeometryDataList.begin() ; itGeometry != mGeometryDataList.end() ; itGeometry++)
    {
        LOGI("... write goem data\n");
        fwrite(&GEOMOBJECT_ID, sizeof(short), 1, fout);
        SE_GeometryData* gd = itGeometry->geo;
        fwrite(&gd->type, sizeof(int), 1, fout);
        fwrite(&gd->vertexNum, sizeof(int), 1, fout);
        fwrite(&gd->faceNum, sizeof(int), 1, fout);
        fwrite(&gd->texVertexNum, sizeof(int), 1, fout);
        fwrite(&gd->colorNum, sizeof(int), 1, fout);
        int j;
        for(j = 0 ; j < gd->vertexNum ; j++)
        {
            fwrite(&gd->vertexArray[j].x, sizeof(float), 1, fout);
            fwrite(&gd->vertexArray[j].y, sizeof(float), 1, fout);
            fwrite(&gd->vertexArray[j].z, sizeof(float), 1, fout);
        }
        for(j = 0 ; j < gd->faceNum ; j++)
        {
            fwrite(&gd->faceArray[j].v[0], sizeof(int), 1, fout);
            fwrite(&gd->faceArray[j].v[1], sizeof(int), 1, fout);
            fwrite(&gd->faceArray[j].v[2], sizeof(int), 1, fout);
            
            SE_Vector3f p0, p1, p2, v1, v2;
            p0.x = gd->vertexArray[gd->faceArray[j].v[0]].x;
            p0.y = gd->vertexArray[gd->faceArray[j].v[0]].y;
            p0.z = gd->vertexArray[gd->faceArray[j].v[0]].z;
            
            p1.x = gd->vertexArray[gd->faceArray[j].v[1]].x;
            p1.y = gd->vertexArray[gd->faceArray[j].v[1]].y;
            p1.z = gd->vertexArray[gd->faceArray[j].v[1]].z;
            
            p2.x = gd->vertexArray[gd->faceArray[j].v[2]].x;
            p2.y = gd->vertexArray[gd->faceArray[j].v[2]].y;
            p2.z = gd->vertexArray[gd->faceArray[j].v[2]].z;
            /*
             SE_Vec3f_Subtract(&p1, &p0, &v1);
             SE_Vec3f_Subtract(&p2, &p0, &v2);
             float a = SE_Vec3f_CrossScalar(&v1, &v2);
             if(a < 0)
             {
             static int kk = 0;
             LOGI("### clock wise face: %d ##\n", kk++);
             }
             else if(a > 0)
             {
             LOGI("## counter clock wise face ##\n");
             }
             */
        }
        if(gd->texVertexNum > 0)
        {
            for(j = 0 ; j < gd->texVertexNum ; j++)
            {
                fwrite(&gd->texVertexArray[j].x, sizeof(float), 1, fout);
                fwrite(&gd->texVertexArray[j].y, sizeof(float), 1, fout);
            }
            for(j = 0 ; j < gd->faceNum ; j++)
            {
                fwrite(&gd->texFaceArray[j].v[0], sizeof(int), 1, fout);
                fwrite(&gd->texFaceArray[j].v[1], sizeof(int), 1, fout);
                fwrite(&gd->texFaceArray[j].v[2], sizeof(int), 1, fout);
            }
        }
        fwrite(&gd->numMapChannel, sizeof(int), 1, fout);
        if(gd->numMapChannel > 0)
        {
            for(int k = 0 ; k < gd->numMapChannel ; k++)
            {
                SE_MapChannel* mc = &gd->mapChannelArray[k];
                fwrite(&mc->numTVertex, sizeof(int), 1, fout);
                fwrite(&mc->numTFaces, sizeof(int), 1, fout);
                for(int n = 0 ; n < mc->numTVertex ; n++)
                {
                    fwrite(&mc->texVertexArray[n].x, sizeof(float), 1 ,fout);
                    fwrite(&mc->texVertexArray[n].y, sizeof(float), 1, fout);
                }
                for(int n = 0 ; n < mc->numTFaces ; n++)
                {
                    fwrite(&mc->texFaceArray[n].v0, sizeof(int), 1, fout);
                    fwrite(&mc->texFaceArray[n].v1, sizeof(int), 1, fout);
                    fwrite(&mc->texFaceArray[n].v2, sizeof(int), 1, fout);
                }
            }
        }
        if(gd->colorNum > 0)
        {
            for(j = 0 ; j < gd->colorNum ; j++)
            {
                fwrite(&gd->colorArray[j].x, sizeof(float), 1, fout);
                fwrite(&gd->colorArray[j].y, sizeof(float), 1, fout);
                fwrite(&gd->colorArray[j].z, sizeof(float), 1, fout);
            }
        }
    }
    std::list<Mesh*>::iterator itMesh;
    for(itMesh = mMeshList.begin() ; itMesh != mMeshList.end() ; itMesh++)
    {
        Mesh* mesh = *itMesh;
        SE_Mesh dmesh;
        SE_Mesh* smesh = &dmesh;
        smesh->geomDataIndex = getItemIndex(mGeometryDataList.begin(), mGeometryDataList.end(), mesh->geometryName);
        smesh->materialIndex = getItemIndex(mMaterialList.begin(), mMaterialList.end(), mesh->materialName);
        smesh->subMeshNum = 0;
        SE_String_Init(&smesh->name, mesh->geometryName.c_str());
        LOGI("... write mesh id\n");
        fwrite(&MESH_ID, sizeof(short), 1, fout);
        fwrite(&smesh->geomDataIndex, sizeof(int), 1, fout);
        fwrite(&smesh->materialIndex, sizeof(int), 1, fout);
        fwrite(&smesh->wireframeColor.x, sizeof(float), 1, fout);
        fwrite(&smesh->wireframeColor.y, sizeof(float), 1, fout);
        fwrite(&smesh->wireframeColor.z, sizeof(float), 1, fout);
        
        fwrite(&smesh->rotateAxis.x, sizeof(float), 1, fout);
        fwrite(&smesh->rotateAxis.y, sizeof(float), 1, fout);
        fwrite(&smesh->rotateAxis.z, sizeof(float), 1, fout);
        
        fwrite(&smesh->rotateAngle, sizeof(float), 1, fout);
        
        fwrite(&smesh->scaleAxis.x, sizeof(float), 1, fout);
        fwrite(&smesh->scaleAxis.y, sizeof(float), 1, fout);
        fwrite(&smesh->scaleAxis.z, sizeof(float), 1, fout);
        
        fwrite(&smesh->scale.x, sizeof(float), 1, fout);
        fwrite(&smesh->scale.y, sizeof(float), 1, fout);
        fwrite(&smesh->scale.z, sizeof(float), 1, fout);
        
        fwrite(&smesh->translate.x, sizeof(float), 1, fout);
        fwrite(&smesh->translate.y, sizeof(float), 1, fout);
        fwrite(&smesh->translate.z, sizeof(float), 1, fout);
        writeString(SE_String_GetData(&smesh->name), fout);
        fwrite(&smesh->subMeshNum, sizeof(int), 1, fout);
    }
    fwrite(&SHADER_ID, sizeof(short), 1, fout);
    size_t shaderCount = mShaderList.size();
    fwrite(&shaderCount, sizeof(int), 1, fout);
    std::list<Shader>::iterator itShader;
    for(itShader = mShaderList.begin() ; itShader != mShaderList.end() ; itShader++)
    {
        Shader s = *itShader;
        writeString(s.name.c_str(), fout);
        writeString(s.vertexData, s.vertexDataLen, fout);
        writeString(s.fragmentData, s.fragmentDataLen, fout);
    }
    fclose(fout);
}

void ObjLoader::loadShader(const char* shaderName, const char* vertexShaderFile, const char* fragmentShaderFile)
{
    Shader s;
    char* outData;
    int len;
    SE_ReadCScriptFile(vertexShaderFile, &outData, &len);
    s.vertexData = outData;
    s.vertexDataLen = len;
    len = 0;
    outData = NULL;
    SE_ReadCScriptFile(fragmentShaderFile, &outData, &len);
    s.fragmentData = outData;
    s.fragmentDataLen = len;
    s.name = shaderName;
    mShaderList.push_back(s);
}
void ObjLoader::process()
{
    size_t line = 0;
    while(mIndex < mLen)
    {
        char buf[LINE_SIZE];
        bool ret = getLine(buf, LINE_SIZE);
        if(!ret)
        {
            break;
        }
        std::vector<std::string> tokenes = getTokens(buf, strlen(buf));
        if(tokenes.size() > 0)
            handleTokenes(tokenes);
        line++;
    }
    if(mIndex < mLen)
    {
        mInited = false;
        LOGI("parse line %d error\n", line);
    }
}