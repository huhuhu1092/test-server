#ifndef SE_MESH_H
#define SE_MESH_H
#include "SE_Vector.h"
class SE_TextureCoordData;
class SE_ImageDataID;
class SE_TextureUnit
{
public:
    SE_TextureUnit();
    ~SE_TextureUnit();
    void setTextureCoordData(SE_TextureCoordData* texCoordData)
    {
        mTexCoord = texCoordData;
    }
    bool setImageData(SE_ImageDataID* imageIDArray, int num)
    {
        mImageArray = imageIDArray;
        mImageNum = num;
    }
    SE_TextureCoordData* getTextureCoordData()
    {
        return mTexCoord;
    }
    int getImageNum()
    {
        return mImageNum;
    }
    SE_ImageDataID getImage(int index)
    {
        if(index < 0 || index >= mImageNum)
            return NULL;
        return mImageArray[index];
    }
    SE_ImageDataID getImage()
    {
        return mImageArray;
    }
    bool hasMultiImage()
    {
        return mImageNum > 1;
    }
private:
    SE_TextureCoordData* mTexCoord;
    SE_ImageDataID* mImageArray;
    int mImageNum;
};
class SE_Texture
{
public:
    SE_Texture();
    ~SE_Texture();
    enum TEXUNIT_TYPE {TEXTURE0, TEXTURE1, TEXTURE2, TEXTURE3, TEXTURE4,
                       TEXTURE5, TEXTURE6, TEXTURE7, TEXUNIT_NUM};
    void setTextureUnit(int texType, SE_TextureUnit* texUnit);
    void removeTextureUnit(int texType);
    SE_TextureUnit* getTexUnit(int texType);
private:
    SE_TextureUnit** mTexUnitArray;
};
class SE_Surface
{
public:
    SE_Surface();
    ~SE_Surface();
    SE_GeometryData* getGeometryData();
    SE_Texture* getTexture();
    SE_MaterialData* getMaterialData();
    int getFacetNum();
    int* getFacetArray();
    SE_Vector3f getColor();
    SE_ProgramDataID getProgramDataID();
    void setGeometryData(SE_GeometryData* geomData);
    void setMaterialData(SE_MaterialData* materialData);
    void setTexture(SE_Texture* texture);
    void setFacets(int* facets, int num);
    void setColor(const SE_Vector3f& color);
    void setProgramDataID(const SE_ProgramDataID& programID);
private:
    SE_Texture* mTexture;
    SE_MaterialData* mMaterialData;
    int* mFacetArray;
    int mFacetNum;
    SE_GeometryData* mGeometryData;
    SE_Vector3f mColor;
    SE_ProgramDataID mProgramDataID;
};
class SE_Mesh
{
public:
    SE_Mesh(int surfaceNum, int texNum);
    ~SE_Mesh();
    SE_GeometryData* getGeometryData();
    SE_Vector3f getColor()
    {
        return mColor;
    }
    SE_Texture* getTexture(int index)
    {
        return mTextureArray[index];
    }
    SE_Surface* getSurface(int index)
    {
        return mSurfaceArray[index];
    }
    void setSurface(int index, SE_Surface* surface)
    {
        mSurfaceArray[index] = surface;
    }
    void setTexture(int index, SE_Texture* texture)
    {
        mTextureArray[index] = texture;
    }
    void setColor(const SE_Vector3f& c)
    {
        mColor = c;
    }
    void setGeometryData(SE_GeometryData* geomData)
    {
        mGeometryData = geomData;
    }
    int getSurfaceNum();
    SE_Surface* getSurface(int index);
private:
    SE_GeometryData* mGeometryData;
    SE_Surface** mSurfaceArray;
    int mSurfaceNum;
    SE_Texture** mTextureArray;
    int mTextureNum;
    SE_Vector3f mColor;
};
#endif
