#ifndef SE_DATATRANSFER_H
#define SE_DATATRANSFER_H
#include "SE_ID.h"
class SE_BufferInput;
class SE_BufferOutput;
class SE_TextureUnitTransfer
{
public:
    SE_TextureUnitTransfer()
    {
        mImageDataNum = 0;
        mType = 0;
        mImageDataArray = NULL;
    }
    ~SE_TextureUnitTransfer()
    {
        if(mImageDataArray)
            delete[] mImageDataArray;
    }
    SE_TextureCoordDataID getTexCoordDataID()
    {
        return mTexCoordID;
    }
    int getImageDataNum()
    {
        return mImageDataNum;
    }
    SE_ImageDataID getImageDataID(int index)
    {
        if(index < 0 || index >= mImageDataNum)
            return NULL;
        return mImageDataArray[index];
    }
    // this is the texture unit type: TEXTURE0, TEXTURE1, ... TEXTURE8,etc.
    int getType()
    {
        return mType;
    } 

    void setTextureCoordDataID(const SE_TextureCoordDataID& texCoordDataID)
    {
        mTexCoordDataID = texCoordDataID;
    }
    void setImageDataID(SE_ImageDataID* imageDataIDArray, int num)
    {
        mImageDataArray = imageDataArray;
        mImageDataNum = num;
    }
    void setType(int type)
    {
        mType = type;
    }
private:
    SE_TextureCoordDataID mTexCoordID;
    int mImageDataNum;
    int mType;
    SE_ImageDataID* mImageDataArray;
};
///////////////////////////////////////////////
class SE_TextureTransfer
{
public:
    SE_TextureTransfer()
    {
        mTexUnitNum = 0;
        mTexUnitTransfer = NULL;
    }
    ~SE_TextureTransfer()
    {
        if(mTexUnitDataTransfer)
            delete[] mTexUnitDataTransfer;
    }

    int getTextureUnitNum()
    {
        return mTexUnitNum;
    }
    SE_TextureUnitTransfer* getTextureUnit(int index)
    {
        if(index < 0 || index >= mTexUnitNum)
            return NULL;
        return mTexUnitTransfer;
    }

    void setTextureUnitTransfer(SE_TextureUnitTransfer* texUnitTransfer, int num)
    {
        mTexUnitTransfer = texUnitTransfer;
        mTexUnitNum = num;
    }
private:
    int mTexUnitNum;
    SE_TextureUnitTransfer* mTexUnitTransfer;
};
/////////////////////////////////////////
class SE_SurfaceTransfer
{
public:
    SE_SurfaceTransfer()
    {
        mTexIndex = -1;
        mFacetNum = 0;
        mFacetArray = NULL;
    }
    ~SE_SurfaceTransfer()
    {
        if(mFacetArray)
            delete[] mFacetArray;
    }
    int getTextureIndex()
    {
        return mTexIndex;
    }
    SE_MaterialDataID getMaterialDataID()
    {
        return mMaterialDataID;
    }
    int getFacetNum()
    {
        return mFacetNum;
    }
    int* getFacetArray()
    {
        return mFacetArray;
    }

    void setTextureIndex(int index)
    {
        mTexIndex = index;
    }
    void setMaterialDataID(const SE_MaterialDataID& materialDataID)
    {
        mMaterialDataID = materialDataID;
    }
    void setFacets(int* facets, int num)
    {
        mFacetArray = facets;
        mFacetNum = num;
    }
private:
    int mTexIndex;
    SE_MaterialDataID mMaterialDataID;
    int mFacetNum;
    int* mFacetArray;
};
//////////////////////////////////////////////////
class SE_MeshTransfer
{
public:
    SE_MeshTransfer()
    {
        mSurfaceNum = 0;
        mSurfaceTransferArray = NULL;
        mTexNum = 0;
        mTexTransferArray = NULL;
    }
    ~SE_MeshTransfer();
    SE_GeometryDataID getGeomDataID()
    {
        return mGeomDataID;
    }
    int getSurfaceNum()
    {
        return mSurfaceNum;
    }
    SE_SurfaceDataTransfer* getSurface(int index)
    {
        if(index < 0 || index >= mSurfaceNum)
            return NULL;
        return &mSurfaceTransferArray[index];
    }
    int getTextureNum()
    {
        return mTexNum;
    }
    SE_TextureTransfer* getTexture(int index)
    {
        if(index < 0 || index >= mTexNum)
            return NULL;
        return &mTexTransferArray[index];
    }
    SE_Vector3f getColor()
    {
        return mColor;
    }

    void setGeometryDataID(const SE_GeometryDataID& geomDataID)
    {
        mGeomDataID = geomDataID;
    }
    void setSurfaceTransfer(SE_SurfaceDataTransfer* surfaceData, int num)
    {
        mSurfaceTransferArray = surfaceData;
        mSurfaceNum = num;
    }
    void setTextureTransfer(SE_TextureTransfer* texTransfer, int num)
    {
        mTexTransferArray = texTransfer;
        mTexNum = num;
    }
    void setColor(const SE_Vector3f& color)
    {
        mColor = color;
    }
    /////////////////////////////////////////////////////
    SE_Mesh* createMesh();
    void createFromMesh(SE_Mesh* mesh);
    void read(SE_BufferInput& inputBuffer);
    void write(SE_BufferOutput& outputBuffer);
private:
    SE_GeometryDataID mGeomDataID;
    int mSurfaceNum;
    SE_SurfaceTransfer* mSurfaceTransferArray;
    int mTexNum;
    SE_TextureTransfer* mTexTransferArray;
    SE_Vector3f mColor;
};

#endif
