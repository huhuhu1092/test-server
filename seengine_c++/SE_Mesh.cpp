#include "SE_Mesh.h"
#include <string.h>
SE_TextureUnit::SE_TextureUnit(int type)
{
    mTexCoord = NULL;
    mImageArray = NULL;
    mImageNum = 0;
}
SE_TextureUnit::~SE_TextureUnit()
{
    if(mImageArray)
        delete[] mImageArray;
}
//////////
SE_Surface::SE_Surface()
{
    mTexture = NULL;
    mMaterialData = NULL;
    mFacetArray = NULL;
    mFacetNum = NULL;
}
SE_Surface::~SE_Surface()
{
    if(mFacetArray)
        delete[] mFacetArray;
}
SE_GeometryData* SE_Surface::getGeometryData()
{
    return mGeometryData;
}
SE_Texture* SE_Surface::getTexture()
{
    return mTexture;
}
SE_MaterialData* SE_Surface::getMaterialData()
{
    return mMaterialData;
}
int SE_Surface::getFacetNum()
{
    return mFacetNum;
}
int* SE_Surface::getFacetArray()
{
    return mFacetArray;
}

void SE_Surface::setGeometryData(SE_GeometryData* geomData)
{
    mGeometryData = geomData;
}
void SE_Surface::setMaterialData(SE_MaterialData* materialData)
{
    mMaterialData = materialData;
}
void SE_Surface::setTexture(SE_Texture* texture)
{
    mTexture = texture;
}
void SE_Surface::setFacets(int* facets, int num)
{
    mFacetArray = facets;
    mFacetNum = num;
}
void SE_Surface::setColor(const SE_Vector3f& color)
{
    mColor = color;
}
SE_Vector3f SE_Surface::getColor()
{
    return mColor;
}
SE_ProgramDataID SE_Surface::getProgramDataID()
{
    return mProgramDataID;
}
void SE_Surface::setProgramDataID(const SE_ProgramDataID& programID)
{
    mProgramDataID = programID;
}
///////
SE_Texture::SE_Texture()
{
    mTexUnitArray = new SE_TextureUnit*[TEXUNIT_NUM];
    memset(mTexUnitArray, 0, sizeof(SE_TextureUnit*) * TEXUNIT_NUM);
}
SE_Texture::~SE_Texture()
{
    if(mTexUnitArray)
    {
        for(int i = 0 ; i < TEXUNIT_NUM ; i++)
        {
            if(mTexUnitArray[i])
                delete mTexUnitArray[i];
        }
        delete[] mTexUnitArray;
    }
}
void SE_Texture::setTextureUnit(int texType, SE_TextureUnit* texUnit)
{
    if(texType < 0 || texType >= TEXUNIT_NUM)
        return ;
    mTexUnitArray[texType] = texUnit;
}
SE_TextureUnit* SE_Texture::getTextureUnit(int texType)
{
    if(texType < 0 || texType >= TEXUNIT_NUM)
        return 0;
	return mTexUnitArray[texType];
}
////////
SE_Mesh::SE_Mesh(int surfaceNum, int texNum)
{
    mGeometryData = NULL;
    mSurfaceNum = surfaceNum;
    mTextureNum = texNum;
    mSurfaceArray = new SE_Surface*[mSurfaceNum];
    mTextureArray = new SE_Texture*[mTextureNum];
    memset(mSurfaceArray, 0, sizeof(SE_Surface*) * mSurfaceNum);
    memset(mTextureArray, 0, sizeof(SE_Texture*) * mTextureNum);
}
SE_Mesh::~SE_Mesh()
{
    for(int i = 0 ; i < mSurfaceNum ; i++)
    {
        if(mSurfaceArray[i])
            delete mSurfaceArray[i];
    }
    for(int i = 0 ; i < mSurfaceNum ; i++)
    {
        if(mTextureArray[i])
            delete mTextureArray[i];
    }
    delete[] mSurfaceArray;
    delete[] mTextureArray;
}
