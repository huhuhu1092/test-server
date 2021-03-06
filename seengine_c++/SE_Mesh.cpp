#include "SE_Mesh.h"
#include "SE_GeometryData.h"
#include "SE_TextureCoordData.h"
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
	mSampleMin = NEAREST;
    mSampleMag = NEAREST;
    mWrapS = REPEAT;
    mWrapT = REPEAT;
	mVertex = NULL;
	mTexVertex = NULL;
}
SE_Surface::~SE_Surface()
{
    if(mFacetArray)
        delete[] mFacetArray;
	if(mVertex)
		delete[] mVertex;
	if(mTexVertex)
		delete[] mTexVertex;
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
void SE_Surface::getVertex(_Vector3f*& vertex, int & vertexNum)
{
    if(mVertex != NULL)
    {
        vertex = mVertex;
        vertexNum = mVertexNum;
        return;
    }
    SE_GeometryData* geomData = getGeometryData();
    int facetNum = getFacetNum();
    int* facets = getFacetArray();
    SE_Vector3i* faceArray = geomData->getFaceArray();
    SE_Vector3f* vertexArray = geomData->getVertexArray();
    mVertex = new _Vector3f[facetNum * 3];//(_Vector3f*)malloc(sizeof(_Vector3f) * facetNum * 3);//
    mVertexNum = facetNum * 3;
    int k = 0;
    for(int i = 0 ; i < facetNum ; i++)
    {
        SE_Vector3i f = faceArray[facets[i]];
        mVertex[k].d[0] = vertexArray[f.x].x;
        mVertex[k].d[1] = vertexArray[f.x].y;
        mVertex[k].d[2] = vertexArray[f.x].z;
        k++;
        mVertex[k].d[0] = vertexArray[f.y].x;
        mVertex[k].d[1] = vertexArray[f.y].y;
        mVertex[k].d[2] = vertexArray[f.y].z;
        k++;
        mVertex[k].d[0] = vertexArray[f.z].x;
        mVertex[k].d[1] = vertexArray[f.z].y;
        mVertex[k].d[2] = vertexArray[f.z].z;
        k++;
    }
    vertex = mVertex;
    vertexNum = mVertexNum;
}
void SE_Surface::getBaseColorTexVertex(_Vector2f*& texVertex, int& texVertexNum)
{
    if(mTexVertex)
    {
        texVertex = mTexVertex;
        texVertexNum = mTexVertexNum;
        return;
    }
    SE_Texture* tex = getTexture();
    if(!tex)
    {
        texVertex = NULL;
        texVertexNum = 0;
        return;
    }
    SE_TextureUnit* texUnit = tex->getTextureUnit(SE_Texture::TEXTURE0);
    SE_ASSERT(texUnit != NULL);
    SE_TextureCoordData* texCoordData = texUnit->getTextureCoordData();
    SE_Vector3i* texFaceArray = texCoordData->getTexFaceArray();
    SE_Vector2f* texVertexArray = texCoordData->getTexVertexArray();
    int texFaceNum = texCoordData->getTexFaceNum();
    int facetNum = getFacetNum();
    int* facets = getFacetArray();
    mTexVertex = new _Vector2f[facetNum * 3];
    mTexVertexNum = facetNum * 3;
    int k = 0 ;
    for(int i = 0 ; i < facetNum ; i++)
    {
        SE_ASSERT(facets[i] < texFaceNum);
        SE_Vector3i f = texFaceArray[facets[i]];
        mTexVertex[k].d[0] = texVertexArray[f.x].x;
        mTexVertex[k].d[1] = texVertexArray[f.x].y;
        k++;
        mTexVertex[k].d[0] = texVertexArray[f.y].x;
        mTexVertex[k].d[1] = texVertexArray[f.y].y;
        k++;
        mTexVertex[k].d[0] = texVertexArray[f.z].x;
        mTexVertex[k].d[1] = texVertexArray[f.z].y;
        k++;
    }
    texVertex = mTexVertex;
    texVertexNum = mTexVertexNum;
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
