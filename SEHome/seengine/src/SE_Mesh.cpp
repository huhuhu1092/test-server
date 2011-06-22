#include "SE_Mesh.h"
#include "SE_GeometryData.h"
#include "SE_TextureCoordData.h"
#include "SE_ShaderProperty.h"
#include <string.h>
#include <set>
SE_TextureUnit::SE_TextureUnit()
{
    mTexCoord = NULL;
    mImageDataIDArray = NULL;
    mImageDataIDNum = 0;
	mImageDataArray = NULL;
	mImageDataNum = 0;

}
SE_TextureUnit::~SE_TextureUnit()
{
    if(mImageDataIDArray)
        delete[] mImageDataIDArray;
	if(mImageDataArray)
		delete[] mImageDataArray;

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
	mVertexNum = 0;
	memset(mTexVertex, 0, sizeof(_Vector2f*) * SE_TEXUNIT_NUM);
    memset(mTexVertexNum, 0, sizeof(int) * SE_TEXUNIT_NUM);
    mFaceVertex = NULL;
    mFaceVertexNum = 0;
    memset(mFaceTexVertex, 0, sizeof(_Vector2f*) * SE_TEXUNIT_NUM);
    memset(mFaceTexVertexNum, 0, sizeof(int) * SE_TEXUNIT_NUM);
    mIndex = NULL;
    mIndexNum = 0; 
    mIndexInGeometryData = NULL;
    mIndexInGeometryDataNum = 0;
	mShaderProperty = NULL;
    //memset(mTexCoordIndex, 0, sizeof(int) * SE_TEXUNIT_NUM);
	//memset(mColorChannelIndex, 0, sizeof(int) * 4);
}
SE_Surface::~SE_Surface()
{
    if(mFacetArray)
        delete[] mFacetArray;
	if(mVertex)
		delete[] mVertex;
	for(int i = 0 ; i < SE_TEXUNIT_NUM ; i++)
	{
	    if(mTexVertex[i])
		    delete[] mTexVertex[i];
		if(mFaceTexVertex[i])
            delete[] mFaceTexVertex[i];
	}
    if(mFaceVertex)
        delete[] mFaceVertex;
    if(mIndex)
        delete[] mIndex;
	if(mIndexInGeometryData)
        delete[] mIndexInGeometryData;
	if(mShaderProperty)
		delete mShaderProperty;
}
void SE_Surface::setShaderProperty(SE_ShaderProperty* sp)
{
	if(mShaderProperty)
		delete mShaderProperty;
	mShaderProperty = sp;
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
SE_RendererID SE_Surface::getRendererID()
{
	return mRendererID;
}
void SE_Surface::getVertexIndexInGeometryData(int*& outArray, int& outNum)
{
    if(mIndexInGeometryData)
    {
        outArray = mIndexInGeometryData;
        outNum = mIndexInGeometryDataNum;
        return;
    }    
    SE_GeometryData* geomData = getGeometryData();
    int facetNum = getFacetNum();
    int* facets = getFacetArray();
    SE_Vector3i* faceArray = geomData->getFaceArray();
    mIndexInGeometryData = new int[facetNum * 3];
    mIndexInGeometryDataNum = facetNum * 3;
    int k = 0;
    for(int i = 0 ; i < facetNum ; i++)
    {
        SE_Vector3i face = faceArray[facets[i]];
        mIndexInGeometryData[k++] = face.x;
        mIndexInGeometryData[k++] = face.y;
        mIndexInGeometryData[k++] = face.z;
    }
    outArray = mIndexInGeometryData;
    outNum = mIndexInGeometryDataNum;
}
void SE_Surface::getFaceVertex(_Vector3f*& vertex, int& vertexNum)
{
    if(mVertex)
    {
        delete[] mVertex;
        mVertex = NULL;
        mVertexNum = 0;
    }
    if(mFaceVertex != NULL)
    {
        vertex = mFaceVertex;
        vertexNum = mFaceVertexNum;
        return;
    }
    SE_GeometryData* geomData = getGeometryData();
    int facetNum = getFacetNum();
    int* facets = getFacetArray();
    SE_Vector3i* faceArray = geomData->getFaceArray();
    SE_Vector3f* vertexArray = geomData->getVertexArray();
    mFaceVertex = new _Vector3f[facetNum * 3];//(_Vector3f*)malloc(sizeof(_Vector3f) * facetNum * 3);//
    mFaceVertexNum = facetNum * 3;
    int k = 0;
    for(int i = 0 ; i < facetNum ; i++)
    {
        SE_Vector3i f = faceArray[facets[i]];
        mFaceVertex[k].d[0] = vertexArray[f.x].x;
        mFaceVertex[k].d[1] = vertexArray[f.x].y;
        mFaceVertex[k].d[2] = vertexArray[f.x].z;
        k++;
        mFaceVertex[k].d[0] = vertexArray[f.y].x;
        mFaceVertex[k].d[1] = vertexArray[f.y].y;
        mFaceVertex[k].d[2] = vertexArray[f.y].z;
        k++;
        mFaceVertex[k].d[0] = vertexArray[f.z].x;
        mFaceVertex[k].d[1] = vertexArray[f.z].y;
        mFaceVertex[k].d[2] = vertexArray[f.z].z;
        k++;
    }
    vertex = mFaceVertex;
    vertexNum = mFaceVertexNum;
}
void SE_Surface::getFaceTexVertex(int texIndex, _Vector2f*& texVertex, int& texVertexNum)
{
	if(texIndex < SE_TEXTURE0 || texIndex >= SE_TEXUNIT_NUM)
	{
		texVertex = NULL;
		texVertexNum = 0;
		return;
	}
    if(mTexVertex[texIndex])
    {
        delete[] mTexVertex[texIndex];
        mTexVertexNum[texIndex] = 0;
    }
    if(mFaceTexVertex[texIndex])
    {
        texVertex = mFaceTexVertex[texIndex];
        texVertexNum = mFaceTexVertexNum[texIndex];
        return;
    }
    SE_Texture* tex = getTexture();
    if(!tex)
    {
        texVertex = NULL;
        texVertexNum = 0;
        return;
    }
    SE_TextureUnit* texUnit = tex->getTextureUnit(texIndex);
    if(!texUnit)
	{
		texVertex = NULL;
		texVertexNum = 0;
		return;
	}
    SE_TextureCoordData* texCoordData = texUnit->getTextureCoordData();
	if(!texCoordData)
	{
		texVertex = NULL;
		texVertexNum = 0;
		return;
	}
    SE_Vector3i* texFaceArray = texCoordData->getTexFaceArray();
    SE_Vector2f* texVertexArray = texCoordData->getTexVertexArray();
	if(texVertexArray == NULL || texFaceArray == NULL)
	{
        texVertex = 0;
        texVertexNum = 0;
		return;
	}
    int texFaceNum = texCoordData->getTexFaceNum();
    int facetNum = getFacetNum();
    int* facets = getFacetArray();
    mFaceTexVertex[texIndex] = new _Vector2f[facetNum * 3];
    mFaceTexVertexNum[texIndex] = facetNum * 3;
    int k = 0 ;
    for(int i = 0 ; i < facetNum ; i++)
    {
        SE_ASSERT(facets[i] < texFaceNum);
        SE_Vector3i f = texFaceArray[facets[i]];
        mFaceTexVertex[texIndex][k].d[0] = texVertexArray[f.x].x;
        mFaceTexVertex[texIndex][k].d[1] = texVertexArray[f.x].y;
        k++;
        mFaceTexVertex[texIndex][k].d[0] = texVertexArray[f.y].x;
        mFaceTexVertex[texIndex][k].d[1] = texVertexArray[f.y].y;
        k++;
        mFaceTexVertex[texIndex][k].d[0] = texVertexArray[f.z].x;
        mFaceTexVertex[texIndex][k].d[1] = texVertexArray[f.z].y;
        k++;
    }
    texVertex = mFaceTexVertex[texIndex];
    texVertexNum = mFaceTexVertexNum[texIndex];
}
void SE_Surface::getVertexIndex(int*& index, int& indexNum)
{
    if(mIndex)
    {
        index = mIndex;
        indexNum = mIndexNum;
        return;
    }
    int facetNum = getFacetNum();
    int* facets = getFacetArray();
    SE_GeometryData* geomData = getGeometryData();
    SE_Vector3i* faceArray = geomData->getFaceArray();
    mIndexNum = facetNum * 3;
    mIndex = new int[mIndexNum];
    int k = 0;
    for(int i = 0 ; i < facetNum ; i++)
    { 
        SE_Vector3i f = faceArray[facets[i]];
        mIndex[k++] = f.x;
        mIndex[k++] = f.y;
        mIndex[k++] = f.z;
    } 
    index = mIndex;
    indexNum = mIndexNum;
}
void SE_Surface::getVertex(_Vector3f*& vertex, int & vertexNum)
{
    if(mFaceVertex)
    {
        delete[] mFaceVertex;
        mFaceVertexNum = 0;
    }
    if(mVertex)
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
	mVertexNum = geomData->getVertexNum();
	mVertex = new _Vector3f[mVertexNum];
	for(int i = 0 ; i < mVertexNum ; i++)
	{
		mVertex[i].d[0] = vertexArray[i].x;
		mVertex[i].d[1] = vertexArray[i].y;
		mVertex[i].d[2] = vertexArray[i].z;
	}
    vertex = mVertex;
    vertexNum = mVertexNum;
}
void SE_Surface::getTexVertex(int texIndex, _Vector2f*& texVertex, int& texVertexNum)
{
	if(texIndex < 0 || texIndex >= SE_TEXUNIT_NUM)
	{
		texVertex = NULL;
		texVertexNum = 0;
		return;
	}
    if(mFaceTexVertex[texIndex])
    {
        delete[] mFaceTexVertex[texIndex];
        mFaceTexVertex[texIndex] = NULL;
        mFaceTexVertexNum[texIndex] = 0;
    }
    if(mTexVertex[texIndex])
    {
        texVertex = mTexVertex[texIndex];
        texVertexNum = mTexVertexNum[texIndex];
        return;
    }
    SE_Texture* tex = getTexture();
    if(!tex)
    {
        texVertex = NULL;
        texVertexNum = 0;
        return;
    }
    SE_TextureUnit* texUnit = tex->getTextureUnit(texIndex);
    if(!texUnit)
	{
		texVertex = NULL;
		texVertexNum = 0;
		return;
	}
    SE_TextureCoordData* texCoordData = texUnit->getTextureCoordData();
	if(!texCoordData)
	{
		texVertex = NULL;
		texVertexNum = 0;
		return;
	}
    SE_Vector3i* texFaceArray = texCoordData->getTexFaceArray();
    SE_Vector2f* texVertexArray = texCoordData->getTexVertexArray();
	if(texVertexArray == NULL || texFaceArray == NULL)
	{
        texVertex = 0;
        texVertexNum = 0;
		return;
	}
	mTexVertexNum[texIndex]  = texCoordData->getTexVertexNum();
	mTexVertex[texIndex] = new _Vector2f[mTexVertexNum[texIndex]];
	for(int i = 0 ; i < mTexVertexNum[texIndex] ; i++)
	{
		mTexVertex[texIndex][i].d[0] = texVertexArray[i].x;
		mTexVertex[texIndex][i].d[1] = texVertexArray[i].y;
	}
	texVertex = mTexVertex[texIndex];
	texVertexNum = mTexVertexNum[texIndex];
}
int SE_Surface::addVertexData(SE_VertexFormat::PosTex0 v, VertexDataList& vertexDataList)
{
    std::list<SE_VertexFormat::PosTex0>::iterator it;
    int i;
    for(i = 0, it = vertexDataList.begin() ; vertexDataList.end() ; it++, i++)
    {
        if(it->pos[0] == v.pos[0] &&
           it->pos[1] == v.pos[1] &&
           it->pos[2] == v.pos[2] &&
           it->tex0[0] == v.tex0[0] &&
           it->tex0[1] == v.tex0[1])
        {
            return i;
        }
    }
    SE_ASSERT(it == vertexData.end());
    vertexDataList.push_back(v);
    return i;
}
SE_VertexBuffer SE_Surface::createPosTex0VertexBuffer()
{
    SE_GeometryData* geomData = getGeometryData();
    int facetNum = getFacetNum();
    int* facets = getFacetArray();
    SE_Vector3i* faceArray = geomData->getFaceArray();
    SE_Vector3f* vertexArray = geomData->getVertexArray();
    SE_Texture* tex = getTexture();
	SE_ASSERT(tex != NULL);
    SE_TextureUnit* texUnit = tex->getTextureUnit(0);
    SE_ASSERT(texUnit != NULL);
    SE_TextureCoordData* texCoordData = texUnit->getTextureCoordData();
	SE_ASSERT(texCoordData != NULL);
    SE_Vector3i* texFaceArray = texCoordData->getTexFaceArray();
    SE_Vector2f* texVertexArray = texCoordData->getTexVertexArray();
	VertexDataList vertexDataList;
	std::list<SE_Vector3i> indexDataList;
	for(int i = 0 ; i < facetNum ; i++)
	{
		int faceIndex = facets[i];
		SE_Vector3i posFace = faceArray[faceIndex];
		SE_Vector3i texFace = texFaceArray[faceIndex];
		SE_VertexFormat::PosTex0 v0, v1, v2;
		v0.pos[0] = vertexArray[posFace.x].x;
		v0.pos[1] = vertexArray[posFace.x].y;
		v0.pos[2] = vertexArray[posFace.x].z;
		v0.tex0[0] = texVertexArray[texFace.x].x;
		v0.tex0[1] = texVertexArray[texFace.x].y;
        int index0 = addVertexData(v0, vertexDataList);
        v1.pos[0] = vertexArray[posFace.y].x;
        v1.pos[1] = vertexArray[posFace.y].y;
        v1.pos[2] = vertexArray[posface.y].z;
        v1.tex0[0] = texVertexArray[texFace.y].x;
        v1.tex0[1] = texVertexArray[texFace.y].y;
        int index1 = addVertexData(v1, vertexDataList);
        v2.pos[0] = vertexArray[posFace.z].x;
        v2.pos[1] = vertexArray[posFace.z].y;
        v2.pos[2] = vertexArray[posface.z].z;
        v2.tex0[0] = texVertexArray[texFace.z].x;
        v2.tex0[1] = texVertexArray[texFace.z].y;
        int index2 = addVertexData(v2, vertexDataList);
        indexDataList.push_back(SE_Vector3i(index0, index1, index2));
	}
    SE_VertexFormat::PosTex0* data = new SE_VertexFormat::PosTex0[vertexDataList.size()];
    VertexDataList::iterator it;
    for(int i = 0 , it = vertexDataList.begin() ; it != vertexDataList.end() ; it++, i++)
    {
        data[i] = *it;
    }
    int* indexData = new int[indexDataList.size() * 3];
    std::list<SE_Vector3i>::iterator itIndex;
    for(int i = 0 , itIndex = indexDataList.begin() ; itIndex != indexDataList.end(); itIndex++, i += 3)
    {
        indexData[i] = it->x;
        indexData[i + 1] = it->y;
        indexData[i + 2] = it->z;       
    }
    SE_VertexBuffer retV;
    retV.vertexData = data;
    retV.vertexDataNum = vertexDataList.size();
    retV.indexData = indexData;
    retV.indexNum = indexDataList.size() * 3;
    return retV;
}
SE_VertexBuffer SE_Surface::getVertexData(SE_VertexFormat::TYPE t)
{
	switch(t)
	{
	case SE_VertexFormat::POSITION_TEX0:
        if(mVertexBuffer.vertexData != NULL)
            return mVertexBuffer;
        else
        {
            mVertexBuffer = createPosTex0VertexBuffer();
            return mVertexBuffer;
        }
		break;
	}
	return SE_VertexBuffer();
}
void SE_Surface::setProgramDataID(const SE_ProgramDataID& programID)
{
    mProgramDataID = programID;
}
void SE_Surface::setRendererID(const SE_RendererID& id)
{
	mRendererID = id;
}
///////
SE_Texture::SE_Texture()
{
    mTexUnitArray = new SE_TextureUnit*[SE_TEXUNIT_NUM];
    memset(mTexUnitArray, 0, sizeof(SE_TextureUnit*) * SE_TEXUNIT_NUM);
}
SE_Texture::~SE_Texture()
{
    if(mTexUnitArray)
    {
        for(int i = 0 ; i < SE_TEXUNIT_NUM ; i++)
        {
            if(mTexUnitArray[i])
                delete mTexUnitArray[i];
        }
        delete[] mTexUnitArray;
    }
}
void SE_Texture::setTextureUnit(int texType, SE_TextureUnit* texUnit)
{
    if(texType < 0 || texType >= SE_TEXUNIT_NUM)
        return ;
    mTexUnitArray[texType] = texUnit;
}
SE_TextureUnit* SE_Texture::getTextureUnit(int texType)
{
    if(texType < 0 || texType >= SE_TEXUNIT_NUM)
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
void SE_Mesh::clearVertexInfo()
{
    for(int i = 0 ; i < mSurfaceNum ; i++)
    {
        SE_Surface* surface = mSurfaceArray[i];
        surface->clearVertexInfo();
    }
}
