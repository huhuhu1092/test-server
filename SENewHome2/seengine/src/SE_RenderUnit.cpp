#include "SE_RenderUnit.h"
#include "SE_Mesh.h"
#include "SE_ResourceManager.h"
#include "SE_Application.h"
#include "SE_ImageData.h"
#include "SE_Log.h"
#include "SE_GeometryData.h"
#include "SE_TextureCoordData.h"
#include "SE_ID.h"
#include "SE_ShaderProgram.h"
#include "SE_Geometry3D.h"
#include "SE_Spatial.h"
#include "SE_Renderer.h"
#include "SE_DataValueDefine.h"
#include <vector>
#include "SE_VertexBuffer.h"
/////////////////////////////////
SE_RenderUnit::SE_RenderUnit()
{
	memset(mHasTexCoord, 0, sizeof(int) * SE_TEXUNIT_NUM);
	memset(mHasTexture, 0, sizeof(int) * SE_TEXUNIT_NUM);
	//memset(mTexCoordIndex, 0, sizeof(int) * SE_TEXUNIT_NUM);
	//mColorBlendMode = SE_TEXTURE0_MODE;
}
SE_RenderUnit::~SE_RenderUnit()
{}
void SE_RenderUnit::getTexImageID(int texIndex, SE_ImageDataID*& imageDataIDArray, int& imageDataIDNum)
{
}
void SE_RenderUnit::getTexImage(int texIndex, SE_ImageData**& imageDataArray, int& imageDataNum)
{}
void SE_RenderUnit::getVertex(_Vector3f*& vertex, int & vertexNum)
{
	vertex = NULL;
	vertexNum = 0;
}
void SE_RenderUnit::getTexVertex(int texIndex, _Vector2f*& texVertex, int& texVertexNum)
{
	texVertex = NULL;
	texVertexNum = 0;
}
SE_Surface* SE_RenderUnit::getSurface()
{
	return NULL;
}
SE_MaterialData* SE_RenderUnit::getMaterialData()
{
    return 0;
}
SE_Vector3f SE_RenderUnit::getColor()
{
    return SE_Vector3f(0, 0, 0);
}
SE_ProgramDataID SE_RenderUnit::getShaderProgramID() const
{
	return SE_ProgramDataID("");
}
void SE_RenderUnit::setRenderState(SE_Spatial::RENDER_STATE_TYPE type, SE_RenderState* renderState, SE_OWN_TYPE own)
{
	if(type < 0 || type >= SE_Spatial::RENDERSTATE_NUM)
		return;
	SE_PointerOwner<SE_RenderState>* p = &mRenderState[type];
	if(p->own == OWN && p->ptr)
		delete p->ptr;
	p->own = own;
	p->ptr = renderState;
}
void SE_RenderUnit::applyRenderState()
{
	if(!mRenderState)
		return;
	for(int i = 0 ; i < SE_Spatial::RENDERSTATE_NUM ; i++)
	{
		SE_RenderState* rs = mRenderState[i].ptr;
		if(rs)
			rs->apply();
	}
}
void SE_RenderUnit::draw()
{}
#ifdef DEBUG0
static int texSize = 0;
#endif

////////////////////////////////
SE_TriSurfaceRenderUnit::SE_TriSurfaceRenderUnit(SE_Surface* surface)
{
    mSurface = surface;
    mVertex = NULL;
    mTexVertex = NULL;
    mVertexNum = 0;
    mTexVertexNum = 0;
    mPrimitiveType = TRIANGLES;
}
SE_Surface* SE_TriSurfaceRenderUnit::getSurface()
{
	return mSurface;
}
void SE_TriSurfaceRenderUnit::getImage(int texIndex, SE_ImageData**& imageDataArray, int& imageDataNum)
{
    SE_Texture* tex = mSurface->getTexture();
    if(!tex)
    {
        imageDataArray = NULL;
        imageDataNum = 0;
        return;
    }
    SE_TextureUnit* texUnit = tex->getTextureUnit(texIndex);
	if(!texUnit)
	{
		imageDataArray = NULL;
		imageDataNum = 0;
		return;
	}
	texUnit->getImageData(imageDataArray, imageDataNum);
}
void SE_TriSurfaceRenderUnit::getImageDataID(int texIndex, SE_ImageDataID*& imageIDArray, int& imageIDNum)
{
   SE_Texture* tex = mSurface->getTexture();
    if(!tex)
    {
        imageIDArray = NULL;
        imageIDNum = 0;
        return;
    }
    SE_TextureUnit* texUnit = tex->getTextureUnit(texIndex);
	if(!texUnit)
	{
		imageIDArray = NULL;
		imageIDNum = 0;
		return;
	}
    imageIDNum = texUnit->getImageDataIDNum();
    imageIDArray = texUnit->getImageDataID();    

}
void SE_TriSurfaceRenderUnit::getTexImageID(int texIndex, SE_ImageDataID*& imageDataIDArray, int& imageDataIDNum)
{
	if(texIndex < SE_TEXTURE0 || texIndex >= SE_TEXUNIT_NUM)
	{
		imageDataIDArray = NULL;
		imageDataIDNum = 0;
		return;
	}
    return getImageDataID(texIndex, imageDataIDArray, imageDataIDNum);
}
void SE_TriSurfaceRenderUnit::getTexImage(int texIndex, SE_ImageData**& imageDataArray, int& imageDataNum)
{
	if(texIndex < SE_TEXTURE0 || texIndex >= SE_TEXUNIT_NUM)
	{
		imageDataArray = NULL;
		imageDataNum = 0;
		return;
	}
    getImage(texIndex, imageDataArray, imageDataNum);
}
void SE_TriSurfaceRenderUnit::getVertex(_Vector3f*& vertex, int & vertexNum)
{
    if(mVertex != NULL)
    {
        vertex = mVertex;
        vertexNum = mVertexNum;
		SE_ASSERT(0);
        return;
    }
    SE_GeometryData* geomData = mSurface->getGeometryData();
    int facetNum = mSurface->getFacetNum();
    int* facets = mSurface->getFacetArray();
    SE_Vector3i* faceArray = geomData->getFaceArray();
    SE_Vector3f* vertexArray = geomData->getVertexArray();
    mVertex = new _Vector3f[facetNum * 3];
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

SE_MaterialData* SE_TriSurfaceRenderUnit::getMaterialData()
{
    SE_MaterialData* md = mSurface->getMaterialData();
    return md;
}
SE_Vector3f SE_TriSurfaceRenderUnit::getColor()
{
    return mSurface->getColor();
}
SE_ProgramDataID SE_TriSurfaceRenderUnit::getShaderProgramID() const
{
	return mSurface->getProgramDataID();
}
SE_TriSurfaceRenderUnit::~SE_TriSurfaceRenderUnit()
{
	if(mVertex)
        delete[] mVertex;
	if(mTexVertex)
        delete[] mTexVertex;
}

void SE_TriSurfaceRenderUnit::getTexVertex(int index, _Vector2f*& texVertex, int& texVertexNum)
{
	if(mPrimitiveType == TRIANGLES)
    {
        mSurface->getFaceTexVertex(index, texVertex, texVertexNum);
    }
    else if(mPrimitiveType == TRIANGLE_STRIP || mPrimitiveType == TRIANGLE_FAN || mPrimitiveType == TRIANGLES_INDEX)
    {
        mSurface->getTexVertex(index, texVertex, texVertexNum);
	}
}

void SE_TriSurfaceRenderUnit::setTexColorBlendMode(SE_ShaderProgram* shaderProgram)
{

}
void SE_TriSurfaceRenderUnit::draw()
{
    if(mPrimitiveType != TRIANGLES &&
	   mPrimitiveType != TRIANGLE_STRIP && 
	   mPrimitiveType != TRIANGLE_FAN &&
	   mPrimitiveType != TRIANGLES_INDEX)
    {
        return;
    }
	const SE_ProgramDataID& spID = mSurface->getProgramDataID();
	SE_ShaderProgram* shaderProgram = SE_Application::getInstance()->getResourceManager()->getShaderProgram(spID);
	if(!shaderProgram)
		return;
	shaderProgram->use();
	const SE_RendererID rendererID = mSurface->getRendererID();
	SE_Renderer* renderer = SE_Application::getInstance()->getResourceManager()->getRenderer(rendererID);
	if(!renderer)
		return;

	renderer->begin(shaderProgram);
	renderer->setMatrix(this);
	renderer->setPrimitiveType(mPrimitiveType);
	renderer->setSurface(mSurface);
	renderer->setImage(this);
	renderer->setColor(this);

    if(!mSurface->getVertexBuffer())
    {
#ifdef DEBUG0
        LOGI("\n\n renderUnit -> vb is NULL\n\n");
#endif
	renderer->setVertex(this);
	renderer->setTexVertex(this);
    }
    else
    {

        renderer->setVertexBuffer(this);
    }
	renderer->setDrawMode(this);
	renderer->draw();
	renderer->end();
    //float matrixData[16];
    //m.getColumnSequence(matrixData);
    //glUniformMatrix4fv(shaderProgram->getWorldViewPerspectiveMatrixUniformLoc(), 1, 0, matrixData); 
    //checkGLError();
    //shaderProgram->use();
	/*
	_Vector3f* vertex = NULL;
	int vertexNum = 0;
	int* indexArray = NULL;
	int indexNum = 0;
    setImageAndColor(shaderProgram);
    setVertex(shaderProgram, vertex, vertexNum, indexArray, indexNum);
	setTexVertex(shaderProgram, vertexNum);
    setTexColorBlendMode(shaderProgram);
#ifdef DEBUG0
	LOGI("### vertexNum = %d #####\n", vertexNum);
#endif
    if(mPrimitiveType == TRIANGLES)
    {
        glDrawArrays(GL_TRIANGLES, 0, vertexNum);
    }
    else if(mPrimitiveType == TRIANGLE_STRIP)
    {
        glDrawArrays(GL_TRIANGLE_STRIP, 0, vertexNum);
    }
    else if(mPrimitiveType == TRIANGLE_FAN)
    {
        glDrawArrays(GL_TRIANGLE_FAN, 0, vertexNum);
    }
    else if(mPrimitiveType == TRIANGLES_INDEX)
    {
        glDrawElements(GL_TRIANGLES, indexNum, GL_UNSIGNED_INT, indexArray);
    }
    //checkGLError();
	*/

}
//////////////////////////////////
SE_LineSegRenderUnit::SE_LineSegRenderUnit(SE_Segment* seg, int num, const SE_Vector3f& color)
{
	mSegmentNum = num;
	mSegments = NULL;
	if(num > 0)
	{
		mSegments = new SE_Segment[num];
	}
	for(int i = 0 ; i < num;  i++)
	{
		mSegments[i] = seg[i];
	}
	mColor = color;
}
SE_LineSegRenderUnit::~SE_LineSegRenderUnit()
{
    if(mSegments)
		delete[] mSegments;
}
void SE_LineSegRenderUnit::draw()
{
	if(!mSegments)
		return;
	SE_ShaderProgram* shaderProgram = SE_Application::getInstance()->getResourceManager()->getShaderProgram(DEFAULT_SHADER);
    if(!shaderProgram)
		return;
	shaderProgram->use();
	SE_Renderer* renderer = SE_Application::getInstance()->getResourceManager()->getRenderer("lineseg_renderer");
	if(!renderer)
		return;
	renderer->begin(shaderProgram);
	renderer->setMatrix(this);
	renderer->setColor(this);
	renderer->setVertex(this);
	renderer->setDrawMode(this);
	renderer->draw();
	renderer->end();
}
