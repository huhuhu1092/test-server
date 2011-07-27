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
#include "SE_ID.h"
#include <vector>
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
SE_TriSurfaceRenderUnit::~SE_TriSurfaceRenderUnit()
{
	if(mVertex)
        delete[] mVertex;
	if(mTexVertex)
        delete[] mTexVertex;
}
/*
void SE_TriSurfaceRenderUnit::setColor(SE_ShaderProgram* shaderProgram)
{
    SE_MaterialData* md = mSurface->getMaterialData();
    float color[3];
	SE_Vector3f c = mSurface->getColor();
    if(md)
    {
		color[0] = md->ambient.x;
		color[1] = md->ambient.y;
		color[2] = md->ambient.z;

    }
    else
    {
        color[0] = c.x;
        color[1] = c.y;
        color[2] = c.z;
    }
    //checkGLError();
	glUniform3fv(shaderProgram->getColorUniformLoc(), 1, color);
	for(int i = 0 ; i < 4 ; i++)
	{
	    c = mSurface->getMarkColor(i);
	    color[0] = c.x;
	    color[1] = c.y;
	    color[2] = c.z;
	    glUniform3fv(shaderProgram->getMarkColorUniformLoc(i), 1, color);
	}
    //checkGLError()
}
void SE_TriSurfaceRenderUnit::setImage(int index , SE_ShaderProgram* shaderProgram)
{
	SE_ImageDataID* imageDataIDArray = NULL;
    int imageDataIDNum = 0;
	SE_ImageData** imageDataArray;
	int imageDataNum;
	bool hasTexture = false;
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    getTexImageID(index, imageDataIDArray, imageDataIDNum);
    getTexImage(index, imageDataArray, imageDataNum);
	if(imageDataIDNum > 0)
	{
		if(imageDataIDNum == 1)
		{
            SE_ImageData* imageData = resourceManager->getImageData(imageDataIDArray[0]);
            loadTexture2D(index, imageData, (SE_WRAP_TYPE)mSurface->getWrapS(), (SE_WRAP_TYPE)mSurface->getWrapT(), (SE_SAMPLE_TYPE)mSurface->getSampleMin(), (SE_SAMPLE_TYPE)mSurface->getSampleMag());
            //glUniform1i(shaderProgram->getTextureUniformLoc(index), index);
			hasTexture = true;
        }
		else
		{
			//load multimap
		}
	}
	else if(imageDataNum > 0)
	{
		if(imageDataNum == 1)
		{
			SE_ImageData* imageData = imageDataArray[0];
		    if(imageData)
		    {
                loadTexture2D(index, imageData, (SE_WRAP_TYPE)mSurface->getWrapS(), (SE_WRAP_TYPE)mSurface->getWrapT(), (SE_SAMPLE_TYPE)mSurface->getSampleMin(), (SE_SAMPLE_TYPE)mSurface->getSampleMag());
                //glUniform1i(shaderProgram->getTextureUniformLoc(index), index);
				hasTexture = true;
			}

		}
		else
		{
			//load multimap
		}
	}
	mHasTexture[index] = hasTexture;
	glUniform1i(shaderProgram->getTextureUniformLoc(index), index);
}
void SE_TriSurfaceRenderUnit::setImageAndColor(SE_ShaderProgram* shaderProgram)
{
	for(int i = 0 ; i < SE_TEXUNIT_NUM ; i++)
	{
		setImage(i, shaderProgram);
	}
	setColor(shaderProgram);
}
*/
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
/*
void SE_TriSurfaceRenderUnit::setVertex(SE_ShaderProgram* shaderProgram, _Vector3f*& vertex, int& vertexNum, int*& indexArray, int& indexNum)
{
    vertex = NULL;
    vertexNum = 0;
	indexArray = NULL;
    indexNum = 0;
    if(mPrimitiveType == TRIANGLES)
    {
        mSurface->getFaceVertex(vertex, vertexNum);
    }
    else if(mPrimitiveType == TRIANGLE_STRIP || mPrimitiveType == TRIANGLE_FAN || mPrimitiveType == TRIANGLES_INDEX)
    {
        mSurface->getVertex(vertex, vertexNum);
        mSurface->getVertexIndex(indexArray, indexNum);
    }
    glVertexAttribPointer(shaderProgram->getPositionAttributeLoc(), 3, GL_FLOAT, GL_FALSE, 0, vertex);
    glEnableVertexAttribArray(shaderProgram->getPositionAttributeLoc());
}
void SE_TriSurfaceRenderUnit::setTexVertex(SE_ShaderProgram* shaderProgram, int vertexNum)
{
    _Vector2f* texVertex = NULL;
    int texVertexNum = 0;
	for(int i = 0 ; i < SE_TEXUNIT_NUM ; i++)
	{
        getTexVertex(i, texVertex, texVertexNum);
		if(texVertexNum > 0)
		{
			SE_ASSERT(vertexNum == texVertexNum);
			mHasTexCoord[i] = 1;
			glVertexAttribPointer(shaderProgram->getTextureCoordAttributeLoc(i), 2, GL_FLOAT, 0, 0, texVertex);
	        glEnableVertexAttribArray(shaderProgram->getTextureCoordAttributeLoc(i));
		}
		else
		{
            mHasTexCoord[i] = 0;
			glDisableVertexAttribArray(shaderProgram->getTextureCoordAttributeLoc(i));
		}
		glUniform1i(shaderProgram->getTexCoordIndexUniformLoc(i), mSurface->getTexCoordIndex(i));
	}
}
*/
void SE_TriSurfaceRenderUnit::setTexColorBlendMode(SE_ShaderProgram* shaderProgram)
{
	//int texMode, colorOp;
	//mSurface->getRealTexModeColorOp(mHasTexture, SE_TEXUNIT_NUM, texMode, colorOp);
	//glUniform1i(shaderProgram->getTexCombineModeUniformLoc(), texMode);
	//glUniform1i(shaderProgram->getColorOpModeUniformLoc(), colorOp);

	/*
	bool textureAllFound = true;
	int index = 0;
	int blendMode = mSurface->getColorBlendMode();
	std::vector<int> textureIndexVector = texBlendProperty.textureBlendProperty[blendMode];
	for(int i = 0 ; i < textureIndexVector.size() ; i++)
	{
		if(!mHasTexture[textureIndexVector[i]])
		{
            textureAllFound = false;
			index = textureIndexVector[i];
			break;
		}
	}
	if(textureAllFound)
	{
        glUniform1i(shaderProgram->getTexCombineModeUniformLoc(), blendMode);
	}
	else
	{
        glUniform1i(shaderProgram->getTexCombineModeUniformLoc(), SE_COLOR_MODE);
	    LOGI("... error: texture %d has no image\n", index);
	}
	*/
	/*
    switch(mColorBlendMode)
	{
	case SE_COLOR_MODE:
		glUniform1i(shaderProgram->getTexCombineModeUniformLoc(), SE_COLOR_MODE);
		break;
	case SE_TEXTURE0_MODE:
    case SE_COLOR_TEXTURE0_MODE:
		if(mHasTexture[SE_TEXTURE0])
		{
            glUniform1i(shaderProgram->getTexCombineModeUniformLoc(), SE_TEXTURE0_MODE);
		} 
		else
		{
			glUniform1i(shaderProgram->getTexCombineModeUniformLoc(), SE_COLOR_MODE);
			LOGI("... error: texture0 has no image\n");
		}
		break;
	case SE_TEXTURE1_MODE:
	case SE_COLOR_TEXTURE1_MODE:
		if(mHasTexture[SE_TEXTURE1])
		{
			glUniform1i(shaderProgram->getTexCombineModeUniformLoc(), SE_TEXTURE1_MODE);
		}
		else
		{
			glUniform1i(shaderProgram->getTexCombineModeUniformLoc(), SE_COLOR_MODE);
			LOGI("... error : texture1 has not loaded\n");
		}
		break;
	case SE_TEXTURE2_MODE:
	case SE_COLOR_TEXTURE2_MODE:
		if(mHasTexture[SE_TEXTURE2])
		{
			glUniform1i(shaderProgram->getTexCombineModeUniformLoc(), SE_TEXTURE2_MODE);
		}
		else
		{
			glUniform1i(shaderProgram->getTexCombineModeUniformLoc(), SE_COLOR_MODE);
			LOGI("... error : texture2 has not loaded\n");
		}
		break;	
	case SE_TEXTURE3_MODE:
	case SE_COLOR_TEXTURE3_MODE:
		if(mHasTexture[SE_TEXTURE1])
		{
			glUniform1i(shaderProgram->getTexCombineModeUniformLoc(), SE_TEXTURE3_MODE);
		}
		else
		{
			glUniform1i(shaderProgram->getTexCombineModeUniformLoc(), SE_COLOR_MODE);
			LOGI("... error : texture3 has not loaded\n");
		}
    	break;
	case SE_TEXTURE0_TEXTURE1_MODE:
		break;
	case SE_TEXTURE0_TEXTURE2_MODE:
		break;
	case SE_TEXTURE0_TEXTURE3_MODE:
		break;
	case SE_TEXTURE1_TEXTURE2_MODE:
		break;
	case SE_TEXTURE1_TEXTURE3_MODE:
		break;
	case SE_TEXTURE2_TEXTURE3_MODE:
		break;
	case SE_COLOR_TEXTURE0_TEXTURE1_MODE:
		break;
	case SE_COLOR_TEXTURE0_TEXTURE2_MODE:
		break;
	case SE_COLOR_TEXTURE0_TEXTURE3_MODE:
		break;
	case SE_COLOR_TEXTURE1_TEXTURE2_MODE:
	case SE_COLOR_TEXTURE1_TEXTURE3_MODE:
	case SE_COLOR_TEXTURE2_TEXTURE3_MODE:
	case SE_TEXTURE0_TEXTURE1_TEXTURE2_MODE:
	case SE_TEXTURE0_TEXTURE1_TEXTURE3_MODE:
	case SE_TEXTURE0_TEXTURE2_TEXTURE3_MODE:
	case SE_TEXTURE1_TEXTURE2_TEXTURE3_MODE:
	case SE_COLOR_TEXTURE0_TEXTURE1_TEXTURE2_MODE:
	case SE_COLOR_TEXTURE0_TEXTURE1_TEXTURE3_MODE:
	case SE_COLOR_TEXTURE1_TEXTURE2_TEXTURE3_MODE:
	case SE_COLOR_TEXTURE0_TEXTURE2_TEXTURE3_MODE:
	case SE_TEXTURE0_TEXTURE1_TEXTURE2_TEXTURE3_MODE:
	case SE_COLOR_TEXTURE0_TEXTURE1_TEXTURE2_TEXTURE3_MODE:

	}
	*/
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
	{
		LOGI("### can not find shader program : %s ####\n", spID.getStr());
		return;
	}
	shaderProgram->use();
	const SE_RendererID rendererID = mSurface->getRendererID();
	SE_Renderer* renderer = SE_Application::getInstance()->getResourceManager()->getRenderer(rendererID);
	if(!renderer)
	{
		LOGI("### can not find renderer : %s ####\n", rendererID.getStr());
		return;
	}
	renderer->begin(shaderProgram);
	renderer->setSurface(mSurface);
	renderer->setPrimitiveType(mPrimitiveType);
	renderer->setMatrix(this);
	renderer->setImage(this);
	renderer->setColor(this);
	renderer->setVertex(this);
	renderer->setTexVertex(this);
    //renderer->setVertexBuffer(this);
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
