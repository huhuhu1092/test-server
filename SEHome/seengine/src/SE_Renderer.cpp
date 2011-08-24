#include "SE_Renderer.h"
#include "SE_ShaderProgram.h"
#include "SE_RenderUnit.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_Mesh.h"
#include "SE_Geometry3D.h"
#include "SE_ShaderProperty.h"
#include "SE_Log.h"
#ifdef GLES_20
    #include <GLES2/gl2.h>
#else
    #include <GLES/gl.h>
#endif
static void checkGLError()
{
	/*
    GLenum error = glGetError();
    if(error != GL_NO_ERROR)
    {
        LOGI("### gl error = %d ####\n", error);
        SE_ASSERT(0);
    }
	*/
}
IMPLEMENT_OBJECT(SE_Renderer)
SE_Renderer::SE_Renderer()
{
}
SE_Renderer::~SE_Renderer()
{}
void SE_Renderer::setMatrix(SE_RenderUnit* renderUnit)
{
	SE_Matrix4f m = renderUnit->getViewToPerspectiveMatrix().mul(renderUnit->getWorldTransform());
	float matrixData[16];
    m.getColumnSequence(matrixData);
    glUniformMatrix4fv(mBaseShaderProgram->getWorldViewPerspectiveMatrixUniformLoc(), 1, 0, matrixData); 
}
void SE_Renderer::loadTexture2D(int index, SE_ImageData* imageData, SE_WRAP_TYPE wrapS, SE_WRAP_TYPE wrapT, SE_SAMPLE_TYPE min, SE_SAMPLE_TYPE mag)
{
    if(imageData == NULL)
    {
        LOGI("### can not load texture: ###\n");
        return;
    }
    glEnable(GL_TEXTURE_2D);
    //checkGLError();
	GLenum texType = GL_TEXTURE0;
	switch(index)
	{
	case 0:
		texType = GL_TEXTURE0;
		break;
	case 1:
		texType = GL_TEXTURE1;
	    break;
	case 2:
		texType = GL_TEXTURE2;
		break;
	case 3:
		texType = GL_TEXTURE3;
		break;
	default:
		break;
	}
    glPixelStorei(GL_UNPACK_ALIGNMENT,1);
    checkGLError();
	glActiveTexture(texType);
    checkGLError();
    GLuint texid = imageData->getTexID();
#ifdef DEBUG0
	LOGI("## texid = %d ##\n", texid);
#endif
    if(texid == 0)
    {
        glGenTextures(1, &texid);
        checkGLError();
        imageData->setTexID(texid);
#ifdef DEBUG0
		LOGI("### texSize = %d ###\n", texSize);
		texSize++;
#endif
    }
    else
    {
		if(glIsTexture(texid) == GL_TRUE)
		{
			//LOGI("### is texture ####\n");
            glBindTexture(GL_TEXTURE_2D, texid);
		    return;
		}
        else
        {
#if defined(ANDROID)
			LOGI("### rebind texture ###\n");
            glGenTextures(1, &texid);
            checkGLError();
            imageData->setTexID(texid);
#elif defined(WIN32)
            glBindTexture(GL_TEXTURE_2D, texid);
		    return;

#endif
		}
    }
    glBindTexture(GL_TEXTURE_2D, texid);
    checkGLError();
    if(!imageData->isCompressTypeByHardware())
    {
        GLint internalFormat = GL_RGB;
        GLenum format = GL_RGB;
        GLenum type = GL_UNSIGNED_BYTE;
        if(imageData->getPixelFormat() == SE_ImageData::RGBA)
        {
            internalFormat = GL_RGBA;
            format = GL_RGBA;
        }
        if(imageData->getPixelFormat() == SE_ImageData::RGB_565)
        {
            type = GL_UNSIGNED_SHORT_5_6_5;
        }
		if(!imageData->isSizePower2())
		{
			imageData->getDataPower2();
		}
        glTexImage2D(GL_TEXTURE_2D, 0, internalFormat, imageData->getWidthPower2(), imageData->getHeightPower2(),0, format, type, imageData->getDataPower2());
        checkGLError();
    }
    else
    {
    }
	GLint wraps , wrapt;
    switch(wrapS)
    {
    case REPEAT:
        wraps = GL_REPEAT;
        break;
    case CLAMP:
        wraps = GL_CLAMP_TO_EDGE;
		break;
    default:
        wraps = GL_REPEAT;
    }
    switch(wrapT)
    {
    case REPEAT:
        wrapt = GL_REPEAT;
		break;
    case CLAMP:
        wrapt = GL_CLAMP_TO_EDGE;
        break;
    default:
        wrapt = GL_REPEAT;
        break;
    }
	GLint sampleMin, sampleMag;
    switch(min)
    {
    case NEAREST:
        sampleMin = GL_NEAREST;
        break;
    case LINEAR:
        sampleMin = GL_LINEAR;
        break;
    default:
        sampleMin = GL_LINEAR;
    }
    switch(mag)
    {
    case NEAREST:
        sampleMag = GL_NEAREST;
        break;
    case LINEAR:
        sampleMag = GL_LINEAR;
        break;
    default:
        sampleMag = GL_LINEAR;
    }
            
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, wraps);
    checkGLError();
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, wrapt);
    checkGLError();
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, sampleMin );
    checkGLError();
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, sampleMag ); 
    checkGLError();
	//glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
}
void SE_Renderer::setVertexBuffer(SE_RenderUnit* renderUnit)
{}
void SE_Renderer::setImage(SE_RenderUnit* renderUnit)
{
}
void SE_Renderer::setImage(int texIndex, SE_RenderUnit* renderUnit)
{
	SE_ImageDataID* imageDataIDArray = NULL;
	int imageDataIDNum = 0;
	SE_ImageData** imageDataArray;
	int imageDataNum;
	bool hasTexture = false;
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	renderUnit->getTexImageID(texIndex, imageDataIDArray, imageDataIDNum);
	renderUnit->getTexImage(texIndex, imageDataArray, imageDataNum);
	if(imageDataIDNum > 0)
	{
		if(imageDataIDNum == 1)
		{
			SE_ImageData* imageData = resourceManager->getImageData(imageDataIDArray[0]);
			loadTexture2D(texIndex, imageData, (SE_WRAP_TYPE)mSurface->getWrapS(), (SE_WRAP_TYPE)mSurface->getWrapT(), (SE_SAMPLE_TYPE)mSurface->getSampleMin(), (SE_SAMPLE_TYPE)mSurface->getSampleMag());
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
				loadTexture2D(texIndex, imageData, (SE_WRAP_TYPE)mSurface->getWrapS(), (SE_WRAP_TYPE)mSurface->getWrapT(), (SE_SAMPLE_TYPE)mSurface->getSampleMin(), (SE_SAMPLE_TYPE)mSurface->getSampleMag());
				hasTexture = true;
			}

		}
		else
		{
			//load multimap
		}
	}
	mHasTexture[texIndex] = hasTexture;
}
void SE_Renderer::setColor(SE_RenderUnit* renderUnit)
{}
void SE_Renderer::setVertex(SE_RenderUnit* renderUnit)
{
	if(mPrimitiveType == TRIANGLES)
    {
        mSurface->getFaceVertex(mVertex, mVertexNum);
    }
    else if(mPrimitiveType == TRIANGLE_STRIP || mPrimitiveType == TRIANGLE_FAN || mPrimitiveType == TRIANGLES_INDEX)
    {
        mSurface->getVertex(mVertex, mVertexNum);
        mSurface->getVertexIndex(mIndexArray, mIndexNum);
    }
    glVertexAttribPointer(mBaseShaderProgram->getPositionAttributeLoc(), 3, GL_FLOAT, GL_FALSE, 0, mVertex);
    glEnableVertexAttribArray(mBaseShaderProgram->getPositionAttributeLoc());
}
void SE_Renderer::setTexVertex(SE_RenderUnit* renderUnit)
{}
void SE_Renderer::setDrawMode(SE_RenderUnit* renderUnit)
{}
void SE_Renderer::setTexVertex(int index, SE_RenderUnit* renderUnit)
{
    renderUnit->getTexVertex(index, mTexVertex, mTexVertexNum);
	if(mTexVertexNum > 0)
	{
		SE_ASSERT(mVertexNum == mTexVertexNum);
		mHasTexCoord[index] = 1;
	}
	else
	{
        mHasTexCoord[index] = 0;
	}
}
void SE_Renderer::begin(SE_ShaderProgram* shaderProgram)
{
	reset();
	mBaseShaderProgram = shaderProgram;
}
void SE_Renderer::reset()
{
	mBaseShaderProgram = NULL;
	mVertex = NULL;
	mVertexNum = 0;
	mIndexArray = NULL;
	mIndexNum = 0;
	mSurface = NULL;
	mTexVertexNum = 0;
	mTexVertex = NULL;
	mPrimitiveType = TRIANGLES;
	for(int i = 0 ; i < SE_TEXUNIT_NUM ; i++)
	{
		mHasTexture[i] = 0;
		mHasTexCoord[i] = 0;
	}
}
void SE_Renderer::draw()
{
    if(mPrimitiveType == TRIANGLES)
    {
        glDrawArrays(GL_TRIANGLES, 0, mVertexNum);
        //glDrawElements(GL_TRIANGLES, mVertexBuffer.indexNum, GL_UNSIGNED_INT, mVertexBuffer.indexData);
    }
    else if(mPrimitiveType == TRIANGLE_STRIP)
    {
        glDrawArrays(GL_TRIANGLE_STRIP, 0, mVertexNum);
    }
    else if(mPrimitiveType == TRIANGLE_FAN)
    {
        glDrawArrays(GL_TRIANGLE_FAN, 0, mVertexNum);
    }
    else if(mPrimitiveType == TRIANGLES_INDEX)
    {
        glDrawElements(GL_TRIANGLES, mIndexNum, GL_UNSIGNED_INT, mIndexArray);
    }
}
void SE_Renderer::end()
{
}
void SE_Renderer::setClearColor(const SE_Vector4f& color)
{
	glClearColor(color.x, color.y, color.z, color.w);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}
void SE_Renderer::clear(int pattern)
{
	switch(pattern)
	{
	case SE_DEPTH_BUFFER:
		glClear(GL_DEPTH_BUFFER_BIT);
		break;
	case SE_COLOR_BUFFER:
		glClear(GL_COLOR_BUFFER_BIT);
		break;
	case SE_DEPTH_BUFFER|SE_COLOR_BUFFER:
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
		break;
	default:
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	}
}
void SE_Renderer::setViewport(int x, int y, int w, int h)
{
	glViewport(x, y, w, h);
}
//////////////////////////////
IMPLEMENT_OBJECT(SE_ColorExtractRenderer)
SE_ColorExtractRenderer::SE_ColorExtractRenderer()
{
}
SE_ColorExtractRenderer::~SE_ColorExtractRenderer()
{}
void SE_ColorExtractRenderer::begin(SE_ShaderProgram* shaderProgram)
{
	SE_Renderer::begin(shaderProgram);
	mShaderProgram = (SE_ColorExtractShaderProgram*)shaderProgram;
}
void SE_ColorExtractRenderer::setImage(SE_RenderUnit* renderUnit)
{
	for(int i = 0 ; i < SE_TEXUNIT_NUM ; i++)
	{
		SE_Renderer::setImage(i, renderUnit);
		{
			glUniform1i(mShaderProgram->getTextureUniformLoc(i), i);
		}
	}
}
void SE_ColorExtractRenderer::setColor(SE_RenderUnit* renderUnit)
{
	/*
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
	glUniform3fv(mShaderProgram->getColorUniformLoc(), 1, color);
	for(int i = 0 ; i < 4 ; i++)
	{
	    c = mSurface->getMarkColor(i);
	    color[0] = c.x;
	    color[1] = c.y;
	    color[2] = c.z;
	    glUniform3fv(mShaderProgram->getMarkColorUniformLoc(i), 1, color);
	}
	*/
}

void SE_ColorExtractRenderer::setTexVertex(SE_RenderUnit* renderUnit)
{
	for(int i = 0 ; i < SE_TEXUNIT_NUM ; i++)
	{
		SE_Renderer::setTexVertex(i, renderUnit);
		if(mHasTexCoord[i])
		{
		    glVertexAttribPointer(mShaderProgram->getTextureCoordAttributeLoc(i), 2, GL_FLOAT, 0, 0, mTexVertex);
            glEnableVertexAttribArray(mShaderProgram->getTextureCoordAttributeLoc(i));
		}
		else
		{
		    glDisableVertexAttribArray(mShaderProgram->getTextureCoordAttributeLoc(i));
		}
	}
}
void SE_ColorExtractRenderer::setDrawMode(SE_RenderUnit* renderUnit)
{
	int colorOp;
	SE_ColorExtractShaderProperty* sp = (SE_ColorExtractShaderProperty*)mSurface->getShaderProperty();
	colorOp = sp->getColorOperationMode();
	glUniform1i(mShaderProgram->getColorOpModeUniformLoc(), colorOp);
}
////////////////////////////
IMPLEMENT_OBJECT(SE_SimpleSurfaceRenderer)
void SE_SimpleSurfaceRenderer::setImage(SE_RenderUnit* renderUnit)
{
	SE_Renderer::setImage(0, renderUnit);
	if(mHasTexture[0])
	{
		glUniform1i(mShaderProgram->getTextureUniformLoc(), 0);
		glUniform1i(mShaderProgram->getShadingModeUniformLoc(), 1);
	}
	else
	{
		glUniform1i(mShaderProgram->getShadingModeUniformLoc(), 0);
	}
}
void SE_SimpleSurfaceRenderer::begin(SE_ShaderProgram* shaderProgram)
{
	SE_Renderer::begin(shaderProgram);
	mShaderProgram = (SE_SimpleSurfaceShaderProgram*)shaderProgram;
}
void SE_SimpleSurfaceRenderer::setColor(SE_RenderUnit* renderUnit)
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
	glUniform3fv(mShaderProgram->getColorUniformLoc(), 1, color);
}
void SE_SimpleSurfaceRenderer::setVertexBuffer(SE_RenderUnit* renderUnit)
{
    SE_VertexBuffer vb = mSurface->getVertexBuffer(SE_VertexFormat::POSITION_TEX0);
    int vertex_pos_size = 3;
    int vertex_tex0_size = 2;
    int vertex_pos_offset = 0;
    int vertex_tex0_offset = 3;
    int vertex_size = vertex_pos_size + vertex_tex0_size;
    glVertexAttribPointer(mBaseShaderProgram->getPositionAttributeLoc(), vertex_pos_size, GL_FLOAT, GL_FALSE, vertex_size * sizeof(float), vb.vertexData);
    glEnableVertexAttribArray(mBaseShaderProgram->getPositionAttributeLoc());
    glVertexAttribPointer(mShaderProgram->getTexCoordAttributeLoc(), vertex_tex0_size, GL_FLOAT, GL_FALSE, vertex_size * sizeof(float), vb.vertexData + vertex_tex0_offset);
	glEnableVertexAttribArray(mShaderProgram->getTexCoordAttributeLoc());
    mVertexBuffer = vb;

}
void SE_SimpleSurfaceRenderer::setTexVertex(SE_RenderUnit* renderUnit)
{
	SE_Renderer::setTexVertex(0, renderUnit);
	if(mHasTexCoord[0])
	{
        glVertexAttribPointer(mShaderProgram->getTexCoordAttributeLoc(), 2, GL_FLOAT, 0, 0, mTexVertex);
		glEnableVertexAttribArray(mShaderProgram->getTexCoordAttributeLoc());
	}
	else
	{
        glDisableVertexAttribArray(mShaderProgram->getTexCoordAttributeLoc());
	}
}
void SE_SimpleSurfaceRenderer::setDrawMode(SE_RenderUnit* renderUnit)
{}
////////////////////////////////////

IMPLEMENT_OBJECT(SE_ColorEffectRenderer)
SE_ColorEffectRenderer::SE_ColorEffectRenderer()
{
	mShaderProgram = NULL;
	mShaderProperty = NULL;
}
SE_ColorEffectRenderer::~SE_ColorEffectRenderer()
{}
void SE_ColorEffectRenderer::setMatrix(SE_RenderUnit* renderUnit)
{
	SE_Renderer::setMatrix(renderUnit);
}
void SE_ColorEffectRenderer::setImage(SE_RenderUnit* renderUnit)
{
    if(!mShaderProperty)
	{
		mShaderProperty = (SE_ColorEffectShaderProperty*)mSurface->getShaderProperty();
	}
	SE_Renderer::setImage(mShaderProperty->getBackgroundTexture(), renderUnit);
	SE_Renderer::setImage(mShaderProperty->getChannelTexture(), renderUnit);
	glUniform1i(mShaderProgram->getTextureUniformLoc(0), mShaderProperty->getBackgroundTexture());
	glUniform1i(mShaderProgram->getTextureUniformLoc(1), mShaderProperty->getChannelTexture());
	int start = 0;
	for(int i = 2 ; i < 6 ; i++)
	{
		glUniform1i(mShaderProgram->getTextureUniformLoc(i), mShaderProperty->getMarkTexture(start));
		start++;
	}
}
void SE_ColorEffectRenderer::setColor(SE_RenderUnit* renderUnit)
{
	SE_Renderer::setColor(renderUnit);
}
void SE_ColorEffectRenderer::setVertex(SE_RenderUnit* renderUnit)
{
	SE_Renderer::setVertex(renderUnit);
}
void SE_ColorEffectRenderer::setTexVertex(SE_RenderUnit* renderUnit)
{
	SE_Renderer::setTexVertex(0, renderUnit);
	if(mHasTexCoord[0])
	{
		glVertexAttribPointer(mShaderProgram->getTexCoordAttribLoc(), 2, GL_FLOAT, 0, 0, mTexVertex);
		glEnableVertexAttribArray(mShaderProgram->getTexCoordAttribLoc());
	}
	else
	{
		glDisableVertexAttribArray(mShaderProgram->getTexCoordAttribLoc());
	}
}
void SE_ColorEffectRenderer::setDrawMode(SE_RenderUnit* renderUnit)
{

	glUniform1f(mShaderProgram->getBackgroundAlphaUniformLoc(), mShaderProperty->getBackgroudnAlpha());
	for(int i = 0 ; i < 4 ; i++)
	{
	    glUniform1i(mShaderProgram->getHasTextureUniformLoc(i), 0);
		glUniform1i(mShaderProgram->getHasMarkUniformLoc(i), mShaderProperty->getHasMark(i));
		glUniform1i(mShaderProgram->getMarkFunctionUniformLoc(i), mShaderProperty->getMarkFunction(i));
		glUniform1f(mShaderProgram->getMarkAlphaUniformLoc(i), mShaderProperty->getMarkAlpha(i));
		glUniform1i(mShaderProgram->getTextureFnUniformLoc(i), mShaderProperty->getTextureFn(i));
		SE_Vector3f v = mShaderProperty->getMarkColor(i);
		glUniform3f(mShaderProgram->getMarkColorUniformLoc(i), v.x, v.y, v.z);
	}


}
void SE_ColorEffectRenderer::begin(SE_ShaderProgram* shaderProgram)
{
	SE_Renderer::begin(shaderProgram);
	mShaderProgram = (SE_ColorEffectShaderProgram*)shaderProgram;
}
void SE_ColorEffectRenderer::draw()
{
	SE_Renderer::draw();
}
void SE_ColorEffectRenderer::end()
{
	//mShaderProgram->validate();
}

///////////////////////////////////////
IMPLEMENT_OBJECT(SE_LineSegRenderer)
SE_LineSegRenderer::SE_LineSegRenderer()
{
	mPoints = NULL;
	mPointNum = 0;
}
SE_LineSegRenderer::~SE_LineSegRenderer()
{}
void SE_LineSegRenderer::begin(SE_ShaderProgram* shaderProgram)
{
	SE_Renderer::begin(shaderProgram);
	mPoints = NULL;
	mPointNum = 0;
	mShaderProgram = (SE_SimpleSurfaceShaderProgram*)shaderProgram;
}
void SE_LineSegRenderer::setColor(SE_RenderUnit* renderUnit)
{
	SE_LineSegRenderUnit* ru = (SE_LineSegRenderUnit*)renderUnit;
	SE_Vector3f colorv = ru->getColor();
	float color[3];
	color[0] = colorv.x;
	color[1] = colorv.y;
	color[2] = colorv.z;
	glUniform3fv(mShaderProgram->getColorUniformLoc(), 1, color);
}
void SE_LineSegRenderer::setMatrix(SE_RenderUnit* renderUnit)
{
	SE_Matrix4f m;
	m.identity();
	m = renderUnit->getViewToPerspectiveMatrix().mul(m);
	float data[16];
	m.getColumnSequence(data);
    glUniformMatrix4fv(mShaderProgram->getWorldViewPerspectiveMatrixUniformLoc(), 1, 0, data);
}
void SE_LineSegRenderer::setVertex(SE_RenderUnit* renderUnit)
{
	SE_LineSegRenderUnit* ru  = (SE_LineSegRenderUnit*)renderUnit;
	int segmentNum = ru->getSegmentsNum();
	SE_Segment* segments = ru->getSegments();
	mPoints = new _Vector3f[segmentNum * 2];
	if(!mPoints)
		return;
	mPointNum = segmentNum * 2;
	int k = 0;
	for(int i = 0 ; i < segmentNum ; i++)
	{
		const SE_Segment& se = segments[i];
		const SE_Vector3f& start = se.getStart();
		const SE_Vector3f& end = se.getEnd();
		for(int i = 0 ; i < 3 ; i++)
		    mPoints[k].d[i] = start.d[i];
		k++;
		for(int i = 0 ; i < 3 ; i++)
		    mPoints[k].d[i] = end.d[i];
		k++;
	}
	glVertexAttribPointer(mShaderProgram->getPositionAttributeLoc(), 3, GL_FLOAT,
		                  GL_FALSE, 0, mPoints);
	glEnableVertexAttribArray(mShaderProgram->getPositionAttributeLoc());
}
void SE_LineSegRenderer::draw()
{
	if(!mPoints)
		return;
	glDrawArrays(GL_LINES, 0, mPointNum);
}
void SE_LineSegRenderer::end()
{
	if(mPoints)
		delete[] mPoints;
}
void SE_LineSegRenderer::setDrawMode(SE_RenderUnit* renderUnit)
{
	glUniform1i(mShaderProgram->getShadingModeUniformLoc(), 0);
}
