#include "SE_Renderer.h"
#include "SE_ShaderProgram.h"
#include "SE_RenderUnit.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_Mesh.h"
#ifdef GLES_20
    #include <GLES2/gl2.h>
#else
    #include <GLES/gl.h>
#endif
SE_Renderer::SE_Renderer()
{
    reset();
}
SE_Renderer::~SE_Renderer()
{}
void SE_Renderer::setMatrix(SE_RenderUnit* renderUnit)
{
	SE_Matrix4f m = renderUnit->getViewToPerspectiveMatrix().mul(renderUnit->getWorldTransform());
	float matrixData[16];
    m.getColumnSequence(matrixData);
    glUniformMatrix4fv(mShaderProgram->getWorldViewPerspectiveMatrixUniformLoc(), 1, 0, matrixData); 
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
			LOGI("### is texture ####\n");
		}
        glBindTexture(GL_TEXTURE_2D, texid);
		return;
		/*
		GLenum error = glGetError();
		if(error == GL_NO_ERROR)
		{
			return;
		}
		else if(error == GL_INVALID_ENUM)
		{
			LOGI("### bindtexture error ###\n");
            glGenTextures(1, &texid);
            checkGLError();
            imageData->setTexID(texid);
		}
		*/
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
        glTexImage2D(GL_TEXTURE_2D, 0, internalFormat, imageData->getWidth(), imageData->getHeight(),0, format, type, imageData->getData());
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
void SE_Renderer::setImage(SE_RenderUnit* renderUnit)
{
}
void SE_Renderer::setColor(SE_RenderUnit* renderUnit)
{}
void SE_Renderer::setVertex(SE_RenderUnit* renderUnit)
{}
void SE_Renderer::setTexVertex(SE_RenderUnit* renderUnit)
{}
void SE_Renderer::setDrawMode(SE_RenderUnit* renderUnit)
{}
void SE_Renderer::begin(SE_ShaderProgram* shaderProgram)
{
	mShaderProgram = shaderProgram;
	mVertex = NULL;
	mVertexNum = 0;
	mIndexArray = NULL;
	mIndexNum = 0;
}
void SE_Renderer::reset()
{
	mShaderProgram = NULL;
	mVertex = NULL;
	mVertexNum = 0;
	mIndexArray = NULL;
	mIndexNum = 0;
}
void SE_Renderer::draw(int primitiveType)
{
    if(mPrimitiveType == TRIANGLES)
    {
        glDrawArrays(GL_TRIANGLES, 0, mVertexNum);
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
//////////////////////////////
SE_SurfaceRenderer::SE_SurfaceRenderer()
{
	mSurface = NULL;
}
SE_SurfaceRenderer::~SE_SurfaceRenderer()
{}
void SE_SurfaceRenderer::setImage(SE_RenderUnit* renderUnit)
{
    for(int index = 0 ; index < SE_TEXUNIT_NUM ; index++)
	{
		SE_ImageDataID* imageDataIDArray = NULL;
		int imageDataIDNum = 0;
		SE_ImageData** imageDataArray;
		int imageDataNum;
		bool hasTexture = false;
		SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
		renderUnit->getTexImageID(index, imageDataIDArray, imageDataIDNum);
		renderUnit->getTexImage(index, imageDataArray, imageDataNum);
		if(imageDataIDNum > 0)
		{
			if(imageDataIDNum == 1)
			{
				SE_ImageData* imageData = resourceManager->getImageData(imageDataIDArray[0]);
				loadTexture2D(index, imageData, (SE_WRAP_TYPE)mSurface->getWrapS(), (SE_WRAP_TYPE)mSurface->getWrapT(), (SE_SAMPLE_TYPE)mSurface->getSampleMin(), (SE_SAMPLE_TYPE)mSurface->getSampleMag());
				glUniform1i(shaderProgram->getTextureUniformLoc(index), index);
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
					glUniform1i(shaderProgram->getTextureUniformLoc(index), index);
					hasTexture = true;
				}

			}
			else
			{
				//load multimap
			}
		}
		mHasTexture[index] = hasTexture;
		//glUniform1i(mShaderProgram->getTextureUniformLoc(index), index);
	}
}
void SE_SurfaceRenderer::setColor(SE_RenderUnit* renderUnit)
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
	for(int i = 0 ; i < 4 ; i++)
	{
	    c = mSurface->getMarkColor(i);
	    color[0] = c.x;
	    color[1] = c.y;
	    color[2] = c.z;
	    glUniform3fv(mShaderProgram->getMarkColorUniformLoc(i), 1, color);
	}
}
void SE_SurfaceRenderer::setVertex(SE_RenderUnit* renderUnit)
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
    glVertexAttribPointer(mShaderProgram->getPositionAttributeLoc(), 3, GL_FLOAT, GL_FALSE, 0, mVertex);
    glEnableVertexAttribArray(mShaderProgram->getPositionAttributeLoc());
}
void SE_SurfaceRenderer::setTexVertex(SE_RenderUnit* renderUnit)
{
    _Vector2f* texVertex = NULL;
    int texVertexNum = 0;
	for(int i = 0 ; i < SE_TEXUNIT_NUM ; i++)
	{
        renderUnit->getTexVertex(i, texVertex, texVertexNum);
		if(texVertexNum > 0)
		{
			SE_ASSERT(mVertexNum == texVertexNum);
			mHasTexCoord[i] = 1;
			glVertexAttribPointer(mShaderProgram->getTextureCoordAttributeLoc(i), 2, GL_FLOAT, 0, 0, texVertex);
	        glEnableVertexAttribArray(mShaderProgram->getTextureCoordAttributeLoc(i));
		}
		else
		{
            mHasTexCoord[i] = 0;
			glDisableVertexAttribArray(mShaderProgram->getTextureCoordAttributeLoc(i));
		}
		glUniform1i(mShaderProgram->getTexCoordIndexUniformLoc(i), mSurface->getTexCoordIndex(i));
	}
}
void SE_SurfaceRenderer::setDrawMode(SE_RenderUnit* renderUnit)
{
	int texMode, colorOp;
	mSurface->getRealTexModeColorOp(mHasTexture, SE_TEXUNIT_NUM, texMode, colorOp);
	glUniform1i(mShaderProgram->getTexCombineModeUniformLoc(), texMode);
	glUniform1i(mShaderProgram->getColorOpModeUniformLoc(), colorOp);
}