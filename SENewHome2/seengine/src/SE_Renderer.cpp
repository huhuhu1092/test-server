#include "SE_Renderer.h"
#include "SE_ShaderProgram.h"
#include "SE_RenderUnit.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_Mesh.h"
#include "SE_Geometry3D.h"
#include "SE_ShaderProperty.h"
#include "SE_Log.h"
#include "SE_SkinJointController.h"
#include "SE_BipedController.h"
#include "SE_SimObjectManager.h"
#include "SE_Camera.h"

#ifdef GLES_20
    #include <GLES2/gl2.h>
#include<GLES2/gl2ext.h>
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
    mVertexBuffer = NULL;
}
SE_Renderer::~SE_Renderer()
{
    
}
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
        //LOGI("### can not load texture: ###\n");
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
        //upload compressed texture format to opengl
        GLint internalFormat = GL_RGB;
        if(imageData->getCompressType() == SE_ImageData::OGL_PVRTC2)
        {
            LOGI("\n\nUse PVR compressed texture\n\n");
            internalFormat = GL_COMPRESSED_RGB_PVRTC_2BPPV1_IMG;
            glCompressedTexImage2D(GL_TEXTURE_2D, 0, internalFormat, imageData->getWidth(), imageData->getHeight(),0,imageData->getDataSizeInByte(),imageData->getData());
            checkGLError();

        }
        else if(imageData->getCompressType() == SE_ImageData::ETC_RGB_4BPP)
        {
            internalFormat = GL_ETC1_RGB8_OES;
            glCompressedTexImage2D(GL_TEXTURE_2D, 0, internalFormat, imageData->getWidth(), imageData->getHeight(),0,imageData->getDataSizeInByte(),imageData->getData());
            checkGLError();
        }

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
            for(int i = 0; i < imageDataIDNum; ++i)
            {
                SE_ImageData* imageData = resourceManager->getImageData(imageDataIDArray[i]);
			    loadTexture2D(i, imageData, (SE_WRAP_TYPE)mSurface->getWrapS(), (SE_WRAP_TYPE)mSurface->getWrapT(), (SE_SAMPLE_TYPE)mSurface->getSampleMin(), (SE_SAMPLE_TYPE)mSurface->getSampleMag());
			    hasTexture = true;
            }

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

    if(mSurface->isVboDraw())
    {
        GLuint vboid = mSurface->getVboID(SE_Surface::VBO_VERTEX);
        if(vboid == 0)
        {            
            glGenBuffers(1,&vboid);

            mSurface->setVboID(SE_Surface::VBO_VERTEX,vboid);

            GLsizeiptr size = mVertexNum * sizeof(_Vector3f);

            glBindBuffer(GL_ARRAY_BUFFER,vboid);
            glBufferData(GL_ARRAY_BUFFER,size,&mVertex->d[0],GL_STATIC_DRAW);            
            
            glEnableVertexAttribArray(mBaseShaderProgram->getPositionAttributeLoc());
            glVertexAttribPointer(mBaseShaderProgram->getPositionAttributeLoc(), 3, GL_FLOAT, GL_FALSE, 0, 0);
            
        }
        else
        {
            //active this vbo
            glBindBuffer(GL_ARRAY_BUFFER,vboid);
            glEnableVertexAttribArray(mBaseShaderProgram->getPositionAttributeLoc());
            glVertexAttribPointer(mBaseShaderProgram->getPositionAttributeLoc(), 3, GL_FLOAT, GL_FALSE, 0, 0);
            
        }
    }
    else
    {
        //no vbo mode
        glBindBuffer(GL_ARRAY_BUFFER,0);
        glVertexAttribPointer(mBaseShaderProgram->getPositionAttributeLoc(), 3, GL_FLOAT, GL_FALSE, 0, mVertex);
        glEnableVertexAttribArray(mBaseShaderProgram->getPositionAttributeLoc());
    }



}

void SE_Renderer::end()
{
    mVertexBuffer = NULL;
    if(mSurface->isVboDraw())
    {
        glBindBuffer(GL_ARRAY_BUFFER,0);
    }    
    else
    {
        for(int j = 0; j < (int)SE_Surface::VBO_TYPECOUNT;++j)
        {
            GLuint vboid = mSurface->getVboID((SE_Surface::vboType)j);
            if(vboid != 0)
            {
                mSurface->setVboID((SE_Surface::vboType)j,0);
                glDeleteBuffers(1,&vboid);
                LOGI("Release vbo success!!\n");
            }
            else
            {
                //If vertex vbo not use,no need continue test.
                break;
            }
        }
    }
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
        mVertexBuffer = NULL;
}
void SE_Renderer::draw()
{    
    if(mPrimitiveType == TRIANGLES)
    {
        if(mVertexBuffer)
        {            
            glDrawElements(GL_TRIANGLES, mVertexBuffer->indexNum, GL_UNSIGNED_SHORT, mVertexBuffer->indexData); 
            
        }
        else
        {            
            glDrawArrays(GL_TRIANGLES, 0, mVertexNum);
        }
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
void SE_Renderer::colorMask(bool red, bool green, bool blue, bool alpha)
{
    glColorMask(red, green, blue, alpha);
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
		glUniform1i(((SE_SimpleSurfaceShaderProgram*)mBaseShaderProgram)->getTextureUniformLoc(), 0);
		glUniform1i(((SE_SimpleSurfaceShaderProgram*)mBaseShaderProgram)->getShadingModeUniformLoc(), 1);
	}
	else
	{
		glUniform1i(((SE_SimpleSurfaceShaderProgram*)mBaseShaderProgram)->getShadingModeUniformLoc(), 0);
	}
}
void SE_SimpleSurfaceRenderer::begin(SE_ShaderProgram* shaderProgram)
{

/*
    int ccount;
    glGetIntegerv(GL_NUM_COMPRESSED_TEXTURE_FORMATS,&ccount);

    int *p = NULL;
    p = new int[ccount];
    glGetIntegerv(GL_COMPRESSED_TEXTURE_FORMATS,p);

    for ( int i = 0; i < ccount; ++i )
	{
		LOGI( "                                           0x%X\n", p[i] );
	}

*/
	SE_Renderer::begin(shaderProgram);
	mBaseShaderProgram = (SE_SimpleSurfaceShaderProgram*)shaderProgram;
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
	glUniform3fv(((SE_SimpleSurfaceShaderProgram*)mBaseShaderProgram)->getColorUniformLoc(), 1, color);   
}
void SE_SimpleSurfaceRenderer::setVertexBuffer(SE_RenderUnit* renderUnit)
{
    SE_VertexBuffer *vb = mSurface->getVertexBuffer();    

    int vertex_pos_size = vb->mPosSize;
    int vertex_tex0_size = vb->mTex0Size;
    int vertex_tex0_offset = vb->mPosSize;// v0 v1 v2 s t|

    int vertex_size = vb->getDataStride();
    
    glVertexAttribPointer(((SE_SimpleSurfaceShaderProgram*)mBaseShaderProgram)->getPositionAttributeLoc(), vertex_pos_size, GL_FLOAT, GL_FALSE, vertex_size * sizeof(float), vb->vertexData);
    glEnableVertexAttribArray(((SE_SimpleSurfaceShaderProgram*)mBaseShaderProgram)->getPositionAttributeLoc());
    
    if(vb->hasVBState(SE_VertexBuffer::VBS_TEXTURE0))
    {
        glVertexAttribPointer(((SE_SimpleSurfaceShaderProgram*)mBaseShaderProgram)->getTexCoordAttributeLoc(), vertex_tex0_size, GL_FLOAT, GL_FALSE, vertex_size * sizeof(float), vb->vertexData + vertex_tex0_offset);
	glEnableVertexAttribArray(((SE_SimpleSurfaceShaderProgram*)mBaseShaderProgram)->getTexCoordAttributeLoc());
    }
    else
    {
        glDisableVertexAttribArray(((SE_SimpleSurfaceShaderProgram*)mBaseShaderProgram)->getTexCoordAttributeLoc());
    }

    mVertexBuffer = vb;
}
void SE_SimpleSurfaceRenderer::setTexVertex(SE_RenderUnit* renderUnit)
{
	SE_Renderer::setTexVertex(0, renderUnit);
	if(mHasTexCoord[0])
	{
        if(mSurface->isVboDraw())
        {
            GLuint vboid = mSurface->getVboID(SE_Surface::VBO_TEXCOORD);
            if(vboid == 0)
            {            
                glGenBuffers(1,&vboid);

                mSurface->setVboID(SE_Surface::VBO_TEXCOORD,vboid);

                GLsizeiptr size = mTexVertexNum * sizeof(_Vector2f);
                
                glBindBuffer(GL_ARRAY_BUFFER,vboid);
                glBufferData(GL_ARRAY_BUFFER,size,mTexVertex,GL_STATIC_DRAW);

                
                glEnableVertexAttribArray(((SE_SimpleSurfaceShaderProgram*)mBaseShaderProgram)->getTexCoordAttributeLoc());
                glVertexAttribPointer(((SE_SimpleSurfaceShaderProgram*)mBaseShaderProgram)->getTexCoordAttributeLoc(), 2, GL_FLOAT, GL_FALSE, 0, 0);

            }
            else
            {
                //active this vbo
                glBindBuffer(GL_ARRAY_BUFFER,vboid);
                glEnableVertexAttribArray(((SE_SimpleSurfaceShaderProgram*)mBaseShaderProgram)->getTexCoordAttributeLoc());
                glVertexAttribPointer(((SE_SimpleSurfaceShaderProgram*)mBaseShaderProgram)->getTexCoordAttributeLoc(), 2, GL_FLOAT, GL_FALSE, 0, 0);
                
            }
        }
        else
        {
            glBindBuffer(GL_ARRAY_BUFFER,0);
            glVertexAttribPointer(((SE_SimpleSurfaceShaderProgram*)mBaseShaderProgram)->getTexCoordAttributeLoc(), 2, GL_FLOAT, 0, 0, mTexVertex);
		    glEnableVertexAttribArray(((SE_SimpleSurfaceShaderProgram*)mBaseShaderProgram)->getTexCoordAttributeLoc());
        }
	}
	else
	{
        glDisableVertexAttribArray(((SE_SimpleSurfaceShaderProgram*)mBaseShaderProgram)->getTexCoordAttributeLoc());
	}
}
void SE_SimpleSurfaceRenderer::setDrawMode(SE_RenderUnit* renderUnit)
{}
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
///////////////////////////////
IMPLEMENT_OBJECT(SE_FadeInOutRenderer)
void SE_FadeInOutRenderer::setImage(SE_RenderUnit* renderUnit)
{
	SE_Renderer::setImage(0, renderUnit);
	if(mHasTexture[0])
	{
		glUniform1i(((SE_FadeInOutShaderProgram*)mBaseShaderProgram)->getTextureUniformLoc(), 0);
		glUniform1i(((SE_FadeInOutShaderProgram*)mBaseShaderProgram)->getShadingModeUniformLoc(), 1);
	}
	else
	{
		glUniform1i(((SE_FadeInOutShaderProgram*)mBaseShaderProgram)->getShadingModeUniformLoc(), 0);
	}
}
void SE_FadeInOutRenderer::begin(SE_ShaderProgram* shaderProgram)
{
	SE_Renderer::begin(shaderProgram);
	mBaseShaderProgram = (SE_FadeInOutShaderProgram*)shaderProgram;
}
void SE_FadeInOutRenderer::setColor(SE_RenderUnit* renderUnit)
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
	glUniform3fv(((SE_FadeInOutShaderProgram*)mBaseShaderProgram)->getColorUniformLoc(), 1, color);

    //process alpha
    float alpha = mSurface->getAlpha();
    GLint result = ((SE_FadeInOutShaderProgram*)mBaseShaderProgram)->getAlphaUniformLoc();

    if(result != -1)
    {
        glUniform1f(result,alpha);
    }

}
void SE_FadeInOutRenderer::setTexVertex(SE_RenderUnit* renderUnit)
{
LOGI("#########fadein fadout !!!!!!!!!!!!!!!!!!!\n\n");
	SE_Renderer::setTexVertex(0, renderUnit);
	if(mHasTexCoord[0])
	{
        glVertexAttribPointer(((SE_FadeInOutShaderProgram*)mBaseShaderProgram)->getTexCoordAttributeLoc(), 2, GL_FLOAT, 0, 0, mTexVertex);
		glEnableVertexAttribArray(((SE_FadeInOutShaderProgram*)mBaseShaderProgram)->getTexCoordAttributeLoc());
	}
	else
	{
        glDisableVertexAttribArray(((SE_FadeInOutShaderProgram*)mBaseShaderProgram)->getTexCoordAttributeLoc());
	}
}
void SE_FadeInOutRenderer::setDrawMode(SE_RenderUnit* renderUnit)
{}



////////////////////////////////////
IMPLEMENT_OBJECT(SE_SkeletalAnimationRenderer)
void SE_SkeletalAnimationRenderer::setImage(SE_RenderUnit* renderUnit)
{
	SE_Renderer::setImage(0, renderUnit);
	if(mHasTexture[0])
	{
		glUniform1i(((SE_SkeletalAnimationShaderProgram*)mBaseShaderProgram)->getTextureUniformLoc(), 0);
		glUniform1i(((SE_SkeletalAnimationShaderProgram*)mBaseShaderProgram)->getShadingModeUniformLoc(), 1);
	}
	else
	{
		glUniform1i(((SE_SkeletalAnimationShaderProgram*)mBaseShaderProgram)->getShadingModeUniformLoc(), 0);
	}    
}
void SE_SkeletalAnimationRenderer::begin(SE_ShaderProgram* shaderProgram)
{
	SE_Renderer::begin(shaderProgram);
	mBaseShaderProgram = (SE_SkeletalAnimationShaderProgram*)shaderProgram;

}

void SE_SkeletalAnimationRenderer::setTexVertex(SE_RenderUnit* renderUnit)
{
	SE_SimpleSurfaceRenderer::setTexVertex(renderUnit);	
    //set skeleton info
    setAnimationData();
}

void SE_SkeletalAnimationRenderer::setAnimationData()
{
    SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();

    const char *controllerID = mSurface->getCurrentBipedControllerID();
    SE_BipedController* bipedController = mSurface->getCurrentBipedController();    

    //set boneIndex
    if(mSurface->getSkeletonIndex() != NULL)
    {
        glVertexAttribPointer(((SE_SkeletalAnimationShaderProgram*)mBaseShaderProgram)->getBoneIndexAttributeLoc(), 4, GL_FLOAT, 0, 0, mSurface->getSkeletonIndex());
        glEnableVertexAttribArray(((SE_SkeletalAnimationShaderProgram*)mBaseShaderProgram)->getBoneIndexAttributeLoc());
    }
    else
    {
        glDisableVertexAttribArray(((SE_SkeletalAnimationShaderProgram*)mBaseShaderProgram)->getBoneIndexAttributeLoc());
    }

    if(mSurface->getSkeletonWeight() != NULL)
    {
        glVertexAttribPointer(((SE_SkeletalAnimationShaderProgram*)mBaseShaderProgram)->getBoneWeightAttributeLoc(), 4, GL_FLOAT, 0, 0, mSurface->getSkeletonWeight());
        glEnableVertexAttribArray(((SE_SkeletalAnimationShaderProgram*)mBaseShaderProgram)->getBoneWeightAttributeLoc());
    }
    else
    {
        glDisableVertexAttribArray(((SE_SkeletalAnimationShaderProgram*)mBaseShaderProgram)->getBoneWeightAttributeLoc());
    }

    int frameindex = mSurface->getCurrentFrameIndex();
    int bipCount = bipedController->oneBipAnimation.size();

    glUniformMatrix4fv(((SE_SkeletalAnimationShaderProgram*)mBaseShaderProgram)->getBoneMatrixUniformLoc(),bipCount,0,&bipedController->AllFrameFinalTransformToShader[frameindex][0]);

}

////////////////////////////////////
IMPLEMENT_OBJECT(SE_SimpleLightingRenderer)
void SE_SimpleLightingRenderer::begin(SE_ShaderProgram* shaderProgram)
{
	SE_Renderer::begin(shaderProgram);
	mBaseShaderProgram = (SE_SimpleLightingShaderProgram*)shaderProgram;

}

void SE_SimpleLightingRenderer::setImage(SE_RenderUnit* renderUnit)
{
	SE_SimpleSurfaceRenderer::setImage(renderUnit);	

    if(!mHasTexture[0])
    {
        //no texture set material
        setMaterialData(renderUnit);
    }
    setLightingData(renderUnit);
}

void SE_SimpleLightingRenderer::setMaterialData(SE_RenderUnit* renderUnit)
{
    SE_MaterialData* md = mSurface->getMaterialData();
    float material[3];
    if(md)
    {
		material[0] = md->ambient.x;
		material[1] = md->ambient.y;
		material[2] = md->ambient.z;
        
        if(((SE_SimpleLightingShaderProgram*)mBaseShaderProgram)->getMaterialAmbientUniformLoc() != -1)
        {
            glUniform3fv(((SE_SimpleLightingShaderProgram*)mBaseShaderProgram)->getMaterialAmbientUniformLoc(), 1, material);
        }


        material[0] = md->diffuse.x;
		material[1] = md->diffuse.y;
		material[2] = md->diffuse.z;
        if(((SE_SimpleLightingShaderProgram*)mBaseShaderProgram)->getMaterialDiffuseUniformLoc() != -1)
        {
            glUniform3fv(((SE_SimpleLightingShaderProgram*)mBaseShaderProgram)->getMaterialDiffuseUniformLoc(), 1, material);
        }
        

        material[0] = md->specular.x;
		material[1] = md->specular.y;
		material[2] = md->specular.z;
        if(((SE_SimpleLightingShaderProgram*)mBaseShaderProgram)->getMaterialSpecularUniformLoc() != -1)
        {
            glUniform3fv(((SE_SimpleLightingShaderProgram*)mBaseShaderProgram)->getMaterialSpecularUniformLoc(), 1, material);
        }
        

        material[0] = md->shiny * 100.0;
		material[1] = 0.0f;
		material[2] = 0.0f;
        if(((SE_SimpleLightingShaderProgram*)mBaseShaderProgram)->getMaterialShinessUniformLoc() != -1)
        {
            glUniform3fv(((SE_SimpleLightingShaderProgram*)mBaseShaderProgram)->getMaterialShinessUniformLoc(), 1, material);
        }        

    }
    
}

void SE_SimpleLightingRenderer::setLightingData(SE_RenderUnit* renderUnit)
{   
    SE_Camera *camera = SE_Application::getInstance()->getCurrentCamera();
    SE_Vector3f cameraLocation = camera->getLocation();

    SE_Matrix4f w2m = renderUnit->getWorldTransform().inverse();
    SE_Matrix4f m2w = renderUnit->getWorldTransform();
    
	float matrixData[16];
    m2w.getColumnSequence(matrixData);
    glUniformMatrix4fv(((SE_SimpleLightingShaderProgram*)mBaseShaderProgram)->getModelToWorldUniformLoc(), 1, 0, matrixData); 


    //w2m.identity();

    float lightpos[3];
    SE_Vector4f lightV(renderUnit->getSurface()->getLightPos(),1);
    SE_Vector4f lightModel = w2m.map(lightV);   

    lightpos[0] = lightModel.x;
    lightpos[1] = lightModel.y;
    lightpos[2] = lightModel.z;

    glUniform3fv(((SE_SimpleLightingShaderProgram*)mBaseShaderProgram)->getLightPosUniformLoc(), 1, lightpos);


    float eyepos[3];
    SE_Vector4f eyeV(lightV.xyz(),1);    
    SE_Vector4f eyeModel = w2m.map(eyeV);
    
    eyepos[0] = eyeModel.x;
    eyepos[1] = eyeModel.y;
    eyepos[2] = eyeModel.z;

    glUniform3fv(((SE_SimpleLightingShaderProgram*)mBaseShaderProgram)->getEyePosUniformLoc(), 1, eyepos);
}

////////////////////////////////////Normal Map////////////////////
IMPLEMENT_OBJECT(SE_NormalMapRenderer)
void SE_NormalMapRenderer::begin(SE_ShaderProgram* shaderProgram)
{
	SE_SimpleLightingRenderer::begin(shaderProgram);
	mBaseShaderProgram = (SE_NormalMapShaderProgram*)shaderProgram;

}

void SE_NormalMapRenderer::setImage(SE_RenderUnit* renderUnit)
{
	SE_SimpleLightingRenderer::setImage(renderUnit);

    glUniform1i(((SE_NormalMapShaderProgram*)mBaseShaderProgram)->getNormalMapUniformLoc(), 1);

    generateTangentSpace(renderUnit);
	
}

void SE_NormalMapRenderer::generateTangentSpace(SE_RenderUnit* renderUnit)
{   
    _Vector3f *tangentArray = NULL;
    int num = 0;
    renderUnit->getSurface()->getVertexTangent(tangentArray,num);

    if(tangentArray)
    {
        if(mSurface->isVboDraw())
        {
            GLuint vboid = mSurface->getVboID(SE_Surface::VBO_NORMALMAP);
            if(vboid == 0)
            {            
                glGenBuffers(1,&vboid);

                mSurface->setVboID(SE_Surface::VBO_NORMALMAP,vboid);

                GLsizeiptr size = num * sizeof(_Vector3f);

                glBindBuffer(GL_ARRAY_BUFFER,vboid);
                glBufferData(GL_ARRAY_BUFFER,size,&tangentArray->d[0],GL_STATIC_DRAW);            
                
                glEnableVertexAttribArray(((SE_NormalMapShaderProgram*)mBaseShaderProgram)->getTangentSpaceAttributeLoc());
                glVertexAttribPointer(((SE_NormalMapShaderProgram*)mBaseShaderProgram)->getTangentSpaceAttributeLoc(), 3, GL_FLOAT, GL_FALSE, 0, 0);
                
            }
            else
            {
                //active this vbo
                glBindBuffer(GL_ARRAY_BUFFER,vboid);
                glEnableVertexAttribArray(((SE_NormalMapShaderProgram*)mBaseShaderProgram)->getTangentSpaceAttributeLoc());
                glVertexAttribPointer(((SE_NormalMapShaderProgram*)mBaseShaderProgram)->getTangentSpaceAttributeLoc(), 3, GL_FLOAT, GL_FALSE, 0, 0);
                
            }
        }
        else
        {
            //use vertex array
            glBindBuffer(GL_ARRAY_BUFFER,0);
            glVertexAttribPointer(((SE_NormalMapShaderProgram*)mBaseShaderProgram)->getTangentSpaceAttributeLoc(), 3, GL_FLOAT, GL_FALSE, 0, tangentArray);
            glEnableVertexAttribArray(((SE_NormalMapShaderProgram*)mBaseShaderProgram)->getTangentSpaceAttributeLoc());
        }
    }
}


