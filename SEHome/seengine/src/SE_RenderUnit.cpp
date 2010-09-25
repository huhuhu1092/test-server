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
static void checkGLError()
{
	/*
    GLenum error = glGetError();
    if(error != GL_NO_ERROR)
    {
        LOGI("### gl error = %d ####\n", error);
        //SE_ASSERT(0);
    }
	*/
}
/////////////////////////////////
SE_RenderUnit::~SE_RenderUnit()
{}
void SE_RenderUnit::getBaseColorImageID(SE_ImageDataID*& imageIDArray, int& imageIDNum)
{
	imageIDArray = NULL;
	imageIDNum = 0;
}
void SE_RenderUnit::getBaseColorImage(SE_ImageData**& imageDataArray, int& imageDataNum)
{
	imageDataArray = NULL;
	imageDataNum = 0;
}
SE_ImageDataID SE_RenderUnit::getBumpMapImageID()
{
    return SE_ImageDataID::INVALID;
}
SE_ImageDataID SE_RenderUnit::getCubeMapImageID()
{
    return SE_ImageDataID::INVALID;
}
void SE_RenderUnit::getVertex(_Vector3f*& vertex, int & vertexNum)
{
	vertex = NULL;
	vertexNum = 0;
}
void SE_RenderUnit::getBaseColorTexVertex(_Vector2f*& texVertex, int& texVertexNum)
{
	texVertex = NULL;
	texVertexNum = 0;
}

void SE_RenderUnit::getBumpMapTexVertex(_Vector2f*& texVertex, int& texVertexNum)
{
	texVertex = NULL;
	texVertexNum = 0;
}
bool SE_RenderUnit::bumpMapCoordSameAsBaseColor()
{
    return true;
}

void SE_RenderUnit::getCubeMapTexVertex(_Vector2f*& texVertex, int& texVertexNum)
{
	texVertex = NULL;
	texVertexNum = 0;
}
bool SE_RenderUnit::cubeMapCoordSameAsBaseColor()
{
    return true;
}

SE_MaterialData* SE_RenderUnit::getMaterialData()
{
    return 0;
}
SE_Vector3f SE_RenderUnit::getColor()
{
    return SE_Vector3f(0, 0, 0);
}

void SE_RenderUnit::draw()
{}
#ifdef DEBUG0
static int texSize = 0;
#endif
void SE_RenderUnit::loadBaseColorTexture2D(SE_ImageData* imageData, SE_WRAP_TYPE wrapS, SE_WRAP_TYPE wrapT, SE_SAMPLE_TYPE min, SE_SAMPLE_TYPE mag)
{
    if(imageData == NULL)
    {
        LOGI("### can not load texture: ###\n");
        return;
    }
    glEnable(GL_TEXTURE_2D);
    //checkGLError();
    glPixelStorei(GL_UNPACK_ALIGNMENT,1);
    checkGLError();
	glActiveTexture(GL_TEXTURE0);
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
        glBindTexture(GL_TEXTURE_2D, texid);
        return;
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
    default:
        wraps = GL_REPEAT;
    }
    switch(wrapT)
    {
    case REPEAT:
        wrapt = GL_REPEAT;
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
    return;
}

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
void SE_TriSurfaceRenderUnit::getBaseColorImageID(SE_ImageDataID*& imageIDArray, int& imageIDNum)
{
    SE_Texture* tex = mSurface->getTexture();
    if(!tex)
    {
        imageIDArray = NULL;
        imageIDNum = 0;
        return;
    }
    SE_TextureUnit* texUnit = tex->getTextureUnit(SE_Texture::TEXTURE0);
	if(!texUnit)
	{
		imageIDArray = NULL;
		imageIDNum = 0;
		return;
	}
    imageIDNum = texUnit->getImageDataIDNum();
    imageIDArray = texUnit->getImageDataID();    
}
static SE_ImageDataID getImageDataID(SE_Surface* surface, int texType)
{
    SE_Texture* tex = surface->getTexture();
    if(!tex)
        return SE_ImageDataID::INVALID;
    SE_TextureUnit* texUnit = tex->getTextureUnit(texType);
    if(!texUnit)
        return SE_ImageDataID::INVALID;
    int imageIDNum = texUnit->getImageDataIDNum();
    SE_ASSERT(imageIDNum == 1);
    SE_ImageDataID id = texUnit->getImageDataID(0);
    return id;

}
SE_ImageDataID SE_TriSurfaceRenderUnit::getBumpMapImageID()
{
    return getImageDataID(mSurface, SE_Texture::TEXTURE1);
}
SE_ImageDataID SE_TriSurfaceRenderUnit::getCubeMapImageID()
{
    return getImageDataID(mSurface, SE_Texture::TEXTURE2);
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

void SE_TriSurfaceRenderUnit::getBaseColorTexVertex(_Vector2f*& texVertex, int& texVertexNum)
{
    if(mTexVertex)
    {
        texVertex = mTexVertex;
        texVertexNum = mTexVertexNum;
        return;
    }
    SE_Texture* tex = mSurface->getTexture();
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
    int facetNum = mSurface->getFacetNum();
    int* facets = mSurface->getFacetArray();
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

void SE_TriSurfaceRenderUnit::getBumpMapTexVertex(_Vector2f*& texVertex, int& texVertexNum)
{}
bool SE_TriSurfaceRenderUnit::bumpMapCoordSameAsBaseColor()
{
    return true;
}
void SE_TriSurfaceRenderUnit::getCubeMapTexVertex(_Vector2f*& texVertex, int& texVertexNum)
{}
bool SE_TriSurfaceRenderUnit::cubeMapCoordSameAsBaseColor()
{
    return true;
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
void SE_TriSurfaceRenderUnit::getBaseColorImage(SE_ImageData**& imageDataArray, int& imageDataNum)
{
    SE_Texture* tex = mSurface->getTexture();
    if(!tex)
    {
        imageDataArray = NULL;
        imageDataNum = 0;
        return;
    }
    SE_TextureUnit* texUnit = tex->getTextureUnit(SE_Texture::TEXTURE0);
	if(!texUnit)
	{
		imageDataArray = NULL;
		imageDataNum = 0;
		return;
	}
	texUnit->getImageData(imageDataArray, imageDataNum);
}
void SE_TriSurfaceRenderUnit::setColorAndMaterial(SE_ShaderProgram* shaderProgram)
{
    SE_MaterialData* md = mSurface->getMaterialData();
    float color[3];
    if(md)
    {
		color[0] = md->ambient.x;
		color[1] = md->ambient.y;
		color[2] = md->ambient.z;

    }
    else
    {
        SE_Vector3f c = mSurface->getColor();
        color[0] = c.x;
        color[1] = c.y;
        color[2] = c.z;
    }
    //checkGLError();
	glUniform3fv(shaderProgram->getColorUnifyLoc(), 1, color);
    //checkGLError();
	glUniform1i(shaderProgram->getShadingModeUnifyLoc(), 0);
    //checkGLError();
}
void SE_TriSurfaceRenderUnit::draw()
{
    SE_Matrix4f m = mViewToPerspective.mul(mWorldTransform);
	const SE_ProgramDataID& spID = mSurface->getProgramDataID();
	SE_ShaderProgram* shaderProgram = SE_Application::getInstance()->getResourceManager()->getShaderProgram(spID);
    if(!shaderProgram)
	{
		shaderProgram = SE_Application::getInstance()->getResourceManager()->getShaderProgram("main_vertex_shader");
	}
    //shaderProgram->use();
    SE_ImageDataID* imageDataArray = NULL;
    int imageDataNum = 0;
    getBaseColorImageID(imageDataArray, imageDataNum);
	SE_ImageData** baseColorImageDataArray;
	int baseColorImageDataNum;
	getBaseColorImage(baseColorImageDataArray, baseColorImageDataNum);
    if(imageDataArray)
    {
		SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
        SE_ImageData* imageData = resourceManager->getImageData(imageDataArray[0]);
        loadBaseColorTexture2D(imageData, (SE_WRAP_TYPE)mSurface->getWrapS(), (SE_WRAP_TYPE)mSurface->getWrapT(), (SE_SAMPLE_TYPE)mSurface->getSampleMin(), (SE_SAMPLE_TYPE)mSurface->getSampleMag());
        glUniform1i(shaderProgram->getBaseColorTextureUnifyLoc(), 0);
        //checkGLError();
		glUniform1i(shaderProgram->getShadingModeUnifyLoc(), 1);
        //checkGLError();
		
    }
    else if(baseColorImageDataNum > 0)
	{
        SE_ImageData* imageData = baseColorImageDataArray[0];
		if(imageData)
		{
            loadBaseColorTexture2D(imageData, (SE_WRAP_TYPE)mSurface->getWrapS(), (SE_WRAP_TYPE)mSurface->getWrapT(), (SE_SAMPLE_TYPE)mSurface->getSampleMin(), (SE_SAMPLE_TYPE)mSurface->getSampleMag());
            glUniform1i(shaderProgram->getBaseColorTextureUnifyLoc(), 0);
            //checkGLError();
		    glUniform1i(shaderProgram->getShadingModeUnifyLoc(), 1);
            //checkGLError();
		}
		else
		{
            setColorAndMaterial(shaderProgram);
		    glDisable(GL_TEXTURE_2D);
		}
	}
	else
	{
        setColorAndMaterial(shaderProgram);
		glDisable(GL_TEXTURE_2D);
    }
    float matrixData[16];
    m.getColumnSequence(matrixData);
    glUniformMatrix4fv(shaderProgram->getWorldViewPerspectiveMatrixUnifyLoc(), 1, 0, matrixData); 
    //checkGLError();
    _Vector3f* vertex = NULL;
    int vertexNum = 0;
    _Vector2f* texVertex = NULL;
    int texVertexNum = 0;
    mSurface->getVertex(vertex, vertexNum);
    mSurface->getBaseColorTexVertex(texVertex, texVertexNum);
	if(texVertexNum > 0)
        SE_ASSERT(vertexNum == texVertexNum);
    glVertexAttribPointer(shaderProgram->getPositionAttributeLoc(), 3, GL_FLOAT, GL_FALSE, 0, vertex);
    //checkGLError();
	if(texVertex)
    {
        glVertexAttribPointer(shaderProgram->getBaseColorTexCoordAttributeLoc(), 2, GL_FLOAT, 0, 0, texVertex);
        //checkGLError();
    }
    glEnableVertexAttribArray(shaderProgram->getPositionAttributeLoc());
    //checkGLError();
    if(texVertex)
    {
	    glEnableVertexAttribArray(shaderProgram->getBaseColorTexCoordAttributeLoc());
        //checkGLError();
    }
    else
    {
        glDisableVertexAttribArray(shaderProgram->getBaseColorTexCoordAttributeLoc());
        //checkGLError();
    }
#ifdef DEBUG0
	LOGI("### vertexNum = %d #####\n", vertexNum);
#endif
    glDrawArrays(GL_TRIANGLES, 0, vertexNum);
    //checkGLError();

}
//////////////////////////////////
SE_LineSegRenderUnit::SE_LineSegRenderUnit(SE_Segment* seg, int num, const SE_Vector3f& color)
{
	mSegmentNum = num;
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
void SE_LineSegRenderUnit::draw()
{
	SE_ShaderProgram* shaderProgram = SE_Application::getInstance()->getResourceManager()->getShaderProgram("main_vertex_shader");
	float color[3];
	color[0] = mColor.x;
	color[1] = mColor.y;
	color[2] = mColor.z;
	glUniform3fv(shaderProgram->getColorUnifyLoc(), 1, color);
	glUniform1i(shaderProgram->getShadingModeUnifyLoc(), 0);
	SE_Matrix4f m;
	m.identity();
	m = mViewToPerspective.mul(m);
	float data[16];
	m.getColumnSequence(data);
    glUniformMatrix4fv(shaderProgram->getWorldViewPerspectiveMatrixUnifyLoc(), 1, 0, data);
    _Vector3f* points = new _Vector3f[mSegmentNum * 2];
	if(!points)
		return;
	int k = 0;
	for(int i = 0 ; i < mSegmentNum ; i++)
	{
		const SE_Segment& se = mSegments[i];
		const SE_Vector3f& start = se.getStart();
		const SE_Vector3f& end = se.getEnd();
		for(int i = 0 ; i < 3 ; i++)
		    points[k].d[i] = start.d[i];
		k++;
		for(int i = 0 ; i < 3 ; i++)
		    points[k].d[i] = end.d[i];
		k++;
	}
	glVertexAttribPointer(shaderProgram->getPositionAttributeLoc(), 3, GL_FLOAT,
		                  GL_FALSE, 0, points);
	glEnableVertexAttribArray(shaderProgram->getPositionAttributeLoc());
	glDrawArrays(GL_LINES, 0, mSegmentNum * 2);
    delete[] points;
}
