#include "SE_RenderUnit.h"
#include "SE_Mesh.h"

SE_RenderUnit::~SE_RenderUnit()
{}
void SE_RenderUnit::getBaseColorImageID(SE_ImageDataID*& imageIDArray, int& imageIDNum)
{}
SE_ImageDataID SE_RenderUnit::getBumpMapImageID()
{
    return SE_ImageDataID::INVALID;
}
SE_ImageDataID SE_RenderUnit::getCubeMapImageID()
{
    return SE_ImageDataID::INVALID;
}
void SE_RenderUnit::getVertex(SE_Vector3f*& vertex, int & vertexNum)
{}
void SE_RenderUnit::getBaseColorTexVertex(SE_Vector2f*& texVertex, int& texVertexNum)
{}

void SE_RenderUnit::getBumpMapTexVertex(SE_Vector2f* texVertex, int& texVertexNum)
{}
bool SE_RenderUnit::bumpMapCoordSameAsBaseColor()
{
    return true;
}

void SE_RenderUnit::getCubeMapTexVertex(SE_Vector2f* texVertex, int& texVertexNum)
{}
bool SE_RenderUnit::cubeMapCoordSameAsBaseColor()
{
    return true;
}

SE_MaterialData SE_RenderUnit::getMaterialData()
{
    return SE_MaterialData();
}
SE_Vector3f SE_RenderUnit::getColor()
{
    return SE_Vector3f(0, 0, 0);
}
void SE_RenderUnit::loadBaseColorTexture2D(const SE_ImageDataID& imageDataID, WRAP_TYPE wrapS, WRAP_TYPE wrapT, SAMPLE_TYPE min, SAMPLE_TYPE mag)
{
    SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    SE_ImageData* imageData = resourceManager->getImageData(imageDataID);
    if(imageData == NULL)
    {
        LOGI("### can not load texture: %s ###\n", imageDataID.getStr());
        return 0;
    }
    glEnable(GL_TEXTURE_2D);
    glPixelStorei(GL_UNPACK_ALIGNMENT,1);
	glActiveTexture(GL_TEXTURE0);
    GLuint texid = imageData->getTexID();
    if(!glIsTexture(texid))
    {
        glGenTexture(1, &texid);
        imageData->setTexID(texid);
    }
    glBindTexture(GL_TEXTURE_2D, texid);
    if(!imageData->isCompressTypeByHardware)
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
    }
    else
    {
    }
    GLint wraps, wrapt;
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
        sampleMin = GL_NEAREAT;
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
        sampleMag = GL_NEAREAT;
        break;
    case LINEAR:
        sampleMag = GL_LINEAR;
        break;
    default:
        sampleMag = GL_LINEAR;
    }
            
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, wraps);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, wrapt);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, sampleMin );
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, sampleMag ); 
    return texid;
}

////////////////////////////////
SE_TriSurfaceRenderUnit::SE_TriSurfaceRenderUnit(SE_Surface* surface)
{
    mSurface = surface;
    mVertex = NULL;
    mTexVertex = NULL;
    mVertexNum = 0;
    mTexVertexNum = 0;
    mPrimitiveType = SE_PRIMITIVE_TPE::TRIANGLES;
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
    imageIDNum = texUnit->getImageNum();
    imageIDArray = texUnit->getImage();    
}
static SE_ImageDataID getImageDataID(SE_Surface* surface, int texType)
{
    SE_Texture* tex = surface->getTexture();
    if(!tex)
        return SE_ImageDataID::INVALID;
    SE_TextureUnit* texUnit = tex->getTextureUnit(texType);
    if(!texUnit)
        return SE_ImageDataID::INVALID;
    int imageIDNum = texUnit->getImageNum();
    SE_ASSERT(imageIDNum == 1);
    SE_ImageDataID id = texUnit->getImage(0);
    return id;

}
SE_ImageDataID SE_TriSurfaceRenderUnit::getBumpMapImageID()
{
    return getImageDataID(mSurface, SE_Texture::TEXTURE1);
}
SE_ImageID SE_TriSurfaceRenderUnit::getCubeMapImageID()
{
    return getImageDataID(mSurface, SE_Texture::TEXTURE2);
}
void SE_TriSurfaceRenderUnit::getVertex(SE_Vector3f*& vertex, int & vertexNum)
{
    if(mVertex != NULL)
    {
        vertex = mVertex;
        vertexNum = mVertexNum;
        return;
    }
    SE_GeometryData* geomData = mSurface->getGeometryData();
    int facetNum = mSurface->getFacetNum();
    int* facets = mSurface->getFacetArray();
    SE_Vector3i* faceArray = geomData->getFaceArray();
    SE_Vector3f* vertexArray = geomData->getVertexArray();
    mVertex = new SE_Vector3f[facetNum * 3];
    mVertexNum = facetNum * 3;
    int k = 0;
    for(int i = 0 ; i < facetNum ; i++)
    {
        SE_Vector3i f = faceArray[facets[i]];
        mVertex[k++] = vertexArray[f.x];
        mVertex[k++] = vertexArray[f.y];
        mVertex[k++] = vertexArray[f.z];
    }
    vertex = mVertex;
    vertexNum = mVertexNum;
}

void SE_TriSurfaceRenderUnit::getBaseColorTexVertex(SE_Vector2f*& texVertex, int& texVertexNum)
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
    SE_Vector3i* texFaceArray = texCoordData->getFaceArray();
    SE_Vector2f* texVertexArray = texCoordData->getTexVertexArray();
    int facetNum = mSurface->getFacetNum();
    int* facets = mSurface->getFacetArray();
    mTexVertex = new SE_Vector2f[facetNum * 3];
    mTexVertexNum = facetNum * 3;
    int k = 0 ;
    for(int i = 0 ; i < facetNum ; i++)
    {
        SE_Vector3i f = texFaceArray[facets[i]];
        mTexVertex[k++] = texVertexArray[f.x];
        mTexVertex[k++] = texVertexArray[f.y];
        mTexVertex[k++] = texVertexArray[f.z];
    }
    texVertex = mTexVertex;
    texVertexNum = mTexVertexNum;
}

void SE_TriSurfaceRenderUnit::getBumpMapTexVertex(SE_Vector2f* texVertex, int& texVertexNum)
{}
bool SE_TriSurfaceRenderUnit::bumpMapCoordSameAsBaseColor()
{
    return true;
}
void SE_TriSurfaceRenderUnit::getCubeMapTexVertex(SE_Vector2f* texVertex, int& texVertexNum)
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
    delete mVertex;
    delete mTexVertex;
}
void SE_TriSurfaceRenderUnit::draw()
{
    SE_Matrix4f m = mViewToPerspectiveMatrix.mul(mWorldTransform);
    SE_ShaderProgram* shaderProgram = mSurface->getShaderProgram();
    shaderProgram->use();
    SE_ImageDataID* imageDataArray;
    int imageDataNum;
    getBaseColorImageID(imageDataArray, imageDataNum);
    if(imageDataArray)
    {
        loadBaseColorTexture2D(imageDataArray[0], mSurface->getWraps(), mSurface->getWrapT(), mSurface->getMinSample(), mSurface->getMagSample());
        glUniform1i(shaderProgram->getBaseColorTextureUnifyLoc, 0);
		glUniform1i(shaderProgram->getShadingModeUnifyLoc, 1);
    }
    else
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
		glUniform3fv(shaderProgram->getColorUnifyLoc(), 1, color);
		glUniform1i(shaderProgram->getShaderModeUnifyLoc(), 0);
    }
    float matrixData[16];
    m.getColumnSequence(matrixData);
    glUniformMatrix4fv(shaderProgram->getWorldViewPerspectiveMatrixUnifyLoc(), 1, 0, matrixData); 
    SE_Vector3f* vertex;
    int vertexNum;
    SE_Vector2f* texVertex;
    int texVertexNum;
    getVertex(vertex, vertexNum);
    getBaseColorTexVertex(texVertex, texVertexNum);
    SE_ASSERT(vertexNum == texVertexNum);
    glVertexAttribPointer(shaderProgram->getPositionAttributeLoc(), 3, GL_FLOAT, GL_FALSE, 0, vertex);
	if(texVertex)
        glVertexAttribPointer(shaderProgram->getBaseColorTexCoordAttributeLoc(), 2, GL_FLOAT, 0, 0, texVertex);
    glEnableVertexAttribArray(shaderProgram->getPositionAttributeLoc());
	glEnableVertexAttribArray(shaderProgram->getBaseColorTexCoordAttributeLoc());
    glDrawArrays(GL_TRIANGLES, 0, vertexNum);

}
