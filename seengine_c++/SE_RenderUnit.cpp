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

SE_MaterialData SE_TriSurfaceRenderUnit::getMaterialData()
{
    SE_MaterialData* md = mSurface->getMaterialData();
    if(md)
    {
        return md;
    }
    else
    {
        return SE_MaterialData();
    }
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
void SE_TriSurfaceRenderUnit::draw(SE_Renderer& renderer)
{
    SE_Matrix4f m = mViewToPerspectiveMatrix.mul(mWorldTransform);
    renderer.useProgram(mSurface->getProgramID());
    renderer.setMatrix(m);
    SE_ImageDataID* imageDataArray;
    int imageDataNum;
    getBaseColorImageID(imageDataArray, imageDataNum);
    renderer.loadTexture(imageDataArray, imageDataNum);
    renderer.setSampler(mMinSample, mMagSample);
    SE_Vector3f* vertex;
    int vertexNum;
    SE_Vector2f* texVertex;
    int texVertexNum;
    renderer.drawTriangles(vertex, vertexNum, texVertex, texVertexNum);
}
