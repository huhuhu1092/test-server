#include "SE_Primitive.h"
#include "SE_GeometryData.h"
#include "SE_TextureCoordData.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include <memory>
//SE_PrimitiveID SE_Primitive::normalizeRectPrimitiveID = SE_CommonID(0, 0, 0, 0);
//SE_PrimitiveID SE_Primitive::normalizeCubePrimitiveID = SE_CommonID(0, 0, 0, 1);
SE_RectPrimitive::SE_RectPrimitive(const SE_Rect3D& rect) : mRect3D(rect)
{
    memset(mImageDataArray, 0, sizeof(SE_Wrapper<_ImageData>*) * SE_Texture::TEXUNIT_NUM);
    mGeometryData = NULL;
    mTexCoordData = NULL;
    mMaterialData = NULL;
    mSampleMin = 0;
    mSampleMag = 0;
    mWrapS = 0;
    mWrapT = 0;
}
SE_RectPrimitive::~SE_RectPrimitive()
{
    mGeometryData->dec();
    mTexCoordData->dec();
    if(mGeometryData->getNum() == 0)
        delete mGeometryData;
    if(mTexCoordData->getNum() == 0)
        delete mTexCoordData;
    if(mMaterialData)
        delete mMaterialData;
    for(int i = 0 ; i < SE_Texture::TEXUNIT_NUM ; i++)
    {
        SE_Wrapper<_ImageData>* p = mImageDataArray[i];
		if(p)
		{
			p->dec();
			if(p->getNum() == 0)
			{
				delete p;
				mImageDataArray[i] = NULL;
			}
		}
    }
}
void SE_RectPrimitive::create(const SE_Rect3D& rect, SE_RectPrimitive*& outPrimitive, SE_PrimitiveID& outPrimitiveID)
{
    SE_RectPrimitive* rectPrimitive = new SE_RectPrimitive(rect);
    if(!rectPrimitive)
    {
        outPrimitive = NULL;
        outPrimitiveID = SE_PrimitiveID::INVALID;
        return;
    }
	SE_Vector3f* vertex = new SE_Vector3f[4];
	if(!vertex)
    {
        delete rectPrimitive;
        outPrimitive = NULL;
        outPrimitiveID = SE_PrimitiveID::INVALID;
		return;
    }
	rectPrimitive->mRect3D.getVertex(vertex);
    SE_Vector3i* faces = new SE_Vector3i[2];
    if(!faces)
    {
        delete rectPrimitive;
        delete[] vertex;
        outPrimitive = NULL;
        outPrimitiveID = SE_PrimitiveID::INVALID;
		return;
    }
    rectPrimitive->mRect3D.getFaces(faces);
    SE_GeometryData* geomData = new SE_GeometryData;
    if(!geomData)
    {
        delete rectPrimitive;
        delete[] vertex;
        delete[] faces;
        outPrimitive = NULL;
        outPrimitiveID = SE_PrimitiveID::INVALID;
		return;
    }
    geomData->setVertexArray(vertex, 4);
	geomData->setFaceArray(faces, 2);
	rectPrimitive->mGeometryData = new SE_Wrapper<SE_GeometryData>(geomData, SE_Wrapper<SE_GeometryData>::NOT_ARRAY);
    if(!rectPrimitive->mGeometryData)
    {
        delete rectPrimitive;
        delete geomData;
        outPrimitive = NULL;
        outPrimitiveID = SE_PrimitiveID::INVALID;
		return;
    }
    SE_Vector2f* texVertex = new SE_Vector2f[4];
    if(!texVertex)
    {
        delete rectPrimitive;
        outPrimitive = NULL;
        outPrimitiveID = SE_PrimitiveID::INVALID;
		return;
    }
    texVertex[0] = SE_Vector2f(0, 0);
    texVertex[1] = SE_Vector2f(1, 0);
    texVertex[2] = SE_Vector2f(1, 1);
    texVertex[3] = SE_Vector2f(0, 1);
    SE_Vector3i* texFaces = new SE_Vector3i[2];
    if(!texFaces)
    {
        delete rectPrimitive;
        delete[] texVertex;
        outPrimitive = NULL;
        outPrimitiveID = SE_PrimitiveID::INVALID;
		return;
    }
    memcpy(texFaces, faces, sizeof(SE_Vector3i) * 2);
    SE_TextureCoordData* texCoordData = new SE_TextureCoordData;
    if(!texCoordData)
    {
        delete rectPrimitive;
        delete[] texVertex;
        delete[] texFaces;
        outPrimitive = NULL;
        outPrimitiveID = SE_PrimitiveID::INVALID;
		return;
    }
    texCoordData->setTexVertexArray(texVertex, 4);
    texCoordData->setTexFaceArray(texFaces, 2);
    rectPrimitive->mTexCoordData = new SE_Wrapper<SE_TextureCoordData>(texCoordData, SE_Wrapper<SE_TextureCoordData>::NOT_ARRAY);
    if(!rectPrimitive->mTexCoordData)
    {
        delete rectPrimitive;
        delete texCoordData;
        outPrimitive = NULL;
        outPrimitiveID = SE_PrimitiveID::INVALID;
		return;
    }
    outPrimitive = rectPrimitive;
    outPrimitiveID = SE_ID::createPrimitiveID();
    SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    resourceManager->setPrimitive(outPrimitiveID, outPrimitive);
}
SE_RectPrimitive* SE_RectPrimitive::clone()
{
     SE_RectPrimitive* primitive = new SE_RectPrimitive(mRect3D);
     if(!primitive)
         return NULL;
     if(mGeometryData)
     {
         primitive->mGeometryData = mGeometryData;
         mGeometryData->inc();
     }
     if(mTexCoordData)
     {
         primitive->mTexCoordData = mTexCoordData;
         mTexCoordData->inc();
     }
     if(mMaterialData)
     {
         primitive->mMaterialData = new SE_MaterialData;
         *primitive->mMaterialData = * mMaterialData;
     }
     primitive->mSampleMin = mSampleMin;
     primitive->mSampleMag = mSampleMag;
     primitive->mWrapS = mWrapS;
     primitive->mWrapT = mWrapT;
     primitive->mColor = mColor;
     primitive->mProgramDataID = mProgramDataID;
     for(int i = 0 ; i < SE_Texture::TEXUNIT_NUM ; i++)
     {
         if(mImageDataArray[i])
         {
             primitive->mImageDataArray[i] = mImageDataArray[i];
             mImageDataArray[i]->inc();
         }
     }
}
void SE_RectPrimitive::setImageData(SE_ImageData* imageData, SE_Texture::TEXUNIT_TYPE texUnitType, SE_OWN_TYPE own)
{
	if(texUnitType >= SE_Texture::TEXUNIT_NUM || texUnitType < SE_Texture::TEXTURE0)
		return;
    _ImageData* img = new _ImageData;
    if(!img)
        return;
    img->imageData = imageData;
    img->own = own;
    SE_Wrapper<_ImageData>* imageDataWrapper = mImageDataArray[texUnitType];
	if(imageDataWrapper)
	{
        imageDataWrapper->dec();
        if(imageDataWrapper->getNum() == 0)
        {
            delete imageDataWrapper;
            mImageDataArray[texUnitType] = NULL;
        }
	}
    imageDataWrapper = new SE_Wrapper<_ImageData>(img, SE_Wrapper<_ImageData>::NOT_ARRAY);
    mImageDataArray[texUnitType] = imageDataWrapper;
}

SE_Mesh* SE_RectPrimitive::createMesh()
{
	std::auto_ptr<SE_Mesh> mesh(new SE_Mesh(1, 1)); // rect has just one surface and one texture
	if(!mesh.get())
		return NULL;
    std::auto_ptr<SE_Surface> surface(new SE_Surface);
    std::auto_ptr<SE_Texture> texture(new SE_Texture);
	mesh->setGeometryData(mGeometryData->getPtr());
    surface->setGeometryData(mGeometryData->getPtr());
    surface->setMaterialData(mMaterialData);
    surface->setTexture(texture.get());
    int facetNum = 2;
    int* facet = new int[2];
    facet[0] = 0;
    facet[1] = 1;
    surface->setFacets(facet, facetNum);
    surface->setColor(mColor);
    surface->setProgramDataID(mProgramDataID);
    surface->setSampleMin(mSampleMin);
    surface->setSampleMag(mSampleMag);
    surface->setWrapS(mWrapS);
    surface->setWrapT(mWrapT);
    for(int i = 0 ; i < SE_Texture::TEXUNIT_NUM ; i++)
    {
        SE_TextureUnit* texUnit = new SE_TextureUnit();
        texUnit->setImageDataNum(1);
		if(mImageDataArray[i])
		    texUnit->setImageData(0, mImageDataArray[i]->getPtr()->imageData);
        texUnit->setTextureCoordData(mTexCoordData->getPtr());
        texture->setTextureUnit(i, texUnit);
    }
    mesh->setSurface(0, surface.get());
    mesh->setTexture(0, texture.get());
    surface.release();
    texture.release();
	SE_Mesh* ret = mesh.release();
	return ret;
}
