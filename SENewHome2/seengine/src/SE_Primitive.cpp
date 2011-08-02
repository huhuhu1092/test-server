#include "SE_Primitive.h"
#include "SE_GeometryData.h"
#include "SE_TextureCoordData.h"
#include "SE_Application.h"
#include "SE_Utils.h"
#include "SE_ResourceManager.h"
#include "SE_Mesh.h"
#include "SE_Geometry.h"
#include "SE_MeshSimObject.h"
#include "SE_SimObjectManager.h"
#include "SE_SceneManager.h"
#include "SE_CommonNode.h"
#include "SE_DataValueDefine.h"
#include "SE_Log.h"
#include <memory>
//SE_PrimitiveID SE_Primitive::normalizeRectPrimitiveID = SE_CommonID(0, 0, 0, 0);
//SE_PrimitiveID SE_Primitive::normalizeCubePrimitiveID = SE_CommonID(0, 0, 0, 1);
SE_RectPrimitive::SE_RectPrimitive(const SE_Rect3D& rect) : mRect3D(rect)
{
    memset(mImageDataArray, 0, sizeof(SE_Wrapper<_ImageData>*) * SE_TEXUNIT_NUM);
    mGeometryData = NULL;
    mTexCoordData = NULL;
    mMaterialData = NULL;
    mSampleMin = 0;
    mSampleMag = 0;
    mWrapS = 0;
    mWrapT = 0;
    mAdjustedStartX = 0;
    mAdjustedStartY = 0;
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
    for(int i = 0 ; i < SE_TEXUNIT_NUM ; i++)
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
     for(int i = 0 ; i < SE_TEXUNIT_NUM ; i++)
     {
         if(mImageDataArray[i])
         {
             primitive->mImageDataArray[i] = mImageDataArray[i];
             mImageDataArray[i]->inc();
         }
     }
     return primitive;
}
/*
void SE_RectPrimitive::setImagePortion(const SE_ImageDataPortion& portion)
{
    if(mTexCoordData)
    {
        SE_TextureCoordData* texCoordData = mTexCoordData->getPtr();
        memcpy(texFaces, texCoordData->getTexFaceArray, sizeof(SE_Vector3i) * 2);
        mTexCoordData->dec();
        if(mTexCoordData->getNum() == 0)
        {
            delete mTexCoordData;
            mTexCoordData = NULL;
        }
    }
    SE_TextureCoordData* texCoordData = new SE_TextureCoordData;
    SE_Vector2f* texVertex = new SE_Vector2f[4];
    if(!portion.isValid())
    {
        texVertex[0] = SE_Vector2f(startx / (float)power2Width, starty / (float)power2Height);
        texVertex[1] = SE_Vector2f(1 - startx / (float)power2Width, starty / (float)power2Height);
        texVertex[2] = SE_Vector2f(1 - startx / (float)power2Width, 1 - starty / (float)power2Height);
        texVertex[3] = SE_Vector2f(startx / (float)power2Width, 1 - starty / (float)power2Height);
    }
    else
    {
        int portionx = imageDataPortion.getX();
        int portiony = imageDataPortion.getY();
        int portionw = imageDataPortion.getWidth();
        int portionh = imageDataPortion.getHeight();
        texVertex[0] = SE_Vector2f((startx + portionx) / (float)power2Width, (power2Height - (starty + height - portionh - portiony)) / (float)power2Height);
        texVertex[1] = SE_Vector2f((startx + portionx + portionw) / (float)power2Width, (power2Height - (starty + height - portionh - portiony)) / (float)power2Height);
        texVertex[2] = SE_Vector2f((startx + portionx + portionw) / (float)power2Width, (power2Height - starty - portiony) / (float)portion2Height);
        texVertex[3] = SE_Vector2f((startx + portionx) / (float)power2Width,  (power2Height - starty - portiony) / (float)portion2Height);
    }
    texCoordData->setTexVertexArray(texVertex, 4);
    texCoordData->setTexFaceArray(texFaces, 2);
    mTexCoordData  = new SE_Wrapper<SE_TextureCoordData>(texCoordData, SE_Wrapper<SE_TextureCoordData>::NOT_ARRAY);
}
*/
void SE_RectPrimitive::setImageData(SE_ImageData* imageData, SE_TEXUNIT_TYPE texUnitType, SE_OWN_TYPE own, SE_ImageDataPortion imageDataPortion)
{
    if(texUnitType >= SE_TEXUNIT_NUM || texUnitType < SE_TEXTURE0)
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
    if(!imageData)
    {
        return;
    }
    int width = imageData->getWidth();
    int height = imageData->getHeight();
    int power2Width = width;
    int power2Height = height;
    if(!SE_Util::isPower2(width))
    {
        power2Width = SE_Util::higherPower2(width);
    }
    if(!SE_Util::isPower2(height))
    {
        power2Height = SE_Util::higherPower2(height);
    }
    int startx = 0;
    int starty = 0;
    if(width != power2Width || height != power2Height)
    {
        int pixelSize = imageData->getPixelSize();
        int size = power2Width * power2Height * pixelSize;
        char* data = new char[size];
        if(!data)
        {
            delete imageDataWrapper;
            mImageDataArray[texUnitType] = NULL;
            return;
        }
        memset(data, 0, size);
        char* src = imageData->getData();
        starty = (power2Height - height) >> 1;
        startx = (power2Width - width) >> 1;
        for(int y = 0 ; y < height ; y++)
        {
            char* ydst = &data[(starty + y) * power2Width * pixelSize];
            memcpy(&ydst[startx * pixelSize], src, width * pixelSize);
            src += width * pixelSize;
        }
        SE_ImageData* imageDataPower2 = new SE_ImageData;
        if(!imageDataPower2)
        {
            delete[] data;
            delete imageDataWrapper;
            mImageDataArray[texUnitType] = NULL;
            return;
        }
        imageDataPower2->setWidth(power2Width);
        imageDataPower2->setHeight(power2Height);
        imageDataPower2->setPixelFormat(imageData->getPixelFormat());
        imageDataPower2->setBytesPerRow(power2Width * pixelSize);
        imageDataPower2->setCompressType(SE_ImageData::RAW);
        imageDataPower2->setData(data);
        delete imageDataWrapper;
        img = new _ImageData;
        img->imageData = imageDataPower2;
        img->own = OWN;
        imageDataWrapper = new SE_Wrapper<_ImageData>(img, SE_Wrapper<_ImageData>::NOT_ARRAY);
        mImageDataArray[texUnitType] = imageDataWrapper;
    }
    SE_Vector3i* texFaces = new SE_Vector3i[2];
    if(mTexCoordData)
    {
        SE_TextureCoordData* texCoordData = mTexCoordData->getPtr();
        memcpy(texFaces, texCoordData->getTexFaceArray(), sizeof(SE_Vector3i) * 2);
        mTexCoordData->dec();
        if(mTexCoordData->getNum() == 0)
        {
            delete mTexCoordData;
            mTexCoordData = NULL;
        }
    }
    SE_TextureCoordData* texCoordData = new SE_TextureCoordData;
    SE_Vector2f* texVertex = new SE_Vector2f[4];
    SE_Wrapper<_ImageData>* imageDataPower2 = mImageDataArray[texUnitType];

    if(!imageDataPortion.isValid())
    {
        if(imageDataPower2->getPtr()->imageData->isFliped())
        {
            texVertex[0] = SE_Vector2f(startx / (float)power2Width, starty / (float)power2Height);
            texVertex[1] = SE_Vector2f(1 - startx / (float)power2Width, starty / (float)power2Height);
            texVertex[2] = SE_Vector2f(1 - startx / (float)power2Width, 1 - starty / (float)power2Height);
            texVertex[3] = SE_Vector2f(startx / (float)power2Width, 1 - starty / (float)power2Height);
        }
        else
        {
            texVertex[0] = SE_Vector2f(startx / (float)power2Width, 1 - starty / (float)power2Height);
            texVertex[1] = SE_Vector2f(1 - startx / (float)power2Width, 1 - starty / (float)power2Height);
            texVertex[2] = SE_Vector2f(1 - startx / (float)power2Width, starty / (float)power2Height);
            texVertex[3] = SE_Vector2f(startx / (float)power2Width, starty / (float)power2Height);
        }
    }
    else
    {
        int portionx = imageDataPortion.getX();
        int portiony = imageDataPortion.getY();
        int portionw = imageDataPortion.getWidth();
        int portionh = imageDataPortion.getHeight();
        if(imageDataPower2->getPtr()->imageData->isFliped())
        {
            texVertex[0] = SE_Vector2f((startx + portionx) / (float)power2Width, (power2Height - starty - portionh - portiony) / (float)power2Height);
            texVertex[1] = SE_Vector2f((startx + portionx + portionw) / (float)power2Width, (power2Height - starty - portionh - portiony) / (float)power2Height);
            texVertex[2] = SE_Vector2f((startx + portionx + portionw) / (float)power2Width, (power2Height - starty - portiony) / (float)power2Height);
            texVertex[3] = SE_Vector2f((startx + portionx) / (float)power2Width,  (power2Height - starty - portiony) / (float)power2Height);
        }
        else
        {
            texVertex[0] = SE_Vector2f((startx + portionx) / (float)power2Width,  (starty + portiony + portionh) / (float)power2Height);
            texVertex[1] = SE_Vector2f((startx + portionx + portionw) / (float)power2Width, (starty + portiony + portionh) / (float)power2Height);
            texVertex[2] = SE_Vector2f((startx + portionx + portionw) / (float)power2Width, (starty + portiony) / (float)power2Height);
            texVertex[3] = SE_Vector2f((startx + portionx) / (float)power2Width, (starty + portiony) / (float)power2Height);
        }
    }
    texCoordData->setTexVertexArray(texVertex, 4);
    texCoordData->setTexFaceArray(texFaces, 2);
    mTexCoordData  = new SE_Wrapper<SE_TextureCoordData>(texCoordData, SE_Wrapper<SE_TextureCoordData>::NOT_ARRAY);
    mAdjustedStartX = startx;
    mAdjustedStartY = starty;
}

void SE_RectPrimitive::createMesh(SE_Mesh**& outMesh, int& outMeshNum)
{
    std::auto_ptr<SE_Mesh> mesh(new SE_Mesh(1, 1)); // rect has just one surface and one texture
    if(!mesh.get())
    {
        outMesh = NULL;
        outMeshNum = 0;
        return;
    }
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

    for(int i = 0 ; i < SE_TEXUNIT_NUM ; i++)
    {
        if(mImageDataArray[i])
        {
			SE_TextureUnit* texUnit = new SE_TextureUnit();
			texUnit->setSampleMin(mSampleMin);
            texUnit->setSampleMag(mSampleMag);
            texUnit->setWrapS(mWrapS);
            texUnit->setWrapT(mWrapT);
            texUnit->setImageDataNum(1);
            mImageData = mImageDataArray[i]->getPtr()->imageData;
            texUnit->setImageData(i, mImageData);
			texUnit->setTextureCoordData(mTexCoordData->getPtr());
            texture->setTextureUnit(i, texUnit);
        }        
    }
    mesh->setSurface(0, surface.get());
    mesh->setTexture(0, texture.get());
    surface.release();
    texture.release();
    SE_Mesh* ret = mesh.release();
    outMesh = new SE_Mesh*[1];
    outMesh[0] = ret;
    outMeshNum = 1;
    return;
}

/////////
void SE_BoxPrimitive::create(const SE_Vector3f& scale, SE_BoxPrimitive*& outPrimitive, SE_PrimitiveID& outPrimitiveID)
{
    float e[6][2] = { {scale.x, scale.y},
                      {scale.x, scale.y },
                      {scale.z, scale.y},
                      {scale.z, scale.y},
                      {scale.x, scale.z},
                      {scale.x, scale.z}
    }; 
    //front, back, left, right, top, bottom
    SE_Rect3D rectArray[] = { SE_Rect3D(SE_Vector3f(0, 0, 1), SE_Vector3f(1, 0, 0), SE_Vector3f(0, 1, 0), e[0]),
                              SE_Rect3D(SE_Vector3f(0, 0, -1), SE_Vector3f(-1, 0, 0),SE_Vector3f(0, 1, 0), e[1]),
                              SE_Rect3D(SE_Vector3f(-1, 0, 0), SE_Vector3f(0, 0, 1), SE_Vector3f(0, 1, 0), e[2]),
                              SE_Rect3D(SE_Vector3f(1, 0, 0), SE_Vector3f(0, 0, -1), SE_Vector3f(0, 1, 0), e[3]),
                              SE_Rect3D(SE_Vector3f(0, 1, 0), SE_Vector3f(1, 0, 0), SE_Vector3f(0, 0, -1), e[4]),
                              SE_Rect3D(SE_Vector3f(0, -1, 0), SE_Vector3f(1, 0, 0), SE_Vector3f(0, 0, 1), e[5])
                            };
#ifdef DEBUG
    {
        SE_Vector3f vertexarray[8] = {
                                      SE_Vector3f(-1, -1, 1).mul(scale),
                                      SE_Vector3f(1, -1, 1).mul(scale),
                                      SE_Vector3f(1, 1, 1).mul(scale),
                                      SE_Vector3f(-1, 1, 1).mul(scale),
                                      SE_Vector3f(-1, -1, -1).mul(scale),
                                      SE_Vector3f(1, -1, -1).mul(scale),
                                      SE_Vector3f(1, 1, -1).mul(scale),
                                      SE_Vector3f(-1, 1, -1).mul(scale)
                                     };
        int index[6][4] = {
                           {0, 1, 2, 3},
                           {5, 4, 7, 6},
                           {4, 0, 3, 7},
                           {1, 5, 6 ,2},
                           {3, 2, 6, 7},
                           {4, 5, 1, 0}
                         }; 
        for(int i = 0 ; i < 6 ; i++)
        {
            SE_Vector3f vertex[4];
            SE_Rect3D* rect = &rectArray[i];
             rect->getVertex(vertex);
            for(int j = 0 ; j < 4 ; j++)
            {
                SE_ASSERT(vertex[j] == vertexarray[index[i][j]]);
            }
        }
    }
#endif
    SE_BoxPrimitive* boxPrimitive = new SE_BoxPrimitive;
    for(int i = 0 ; i < 6 ; i++)
    {   
        SE_RectPrimitive* rectPrimitive = NULL;
        SE_PrimitiveID rectPrimitiveID;
        const SE_Rect3D& rect = rectArray[i];
        SE_RectPrimitive::create(rect, boxPrimitive->mRectPrimitive[i],
        boxPrimitive->mRectPrimitiveID[i]);
    }
    boxPrimitive->mScale = scale;
    outPrimitive = boxPrimitive;
    outPrimitiveID = SE_Application::getInstance()->createCommonID();
}

SE_BoxPrimitive::SE_BoxPrimitive()
{
    memset(mRectPrimitive, 0, sizeof(SE_RectPrimitive*) * 6);
}

SE_BoxPrimitive::~SE_BoxPrimitive()
{
    for(int i = 0 ; i < 6 ; i++)
    {
        SE_Application::getInstance()->getResourceManager()->removePrimitive(mRectPrimitiveID[i]);
    }
}

void SE_BoxPrimitive::createMesh(SE_Mesh**& outMesh, int& outMeshNum)
{
    outMesh = new SE_Mesh*[6];
    outMeshNum = 6;
    memset(outMesh, 0, sizeof(SE_Mesh*) * 6);
    for(int i = 0 ; i < 6 ; i++)
    {
        if(mRectPrimitive[i])
        {
            SE_Mesh** primitive = NULL;
            int primitiveNum = 0;
            mRectPrimitive[i]->createMesh(primitive, primitiveNum);
            if(primitive)
                outMesh[i] = primitive[0];
        }
    }
}

SE_BoxPrimitive* SE_BoxPrimitive::clone()
{
    return NULL;
}
//////////////////////////////////////////////////////////////////////////////////////
SE_UserObject::SE_UserObject()
{
    mGeometryData = NULL;
    mTexCoordData = NULL;
    mGeometry = NULL;
    mMaterialData = NULL;
    mSampleMin = 0;
    mSampleMag = 0;
    mWrapS = 0;
    mWrapT = 0;
    mVertexArray = NULL;
    mVertexIndexArray = NULL;
    mFacetIndexArray = NULL;
    mTextureCoorArray = NULL;
    mTextureCoorIndexArray = NULL;
    mVertexNum = 0;
    mVertexIndexNum = 0;
    mFacetIndexNum = 0;
    mTextureCoorNum = 0;
    mTextureCoorIndexNum = 0;
    mNeedBlending = false;
    mNeedDepthTest = true;
    mLastLayerInWorld = false;
    mLayerIndex = 0;
    mAlpha = 1.0;
    mProgramDataID = DEFAULT_SHADER;        
    mRendererID = DEFAULT_RENDERER;
    mVisible = true;
    mImageData = NULL;
    mImageDataIDArray = NULL;
}

SE_UserObject::~SE_UserObject()
{
    LOGI("release user object name: %s\n", mObjectName);
    mGeometryData->dec();
    mTexCoordData->dec();
    SE_Spatial* s = SE_Application::getInstance()->getSceneManager()->removeSpatial(mGeometry->getSpatialID());
	SE_ASSERT(s == mGeometry);
    delete mGeometry;
    //SE_Application::getInstance()->getSceneManager()->updateSpatialIDMap();

    if(mGeometryData->getNum() == 0)
        delete mGeometryData;
    if(mTexCoordData->getNum() == 0)
        delete mTexCoordData;
    if(mMaterialData)
        delete mMaterialData;
    if(mImageData && mImgOwn == OWN)
        delete mImageData;
    if(mImageDataIDArray && mImgOwn == OWN) {
        SE_ResourceManager *resourceManager = SE_Application::getInstance()->getResourceManager();
        resourceManager->removeImageData(mImageDataIDArray[0]);
    }
}

SE_Spatial* SE_UserObject::create(SE_Scene* scene, SE_Spatial* parent)
{
    resizeImage();
    SE_GeometryData* geomData = new SE_GeometryData;
    geomData->setVertexArray(mVertexArray, mVertexNum);
    geomData->setFaceArray(mVertexIndexArray, mVertexIndexNum);
    mGeometryData = new SE_Wrapper<SE_GeometryData>(geomData, SE_Wrapper<SE_GeometryData>::NOT_ARRAY);

    SE_TextureCoordData* texCoordData = new SE_TextureCoordData;
    texCoordData->setTexVertexArray(mTextureCoorArray, mTextureCoorNum);
    texCoordData->setTexFaceArray(mTextureCoorIndexArray, mTextureCoorIndexNum);
    mTexCoordData = new SE_Wrapper<SE_TextureCoordData>(texCoordData, SE_Wrapper<SE_TextureCoordData>::NOT_ARRAY);

    SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    SE_SpatialID spatialID = SE_ID::createSpatialID();
    mGeometry = new SE_NewGeometry(spatialID);
	scene->addSpatial(parent, mGeometry);
    resourceManager->setPrimitive(spatialID, this);
    SE_Mesh** meshArray = NULL;
    int meshNum = 0;
    createMesh(meshArray, meshNum);

    for(int i = 0 ; i < meshNum ; i++)
    {
        SE_Mesh* mesh = meshArray[i];
        SE_MeshSimObject* simObj = new SE_MeshSimObject(mesh, OWN);
        simObj->setName(mObjectName);
        SE_SimObjectID simID = SE_ID::createSimObjectID();
        simObj->setID(simID);
        mGeometry->attachSimObject(simObj);
        SE_Application::getInstance()->getSimObjectManager()->set(simID, simObj);
    }
    mGeometry->setMovable(false);
    mGeometry->setRemovable(true);
    mGeometry->setCollisionable(false);
    mGeometry->updateWorldTransform();
    mGeometry->setBVType(1);
    mGeometry->updateBoundingVolume();
    mGeometry->setLocalTranslate(mLocalTranslate);
    mGeometry->setLocalRotate(mLocalRotate);
    mGeometry->setLocalScale(mLocalScale);
    mGeometry->updateWorldTransform();
    mGeometry->updateBoundingVolume();
    if(meshArray)
    {
        delete[] meshArray;
    }

    SE_BlendState* blendState = (SE_BlendState *)mGeometry->getRenderState(SE_Spatial::BLENDSTATE);
    if(!blendState)
    {
        blendState = new SE_BlendState();
        mGeometry->setRenderState(SE_Spatial::BLENDSTATE, blendState);     
    }
    if(mNeedBlending)
    {
        blendState->setBlendProperty(SE_BlendState::BLEND_ENABLE);
    }
    else
    {
        blendState->setBlendProperty(SE_BlendState::BLEND_DISABLE);
    }

    SE_DepthTestState* depthTestState = (SE_DepthTestState *)mGeometry->getRenderState(SE_Spatial::DEPTHTESTSTATE);
    if(!depthTestState)
    {
        depthTestState = new SE_DepthTestState();
        mGeometry->setRenderState(SE_Spatial::DEPTHTESTSTATE, depthTestState);
    }
    if(mNeedDepthTest)
    {
        depthTestState->setDepthTestProperty(SE_DepthTestState::DEPTHTEST_ENABLE);
    }
    else
    {
        depthTestState->setDepthTestProperty(SE_DepthTestState::DEPTHTEST_DISABLE);
    }
    mGeometry->updateRenderState();
    SE_CommonNode * node = (SE_CommonNode *)parent;
    if(mLastLayerInWorld)
    {
        int maxlayer = node->getLastestLayerInWorld();
        mGeometry->setLocalLayer(maxlayer + mLayerIndex);  
    }
    else
    {
        mGeometry->setLocalLayer(mLayerIndex); 
    }
    mGeometry->updateWorldLayer();
    mGeometry->setAlpha(mAlpha);
    mGeometry->setVisible(mVisible);
    return mGeometry;
}

void SE_UserObject::createMesh(SE_Mesh**& outMesh, int& outMeshNum)
{
    std::auto_ptr<SE_Mesh> mesh(new SE_Mesh(1, 1));
    if(!mesh.get())
    {
    outMesh = NULL;
    outMeshNum = 0;
    return;
    }
    std::auto_ptr<SE_Surface> surface(new SE_Surface);
    std::auto_ptr<SE_Texture> texture(new SE_Texture);
    mesh->setGeometryData(mGeometryData->getPtr());
    surface->setGeometryData(mGeometryData->getPtr());
    surface->setMaterialData(mMaterialData);
    surface->setTexture(texture.get());
    surface->setFacets(mFacetIndexArray, mFacetIndexNum);
    surface->setColor(mColor);
    surface->setProgramDataID(mProgramDataID);
    surface->setRendererID(mRendererID);

    SE_TextureUnit* texUnit = new SE_TextureUnit();
	texUnit->setSampleMin(mSampleMin);
    texUnit->setSampleMag(mSampleMag);
    texUnit->setWrapS(mWrapS);
    texUnit->setWrapT(mWrapT);
    texUnit->setImageDataNum(1);
    if(mImageData)
    {
        texUnit->setImageData(0, mImageData);
    } else if (mImageDataIDArray) {
        texUnit->setImageDataID(mImageDataIDArray, 1);
    }
    texUnit->setTextureCoordData(mTexCoordData->getPtr());
    texture->setTextureUnit(0, texUnit);
    mesh->setSurface(0, surface.get());
    mesh->setTexture(0, texture.get());
    surface.release();
    texture.release();
    SE_Mesh* ret = mesh.release();
    outMesh = new SE_Mesh*[1];
    outMesh[0] = ret;
    outMeshNum = 1;
    return;
}
void SE_UserObject::resizeImage()
{
    SE_ImageData* imageData = NULL;
    SE_ResourceManager *resourceManager = SE_Application::getInstance()->getResourceManager();
    if(mImageData) {
        imageData = mImageData;
    }  else if (mImageDataIDArray) {
        imageData = resourceManager->getImageData(mImageDataIDArray[0]);
    }
    if (!imageData)
        return;
    SE_ImageCodec::resizeImageData(imageData);
    if (imageData->hasResized()) {
        int width = imageData->getPreWidth();
        int height = imageData->getPreHeight();
        int power2Width = imageData->getWidth();
        int  power2Height = imageData->getHeight();
        int starty = (power2Height - height) >> 1;
        int startx = (power2Width - width) >> 1;
        for (int i=0; i<mTextureCoorNum; i++) {
            mTextureCoorArray[i].x = (startx + mTextureCoorArray[i].x * width) / power2Width;
            mTextureCoorArray[i].y = (starty + mTextureCoorArray[i].y * height) / power2Height;
        }
    }
}




