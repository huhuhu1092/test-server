#include "SE_Primitive.h"
#include "SE_GeometryData.h"
#include "SE_TextureCoordData.h"
#include "SE_Application.h"
#include "SE_Utils.h"
#include "SE_ResourceManager.h"
#include "SE_Mesh.h"
#include <memory>
#include <math.h>
//SE_PrimitiveID SE_Primitive::normalizeRectPrimitiveID = SE_CommonID(0, 0, 0, 0);
//SE_PrimitiveID SE_Primitive::normalizeCubePrimitiveID = SE_CommonID(0, 0, 0, 1);
SE_Primitive::SE_Primitive()
{
    mSampleMin = 0;
    mSampleMag = 0;
    mWrapS = 0;
    mWrapT = 0;
}
SE_RectPrimitive::SE_RectPrimitive()
{
    memset(mImageDataArray, 0, SE_TEXUNIT_NUM * sizeof(SE_ImageData*));
    mGeometryData = NULL;
    mTexCoordData = NULL;
    mMaterialData = NULL;
    mU = 1.0f;
    mV = 1.0f;    
}
SE_RectPrimitive::SE_RectPrimitive(const SE_Rect3D& rect) : mRect3D(rect)
{
    memset(mImageDataArray, 0, SE_TEXUNIT_NUM * sizeof(SE_ImageData*));
    mGeometryData = NULL;
    mTexCoordData = NULL;
    mMaterialData = NULL;
    mU = 1.0f;
    mV = 1.0f; 
    //mAdjustedStartX = 0;
    //mAdjustedStartY = 0;
}
SE_RectPrimitive::~SE_RectPrimitive()
{
    if(mGeometryData)
        delete mGeometryData;
    if(mTexCoordData)
        delete mTexCoordData;
    /*
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
    */
}
bool SE_RectPrimitive::createGeometryData(SE_RectPrimitive* rectPrimitive)
{
	SE_Vector3f* vertex = new SE_Vector3f[4];
    if(!vertex)
        return false;
    rectPrimitive->mRect3D.getVertex(vertex);
    SE_Vector3i* faces = new SE_Vector3i[2];
    if(!faces)
    {
        delete[] vertex;
        return false;
    }
    rectPrimitive->mRect3D.getFaces(faces);
    SE_GeometryData* geomData = new SE_GeometryData;
    if(!geomData)
    {
        delete[] vertex;
        delete[] faces;
        return false;
    }
    geomData->setVertexArray(vertex, 4);
	geomData->setFaceArray(faces, 2);
	rectPrimitive->mGeometryData = geomData;
    return true;
}
bool SE_RectPrimitive::createTexCoordData(SE_RectPrimitive* rectPrimitive, const SE_Vector2f& v0, const SE_Vector2f& v1, const SE_Vector2f& v2, const SE_Vector2f& v3, const SE_Vector2f& v4, const SE_Vector2f& v5, const SE_Vector2f& v6, const SE_Vector2f& v7, const SE_Vector2f& v8, int uRectSize, int vRectSize, float uReminder, float vReminder)
{
    SE_Vector2f* texVertex = NULL;
    int texVertexNum = 0;
	int faceNum = rectPrimitive->mGeometryData->getFaceNum();
    SE_Vector3i* texFaces = new SE_Vector3i[faceNum];
    if(!texFaces)
    {
        delete[] texVertex;
		return false;
    }
    if(rectPrimitive->mU == 1.0f && rectPrimitive->mV == 1.0f)
    {
        texVertex = new SE_Vector2f[4];
        texVertexNum = 4;
        if(!texVertex)
        {
            delete[] texFaces;
		    return false;
        }
        texVertex[0] = v0;
        texVertex[1] = v1;
        texVertex[2] = v2;
        texVertex[3] = v3;
        SE_ASSERT(faceNum == 2);
	    SE_Vector3i* faces = rectPrimitive->mGeometryData->getFaceArray();
	    memcpy(texFaces, faces, faceNum * sizeof(SE_Vector3i));
    }
    else
    {
		texVertexNum = 9;
        texVertex = new SE_Vector2f[9];
        if(!texVertex)
        {
            delete[] texFaces;
            return false;
        }
        texVertex[0] = v0;
        texVertex[1] = v1;
        texVertex[2] = v2;
        texVertex[3] = v3;
        texVertex[4] = v4;
		texVertex[5] = v5;
		texVertex[6] = v6;
		texVertex[7] = v7;
		texVertex[8] = v8;
		
        int k = 0;
        for(int i = 0 ; i < vRectSize - 1; i++)
        {
            for(int j = 0 ; j < uRectSize - 1 ; j++)
            {
                texFaces[k].x = 3;
                texFaces[k].y = 0;
                texFaces[k].z = 2;
                k++;
                texFaces[k].x = 0;
                texFaces[k].y = 1;
                texFaces[k].z = 2;
                k++;
            }
            if(uReminder != 0.0f)
            {
                texFaces[k].x = 3;
                texFaces[k].y = 0;
                texFaces[k].z = 5;
                k++;
                texFaces[k].x = 0;
                texFaces[k].y = 4;
                texFaces[k].z = 5;
                k++;
            }
            else
            {
                texFaces[k].x = 3;
                texFaces[k].y = 0;
                texFaces[k].z = 2;
                k++;
                texFaces[k].x = 0;
                texFaces[k].y = 1;
                texFaces[k].z = 2;
                k++;
            }
        }  
        if(vReminder != 0.0f)
        {
            for(int j = 0 ; j < uRectSize - 1 ; j++)
            {
                texFaces[k].x = 3;
                texFaces[k].y = 6;
                texFaces[k].z = 2;
                k++;
                texFaces[k].x = 6;
                texFaces[k].y = 7;
                texFaces[k].z = 2;
                k++;
            }
            if(uReminder != 0.0f)
            {
                texFaces[k].x = 3;
                texFaces[k].y = 6;
                texFaces[k].z = 5;
                k++;
                texFaces[k].x = 6;
                texFaces[k].y = 8;
                texFaces[k].z = 5;
                k++;
            }
            else
            {
                texFaces[k].x = 3;
                texFaces[k].y = 6;
                texFaces[k].z = 2;
                k++;
                texFaces[k].x = 6;
                texFaces[k].y = 7;
                texFaces[k].z = 2;
                k++;
            }
        }      
        else
        {
            for(int j = 0 ; j < uRectSize - 1 ; j++)
            {
                texFaces[k].x = 3;
                texFaces[k].y = 0;
                texFaces[k].z = 2;
                k++;
                texFaces[k].x = 0;
                texFaces[k].y = 1;
                texFaces[k].z = 2;
                k++;
            }
            if(uReminder != 0.0f)
            {
                texFaces[k].x = 3;
                texFaces[k].y = 0;
                texFaces[k].z = 5;
                k++;
                texFaces[k].x = 0;
                texFaces[k].y = 4;
                texFaces[k].z = 5;
                k++;
            }
            else
            {
                texFaces[k].x = 3;
                texFaces[k].y = 0;
                texFaces[k].z = 2;
                k++;
                texFaces[k].x = 0;
                texFaces[k].y = 1;
                texFaces[k].z = 2;
                k++;
            }
        }
        SE_ASSERT(k == faceNum);

    }
    SE_TextureCoordData* texCoordData = new SE_TextureCoordData;
    if(!texCoordData)
    {
        delete[] texVertex;
        delete[] texFaces;
        return false;
    }

    texCoordData->setTexVertexArray(texVertex, texVertexNum);
    texCoordData->setTexFaceArray(texFaces, faceNum);
    rectPrimitive->mTexCoordData = texCoordData;
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    rectPrimitive->mTexCoordDataID = SE_ID::createTextureCoordDataID();
    resourceManager->setTextureCoordData(rectPrimitive->mTexCoordDataID, rectPrimitive->mTexCoordData);
   
    return true;
}
void SE_RectPrimitive::getUVProperty(float uv, int& size, float& floor, float& reminder)
{
    floor = floorf(uv);
    reminder = uv - floor;
    if(reminder == 0)
        size = (int)floor;
    else 
        size = (int)floor + 1;
    
}
void SE_RectPrimitive::create(const SE_Rect3D& rect, float u, float v, SE_Primitive*& outPrimitive, SE_PrimitiveID& outPrimitiveID)
{
    SE_ASSERT(u >= 1.0 && v >= 1.0);
    if((v < 1.0f || v < 1.0f) || (v == 1.0f && u == 1.0f))
        return create(rect, outPrimitive, outPrimitiveID);
    SE_RectPrimitive* rectPrimitive = new SE_RectPrimitive(rect);
    if(!rectPrimitive)
    {
        outPrimitive = NULL;
        outPrimitiveID = SE_PrimitiveID::INVALID;
        return;
    }
    rectPrimitive->mU = u;
    rectPrimitive->mV = v;

    float uStep = 0;
    float vStep = 0;
    float uReminderStep = 0;
    float vReminderStep = 0;
    int uRectSize = 0;
    int vRectSize = 0;
    float uReminder = 0;
    float vReminder = 0;
    float uFloor = 0;
    float vFloor = 0;
    float rectExtent[2];
    getUVProperty(u, uRectSize, uFloor, uReminder);
    getUVProperty(v, vRectSize, vFloor, vReminder);
    SE_Vector3f rectCenter;
    SE_Vector3f rectXAxis, rectYAxis;
    rect.getExtent(rectExtent);
    rectCenter = rect.getCenter();
    rectXAxis = rect.getXAxis();
    rectYAxis = rect.getYAxis();
    
    uStep = (2 * rectExtent[0]) / u;
    uReminderStep = uStep * uReminder;
    vStep = (2 * rectExtent[1]) / v;
    vReminderStep = vStep * vReminder;

    SE_Vector3f startPoint = rectCenter - rectXAxis * rectExtent[0] + rectYAxis * rectExtent[1];
    int rowNum = vRectSize + 1;
    int colNum = uRectSize + 1;
    int vertexNum = rowNum * colNum;
    SE_Vector3f* vertex = new SE_Vector3f[vertexNum];
    for(int i = 0 ; i < vRectSize; i++)
    {
        int j;
        for(j = 0 ; j < uRectSize; j++)
        {
             vertex[i * colNum  + j] = startPoint + rectXAxis * (uStep * j) - rectYAxis * (vStep * i);
        }
        if(uReminderStep != 0.0f )
        {
            vertex[i * colNum + j] = startPoint + rectXAxis * (uStep * (uRectSize - 1) + uReminderStep) - rectYAxis * (vStep * i);
        }
        else
        {
            vertex[i * colNum + j] = startPoint + rectXAxis * (uStep * uRectSize) - rectYAxis * (vStep * i);
        }
    }
    if(vReminderStep != 0.0f)
    {
        int j;
        for(j = 0 ; j < uRectSize ; j++)
        {
            vertex[vRectSize * colNum + j] = startPoint + rectXAxis * (uStep * j) - rectYAxis * (vStep * (vRectSize - 1) + vReminderStep);
        }
        if(uReminder != 0.0f)
        {
            vertex[vRectSize * colNum + j] = startPoint + rectXAxis * (uStep * (uRectSize - 1) + uReminderStep) - rectYAxis * (vStep * (vRectSize - 1) + vReminderStep);
        }
        else
        {
            vertex[vRectSize * colNum + j] = startPoint + rectXAxis * (uStep * uRectSize) - rectYAxis * (vStep * (vRectSize - 1) + vReminderStep);
        }

    }
    else
    {
        int j;
        for(j = 0 ; j < uRectSize ; j++)
        {
            vertex[vRectSize * colNum + j] = startPoint + rectXAxis * (uStep * j) - rectYAxis * (vStep * vRectSize);
        }
        if(uReminder != 0.0f)
        {
            vertex[vRectSize * colNum + j] = startPoint + rectXAxis * (uStep * (uRectSize - 1) + uReminderStep) - rectYAxis * (vStep * vRectSize);

        }
        else
        {
             vertex[vRectSize * colNum + j] = startPoint + rectXAxis * (uStep * uRectSize) - rectYAxis * (vStep * vRectSize);

        }
    }
    int faceSize = uRectSize * vRectSize * 2;
    SE_Vector3i* face = new SE_Vector3i[faceSize];
    int k = 0;
    for(int i = 0 ; i < vRectSize ; i++)
    {
        for(int j = 0 ; j < uRectSize ; j++)
        {
            face[k].x = i * colNum + j;
            face[k].y = (i + 1) * colNum + j;
            face[k].z = i * colNum + j + 1;
            k++;
            face[k].x = (i + 1) * colNum + j;
            face[k].y = (i + 1) * colNum + j + 1;
            face[k].z = i * colNum + j + 1;
            k++;
        }
    }
    SE_GeometryData* geomData = new SE_GeometryData;
    geomData->setVertexArray(vertex, vertexNum);
    geomData->setFaceArray(face, faceSize);
    rectPrimitive->mGeometryData = geomData;
    outPrimitive = rectPrimitive;
    outPrimitiveID = SE_ID::createPrimitiveID();
    SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    resourceManager->setPrimitive(outPrimitiveID, outPrimitive);
    rectPrimitive->mGeometryDataID = SE_ID::createGeometryDataID();
    resourceManager->setGeometryData(rectPrimitive->mGeometryDataID, rectPrimitive->mGeometryData);
    //create texture coordinate
    //
}
void SE_RectPrimitive::create(const SE_Rect3D& rect, SE_Primitive*& outPrimitive, SE_PrimitiveID& outPrimitiveID)
{
    SE_RectPrimitive* rectPrimitive = new SE_RectPrimitive(rect);
    if(!rectPrimitive)
    {
        outPrimitive = NULL;
        outPrimitiveID = SE_PrimitiveID::INVALID;
        return;
    }
    bool ret = createGeometryData(rectPrimitive);
    if(!ret)
    {
        delete rectPrimitive;
        outPrimitive = NULL;
        outPrimitiveID = SE_PrimitiveID::INVALID;
		return;
    }
    outPrimitive = rectPrimitive;
    outPrimitiveID = SE_ID::createPrimitiveID();
    SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    resourceManager->setPrimitive(outPrimitiveID, outPrimitive);
    rectPrimitive->mGeometryDataID = SE_ID::createGeometryDataID();
    resourceManager->setGeometryData(rectPrimitive->mGeometryDataID, rectPrimitive->mGeometryData);
}
SE_RectPrimitive* SE_RectPrimitive::clone()
{
     SE_RectPrimitive* primitive = new SE_RectPrimitive(mRect3D);
     if(!primitive)
         return NULL;
     if(mGeometryData)
     {
         primitive->mGeometryData = mGeometryData;
         primitive->mGeometryDataID = mGeometryDataID;
     }
     if(mTexCoordData)
     {
         primitive->mTexCoordData = mTexCoordData;
         primitive->mTexCoordDataID = mTexCoordDataID;
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
         }
     }
	 return primitive;
}
void SE_RectPrimitive::setImageData(int index , SE_ImageData* imageData, SE_TEXUNIT_TYPE texUnitType, SE_ImageDataPortion imageDataPortion)
{
	if(texUnitType >= SE_TEXUNIT_NUM || texUnitType < SE_TEXTURE0)
		return;
    if(!imageData)
    {
        return;
    }
    SE_Vector2f v0, v1, v2, v3, v4, v5, v6, v7, v8;    
    float startx = 0, starty = 0;
    int portionx = 0;
    int portiony = 0;
    int portionw = 0;
    int portionh = 0;
    if(!imageData->isSizePower2())
    {
        imageData->getDataPower2();
    }
    if(imageDataPortion.isValid())
    {
        portionx = imageDataPortion.getX();
        portiony = imageDataPortion.getY();
        portionw = imageDataPortion.getWidth();
        portionh = imageDataPortion.getHeight();
    }
    else
    {
        portionw = imageData->getWidth();
        portionh = imageData->getHeight();
    }
    startx = imageData->getRealStartX();
    starty = imageData->getRealStartY();
    float power2Width = imageData->getWidthPower2();
    float power2Height = imageData->getHeightPower2();

	if(imageData->isFliped())
	{
        v0 = SE_Vector2f((startx + portionx) / power2Width, (starty + portiony) / power2Height);
        v1 = SE_Vector2f((startx + portionx + portionw) / power2Width, (starty + portiony) / power2Height);
        v2 = SE_Vector2f((startx + portionx + portionw) / power2Width, (starty + portiony + portionh) / power2Height);
        v3 = SE_Vector2f((startx + portionx) / power2Width, (starty + portiony + portionh) / power2Height);       
	}
	else
	{
		/*
        v0 = SE_Vector2f((startx + portionx) / power2Width, 1 - (starty + portiony) / power2Height);
        v1 = SE_Vector2f((startx + portionx + portionw) / power2Width, 1 - (starty + portiony) / power2Height);
        v2 = SE_Vector2f((startx + portionx + portionw) / power2Width, 1 - (starty + portiony + portionh) / power2Height);
        v3 = SE_Vector2f((startx + portionx) / power2Width, 1 - (starty + portiony + portionh) / power2Height); 
		*/
		v0 = SE_Vector2f((startx + portionx) / power2Width, (starty + portiony + portionh) / power2Height);
		v1 = SE_Vector2f((startx + portionx + portionw) / power2Width, (starty + portiony + portionh) / power2Height);
		v2 = SE_Vector2f((startx + portionx + portionw) / power2Width, (starty + portiony) / power2Height);
		v3 = SE_Vector2f((startx + portionx) / power2Width, (starty + portiony) / power2Height);
	}
	float uFloor = 0, vFloor = 0;
    float uReminder = 0, vReminder = 0;
    int uRectSize = 0, vRectSize = 0;	
	if(mU > 1.0f || mV > 1.0f)
	{

        getUVProperty(mU, uRectSize, uFloor, uReminder);
        getUVProperty(mV, vRectSize, vFloor, vReminder);
		float stepx = v1.x - v0.x;
		float stepy = SE_Fabs(v0.y - v3.y);
		v4 = SE_Vector2f(v0.x + stepx * uReminder, v0.y);
	    v5 = SE_Vector2f(v0.x + stepx * uReminder, v3.y);
		if(imageData->isFliped())
		{
		    v6 = SE_Vector2f(v0.x, v0.y + stepy * vReminder);
		    v7 = SE_Vector2f(v1.x, v0.y + stepy * vReminder);
			v8 = SE_Vector2f(v0.x + stepx * uReminder, v0.y + stepy * vReminder);
		}
		else
		{
			v6 = SE_Vector2f(v0.x, v0.y - stepy * vReminder);
			v7 = SE_Vector2f(v1.x, v0.y - stepy * vReminder);
			v8 = SE_Vector2f(v0.x + stepx * uReminder, v0.y - stepy * vReminder);
		}
	}
    bool ret = createTexCoordData(this, v0, v1, v2, v3, v4, v5, v6, v7, v8, uRectSize, vRectSize, uReminder, vReminder);
    if(!ret)
        return;
    mImageDataArray[texUnitType] = imageData; 
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
	mesh->setGeometryData(mGeometryData);
    surface->setGeometryData(mGeometryData);
    surface->setMaterialData(mMaterialData);
    surface->setTexture(texture.get());
	int facetNum = mGeometryData->getFaceNum();
    int* facet = new int[facetNum];
	for(int i = 0 ; i < facetNum ; i++)
	{
        facet[i] = i;
	}
    surface->setFacets(facet, facetNum);
    surface->setColor(mColor);
    surface->setProgramDataID(mProgramDataID);
    surface->setSampleMin(mSampleMin);
    surface->setSampleMag(mSampleMag);
    surface->setWrapS(mWrapS);
    surface->setWrapT(mWrapT);
    for(int i = 0 ; i < SE_TEXUNIT_NUM ; i++)
    {
	    //surface->setTexCoordIndex(i, 0);// all texunit use tex0's texcoord
        SE_TextureUnit* texUnit = new SE_TextureUnit();
        texUnit->setImageDataNum(1);
		if(mImageDataArray[i])
		    texUnit->setImageData(0, mImageDataArray[i]);
		if(i == 0)
            texUnit->setTextureCoordData(mTexCoordData);
        texture->setTextureUnit(i, texUnit);
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
		SE_Primitive* rectPrimitive = NULL;
	    SE_PrimitiveID rectPrimitiveID;
		const SE_Rect3D& rect = rectArray[i];
	    SE_RectPrimitive::create(rect, rectPrimitive, rectPrimitiveID);
		boxPrimitive->mRectPrimitive[i] = (SE_RectPrimitive*)rectPrimitive;
		boxPrimitive->mRectPrimitiveID[i] = rectPrimitiveID;
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
///////////////////////////////////////////////////

SE_RectPatch::SE_RectPatch(SE_RECTPATCH_TYPE t) : mType(t)
{
    mSampleMin = 0;
    mSampleMag = 0;
    mWrapS = 0;
    mWrapT = 0;
    for(int i = 0 ; i < SE_TEXUNIT_NUM ; i++)
        mImageData[i] = NULL;
}
 
void SE_RectPatch::create(const SE_Rect3D& rect, SE_RECTPATCH_TYPE t, SE_Primitive*& outPrimitive, SE_PrimitiveID& outPrimitiveID)
{
	if(t != SE_PATCH_R1_C3 && t != SE_PATCH_R3_C1 && t != SE_PATCH_R3_C3)
	{
        outPrimitive = NULL;
        outPrimitiveID = SE_PrimitiveID::INVALID;
		return;
	}
    SE_RectPatch* rectPatch = new SE_RectPatch(t);
	if(!rectPatch)
    {
        outPrimitive = NULL;
        outPrimitiveID = SE_PrimitiveID::INVALID;
        return;
    }    
    float rectExtent[2];
    SE_Vector3f vertex[4];
    SE_Vector3f rectCenter;
    SE_Vector3f rectXAxis, rectYAxis;
    rect.getExtent(rectExtent);
    rect.getVertex(vertex);
    rectCenter = rect.getCenter();
    rectXAxis = rect.getXAxis();
    rectYAxis = rect.getYAxis();
    /*
    SE_Vector3f* vertexRectPatch = NULL;
    switch(t)
    {
    case SE_PATCH_R1_C3:
    case SE_PATCH_R3_C1:
        vertexRectPatch = new SE_Vector3f[8];
        break;
	case SE_PATCH_R3_C3:
        vertexRectPatch = new SE_Vector3f[16];
        break;
    default:
        break;
    }
    if(!vertexRectPatch)
    {
        delete rectPatch;
        outPrimitive = NULL;
        outPrimitiveID = SE_PrimitiveID::INVALID;
        return;
    }
    SE_Vector3f v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15;
    */
    int geometryNum = 0;
    switch(t)
    {
    case SE_PATCH_R1_C3:
    case SE_PATCH_R3_C1:
        geometryNum = 3;
    case SE_PATCH_R3_C3:
        geometryNum = 9;
    }
    SE_ASSERT(geometryNum != 0);
    rectPatch->mGeometryData.resize(geometryNum);
    for(int i = 0 ; i < geometryNum ; i++)
    {
        SE_GeometryData* geomData = new SE_GeometryData;
        SE_Vector3f* v = new SE_Vector3f[4];
        v[0] = vertex[0];//v0;
        v[1] = vertex[1];//v1;
        v[2] = vertex[2];//v6;
        v[3] = vertex[3];//v7;
        SE_Vector3i* f = new SE_Vector3i[2];
        f[0].x = 0;
        f[0].y = 1;
        f[0].z = 2;
        f[1].x = 0;
        f[1].y = 2;
        f[1].z = 3;
        geomData->setVertexArray(v, 4);
        geomData->setFaceArray(f, 2);
        rectPatch->mGeometryData[i] = geomData;
    }
    switch(t)
    {
    case SE_PATCH_R1_C3:
    case SE_PATCH_R3_C1:
        {

            /*
            // rect 1
            SE_GeometryData* geomData = new SE_GeometryData;
            SE_Vector3f* v = new SE_Vector3f[4];
            v[0] = vertex[0];//v0;
            v[1] = vertex[1];//v1;
            v[2] = vertex[2];//v6;
            v[3] = vertex[3];//v7;
            SE_Vector3i* f = new SE_Vector3i[2];
            f[0].x = 0;
            f[0].y = 1;
            f[0].z = 2;
            f[1].x = 0;
            f[1].y = 2;
            f[1].z = 3;
            geomData->setVertexArray(v, 4);
            geomData->setFaceArray(f, 2);
            rectPatch->mGeometryData[0] = geomData;
            // rect 2
            geomData = new SE_GeometryData;
            v = new SE_Vector3f[4];
            v[0] = vertex[0];//v1;
            v[1] = vertex[1];//v2;
            v[2] = vertex[2];//v5;
            v[3] = vertex[3];//v6;
            f = new SE_Vector3i[2];
            f[0].x = 0;
            f[0].y = 1;
            f[0].z = 2;
            f[1].x = 0;
            f[1].y = 2;
            f[1].z = 3;
            geomData->setVertexArray(v, 4);
            geomData->setFaceArray(f, 2);
            rectPatch->mGeometryData[1] = geomData; 
            //rect 3
            geomData = new SE_GeometryData;
            v = new SE_Vector3f[4];
            v[0] = vertex[0];//v2;
            v[1] = vertex[1];//v3;
            v[2] = vertex[2];//v4;
            v[3] = vertex[3];//v5;
            f = new SE_Vector3i[2];
            f[0].x = 0;
            f[0].y = 1;
            f[0].z = 2;
            f[1].x = 0;
            f[1].y = 2;
            f[1].z = 3;
            geomData->setVertexArray(v, 4);
            geomData->setFaceArray(f, 2);
            rectPatch->mGeometryData[2] = geomData;
            */
        }
        break;
    /*
    case SE_PATCH_R3_C1:
        {
            rectPatch->mGeometryData.resize(3);
            //rect 1
            SE_GeometryData* geomData = new SE_GeometryData;
            SE_Vector3f* v = new SE_Vector3f[4];
            v[0] = vertex[0];//v0;
            v[1] = vertex[1];//v1;
            v[2] = vertex[2];//v2;
            v[3] = vertex[3];//v7;
            SE_Vector3i* f = new SE_Vector3i[2];
            f[0].x = 0;
            f[0].y = 1;
            f[0].z = 2;
            f[1].x = 0;
            f[1].y = 2;
            f[1].z = 3;
            geomData->setVertexArray(v, 4);
            geomData->setFaceArray(f, 2);
            rectPatch->mGeometryData[0] = geomData;
            //rect 2
            geomData = new SE_GeometryData;
            v = new SE_Vector3f[4];
            v[0] = vertex[0];//v7;
            v[1] = vertex[1];//v2;
            v[2] = vertex[2];//v3;
            v[3] = vertex[3];//v6;
            f = new SE_Vector3i[2];
            f[0].x = 0;
            f[0].y = 1;
            f[0].z = 2;
            f[1].x = 0;
            f[1].y = 2;
            f[1].z = 3;
            geomData->setVertexArray(v, 4);
            geomData->setFaceArray(f, 2);
            rectPatch->mGeometryData[1] = geomData;

            //rect 3
            geomData = new SE_GeometryData;
            v = new SE_Vector3f[4];
            v[0] = vertex[0];//v6;
            v[1] = vertex[1];//v3;
            v[2] = vertex[2];//v4;
            v[3] = vertex[3];//v5;
            f = new SE_Vector3i[2];
            f[0].x = 0;
            f[0].y = 1;
            f[0].z = 2;
            f[1].x = 0;
            f[1].y = 2;
            f[1].z = 3;
            geomData->setVertexArray(v, 4);
            geomData->setFaceArray(f, 2);
            rectPatch->mGeometryData[2] = geomData;
        }
        break;
    */
    case SE_PATCH_R3_C3:
        {
            //rectPatch->mGeometryData.resize(9);
            /*
            float stepx = (rectExtent[0] * 2) / 3;
            float stepy = (rectExtent[1] * 2) / 3;
            v0 =  rectCenter - rectXAxis * rectExtent[0] - rectYAxis * rectExtent[1];
            v1 = v0 + rectXAxis * stepx;
            v2 = v0 + rectXAxis * (stepx * 2);
            v3 =  rectCenter + rectXAxis * rectExtent[0] - rectYAxis * rectExtent[1];
            v4 = v3 + rectYAxis * stepy;
            v5 = v3 + rectYAxis * (stepy * 2);
            v6 =  rectCenter + rectXAxis * rectExtent[0] + rectYAxis * rectExtent[1];
            v7 = v6 - rectXAxis * stepx;
            v8 = v6 - rectXAxis * (stepx * 2);
            v9 = rectCenter - rectXAxis * rectExtent[0] + rectYAxis * rectExtent[1];
            v10 = v9 - rectYAxis * stepy;
            v11 = v9 - rectYAxis * stepy;
            v12 = v11 + rectXAxis * stepx;
            v13 = v11 + rectXAxis * (stepx * 2);
            v14 = v13 + rectYAxis * stepy;
            v15 = v14 - rectXAxis * stepx;
            */
            /*
            //rect 1
            SE_GeometryData* geomData = new SE_GeometryData;
            SE_Vector3f* v = new SE_Vector3f[4];
            v[0] = vertex[0];//v0;
            v[1] = vertex[1];//v1;
            v[2] = vertex[2];//v12;
            v[3] = vertex[3];//v11;
            SE_Vector3i* f = new SE_Vector3i[2];
            f[0].x = 0;
            f[0].y = 1;
            f[0].z = 2;
            f[1].x = 0;
            f[1].y = 2;
            f[1].z = 3;
            geomData->setVertexArray(v, 4);
            geomData->setFaceArray(f, 2);
            rectPatch->mGeometryData[0] = geomData;

            //rect 2
            geomData = new SE_GeometryData;
            v = new SE_Vector3f[4];
            v[0] = vertex[0];//v1;
            v[1] = vertex[1];//v2;
            v[2] = vertex[2];//v13;
            v[3] = vertex[3];//v12;
            f = new SE_Vector3i[2];
            f[0].x = 0;
            f[0].y = 1;
            f[0].z = 2;
            f[1].x = 0;
            f[1].y = 2;
            f[1].z = 3;
            geomData->setVertexArray(v, 4);
            geomData->setFaceArray(f, 2);
            rectPatch->mGeometryData[1] = geomData;

            //rect 3
            geomData = new SE_GeometryData;
            v = new SE_Vector3f[4];
            v[0] = vertex[0];//v2;
            v[1] = vertex[1];//v3;
            v[2] = vertex[2];//v4;
            v[3] = vertex[3];//v13;
            f = new SE_Vector3i[2];
            f[0].x = 0;
            f[0].y = 1;
            f[0].z = 2;
            f[1].x = 0;
            f[1].y = 2;
            f[1].z = 3;
            geomData->setVertexArray(v, 4);
            geomData->setFaceArray(f, 2);
            rectPatch->mGeometryData[2] = geomData;

            //rect 4
            geomData = new SE_GeometryData;
            v = new SE_Vector3f[4];
            v[0] = vertex[0];//v13;
            v[1] = vertex[1];//v4;
            v[2] = vertex[2];//v5;
            v[3] = vertex[3];//v14;
            f = new SE_Vector3i[2];
            f[0].x = 0;
            f[0].y = 1;
            f[0].z = 2;
            f[1].x = 0;
            f[1].y = 2;
            f[1].z = 3;
            geomData->setVertexArray(v, 4);
            geomData->setFaceArray(f, 2);
            rectPatch->mGeometryData[3] = geomData;

            //rect 5
            geomData = new SE_GeometryData;
            v = new SE_Vector3f[4];
            v[0] = vertex[0];//v14;
            v[1] = vertex[1];//v5;
            v[2] = vertex[2];//v6;
            v[3] = vertex[3];//v7;
            f = new SE_Vector3i[2];
            f[0].x = 0;
            f[0].y = 1;
            f[0].z = 2;
            f[1].x = 0;
            f[1].y = 2;
            f[1].z = 3;
            geomData->setVertexArray(v, 4);
            geomData->setFaceArray(f, 2);
            rectPatch->mGeometryData[4] = geomData;

            //rect6
            geomData = new SE_GeometryData;
            v = new SE_Vector3f[4];
            v[0] = vertex[0];//v15;
            v[1] = vertex[1];//v14;
            v[2] = vertex[2];//v7;
            v[3] = vertex[3];//v8;
            f = new SE_Vector3i[2];
            f[0].x = 0;
            f[0].y = 1;
            f[0].z = 2;
            f[1].x = 0;
            f[1].y = 2;
            f[1].z = 3;
            geomData->setVertexArray(v, 4);
            geomData->setFaceArray(f, 2);
            rectPatch->mGeometryData[5] = geomData;

            //rect 7
            geomData = new SE_GeometryData;
            v = new SE_Vector3f[4];
            v[0] = vertex[0];//v10;
            v[1] = vertex[1];//v15;
            v[2] = vertex[2];//v8;
            v[3] = vertex[3];//v9;
            f = new SE_Vector3i[2];
            f[0].x = 0;
            f[0].y = 1;
            f[0].z = 2;
            f[1].x = 0;
            f[1].y = 2;
            f[1].z = 3;
            geomData->setVertexArray(v, 4);
            geomData->setFaceArray(f, 2);
            rectPatch->mGeometryData[6] = geomData;

            //rect 8
            geomData = new SE_GeometryData;
            v = new SE_Vector3f[4];
            v[0] = vertex[0];//v11;
            v[1] = vertex[1];//v12;
            v[2] = vertex[2];//v15;
            v[3] = vertex[3];//v10;
            f = new SE_Vector3i[2];
            f[0].x = 0;
            f[0].y = 1;
            f[0].z = 2;
            f[1].x = 0;
            f[1].y = 2;
            f[1].z = 3;
            geomData->setVertexArray(v, 4);
            geomData->setFaceArray(f, 2);
            rectPatch->mGeometryData[7] = geomData;

            //rect 9
            geomData = new SE_GeometryData;
            v = new SE_Vector3f[4];
            v[0] = vertex[0];//v12;
            v[1] = vertex[1];//v13;
            v[2] = vertex[2];//v14;
            v[3] = vertex[3];//v15;
            f = new SE_Vector3i[2];
            f[0].x = 0;
            f[0].y = 1;
            f[0].z = 2;
            f[1].x = 0;
            f[1].y = 2;
            f[1].z = 3;
            geomData->setVertexArray(v, 4);
            geomData->setFaceArray(f, 2);
            rectPatch->mGeometryData[8] = geomData;
            */
        }
        break;
    default:
        break;
    }
	outPrimitive = rectPatch;
    outPrimitiveID = SE_ID::createPrimitiveID();
    SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    resourceManager->setPrimitive(outPrimitiveID, outPrimitive);
}

SE_RectPatch::_TexCoordSet SE_RectPatch::calculateImage(SE_RECTPATCH_TYPE t, float startx, float starty, 
															  float portionx, float portiony, 
															  float portionw, float portionh, 
															  float power2Width, float power2Height, 
															  float stepx, float stepy, bool isFliped)
{
    _TexCoordSet texset;
    switch(t)
    {
    case SE_PATCH_R1_C3:
        {
            if(isFliped)
            {
				texset.tex[0] = SE_Vector2f((startx + portionx) / power2Width, (starty + portiony) / power2Height);
				texset.tex[1] = SE_Vector2f((startx + portionx + stepx) /power2Width, (starty + portiony) / power2Height);
				texset.tex[2] = SE_Vector2f((startx + portionx + stepx * 2) / power2Width, (starty + portiony) / power2Height);
				texset.tex[3] = SE_Vector2f((startx + portionx + portionw) / power2Width, (starty + portiony) / power2Height);
				texset.tex[4] = SE_Vector2f((startx + portionx + portionw) / power2Width, (starty + portiony + portionh) / power2Height);
				texset.tex[5] = SE_Vector2f((startx + portionx + 2 * stepx) / power2Width, (starty + portiony + portionh) / power2Height);
				texset.tex[6] = SE_Vector2f((startx + portionx + stepx) / power2Width, (starty + portiony + portionh) / power2Height);
				texset.tex[7] = SE_Vector2f((startx + portionx) / power2Width, (starty + portiony + portionh) / power2Height);
				
			}
			else
			{
				texset.tex[0] = SE_Vector2f((startx + portionx) / power2Width, (starty + portiony + portionh) / power2Height);
				texset.tex[1] = SE_Vector2f((startx + portionx + stepx) /power2Width, (starty + portiony + portionh) / power2Height);
				texset.tex[2] = SE_Vector2f((startx + portionx + stepx * 2) / power2Width, (starty + portiony + portionh) / power2Height);
				texset.tex[3] = SE_Vector2f((startx + portionx + portionw) / power2Width, (starty + portiony + portionh) / power2Height);
				texset.tex[4] = SE_Vector2f((startx + portionx + portionw) / power2Width, (starty + portiony) / power2Height);
				texset.tex[5] = SE_Vector2f((startx + portionx + 2 * stepx) / power2Width, (starty + portiony) / power2Height);
				texset.tex[6] = SE_Vector2f((startx + portionx + stepx) / power2Width, (starty + portiony) / power2Height);
				texset.tex[7] = SE_Vector2f((startx + portionx) / power2Width, (starty + portiony) / power2Height);
				
			}
        }
        break;
    case SE_PATCH_R3_C1:
        {
			if(isFliped)
			{
				texset.tex[0] = SE_Vector2f((startx + portionx) / power2Width, (starty + portiony) / power2Height);
				texset.tex[1] = SE_Vector2f((startx + portionx + portionw) / power2Width, (starty + portiony) /power2Height);
				texset.tex[2] = SE_Vector2f((startx + portionx + portionw) / power2Width, (starty + portiony + stepy) / power2Height);
				texset.tex[3] = SE_Vector2f((startx + portionx + portionw) / power2Width, (starty + portiony + 2 * stepy) / power2Height);
				texset.tex[4] = SE_Vector2f((startx + portionx + portionw) / power2Width, (starty + portiony + portionh) / power2Height);
				texset.tex[5] = SE_Vector2f((startx + portionx) / power2Width, (starty + portiony + portionh) / power2Height);
				texset.tex[6] = SE_Vector2f((startx + portionx) / power2Width, (starty + portiony + stepy * 2) / power2Height);
				texset.tex[7] = SE_Vector2f((startx + portionx) / power2Width, (starty + portiony + stepy) / power2Height);
				
			}
			else
			{
				texset.tex[0] = SE_Vector2f((startx + portionx) / power2Width, (starty + portiony + portionh) / power2Height);
				texset.tex[1] = SE_Vector2f((startx + portionx + portionw) / power2Width, (starty + portiony + portionh) /power2Height);
				texset.tex[2] = SE_Vector2f((startx + portionx + portionw) / power2Width, (starty + portiony + stepy * 2) / power2Height);
				texset.tex[3] = SE_Vector2f((startx + portionx + portionw) / power2Width, (starty + portiony + stepy) / power2Height);
				texset.tex[4] = SE_Vector2f((startx + portionx + portionw) / power2Width, (starty + portiony) / power2Height);
				texset.tex[5] = SE_Vector2f((startx + portionx) / power2Width, (starty + portiony) / power2Height);
				texset.tex[6] = SE_Vector2f((startx + portionx) / power2Width, (starty + portiony + stepy) / power2Height);
				texset.tex[7] = SE_Vector2f((startx + portionx) / power2Width, (starty + portiony + stepy * 2) / power2Height);
				
			}
        }
        break;
    case SE_PATCH_R3_C3:
        {
			if(isFliped)
			{
            texset.tex[0] = SE_Vector2f((startx + portionx) / power2Width, (starty + portiony) / power2Height);
            texset.tex[1] = SE_Vector2f((startx + portionx + stepx) / power2Width, (starty + portiony) / power2Height);
            texset.tex[2] = SE_Vector2f((startx + portionx + 2 * stepx) / power2Width, (starty + portiony) / power2Height);
            texset.tex[3] = SE_Vector2f((startx + portionx + portionw) / power2Width, (starty + portiony) / power2Height);
            texset.tex[4] = SE_Vector2f((startx + portionx + portionw) / power2Width, (starty + portiony + stepy) / power2Height);
            texset.tex[5] = SE_Vector2f((startx + portionx + portionw) / power2Width, (starty + portiony + 2 * stepy) / power2Height);
            texset.tex[6] = SE_Vector2f((startx + portionx + portionw) / power2Width, (starty + portiony + portionh) / power2Height);
            texset.tex[7] = SE_Vector2f((startx + portionx + 2 * stepx) / power2Width, (starty + portiony + portionh) / power2Height);
            texset.tex[8] = SE_Vector2f((startx + portionx + stepx) / power2Width, (starty + portiony + portionh) / power2Height);
            texset.tex[9] = SE_Vector2f((startx + portionx) / power2Width, (starty + portiony + portionh) / power2Height);
            texset.tex[10] = SE_Vector2f((startx + portionx) / power2Width, (starty + portiony + 2 * stepy) / power2Height);
            texset.tex[11] = SE_Vector2f((startx + portionx) / power2Width, (starty + portiony + stepy) / power2Height);
            texset.tex[12] = SE_Vector2f((startx + portionx + stepx)/ power2Width, (starty + portiony + stepy) / power2Height);
            texset.tex[13] = SE_Vector2f((startx + portionx + 2 * stepx) / power2Width, (starty + portiony + stepy) / power2Height);
            texset.tex[14] = SE_Vector2f((startx + portionx + 2 * stepx) / power2Width, (starty + portiony + 2 * stepy) / power2Height);
            texset.tex[15] = SE_Vector2f((startx + portionx + stepx) / power2Width, (starty + portiony + 2 * stepy) / power2Height);

			}
			else
			{
				texset.tex[0] = SE_Vector2f((startx + portionx) / power2Width, (starty + portiony) / power2Height);
				texset.tex[1] = SE_Vector2f((startx + portionx + stepx) / power2Width, (starty + portiony) / power2Height);
				texset.tex[2] = SE_Vector2f((startx + portionx + 2 * stepx) / power2Width, (starty + portiony) / power2Height);
				texset.tex[3] = SE_Vector2f((startx + portionx + portionw) / power2Width, (starty + portiony) / power2Height);
				texset.tex[4] = SE_Vector2f((startx + portionx + portionw) / power2Width, (starty + portiony + stepy) / power2Height);
				texset.tex[5] = SE_Vector2f((startx + portionx + portionw) / power2Width, (starty + portiony + 2 * stepy) / power2Height);
				texset.tex[6] = SE_Vector2f((startx + portionx + portionw) / power2Width, (starty + portiony + portionh) / power2Height);
				texset.tex[7] = SE_Vector2f((startx + portionx + 2 * stepx) / power2Width, (starty + portiony + portionh) / power2Height);
				texset.tex[8] = SE_Vector2f((startx + portionx + stepx) / power2Width, (starty + portiony + portionh) / power2Height);
				texset.tex[9] = SE_Vector2f((startx + portionx) / power2Width, (starty + portiony + portionh) / power2Height);
				texset.tex[10] = SE_Vector2f((startx + portionx) / power2Width, (starty + portiony + 2 * stepy) / power2Height);
				texset.tex[11] = SE_Vector2f((startx + portionx) / power2Width, (starty + portiony + stepy) / power2Height);
				texset.tex[12] = SE_Vector2f((startx + portionx + stepx)/ power2Width, (starty + portiony + stepy) / power2Height);
				texset.tex[13] = SE_Vector2f((startx + portionx + 2 * stepx) / power2Width, (starty + portiony + stepy) / power2Height);
				texset.tex[14] = SE_Vector2f((startx + portionx + 2 * stepx) / power2Width, (starty + portiony + 2 * stepy) / power2Height);
				texset.tex[15] = SE_Vector2f((startx + portionx + stepx) / power2Width, (starty + portiony + 2 * stepy) / power2Height);

			}
        }
        break;
    default:
        break;
    }
    return texset;
}
SE_RectPatch::_TexCoordSet SE_RectPatch::calculateImageNoFliped(SE_RECTPATCH_TYPE t, float startx, float starty, float portionx, float portiony, float portionw, float portionh, float power2Width, float power2Height, float stepx, float stepy)
{
    _TexCoordSet texset = calculateImage(t, startx, starty, portionx, portiony, portionw, portionh, power2Width, power2Height, stepx, stepy, true);
    for(int i = 0 ; i < 16 ; i++)
    {
        texset.tex[i].y = 1 - texset.tex[i].y;
    }
    return texset;
}
void SE_RectPatch::setTextureCoord(const _TexCoordSet& texCoordSet, int texCoordDataIndex, int v0 , int v1, int v2, int v3)
{
    SE_TextureCoordData* texCoordData = new SE_TextureCoordData;
    SE_Vector2f* texVertex = new SE_Vector2f[4];
    texVertex[0] = texCoordSet.tex[v0];
    texVertex[1] = texCoordSet.tex[v1];
    texVertex[2] = texCoordSet.tex[v2];
    texVertex[3] = texCoordSet.tex[v3];;
    SE_Vector3i* texFace = new SE_Vector3i[2];
    texFace[0].x = 0;
    texFace[0].y = 1;
    texFace[0].z = 2;
    texFace[1].x = 0;
    texFace[1].y = 2;
    texFace[1].z = 3;
    texCoordData->setTexVertexArray(texVertex, 4);
    texCoordData->setTexFaceArray(texFace, 2); 
    mTextureCoordData[texCoordDataIndex] = texCoordData;

}

void SE_RectPatch::setImageData(int index , SE_ImageData* imageData, SE_TEXUNIT_TYPE texUnitType, 
								SE_ImageDataPortion imageDataPortion)
{
	if(!imageData)
		return;
    mImageData[texUnitType] = imageData;
	char* data = imageData->getData();
	char* dataPower2 = imageData->getDataPower2();
	float width, height, power2Width, power2Height, startx, starty;
	width = (float)imageData->getWidth();
	height = (float)imageData->getHeight();
	startx = (float)imageData->getRealStartX();
	starty = imageData->getRealStartY();
	if(imageData->isSizePower2())
	{
		power2Width = (float)imageData->getWidth();
		power2Height = (float)imageData->getHeight();
	}
	else
	{
		power2Width = (float)imageData->getWidthPower2();
		power2Height = (float)imageData->getHeightPower2();
	}
    float portionx, portiony, portionw, portionh;
    if(imageDataPortion.isValid())
    {
        portionx = imageDataPortion.getX();
        portiony = imageDataPortion.getY();
        portionw = imageDataPortion.getWidth();
        portionh = imageDataPortion.getHeight();
    }
    else
    {
        portionx = 0;
        portiony = 0;
        portionw = width;
        portionh = height;
    }
    float stepx = portionw / 3;
	float stepy = portionh / 3;
    mPaddingX = stepx;
    mPaddingY = stepy;
    _TexCoordSet texCoordset;
    if(imageData->isFliped())
    {
        texCoordset = calculateImage(mType, startx, starty, portionx, portiony, portionw, portionh, power2Width, power2Height, stepx, stepy, true); 
    }
    else
    {
        texCoordset = calculateImage(mType, startx, starty, portionx, portiony, portionw, portionh, power2Width, power2Height, stepx, stepy, false);
    }
    switch(mType)
    {
    case SE_PATCH_R1_C3:
        {
            mTextureCoordData.resize(3);
            setTextureCoord(texCoordset, 0, 0, 1, 6, 7);
            setTextureCoord(texCoordset, 1, 1, 2, 5, 6);
            setTextureCoord(texCoordset, 2, 2, 3, 4, 5);

        }
        break;
    case SE_PATCH_R3_C1:
        {
            mTextureCoordData.resize(3);
            setTextureCoord(texCoordset, 0, 0, 1, 2, 7);
            setTextureCoord(texCoordset, 1, 7, 2, 3, 6);
            setTextureCoord(texCoordset, 2, 6, 3, 4, 5);
        } 
        break;
    case SE_PATCH_R3_C3:
        {
            mTextureCoordData.resize(9);
            setTextureCoord(texCoordset, 0, 0, 1, 12, 11);
            setTextureCoord(texCoordset, 1, 1, 2, 13, 12);
            setTextureCoord(texCoordset, 2, 2, 3, 4, 13);
            setTextureCoord(texCoordset, 3, 13, 4, 5, 14);
            setTextureCoord(texCoordset, 4, 14, 5, 6, 7);
            setTextureCoord(texCoordset, 5, 15, 14, 7, 8);
            setTextureCoord(texCoordset, 6, 10, 15, 8, 9);
            setTextureCoord(texCoordset, 7, 11, 12, 15, 10);
            setTextureCoord(texCoordset, 8, 12, 13, 14, 15);
            //
        }

        break;
    default:
        break;
    }
}

void SE_RectPatch::createMesh(SE_Mesh**& outMesh, int& outMeshNum) 
{
    int meshNum = 0;
    switch(mType)
    {
    case SE_PATCH_R1_C3:
        meshNum = 3;
        break;
    case SE_PATCH_R3_C1:
        meshNum = 3;
        break;
    case SE_PATCH_R3_C3:
        meshNum = 9;
        break;
    default:
        meshNum = 0;
        break;
    }
    if(meshNum == 0)
    {
        outMesh = NULL;
        outMeshNum = 0;
        return;
    }
    outMesh = new SE_Mesh*[meshNum];
    outMeshNum = meshNum;
    for(int i = 0 ; i < meshNum ; i++)
    {
        SE_Mesh* mesh = new SE_Mesh(1, 1);
        SE_Surface* surface = new SE_Surface;
        SE_Texture* texture = new SE_Texture;
        mesh->setGeometryData(mGeometryData[i]);
        surface->setGeometryData(mGeometryData[i]);
        surface->setTexture(texture);
        int facetNum = 2;
        int * facets = new int[2];
        facets[0] = 0;
        facets[1] = 1;
        surface->setFacets(facets, facetNum);
        surface->setColor(mColor);
        surface->setProgramDataID(mProgramDataID);
        surface->setSampleMin(mSampleMin);
        surface->setSampleMag(mSampleMag);
        surface->setWrapS(mWrapS);
        surface->setWrapT(mWrapT);
        for(int j = 0 ; j < SE_TEXUNIT_NUM ; j++)
        {
            SE_TextureUnit* texUnit = new SE_TextureUnit();
            texUnit->setImageDataNum(1);
		    if(mImageData[j])
		        texUnit->setImageData(0, mImageData[j]);
		    if(j == 0)
                texUnit->setTextureCoordData(mTextureCoordData[i]);
            texture->setTextureUnit(j, texUnit);
        }
        mesh->setSurface(0, surface);
        mesh->setTexture(0, texture);
        outMesh[i] = mesh; 
    }
    
}
void SE_RectPatch::createGeometryData()
{
}
