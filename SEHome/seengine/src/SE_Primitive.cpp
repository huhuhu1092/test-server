#include "SE_Primitive.h"
#include "SE_GeometryData.h"
#include "SE_TextureCoordData.h"
#include <memory>
SE_RectPrimitive::SE_RectPrimitive(const SE_Rect3D& rect) : mRect3D(rect)
{
    memset(mImageDataArray, 0, sizeof(_ImageData*) * SE_Texture::TEXUNIT_NUM);
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
    if(mGeometryData)
        delete mGeometryData;
    if(mTexCoordData)
        delete mTexCoordData;
    if(mMaterialData)
        delete mMaterialData;
}
SE_Mesh* SE_RectPrimitive::createMesh()
{
	std::auto_ptr<SE_Mesh> mesh(new SE_Mesh(1, 1)); // rect has just one surface and one texture
	if(!mesh.get())
		return NULL;
	SE_Vector3f* vertex = new SE_Vector3f[4];
	if(!vertex)
		return NULL;
	mRect3D.getVertex(vertex);
	std::auto_ptr<SE_GeometryData> geomData(new SE_GeometryData());
    std::auto_ptr<SE_TextureCoordData> texCoordData(new SE_TextureCoordData);
	if(!geomData.get())
	{
		delete[] vertex;
		return NULL;
	}
    if(!texCoordData.get())
    {
        delete[] vertex;
        return NULL;
    }
	SE_Vector3i*  faces =  new  SE_Vector3i[2];
	if(!faces)
	{
		delete[] vertex;
		return NULL;
	}
	mRect3D.getFaces(faces);
	geomData->setVertexArray(vertex, 4);
	geomData->setFaceArray(faces, 2);
	mesh->setGeometryData(geomData.get());
    mGeometryData = geomData.get();
	geomData.release();
    std::auto_ptr<SE_Surface> surface(new SE_Surface);
    std::auto_ptr<SE_Texture> texture(new SE_Texture);
    surface->setGeometryData(mGeometryData);
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
    SE_Vector2f* texVertex = new SE_Vector2f[4];
    texVertex[0] = SE_Vector2f(0, 0);
    texVertex[1] = SE_Vector2f(1, 0);
    texVertex[2] = SE_Vector2f(1, 1);
    texVertex[3] = SE_Vector2f(0, 1);
    SE_Vector3i* texFaces = new SE_Vector3i[2];
    memcpy(texFaces, faces, sizeof(SE_Vector3i) * 2);
    for(int i = 0 ; i < SE_Texture::TEXUNIT_NUM ; i++)
    {
        SE_TextureUnit* texUnit = new SE_TextureUnit();
        texUnit->setImageDataNum(1);
		texUnit->setImageData(0, mImageDataArray[i].imageData);
        texture->setTextureUnit(i, texUnit);
    }
    mesh->setSurface(0, surface.get());
    mesh->setTexture(0, texture.get());
    surface.release();
    texture.release();
	SE_Mesh* ret = mesh.release();
	return ret;
}
