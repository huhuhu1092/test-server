#include "SE_DataTransfer.h"
#include "SE_Mesh.h"
#include "SE_Utils.h"
#include "SE_Buffer.h"
#include "SE_Common.h"
#include "SE_ResourceManager.h"
#include <string>
#include "SE_Log.h"
SE_MeshTransfer::~SE_MeshTransfer()
{
    if(mSurfaceTransferArray)
        delete[] mSurfaceTransferArray;
    if(mTexTransferArray)
        delete[] mTexTransferArray;
}
SE_Mesh* SE_MeshTransfer::createMesh(SE_ResourceManager* resourceManager)
{
    SE_Mesh* mesh = new SE_Mesh(mSurfaceNum, mTexNum);
    mesh->setGeometryData(resourceManager->getGeometryData(mGeomDataID));
    mesh->mKeyFrames = mKeyFrames;
    if(mTexNum == 0)
    {        
        mesh->setTexture(0,NULL);
    }

    if(mSurfaceNum == 0)
    {
        mesh->setSurface(0, NULL); 
    }
    for(int i = 0 ; i < mTexNum ; i++)
    {
        SE_TextureTransfer* textureTransfer = &mTexTransferArray[i];
        SE_Texture* texture = new SE_Texture;
        for(int j = 0 ; j < textureTransfer->getTextureUnitNum() ; j++)
        {
            SE_TextureUnitTransfer* texUnitTransfer = textureTransfer->getTextureUnit(j);
			SE_TextureUnit* texUnit = new SE_TextureUnit();
            texUnit->setTextureCoordData(resourceManager->getTextureCoordData(texUnitTransfer->getTexCoordDataID()));
            SE_ImageDataID* imageDataArray = new SE_ImageDataID[texUnitTransfer->getImageDataNum()];
            for(int n = 0 ; n < texUnitTransfer->getImageDataNum() ; n++)
            {
                imageDataArray[n] = texUnitTransfer->getImageDataID(n);
                //SE_ImageDataID id = texUnitTransfer->getImageDataID(n);
				//SE_ImageData* imageData = resourceManager->getImageData(id);
                //imageDataArray[n] = *imageData;
            }
            texUnit->setImageDataID(imageDataArray, texUnitTransfer->getImageDataNum());
            texture->setTextureUnit(texUnitTransfer->getType(), texUnit);
        }
        mesh->setTexture(i, texture);
    }
    for(int i = 0 ; i < mSurfaceNum; i++)
    {
        SE_SurfaceTransfer* surfaceTransfer = &mSurfaceTransferArray[i];
        SE_Surface* surface = new SE_Surface;
        surface->setMaterialData(resourceManager->getMaterialData(surfaceTransfer->getMaterialDataID()));
        surface->setProgramDataID(surfaceTransfer->getProgramDataID());
		surface->setRendererID(surfaceTransfer->getRendererID());
        int*  facetArray= new int[surfaceTransfer->getFacetNum()];
        memmove(facetArray, surfaceTransfer->getFacetArray(), sizeof(int) * surfaceTransfer->getFacetNum());
        surface->setGeometryData(mesh->getGeometryData());
        surface->setFacets(facetArray, surfaceTransfer->getFacetNum());
        int texIndex = surfaceTransfer->getTextureIndex();
        SE_Texture* texture = mesh->getTexture(texIndex);
        surface->setTexture(texture);   

        //set vertex buffer
        surface->setVertexBuffer(resourceManager->getVertexBuffer(mVbID));
        mesh->setSurface(i, surface); 
    }
    return mesh;
}
void SE_MeshTransfer::createFromMesh(SE_Mesh* mesh)
{
}
void SE_MeshTransfer::read(SE_BufferInput& inputBuffer)
{
    mGeomDataID.read(inputBuffer);

    //read vertex buffer id
    mVbID.read(inputBuffer);

    int i, j;
///////////////////add by liusong begin///////////////////////
    int keyFrameSize = inputBuffer.readInt();
    for(i = 0 ; i < keyFrameSize ; i++)
    {
	SE_KeyFrame<SE_NewTransform>* frame = new SE_KeyFrame<SE_NewTransform>();
	frame->key = inputBuffer.readInt();
	float* out = new float[16];
	for(int i = 0; i < 16; i++)
	{
		out[i] = inputBuffer.readFloat();
	}
	frame->data.matrix = SE_Matrix4f(out);
	int hasMesh = inputBuffer.readInt();
	if (hasMesh > 0) {
		int numVertexes = inputBuffer.readInt();
		frame->data.vertexNum = numVertexes;
		frame->data.vertexArray = new SE_Vector3f[numVertexes];
		for(j = 0 ; j < numVertexes ; j++)
		{
		    frame->data.vertexArray[j].x = inputBuffer.readFloat();
		    frame->data.vertexArray[j].y = inputBuffer.readFloat();
		    frame->data.vertexArray[j].z = inputBuffer.readFloat();
		}
	} else {
		frame->data.vertexNum = 0;
	}
	mKeyFrames.push_back(frame);
    }
////////////////////add by liusong end///////////////////////
    mColor = inputBuffer.readVector3f();
    mTexNum = inputBuffer.readInt();
    if(mTexNum > 0)
    {
        mTexTransferArray  = new SE_TextureTransfer[mTexNum];
    }

    for(i = 0 ; i < mTexNum ; i++)
    {
        SE_TextureTransfer* textureTransfer = &mTexTransferArray[i];
        int texUnitNum = inputBuffer.readInt();
        SE_TextureUnitTransfer* texUnitTransfer = new SE_TextureUnitTransfer[texUnitNum];
        textureTransfer->setTextureUnitTransfer(texUnitTransfer, texUnitNum);
        for(int j = 0 ; j < texUnitNum ; j++)
        {
            int type = inputBuffer.readInt();
            SE_TextureCoordDataID texDataID;
            texDataID.read(inputBuffer);
            texUnitTransfer->setTextureCoordDataID(texDataID);
            int imageNum = inputBuffer.readInt();
            SE_ImageDataID* imageDataIDArray = new SE_ImageDataID[imageNum];
            for(int n = 0 ; n < imageNum ; n++)
            {
                std::string str = inputBuffer.readString();
                SE_ImageDataID imageID(str.c_str());
                imageDataIDArray[n] = imageID;
            }
            texUnitTransfer->setImageDataID(imageDataIDArray, imageNum);
            texUnitTransfer->setType(type);
        }
    } 
    mSurfaceNum = inputBuffer.readInt();
    mSurfaceTransferArray = new SE_SurfaceTransfer[mSurfaceNum];
    for(i = 0 ; i < mSurfaceNum ; i++)
    {
        SE_SurfaceTransfer* surfaceTransfer = &mSurfaceTransferArray[i];
        SE_MaterialDataID materialID;
        materialID.read(inputBuffer);
        surfaceTransfer->setMaterialDataID(materialID);
        int facetNum = inputBuffer.readInt();
        int* facets = new int[facetNum];
        for(j = 0 ; j < facetNum ; j++)
        {
            facets[j] = inputBuffer.readInt();
        }
        surfaceTransfer->setFacets(facets, facetNum);
        SE_ProgramDataID pid;
        pid.read(inputBuffer);
        surfaceTransfer->setProgramDataID(pid);
		SE_RendererID renderid;
		renderid.read(inputBuffer);
		surfaceTransfer->setRendererID(renderid);
		int texIndex = inputBuffer.readInt();
        surfaceTransfer->setTextureIndex(texIndex);
    }

}
void SE_MeshTransfer::write(SE_BufferOutput& outBuffer)
{
    mGeomDataID.write(outBuffer);
    outBuffer.writeInt(mTexNum);
    if(mTexNum > 0)
    {
        for(int i = 0 ; i < mTexNum ; i++)
        {
            SE_TextureTransfer* textureTransfer = &mTexTransferArray[i];
            outBuffer.writeInt(textureTransfer->getTextureUnitNum());
            for(int j = 0 ; j < textureTransfer->getTextureUnitNum() ; j++)
            {
                SE_TextureUnitTransfer* texUnit = textureTransfer->getTextureUnit(j);
                outBuffer.writeInt(texUnit->getType());
                texUnit->getTexCoordDataID().write(outBuffer);
                outBuffer.writeInt(texUnit->getImageDataNum());
                for(int k = 0 ; k < texUnit->getImageDataNum() ; k++)
                {
                    texUnit->getImageDataID(k).write(outBuffer);
                }
            }
        }
    } 
    outBuffer.writeInt(mSurfaceNum);
    SE_ASSERT(mSurfaceNum > 0);
    for(int i = 0 ; i < mSurfaceNum ; i++)
    {
        SE_SurfaceTransfer* surfaceTransfer = &mSurfaceTransferArray[i];
        outBuffer.writeInt(surfaceTransfer->getTextureIndex());
        surfaceTransfer->getMaterialDataID().write(outBuffer);
        outBuffer.writeInt(surfaceTransfer->getFacetNum());
        int* facetArray = surfaceTransfer->getFacetArray();
        for(int j = 0 ; j < surfaceTransfer->getFacetNum() ; j++)
        {
            outBuffer.writeInt(facetArray[i]);
        }
    }
}
