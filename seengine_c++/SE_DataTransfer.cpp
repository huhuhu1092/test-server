#include "SE_DataTransfer.h"
#include "SE_Mesh.h"
#include "SE_Utils.h"
#include "SE_Buffer.h"
SE_MeshTransfer::~SE_MeshTransfer()
{
    if(mSurfaceTransferArray)
        delete[] mSufaceTransferArray;
    if(mTexTransferArray)
        delete[] mTexTransferArray;
}
SE_Mesh* SE_MeshTransfer::createMesh(SE_ResourceManager* resourceManager)
{
    SE_Mesh* mesh = new SE_Mesh(mSurfaceNum, mTexNum);
    mesh->setGeometryData(resourceManager->getGeometryData(mGeomDataID));
    for(int i = 0 ; i < mTexNum ; i++)
    {
        SE_TextureTransfer* textureTransfer = &mTexTransferArray[i];
        SE_Texture texture = new SE_Texture;
        for(int j = 0 ; j < textureTransfer->getTextureUnitNum() ; j++)
        {
            SE_TextureUnitTransfer* texUnitTransfer = textureTransfer->getTextureUnit(j);
            SE_TextureUnit* texUnit = new SE_TextureUnit;
            texUnit->setTextureCoordData(resourceManager->getTexCoordData(texUnitTransfer->getTexCoordDataID()));
            SE_ImageDataID* imageDataID = new SE_ImageDataID[texUnitTransfer->getImageDataNum()];
            for(int n = 0 ; n < texUnitTransfer->getImageDataNum() ; n++)
            {
                imageDataID[n] = *texUnitTransfer->getImageDataID(n);
            }
            texUnit->setImageData(imageDataID, texUnitTransfer->getImageDataNum());
            texture->setTextureUnit(texUnitTransfer->getType(), texUnit);
        }
        mesh->setTexture(i, texture);
    }
    for(int i = 0 ; i < mSurfaceNum; i++)
    {
        SE_SurfaceDataTransfer* surfaceTransfer = &mSurfaceTransferArray[i];
        SE_Surface* surface = new SE_Surface;
        surface->setMaterialData(resourceManager->getMaterialData(surfaceTransfer->materialDataID));
        int*  facetArray= new int[surfaceTransfer->getFacetNum()];
        memmove(facetArray, surfaceTransfer->getFacetArray(), sizeof(int) * surfaceTransfer->getFacetNum());
        surface->setGeometryData(mesh->getGeometryData());
        surface->setFacets(facetArray, surfaceTransfer->getFacetNum());
        int texIndex = surfaceTransfer->getTextureIndex();
        SE_Texture* texture = mesh->getTexture(texIndex);
        surface->setTexture(texture);   
        mesh->setSurface(i, surface); 
    }
    return mesh;
}
void SE_MeshTransfer::createFromMesh(SE_Mesh* mesh)
{
}
void SE_MeshTransfer::createFromBytes(char* data, int startPos, int len)
{
    while(startPos < len)
    {
        int id[4];
        int currPos = startPos;
        int i;
        for(i = 0 ; i < 4 ; i++)
        {
            id[i] = readInt(data, &currPos);
        }
        SE_GeometryDataID geomid(id[0], id[1], id[2], id[3]);
        mGeomDataID = geomid;
        readVector3f(mColor, data, &currPos);
        mTexNum = readInt(data, &currPos);
        if(mTexNum > 0)
        {
            mTexTransferArray  = new SE_TextureTransfer[mTexNum];
        }
        for(i = 0 ; i < textureNum ; i++)
        {
            SE_TextureTransfer* textureTransfer = &mTexTransferArray[i];
            int texUnitNum = readInt(data, &currPos);
            SE_TextureUnitTransfer* texUnitTransfer = new SE_TextureUnitTransfer[texUnitNum];
            textureTransfer->setTextureUnitTransfer(texUnitTransfer, texUnitNum);
            for(int j = 0 ; j < texUnitNum ; j++)
            {
                int type = readInt(data, &currPos);
                for(int t = 0 ; t < 4 ; t++)
                {
                    id[t] = readInt(data, &currPos);
                }
                SE_TextureCoordDataID texDataID(id[0], id[1], id[2], id[3]);
                texUnitTransfer->setTextureCoordDataID(texDataID);
                int imageNum = readInt(data, &currPos);
                SE_ImageDataID* imageDataIDArray = new SE_ImageDataID[imageNum];
                for(int n = 0 ; n < imageNum ; n++)
                {
                    std::string str = readString(data, &currPos);
                    SE_ImageDataID imageID(str.c_str());
                    imageDataIDArray[n] = imageID;
                }
                texUnitTransfer->setImageDataID(imageDataIDArray, imageNum);
            }
        } 
        mSurfaceNum = readInt(data, &currPos);
        mSurfaceTransferArray = new SE_SurfaceTransfer[mSurfaceNum];
        for(i = 0 ; i < mSurfaceNum ; i++)
        {
            SE_SurfaceTransfer* surfaceTransfer = &mSurfaceTransferArray[i];
            int texIndex = readInt(data, &currPos);
            int j;
            for(j = 0 ; j < 4 ; j++)
            {
                id[j] = readInt(data, &currPos);
            }
            SE_MaterialDataID materialID(id[0], id[1], id[2], id[3]);
            surfaceTransfer->setMaterialDataID(materialID);
            surfaceTransfer->setTextureIndex(texIndex);
            int facetNum = readInt(data, &currPos);
            int* facets = new int[facetNum];
            for(j = 0 ; j < facetNum ; j++)
            {
                facets[j] = readInt(data, &currPos);
            }
            surfaceTransfer->setFacets(facets, facetNum);
        }
    }

}
void SE_MeshTransfer::writeToBytes(char*& data, int& len)
{
    SE_BufferOutput outBuffer;  
    mGeomDataID.write(outBuffer);
    outBuffer.writeInt(mTexNum);
    if(mTexNum > 0)
    {
        for(int i = 0 ; i < mTexNum ; i++)
    } 
}
