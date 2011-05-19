#include "SE_ImageDataElement.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_Primitive.h"
#include "SE_Mesh.h"
#include "SE_DataValueDefine.h"
SE_ImageDataElement::SE_ImageDataElement(const SE_ImageDataID& imageDataID)
{
	mImageDataID = imageDataID;
}
void SE_ImageDataElement::spawn()
{

}
void SE_ImageDataElement::layout()
{
	SE_ImageData* imageData = getImageData();
	if(imageData)
	    calculateRect(mPivotX, mPivotY, imageData->getWidth(), imageData->getHeight());
	setWidth(imageData->getWidth());
	setHeight(imageData->getHeight());
}
SE_Spatial* SE_ImageDataElement::createSpatial()
{
	SE_ImageData* imageData = getImageData();
	if(!imageData)
		return NULL;
	return createSpatialByImage();
}
SE_Element* SE_ImageDataElement::clone()
{
	return NULL;
}
void SE_ImageDataElement::setImageData(SE_Primitive* primitive)
{
	SE_ImageData* imageData = getImageData();
	primitive->setImageData(0, imageData, SE_TEXTURE0);
}
void SE_ImageDataElement::setSurface(SE_Surface* surface)
{
	surface->setProgramDataID(DEFAULT_SHADER);
	surface->setRendererID(DEFAULT_RENDERER);
}
SE_ImageData* SE_ImageDataElement::getImageData()
{
	SE_ResourceManager* resourceManager = SE_GET_RESOURCEMANAGER();
	SE_ImageData* imageData = resourceManager->getImageData(mImageDataID);
	return imageData;
	
}
