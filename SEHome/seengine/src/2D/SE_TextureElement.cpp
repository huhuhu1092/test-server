#include "SE_TextureElement.h"
#include "SE_RenderTarget.h"
#include "SE_RenderTargetManager.h"
#include "SE_Utils.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_Camera.h"
#include "SE_CameraManager.h"
#include "SE_ImageData.h"
#include "SE_ElementManager.h"
#include "SE_Mesh.h"
#include "SE_DataValueDefine.h"
#include "SE_ImageDataElement.h"
#include "SE_SpatialManager.h"
#include <math.h>
SE_TextureElement::SE_TextureElement(const SE_StringID& uri) : mImageData(NULL), mContentChild(NULL), mImageDataElement(NULL)
{
    setURI(uri);
    init();
}
SE_TextureElement::~SE_TextureElement()
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	resourceManager->removeImageData(mImageDataID);
	SE_RenderTargetManager* rm = SE_Application::getInstance()->getRenderTargetManager();
	SE_RenderTarget* renderTarget = rm->remove(mRenderTargetID);
	rm->release(renderTarget);
	
	//if(mContentChild)
	//	delete mContentChild;
}
SE_Element* SE_TextureElement::clone()
{
	//SE_TextureElement* element = new SE_TextureElement(getURI());
	//SE_2DNodeElement::clone(this, element);
	//return element;
    return NULL;
}
SE_2DNodeElement* SE_TextureElement::createChildContent(const SE_StringID& strURL)
{
    SE_Util::SplitStringList strList = SE_Util::splitString(strURL.getStr(), "/");
    SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    SE_ElementSchema* elementSchema = resourceManager->getElementSchema(strURL.getStr());
    SE_2DNodeElement* e = (SE_2DNodeElement*)elementSchema->createElement();
    e->setMountPoint(0, 0);
    e->setOwnRenderTargetCamera(true);
	return e;
}
void SE_TextureElement::init()
{
	SE_StringID strURL = getURL();
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    SE_ImageDataID imageDataID = strURL.getStr();
	SE_ImageData* imageData = resourceManager->getImageData(imageDataID);
	SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();

	if(!imageData)
	{
	    SE_2DNodeElement* e = createChildContent(strURL);
		imageData = new SE_ImageData;
		imageData->setIsFliped(true);
		resourceManager->setImageData(imageDataID, imageData);
        mImageData = imageData;
        mImageDataID = imageDataID;
        mContentChild = e;
		elementManager->add(this, mContentChild);
	}
    else
    {
        mImageData = imageData;
        mImageDataID = imageDataID;
    }
	mImageDataElement = new SE_ImageDataElement(mImageDataID);
	mImageDataElement->setPivotX(0);
	mImageDataElement->setPivotY(0);
	mImageDataElement->setMountPoint(0, 0);
	elementManager->add(this, mImageDataElement);
}
void SE_TextureElement::update(SE_ParamValueList& paramValueList)
{
	/*
    if(mShareContent)
        return;
    if(!mContentChild)
        return;
    SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
	SE_Element* tmpElement = elementManager->remove(mContentChild->getID());
    SE_ASSERT(tmpElement == mContentChild);
	elementManager->release(mContentChild, SE_RELEASE_NO_DELAY);
    SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    resourceManager->removeImageData(mImageDataID);
	SE_RenderTargetManager* rm = SE_Application::getInstance()->getRenderTargetManager();
	rm->remove(mRenderTargetID);
    init();
    spawn();
    layout();
    update(0);
    updateSpatial();
	*/
}
void SE_TextureElement::update(const SE_AddressID& address, const SE_Value& value)
{
	/*
    if(mShareContent)
        return;
    if(!mContentChild)
        return;
		SE_2DNodeElement::update(address, value);
	*/
}
void SE_TextureElement::setImageData(SE_Primitive* primitive)
{
	//primitive->setImageData(0, mImageData, SE_TEXTURE0);
}
void SE_TextureElement::setSurface(SE_Surface* surface)
{
	//surface->setProgramDataID(DEFAULT_SHADER);
	//surface->setRendererID(DEFAULT_RENDERER);
}
void SE_TextureElement::spawn()
{
	/*
    if(mShareContent)
        return;
	if(mContentChild)
    {
	    mContentChild->spawn();
	}
	*/
	SE_2DNodeElement::spawn();
}
void SE_TextureElement::update(const SE_TimeKey& key)
{
	/*
	if(mContentChild)
	mContentChild->update(key);
	*/
	SE_2DNodeElement::update(key);
}
void SE_TextureElement::setRenderTargetID(const SE_RenderTargetID& renderTarget)
{
    mRenderTargetID = renderTarget;
	SE_ASSERT(mImageDataElement != NULL);
	mImageDataElement->setRenderTargetID(renderTarget);
    if(mContentRenderTargetID.isValid())
        return;
	if(mContentChild)
	{
	    SE_TextureTarget* textureTarget = new SE_TextureTarget(mImageData);
	    SE_RenderTargetManager* renderTargetManager = SE_Application::getInstance()->getRenderTargetManager();
        mContentRenderTargetID = renderTargetManager->add(textureTarget);
	    textureTarget->setBackground(SE_Vector4f(0.0, 0, 0, 0.0));
	    mContentChild->setRenderTargetID(mContentRenderTargetID);
	    float ratio = mHeight / mWidth;
	    float angle = 2 * SE_RadianToAngle(atanf(mWidth / 20.0f));
        SE_Camera* camera = new SE_Camera;
	    float left = mContentChild->getDeltaLeft() + mWidth / 2;
	    float top = mContentChild->getDeltaTop() + mHeight / 2;
    	SE_Vector3f v(left , top , 10);
	    camera->setLocation(v);
		if(SE_Application::getInstance()->isScreenRotate())
		{
	        camera->create(v, SE_Vector3f(0, -1, 0), SE_Vector3f(1, 0, 0), SE_Vector3f(0, 0, 1), angle, ratio, 1, 50);
		}
		else
	        camera->create(v, SE_Vector3f(1, 0, 0), SE_Vector3f(0, 1, 0), SE_Vector3f(0, 0, 1), angle, ratio, 1, 50);

	    camera->setViewport(0, 0, (int)mWidth, (int)mHeight);
	    mImageData->setPixelFormat(SE_ImageData::RGBA);
	    SE_CameraManager* cameraManager = SE_Application::getInstance()->getCameraManager();
	    SE_CameraID cameraID = cameraManager->add(camera);
	    textureTarget->setWidth(mWidth);
	    textureTarget->setHeight(mHeight);
	    textureTarget->setCamera(cameraID);
	    textureTarget->create();
	}
}
void SE_TextureElement::setRenderTargetSeq(const SE_RenderTargetSeq& renderTargetSeq)
{
    mRenderTargetSeq = renderTargetSeq;
    if(mContentChild)
	{
        SE_RenderTargetSeq t = renderTargetSeq - 1;
        mContentChild->setRenderTargetSeq(t);
    }
	SE_ASSERT(mImageDataElement != NULL);
	mImageDataElement->setRenderTargetSeq(renderTargetSeq);
}
void SE_TextureElement::layout()
{
	if(!mContentChild)
	{
		return SE_2DNodeElement::layout();
	}
	else
	{
	    calculateRect(mPivotX, mPivotY, 0, 0);
        mContentChild->layout();
	    mWidth = mContentChild->getWidth();
 	    mHeight = mContentChild->getHeight();
	    SE_ASSERT(mWidth > 0 && mHeight > 0);
	    mImageData->setWidth((int)mWidth);
	    mImageData->setHeight((int)mHeight);
		SE_ASSERT(mImageDataElement);
		mImageDataElement->layout();
	}
}
SE_Spatial* SE_TextureElement::createSpatial()
{
	return SE_2DNodeElement::createSpatial();
	/*
    if(mShareContent)
    {
        return createSpatialByImage();
    }
    else if(mContentChild)
    {    
        SE_Spatial* node = new SE_CommonNode;
        std::string nodeStr = std::string(getFullPathName().getStr()) + "_" + "TextureNode";
        node->setName(nodeStr.c_str());
        SE_Spatial* spatial = createSpatialByImage();
        SE_Spatial* contentChildSpatial = mContentChild->createSpatial();
        SE_SpatialManager* spatialManager = SE_Application::getInstance()->getSpatialManager();
        spatialManager->add(node, spatial);
        spatialManager->add(node, contentChildSpatial);
        return node;
    }
    else
    {
        return NULL;
	}
	*/
}
