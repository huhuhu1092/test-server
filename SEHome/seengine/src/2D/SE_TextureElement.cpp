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
#include "SE_CommonNode.h"
#include "SE_SpatialManager.h"
#include <math.h>
SE_TextureElement::SE_TextureElement(const SE_StringID& uri) : mImageData(NULL), mContentChild(NULL), mShareContent(false)
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
	if(mContentChild)
		delete mContentChild;
}
SE_Element* SE_TextureElement::clone()
{
	SE_TextureElement* element = new SE_TextureElement(getURI());
	SE_2DNodeElement::clone(this, element);
	return element;
}
SE_2DNodeElement* SE_TextureElement::createChildContent()
{
    SE_StringID strURL = getURL();
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
	SE_2DNodeElement* e = createChildContent();
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    SE_ImageDataID imageDataID = e->getFullPathName().getStr();
	SE_ImageData* imageData = resourceManager->getImageData(imageDataID);
	if(!imageData)
	{
		imageData = new SE_ImageData;
		resourceManager->setImageData(imageDataID, imageData);
        mImageData = imageData;
        mImageDataID = imageDataID;
	}
    else
    {
        mImageData = imageData;
        mImageDataID = imageDataID;
        mShareContent = true;
    }
    if(!mShareContent)
    {
        SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
        mContentChild = e;
        elementManager->add(this, mContentChild);
    }
	else
	{
		delete e;
	}
}
void SE_TextureElement::update(SE_ParamValueList& paramValueList)
{
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
}
void SE_TextureElement::update(const SE_AddressID& address, const SE_Value& value)
{
    if(mShareContent)
        return;
    if(!mContentChild)
        return;
    SE_2DNodeElement::update(address, value);
}
void SE_TextureElement::setImageData(SE_Primitive* primitive)
{
	primitive->setImageData(0, mImageData, SE_TEXTURE0);
}
void SE_TextureElement::setSurface(SE_Surface* surface)
{
	surface->setProgramDataID(DEFAULT_SHADER);
	surface->setRendererID(DEFAULT_RENDERER);
}
void SE_TextureElement::spawn()
{
    if(mShareContent)
        return;
	if(mContentChild)
    {
	    mContentChild->spawn();
    }
}
void SE_TextureElement::update(const SE_TimeKey& key)
{
    if(mShareContent)
        return;
	if(mContentChild)
		mContentChild->update(key);
}
/*
void SE_TextureElement::setImage(const SE_ImageDataID& id, SE_ImageData* imageData)
{
	mImageDataID = id;
	mImageData = imageData;
}
*/
void SE_TextureElement::setRenderTargetID(const SE_RenderTargetID& renderTarget)
{
    mRenderTargetID = renderTarget;
    if(mShareContent)
        return;
	SE_TextureTarget* textureTarget = new SE_TextureTarget(mImageData);
	SE_RenderTargetManager* renderTargetManager = SE_Application::getInstance()->getRenderTargetManager();
    mContentRenderTargetID = renderTargetManager->add(textureTarget);
	textureTarget->setBackground(SE_Vector4f(0, 0, 0, 0));
	mContentChild->setRenderTargetID(mContentRenderTargetID);
	float ratio = mHeight / mWidth;
	float angle = 2 * SE_RadianToAngle(atanf(mWidth / 20.0f));
    SE_Camera* camera = new SE_Camera;
	float left = mContentChild->getDeltaLeft() + mWidth / 2;
	float top = mContentChild->getDeltaTop() + mHeight / 2;
	SE_Vector3f v(left , top , 10);
	camera->setLocation(v);
	camera->create(v, SE_Vector3f(1, 0, 0), SE_Vector3f(0, -1, 0), SE_Vector3f(0, 0, 1), angle, ratio, 1, 50);
	camera->setViewport(0, 0, (int)mWidth, (int)mHeight);
	mImageData->setWidth((int)mWidth);
	mImageData->setHeight((int)mHeight);
	mImageData->setPixelFormat(SE_ImageData::RGBA);
	SE_CameraManager* cameraManager = SE_Application::getInstance()->getCameraManager();
	SE_CameraID cameraID = cameraManager->add(camera);
	textureTarget->setWidth(mWidth);
	textureTarget->setHeight(mHeight);
	textureTarget->setCamera(cameraID);
	textureTarget->create();
}
void SE_TextureElement::setRenderTargetSeq(const SE_RenderTargetSeq& renderTargetSeq)
{
    mRenderTargetSeq = renderTargetSeq;
    if(mShareContent)
        return;
    SE_RenderTargetSeq t = renderTargetSeq - 1;
    if(mContentChild)
    {
        mContentChild->setRenderTargetSeq(t);
    }    
}
void SE_TextureElement::layout()
{
    if(mShareContent)
    {
        return;
    }
	if(!mContentChild)
	{
		return;
	}
	calculateRect(mPivotX, mPivotY, 0, 0);
    mContentChild->layout();
	mWidth = mContentChild->getWidth();
	mHeight = mContentChild->getHeight();
}
SE_Spatial* SE_TextureElement::createSpatial()
{
    if(mShareContent)
    {
        return createSpatialByImage();
    }
    else if(mContentChild)
    {    
        SE_CommonNode* node = new SE_CommonNode;
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
}
