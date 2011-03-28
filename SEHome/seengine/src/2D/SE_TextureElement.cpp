#include "SE_TextureElement.h"
#include "SE_RenderTarget.h"
#include "SE_RenderTargetManager.h"
#include "SE_Utils.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_Camera.h"
#include "SE_CameraManager.h"
#include "SE_ImageData.h"
SE_TextureElement::SE_TextureElement(const SE_StringID& uri)
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
void SE_TextureElement::init()
{
	SE_StringID strURL = getURL();
	SE_Util::SplitStringList strList = SE_Util::splitString(strURL.getStr(), "/");
    SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	SE_ElementSchema* elementSchema = resourceManager->getElementSchema(strURL.getStr());
    mContentChild = (SE_2DNodeElement*)elementSchema->createElement();
    SE_ImageDataID imageDataID = mContentChild->getFullPathName().getStr();
	SE_ImageData* imageData = resourceManager->getImageData(imageDataID);
	if(!imageData)
	{
		imageData = new SE_ImageData;
		resourceManager->setImageData(imageDataID, imageData);
	}
    mImageData = imageData;
    mImageDataID = imageDataID;
    mContentChild->setMountPoint(0, 0);
    mContentChild->setOwnRenderTargetCamera(true);
}
void SE_TextureElement::update(SE_ParamValueList& paramValueList)
{
    SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
	SE_Element* tmpElement = elementManager->remove(mContentChild->getID());
    SE_ASSERT(tmpElement == mContentChild);
	elementManager->release(mContentChild);
    //delete mContentChild;
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
	SE_ASSERT(getChildren().size() == 0);
	if(mContentChild)
		mContentChild->spawn();
}
void SE_TextureElement::update(const SE_TimeKey& key)
{
	if(mContentChild)
		mContentChild->update(key);
}
void SE_TextureElement::setImage(const SE_ImageDataID& id, SE_ImageData* imageData)
{
	mImageDataID = id;
	mImageData = imageData;
}
void SE_TextureElement::layout()
{
	if(!mContentChild)
	{
		return;
	}
	calculateRect(INVALID_GEOMINFO, INVALID_GEOMINFO, 0, 0);
    mContentChild->layout();
	mWidth = mContentChild->getWidth();
	mHeight = mContentChild->getHeight();
	SE_TextureTarget* textureTarget = new SE_TextureTarget(mImageData);
	SE_RenderTargetManager* renderTargetManager = SE_Application::getInstance()->getRenderTargetManager();
    mRenderTargetID = renderTargetManager->add(textureTarget);
	textureTarget->setBackground(SE_Vector4f(0, 0, 0, 0));
	mContentChild->setRenderTargetID(mRenderTargetID);
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
SE_Spatial* SE_TextureElement::createSpatial()
{
    SE_Spatial* spatial = createSpatialByImage();
	return spatial;
}
