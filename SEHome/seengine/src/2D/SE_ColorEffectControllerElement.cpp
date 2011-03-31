#include "SE_ColorEffectControllerElement.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_ParamManager.h"
#include "SE_ColorEffectController.h"
#include "SE_ElementManager.h"
#include "SE_ImageElement.h"
#include "SE_TextureElement.h"
#include "SE_RenderTarget.h"
#include "SE_RenderTargetManager.h"
#include "SE_ShaderProperty.h"
#include "SE_Mesh.h"
#include "SE_SpatialManager.h"
#include "SE_DataValueDefine.h"
#include "SE_Log.h"
SE_ColorEffectControllerElement::SE_ColorEffectControllerElement(const SE_StringID& uri)
{
	setURI(uri);
	SE_StringID url = getURL();
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	mColorEffectController = resourceManager->getColorEffectController(url.getStr());
    if(mColorEffectController)
    {
	    mName = mColorEffectController->getID().getStr();
        setMountPoint(mColorEffectController->getMountPoint());
    }
    else
    {
        LOGI("#### can not load coloreffectcontroller : %s ####", url.getStr());
    }
	mCurrentElement = NULL;
}
SE_Element* SE_ColorEffectControllerElement::clone()
{
	SE_ColorEffectControllerElement* element = new SE_ColorEffectControllerElement(getURI());
	SE_2DNodeElement::clone(this, element);
	return element;
}
void SE_ColorEffectControllerElement::update(SE_ParamValueList& paramValueList)
{
    SE_StringID url = getURL();
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	mColorEffectController = resourceManager->getColorEffectController(url.getStr());
	mName = mColorEffectController->getID().getStr();
	mCurrentElement = NULL;
    clearChildren(); 
    spawn();
    layout();
    update(0);
}
void SE_ColorEffectControllerElement::update(const SE_AddressID& address, const SE_Value& value)
{
    SE_2DNodeElement::update(address, value);
}

void SE_ColorEffectControllerElement::update(const SE_TimeKey& key)
{
	std::vector<SE_Element*> children = getChildren();
	if(children.empty())
		return;
	std::vector<SE_Element*>::iterator it;
	SE_Element* first = NULL;
	SE_Element* second = NULL;
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	SE_SimObjectManager* simObjectManager = SE_Application::getInstance()->getSimObjectManager();
	SE_SpatialManager* spatialManager = SE_Application::getInstance()->getSpatialManager();
	std::vector<SE_Element*>::iterator currIt = children.end();
	for(it = children.begin() ; it != children.end() ; it++)
	{
		SE_Element* e = *it;
		if(e->getTimeKey() >= key)
		{
            currIt = it;
			break;
		}
	}
	if(currIt == children.end())
	{
		currIt--;
		first = *currIt;
		second = NULL;
	}
	else
	{
		if(currIt == children.begin())
		{
			if((*currIt)->getTimeKey() == key)
			{
				first = second = *currIt;
			}
			else
			{
			    first = NULL;
			    second = *currIt;
			}
		}
		else
		{
			second = *currIt;
			currIt--;
			first = second;
		}
	}
	if(first == NULL)
	{
		LOGI("... current key is less than element key");
	}
	else
	{
		if(mCurrentElement)
		{
			mCurrentElement->dismiss();
		}

		mCurrentElement = (SE_2DNodeElement*)first;
	}
}
SE_Spatial* SE_ColorEffectControllerElement::createSpatial()
{
	if(mCurrentElement)
	{
	    SE_Spatial* node = createNode();
        SE_Spatial* spatial = mCurrentElement->createSpatial();
        SE_SpatialManager* spatialManager = SE_Application::getInstance()->getSpatialManager();
		spatialManager->add(node, spatial);
		return node;
	}
	else
    {
        LOGI("### color effect controller < %s > return NULL spatial ####\n", getURL().getStr());
		return NULL;
    }
}
void SE_ColorEffectControllerElement::spawn()
{
	if(!mColorEffectController)
		return;
	SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
	std::vector<SE_TimeKey> keys = mColorEffectController->getKeys();
	for(int i = 0 ; i < keys.size() ; i++)
	{
		SE_ColorEffectFrame* f = mColorEffectController->getKeyFrame(keys[i]);
        SE_2DNodeElement* e = (SE_2DNodeElement*)f->createElement();
		e->setPivotX(f->getPivotX());
		e->setPivotY(f->getPivotY());
		e->setMountPointRef(f->getMountPointRef());
		elementManager->add(this->getID(), e, true);
		e->spawn();
	}
}
SE_ColorEffectControllerElement::~SE_ColorEffectControllerElement()
{}
void SE_ColorEffectControllerElement::layout()
{
	calculateRect(mColorEffectController->getPivotX(), mColorEffectController->getPivotY(), 0, 0);
	SE_2DNodeElement::layout();
}
///////////////
SE_ColorEffectElement::SE_ColorEffectElement()
{
    mBackgroundImageData = NULL;
	mBackgroundElement = NULL;
    mChannelImageData = NULL;
	mChannelElement = NULL;
	for(int i = 0 ; i < MARK_NUM ; i++)
    {
        mTextureImageData[i] = NULL;
		mTextureElement[i] = NULL;
    }
	mBackgroundArity = 0;
	mChannelArity = 0;
    mBackgroundAlphaValue = 255;
}
SE_ColorEffectElement::~SE_ColorEffectElement()
{
	if(mBackgroundElement)
		delete mBackgroundElement;
	if(mChannelElement)
		delete mChannelElement;
	for(int i = 0 ; i < MARK_NUM ; i++)
	{
		if(mTextureElement[i])
			delete mTextureElement[i];
	}

}
SE_Element* SE_ColorEffectElement::clone()
{
	SE_ColorEffectElement* element = new SE_ColorEffectElement;
	SE_2DNodeElement::clone(this, element);
	return element;
}
void SE_ColorEffectElement::update(const SE_AddressID& address, const SE_Value& value)
{
    SE_2DNodeElement::update(address, value);
}

void SE_ColorEffectElement::update(const SE_TimeKey& key)
{}
SE_ImageElement* SE_ColorEffectElement::createImageElement(const SE_StringID& textureURL, SE_ImageData*& imageData)
{
    SE_ImageElement* imageElement = new SE_ImageElement(textureURL);
	imageElement->spawn();
	imageElement->layout();
	imageData = createImageData(textureURL);
    imageData->setWidth(imageElement->getWidth());
	imageData->setHeight(imageElement->getHeight());
	SE_TextureTarget* renderTarget = new SE_TextureTarget(imageData);
	SE_RenderTargetManager* renderTargetManager = SE_Application::getInstance()->getRenderTargetManager();
	SE_RenderTargetID renderTargetID = renderTargetManager->add(renderTarget);
	SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
	imageElement->setRenderTargetID(renderTargetID);
    SE_CameraID cameraID = createRenderTargetCamera(imageElement->getDeltaLeft(), 
		                                            imageElement->getDeltaTop(), 
												    imageElement->getWidth(),
												    imageElement->getHeight());
	renderTarget->setCamera(cameraID);
	renderTarget->setCameraType(SE_RenderTarget::GLOBAL_CAMERA);
	renderTarget->create();
	return imageElement;
}
bool SE_ColorEffectElement::isTextureEnd(std::vector<SE_Element*>::iterator textureIt[4], SE_Element* texture[4])
{
	bool ret = false;
	for(int i = 0 ; i < MARK_NUM ; i++)
	{
		if(texture[i])
		{
			ret = textureIt[i] == texture[i]->getChildren().end();
			if(ret)
				break;
		}
	}
	return ret;
}
SE_Element* SE_ColorEffectElement::mergeElement(SE_Element* background, SE_Element* channel, 
												SE_Element* texture[4])
{
	SE_Element* element = NULL;
	SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
    std::vector<SE_Element*> backgroundChildren = background->getChildren();
    std::vector<SE_Element*> channelChildren = channel->getChildren();
    if(!backgroundChildren.empty() && !channelChildren.empty())
	{
		element = background->clone();
        std::vector<SE_Element*>::iterator backgroundIt = backgroundChildren.begin();
        std::vector<SE_Element*>::iterator channelIt = channelChildren.begin();
        std::vector<SE_Element*>::iterator textureIt[MARK_NUM];
		for(int i = 0 ; i < MARK_NUM ; i++)
		{
			if(texture[i])
				textureIt[i] = texture[i]->getChildren().begin();
		}
		while(backgroundIt != backgroundChildren.end() &&
			channelIt != channelChildren.end() && !isTextureEnd(textureIt, texture))
		{
			SE_Element* bc = *backgroundIt;
			SE_Element* cc = *channelIt;
			SE_Element* tc[MARK_NUM] = {NULL, NULL, NULL, NULL};
			for(int i = 0 ; i < MARK_NUM ; i++)
			{
				if(texture[i])
					tc[i] = *textureIt[i];
			}
			SE_Element* child = mergeElement(bc, cc, tc);
			elementManager->add(getID(), child, true);
			backgroundIt++;
			channelIt++;
			for(int i = 0 ; i < MARK_NUM ; i++)
			{
				if(texture[i])
					textureIt[i]++;
			}
		}
		SE_ASSERT(backgroundIt == backgroundChildren.end() && channelIt == channelChildren.end());
		for(int i = 0 ; i < MARK_NUM ; i++)
		{
			if(texture[i])
				SE_ASSERT(textureIt[i] == texture[i]->getChildren().end());
		}
		return element;
 	}
	else // has no child it must be SE_ImageElement
	{
		SE_ASSERT(backgroundChildren.empty() && channelChildren.empty());
		for(int i = 0 ; i < 4 ; i++)
		{
			if(texture[i])
				SE_ASSERT(texture[i]->getChildren().empty());
		}
        SE_StringID backgroundImageURI = background->getURI();
		SE_StringID channelImageURI = channel->getURI();
		SE_StringID textureURI[MARK_NUM];
		for(int i = 0 ; i < MARK_NUM ; i++)
		{
			if(texture[i])
				textureURI[i] = texture[i]->getURI();
		}
		SE_ColorEffectElement* retElement = (SE_ColorEffectElement*)background->clone();
		retElement->setBackgroundValue(backgroundImageURI);
		retElement->setChannelValue(channelImageURI);
		retElement->setBackgroundAlphaValue(mBackgroundAlphaValue);
		retElement->setBackgroundAlphaAddress(mBackgroundAlphaAddress);
		for(int i = 0 ; i < MARK_NUM ; i++)
		{
		    _TextureMark tm;
		    tm.mTextureValue = textureURI[i];
			tm.mColorAlphaAddress = mTextureMark[i].mColorAlphaAddress;
			tm.mColorAlphaValue = mTextureMark[i].mColorAlphaValue;
			tm.mFnAddress = mTextureMark[i].mFnAddress;
			tm.mFnValue = mTextureMark[i].mFnValue;
			tm.mTextureFnAddress = mTextureMark[i].mTextureFnAddress;
			tm.mTextureFnValue = mTextureMark[i].mTextureFnValue;
			tm.mColorAddress = mTextureMark[i].mColorAddress;
			tm.mColorValue= mTextureMark[i].mColorValue;
			retElement->setTextureMark(i, tm);
		}
		return retElement;
	}
}

void SE_ColorEffectElement::mergeElement()
{
    if(!mBackgroundElement)
		return;
	SE_Element* textureElement[4] = {mTextureImageElement[0], mTextureImageElement[1], mTextureImageElement[2],
	                                 mTextureImageElement[3]};
    mMergedElement = (SE_2DNodeElement*)mergeElement(mBackgroundImageElement, mChannelImageElement, textureElement);       
	//addChild(element);
}
void SE_ColorEffectElement::setSurface(SE_Surface* surface)
{
    SE_ColorEffectShaderProperty* cfp = new SE_ColorEffectShaderProperty;
	cfp->setBackgroundTexture(SE_TEXTURE0);
	cfp->setChannelTexture(SE_TEXTURE1);
	cfp->setBackgroundAlpha(mBackgroundAlphaValue / 255.0);
	for(int i = 0 ; i < MARK_NUM ; i++)
	{
        _TextureMark& tm = mTextureMark[i];
		if(tm.valid)
		{
			cfp->setHasMark(i, true);
		}
		else
			cfp->setHasMark(i, false);
		if(tm.mTextureValue.isValid())
		{
			cfp->setHasTex(i, true);
		}
		else
		{
			cfp->setHasTex(i, false);
		}
		cfp->setMarkAlpha(i, tm.mColorAlphaValue / 255.0);
		SE_Vector3i color;
		color.x = tm.mColorValue.data[0].value;
		color.y = tm.mColorValue.data[1].value;
		color.z = tm.mColorValue.data[2].value;
		cfp->setMarkColor(i, SE_Vector3f(color.x /255.0, color.y / 255.0, color.z / 255.0));
		cfp->setMarkFunction(i, tm.mFnValue);
	}
	surface->setShaderProperty(cfp);
	surface->setProgramDataID(COLOREFFECT_SHADER);
	surface->setRendererID(COLOREFFECT_RENDERER);
}
SE_Spatial* SE_ColorEffectElement::createSpatial()
{
	if(mBackgroundElement)
	{
        SE_Spatial* spatial = mMergedElement->createSpatial();
		return spatial;
	}
	else
	{
        return createSpatialByImage();
	}
}
void SE_ColorEffectElement::setImageData(SE_Primitive* primitive, SE_ImageData* imageData, SE_TEXUNIT_TYPE texType)
{
	SE_ImageDataPortion dp;
	dp.setX(0);
	dp.setY(0);
	dp.setWidth(imageData->getWidth());
	dp.setHeight(imageData->getHeight());
    primitive->setImageData(0, imageData, texType, dp);
}
void SE_ColorEffectElement::setImageData(SE_Primitive* primitive, const SE_StringID& imageID, SE_TEXUNIT_TYPE texType)
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	SE_ImageUnit iu = resourceManager->getImageUnit(imageID.getStr());
	SE_ImageData* imageData = resourceManager->getImageData(iu.imageDataID);
	SE_ImageDataPortion dp;
	dp.setX(iu.imageRect.x);
	dp.setY(iu.imageRect.y);
	dp.setWidth(iu.imageRect.width);
	dp.setHeight(iu.imageRect.height);
	primitive->setImageData(0, imageData, texType, dp);
}
void SE_ColorEffectElement::setImageData(SE_Primitive* primitive)
{
    if(mBackgroundArity > 1)
	{
		setImageData(primitive, mBackgroundImageData, SE_TEXTURE0);
	}
	else if(mBackgroundArity == 1)
	{
        setImageData(primitive, mBackgroundValue, SE_TEXTURE0);
	}
	if(mChannelArity > 1)
	{
		setImageData(primitive, mChannelImageData, SE_TEXTURE1);
	}
	else if(mChannelArity == 1)
	{
		setImageData(primitive, mChannelValue, SE_TEXTURE1);
	}
	int start = SE_TEXTURE2;
	for(int i = 0 ; i < MARK_NUM ; i++)
	{
        if(mTextureMark[i].mTextureArity > 1)
		{
			setImageData(primitive, mTextureImageData[i], (SE_TEXUNIT_TYPE)start);
		}
		else if(mTextureMark[i].mTextureArity == 1)
		{
			setImageData(primitive, mTextureMark[i].mTextureValue, (SE_TEXUNIT_TYPE)start);
		}
		start++;
	}
}
int SE_ColorEffectElement::uriToInt(const SE_StringID& uri)
{
	SE_ParamManager* paramManager = SE_Application::getInstance()->getParamManager();
	SE_URI intURI(uri.getStr());
	SE_StringID intURL = intURI.getURL();
	int ret = -1;
	if(SE_Util::isDigit(intURL.getStr()))
	{
		ret = atoi(intURL.getStr());
	}
	else
	{
        bool ok = false;
		int tmp = paramManager->getInt(intURL.getStr(), ok);
		if(ok)
			ret = tmp;
	}
	return ret;
}
void SE_ColorEffectElement::calculateValue()
{
	SE_URI background(mBackgroundAddress.getStr());
	mBackgroundValue = background.getURL();
	SE_URI channel(mChannelAddress.getStr());
	mChannelValue = channel.getURL();
    mBackgroundAlphaValue = uriToInt(mBackgroundAlphaAddress);
	for(int i = 0 ; i < MARK_NUM ; i++)
	{
        _TextureMark& tm = mTextureMark[i];
		if(tm.valid)
		{
		    SE_URI textureURI(tm.mTextureAddress.getStr());
		    tm.mTextureValue = textureURI.getURL();
		    tm.mFnValue = uriToInt(tm.mFnAddress);
		    tm.mColorAlphaValue = uriToInt(tm.mColorAlphaAddress);
		    SE_URI colorURI(tm.mColorAddress.getStr());
		    SE_StringID colorURL = colorURI.getURL();
		    tm.mColorValue = SE_Util::stringToSignColor(colorURL.getStr());
		    colorURI.setURI(tm.mColor2Address);
		    colorURL = colorURI.getURL();
		    tm.mColor2Value = SE_Util::stringToSignColor(colorURL.getStr());
		    tm.mTextureFnValue = uriToInt(tm.mTextureFnAddress);
		}
	}
}
SE_XMLTABLE_TYPE SE_ColorEffectElement::getBackgroundType()
{
	SE_ExtractImageStr backgroundExtractImage = SE_Util::stringToExtractImage(mBackgroundValue.getStr());
	if(backgroundExtractImage.base == "")
		return SE_IMAGE_TABLE;
	SE_Util::SplitStringList strList = SE_Util::splitString(backgroundExtractImage.base.c_str(), "/");
    SE_ResourceManager* resourceManager = SE_Application::getInstance() ->getResourceManager();
	SE_XMLTABLE_TYPE t = resourceManager->getXmlType(strList[0].c_str());
	return t;
}
void SE_ColorEffectElement::getExtractImageProperty(SE_XMLTABLE_TYPE& t, int& width, int& height)
{
    SE_ExtractImageStr backgroundExtractImage = SE_Util::stringToExtractImage(mBackgroundValue.getStr());
	SE_Util::SplitStringList strList = SE_Util::splitString(backgroundExtractImage.base.c_str(), "/");
	SE_ResourceManager* resourceManager = SE_Application::getInstance() ->getResourceManager();
	if(backgroundExtractImage.base == "")
	{
		t = SE_IMAGE_TABLE;
	}
	else
	{
	    t = resourceManager->getXmlType(strList[0].c_str());
	}
	std::string imageURL;
	if(backgroundExtractImage.base == "")
	{
		imageURL = backgroundExtractImage.red;
	}
	else
		imageURL = backgroundExtractImage.base;
	SE_ImageUnit iu = resourceManager->getImageUnit(imageURL.c_str());
	width = iu.imageRect.width;
	height = iu.imageRect.height;
	mBackgroundArity = backgroundExtractImage.getImageNum();
	SE_ExtractImageStr channelExtractImage = SE_Util::stringToExtractImage(mChannelValue.getStr());
	mChannelArity = channelExtractImage.getImageNum();
	for(int i = 0 ; i < MARK_NUM  ; i++)
	{
		_TextureMark& tm = mTextureMark[i];
		if(tm.valid)
		{
		    SE_ExtractImageStr textureExtractImage = SE_Util::stringToExtractImage(tm.mTextureValue.getStr());
		    tm.mTextureArity = textureExtractImage.getImageNum();
		}
	}
}
void SE_ColorEffectElement::layout()
{

    if(mBackgroundType == SE_IMAGE_TABLE)
    {
        calculateRect(mPivotX, mPivotY, mImageWidth, mImageHeight);
    }
    else if(mBackgroundType == SE_ELEMENT_TABLE)
    {
    	calculateRect(mPivotX, mPivotY, 0, 0);
    }
    //SE_2DNodeElement::layout();
	/*
	if(mBackgroundElement)
	{
		mBackgroundElement->layout();
		int width = mBackgroundElement->getWidth();
		int height = mBackgroundElement->getHeight();
		setWidth(width);
		setHeight(height);
	}
	else
	{
		LOGI("... color effect width = %f, height = %f", mWidth, mHeight);
	}
	if(mBackgroundArity > 1)
	{
	    mBackgroundImageDataID = mBackgroundValue.getStr();
        mBackgroundImageElement = createImageElement(mBackgroundValue, mBackgroundImageData);
	}
	if(mChannelArity > 1)
	{
		mChannelImageDataID = mChannelValue.getStr();
		mChannelImageElement = createImageElement(mChannelImageDataID, mChannelImageData);
	}
	for(int i = 0 ; i < MARK_NUM ; i++)
	{
		if(mTextureMark[i].mTextureArity > 1)
		{
			mTextureImageDataID[i] = mTextureMark[i].mTextureValue;
			mTextureImageElement[i] = createImageElement(mTextureImageDataID[i], mTextureImageData[i]);
		}
	}
	*/
}
void SE_ColorEffectElement::spawn()
{
	calculateValue();
	SE_XMLTABLE_TYPE t;
	int width = 0, height = 0;
	getExtractImageProperty(t, width, height);
	mBackgroundType = t;
    mImageWidth = width;
    mImageHeight = height;
	if(t == SE_ELEMENT_TABLE)
	{
	    SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
		if(mBackgroundElement)
			delete mBackgroundElement;
		SE_ElementSchema* es = resourceManager->getElementSchema(mBackgroundValue.getStr());
		mBackgroundElement = (SE_2DNodeElement*)es->createElement();
		mBackgroundElement->setRect(0, 0, 0, 0);
		mBackgroundElement->spawn();
		if(mChannelElement)
			delete mChannelElement;
		es = resourceManager->getElementSchema(mChannelValue.getStr());
		mChannelElement = (SE_2DNodeElement*)es->createElement();
        mChannelElement->setRect(0, 0, 0, 0);
		mChannelElement->spawn();
		for(int i = 0 ; i < MARK_NUM ; i++)
		{
			_TextureMark& tm = mTextureMark[i];
			if(tm.mTextureValue.isValid())
			{
			    if(mTextureElement[i])
				    delete mTextureElement[i];
				es = resourceManager->getElementSchema(tm.mTextureValue.getStr());
				mTextureElement[i] = (SE_2DNodeElement*)es->createElement();
				mTextureElement[i]->setRect(0, 0, 0, 0);
				mTextureElement[i]->spawn();
			}
		}
        mergeElement();
	}
}
