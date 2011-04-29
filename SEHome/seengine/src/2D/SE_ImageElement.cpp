#include "SE_ImageElement.h"
#include "SE_TimeKey.h"
#include "SE_Value.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_Spatial.h"
#include "SE_SpatialManager.h"
#include "SE_ShaderProperty.h"
#include "SE_Mesh.h"
#include "SE_DataValueDefine.h"
SE_ImageElement::SE_ImageElement(const SE_StringID& uri)
{
	setURI(uri);
	mImageUnits[0].imageUnit = &mBaseColor;
	mImageUnits[1].imageUnit = &mRChannel;
    mImageUnits[2].imageUnit = &mGChannel;
	mImageUnits[3].imageUnit = &mBChannel;
    mImageUnits[4].imageUnit = &mAChannel;
    mImageWidth = 0;
    mImageHeight = 0;
}
SE_Element* SE_ImageElement::clone()
{
	SE_ImageElement* element = new SE_ImageElement(getURI());
	SE_2DNodeElement::clone(this, element);
	return element;
}
SE_ImageElement::~SE_ImageElement()
{

}
void SE_ImageElement::initImage()
{
	SE_StringID url = getURL();
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	SE_ExtractImageStr imageStr = SE_Util::stringToExtractImage(url.getStr());
	if(imageStr.base != "")
	{
	    mBaseColor = resourceManager->getImageUnit(imageStr.base.c_str());
	}
	if(imageStr.red != "")
	{
	    mRChannel =  resourceManager->getImageUnit(imageStr.red.c_str());
	}
	if(imageStr.green != "")
	{
 	    mGChannel =  resourceManager->getImageUnit(imageStr.green.c_str());
	}
	if(imageStr.blue != "")
	{
	    mBChannel =  resourceManager->getImageUnit(imageStr.blue.c_str());
	}
	if(imageStr.alpha != "")
	{
	    mAChannel = resourceManager->getImageUnit(imageStr.alpha.c_str());
	}
}
void SE_ImageElement::update(SE_ParamValueList& paramValueList)
{
    spawn();
    SE_SpatialManager* spatialManager = SE_Application::getInstance()->getSpatialManager();
    SE_Spatial* spatial = spatialManager->get(getID());
	SE_Element* parent = getParent();
	if(!parent)
		return;
    SE_Spatial* parentSpatial = spatialManager->get(parent->getID());
    if(!spatial && !parentSpatial)
    {
        return ;
    }
    else if(spatial && !parentSpatial)
    {
        SE_ASSERT(0);
    }
    else if(!spatial && parentSpatial)
    {

        SE_Spatial* s = createSpatial();
        spatialManager->add(parentSpatial->getID(), s, true);
    }
    else if(spatial && parentSpatial)
    {
        SE_Spatial* spatial = spatialManager->remove(getID());
        SE_Spatial* s = createSpatial();
        spatialManager->add(parentSpatial->getID(), s, true);
		spatialManager->release(spatial);
    }
}
void SE_ImageElement::update(const SE_AddressID& address, const SE_Value& value)
{
    SE_2DNodeElement::update(address, value);
}

bool SE_ImageElement::isValid()
{
	int count = 0;
	for(int i = 0 ; i < IMG_SIZE ; i++)
	{
		_ImageUnitData* iuData = &mImageUnits[i];
		if(iuData->imageUnit->imageDataID.isValid())
			count++;
	}
	return count > 0;
}
void SE_ImageElement::setImageData(SE_Primitive* primitive)
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	int j = 0;
    for(int i = 0 ; i < IMG_SIZE ; i++)
	{
		SE_ImageUnit* imageUnit = mImageUnits[i].imageUnit;
		if(imageUnit->isValid())
		{
			SE_ImageData* imageData = resourceManager->getImageData(imageUnit->imageDataID.getStr());
		    if(!imageData)
		    {
			    imageData = resourceManager->loadImage(imageUnit->imageDataID.getStr(), false);
		    } 
            SE_ImageDataPortion dp;
		    dp.setX(imageUnit->imageRect.x);
		    dp.setY(imageUnit->imageRect.y);
		    dp.setWidth(imageUnit->imageRect.width);
		    dp.setHeight(imageUnit->imageRect.height);
		    primitive->setImageData(0, imageData, (SE_TEXUNIT_TYPE)j, dp);
			j++;
			mImageUnits[i].valid = 1;
		}
	}
}
static int getColorIndex(const std::string c)
{
	if(c == "r")
		return 0;
	else if(c == "g")
		return 1;
	else if(c == "b")
		return 2;
	else if(c == "a")
		return 3;
	return 0;
}
void SE_ImageElement::setSurface(SE_Surface* surface)
{
	static const int DEFAULT_COLOR = 0;
    static const int REPLACE_R = 1;
    static const int REPLACE_G = 2;
    static const int REPLACE_B = 3;
    static const int REPLACE_A = 4;

    static const int REPLACE_RG = 5;
    static const int REPLACE_RB = 6;
    static const int REPLACE_RA = 7;
    static const int REPLACE_GB = 8;
    static const int REPLACE_GA = 9;
    static const int REPLACE_BA = 10;

    static const int REPLACE_RGB = 11;
    static const int REPLACE_RGA = 12;
    static const int REPLACE_GBA = 13;
    static const int REPLACE_RBA = 14;
    static const int REPLACE_RGBA = 15;
    static const int NO_REPLACE = 16;
	int pattern[] = {0, REPLACE_A, REPLACE_B, REPLACE_BA, REPLACE_G, REPLACE_GA, 
	                 REPLACE_GB, REPLACE_GBA, REPLACE_R, REPLACE_RA, REPLACE_RB, REPLACE_RBA,
	                 REPLACE_RG, REPLACE_RGA, REPLACE_RGB, REPLACE_RGBA, NO_REPLACE};
	int index = 0;
	SE_ColorExtractShaderProperty* sp = new SE_ColorExtractShaderProperty;
	for(int i = 1 ; i < 5 ; i++)
	{
        index |= (mImageUnits[i].valid << (4 - i));
		if(mImageUnits[i].valid)
		{
			std::string ext = mImageUnits[i].imageUnit->ext.getStr();
			int c = getColorIndex(ext);
			sp->setColorChannelIndex(i - 1, c);
		}
	}
	int op = pattern[index];
	if(mBaseColor.isValid() && op == 0)
	{
		op = NO_REPLACE;
        delete sp;
        surface->setProgramDataID(DEFAULT_SHADER);
	    surface->setRendererID(DEFAULT_RENDERER);
	}
    else
    {    
	    sp->setColorOperationMode(op);
	    surface->setShaderProperty(sp);
        surface->setProgramDataID(COLOREXTRACT_SHADER);
	    surface->setRendererID(COLOREXTRACT_RENDERER);
    }
}
void SE_ImageElement::update(const SE_TimeKey& key)
{}
void SE_ImageElement::spawn()
{
	initImage();
	SE_ImageUnit iu = mBaseColor;
	float pivotx, pivoty, width, height;
	if(iu.imageDataID.isValid())
	{
		width = iu.imageRect.width;
		height = iu.imageRect.height;
		mPivotX = iu.imageRect.pivotx;
		mPivotY = iu.imageRect.pivoty;
	}
	else
	{
		iu = mRChannel;
		width = iu.imageRect.width;
		height = iu.imageRect.height;
		mPivotX = iu.imageRect.pivotx;
		mPivotY = iu.imageRect.pivoty;
	}
    mImageWidth = width;
    mImageHeight = height;
	SE_ASSERT(mImageWidth != 0 && mImageHeight != 0);
}
void SE_ImageElement::layout()
{
	//SE_2DNodeElement::layout();
    if(mFillType == SE_WRAP_CONTENT)
    {
	    mWidth = mImageWidth;
	    mHeight = mImageHeight;
    }
    else
    {
        SE_2DNodeElement* parent = (SE_2DNodeElement*)getParent();
        mPivotX = 0;
        mPivotY = 0;
        setMountPoint(0, 0);
		if(parent)
		{
            mWidth = parent->getWidth();
            mHeight = parent->getHeight();
		    if(mFillType == SE_TILE_PARENT)
		    {
		        mU = mWidth / mImageWidth;
		        mV = mHeight / mImageHeight;
		    }
		}
    }
	calculateRect(mPivotX, mPivotY, mWidth, mHeight);
}
SE_Spatial* SE_ImageElement::createSpatial()
{
	if(isValid())
	{
	    return createSpatialByImage();
	}
	else
		return NULL;

}
