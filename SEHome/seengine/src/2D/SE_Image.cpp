#include "SE_Image.h"
#include "SE_Mesh.h"
#include "SE_Utils.h"
#include "SE_ImageMap.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_ImageMap.h"
#include "SE_Primitive.h"
#include "SE_Log.h"
#include "SE_Common.h"
#include "SE_ImageData.h"
#include "SE_ShaderProperty.h"
#include "SE_Element.h"
#include "SE_RenderTarget.h"
#include "SE_RenderTargetManager.h"
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
enum _STR_STATE {_START, _ERROR, REPLACE, READY_R, READY_G, READY_B, READY_A,R, G, B, A};
static const char REPLACESTART = '[';
static const char REPLACEEND = ']';
static const char RED = 'r';
static const char GREEN = 'g';
static const char BLUE = 'b';
static const char ALPHA = 'a';
static bool isDelimit(int c)
{
	if(c <= 32)
		return true;
	else 
		return false;
}
SE_Image::SE_Image()
{
}
SE_Image::SE_Image(const char* url)
{
	if(url)
	{
	    mUrl = url;
	}
	mImageUnits[0].imageUnit = &mBaseColor;
	mImageUnits[1].imageUnit = &mRChannel;
    mImageUnits[2].imageUnit = &mGChannel;
	mImageUnits[3].imageUnit = &mBChannel;
    mImageUnits[4].imageUnit = &mAChannel;

	mWidth = mHeight = 0;
	parse();
	calculateDimension();
}
void SE_Image::calculateDimension()
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	SE_ImageUnit iu = mBaseColor;//resourceManager->getImageUnit(mBaseColor.imageDataID.getStr());
	if(iu.imageDataID.isValid())
	{
		mWidth = iu.imageRect.width;
		mHeight = iu.imageRect.height;
		mPivotx = iu.imageRect.pivotx;
		mPivoty = iu.imageRect.pivoty;
	}
	else
	{
		iu = mRChannel;//resourceManager->getImageUnit(mRChannel.imageDataID.getStr());
		mWidth = iu.imageRect.width;
		mHeight = iu.imageRect.height;
		mPivotx = iu.imageRect.pivotx;
		mPivoty = iu.imageRect.pivoty;
	}
}
int SE_Image::getWidth()
{
	return mWidth;
}
int SE_Image::getHeight()
{
	return mHeight;
}
SE_Image* SE_Image::clone()
{
	SE_Image* img = new SE_Image;
	img->mUrl = mUrl;
	img->mAChannel = mAChannel;
	img->mBaseColor = mBaseColor;
	img->mBChannel = mBChannel;
	img->mGChannel = mGChannel;
	img->mRChannel = mRChannel;
	img->mImageUnits[0].imageUnit = &img->mBaseColor;
	img->mImageUnits[1].imageUnit = &img->mRChannel;
    img->mImageUnits[2].imageUnit = &img->mGChannel;
	img->mImageUnits[3].imageUnit = &img->mBChannel;
    img->mImageUnits[4].imageUnit = &img->mAChannel;
	for(int i = 0 ; i < IMG_SIZE ; i++)
	{
	    img->mImageUnits[i].valid = mImageUnits[i].valid;
	}
	img->mWidth = mWidth;
	img->mHeight = mHeight;
	img->mPivotx = mPivotx;
	img->mPivoty = mPivoty;
	return img;


}
SE_ImageData* SE_Image::getImageData(const SE_ImageUnit& imageUnit)
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	SE_ImageData* imageData = NULL;
	if(imageUnit.isValid())
	{
		imageData = resourceManager->getImageData(imageUnit.imageDataID.getStr());
		if(!imageData)
		{
			imageData = resourceManager->loadImage(imageUnit.imageDataID.getStr());
		}
	}
	return imageData;
}
void SE_Image::setImageData(SE_RectPrimitive* primitive)
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	int j = 0;
    for(int i = 0 ; i < 5 ; i++)
	{
		SE_ImageUnit* imageUnit = mImageUnits[i].imageUnit;
		if(imageUnit->isValid())
		{
			SE_ImageData* imageData = resourceManager->getImageData(imageUnit->imageDataID.getStr());
		    if(!imageData)
		    {
			    imageData = resourceManager->loadImage(imageUnit->imageDataID.getStr());
		    } 
            SE_ImageDataPortion dp;
		    dp.setX(imageUnit->imageRect.x);
		    dp.setY(imageUnit->imageRect.y);
		    dp.setWidth(imageUnit->imageRect.width);
		    dp.setHeight(imageUnit->imageRect.height);
		    primitive->setImageData(imageData, (SE_TEXUNIT_TYPE)j, NOT_OWN, dp);
			j++;
			mImageUnits[i].valid = 1;
		}
	}

}
static int getColorIndex(std::string c)
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
void SE_Image::setSurface(SE_Surface* surface)
{
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
	}
	sp->setColorOperationMode(op);
	surface->setShaderProperty(sp);
}
int SE_Image::getValidImageNum()
{
	int count = 0;
	for(int i = 0 ; i < IMG_SIZE ; i++)
	{
		_ImageUnitData* iuData = &mImageUnits[i];
		if(iuData->imageUnit->imageDataID.isValid())
			count++;
	}
	return count;
}
void SE_Image::parse()
{
	std::string::iterator it;
	std::string base, red, green, blue, alpha;
	int currState = _START;
	std::string currStr;
	bool hasReplace = false;
	for(it = mUrl.begin() ; it != mUrl.end() ; it++)
	{
        int c = *it;
        switch(currState)
		{
		case _START:
			if(c == REPLACESTART)
			{
                currState = REPLACE;
				base = currStr;
				hasReplace = true;
			}
			else
			{
			    if(!isDelimit(c))
			    {
				    currStr += c;
			    }
			}
			break;
		case REPLACE:
			if(c == RED || c == GREEN || c == BLUE || c == ALPHA)
			{
			    switch(c)
				{
				case RED:
					currState = R;
					break;
				case GREEN:
					currState = G;
					break;
				case BLUE:
					currState = B;
					break;
				case ALPHA:
					currState = A;
					break;
				}
			}
			else
			{
				if(!isDelimit(c))
				{
					if(c == REPLACEEND)
					{
						currState = _START;
					}
					else
					{
						LOGE("... error : first replace should be : r, g, b, a\n");
					}
				}
			}
			break;
		case R:
			if(c == '=')
			{
				currState = READY_R;
				currStr = "";
			}
			else
			{
				if(!isDelimit(c))
				{
				    if(c == REPLACEEND)
				    {
						currState = _START;
				    }
					else
					{
						LOGE("... error : in [] it should has = \n");
					}
				}
			}
			break;
		case G:
			if(c == '=')
			{
				currState = READY_G;
				currStr = "";
			}
			else
			{
				if(!isDelimit(c))
				{
				    if(c == REPLACEEND)
				    {
						currState = _START;
				    }
					else
					{
						LOGE("... error : in [] it should has = \n");
					}
				}
			}
			break;
		case B:
			if(c == '=')
			{
				currState = READY_B;
				currStr = "";
			}
			else
			{
				if(!isDelimit(c))
				{
				    if(c == REPLACEEND)
				    {
						currState = _START;
				    }
					else
					{
						LOGE("... error : in [] it should has = \n");
					}
				}
			}
			break;
		case A:
			if(c == '=')
			{
				currState = READY_A;
				currStr = "";
			}
			else
			{
				if(!isDelimit(c))
				{
				    if(c == REPLACEEND)
				    {
						currState = _START;
				    }
					else
					{
						LOGE("... error : in [] it should has = \n");
					}
				}
			}
			break;
		case READY_R:
			if(!isDelimit(c))
			{
				if(c == REPLACEEND)
				{
					red = currStr;
					currState = _START;
				}
				else
				{
					currStr += c;
				}
			}
			break;
		case READY_G:
			if(!isDelimit(c))
			{
				if(c == REPLACEEND)
				{
					green = currStr;
					currState = _START;
				}
				else
				{
					currStr += c;
				}
			}
			break;
		case READY_B:
			if(!isDelimit(c))
			{
				if(c == REPLACEEND)
				{
					blue = currStr;
					currState = _START;
				}
				else
				{
					currStr += c;
				}
			}
			break;
		case READY_A:
			if(!isDelimit(c))
			{
				if(c == REPLACEEND)
				{
					alpha = currStr;
					currState = _START;
				}
				else
				{
					currStr += c;
				}
			}
			break;
		}
	}
	if(currState != _START)
	{
		LOGE("... string input has error. please check\n");
	}
	if(!hasReplace)
	{
		base = currStr;
	}
	mBaseColor = createImageDataFullPath(base.c_str());
	mRChannel =  createImageDataFullPath(red.c_str());
	mGChannel =  createImageDataFullPath(green.c_str());
	mBChannel =  createImageDataFullPath(blue.c_str());
	mAChannel = createImageDataFullPath(alpha.c_str());
}
SE_ImageUnit SE_Image::createImageDataFullPath(const char* inputstr)
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	return resourceManager->getImageUnit(inputstr);
}
////////////////////////////
SE_ColorEffectImage::SE_ColorEffectImage()
{
	mBackground = NULL;
	mChannel = NULL;
	for(int i = 0 ; i < TEX_SIZE ; i++)
	{
		mTexture[i] = NULL;
		mFunction[i] = -1;
		mAlpha[i] = -1;
		mColor[i].x = -1;
		mColor[i].y = -1;
		mColor[i].z = -1;
	}
}
SE_ColorEffectImage::~SE_ColorEffectImage()
{
	if(mBackground)
		delete mBackground;
	if(mChannel)
		delete mChannel;
	for(int i = 0 ; i < TEX_SIZE ; i++)
	{
		if(mTexture[i])
			delete mTexture[i];
	}
}
void SE_ColorEffectImage::setBackground(const SE_StringID& background)
{
	if(mBackground)
	{
		delete mBackground;
		mBackground = NULL;
	}
	mBackground = new SE_Image(background.getStr());
}
void SE_ColorEffectImage::setChannel(const SE_StringID& channel)
{
	if(mChannel)
	{
		delete mChannel;
		mChannel = NULL;
	}
	mChannel = new SE_Image(channel.getStr());
}
void SE_ColorEffectImage::setTexture(int index, const SE_StringID& texture)
{
	SE_Util::SplitStringList strList = SE_Util::splitString(texture.getStr(), "/");
	if(strList.size() == 2)// this is an action
	{
	}
	else if(strList.size() >= 3) // this is an ordinary image
	{
		mTexture[index] = new SE_Image(texture.getStr());
	}
}
int SE_ColorEffectImage::getValidImageNum()
{
    int count = 0;
	if(mBackground)
	{
	    count += mBackground->getValidImageNum();
	}
	if(mChannel)
	{
		count += mChannel->getValidImageNum();
	}
	for(int i = 0 ; i < TEX_SIZE ; i++)
	{
		if(mTexture[i])
		{
			count += mTexture[i]->getValidImageNum();
		}
	}
	return count;
}
SE_ImageData* SE_ColorEffectImage::createTextureElement(SE_Element* parent, SE_Image* img)
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	SE_ImageData* imgData = resourceManager->getImageData(img->getUrl().c_str());
	if(!imgData)
	{
		SE_ImageElement* e = new SE_ImageElement(img);
	    parent->addChild(e);
	    e->spawn();
		imgData = new SE_ImageData;
		SE_RenderTargetManager* renderTargetManager = SE_Application::getInstance()->getRenderTargetManager();
	    resourceManager->setImageData(img->getUrl().c_str(), imgData);
		SE_RenderTarget* renderTarget = new SE_TextureTarget(imgData);
	    SE_RenderTargetID renderTargetID = renderTargetManager->addRenderTarget(renderTarget);
	    e->setRenderTarget(renderTargetID);
	}
	return imgData;
}
void SE_ColorEffectImage::createTextureElement(SE_Element* parent)
{
	int backgroundValidImage = mBackground->getValidImageNum();
	SE_RenderTargetManager* renderTargetManager = SE_Application::getInstance()->getRenderTargetManager();
	if(backgroundValidImage > 1)
	{
        SE_Image* img = mBackground->clone();
        mBackgroundImageData = createTextureElement(parent, img);
	}
	int channelValidImage = mChannel->getValidImageNum();
	if(channelValidImage > 1)
	{
		SE_Image* img = mChannel->clone();
		mChannelImageData = createTextureElement(parent, img);
	}
	for(int i = 0 ; i < TEX_SIZE ; i++)
	{
		SE_Image* img = mTexture[i];
		if(img && img->getValidImageNum() > 1)
		{
			mTextureImageData[i] = createTextureElement(parent, img);
		}
	}
}
void SE_ColorEffectImage::setImageData(SE_RectPrimitive* primitive, SE_TEXUNIT_TYPE texType, SE_ImageData* imageData, SE_Image* image)
{
    if(imageData)
	{
		SE_ImageDataPortion dp;
		dp.setX(0);
		dp.setY(0);
		dp.setWidth(imageData->getWidth());
		dp.setHeight(imageData->getHeight());
        primitive->setImageData(imageData, SE_TEXTURE0, NOT_OWN, dp);
	}
	else
	{
		SE_ImageData* imageDataTmp = getImageData(img->getBaseColor());
		SE_ImageDataPortion dp;
		dp.setX(img->getBaseColor().imageRect.x);
		dp.setY(img->getBaseColor().imageRect.y);
		dp.setWidth(img->getBaseColor().imageRect.width);
		dp.setHeight(img->getBaseColor().imageRect.height);
		primitive->setImageData(imageDataTmp, SE_TEXTURE0, NOT_OWN, dp);
	}
}
void SE_ColorEffectImage::setImageData(SE_RectPrimitive* primivite)
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    setImageData(primitive, SE_TEXTURE0, mBackgroundImageData, mBackground);
	setImageData(primitive, SE_TEXTURE1, mChannelImageData, mChannelImage);
	SE_TEXUNIT_TYPE start = SE_TEXTURE2;
    for(int i = 0 ; i < TEX_SIZE ; i++)
	{
		SE_Image* image = mTexture[i];
		SE_ImageData* imageData = mTextureImageData[i];
		if(image)
		{
            setImageData(primitive, start, imageData, image);
			start = (SE_TEXUNIT_TYPE)((int)start + 1);
		}
	}
}
void SE_ColorEffectImage::setSurface(SE_Surface* surface)
{

}