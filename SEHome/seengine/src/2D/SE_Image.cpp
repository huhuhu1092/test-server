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
	}
	else
	{
		iu = mRChannel;//resourceManager->getImageUnit(mRChannel.imageDataID.getStr());
		mWidth = iu.imageRect.width;
		mHeight = iu.imageRect.height;
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
	for(int i = 1 ; i < 5 ; i++)
	{
        index |= (mImageUnits[i].valid << (4 - i));
		if(mImageUnits[i].valid)
		{
			std::string ext = mImageUnits[i].imageUnit->ext.getStr();
			int c = getColorIndex(ext);
			surface->setColorChannelIndex(i - 1, c);
		}
	}
	int op = pattern[index];
	if(mBaseColor.isValid() && op == 0)
	{
		op = NO_REPLACE;
	}
	surface->setColorOperation(op);
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