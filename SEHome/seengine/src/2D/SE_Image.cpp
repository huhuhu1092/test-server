#include "SE_Image.h"
#include "SE_Mesh.h"
#include "SE_Utils.h"
#include "SE_ImageMap.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_ImageTable.h"
#include "SE_ImageMap.h"
#include "SE_Log.h"
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
SE_Image::SE_Image(const char* url, const char* imagemapref)
{
	if(url)
	{
	    mUrl = url;
	}
	if(imagemapref)
	{
	    mDefaultImageMapRef = imagemapref;
	}
	parse();
	calculateDimension();
}
void getImageWidthHeight(SE_Util::SplitStringList& stringList, int& width, int& height)
{
	std::string fileName = stringList[0];
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	std::string filePath = std::string(resourceManager->getLayoutPath()) + SE_SEP + fileName;

}
void SE_Image::calculateDimension()
{
	SE_Util::SplitStringList strList = SE_Util::splitString(mBaseColor.getStr(), "/");
	if(strList.size() == 3)
	{
	}
	else
	{
		SE_Util::SplitStringList strList = SE_Util::splitString(mRChannel.getStr(), "/");
		if(strList.size() != 3)
		{
			LOGE("... image string error \n");
			return;
		}
	}
}
int SE_Image::getWidth()
{
	return 0;
}
int SE_Image::getHeight()
{
	return 0;
}
void SE_Image::setSurface(SE_Surface* surface)
{}
void SE_Image::parse()
{
	std::string::iterator it;
	std::string base, red, green, blue, alpha;
	int currState = _START;
	std::string currStr;
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
	mBaseColor = createImageDataFullPath(base.c_str());
	mRChannel =  createImageDataFullPath(red.c_str());
	mGChannel =  createImageDataFullPath(green.c_str());
	mBChannel =  createImageDataFullPath(blue.c_str());
	mAChannel = createImageDataFullPath(alpha.c_str());
}
SE_StringID SE_Image::createImageDataFullPath(const char* inputstr)
{
	SE_Util::SplitStringList strList = SE_Util::splitString(inputstr, "/");
	if(strList.size() < 2)
	{
		LOGI("... image data ID path error \n");
		return SE_StringID::INVALID;
	}
	if(strList.size() == 2)
	{
		std::string str = mDefaultImageMapRef + inputstr;
		return SE_StringID(str.c_str());
	}
	return SE_StringID(inputstr);
}