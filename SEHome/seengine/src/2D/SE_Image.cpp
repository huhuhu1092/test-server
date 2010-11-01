#include "SE_Image.h"
#include "SE_Mesh.h"
#include "SE_Utils.h"
#include "SE_ImageMap.h"
enum STR_STATE {START, ERROR, REPLACE, READY_R, READY_G, READY_B, READY_A,R, G, B, A};
static const char REPLACESTART = '[';
static const char REPLACEEND = ']';
static const char RED = 'r';
static const char GREEN = 'g';
static const char BLUE = 'b';
static const char alpha = 'a';
static bool isDelimit(int c)
{
	if(c <= 32)
		return true;
	else 
		return false
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
void SE_Image::calculateDimension()
{
	if(mBaseColor)
}
int SE_Image::getWidth()
{}
int SE_Image::getHeight()
{}
void SE_Image::setSurface(SE_Surface* surface)
{}
void SE_Image::parse()
{
	std::string::iterator it;
	std::string base, red, green, blue, alpha;
	int currState = START;
	std::string currStr;
	for(it = mUrl.begin() ; it != mUrl.end() ; it++)
	{
        int c = *it;
        switch(currState)
		{
		case START:
			if(c == REPLACESTART)
			{
                currState = REPLACE;
				base = currStr;
			}
			else
			{
			    if(!isDelimit(c))
			    {
				    currStr = currStr + c;
			    }
			}
			break;
		case END:
			break;
		case ERROR:
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
						currState = START;
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
						currState = START;
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
						currState = START;
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
						currState = START;
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
						currState = START;
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
					currState = START;
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
					currState = START;
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
					currState = START;
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
					currState = START;
				}
				else
				{
					currStr += c;
				}
			}
			break;
		}
	}
	if(currState != START)
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
		LOGE("... image data ID path error \n");
		return SE_StringID::INVALID;
	}
	if(strList.size() == 2)
	{
		std::string str = mDefaultImageMapRef + inputstr;
		return SE_StringID(str.c_str());
	}
	return SE_StringID(inputstr);
}