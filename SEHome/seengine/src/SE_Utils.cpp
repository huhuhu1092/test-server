#include "SE_Utils.h"
#include "SE_Common.h"
#include "SE_Log.h"
#include <string.h>
#include <list>
#if defined(WIN32)
    #include <winsock2.h>
#else
    #include <unistd.h>
    #if defined(ANDOIRD)
        #include <sys/endian.h>
    #else
        #include <netinet/in.h>
    #endif
#endif
#include "SE_Application.h"
#include "SE_ParamManager.h"
////////////////
static const char ws[] = " \t";
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
/////////////////
unsigned int SE_Util::host2NetInt32(unsigned int i)
{
	return htonl(i);
}
unsigned short SE_Util::host2NetInt16(unsigned short s)
{
	return htons(s);
}
unsigned int SE_Util::net2HostInt32(unsigned int i)
{
	return ntohl(i);
}
unsigned short SE_Util::net2HostInt16(unsigned short s)
{
	return ntohs(s);
}
void SE_Util::sleep(unsigned int s)
{
#if defined(WIN32)
    Sleep(s);
#else
    //sleep(s);
#endif
}
int SE_Util::findHighBit(int v)
{
	unsigned int bit = 0;
	while(v > 1)
	{
		bit++;
		v >>= 1;
	}
	return bit;
}
bool SE_Util::isPower2(int v)
{
	if(v >= 0)
	    return (v & (v - 1)) == 0;
	else
		return 0;
}
int SE_Util::higherPower2(int v)
{
    if(isPower2(v))
        return v;
	return 1 << (findHighBit(v) + 1);
}
int SE_Util::lowerPower2(int v)
{
	if(isPower2(v))
		return v;
	return 1 << (findHighBit(v));
}
static inline size_t utf8_char_len(unsigned char ch)
{
    return ((0xe5000000 >> ((ch >> 3) & 0x1e)) & 3) + 1;
}
#define UTF8_SHIFT_AND_MASK(unicode, byte)  (unicode)<<=6; (unicode) |= (0x3f & (byte));

static inline unsigned int utf8_to_utf32(const unsigned char *src, size_t length)
{
    unsigned int unicode;

    switch (length)
    {
        case 1:
            return src[0];
        case 2:
            unicode = src[0] & 0x1f;
            UTF8_SHIFT_AND_MASK(unicode, src[1])
            return unicode;
        case 3:
            unicode = src[0] & 0x0f;
            UTF8_SHIFT_AND_MASK(unicode, src[1])
            UTF8_SHIFT_AND_MASK(unicode, src[2])
            return unicode;
        case 4:
            unicode = src[0] & 0x07;
            UTF8_SHIFT_AND_MASK(unicode, src[1])
            UTF8_SHIFT_AND_MASK(unicode, src[2])
            UTF8_SHIFT_AND_MASK(unicode, src[3])
            return unicode;
        default:
            return 0xffff;
    }
    //printf("Char at %p: len=%d, utf-16=%p\n", src, length, (void*)result);
}
void utf8_to_utf16(const unsigned char *src, size_t srcLen,
                   wchar_t* dst, const size_t dstLen)
{
    const unsigned char* const end = src + srcLen;
    const wchar_t* const dstEnd = dst + dstLen;
    while (src < end && dst < dstEnd) {
        size_t len = utf8_char_len(*src);
        unsigned int codepoint = utf8_to_utf32((unsigned char*)src, len);

        // Convert the UTF32 codepoint to one or more UTF16 codepoints
        if (codepoint <= 0xFFFF) {
            // Single UTF16 character
            *dst++ = (wchar_t) codepoint;
        } else {
            // Multiple UTF16 characters with surrogates
            codepoint = codepoint - 0x10000;
            *dst++ = (wchar_t) ((codepoint >> 10) + 0xD800);
            *dst++ = (wchar_t) ((codepoint & 0x3FF) + 0xDC00);
        }

        src += len;
    }
    if (dst < dstEnd) {
        *dst = 0;
    }
}
wchar_t* SE_Util::utf8ToUnicode(const char* utf8str)
{
    if(!utf8str)
        return NULL;
    int len = strlen(utf8str);
    if(len == 0)
        return NULL;
    size_t chars = 0;
    const char* end = utf8str + len;
    const char* p = utf8str;
    while(p < end)
    {
        chars++;
        int utf8len = utf8_char_len(*p);
        unsigned int codepoint = utf8_to_utf32((const unsigned char*)p, utf8len);
        if(codepoint > 0xFFFF)
            chars++;
        p += utf8len;
    }
    size_t bufSize = (chars + 1) * sizeof(wchar_t);
    wchar_t* buf = new wchar_t[bufSize];
    if(buf)
    {
        p = utf8str;
        wchar_t* str = buf;
        utf8_to_utf16((const unsigned char*)p, len, str, bufSize);
    }
    return buf;
}
SE_Util::SplitStringList SE_Util::splitString(const char* path, const char* split)
{
    std::list<std::string> retList;
    std::vector<std::string> ret;
    if(!path)
        return ret;
    if(!split)
    {
        ret.resize(1);
        ret[0] = path;
        return ret;
    }
    std::string str = path;
    std::string strSplit = split;
    std::string::size_type pos = 0;
    std::string::size_type start = 0;
    while(pos < str.size())
    {
        pos = str.find_first_of(strSplit, start);
        if(pos != std::string::npos)
        {
            std::string::size_type n = pos - start;
            if(n > 0)
            {
                std::string subStr = str.substr(start, n);
                retList.push_back(subStr);
            }
            start = pos + 1;
        }
        else
        {
            std::string subStr = str.substr(start);
			if(subStr != "")
                retList.push_back(subStr);
            pos = str.size();
        }
    }
    if(retList.empty())
    {
        retList.push_back(path);
    }
    ret.resize(retList.size());
    std::list<std::string>::iterator it;
    int i = 0;
    for(it = retList.begin() ; it != retList.end() ;it++)
    {
        ret[i++] = *it;
    }
    return ret;
    
}
std::string SE_Util::trimLeft(const char* str)
{
	std::string inputstr(str);
	size_t found = inputstr.find_first_not_of(ws);
	if(found == std::string::npos)
	{
		return "";
	}
	return inputstr.substr(found);
}

std::string SE_Util::trimRight(const char* str)
{
	std::string inputstr(str);
	size_t found = inputstr.find_last_not_of(ws);
	if(found == std::string::npos)
		return "";
	else
	{
		inputstr.erase(found + 1);
		return inputstr;
	}
}
std::string SE_Util::trim(const char* str)
{
	std::string inputstr(str);
	std::string newstr = trimLeft(inputstr.c_str());
	newstr = trimRight(newstr.c_str());
	return newstr;
}
std::string SE_Util::stringReplace(std::string& src, const std::string& beReplacedStr, const std::string& replaceStr)
{
	std::string::size_type index = 0; 
	std::string::size_type beginIndex = 0;
	while((index = src.find_first_of(beReplacedStr, beginIndex)) != std::string::npos)
	{
		src.replace(index, beReplacedStr.size(), replaceStr);
		beginIndex = index + 1;
	}
	return src;
}
SE_SignColor SE_Util::stringToSignColor(const std::string& str)
{
	SE_SignColor color;
	SE_Util::SplitStringList strList = SE_Util::splitString(str.c_str(), " \t");
	SE_ASSERT(strList.size() == 3);
    std::string signstr = "+-";
	for(int i = 0 ; i < strList.size() ; i++)
    {
        std::string str = strList[i];
        std::string::size_type n = str.find_first_of(signstr, 0);
        std::string numstr;
		if(n == std::string::npos)
        {
			color.data[i].sign = SE_SignColor::SIGN_NO;
			color.data[i].value = atoi(str.c_str());
        }
        else if(str[n] == '+')
        {
            color.data[i].sign = SE_SignColor::SIGN_PLUS;
            numstr = str.substr(1);
		    color.data[i].value = atoi(numstr.c_str());
        }
        else if(str[n] == '-')
        {
            color.data[i].sign = SE_SignColor::SIGN_MINUS;
            numstr = str.substr(1);
			color.data[i].value = atoi(numstr.c_str());
        }
    }
	return color;
}
SE_ExtractImageStr SE_Util::stringToExtractImage(const std::string& url)
{
	std::string::const_iterator it;
	std::string base, red, green, blue, alpha;
	int currState = _START;
	std::string currStr;
	bool hasReplace = false;
	for(it = url.begin() ; it != url.end() ; it++)
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
	SE_ExtractImageStr image;
	image.base = base;
	image.red = red;
	image.green = green;
	image.blue = blue;
	image.alpha = alpha;
	return image;
}
bool SE_Util::isDigit(const char* str)
{
	std::string value = str;
	std::string::iterator it;
	bool bDigit = true;
	for(it = value.begin() ; it != value.end() ; it++)
	{
		switch(*it)
		{
		case '0':
			break;
		case '1':
			break;
		case '2':
			break;
		case '3':
			break;
		case '4':
			break;
		case '5':
			break;
		case '6':
			break;
		case '7':
			break;
		case '8':
			break;
		case '9':
			break;
		case '.':
			break;
		default:
			bDigit = false;
			break;
		}
	}
	return bDigit;
}
SE_Util::SplitStringList SE_Util::extractParamString(const char* str)
{
	const int START_PARAM = 0;
	const int END_PARAM = 1;
	const int ERROR_PARAM = 2;
	int state = END_PARAM;
	std::string paramString = str;
	std::string::size_type pos;
	std::string::size_type startParam = std::string::npos;
	std::string::size_type endParam = std::string::npos;
	std::list<std::string> paramList;
	std::vector<std::string> paramVector;
	for(pos = 0 ; pos < paramString.size() ; pos++)
	{
		if(paramString[pos] == '[')
		{
			if(state != END_PARAM)
			{
				state = ERROR_PARAM;
			}
			else
			{
                state = START_PARAM;
			    startParam = pos;
			}
		}
		else if(paramString[pos] == ']')
		{
			if(state == START_PARAM)
			{
				endParam = pos;
				state = END_PARAM;
				std::string::size_type n = endParam - (startParam + 1);
		        std::string subString = paramString.substr(startParam + 1, n);
				paramList.push_back(subString);
			}
			else
			{
				LOGI("... has no [ before ]\n");
				state = ERROR_PARAM;
			}
		}
		else
		{
		}
		if(state == ERROR_PARAM)
		{
			break;
		}
	}
	if(state == END_PARAM)
	{
		paramVector.resize(paramList.size());
		copy(paramList.begin(), paramList.end(), paramVector.begin());
	}
	return paramVector;
}
static std::string getParamValue(const std::string& str, std::string::size_type startpos, std::string::size_type endpos)
{
	if(startpos != std::string::npos && endpos != std::string::npos)
	{
		std::string::size_type n = endpos - (startpos + 1);
		std::string subString = str.substr(startpos + 1, n);
		SE_ParamManager* paramManager = SE_Application::getInstance()->getParamManager();
	    bool ok = false;
		std::string value = paramManager->getString(subString.c_str(), ok);
	    if(ok)
	    {
			return value;
	    }
		else
			return BAD_STR;
	}
	else if(startpos == std::string::npos && endpos == std::string::npos)
	{
		return str;
	}
	else
	{
		return BAD_STR;
	}
}
std::string SE_Util::resolveParamString(const char* str)
{
	//SE_Util::SplitStringList strList = SE_Util::splitString(str, "/");
	//SE_Util::SplitStringList::iterator it;
	const int START_PARAM = 0;
	const int END_PARAM = 1;
	const int ERROR_PARAM = 2;
	int state = END_PARAM;
	std::string paramString = str;
	std::string::size_type pos;
	std::string::size_type startParam = std::string::npos;
	std::string::size_type endParam = std::string::npos;
	for(pos = 0 ; pos < paramString.size() ; pos++)
	{
		if(paramString[pos] == '[')
		{
            state = START_PARAM;
			startParam = pos;
		}
		else if(paramString[pos] == ']')
		{
			if(state == START_PARAM)
			{
				endParam = pos;
				state = END_PARAM;
				std::string value = getParamValue(paramString, startParam, endParam);
				std::string::size_type n = endParam - startParam + 1;
				paramString.replace(startParam, n, value);
			}
			else
			{
				LOGI("... has no [ before ]\n");
				state = ERROR_PARAM;
				break;
			}
		}
		else
		{
		}
	}
    if(state != END_PARAM)
		return BAD_STR;
	else
		return paramString;
}
std::string SE_Util::intToString(int i)
{
    char buf[10];
	memset(buf, 0, 10);
#if defined(WIN32)
	_snprintf(buf, 9, "%d", i);
#else
	snprintf(buf, 9, "%d", i);
#endif
	std::string str = buf;
	return str;
}
std::string SE_Util::floatToString(float f)
{
    char buf[20];
	memset(buf, 0, 20);
#if defined(WIN32)
	_snprintf(buf, 19, "%f", f);
#else
	snprintf(buf, 19, "%f", f);
#endif
	std::string str = buf;
	return str;
}
SE_CharCode SE_Util::stringToCharCode(const char* s, int len)
{
    enum {SE_UNICODE_MODE, SE_ASCII_MODE};
    if(s == NULL || len == 0)
        return SE_CharCode();
    return SE_CharCode();
}
static int utf32_at_internal(const char* cur, size_t *num_read)
{
    const char first_char = *cur;
    if ((first_char & 0x80) == 0) { // ASCII
        *num_read = 1;
        return *cur;
    }
    cur++;
    unsigned int mask, to_ignore_mask;
    size_t num_to_read = 0;
    unsigned int utf32 = first_char;
    for (num_to_read = 1, mask = 0x40, to_ignore_mask = 0xFFFFFF80;
         (first_char & mask);
         num_to_read++, to_ignore_mask |= mask, mask >>= 1) {
        // 0x3F == 00111111
        utf32 = (utf32 << 6) + (*cur++ & 0x3F);
    }
    to_ignore_mask |= mask;
    utf32 &= ~(to_ignore_mask << (6 * (num_to_read - 1)));

    *num_read = num_to_read;
    return static_cast<int>(utf32);
}
size_t SE_Util::getUtf32LenFromUtf8(const char* src, size_t src_len)
{
    if (src == NULL || src_len == 0) {
        return 0;
    }
    size_t ret = 0;
    const char* cur;
    const char* end;
    size_t num_to_skip;
    for (cur = src, end = src + src_len, num_to_skip = 1;
         cur < end;
         cur += num_to_skip, ret++) {
        const char first_char = *cur;
        num_to_skip = 1;
        if ((first_char & 0x80) == 0) {  // ASCII
            continue;
        }
        int mask;

        for (mask = 0x40; (first_char & mask); num_to_skip++, mask >>= 1) {
        }
    }
    return ret;    
}
size_t SE_Util::utf8ToUtf32(const char* src, size_t src_len, unsigned int* dst, size_t dst_len)
{
    if (src == NULL || src_len == 0 || dst == NULL || dst_len == 0) {
        return 0;
    }

    const char* cur = src;
    const char* end = src + src_len;
    unsigned int* cur_utf32 = dst;
    const unsigned int* end_utf32 = dst + dst_len;
    while (cur_utf32 < end_utf32 && cur < end) {
        size_t num_read;
        *cur_utf32++ =
                static_cast<unsigned int>(utf32_at_internal(cur, &num_read));
        cur += num_read;
    }
    if (cur_utf32 < end_utf32) {
        *cur_utf32 = 0;
    }
    return static_cast<size_t>(cur_utf32 - dst);    
}
