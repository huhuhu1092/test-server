#include "SE_Utils.h"
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
////////////////
static const char ws[] = " \t";
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
        pos = str.find(strSplit, start);
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