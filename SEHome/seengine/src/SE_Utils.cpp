#include "SE_Utils.h"
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
