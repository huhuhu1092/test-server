#ifndef SE_UTILS_H
#define SE_UTILS_H
#include <wchar.h>
class SE_Util
{
public:
    static unsigned int host2NetInt32(unsigned int i);
    static unsigned short host2NetInt16(unsigned short s);
    static unsigned int net2HostInt32(unsigned int i);
    static unsigned short net2HostInt16(unsigned short s);
    static void sleep(unsigned int s);
	static int findHighBit(int v);
	static int higherPower2(int v);
	static int lowerPower2(int v);
	static bool isPower2(int v);
    static wchar_t* utf8ToUnicode(const char* utf8str);
	/*
	template <class T>
	static T min(T a, T b)
	{
		return a > b ? b : a;
	}
	template <class T>
	static T max(T a, T b)
	{
		return a > b ? a : b;
	}
	*/
};

#endif /** end SE_UTILS_H*/
