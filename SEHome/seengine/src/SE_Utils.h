#ifndef SE_UTILS_H
#define SE_UTILS_H

class SE_Util
{
public:
    static unsigned int host2NetInt32(unsigned int i);
    static unsigned short host2NetInt16(unsigned short s);
    static unsigned int net2HostInt32(unsigned int i);
    static unsigned short net2HostInt16(unsigned short s);
    static void sleep(unsigned int s);
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
};

#endif /** end SE_UTILS_H*/
