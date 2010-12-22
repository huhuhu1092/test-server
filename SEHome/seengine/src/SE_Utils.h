#ifndef SE_UTILS_H
#define SE_UTILS_H
#include <wchar.h>
#include <string>
#include <vector>
#include <list>
template <typename T>
typename std::list<T>::iterator listElementRef(std::list<T>& data, int index)
{
	typename std::list<T>::iterator it = data.begin();
	int i = 0;
	while(it != data.end() && i < index)
		it++;
	return it;
}
struct SE_DeleteObject
{
	template <typename T>
	void operator()(T* ptr) const
	{
		delete ptr;
	}
};
struct SE_SignColor
{
	enum {SIGN_NO, SIGN_PLUS, SIGN_MINUS};
	struct 
	{
	    int sign;
	    int value;
	} data[3];
};
struct SE_ExtractImageStr
{
	std::string base;
	std::string red;
	std::string green;
	std::string blue;
	std::string alpha;
	int getImageNum()
	{
		int num = 0;
		std::string pStr[5] = {&base, &red, &green, &blue, &alpha};
		for(int i = 0 ; i < 5 ; i++)
		{
			if(!pStr[i]->empty())
				num++;
		}
		return num;
	}
};
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
	typedef std::vector<std::string> SplitStringList;
    static SplitStringList splitString(const char* path, const char* split);
	static std::string trimLeft(const char* str);
	static std::string trimRight(const char* str);
	static std::string trim(const char* str);
	static std::string stringReplace(std::string& src, const std::string& beReplacedStr, const std::string& replaceStr);
	static SE_SignColor stringToSignColor(const std::string& str); 
	static SE_ExtractImageStr stringToExtractImage(const std::string& url);
};

#endif /** end SE_UTILS_H*/
