#ifndef SE_UTILS_H
#define SE_UTILS_H
#include <wchar.h>
#include <string>
#include <vector>
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
};

#endif /** end SE_UTILS_H*/
