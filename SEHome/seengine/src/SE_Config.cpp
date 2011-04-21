#include "SE_Config.h"
#include "SE_IO.h"
SE_Config::SE_Config(const char* fileName)
{
	char* data = NULL;
	int dataLen = 0;
	SE_IO::readFileAll(fileName, data, dataLen);
	if(data)
	{
		parse(data, dataLen);
	}
	delete[] data;
}
static bool isSkipChar(char c)
{
	if(c == ' ' || c == '\t' || c == '\r')
		return true;
}
char* SE_Config::readLine(char* data, int& pos)
{
	int start = pos;
	while(data[pos] != '\n' && data[pos] != '\0')
		pos++;
	data[pos] = '\0';
	return &data[start];
}
template <typename T, typename STRING_TO_T>
static void stringListToFloatArray(SE_Util::SplitStringList& strList, T*& out, int& len, STRING_TO_T stringToTFun)
{
	len = strList.size() - 1;
	out = new T[len];
	if(out == NULL)
	{
		len = 0;
		return;
	}
	for(int i = 1 , j = 0; i < strList.size() ; i++, j++)
	{
		out[j] = stringToTFun(strList[i].c_str());
	}
}

static std::string composeString(SE_Util::SplitString& strList, int start)
{
	std::string str;
	for(int i = start ; i < strList.size() ; i++)
	{
		str += strList[i];
		str += " ";
	}
	return SE_Util::trim(str);
}
void SE_Config::handleLine(const std::string& line)
{
	std::string lineData = SE_Util::trim(line.c_str());
	if(lineData.size() == 0)
		return;
	if(lineData == "")
		return;
	if(lineData.size() > 0 && lineData[0] == "#")
		return;
	SE_Util::SplitStringList data = SE_Util::splitStringRaw(lineData.c_str(), "=");
	SE_ASSERT(data.size() == 2);
	std::string left = data[0];
	std::string right = data[1];
	left = SE_Util::trim(left);
	right = SE_Util::trim(right);
	SE_Util::SplitStringList rightValue = SE_Util::splitStringRaw(right.c_str(), " \t");
	SE_ASSERT(rightValue.size() > 1);
	std::string rightValueType = rightValue[0];
	if(rightValueType == "int")
	{
		if(rightValue.size() == 2)
		{
		    int i = SE_Util::stringToInt(rightValue[1]);
		    SE_Value v;
		    v.setInt(i);
		    mDataMap[left] = v;
		}
	}
	else if(rightValueType == "float")
	{
		if(rightValue.size() == 2)
		{
		    float f = SE_Util::stringToFloat(rightValue[1]);
		    SE_Value v;
		    v.setFloat(f);
		    mDataMap[left] = v;
		}
		
	}
	else if(rightValueType == "vec2f")
	{
		if(rightValue.size() == 3)
		{
		    float* fa = NULL;
		    int len = 0;
		    stringListToTArray(rightValue, fa, len, SE_Util::stringToFloat);
			SE_Vector2f vf;
		    vf.x = fa[0];
			vf.y = fa[1];
			SE_Value v;
			v.setVector2f(vf);
			mDataMap[left] = v;
		}
	}
	else if(rightValueType == "vec3f")
	{
		if(rightValue.size() == 4)
		{
			float* fa = NULL;
			int len = 0;
			stringListToTArray(rightValue, fa, len, SE_Util::stringToFloat);
			SE_Vector3f f;
			f.x = fa[0];
			f.y = fa[1];
			f.z = fa[2];
			SE_Value v;
			v.setVector3f(f);
			mDataMap[left] = v;
		}
	}
    else if(rightValueType == "vec4f")
	{
		if(rightValue.size() == 5)
		{
  		    float* fa = NULL;
		    int len = 0;
			stringListToTArray(rightValue, fa, len, SE_Util::stringToFloat);	
			SE_Vector4f f;
			f.x = fa[0];
			f.y = fa[1];
			f.z = fa[2];
			f.w = fa[3];
			SE_Value v;
			v.setVector4f(f);
			mDataMap[left] = v;
		}
	}
	else if(rightValueType == "vec3i")
	{
		if(rightValue.size() == 4)
		{
		    int* ia = NULL;
		    int len = 0;
		    stringListToTArray(rightValue, ia, len, SE_Util::stringToInt);
			SE_Vector3i f;
			f.x = ia[0];
			f.y = ia[1];
			f.z = ia[2];
			SE_Vaclue v;
			v.setVector3i(f);
			mDataMap[left] = v;
		}
	}
	else if(rightValueType == "string")
	{
		SE_Value v;
		std::string str;
		for(int i = 1 ; i < rightValue.size() ; i++)
		{
			str += rightValue[i];
			str += " ";
		}
		str = SE_Util::trim(str);
		if(str == "")
		{
			LOGI("## error : string value is NULL ##\n");
		}
		else
		{
			v.setAscii(str.c_str());
			mDataMap[left] = v;
		}
	}
	else
	{
		LOGI("## error: confige file has not this value type : %s ##\n", rightValueType.c_str());
	}
}
void SE_Config::parse(char* data, int dataLen)
{
	/*
    int pos = 0;
	while(pos < dataLen)
	{
		char* line = readLine(data, pos);
		SE_ASSERT(pos < dataLen);
		handleLine(line);
		}
	*/
	std::string str(data, dataLen);
	SE_Util::SplitStringList lineList = SE_Util::splitStringRaw(str.c_str(), "\n");
	SE_Util::SplitStringList::iterator it;
	for(it = lineList.begin() ; it != lineList.end() ; it++)
	{
		std::string line = *it;
		handleLine(line);
	}
}
int SE_Config::getInt(const char* id, int defaultValue)
{}
std::string SE_Config::getString(const char* id, const std::string& defaultValue)
{}
float SE_Config::getFloat(const char* id, float defaultValue)
{}
SE_Vector2f SE_Config::getVector2f(const char* id, const SE_Vector2f& defaultValue)
{}
SE_Vector3f SE_Config::getVector3f(const char* id, const SE_Vector3f& defaultValue)
{}
SE_Vector3i SE_Config::getVector3i(const char* id, const SE_Vector3i& defaultValue)
{}
