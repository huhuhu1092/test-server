#include "SE_URI.h"
#include "SE_Utils.h"
#include "SE_Log.h"
#include "SE_Application.h"
#include "SE_ParamManager.h"
SE_URI::SE_URI(const char* str)
{
	if(str)
		mURI = str;
	mState = END_PARAM;
	parse();
}
void SE_URI::setURI(const SE_StringID& uri)
{
	mURI = uri;
	mState = END_PARAM;
	parse();
}
SE_URI::_AddressLocation SE_URI::matchPos(std::string::size_type pos) const
{
	_AddressLocation al;
	al.size = 0;
	al.start = 0;
	_AddressLocationList::const_iterator it;
    for(it = mAddressLocationList.begin() ; it != mAddressLocationList.end() ; it++)
	{
		if(pos == it->start)
			return *it;
	}
	return al;
}
SE_StringID SE_URI::getURL() const
{
	if(mState == ERROR_PARAM)
		return mURI;
	std::string retstr = mURI.getStr();
	std::string retDst;
	if(mAddressLocationList.empty())
		return retstr.c_str();
	for(std::string::size_type pos = 0 ; pos < retstr.size() ;)
	{
        _AddressLocation al = matchPos(pos);
		if(al.size != 0)
		{
            SE_ParamManager* paramManager = SE_Application::getInstance()->getParamManager();
	        bool ok = false;
		    std::string value = paramManager->getString(al.address, ok);
	        if(ok)
	        {
			    retDst += value;
	        }
			else
			{
				std::string str = retstr.substr(al.start, al.size);
				retDst += str;
			}
			pos = al.start + al.size;
		}
		else
		{
			retDst += retstr[pos];
			pos++;
		}
	}
	return retDst.c_str();
	/*
	_AddressLocationList::const_iterator it ;
	for(it = mAddressLocationList.begin() ; it != mAddressLocationList.end() ; it++)
	{
		_AddressLocation al = *it;
		SE_ParamManager* paramManager = SE_Application::getInstance()->getParamManager();
	    bool ok = false;
		std::string value = paramManager->getString(al.address, ok);
	    if(ok)
	    {
			retstr.replace(al.start, al.size, value);
	    }
	}
	return retstr.c_str();
	*/
}

bool SE_URI::isContainAddress(const SE_AddressID& address) const
{
	_AddressLocationList::const_iterator it;
	for(it = mAddressLocationList.begin() ; it != mAddressLocationList.end() ; it++)
	{
		_AddressLocation al = *it;
		if(address == al.address)
			return true;
	}
	return false;
}
std::vector<SE_AddressID> SE_URI::getAddress() const
{
	std::vector<SE_AddressID> retv(mAddressLocationList.size());
	_AddressLocationList::const_iterator it;
	int i = 0;
	for(it = mAddressLocationList.begin() ; it != mAddressLocationList.end() ; it++)
	{
		_AddressLocation al = *it;
		retv[i] = al.address;
		i++;
	}
	return retv;
}
void SE_URI::parse()
{
	mAddressLocationList.clear();
	std::string paramString = mURI.getStr();
	std::string::size_type startNotLimit = 0;
	std::string::size_type pos;
	std::string::size_type startParam = std::string::npos;
	std::string::size_type endParam = std::string::npos;
	for(pos = 0 ; pos < paramString.size() ; pos++)
	{
		if(paramString[pos] == '[')
		{
            mState = START_PARAM;
			startParam = pos;
		}
		else if(paramString[pos] == ']')
		{
			if(mState == START_PARAM)
			{
				endParam = pos;
				mState = END_PARAM;
				std::string::size_type n = endParam - (startParam + 1);
		        std::string subString = paramString.substr(startParam + 1, n);
				_AddressLocation al;
				al.address = subString.c_str();
				al.start = startParam;
				al.size = endParam + 1 - startParam;
				mAddressLocationList.push_back(al);
			}
			else
			{
				LOGI("... has no [ before ]\n");
				mState = ERROR_PARAM;
				break;
			}
		}
		else
		{
		}
	}
    if(mState == ERROR_PARAM)
	{
		mAddressLocationList.clear();
	}
}

