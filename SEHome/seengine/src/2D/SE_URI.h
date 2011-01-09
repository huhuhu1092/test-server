#ifndef SE_URI_H
#define SE_URI_H
#include "SE_ID.h"
#include <list>
#include <vector>
class SE_URI
{
public:
	SE_URI(const char* str = NULL);
	void setURI(const SE_StringID& uri);
	SE_StringID getURI() const
	{
		return mURI;
	}
	SE_StringID getURL() const;
	bool isContainAddress(const SE_AddressID& address) const;
	std::vector<SE_AddressID> getAddress() const;
private:
	void parse();
private:
	enum {START_PARAM, END_PARAM, ERROR_PARAM};
	struct _AddressLocation
	{
		SE_AddressID address;
		std::string::size_type start;
		std::string::size_type size;
	};
	SE_URI::_AddressLocation matchPos(std::string::size_type pos) const;
private:
	typedef std::list<_AddressLocation> _AddressLocationList;
	SE_StringID mURI;
	_AddressLocationList mAddressLocationList;
	int mState;
};

#endif
