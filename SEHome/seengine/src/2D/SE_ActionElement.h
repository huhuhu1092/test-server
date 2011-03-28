#ifndef SE_ACTIONELEMENT_H
#define SE_ACTIONELEMENT_H
#include "SE_Element.h"
#include "SE_ID.h"
#include <list>
class SE_Value;
class SE_ParamValueList;

class SE_ActionElement : public SE_2DNodeElement
{
public:
	SE_ActionElement(const SE_StringID& uri);
	void spawn();
	SE_Spatial* createSpatial();
	void update(const SE_TimeKey& key);
    void update(const SE_AddressID& address, const SE_Value& value);
    void update(SE_ParamValueList& paramValueList);
	void addHeadElement(SE_Element* e)
	{
		mHeadElementList.push_back(e);
	}
	SE_Element* clone();
private:
	SE_Action* mAction;
	typedef std::list<SE_Element*> _HeadElementList;
	_HeadElementList mHeadElementList;
};
#endif
