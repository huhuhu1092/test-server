#ifndef SE_STATETABLEELEMENT_H
#define SE_STATETABLEELEMENT_H
#include "SE_Element.h"
class SE_TimeKey;
class SE_Value;
class SE_ParamValueList;
class SE_StateMachine;
class SE_StateTableElement : public SE_2DNodeElement
{
public:
	SE_StateTableElement(const SE_StringID& uri);
	void update(const SE_TimeKey& key);
	void spawn();
	void layout();
    void update(const SE_AddressID& address, const SE_Value& value);
    void update(SE_ParamValueList& paramValueList);
	SE_Spatial* createSpatial();
	SE_Element* clone();
private:
	SE_StateMachine* mStateTable;
};
#endif
