#ifndef SE_SEQUENCEELEMENT_H
#define SE_SEQUENCEELEMENT_H
#include "SE_Element.h"
class SE_TimeKey;
class SE_Value;
class SE_Sequence;
class SE_SequenceElement : public SE_2DNodeElement
{
public:
	SE_SequenceElement(const SE_StringID& uri);
    void spawn();
    void layout();
	SE_Spatial* createSpatial();
	void update(const SE_TimeKey& key);
    void update(const SE_AddressID& address, const SE_Value& value);
    void update(SE_ParamValueList& paramValueList);
	int getKeyFrameNum();
	SE_Element* clone();
private:
	SE_Sequence* mSequence;
	SE_2DNodeElement* mCurrentElement;
};
#endif
