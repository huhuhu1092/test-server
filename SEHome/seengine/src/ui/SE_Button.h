#ifndef SE_BUTTON_H
#define SE_BUTTON_H
#include "SE_Widget.h"
class SE_Button : public SE_Widget
{
public:
    SE_Button();
    ~SE_Button();
    virtual void spawn();
    virtual void update(const SE_TimeKey& timeKey);
	virtual void update(SE_ParamValueList& paramValueList);
	virtual void update(const SE_AddressID& address, const SE_Value& value);
    virtual void layout();
    virtual SE_Spatial* createSpatial();  
    virtual void read(SE_BufferInput& inputBuffer);
    virtual void write(SE_BufferOutput& outputBuffer); 
    virtual SE_Element* clone();
private:
    SE_StringID getImageURI();
protected:
    virtual void clone(SE_Element* src, SE_Element* dst);
private:
	SE_2DNodeElement* mElement;
	SE_ElementID mElementID;
};
#endif
