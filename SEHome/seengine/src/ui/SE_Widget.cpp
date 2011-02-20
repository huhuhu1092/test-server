#include "SE_Widget.h"
SE_Widget::SE_Widget()
{}
SE_Widget::~SE_Widget()
{}
void SE_Widget::spawn()
{
    SE_2DNodeElement::spawn();
}
void SE_Widget::update(const SE_TimeKey& timeKey)
{
    SE_2DNodeElement::update(timeKey);
}
void SE_Widget::update(SE_ParamValueList& paramValueList)
{
    SE_2DNodeElement::update(paramValueList);
}
void SE_Widget::update(const SE_AddressID& address, const SE_Value& value)
{
    SE_2DNodeElement::update(address, value);
}
void SE_Widget::layout()
{
    SE_2DNodeElement::layout();
}
SE_Spatial* SE_Widget::createSpatial()
{
    return SE_2DNodeElement::createSpatial();
}
void SE_Widget::read(SE_BufferInput& inputBuffer)
{}
void SE_Widget::write(SE_BufferOutput& outputBuffer)
{}
SE_Element* SE_Widget::clone()
{
    return NULL;
}
void SE_Widget::clone(SE_Element* src, SE_Element* dst)
{

}
