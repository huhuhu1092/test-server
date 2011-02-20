#ifndef SE_WIDGET_H
#define SE_WIDGET_H
#include "SE_URI.h"
#include "SE_ID.h"
#include "SE_Element.h"
class SE_Widget : public SE_2DNodeElement
{
public:
    SE_Widget();
    ~SE_Widget();
	/*
    void setNormalURI(const SE_StringID& uri)
    {
        mNormalURI.setURI(uri);
    }
    SE_StringID getNormalURI() const
    {
        return mNormalURI.getURI();
    }
    SE_StringID getNorlURL() const
    {
        return mNormalURI.getURL();
    }
    void setHighlightedURI(const SE_StringID& uri)
    {
        mHighlightedURI = uri;
    }
    SE_StringID getHighlightedURI() const
    {
        return mHighlightedURI.getURI();
    }
    SE_StringID getHighlightedURL() const
    {
        return mHighlightedURI.getURL();
    }
    void setSelectedURI(const SE_StringID& uri)
    {
        mSelectedURI = uri;
    }
    SE_StringID getSelectedURI() const
    {
        return mSelectedURI.getURI();
    }
    SE_StringID getSelectedURL() const
    {
        return mSelectedURI.getURL();
    }
    void setInactiveURI(const SE_StringID& uri)
    {
        mInactiveURI = uri;
    }
    SE_StringID getInactiveURI(const SE_StringID& uri)
    {
        return mInactiveURI.getURI();
    }
    SE_StringID getInactiveURL(const SE_StringID& uri)
    {
        return mInactiveURI.getURL();
    }
	*/
public:
    virtual void spawn();
    virtual void update(const SE_TimeKey& timeKey);
	virtual void update(SE_ParamValueList& paramValueList);
	virtual void update(const SE_AddressID& address, const SE_Value& value);
    virtual void layout();
    virtual SE_Spatial* createSpatial();  
    virtual void read(SE_BufferInput& inputBuffer);
    virtual void write(SE_BufferOutput& outputBuffer); 
    virtual SE_Element* clone();
protected:
    virtual void clone(SE_Element* src, SE_Element* dst);
protected:
    //SE_URI mNormalURI;
    //SE_URI mHighlightedURI;
    //SE_URI mSelectedURI;
    //SE_URI mInactiveURI;
};
#endif
