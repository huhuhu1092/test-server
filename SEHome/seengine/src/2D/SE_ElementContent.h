#ifndef SE_ELEMENTCONTENT_H
#define SE_ELEMENTCONTENT_H
#include "SE_ID.h"
class SE_Element;
class SE_ElementSchema;
class SE_ElementContent
{
public:
	SE_ElementContent()
	{
		mParent = NULL;
        mState = 0;
	}
	void setParent(SE_ElementSchema* parent)
	{
		mParent = parent;
	}
	void setURI(const SE_StringID& uri)
	{
		mContentURI = uri;
	}
    int getState() const
    {
        return mState;
    }
    void setState(int state)
    {
        mState = state;
    }
	SE_StringID getURI()
	{
		return mContentURI;
	}
	virtual SE_Element* createElement(float mpx, float mpy) = 0;
	virtual ~SE_ElementContent() {}
	virtual SE_ElementContent* clone();
protected:
	void clone(SE_ElementContent* src, SE_ElementContent* dst);
public:
	void setID(const SE_StringID& id)
	{
		mID = id;
	}
	SE_StringID getID()
	{
		return mID;
	}
private:
	SE_StringID mID;
    SE_ElementSchema* mParent;
	SE_StringID mContentURI;
    int mState;
};
class SE_ImageContent : public SE_ElementContent
{
public:
	SE_ImageContent(const SE_StringID& imageURI);
	SE_Element* createElement(float mpx, float mpy);
	SE_ElementContent* clone();
};

class SE_ActionContent : public SE_ElementContent
{
public:
	SE_ActionContent(const SE_StringID& actionURI);
	SE_Element* createElement(float mpx, float mpy);
	SE_ElementContent* clone();
};
class SE_StateTableContent : public SE_ElementContent
{
public:
	SE_StateTableContent(const SE_StringID& stateTableURI);
	SE_Element* createElement(float mpx, float mpy);
	SE_ElementContent* clone();
};

#endif
