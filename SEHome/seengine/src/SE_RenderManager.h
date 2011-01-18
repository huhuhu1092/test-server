#ifndef SE_RENDERMANAGER_H
#define SE_RENDERMANAGER_H
#include "SE_Matrix.h"
#include "SE_Vector.h"
#include "SE_ID.h"
#include <list>
#include <vector>
class SE_RenderUnit;
class SE_RenderManager
{
public:
    enum RENDER_QUEUE {RQ0, RQ1, RQ2, RQ3, RQ4, RQ5, RQ6, RQ7, RQ_NUM};
	enum {RENDERTARGET_SIZE = 8};
    SE_RenderManager();
    ~SE_RenderManager();
    void beginDraw();
    void endDraw();
    void draw();
    void sort();
    void addRenderUnit(SE_RenderUnit* ru, const SE_RenderTargetID& renderTarget, RENDER_QUEUE rq);
	void setWorldToViewMatrix(const SE_Matrix4f& m)
	{
		mWorldToViewMatrix = m;
	}
	void setPerspectiveMatrix(const SE_Matrix4f& m)
	{
		mPerspectiveMatrix = m;
	}
	void setBackground(const SE_Vector3f& background)
	{
		mBackground = background;
	}    
private:
    typedef std::list<SE_RenderUnit*> RenderUnitList;
	struct _RenderTargetUnit
	{
        RenderUnitList* mRenderQueue[RQ_NUM];
        SE_RenderTargetID mRenderTargetID;
		_RenderTargetUnit()
		{
			for(int i = 0 ; i < RQ_NUM ; i++)
            {
                mRenderQueue[i] = new RenderUnitList;
            }
			mRenderTargetID = 0;
		}
	};
	std::list<_RenderTargetUnit*> mRenderTargetList;
private:
	_RenderTargetUnit* findTarget(const SE_RenderTargetID& id);
	static bool CompareRenderTarget(_RenderTargetUnit* first, _RenderTargetUnit* second)
	{
		if(first->mRenderTargetID < second->mRenderTargetID)
		    return true;
	    else
		    return false;
	}
	void clear();
private:
    SE_Matrix4f mWorldToViewMatrix;
    SE_Matrix4f mPerspectiveMatrix;
	SE_Vector3f mBackground;
};
#endif
