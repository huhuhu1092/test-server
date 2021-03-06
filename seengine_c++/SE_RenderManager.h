#ifndef SE_RENDERMANAGER_H
#define SE_RENDERMANAGER_H
#include "SE_Matrix.h"
#include <list>
class SE_RenderUnit;
class SE_RenderManager
{
public:
    enum RENDER_QUEUE {RQ0, RQ1, RQ2, RQ3, RQ4, RQ5, RQ6, RQ7, RQ_NUM};
    SE_RenderManager();
    ~SE_RenderManager();
    void beginDraw();
    void endDraw();
    void draw();
    void sort();
    void addRenderUnit(SE_RenderUnit* ru, RENDER_QUEUE rq = RQ0 );
	void setWorldToViewMatrix(const SE_Matrix4f& m)
	{
		mWorldToViewMatrix = m;
	}
	void setPerspectiveMatrix(const SE_Matrix4f& m)
	{
		mPerspectiveMatrix = m;
	}
private:
    typedef std::list<SE_RenderUnit*> RenderUnitList;
    RenderUnitList* mRenderQueue[RQ_NUM];
    SE_Matrix4f mWorldToViewMatrix;
    SE_Matrix4f mPerspectiveMatrix;
};
#endif
