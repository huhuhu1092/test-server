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
	enum {RENDERTARGET_SIZE = 8};
    SE_RenderManager();
    ~SE_RenderManager();
    //void beginDraw();
    void endDraw();
    void draw();
    void sort();
    void enableDraw(const SE_SceneRenderSeq& index);
    void disableDraw(const SE_SceneRenderSeq& index);
    //void setSceneTranslucent(const SE_SceneRenderSeq& index, bool translucent);
    //void setSceneCamera(const SE_SceneRenderSeq& index, SE_Camera* camera);
    //void setSceneBackground(const SE_SceneRenderSeq& index, const SE_Vector4f& background);
    void addRenderUnit(SE_RenderUnit* ru, const SE_SceneRenderSeq& sceneRenderSeq, const SE_RenderTargetSeq& renderTargetSeq, const SE_RenderTargetID& renderTarget, SE_RENDER_QUEUE rq);
    /*
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
 */   
private:
    typedef std::list<SE_RenderUnit*> RenderUnitList;
	struct _RenderTargetUnit
	{
        RenderUnitList* mRenderQueue[SE_RQ_NUM];
        SE_RenderTargetID mRenderTargetID;
        SE_RenderTargetSeq mRenderTargetSeq;
		_RenderTargetUnit()
		{
			for(int i = 0 ; i < SE_RQ_NUM ; i++)
            {
                mRenderQueue[i] = new RenderUnitList;
            }
		}
	};
    struct _SceneRenderUnit
    {
        bool translucent;
        bool needDraw;
        std::list<_RenderTargetUnit*> renderTargetUnit;
        _SceneRenderUnit()
        {
            translucent = false;
            needDraw = true;
        }
    };
	//std::list<_RenderTargetUnit*> mRenderTargetList;
private:
	_RenderTargetUnit* findTarget(_SceneRenderUnit* sceneRenderUnit, const SE_RenderTargetID& id);
	static bool CompareRenderTarget(_RenderTargetUnit* first, _RenderTargetUnit* second)
	{
		if(first->mRenderTargetSeq < second->mRenderTargetSeq)
		    return true;
	    else
		    return false;
	}
private:

	void clear();
private:
    _SceneRenderUnit* mSceneRenderUnit[SE_MAX_RENDERSCENE_SIZE];
    //SE_Matrix4f mWorldToViewMatrix;
    //SE_Matrix4f mPerspectiveMatrix;
	//SE_Vector3f mBackground;
};
#endif
