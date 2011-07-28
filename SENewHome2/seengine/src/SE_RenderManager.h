#ifndef SE_RENDERMANAGER_H
#define SE_RENDERMANAGER_H
#include "SE_Matrix.h"
#include "SE_Vector.h"
#include "SE_ID.h"
#include <list>
#include <vector>
class SE_RenderUnit;
class SE_RenderTarget;
class SE_Camera;
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
    void addRenderUnit(SE_RenderUnit* ru, RENDER_QUEUE rq);
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
    bool setCurrentScene(int index)
    {
        if(index < 0 || index >= SE_MAX_SCENE_SIZE)
            return false;
        mCurrentScene = index;
        return true;
    }
    int getCurrentScene() const
    {
        return mCurrentScene;
    }
    void setCurrentRenderTarget(SE_RenderTarget* renderTarget);
    SE_RenderTarget* getCurrentRenderTarget() const;
    void setCurrentCamera(SE_Camera* camera);
    SE_Camera* getCurrentCamera() const;
    void setCurrentBackgroundColor(const SE_Vector4f& c);
    SE_Vector4f getCurrentBackgroundColor() const;
private:
    typedef std::list<SE_RenderUnit*> RenderUnitList;
	struct _SceneUnit
	{
        RenderUnitList mRenderQueue[RQ_NUM];
        SE_RenderTarget* mRenderTarget;
        SE_Camera* mCamera;
        SE_Vector4f mBgColor;
		_SceneUnit()
		{
            /*
			for(int i = 0 ; i < RQ_NUM ; i++)
            {
                mRenderQueue[i] = new RenderUnitList;
            }
            */
			mRenderTarget = NULL;
            mCamera = NULL;
		}
	};
	//std::list<_RenderTargetUnit*> mRenderTargetList;
    
private:
	//_RenderTargetUnit* findTarget(const SE_RenderTargetID& id);
    /*
	static bool CompareRenderTarget(_SceneUnit* first, _RenderTargetUnit* second)
	{
		if(first->mRenderTargetID < second->mRenderTargetID)
		    return true;
	    else
		    return false;
	}
    */
	void clear();
    void drawDepthBufferOnly(bool db);
    void drawOneFrame();
private:
    _SceneUnit mSceneUnits[SE_MAX_SCENE_SIZE];
    int mCurrentScene;
    bool mUseDrawDepthOnlyOp;
    //SE_Matrix4f mWorldToViewMatrix;
    //SE_Matrix4f mPerspectiveMatrix;
	//SE_Vector3f mBackground;
};
#endif
