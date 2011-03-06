#ifndef SE_CURSOR_H
#define SE_CURSOR_H
#include "SE_Vector.h"
class SE_RenderManager;
class SE_SceneRenderSeq;
class SE_MotionEvent;
class SE_Scene;
//curor's width and height is determined by its content
//
class SE_Cursor
{
    friend class SE_SceneManager;
public:
    enum STATE {DOWN, MOVE, UP, CLICKED};
    SE_Cursor(float sceneWidth, float sceneHeight);
    ~SE_Cursor();
	STATE getState() const
	{
		return mState;
	}
    void load(const char* cursorResource);
	void show();
    void render(const SE_SceneRenderSeq& seq, SE_RenderManager& renderManager);
    void setPivotPoint(float x, float y);
    void setMointPoint(float x, float y);
    SE_Vector2f getPivotPoint() const;
    SE_Vector2f getMountPoint() const;
    void handleMotionEvent(const SE_MotionEvent& motionEvent);
    SE_Vector2f getCursorTip() const;
	SE_Vector2f getDisplacement() const;
private:
    SE_Scene* mCursorScene;
    float mPivotX, mPivotY;
    float mMountPointX, mMountPointY;
    float mMountPointXPrev, mMountPointYPrev;
    STATE mState;
    float mSceneWidth, mSceneHeight;
};
#endif
