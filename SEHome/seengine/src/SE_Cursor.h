#ifndef SE_CURSOR_H
#define SE_CURSOR_H
class SE_Cursor
{
public:
    enum STATE {DOWN, MOVE, UP, CANCEL, CLICKED};
    SE_Cursor(float sceneWidth, float sceneHeight);
    ~SE_Cursor();
    void loadCursor(const char* cursorResource);
    void render(const SE_SceneRenderSeq& seq, SE_RenderManager& renderManager);
    void setPivotPoint(float x, float y);
    void setMointPoint(float x, float y);
    SE_Vector2f getPivotPoint();
    SE_Vector2f getMountPoint();
    void handleMotionEvent(const SE_MotionEvent& motionEvent);
    SE_Vector2f getCursorTip() ;
    SE_Vector2f getDisplacement() ;
private:
    SE_Scene* mCursorScene;
    float mPivotX, mPivotY;
    float mMountPointX, mMountPointY;
    float mMountPiontXPrev, mMountPointYPrev;
    STATE mState;
    float mSceneWidth, mSceneHeight;
};
#endif
