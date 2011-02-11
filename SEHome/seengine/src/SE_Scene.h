#ifndef SE_SCENE_H
#define SE_SCENE_H
#include "SE_ID.h"
#include "SE_TreeStruct.h"
class SE_SceneManager;
class SE_RenderManager;
class SE_KeyEvent;
class SE_MotionEvent;
class SE_Scene;
class SE_Scene : public SE_ListStruct<SE_Scene>
{
    friend class SE_SceneManager;
public:
    SE_Scene();
    ~SE_Scene();
    void create(const char* sceneName);
    //SE_SceneID getID();
    void show();
    void exit();
    void hide();
    void render(const SE_SceneRenderSeq& seq, SE_RenderManager& renderManager);
    void setCamera(const SE_CameraID& cameraID);
    void dispatchKeyEvent(const SE_KeyEvent& keyEvent);
    void dispatchMotionEvent(const SE_MotionEvent& motionEvent);
    void setTranslucent(bool bTranslucent)
    {
        mIsTranslucent = bTranslucent;
    }
    bool isTranslucent() const
    {
        return mIsTranslucent;
    }
    void setBound(float x , float y, float width, float height)
    {
        mX = x;
        mY = y;
        mWidth = width;
        mHeight = height;
    }
private:
    SE_Scene(const SE_Scene&);
    SE_Scene& operator=(const SE_Scene&);
private:
    //void setID(const SE_SceneID& sceneID);
private:
    //SE_SceneID mID;
    SE_ElementID mRoot;
    SE_CameraID mCamera;
    bool mIsTranslucent;
    float mX, mY, mWidth, mHeight;// left low corner and width , height
};
#endif
