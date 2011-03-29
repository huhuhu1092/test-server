#ifndef SE_SCENE_H
#define SE_SCENE_H
#include "SE_ID.h"
#include "SE_Common.h"
#include "SE_TreeStruct.h"
#include "SE_Vector.h"
class SE_SceneManager;
class SE_RenderManager;
class SE_KeyEvent;
class SE_MotionEvent;
class SE_Scene;
class SE_Element;
class SE_Spatial;
class SE_Scene : public SE_ListStruct<SE_Scene>
{
    friend class SE_SceneManager;
public:
    SE_Scene(SE_SCENE_TYPE t);
    ~SE_Scene();
    void create(const char* sceneName);
    void show();
    void exit();
    void hide();
    void render(const SE_SceneRenderSeq& seq, SE_RenderManager& renderManager);
    void setCamera(const SE_CameraID& cameraID);
    void setBackground(const SE_Vector4f& color)
    {
        mBackground = color;
    }
    bool dispatchKeyEvent(const SE_KeyEvent& keyEvent);
    bool dispatchMotionEvent(const SE_MotionEvent& motionEvent);
	SE_Element* getPointedElement(float x, float y);
    void setTranslucent(bool bTranslucent)
    {
        mIsTranslucent = bTranslucent;
    }
    bool isTranslucent() const
    {
        return mIsTranslucent;
    }
    void setBound(float width, float height)
    {
        mWidth = width;
        mHeight = height;
    }
	bool isModel() const
	{
		return mIsModel;
	}
	void setModel(bool b)
	{
		mIsModel = b;
	}
    SE_Element* getRootElement();
	SE_Spatial* getRootSpatial();
	SE_Element* findByName(const char* name);
private:
    SE_Scene(const SE_Scene&);
    SE_Scene& operator=(const SE_Scene&);
private:
    //void setID(const SE_SceneID& sceneID);
private:
    //SE_SceneID mID;
    SE_ElementID mRoot;
    SE_CameraID mCamera;
    SE_RenderTargetID mRenderTargetID;
    bool mIsTranslucent;
    float mWidth, mHeight;// left low corner and width , height
    SE_Vector4f mBackground;
    SE_SCENE_TYPE mSceneType;
    bool mIsModel;
	SE_SceneRenderSeq mSceneRenderSeq;
};
#endif
