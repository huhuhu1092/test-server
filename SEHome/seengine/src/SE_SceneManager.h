#ifndef SE_SCENEMANAGER_H
#define SE_SCENEMANAGER_H
#include "SE_TreeStructManager.h"
#include <list>
class SE_Scene;
class SE_KeyEvent;
class SE_MotionEvent;
class SE_Element;
class SE_Cursor;
class SE_2DNodeElement;
class SE_RenderManager;
class SE_PointedElementHandler
{
public:
	virtual ~SE_PointedElementHandler() {}
	virtual void handle(SE_Scene* pointedScene, SE_Element* pointedElement, SE_Cursor* cursor, float x, float y) = 0;
};
/*
 * SceneManager manage the all screen region
 * its geometry is (0, 0, mWidth, mHeight)
 * Scene in SceneManager will be placed in this region
*/
class SE_SceneManager
{
public:
    enum {MAX_SIZE = 2000};
    enum {SIZE = 200};
    SE_SceneManager();
    ~SE_SceneManager();
    SE_SceneID add(SE_Scene* scene);
    SE_SceneID top();
    SE_Scene* get(const SE_SceneID& id);
    void pop();
    void rotate();
    void swap();
    void show(const SE_SceneID& id);
    void hide(const SE_SceneID& id);
    void dismiss(const SE_SceneID& id);
    void render(SE_RenderManager& renderManager);
    void dispatchKeyEvent(const SE_KeyEvent& keyEvent);
    void dispatchMotionEvent(const SE_MotionEvent& motionEvent);
    void setWidth(float width)
    {
        mWidth = width;
    }
    void setHeight(float height)
    {
        mHeight = height;
    }
    void loadCursor(const char* cursorResource, float mx, float my);
	void showCursor();
	void setPointedElementHandler(SE_PointedElementHandler* h)
	{
		if(mPointedElementHandler)
			delete mPointedElementHandler;
		mPointedElementHandler = h;
	}
private:
	struct _PointedData
	{
		SE_Scene* pointedScene;
		SE_Element* pointedElement;
		_PointedData(SE_Scene* s, SE_Element* e)
		{
			pointedScene = s;
			pointedElement = e;
		}
	};
    SE_SceneManager(const SE_SceneManager&);
    SE_SceneManager& operator=(const SE_SceneManager&);
	//void handleMotionEvent(SE_Element* pointedElement, const SE_MotionEvent& motionEvent);
	void handlePointedElement(SE_Scene* pointedScene, SE_Element* pointedElement, SE_Cursor* cursor, float x, float y);
	_PointedData getPointedData(float x, float y);
	std::list<SE_Scene*> getMotionEventScene();
	void handleCursorMotionEvent(const SE_MotionEvent& motionEvent);
	void handlePointedMotionEvent(const SE_MotionEvent& motionEvent);
private:
    SE_TreeStructManager<SE_Scene> mScenes;
    typedef std::list<SE_SceneID> _SceneStack;
    _SceneStack mStack;
    float mWidth;
    float mHeight;
	//SE_ElementID mMotionDownElementID;
	//SE_ElementID mMotionMoveElementID;
	//SE_ElementID mMotionUpElementID;
	//SE_ElementID mMotionCancelElementID;
	//SE_2DNodeElement* mPointedElement;
	//SE_2DNodeElement* mPointedElementPrev;
	//float mPrevX, mPrevY;
	//int mPrevMotionEventType;
	SE_Cursor* mCursor;
	SE_PointedElementHandler* mPointedElementHandler;
};
#endif
