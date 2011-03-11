#include "SE_Cursor.h"
#include "SE_InputEvent.h"
#include "SE_Scene.h"
#include "SE_ID.h"
#include "SE_Camera.h"
#include "SE_Application.h"
#include "SE_CameraManager.h"
#include "SE_Spatial.h"
#include "SE_Element.h"
SE_Cursor::SE_Cursor(float sceneWidth, float sceneHeight)
{
    mCursorScene = NULL;
    mPivotX = mPivotY = 0;
    mMountPointX = mMountPointY = 0;
    mMountPointXPrev = mMountPointYPrev = 0;
    mSceneWidth = sceneWidth;
	mSceneHeight = sceneHeight;;
    mState = UP;
}
SE_Cursor::~SE_Cursor()
{
    if(mCursorScene)
        delete mCursorScene;
}
void SE_Cursor::show()
{
	if(mCursorScene)
		mCursorScene->show();
}
void SE_Cursor::load(const char* cursorResource)
{
    if(mCursorScene)
        delete mCursorScene;
	mCursorScene = new SE_Scene(SE_2D_SCENE);
	mCursorScene->setBound(mSceneWidth, mSceneHeight);
	mCursorScene->create(cursorResource);
	mCursorScene->setTranslucent(true);
	//
	SE_Element* elementRoot = mCursorScene->getRootElement();
	SE_2DNodeElement* e = (SE_2DNodeElement*)elementRoot->findByName("cursor");
	e->setMountPoint(mMountPointX, mMountPointY);
	e->layout();
	mCursorScene->setBound(mSceneWidth, mSceneHeight);
	SE_Camera* camera = SE_Camera::create2DSceneCamera(mSceneWidth, mSceneHeight);
	SE_CameraManager* cameraManager = SE_Application::getInstance()->getCameraManager();
    SE_CameraID cameraID = cameraManager->add(camera);
    mCursorScene->setCamera(cameraID);   
}
void SE_Cursor::render(const SE_SceneRenderSeq& seq, SE_RenderManager& renderManager)
{
    if(mCursorScene)
        mCursorScene->render(seq, renderManager);
}

void SE_Cursor::setPivotPoint(float x, float y)
{
    mPivotX = x;
    mPivotY = y;
}

void SE_Cursor::setMointPoint(float x, float y)
{
    mMountPointX = x;
    mMountPointY = y;
}

SE_Vector2f SE_Cursor::getPivotPoint() const
{
    return SE_Vector2f(mPivotX, mPivotY);
}
SE_Vector2f SE_Cursor::getMountPoint() const
{
    return SE_Vector2f(mMountPointX, mMountPointY);
}
void SE_Cursor::handleMotionEvent(const SE_MotionEvent& motionEvent)
{
	if(!mCursorScene)
		return;
    float x = motionEvent.getX();
	float y = motionEvent.getY();
	bool equal = false;
	bool pointValid = true;
	if(x == mMountPointX && y == mMountPointY)
		equal = true;
	if((mState == UP || mState == CLICKED) && motionEvent.getType() == SE_MotionEvent::DOWN)
	{
		mState = DOWN;
	    mMountPointXPrev = x;
	    mMountPointYPrev = y;
	    SE_Element* selectedElement = mCursorScene->getPointedElement(x, y);
		if(selectedElement)
		{
		    pointValid = false;
		}
		else
		{
			pointValid = true;
		}
		LOGI(" UP --> DOWN\n");
	}
	else if(mState == DOWN && motionEvent.getType() == SE_MotionEvent::DOWN)
	{
		//move state;
	    float deltax = x - mMountPointXPrev;
	    float deltay = y - mMountPointYPrev;
		if(SE_Fabs(deltax) > SE_MotionEvent::MOVE_SLOPE || SE_Fabs(deltay) > SE_MotionEvent::MOVE_SLOPE)
		{
			mState = MOVE;
			pointValid = true;
			LOGI("DOWN --> MOVE\n");
		}
		else
		{
			pointValid = false;
			LOGI("DOWN --> DOWN\n");
		}
	}
	else if(mState == MOVE && motionEvent.getType() == SE_MotionEvent::DOWN)
	{
		pointValid = true;
		mMountPointXPrev = mMountPointX;
		mMountPointYPrev = mMountPointY;
		LOGI("MOVE --> MOVE\n");
	}
	else if(mState == MOVE && 
		   (motionEvent.getType() == SE_MotionEvent::UP || motionEvent.getType() == SE_MotionEvent::CANCEL))
	{
		mState = UP;
		pointValid = true;
		LOGI("MOVE --> UP\n");
	}
	else if(mState == DOWN && motionEvent.getType() == SE_MotionEvent::UP)
	{
		mState = CLICKED;
		pointValid = true;
		LOGI("MOVE --> CLICKED\n");
	}
	else if(mState == DOWN && motionEvent.getType() == SE_MotionEvent::CANCEL)
	{
		mState = UP;
		pointValid = true;
		LOGI("MOVE --> CANCEL\n");
	}
	if(pointValid)
	{
	    mMountPointX = x;
	    mMountPointY = y;
	}
	LOGI("### %f, %f ###\n", mMountPointX, mMountPointY);
	SE_Element* rootElement = mCursorScene->getRootElement();
	SE_2DNodeElement* cursorElement = (SE_2DNodeElement*)rootElement->findByName("cursor");
    if(cursorElement && !equal)
	{
		LOGI("### update spatial ###\n");
		cursorElement->setMountPoint(mMountPointX, mMountPointY);
		cursorElement->layout();
		cursorElement->updateSpatial(false);
	}
}
SE_Vector2f SE_Cursor::getCursorTip() const
{
	SE_Element* rootElement = mCursorScene->getRootElement();
	SE_2DNodeElement* e = (SE_2DNodeElement*)rootElement->findByName("cursorc");
	float pivotx = e->getPivotX();
	float pivoty = e->getPivotY();
    return SE_Vector2f(mMountPointX, mMountPointY - pivoty);
}
SE_Vector2f SE_Cursor::getDisplacement() const
{
	return SE_Vector2f(mMountPointX - mMountPointXPrev, mMountPointY - mMountPointYPrev);
}