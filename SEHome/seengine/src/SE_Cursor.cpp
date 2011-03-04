#include "SE_Cursor.h"
SE_Cursor::SE_Cursor()
{
    mCursorScene = NULL;
    mPivotX = mPivotY = 0;
    mMountPointX = mMountPointY = 0;
    mMountPointXPrev = mMountPointYPrev = 0;
}
SE_Cursor::~SE_Cursor()
{
    if(mCursorScene)
        delete mCursorScene;
}
void SE_Cursor::loadCursor(const char* cursorResource)
{
    if(mCursorScene)
        delete mCursorScene;
	mCursorScene = new SE_Scene(SE_2D_SCENE);
	SE_Camera* camera = SE_Camera::create2DSceneCamera(mSceneWidth, mSceneHeight);
	SE_CameraManager* cameraManager = SE_Application::getInstance()->getCameraManager();
    SE_CameraID cameraID = cameraManager->add(camera);
    mCursorScene->setCamera(cameraID);
    
}
void SE_Cursor::render(const SE_SceneRenderSeq& seq, SE_RenderManager& renderManager)
{}
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
SE_Vector2f SE_Cursor::getPivotPoint()
{
    return SE_Vector2f(mPivotX, mPivotY);
}
SE_Vector2f SE_Cursor::getMountPoint()
{
    return SE_Vector2f(mMountPointX, mMountPointY);
}
void SE_Cursor::handleMotionEvent(const SE_MotionEvent& motionEvent)
{}
SE_Vector2f SE_Cursor::getCursorTip()
{
    return SE_Vector2f(mMountPointX, mMountPiontY - mPivotY);
}
SE_Vector2f SE_Cursor::getDisplacement() 
{
    return SE_Vector2f(mMountPiontX - mMountPointXPrev, mMountPointY - mMountPointYPrev);
}
