#ifndef SE_MOTIONEVENTCAMERA_H
#define SE_MOTIONEVENTCAMERA_H
#include "SE_Camera.h"
#include "SE_InputEvent.h"
class SE_MotionEventCamera : public SE_Camera
{
DECLARE_OBJECT(SE_MotionEventCamera)
public:
	SE_MotionEventCamera();
	~SE_MotionEventCamera();
	void onMotionEvent(SE_MotionEvent* motionEvent);
	void clearState();
private:
	SE_MotionEvent::TYPE mPrevType;
	float mPrevX;
	float mPrevY;
};
#endif