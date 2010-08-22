#include "SE_InputManager.h"
#include "SE_Object.h"
#include "SE_InputEvent.h"
SE_InputManager::SE_InputManager()
{

}
void SE_InputManager::update(SE_KeyEvent* keyEvent)
{
	KeyEventObserverList::iterator it ;
	for(it = mKeyEventObserverList.begin() ; it != mKeyEventObserverList.end() ; it++)
	{
		SE_Object* obj = *it;
		obj->onKeyEvent(keyEvent);
	}
}
void SE_InputManager::update(SE_MotionEvent* motionEvent)
{
    MotionEventObserverList::iterator it ;
	for(it = mMotionEventObserverList.begin() ; it != mMotionEventObserverList.end() ; it++)
	{
		SE_Object* obj = *it;
		obj->onMotionEvent(motionEvent);
	}
}
void SE_InputManager::addKeyEventOberver(SE_Object* keyEventObserver)
{
	mKeyEventObserverList.push_back(keyEventObserver);
}
void SE_InputManager::addMotionEventOberver(SE_Object* motionEventObserver)
{
    mMotionEventObserverList.push_back(motionEventObserver);
}
void SE_InputManager::removeKeyEventObserver(SE_Object* keyEventObserver)
{
	if(keyEventObserver == NULL)
	{
		mKeyEventObserverList.clear();
		return;
	}
	mKeyEventObserverList.remove(keyEventObserver);
}
void SE_InputManager::removeMotionEventObserver(SE_Object* motionEventObserver)
{
	if(motionEventObserver == NULL)
	{
		mMotionEventObserverList.clear();
		return;
	}
	mMotionEventObserverList.remove(motionEventObserver);
}