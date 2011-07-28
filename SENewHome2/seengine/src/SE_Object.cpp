#include "SE_Object.h"
#include "SE_Buffer.h"
#include "SE_Log.h"
/*
SE_Object::ObjProperty::ObjProperty()
{
    LOGI("## create property ##\n");
}
*/
SE_Object::ObjectCreateMap* SE_Object::mObjectCreateMap = NULL;
SE_Object::ObjProperty SE_Object::mObjProp;
SE_Object::SE_Object()
{

}
void SE_Object::read(SE_BufferInput& input)
{}
void SE_Object::write(SE_BufferOutput& output)
{}
void SE_Object::onKeyEvent(SE_KeyEvent* keyEvent)
{}
void SE_Object::onMotionEvent(SE_MotionEvent* motionEvent)
{}
void SE_Object::reg(const char* className, SE_ObjectCreateFunc* ocFunc)
{
    if(!mObjProp.init)
    {
        mObjProp.init = true;
        mObjectCreateMap = new ObjectCreateMap();
    }
    std::string str(className);
    ObjectCreateMap::iterator it = mObjectCreateMap->find(str);
    if(it == mObjectCreateMap->end())
    {
        mObjectCreateMap->insert(std::pair<std::string, SE_ObjectCreateFunc*>(str, ocFunc));
    }
    else
    {
        LOGE("error when register class %s\n", className);
    }
}
void SE_Object::unreg(const char* className)
{
    if(!mObjProp.init)
    {
        mObjProp.init = true;
        mObjectCreateMap = new ObjectCreateMap();
    }
    std::string str(className);
    mObjectCreateMap->erase(str);
}
SE_Object* SE_Object::create(const char* className)
{
    std::string str(className);
    ObjectCreateMap::iterator it = mObjectCreateMap->find(str);
    if(it == mObjectCreateMap->end())
        return NULL;
    else
    {
        return it->second->create();
    }

}
SE_Object::~SE_Object()
{

}
