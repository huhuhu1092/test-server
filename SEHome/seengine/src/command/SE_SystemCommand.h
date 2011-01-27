#ifndef SE_SYSTEMCOMMAND_H
#define SE_SYSTEMCOMMAND_H
#include "SE_Command.h"
#include "SE_Vector.h"
#include "SE_Value.h"
#include <string>
#include <list>
class SE_Application;
class SE_Camera;
class SE_KeyEvent;
class SE_MotionEvent;
class SE_InitAppCommand : public SE_Command
{
public:
    SE_InitAppCommand(SE_Application* app);
    ~SE_InitAppCommand();
    void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
public:
    std::string dataPath;
    std::string fileName; // the prefix of the file
};
//////////////////////////////////////
class SE_UpdateCameraCommand : public SE_Command
{
public:
    SE_UpdateCameraCommand(SE_Application* app);
    ~SE_UpdateCameraCommand();
    void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
public:
    int width;
    int height;
};
///////////////////////////////////////
class SE_KeyEventCommand : public SE_Command
{
public:
	SE_KeyEventCommand(SE_Application* app);
	~SE_KeyEventCommand();
	void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
	SE_KeyEvent* keyEvent;
};
class SE_MotionEventCommand : public SE_Command
{
public:
	SE_MotionEventCommand(SE_Application* app);
	~SE_MotionEventCommand();
	void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
	SE_MotionEvent* motionEvent;
};
////////////////////////////////////////
class SE_LoadSceneCommand : public SE_Command
{
public:
    SE_LoadSceneCommand(SE_Application* app);
    void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
    std::string sceneName;
};
////////////////////////////////////////
class SE_MoveCameraCommand : public SE_Command
{
public:
    SE_MoveCameraCommand(SE_Application* app);
    ~SE_MoveCameraCommand();
    void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
public:
    SE_Camera* camera;
	float rotateAngle;
	SE_AXIS_TYPE axis;
	SE_Vector3f translate;

};
/////////////////////////////////
class SE_FunctionCommand : public Command
{
public:
    SE_FunctionCommand(SE_Application* app);
    ~SE_FunctionCommand();
    void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
    void setFunctionName(const char* name);
    void addParam(const SE_Value& v);
private:
    typedef std::list<SE_Value> ParamList;
    std::string mFuncName;
    ParamList mParam;
};
#endif
