#ifndef SE_2DCOMMAND_H
#define SE_2DCOMMAND_H
#include "SE_Command.h"
#include "SE_Primitive.h"
#include <string>
class SE_Application;
struct EyeData
{
	SE_PrimitiveID leftEyeID;
	SE_PrimitiveID rightEyeID;
	SE_SpatialID leftSpatialID;
};
class SE_Init2D : public SE_Command
{
public:
    SE_Init2D(SE_Application* app);
    ~SE_Init2D();
    void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
public:
    float width;
    float height;
    std::string dataPath;
    std::string fileName;
};
class SE_2DAnimation : public SE_Command
{
public:
    SE_2DAnimation(SE_Application* app, SE_TimeMS duration, TIME_TYPE type);
    ~SE_2DAnimation();
    void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
public:
	int n;
	SE_PrimitiveID leftEyeID;
	SE_PrimitiveID rightEyeID;
	SE_SpatialID leftEyeSpatialID;
	SE_TimeMS duration;
};
#endif
