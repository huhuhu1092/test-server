#include "SE_SystemCommandFactory.h"
#include "SE_SystemCommand.h"
/*
class SE_CreateInitAppCommand : public SE_CreateCommandFunc
{
public:
    SE_Command* create(SE_Application* app, const SE_CommandID& id)
    {
        return new SE_InitAppCommand(app);
    }
};
class SE_CreateUpdateCameraCommand : public SE_CreateCommandFunc
{
public:
	SE_Command* create(SE_Application* app, const SE_CommandID& id)
	{
		return new SE_UpdateCameraCommand(app);
	}
};
*/
SE_SystemCommandFactory::SE_SystemCommandFactory()
{
	SE_CommandEntry* systemCommandEntry[4];
	systemCommandEntry[0] = new SE_CommandEntry(SE_CommandID("SE_InitAppCommand"), new SE_CommandCreateFunc<SE_InitAppCommand>);
	systemCommandEntry[1] = new SE_CommandEntry(SE_CommandID("SE_UpdateCameraCommand"), new SE_CommandCreateFunc<SE_UpdateCameraCommand>);
	systemCommandEntry[2] = new SE_CommandEntry(SE_CommandID("SE_KeyEventCommand"), new SE_CommandCreateFunc<SE_KeyEventCommand>);
	systemCommandEntry[3] = new SE_CommandEntry(SE_CommandID("SE_MotionEventCommand"), new SE_CommandCreateFunc<SE_MotionEventCommand>);
    set(systemCommandEntry, 4);
}
