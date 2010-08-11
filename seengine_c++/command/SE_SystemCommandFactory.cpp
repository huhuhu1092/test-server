#include "SE_SystemCommandFactory.h"
#include "SE_SystemCommand.h"
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

SE_SystemCommandFactory::SE_SystemCommandFactory()
{
	SE_CommandEntry* systemCommandEntry[2];
	systemCommandEntry[0] = new SE_CommandEntry(SE_CommandID("SE_InitAppCommand"), new SE_CreateInitAppCommand);
	systemCommandEntry[1] = new SE_CommandEntry(SE_CommandID("SE_UpdateCameraCommand"), new SE_CreateUpdateCameraCommand);
    set(systemCommandEntry, 2);
}
