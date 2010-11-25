#ifndef SE_TESTCOMMAND_H
#define SE_TESTCOMMAND_H
class SE_TestInitCommand : public SE_Command
{
public:
    SE_TestInitCommand(SE_Application* app);
    ~SE_TestInitCommand();
    void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
public:
    std::string dataPath;
    std::string fileName;
};
#endif