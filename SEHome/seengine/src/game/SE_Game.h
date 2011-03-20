#ifndef SE_GAME_H
#define SE_GAME_H
#include <string>
class SE_Game
{
public:
    SE_Game();
    virtual ~SE_Game();
    void setUserName(const std::string& name)
    {
        mName = name;
    }
    std::string getUserName() const
    {
        return mName;
    }
    void setPassword(const std::string& password)
    {
        mPassword = password;
    }
    std::string getPassword()
    {
        return mPassword;
    }
    std::string getSessionName() const
    {
        return mSessionName;
    }
    void setSessionName(const std::string& str)
    {
        mSessionName = str;
    }
private:
    std::string mName;
    std::string mPassword;
    std::string mSessionName;
};
#endif
