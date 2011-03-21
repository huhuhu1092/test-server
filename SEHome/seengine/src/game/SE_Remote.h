#ifndef SE_REMOTE_H
#define SE_REMOTE_H
#include <string>
#include "SE_Utils.h"
class SE_Remote
{
public:
    void setNetwork(bool iswifi, bool iswap, bool iswirelessnet)
    {
        mIsWifi = iswifi;
        mIsWap = iswap;
        mIsWirelessNet = iswirelessnet;
    }
    bool isWifi() const
    {
        return mIsWifi;
    }
    bool isWap() const
    {
        return mIsWap;
    }
    bool isWirelessNet() const
    {
        return mIsWirelessNet;
    }
    void setServerIP(const std::string& ip)
    {
        mServerIP = ip;
    }
    std::string getServerIP() const
    {
        return mServerIP;
    }
    std::string getPortString() const
    {
        return SE_Util::intToString(mServerPort);
    }
    void setServerPort(int port)
    {
        mServerPort = port;
    }

    int getServerPort() const
    {
        return mServerPort;
    }
    void setProxyIP(const std::string& ip)
    {
        mProxyIP = ip;
    }
    std::string getProxyIP() const
    {
        return mProxyIP;
    }
    void setProxyPort(int port)
    {
        mProxyPort = port;
    }
    int getProxyPort() const
    {
        return mProxyPort;
    }
    std::string getProxyPortString() const
    {
        return SE_Util::intToString(mProxyPort);
    }
private:
    std::string mServerIP;
    std::string mProxyIP;
    int mServerPort;
    int mProxyPort;
    bool mIsWifi;
    bool mIsWap;
    bool mIsWirelessNet;
};
#endif
