#include "SClientManager.h"
#include "SClient.h"
#include "SCommandEventFactory.h"
#include "SNetAddress.h"
#include "SSocket.h"
#include "SUtil.h"
#include "SBufferStream.h"
#include "SLog.h"
#include "SMessageStream.h"
#include "../../server/SCommandEventDefine.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <string>
#include <vector>
using namespace std;
int main(int argc, char** argv)
{
    //fd_set rfds;

    //SNetAddress na("192.168.2.184", SUtil::Host2NetInt16(10000));
    SNetAddress na("127.0.0.1", SUtil::Host2NetInt16(10000));
    SSocketClient client(STREAM, na);
    if(client.getError() != SSocketClient::NO_ERROR)
        return -1;
    SLoginCommandEvent se("aa", "bb");
    char* out;
    int len;
    se.pack(out, len);
    int ret = client.send((unsigned char*)out, len);
    SLog::msg("#### write num = %d ####\n", ret);
    SMessageStream ms;
    int currSocket = client.getSocket();
    while(true)
    {
        unsigned char buffer[258 * 1024];

        //len = client.read(buffer, 258 * 1024);
        len = ::recv(currSocket,buffer, 258 * 1024, 0);
        if(len > 0)
        {
            SLog::msg("len = %d\n", len);
            ms.addMessagePacket(buffer, len);
            SMessage m;
            while((ret = ms.getNextMessage(&m)) == SMessageStream::NO_ERROR)
            {
                int msgid = m.data[0];
                switch(msgid)
                {
                case LOGIN_REPLY:
                    {
                        SBufferStreamInput inputStream((const char*)m.data, m.len);
                        char msgid;
                        inputStream.readChar(msgid);
                        short int dataLen;
                        inputStream.readShort(dataLen);
                        short int count;
                        inputStream.readShort(count);
                        vector<string> stringV(count);
                        for(int i = 0 ; i < count ; i++)
                        {
                            char* str;
                            int strLen;
                            inputStream.readString(str, strLen);
                            stringV[i] = string(str, strLen);
                        } 
                        for(int i = 0 ; i < count ; i++)
                        {
                            SLog::msg("%s\n", stringV[i].c_str());
                        }
                        break;
                    }
                }
                m.release();
            }
        }
        else if(len == 0)
        {
            break;
        }
    }
    return 0;
}
