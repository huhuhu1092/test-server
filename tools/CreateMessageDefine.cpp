#include "CreateMessageDefine.h"
CreateMessageDefine::CreateMessageDefine()
{}
int CreateMessageDefine::read(QIODevice* device)
{
    setDevice(device);
    while(!atEnd())
    {
        readNext();
        if(isStartElement())
        {
            if(name() == "msgdefine" && attributes().value("version") == "1.0")
            {
                readMessageDefine();
            }
            else
            {
                raiseError(QObject::tr("The file is not a message define file"));
            }
        }
    }
}
void CreateMessageDefine::readMessageDefine()
{
    while(!atEnd())
    {
        readNext();
        if(isEndElement())
            break;
        if(isStartElement())
        {
            if(name() == "client")
            {
                //mCurrentString.clear();
                mMsgType = CLIENT;
                readClientMessage();
            }
            else if(name() == "server")
            {
                mMsgType = SERVER;
                readServerMessage();
            }
            else
            {
                raiseError(QObject::tr("Unknown Element"));

            }
        }
    }
}
void CreateMessageDefine::readClientMessage()
{
    while(!atEnd())
    {
        readNext();
        if(isEndElement())
            break;
        if(isStartElement())
        {
            if(name() == "message")
            {
                mCurrentClassItem.clear();
                readMessage();
            }
        }
        mOutputText.push_back(mCurrentClassItem);
    }

}
void CreateMessageDefine::readServerMessage()
{
    while(!atEnd())
    {
        readNext();
        if(isEndElement())
            break;
        if(isStartElement())
        {
            if(name() == "message")
            {
                mCurrentString.clear();
                readMessage();
            }
        }
        mOutputText.push_back(mCurrentClassItem);
    }
}
void CreateMessageDefine::readMessage()
{
    while(!atEnd())
    {
        readNext();
        if(isEndElement())
            break;
        if(isStartElement())
        {
            if(name() == "id")
            {
                readId();
            }
            else if(name() == "pack")
            {
                readPack();
            }
            else if(name() == "unpack")
            {
                readUnpack();
            }
            else if(name() == "handle")
            {
                readHandle();
            }
            else if(name() == "attribute")
            {
                readAttribute();
            }
        }
    }
}
void CreateMessageDefine::readPack()
{
    Q_ASSERT(isStartElement() && name() == "pack");
    QString text = readElementText();
    mCurrentClassItem.packFun = text;
}
void CreateMessageDefine::readUnpack()
{
    Q_ASSERT(isStartElement() && name() == "unpack");
    QString text = readElementText();
    mCurrentClassItem.unpackFun = text;

}
void CreateMessageDefine::readHandle()
{
    Q_ASSERT(isStartElement() && name() == "handle");
    QString text = readElementText();
    mCurrentClassItem.handleFun = text;

}
void CreateMessageDefine::readMessageAttribute()
{
    QString type = attributes().value("type");
    QString name = attributes().value("name");
}
void CreateMessageDefine::readMessageId()
{
    QString idname = attributes().value("name");
    QString className = attributes().value("class");
    if(mMsgType == CLIENT)
    {
        mClientMsgID.push_back(idname);
    }
    else
    {
        mServerMsgID.push_back(idname);
    }
    mCurrentClassItem.name = className;
}
