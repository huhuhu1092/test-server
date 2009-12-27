#include "CreateMessageDefine.h"
#include <QFile>
#include <QIODevice>
CreateMessageDefine::CreateMessageDefine(const QString& saveFileName)
{
    mOutputFileName = saveFileName;
    
}
int CreateMessageDefine::read(QIODevice* device)
{
    setDevice(device);
    while(!atEnd())
    {
        readNext();
        if(isStartElement())
        {
            if(name() == "msgdefine" && attributes().value("version").toString() == "1.0")
            {
                readMessageDefine();
            }
            else
            {
                raiseError(QObject::tr("The file is not a message define file"));
            }
        }
    }
    return !error();
}
void CreateMessageDefine::readMessageDefine()
{
    while(!atEnd())
    {
        readNext();
        if(isEndElement() && name() == "msgdefine")
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
        if(isEndElement() && name() == "client")
            break;
        if(isStartElement())
        {
            if(name() == "message")
            {
                mCurrentClassItem.clear();
                readMessage();
                mOutputText.push_back(mCurrentClassItem);
            }
        }
    }

}
void CreateMessageDefine::readServerMessage()
{
    while(!atEnd())
    {
        readNext();
        if(isEndElement() && name() == "server")
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
void CreateMessageDefine::readMessage()
{
    while(!atEnd())
    {
        readNext();
        QString n = name().toString();
        if(isEndElement() && name() == "message")
            break;
        if(isStartElement())
        {
            if(name() == "id")
            {
                readMessageId();
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
            else if(name() == "constructor")
            {
                readConstructor();
            }
            else if(name() == "destructor")
            {
                readDestructor();
            }
            else if(name() == "attribute")
            {
                readMessageAttribute();
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
    QString type = attributes().value("type").toString();
    QString name = attributes().value("name").toString();
    mCurrentClassItem.attrType.push_back(type);
    mCurrentClassItem.attrName.push_back(name);
}
void CreateMessageDefine::readMessageId()
{
    QString idname = attributes().value("name").toString();
    QString className = attributes().value("class").toString();
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
void CreateMessageDefine::readConstructor()
{
    Q_ASSERT(isStartElement() && name() == "constructor");
    Constructor c;
    c.param =  attributes().value("param").toString();
    QString text = readElementText();
    c.body = text;
    mCurrentClassItem.constructors.push_back(c); 
}
void CreateMessageDefine::readDestructor()
{
    Q_ASSERT(isStartElement() && name() == "destructor");
    QString text = readElementText();
    mCurrentClassItem.destructor = text; 
}
void CreateMessageDefine::save()
{        
    QFile file(mOutputFileName);
    if(!file.open(QFile::WriteOnly))
    {
        return;
    }    
    QString output;
    output += (QString("#ifndef SMESSAGEDEFINE_H\n") + QString("#define SMESSAGEDEFINE_H\n"));
    output += QString("enum CLIENT_MSG_TYPE {\n");
    for(int i = 0 ; i < mClientMsgID.count() ; i++)
    {
        QString str = mClientMsgID.at(i);
        output += QString("    ") + str + QString(",\n");
    }
    output += QString("    ") + QString("NUM") + QString("\n") + QString("};\n"); 
    output += QString("enum SERVER_MSG_TYPE {\n");
    for(int i = 0 ; i < mServerMsgID.count(); i++)
    {
        QString str = mServerMsgID.at(i);
        output += QString("    ") + str + QString(",\n");
    }
    output += QString("    ") + QString("NUM") + QString("\n") + QString("};\n");
    for(int i = 0 ; i < mOutputText.count() ; i++)
    {
        MsgClassItemSet mcis = mOutputText.at(i);
        output += QString("class") + QString(" ") + mcis.name + QString(" :") + QString(" public SCommandEvent\n");
        output += QString("{\n");
        output += QString("public:\n");
        for(int j = 0 ; j < mcis.constructors.count() ; j++)
        { 
            Constructor c = mcis.constructors.at(j);
            output += QString("    ") + mcis.name + QString("param") + QString("\n");
            output += QString("    ") + c.body + QString("\n");
        }
        output += QString("    ") + QString("void pack(char*& out, int& len)") + QString("\n");
        output += QString("    ") + mcis.packFun + QString("\n");
        output += QString("    ") + QString("void unpack(const char* input)") + QString("\n");
        output += QString("    ") + mcis.unpackFun + QString("\n");
        output += QString("    ") + QString("bool handle()") + QString("\n");
        output += QString("    ") + mcis.handleFun + QString("\n");
        output += QString("protected:\n");
        for(int i = 0 ; i < mcis.attrType.count() ; i++)
        {
            QString type = mcis.attrType.at(i);
            QString name = mcis.attrName.at(i);
            output += QString("    ") + type + QString(" ") + name + QString(";\n");
        }
        output += QString("\n};\n");
    }
    output += QString("#endif\n");
    QByteArray data = output.toAscii();
    int i = file.write(data);
    
}

