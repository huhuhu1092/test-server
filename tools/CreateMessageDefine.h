#ifndef CREATEMESSAGEDEFINE_H
#define CREATEMESSAGEDEFINE_H
#include <QXmlStreamReader>
#include <QList>
class CreateMessageDefine : public QXmlStreamReader
{
public:
    CreateMessageDefine();
    int read(QIODevice* device);
private:
    void readMessageDefine();
    void readMessage();
    void readPack();
    void readUnpack();
    void readHandle();
    void readMessageAttribute();
    void readMessageId();
private:
    struct MsgClassItemSet
    {
        void clear()
        {
            name.clear();
            packFun.clear();
            unpackFun.clear();
            handleFun.clear();
            attr.clear();
        }
        QString name;
        QString packFun;
        QString unpackFun;
        QString handleFun;
        QList<QString> attr;
    }

private:
    enum {CLIENT , SERVER};
    MsgClassItemSet mCurrentClassItem;
    QList<QString> mClientMsgID;
    QString mServerMsgID;
    int mMsgType; //CLIENT or SERVER
    QList<MsgClassItemSet> mOutPutText; 
};
#endif
