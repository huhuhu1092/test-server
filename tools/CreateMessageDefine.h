#ifndef CREATEMESSAGEDEFINE_H
#define CREATEMESSAGEDEFINE_H
#include <QXmlStreamReader>
#include <QList>
class CreateMessageDefine : public QXmlStreamReader
{
public:
    CreateMessageDefine(const QString& saveFileName);
    int read(QIODevice* device);
    void save();
private:
    void readMessageDefine();
    void readMessage();
    void readPack();
    void readUnpack();
    void readHandle();
    void readConstructor();
    void readDestructor();
    void readMessageAttribute();
    void readMessageId();
    void readClientMessage();
    void readServerMessage();
private:
    struct Constructor
    { 
        void clear()
        {
            param.clear();
            body.clear();
        }
        QString param;
        QString body;
    };
    struct MsgClassItemSet
    {
        void clear()
        {
            name.clear();
            packFun.clear();
            unpackFun.clear();
            handleFun.clear();
            constructors.clear();
            destructor.clear();
            attrType.clear();
            attrName.clear();
        }
        QString name;
        QList<Constructor> constructors;
        QString destructor;
        QString packFun;
        QString unpackFun;
        QString handleFun;
        QList<QString> attrType;
        QList<QString> attrName;
    };

private:
    enum {CLIENT , SERVER};
    MsgClassItemSet mCurrentClassItem;
    QList<QString> mClientMsgID;
    QString mServerMsgID;
    int mMsgType; //CLIENT or SERVER
    QList<MsgClassItemSet> mOutputText; 
    QString mOutputFileName;
};
#endif
