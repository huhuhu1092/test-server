#ifndef SGAMEMESSAGE_H
#define SGAMEMESSAGE_H
enum CLIENT_MSG_TYPE
{
    CREATE_OBJ,
    DELETE_OBJ,
    GIVE_OBJ_TO,
    GET_OBJ_FROM,
    RELEASE_OBJ,
    MOVE_TO, // this message has a destination point
    ATTACK, // attack other object
    SAY_TO, // say to someone
    NUM
};
enum SERVER_MSG_TYPE
{
    UPDATE_POSITION,
    UPDATE_DIRECTION,
    ERROR_MSG,
    NUM
};
class SCreateObjEvent : public SCommandEvent
{
public:
};
class SDeleteObjEvent : public SCommandEvent
{
public:
};
class SGiveObjToEvent : public SCommandEvent
{
public:
};
class SGetObjFromEvent : public SCommandEvent
{
public:
};
#endif
