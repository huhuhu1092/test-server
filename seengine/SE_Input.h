#ifndef SE_INPUT_H
#define SE_INPUT_H
#include "SE_Common.h"

#ifdef __cplusplus
extern "C" {
#endif
struct SE_World_tag;
enum SE_INPUT_TYPE {SE_KEYBOARD, SE_MOUSE};
enum SE_MOUSESTATE {SE_PRESSED , SE_MOVE, SE_RELEASED};
enum SE_KEYCODE {SE_KEY_LEFT, 
                 SE_KEY_RIGHT, 
                 SE_KEY_FORWARD, 
                 SE_KEY_BACK,
                 SE_KEY_F1, 
                 SE_KEY_F2, 
                 SE_KEY_F3,
                 SE_KEY_F4, 
                 SE_KEY_F5, 
                 SE_KEY_F6, 
                 SE_KEY_F7, 
                 SE_KEY_F8,
                 SE_KEY_NUM};
enum SE_MOUSECODE {SE_LEFTKEY, SE_RIGHTKEY, SE_MIDKEY, SE_MOUSEKEY_NUM};
typedef struct SE_InputEvent_tag
{
    enum SE_INPUT_TYPE inputType;
    float time;
    union 
    {
        struct
        {
            enum SE_MOUSESTATE mt;
            enum SE_MOUSECODE mc;
            float x, y;
        } mouse;
        struct
        {
            enum SE_KEYCODE key;
            int down;
        } keyboard;
    };
} SE_InputEvent;
typedef struct SE_MouseRecord_tag
{
    float x, y;
    enum SE_MOUSESTATE state;
} SE_MouseRecord;
typedef struct SE_InputDevice_tag
{
   int keyState[SE_KEY_NUM]; /*indicate which key is down. 1: down, 0: up*/ 
   SE_MouseRecord mouseState[SE_MOUSEKEY_NUM]; /*indicate which mouse key is pressing. */
   
} SE_InputDevice;

/***/
extern SE_Result SE_InputDevice_Init(SE_InputDevice* inputDevice);
/***
 * SE_HandleInputEvent has the resposibility to release sie
 * */
extern SE_Result SE_HandleInputEvent(struct SE_World_tag* world, SE_InputEvent* inputEvent);

#ifdef __cplusplus
}
#endif

#endif
