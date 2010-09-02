package com.android.se;
import java.nio.Buffer;
import java.lang.String;
public class SEApplication
{
    /*
     * the key and motion event definition is the same as the define in SE_InuptEvent.h
     * */
    public static final int KEY_PRESS = 0;
    public static final int KEY_RELEASE = 1;
    public static final int MOTION_DOWN = 0;
    public static final int MOTION_UP = 1;

    public static final int KEY_UP = 0;
	public static final int KEY_DOWN = 1;
	public static final int	KEY_LEFT = 2;
	public static final int	KEY_RIGHT = 3;
	public static final int	KEY_F1 = 4;
	public static final int	KEY_F2 = 5;
	public static final int	KEY_F3 = 6;
	public static final int	KEY_F4 = 7;
	public static final int	KEY_MENU = 8;
	public static final int	KEY_1 = 9;
	public static final int	KEY_2 = 10;
	public static final int KEY_3 = 11;
	public static final int KEY_4 = 12;
	public static final int KEY_5 = 13;
	public static final int KEY_6 = 14;
	public static final int KEY_7 = 15;
	public static final int KEY_8 = 16;
	public static final int KEY_9 = 17;
	public static final int KEY_0 = 18;
	public static final int KEY_NUM = 19;

    static 
    {
        System.loadLibrary("se");
    }
    public SEApplication()
    {
    }
    public native void init(int userid, String dataPath, String sceneName);
    public native void destroy();
    public native void resize(int width, int height);
    public native void sendKeyCommand(int keyType, int keyCode);
    public native void sendMotionCommand(int motionType, int x, int y);
    public native void sendLoadSceneCommand(String name);
    public native void sendUpdateCameraCommand(int width, int height);
    public native String getResponseName();
    public native int getResponseContentSize();
    public native String getResponseStringValue();
    public native void runOneFrame();
}