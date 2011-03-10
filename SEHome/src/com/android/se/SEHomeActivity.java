package com.android.se;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.os.Looper;
import android.os.MessageQueue;
import android.view.WindowManager;
import android.view.Window;
import android.graphics.PixelFormat;
import android.os.Bundle;
import android.util.Log;
import android.content.pm.ActivityInfo;
import android.content.res.Configuration;
import com.android.se.SEApplication;
import com.android.se.SERenderView;
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import java.util.ArrayList;
import android.util.Log;
import android.content.ComponentName;
import android.content.ContentResolver;
import android.content.ContentValues;
import android.content.Intent;
import android.content.Context;
import android.content.pm.ActivityInfo;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import java.util.List;
import android.content.ActivityNotFoundException;
import android.widget.Toast;
import java.io.File;
import java.io.BufferedReader;  
import java.io.BufferedWriter;  
import java.io.InputStreamReader;  
import java.io.OutputStreamWriter;  
import java.io.PrintWriter;  
import java.net.Socket;  
import android.util.Log;
import java.lang.String;
public class SEHomeActivity extends Activity
{
    private static final String TAG = "SEHomeActivity";
    SEApplication mSEApp;
    SERenderView mRenderView;
    private boolean mKeepScreenOn = true;
    private MessageHandler mMsgH = new MessageHandler();
    private H mH = new H();
    private boolean mReadAppInfoFinish = false;
    private ArrayList<ApplicationInfo> mAppList = null;
    private class H extends Handler
    {
        public static final int READAPPINFO_FINISH = 0;;
        public void handleMessage(Message msg)
        {
            switch(msg.what)
            {
            case READAPPINFO_FINISH:
                Log.e(TAG, "read app info finish ");
                mReadAppInfoFinish = true;
                mAppList = (ArrayList<ApplicationInfo>)msg.obj;
                break;
            }
        }
    }
    class ApplicationInfo 
    {
        CharSequence title;

        Intent intent;
        boolean filtered;



        ApplicationInfo() {
        }
        
        public ApplicationInfo(ApplicationInfo info) {
            title = info.title.toString();
            intent = new Intent(info.intent);
            filtered = info.filtered;
        }

        final void setActivity(ComponentName className, int launchFlags) {
            intent = new Intent(Intent.ACTION_MAIN);
            intent.addCategory(Intent.CATEGORY_LAUNCHER);
            intent.setComponent(className);
            intent.setFlags(launchFlags);
        }

        public String toString() {
            return title.toString();
        }
    }
    private class ApplicationLoader implements Runnable
    {
        ArrayList<ApplicationInfo> mApplicationList = new ArrayList<ApplicationInfo>();
        public void run()
        {
            String name = new String("222.130.196.2");
            Socket socket = null;
            try {
                socket = new Socket(name, 10000);
            }
            catch(Exception ex)
            {
                Log.i(TAG, "#### socket create error ###");
            }
            if(socket.isConnected())
            {
                Log.i(TAG, " ##### connect to server ####");
            }
            /*
            Intent mainIntent = new Intent(Intent.ACTION_MAIN, null);
            mainIntent.addCategory(Intent.CATEGORY_LAUNCHER);
            final PackageManager manager = getPackageManager();
            final List<ResolveInfo> apps = manager.queryIntentActivities(mainIntent, 0);
            if(apps != null)
            {
                final int count = apps.size();
                for(int i = 0 ; i < count ; i++)
                { 
                    ApplicationInfo application = new ApplicationInfo();
                    ResolveInfo info = apps.get(i);
                    application.title = info.loadLabel(manager);
                    if(application.title == null)
                        application.title = info.activityInfo.name;
                    Log.e(TAG, "## tile = " + application.title + " ####");
                    application.setActivity(new ComponentName(
                            info.activityInfo.applicationInfo.packageName,
                            info.activityInfo.name),
                            Intent.FLAG_ACTIVITY_NEW_TASK |
                            Intent.FLAG_ACTIVITY_RESET_TASK_IF_NEEDED);

                    mApplicationList.add(application);
                }
            }
            Message msg = Message.obtain();
            msg.what = H.READAPPINFO_FINISH;
            msg.obj = mApplicationList;
            mH.sendMessage(msg);
            */
        }
    }
    private class MessageHandler implements SERenderView.MessageHandler
    {
        public void handle(String msg)
        {
            Log.e("AAAA", "MessageHandler = " + msg);
            if(msg.equals("Box07") && mReadAppInfoFinish)
            {
                ApplicationInfo info = mAppList.get(0);
                startActivitySafely(info.intent);
            }
        }
    }
    void startActivitySafely(Intent intent)
    {
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        try {
            startActivity(intent);
        } catch (ActivityNotFoundException e) {
            Toast.makeText(this, R.string.activity_not_found, Toast.LENGTH_SHORT).show();
        }
    }

    @Override 
    protected void onCreate(Bundle icicle) 
    {
        super.onCreate(icicle);
        Log.i("SEActivity", "#### onCreate ####");
        super.setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_SENSOR);
        if(foundSEData())
        {
            if(mSEApp == null)
            {
                mSEApp = new SEApplication();
            }
            if(mKeepScreenOn)
            {
                getWindow().setFlags(
                        WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON,
                        WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
            }

            getWindow().setFlags(
                    WindowManager.LayoutParams.FLAG_FULLSCREEN,
                    WindowManager.LayoutParams.FLAG_FULLSCREEN);

            if(mRenderView == null)
            {
                mRenderView = new SERenderView((Context) getApplication(), mSEApp);
                mRenderView.setMessageHandler(mMsgH);
            }
            setContentView(mRenderView);
            Thread loader = new Thread(new ApplicationLoader(), "Application Loader");
            loader.start();
        }
    }
    private boolean foundSEData() {
        return true;
    }

    private boolean fileExists(String s) {
        File f = new File(s);
        return f.exists();
    }
    @Override
    protected void onResume() {
        // Ideally a game should implement onResume() and onPause()
        // to take appropriate action when setIsCollidablethe activity looses focus
        super.onResume();
        Log.e("AGG", "#### onResume ####"); 
        mRenderView.onResume();
    }

    @Override
    protected void onPause() {
        // Ideally a game should implement onResume() and onPause()
        // to take appropriate action when the activity looses focus
        super.onPause();
        Log.e("AGG", "#### onPause ####"); 
        mRenderView.onPause();
    }


}
