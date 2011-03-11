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
import com.android.internal.telephony.Phone;
import android.provider.Telephony.Messaging;
import android.content.ServiceConnection;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.webkit.WebSettings;
import android.text.TextUtils;
import android.net.NetworkConnectivityListener;

public class SEHomeActivity extends Activity
{
    private static final String TAG = "SEHomeActivity";
    private static final int EVENT_DATA_STATE_CHANGED = 2;

    SEApplication mSEApp;
    SERenderView mRenderView;
    private boolean mKeepScreenOn = true;
    private MessageHandler mMsgH = new MessageHandler();
    private H mH = new H();
    private boolean mReadAppInfoFinish = false;
    private ArrayList<ApplicationInfo> mAppList = null;
    private static ServiceHandler mServiceHandler;
    private static NetworkConnectivityListener mConnectivityListener;
    boolean mConnected = false;
            private String mNetworkProfile = Phone.APN_TYPE_DEFAULT;
        private String mNetworkInterface;
        private String mNetworkApn;

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
    private final class ServiceHandler extends Handler {
        
        public void handleMessage(Message msg) {
            Log.e(TAG, "ServiceHandler: new message: " + msg);
            switch (msg.what) {
                case EVENT_DATA_STATE_CHANGED: {
                    if (mConnectivityListener == null) {
                        Log.e(TAG, "why conn listener is null?, will return");
                        return;
                    }
                    
                    NetworkInfo info = mConnectivityListener.getNetworkInfo();
                    if (info == null) {
                        Log.e(TAG, "Network Info is null ! ");
                        return;
                    }

                    if (!TextUtils.equals(info.getApType(), mNetworkProfile)) {
                        Log.e(TAG, "The msg is for (" + info.getApType()
                                + "),  Discard this msg.");
                        return;
                    }
                    
                    if (info.getState() == NetworkInfo.State.CONNECTED) {
                        mConnected = true;
                        Log.d(TAG, "mConnected changed to CONNECTED");

                        if(info.getApType().equals(Phone.APN_TYPE_INTERNET)) {
                            mNetworkInterface = null;
                        }else {
                            mNetworkInterface = info.getInterfaceName();
                        }
                        mNetworkApn = info.getExtraInfo();

                        Log.d(TAG, "bind socket to interface:" + mNetworkInterface + "/" + info.getApType()+", apn_new="+ mNetworkApn);
                        try {

                            Socket.setInterface(mNetworkInterface);
                        } catch (Exception e) {
                            // OMS: only catch this exception, wired crash need further investigation.
                            // #0107183 
                            mConnected = false;
                            Log.e(TAG, "!!! setup network failed, something really strange happened", e);
                            return ;
                        }
                        if(mConnected)
                        {
                            Thread loader = new Thread(new ApplicationLoader(), "Application Loader");
                            loader.start();

                        }
                    } else if (info.getState() == NetworkInfo.State.DISCONNECTED) {
                        String reason = info.getReason();
                        Log.d(TAG, "mConnected changed to DISCONNECTED " + info.getDetailedState() + "/" + reason);
                        mConnected = false;

                        // OMS: There's a trick in ConnectivityManager: the msg of 
                        // DISCONNECTED/DISCONNECTED in "internet" group is a sticky 
                        // one by Android design. 
                        // So we may need to ignore it during opening data connection.
                        if(info.getDetailedState() == NetworkInfo.DetailedState.CONNECTING) {
                            // Ignore if the NetworkInfo.State is DISCONNECTED while
                            // current NetworkInfo.DetailedState is still CONNECTING.
                            Log.d(TAG, "DISCONNECTED ignored as DetailedState is CONNECTING.");
                            return;
                        }

                        // This is the case when failed to open data connection.
                        // TODO: This may come from other app...
                        if(info.getDetailedState() == NetworkInfo.DetailedState.FAILED) {
                            Log.i(TAG, "### DISCONNECTED #############");
                            return;
                        } 

                        // Always ignor REASON_DATA_DISABLED when it's    
                        // from other network type or APN...
                        if(reason != null && reason.equals(Phone.REASON_DATA_DISABLED)) {
                            /* The following block is not used at present, hence we ignore all
                               REASON_DATA_DISABLEDs.
                            if (!TextUtils.equals(info.getApType(), mNetworkProfile)) {
                                Log.e(TAG, "Discard the msg for" + info.getApType());
                                return;
                            }
                            if (!TextUtils.equals(info.getExtraInfo(), mNetworkApn)) {
                                Log.e(TAG, "Discard the msg for" + info.getExtraInfo());
                                return;
                            }*/
                            return;
                        }
                    } else {
                        Log.e(TAG, " >>>>>>> status = "+info.getDetailedState() + "/" + info.getReason());
                    }
                    break;
                }
                default:
                    Log.d(TAG, "unhandled msg, msg="+msg);
                    break;
            }
        }
    }

    private class ApplicationLoader implements Runnable
    {
        ArrayList<ApplicationInfo> mApplicationList = new ArrayList<ApplicationInfo>();

        public void run()
        {
            Log.i(TAG, "##################start create socket ###################3\n");
            createSocket();
/*
            ConnectivityManager mConnMgr = (ConnectivityManager) SEHomeActivity.this.getSystemService(Context.CONNECTIVITY_SERVICE);
            int result = mConnMgr.startUsingNetworkFeature(
                                 ConnectivityManager.TYPE_MOBILE, mNetworkProfile);
            if (mConnectivityListener != null && mServiceHandler != null) {
                try {
                    mConnectivityListener.stopListening();
                    mConnectivityListener.unregisterHandler(mServiceHandler);
                }catch(Exception ee) {
                    Log.w(TAG, "Error when removing listener: ", ee);
                }finally {
                    Log.d(TAG, "set mConnectivityListener to null", new Throwable());
                    mConnectivityListener = null;
                    mServiceHandler = null;
                }
            }
            mServiceHandler = new ServiceHandler();
            mConnectivityListener = new NetworkConnectivityListener();
        try {
            mConnectivityListener.registerHandler(mServiceHandler, EVENT_DATA_STATE_CHANGED);
            mConnectivityListener.startListening(SEHomeActivity.this);

            boolean found = false;
            switch(result)
            {
            case Phone.APN_ALREADY_ACTIVE:
                NetworkInfo[] netInfos = mConnMgr.getAllNetworkInfo();
                for(NetworkInfo netInfo : netInfos) {
                    if(TextUtils.equals(mNetworkProfile, 
                            netInfo.getApType()) && netInfo.isConnected()) {
                        found = true;
                        // remember this for later query
                        mNetworkApn = netInfo.getExtraInfo();
                        mNetworkInterface = netInfo.getInterfaceName();
                        break;
                    }
                }
                if(!found)
                {
                    Log.w(TAG, "No matched NetInfo found!");
                    return;
                }
                if (mNetworkProfile.equals(Phone.APN_TYPE_DEFAULT)) {
                    mNetworkInterface = null;
                    Log.d(TAG, "###### network interface = " + mNetworkInterface + "   ###");
                }else {
                    Log.d(TAG, "bind socket to interface:" + mNetworkInterface + "/" + mNetworkProfile+", apn="+mNetworkApn);
                }
                break;
            case Phone.APN_REQUEST_STARTED:
                found = true;
                Log.d(TAG, "### APN_REQUEST_STARTED ###");
                break;

            case Phone.APN_TYPE_NOT_AVAILABLE:

                Log.d(TAG, "## APN_TYPE_NOT_AVAILABLE ##" );
                break;

            case Phone.APN_REQUEST_FAILED:
                Log.w(TAG, "Cannot establish data connection");
                break;

            default:
                Log.w(TAG, "Unhandled status: " + result);
                break;

            }
        } catch (Exception e) {
            Log.e(TAG, "Failed to start browser connection!!", e);
        }
*/
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
    void createSocket()
    {
            String name = new String("222.130.194.156");
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
            else
            {
                Log.i(TAG, "## can not connect to server ###");
            }
            Log.i(TAG, "#### socket close() ####");
            try {
                socket.close();
            }
            catch(Exception ex)
            {
                Log.i(TAG, "### close error ###");
            }

    }
    void connect()
    {
           ConnectivityManager mConnMgr = (ConnectivityManager) SEHomeActivity.this.getSystemService(Context.CONNECTIVITY_SERVICE);
            int result = mConnMgr.startUsingNetworkFeature(
                                 ConnectivityManager.TYPE_MOBILE, mNetworkProfile);
            if (mConnectivityListener != null && mServiceHandler != null) {
                try {
                    mConnectivityListener.stopListening();
                    mConnectivityListener.unregisterHandler(mServiceHandler);
                }catch(Exception ee) {
                    Log.w(TAG, "Error when removing listener: ", ee);
                }finally {
                    Log.d(TAG, "set mConnectivityListener to null", new Throwable());
                    mConnectivityListener = null;
                    mServiceHandler = null;
                }
            }
            mServiceHandler = new ServiceHandler();
            mConnectivityListener = new NetworkConnectivityListener();
        try {
            mConnectivityListener.registerHandler(mServiceHandler, EVENT_DATA_STATE_CHANGED);
            mConnectivityListener.startListening(this);

            boolean found = false;
            switch(result)
            {
            case Phone.APN_ALREADY_ACTIVE:
                NetworkInfo[] netInfos = mConnMgr.getAllNetworkInfo();
                for(NetworkInfo netInfo : netInfos) {
                    if(TextUtils.equals(mNetworkProfile, 
                            netInfo.getApType()) && netInfo.isConnected()) {
                        found = true;
                        // remember this for later query
                        mNetworkApn = netInfo.getExtraInfo();
                        mNetworkInterface = netInfo.getInterfaceName();
                        break;
                    }
                }
                if(!found)
                {
                    Log.w(TAG, "No matched NetInfo found!");
                    return;
                }
                if (mNetworkProfile.equals(Phone.APN_TYPE_DEFAULT)) {
                    mNetworkInterface = null;
                    Log.d(TAG, "###### network interface = " + mNetworkInterface + "   ###");
                }else {
                    Log.d(TAG, "bind socket to interface:" + mNetworkInterface + "/" + mNetworkProfile+", apn="+mNetworkApn);
                }
                createSocket();
                break;
            case Phone.APN_REQUEST_STARTED:
                found = true;
                Log.d(TAG, "### APN_REQUEST_STARTED ###");
                break;

            case Phone.APN_TYPE_NOT_AVAILABLE:

                Log.d(TAG, "## APN_TYPE_NOT_AVAILABLE ##" );
                break;

            case Phone.APN_REQUEST_FAILED:
                Log.w(TAG, "Cannot establish data connection");
                break;

            default:
                Log.w(TAG, "Unhandled status: " + result);
                break;

            }
        } catch (Exception e) {
            Log.e(TAG, "Failed to start browser connection!!", e);
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

            /*
            getWindow().setFlags(
                    WindowManager.LayoutParams.FLAG_FULLSCREEN,
                    WindowManager.LayoutParams.FLAG_FULLSCREEN);
            */
            if(mRenderView == null)
            {
                mRenderView = new SERenderView((Context) getApplication(), mSEApp);
                mRenderView.setMessageHandler(mMsgH);
            }
            setContentView(mRenderView);
            connect();
            //Thread loader = new Thread(new ApplicationLoader(), "Application Loader");
           // loader.start();
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
