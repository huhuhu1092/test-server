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
import java.io.*;
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
//import android.provider.Telephony.Messaging;
import android.content.ServiceConnection;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.webkit.WebSettings;
import android.text.TextUtils;
import android.provider.Telephony;
import android.provider.Telephony.Carriers;
import android.database.Cursor;
import android.net.http.AndroidHttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.HttpHost;
import org.apache.http.conn.params.ConnRoutePNames;
import org.apache.http.Header;
import org.apache.http.HttpEntity;
import org.apache.http.entity.StringEntity;
import com.android.se.SEMessageContent;
//import android.net.NetworkConnectivityListener;
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
    //private static ServiceHandler mServiceHandler;
    //private static NetworkConnectivityListener mConnectivityListener;
    boolean mConnected = false;
            private String mNetworkProfile = "wap";//Phone.APN_TYPE_DEFAULT;
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
                    Log.i(TAG, "####### EVENT_DATA_STATE_CHANGED ##########");
                    /*                    
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
                            The following block is not used at present, hence we ignore all
                               REASON_DATA_DISABLEDs.
                            if (!TextUtils.equals(info.getApType(), mNetworkProfile)) {
                                Log.e(TAG, "Discard the msg for" + info.getApType());
                                return;
                            }
                            if (!TextUtils.equals(info.getExtraInfo(), mNetworkApn)) {
                                Log.e(TAG, "Discard the msg for" + info.getExtraInfo());
                                return;
                            }
                            
                            return;
                        }
                    } else {
                        Log.e(TAG, " >>>>>>> status = "+info.getDetailedState() + "/" + info.getReason());
                    }
                    */
                    break;
                }
                default:
                    Log.d(TAG, "unhandled msg, msg="+msg);
                    break;
            }
        }
    }
    private String proxyIP = "10.0.0.172";
    private int proxyPort = 80;
    private String serverURI = "http://222.130.193.248";

    private void startGame(String arg1, String arg2)
    {

        boolean ret = false;
        String outStr = new String();
         try {
                AndroidHttpClient androidHttpClient = AndroidHttpClient.newInstance("Opera/9.26");
                HttpHost proxy = new HttpHost(proxyIP, proxyPort);
                androidHttpClient.getParams().setParameter(ConnRoutePNames.DEFAULT_PROXY, proxy);
                HttpPost httpGet = new HttpPost(serverURI + "/cchess/start");
                String str = arg1 + " " + arg2;
                StringEntity sentity = new StringEntity(str);
                httpGet.setEntity(sentity);
                HttpResponse response = androidHttpClient.execute(httpGet);
                if(response.getStatusLine().getStatusCode() != HttpStatus.SC_OK)
                {
                    Log.i(TAG, "## get error ##");
                    ret = false;
                }
                else
                {
                    Log.i(TAG, "## get Ok ##");
                    ret = true;
                    Header[] httpHeader = response.getAllHeaders();
                    for(Header hd : httpHeader)
                    {
                        String n = hd.getName();
                        String v = hd.getValue();
                        Log.i(TAG, "#### " + n + " = " + v + " ###");
                    }
                    HttpEntity httpEntity = response.getEntity();
                    int contentSize = (int)httpEntity.getContentLength();
                    Log.i(TAG, "### content size = " + contentSize + " ######");
                    byte[] content = new byte[contentSize];
                    InputStream inputStream = httpEntity.getContent();
                    inputStream.read(content, 0, contentSize);
                    String ccstr = new String(content);
                    outStr = ccstr;
                    Log.i(TAG, "###########################");
                    Log.i(TAG, ccstr);
                    Log.i(TAG, "###########################");
                }
                androidHttpClient.close();
         }
         catch(Exception e)
         {
             Log.i(TAG, "## exception e = " + e);
         }
         if(ret)
        {
            sendStartCommand(outStr);
        }

    }
    private void move(String session, String color, String movestep)
    {
         try {
                AndroidHttpClient androidHttpClient = AndroidHttpClient.newInstance("Opera/9.26");
                HttpHost proxy = new HttpHost(proxyIP, proxyPort);
                androidHttpClient.getParams().setParameter(ConnRoutePNames.DEFAULT_PROXY, proxy);
                HttpPost httpGet = new HttpPost(serverURI + "/cchess/move");
                String str = session + " " + color + " " + movestep;
                StringEntity sentity = new StringEntity(str);
                httpGet.setEntity(sentity);
                HttpResponse response = androidHttpClient.execute(httpGet);
                if(response.getStatusLine().getStatusCode() != HttpStatus.SC_OK)
                {
                    Log.i(TAG, "## get error ##");
                }
                else
                {
                    Log.i(TAG, "## get Ok ##");
                    Header[] httpHeader = response.getAllHeaders();
                    for(Header hd : httpHeader)
                    {
                        String n = hd.getName();
                        String v = hd.getValue();
                        Log.i(TAG, "#### " + n + " = " + v + " ###");
                    }
                    HttpEntity httpEntity = response.getEntity();
                    int contentSize = (int)httpEntity.getContentLength();
                    Log.i(TAG, "### content size = " + contentSize + " ######");
                    byte[] content = new byte[contentSize];
                    InputStream inputStream = httpEntity.getContent();
                    inputStream.read(content, 0, contentSize);
                    String ccstr = new String(content);
                    Log.i(TAG, "###########################");
                    Log.i(TAG, ccstr);
                    Log.i(TAG, "###########################");
                }
                androidHttpClient.close();
         }
         catch(Exception e)
         {
             Log.i(TAG, "## exception e = " + e);
         }

    }
    private class ApplicationLoader implements Runnable
    {
        ArrayList<ApplicationInfo> mApplicationList = new ArrayList<ApplicationInfo>();
        public String command = new String();
        public String arg1 = new String();
        public String arg2 = new String();
        public String arg3 = new String();
        public void run()
        {
            Log.i(TAG, "##################start create socket ###################3\n");
            if(command.equals("login"))
            {
                Log.i(TAG, "########## login to server ##############");
                login(arg1, arg2);
            }
            else if(command.equals("start"))
            {
                Log.i(TAG, "#### start #####");
                startGame(arg1, arg2);
            }
            else if(command.equals("move"))
            {
                Log.i(TAG, "### move ####");
                move(arg1, arg2, arg3);
            }
        }
    }
    public boolean hasLogin = false;
    private class MessageHandler implements SERenderView.MessageHandler
    {
        public void handle(SEMessageContent msg)
        {
            Log.e("AAAA", "MessageHandler = " + msg);
            boolean tmpLogin = hasLogin;

            if(msg.equals("Box07") && mReadAppInfoFinish)
            {
                ApplicationInfo info = mAppList.get(0);
                startActivitySafely(info.intent);
            }
            else if(msg.command.equals("login"))
            {
                Log.e("AAA", "@@@@@@@@@@@@@@ login message handle ##############");
                if(!tmpLogin)
                {
                    ApplicationLoader la = new ApplicationLoader();
                    la.command = "login";
                    la.arg1 = msg.arg1;
                    la.arg2 = msg.arg2;
                    Thread loader = new Thread(la, "Application Loader");
                    loader.start();
                }
                
            }
            else if(msg.command.equals("start"))
            {
                Log.e("AAA", "####### start message handle ####");
                if(tmpLogin)
                {
                    ApplicationLoader la = new ApplicationLoader();
                    la.command = "start";
                    la.arg1 = msg.arg1;
                    la.arg2 = msg.arg2;
                    Thread loader = new Thread(la, "Application Loader");
                    loader.start();

                }
            }
            else if(msg.command.equals("move"))
            {
                String session = msg.arg1;
                String color = msg.arg2;
                String movestep = msg.arg3;
                ApplicationLoader la = new ApplicationLoader();
                la.command = "move";
                la.arg1 = session;
                la.arg2 = color;
                la.arg3 = movestep;
                Thread loader = new Thread(la, "Application Loader");
                loader.start();

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
    private class CommandRunnable implements Runnable
    {
        public String command = new String();
        public String arg1 = new String();
        public String arg2 = new String();
        public String arg3 = new String();
        public void run()
        {
            if(command.equals("login"))
            {
                mSEApp.runCommand1(command, arg1);
            }
            else if(command.equals("start"))
            {
                mSEApp.runCommand1(command, arg1);
            }
        }
    }

    void sendLoginCommand(String str)
    {

        CommandRunnable r = new CommandRunnable();
        r.command = "login";
        r.arg1 = str;
        mRenderView.queueEvent(r);

    }
    void sendStartCommand(String str)
    {
        CommandRunnable r = new CommandRunnable();
        r.command = "start";
        r.arg1 = str;
        mRenderView.queueEvent(r);
    }
    void login(String arg1 , String arg2)
    {
        boolean tmpLogin = false;
        boolean ret = false;
        String outStr = new String();
         try {
                AndroidHttpClient androidHttpClient = AndroidHttpClient.newInstance("Opera/9.26");
                HttpHost proxy = new HttpHost(proxyIP, proxyPort);
                androidHttpClient.getParams().setParameter(ConnRoutePNames.DEFAULT_PROXY, proxy);
                HttpPost httpGet = new HttpPost(serverURI + "/cchess/login");
                StringEntity sentity = new StringEntity(arg1 + " " + arg2);
                httpGet.setEntity(sentity);
                HttpResponse response = androidHttpClient.execute(httpGet);
                if(response.getStatusLine().getStatusCode() != HttpStatus.SC_OK)
                {
                    Log.i(TAG, "## get error ##");
                    ret = false;
                }
                else
                {
                    Log.i(TAG, "## get Ok ##");
                    ret = true;
                    Header[] httpHeader = response.getAllHeaders();
                    for(Header hd : httpHeader)
                    {
                        String n = hd.getName();
                        String v = hd.getValue();
                        Log.i(TAG, "#### " + n + " = " + v + " ###");
                    }
                    HttpEntity httpEntity = response.getEntity();
                    int contentSize = (int)httpEntity.getContentLength();
                    Log.i(TAG, "### content size = " + contentSize + " ######");
                    byte[] content = new byte[contentSize];
                    InputStream inputStream = httpEntity.getContent();
                    inputStream.read(content, 0, contentSize);
                    String str = new String(content);
                    tmpLogin = true;
                    outStr = str;
                    Log.i(TAG, "###########################");
                    Log.i(TAG, "###########################");
                }
                androidHttpClient.close();
         }
         catch(Exception e)
         {
             Log.i(TAG, "## exception e = " + e);
         }
         if(ret)
         {
             hasLogin = tmpLogin;
             sendLoginCommand(outStr);
         }

        /* 
            String name = new String("10.0.0.172");
            Socket socket = null;
            try {
                    socket = new Socket(name, 80);
            }
            catch(Exception ex)
            {
                Log.i(TAG, "#### socket create error ###");
            }

            if(socket != null && socket.isConnected())
            {
                Log.i(TAG, " ##### connect to server ####");
            }
            else
            {
                Log.i(TAG, "## can not connect to server ###");
            }
             /// write and read data
            OutputStreamWriter out = null;
            try
            {
                OutputStream raw = socket.getOutputStream();
                OutputStream buffered = new BufferedOutputStream(raw);
                out = new OutputStreamWriter(buffered, "ASCII");
                String outData = "GET / HTTP/1.1\r\nHost: wap.qidian.cn\r\nUser-Agent: Opera/9.26\r\nACCEPT-ENCODING:deflate, gzip, x-gzip, identity, *;q=0\r\nACCEPT-CHARSET:iso-8859-1, utf-8, utf-16, *;q=0.1\r\nACCEPT:text/html, text/vnd.wap.wml, text/plain\r\n\r\n";
                out.write(outData, 0, outData.length());
            }
            catch(Exception e)
            {
                Log.i(TAG, "##### exception error when write data ###");
            }
            finally
            {

            }
            Log.i(TAG, "#### write data #####");
            InputStream inputStream = null;
            try
            {
                inputStream = socket.getInputStream();
                BufferedReader in = new BufferedReader(new InputStreamReader(inputStream));
                String str = null;
                do
                {
                    str = in.readLine();
                    if(str != null)
                        Log.i(TAG, "### " + str + " ####");
                } while(str != null);
            }
            catch(Exception e)
            {
                Log.i(TAG, "#### read data exception " + e + "####");
            }
            finally
            {

            }
            ///
            Log.i(TAG, "#### socket close() ####");
            try {

                if(socket == null)
                {
                    socket.close();
                }
            }
            catch(Exception ex)
            {
                Log.i(TAG, "### close error ###");
            }
        */   
    }
    static String[] CARRIER_PROJECTION = { 
            Carriers.TYPE, 
            Carriers.APN, 
            Carriers.PROXY, 
            Carriers.PORT };
   static int COL_TYPE = 0;
    static int COL_APN = 1;
    static int COL_PROXY = 2;
    static int COL_PORT = 3;
    ProfileAttribute mNetworkParam;
    public class ProfileAttribute
    {
        public String mApnName;
        public String mType;
        public String mProxyHost;
        public int mProxyPort = 0;
        public void reset()
        {
            mApnName = "";
            mType = "";
            mProxyHost = null;
            mProxyPort = 0;
        }
    }
    private int getPort(String port) {
        // Should we use always use default 80??
        if(port == null || port.length() <= 0) return 80;

        int ret = 0;
        try {
            ret = Integer.parseInt(port.trim());
        }catch(Exception ee) {
            Log.w(TAG, "Invalid port");
        }
        // Should we use always use default 80??
        return ret > 0 ? ret : 80;
    }

    void loadNetworkParam()
    {
        final String where = "(" + Carriers.TYPE + " = '" + mNetworkProfile + "') AND ( " + Carriers.APN + " = '" + mNetworkApn + "' )";
        Cursor c = getContentResolver().query(Telephony.Carriers.CONTENT_URI, 
                CARRIER_PROJECTION, where, null, null) ; 
        if(c == null)
        {
            Log.e(TAG, "****** No proxy############");
            return;
        }
        try {
            while(c.moveToNext())
            {
                String type = c.getString(COL_TYPE);
                String apn = c.getString(COL_APN);
                Log.i(TAG, ">>>> type = " + type + ", apn = " + apn + " #####");
                if(type.equals(mNetworkProfile) && mNetworkApn.equals(apn))
                {
                    mNetworkParam = new ProfileAttribute();
                    mNetworkParam.mType = mNetworkProfile; 
                    mNetworkParam.mApnName = mNetworkApn; 
                    mNetworkParam.mProxyHost = c.getString(COL_PROXY);
                    String port = c.getString(COL_PORT);
                    Log.d(TAG, ">>>>>>>>>>>>>> proxy="+mNetworkParam.mProxyHost+", port="+port);
                    mNetworkParam.mProxyPort = getPort(port);
                    c.close();
                    //return pa;
                    return;

                }

            }
        }
        catch(Exception e)
        {
            Log.i(TAG, " ################## exception e = " + e);
        }
        finally {
            c.close();
        }
    }
    void setupNetwork()
    {

    }
    void connect()
    {
           ConnectivityManager mConnMgr = (ConnectivityManager) SEHomeActivity.this.getSystemService(Context.CONNECTIVITY_SERVICE);
            int result = mConnMgr.startUsingNetworkFeature(
                                 ConnectivityManager.TYPE_MOBILE, mNetworkProfile);
            /*
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
            */
        try {
            //mConnectivityListener.registerHandler(mServiceHandler, EVENT_DATA_STATE_CHANGED);
            //mConnectivityListener.startListening(this);

            boolean found = false;
            NetworkInfo[] netInfos;
            switch(result)
            {
            case Phone.APN_ALREADY_ACTIVE:
                netInfos = mConnMgr.getAllNetworkInfo();
                for(NetworkInfo netInfo : netInfos) {
                    /*
                    if(TextUtils.equals(mNetworkProfile, 
                            netInfo.getApType()) && netInfo.isConnected()) {
                        found = true;
                        // remember this for later query
                        mNetworkApn = netInfo.getExtraInfo();
                        mNetworkInterface = netInfo.getInterfaceName();
                        break;
                    }
                    */
                    Log.i(TAG, "#### network info name = " + netInfo.getTypeName() + "####" );
                }
                /*
                if(!found)
                {
                    Log.w(TAG, "No matched NetInfo found!");
                    return;
                }
                */
                if (mNetworkProfile.equals(Phone.APN_TYPE_DEFAULT)) {
                    mNetworkInterface = null;
                    Log.d(TAG, "###### network interface = " + mNetworkInterface + "   ###");
                }else {
                    Log.d(TAG, "bind socket to interface:" + mNetworkInterface + "/" + mNetworkProfile+", apn="+mNetworkApn);
                }
                //createSocket();
                setupNetwork();
                break;
            case Phone.APN_REQUEST_STARTED:
                netInfos = mConnMgr.getAllNetworkInfo();
                for(NetworkInfo netInfo : netInfos) {
                    /*
                    if(TextUtils.equals(mNetworkProfile, 
                            netInfo.getApType()) && netInfo.isConnected()) {
                        found = true;
                        // remember this for later query
                        mNetworkApn = netInfo.getExtraInfo();
                        mNetworkInterface = netInfo.getInterfaceName();
                        break;
                    }
                    */
                    Log.i(TAG, "#### network info name = " + netInfo.getTypeName() + "####" );
                }

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

            getWindow().setFlags(
                    WindowManager.LayoutParams.FLAG_FULLSCREEN,
                    WindowManager.LayoutParams.FLAG_FULLSCREEN);
            if(mRenderView == null)
            {
                mRenderView = new SERenderView((Context) getApplication(), mSEApp);
                mRenderView.setMessageHandler(mMsgH);
            }
            setContentView(mRenderView);
            //connect();
            //Thread loader = new Thread(new ApplicationLoader(), "Application Loader");
            //loader.start();
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
