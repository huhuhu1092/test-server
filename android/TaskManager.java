package com.android.systemui.statusbar;
import com.android.systemui.R;

import android.app.ActivityManager;
import android.app.ActivityManager.RunningAppProcessInfo;
import android.app.ActivityManagerNative;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.graphics.drawable.Drawable;
import android.util.AttributeSet;
import android.util.Log;
import android.os.Handler;
import android.os.Message;
import android.os.Process;
import android.os.RemoteException;
import android.provider.Telephony;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.ImageView;
import android.widget.TextView;
import android.util.Xml;
import android.content.pm.PackageManager.NameNotFoundException;

import java.util.Properties;
import java.io.FileInputStream;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;

//add lib
import org.xmlpull.v1.XmlPullParser;
import com.android.internal.util.XmlUtils;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.RandomAccessFile;
import java.io.Reader; 
public class TaskManager extends LinearLayout {

    private static final String TAG = "TaskManager";
    private ArrayList<RunningAppProcessInfo> mInfo;    
    private PackageManager mPm; 
    private ActivityManager mAm;
    private ArrayList<String> mFilteredProcess = new ArrayList<String>();
    
    private Context mContext;
    private LayoutInflater mInflater;
    
    private boolean mUpdating = false;
    
    private final BroadcastReceiver mApplicationsReceiver = new ApplicationsIntentReceiver();    
    private HashMap<String, ResolveInfo> mApplications;
    private Drawable mDefaultIcon;
    
    private static final int EVENT_UPDATE = 1;
        Handler mHandler = new Handler() {
        @Override
        public void handleMessage(Message msg) {            
            switch (msg.what) {
                case EVENT_UPDATE:
               	    update();
               	    Message msg2 = Message.obtain(mHandler, EVENT_UPDATE);
                    mHandler.sendMessageDelayed(msg2, 5000);
                    break;
            }
        }
    };
    public TaskManager(Context context, AttributeSet attrs) {
        super(context, attrs);
        mContext = context;
        
        IntentFilter filter = new IntentFilter(Intent.ACTION_PACKAGE_ADDED);
        filter.addAction(Intent.ACTION_PACKAGE_REMOVED);
        filter.addAction(Intent.ACTION_PACKAGE_CHANGED);
        filter.addDataScheme("package");
        mContext.registerReceiver(mApplicationsReceiver, filter);     
        mPm = mContext.getPackageManager();   
        mAm = (ActivityManager)mContext.getSystemService(Context.ACTIVITY_SERVICE);

    }
    public void setUpdates(boolean update) {
    	if (update != mUpdating) {
            mUpdating = update;
            if (update) {
            	getApplications(true);
                
                update();
                Message msg = Message.obtain(mHandler, EVENT_UPDATE);
                mHandler.sendMessageDelayed(msg, 5000);
            } else {
            	mHandler.removeMessages(EVENT_UPDATE);
            }
    	}    	
    }
    public void killAll() {
        for (int i = 0; i < getChildCount(); i++) {
        	String processName = (String) getChildAt(i).getTag();//((Integer) getChildAt(i).getTag()).intValue();
        	
        	try {
                mAm.killBackgroundProcesses(processName);
            } catch (Exception e) {}
        }
    }   
    private void getApplications(boolean expanded) {
        if (expanded && mApplications != null)
            return;
        
        if (mApplications == null)
        	mApplications = new HashMap<String, ResolveInfo>();
        
        mApplications.clear();
        
    	PackageManager manager = mContext.getPackageManager();

        Intent mainIntent = new Intent(Intent.ACTION_MAIN, null);
        mainIntent.addCategory(Intent.CATEGORY_LAUNCHER);
        final List<ResolveInfo> apps = manager.queryIntentActivities(mainIntent, 0);
        for (int i = 0; i < apps.size(); i++) {                
            ResolveInfo info = apps.get(i);
            String name = info.activityInfo.applicationInfo.packageName;
            if (mApplications.containsKey(name))
                continue;
            
            mApplications.put(name, info);
        }
        mDefaultIcon = mContext.getPackageManager().getDefaultActivityIcon();
    }
    private void initFilteredProcess(){
        mFilteredProcess.add("com.android.email");
        mFilteredProcess.add("com.android.mms");
        mFilteredProcess.add("com.android.launcher");
        mFilteredProcess.add("com.android.providers.calendar");
        mFilteredProcess.add("com.android.bluetooth");
        mFilteredProcess.add("android.process.media");    
        mFilteredProcess.add("system");
        mFilteredProcess.add("com.android.systemui");
        mFilteredProcess.add("com.android.phone");
        mFilteredProcess.add("android.process.acore");
        mFilteredProcess.add("com.android.providers.userdictionary");
        mFilteredProcess.add("com.android.inputmethod.latin");    
      
    }
    
    private boolean isFiltered(String packageName) {
        for (int i = 0; i < mFilteredProcess.size(); i++) {
            if (packageName.equals(mFilteredProcess.get(i)))
                return true;
        }
        
        return false;
    }
    private List<ActivityManager.RunningAppProcessInfo> getRunningAppProcessesList() {
        return mAm.getRunningAppProcesses();
    }
    private List<ApplicationInfo> getAppRunning()
    {
        List<ApplicationInfo> appList =new ArrayList<ApplicationInfo> ();
        List<ActivityManager.RunningAppProcessInfo> procList = getRunningAppProcessesList();
        if ((procList == null) || (procList.size() == 0)) {
            return appList;
        }
        // Retrieve running processes from ActivityManager
        for (ActivityManager.RunningAppProcessInfo appProcInfo : procList) {
            if ((appProcInfo != null)  && (appProcInfo.pkgList != null)){
                int size = appProcInfo.pkgList.length;
                Log.i(TAG, "app running pkg size = " + size + " #####");
                Log.i(TAG, "app running processName = " + appProcInfo.processName + " ####");
                for (int i = 0; i < size; i++) {
                    ApplicationInfo appInfo = null;
                    try {
                        appInfo = mPm.getApplicationInfo(appProcInfo.pkgList[i], 
                                PackageManager.GET_UNINSTALLED_PACKAGES);
                    } catch (NameNotFoundException e) {
                       Log.w(TAG, "Error retrieving ApplicationInfo for pkg:"+appProcInfo.pkgList[i]);
                       continue;
                    }
                    if(appInfo != null) {
                        Log.i(TAG, "app running appInfo = " + mPm.getApplicationLabel(appInfo) + " #####");
                        appList.add(appInfo);
                    }
                }
            }
        }
        return appList;
    }
    private void update() {
    	PackageManager pm = mContext.getPackageManager();
    	if (mInflater == null)
            mInflater = (LayoutInflater) mContext.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
        //debug
        //List<ApplicationInfo> ppp = getAppRunning();
        //end 
        mInfo = (ArrayList<ActivityManager.RunningAppProcessInfo>)getRunningAppProcessesList();
        if (mInfo == null) {
            mInfo = new ArrayList<RunningAppProcessInfo>(0);
        }
        
        if(mFilteredProcess.size() == 0){
        	initFilteredProcess();
        }
        
        removeAllViews();
        
        for (int i = 0; i < mInfo.size(); i++) {
            
            if (isFiltered(mInfo.get(i).processName))
            	continue;
            
            ResolveInfo ri = mApplications.get(mInfo.get(i).processName);
            
            View view = mInflater.inflate(R.layout.status_bar_task_item, this, false);
             
            ImageView icon = (ImageView)view.findViewById(R.id.task_icon);
            TextView name = (TextView)view.findViewById(R.id.task_name);
            if (ri != null) {            	
                icon.setImageDrawable(ri.activityInfo.loadIcon(pm));  
                Log.i(TAG, "######## name = " + ri.loadLabel(pm));      
                name.setText(ri.loadLabel(pm));
            } else {                
                ApplicationInfo ai = null;
                try {
                    ai = pm.getApplicationInfo(mInfo.get(i).processName, 0);
                } catch (Exception e) {                	
                }
                if (ai != null) {
                	icon.setImageDrawable(pm.getApplicationIcon(ai));
                    Log.i(TAG, "########2 name = " + pm.getApplicationLabel(ai));     
                	name.setText(pm.getApplicationLabel(ai));
                } else {
                	icon.setImageDrawable(pm.getDefaultActivityIcon());
                	name.setText(mInfo.get(i).processName);
                }
            }            

            View button = view.findViewById(R.id.task_end_button);
            button.setOnClickListener(new Launcher(mInfo.get(i).pid, mInfo.get(i).processName));
            
            view.setTag(mInfo.get(i).processName);
            addView(view);
        }
    }
    private class Launcher implements View.OnClickListener {
        private int mPid;
        private String mProcessName;

        Launcher(int pid, String processName) {
            mPid = pid;
            mProcessName = processName;
        }

        public void onClick(View v) {
            try {
                //Process.killProcess(mPid);
                mAm.killBackgroundProcesses(mProcessName);
                Log.i(TAG, "killed process pid = " + mPid + ", name = " + mProcessName) ;
            } catch (Exception e) {}

            mHandler.removeMessages(EVENT_UPDATE);
            Message msg = Message.obtain(mHandler, EVENT_UPDATE);
            mHandler.sendMessageDelayed(msg, 1000);
        }
    }
    private class ApplicationsIntentReceiver extends BroadcastReceiver {
        public void onReceive(Context context, Intent intent) {            
            getApplications(false);
        }
    }    
 
}
