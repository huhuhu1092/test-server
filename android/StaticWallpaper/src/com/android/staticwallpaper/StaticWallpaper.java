/*
 * Copyright (C) 2009 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.android.staticwallpaper;
import android.content.res.Resources;
import android.service.wallpaper.WallpaperService;
import android.os.Bundle;
import android.renderscript.RenderScript;
import android.view.SurfaceHolder;
import android.view.Surface;
import android.graphics.PixelFormat;
import android.graphics.Bitmap;
import android.graphics.drawable.BitmapDrawable;
import android.service.wallpaper.WallpaperService;
import android.util.Log;
import android.view.MotionEvent;
import android.view.SurfaceHolder;
import android.content.Context;
import android.content.IntentFilter;
import android.content.Intent;
import android.content.BroadcastReceiver;
import android.app.WallpaperManager;
import android.graphics.drawable.Drawable;
import android.os.HandlerThread;
import android.os.Process;
import android.database.ContentObserver;
import android.provider.Contacts;
import android.provider.ContactsContract;
import android.telephony.TelephonyManager;
import android.telephony.PhoneNumberUtils;
import android.os.Message;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Rect;
import android.os.Bundle;
import android.graphics.Rect;
import android.graphics.Region.Op;
import android.graphics.Paint;
import android.graphics.drawable.Drawable;
import android.graphics.Canvas;
import android.text.StaticLayout;
import android.text.TextPaint;
import android.database.Cursor;
import android.net.Uri;
import android.provider.Contacts;
import oms.contacts.ContactsExt;
import android.provider.Telephony.MessageSummary;
import android.provider.Telephony.MmsSms.Folder;
import android.os.Handler;
import android.text.Layout;
import java.util.TimeZone;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.IntBuffer;
import java.util.ArrayList;
import java.util.Random;
public class StaticWallpaper extends WallpaperService {
    private static final String TAG = "StaticWallpaper";
    WallpaperManager mWallpaperManager;
    private HandlerThread mThread;
	private final String mQueryArgs = MessageSummary.TYPE+"!="+MessageSummary.MsgType.SIMSMS +" and "+ MessageSummary.READ + " = 0 "
	                                  +" and "+ MessageSummary.FOLDER_ID +" != "+ Folder.NAME_ID_MAP.get(Folder.NAME_SECURITY)
	                                  +" and "+ MessageSummary.FOLDER_ID +" != "+ Folder.NAME_ID_MAP.get(Folder.NAME_BLOCK);
    private Cursor mCursor;
    private Context mContext;
    private ContentObserver mObserver;
    private MmsData mMmsData;
    private String mMeStr = "Me";
    private String mLocalNumber;
    private Handler mMsgHandler;
    private Handler mH;
    private final int MSG_SMS_UPDATE = 1;
    private final int MSG_IMAGE_UPDATE = 2;
    private String[] mTsString = {"aa", "bb", "cc"};
    private String mTest;
    private String mSmsHeader1;
    private String mSmsHeader2;
    private String mSmsHeader3;
    private String mSmsSender1;
    private String mSmsSender2;
    private TextPaint mTextPaint = new TextPaint();
    private int mTextColor;
    private int mTextSize;
    private Resources mRes;
    private int mTextBgLeft;
    private int mTextBgTop;
    private int mTextBgWidth;
    private int mTextBgHeight;
    private int mTextVSpacing;
    private int mPaddingLeft;
    private int mPaddingRight;
    private int mPaddingTop;
    private int mPaddingBottom;
    private MmsDataGetter mMmsDataGetter;
    private Drawable mBgDrawable;
    private class StaticWallpaperHandler extends Handler
    {
        public void handleMessage(Message msg)
        {
            switch(msg.what)
            {
            case MSG_IMAGE_UPDATE:
                {
                    if(mMsgHandler != null)
                    {
                        Message m = Message.obtain();
                        m.what = MSG_IMAGE_UPDATE;
                        mMsgHandler.sendMessage(m);
                    }
                    Message m2 = Message.obtain();
                    m2.what = MSG_IMAGE_UPDATE;
                    mH.sendMessageDelayed(m2, 1000);
                }
            default:
                break;
            }
        }
    }
    private class MmsData
    {
        public int count;
        public String[] nameArray;
        public String[] transferedNameArray;
    }
    private interface MmsDataGetter
    {
        public MmsData get();
    }
    private class MmsDataGetterMock implements MmsDataGetter
    {
        public MmsData get()
        {
            MmsData md = new MmsData();
            md.count = 4;
            md.nameArray = new String[md.count];
            md.nameArray[0] = "AAgasd1";
            md.nameArray[1] = "AAgasd2";
            md.nameArray[2] = "AAgasd3";
            md.nameArray[3] = "AAgas4";
            //md.nameArray[4] = "AAgasd5";
            return md;
        }
    }
    private class MmsDataGetterDatabase implements MmsDataGetter
    {
        public MmsData get()
        {
            mCursor.moveToFirst();
            MmsData md = new MmsData();
            md.count = mCursor.getCount();
            if(md.count > 0)
            {
                md.nameArray = new String[md.count];
                for(int i = 0 ; i < md.count ; i++)
                {
                    mCursor.moveToPosition(i);
                    String name = mCursor.getString(mCursor.getColumnIndexOrThrow(MessageSummary.ADDRESS));
                    md.nameArray[i] = name;
                }
            }
            return md;

        }
    }
	public class MmsDBObserver extends ContentObserver{
    	public MmsDBObserver() 
	    {
            super(new Handler());
	    }

	    @Override
	    public void onChange(boolean selfChange) {
            dumpCurrentThread("MmsDBObserver OnChange");
            update();
            Message msg = Message.obtain();
            msg.what = MSG_SMS_UPDATE;
            if(mMsgHandler != null)
                mMsgHandler.sendMessage(msg);
	    }
	}

    private void update()
    {
        setMmsDatabaseCursor();
        mMmsData = getMmsData();
    }    
    private void dumpCurrentThread(String str)
    {
        Thread tmp = Thread.currentThread();
        long threadId = tmp.getId();
        Log.i(TAG, "### thread id = " + threadId + ", fun = " + str + " #####");
    }
    private void setMmsDatabaseCursor()
    {
        Uri uri = MessageSummary.CONTENT_URI;
		mCursor = mContext.getContentResolver().query(uri, null, mQueryArgs, null, MessageSummary.LOCAL_DATE );

    }
    private MmsData getMmsData()
    {
        if(mMmsDataGetter == null)
        {
            mMmsDataGetter = new MmsDataGetterDatabase();
            //mMmsDataGetter = new MmsDataGetterMock();
        }
        return mMmsDataGetter.get();
        /*
        mCursor.moveToFirst();
        MmsData md = new MmsData();
        md.count = mCursor.getCount();
        if(md.count > 0)
        {
            md.nameArray = new String[md.count];
            for(int i = 0 ; i < md.count ; i++)
            {
                mCursor.moveToPosition(i);
                String name = mCursor.getString(mCursor.getColumnIndexOrThrow(MessageSummary.ADDRESS));
                md.nameArray[i] = name;
            }
        }
        return md;
        */
    }
    @Override
    public void onCreate() {
        super.onCreate();
        mContext = getApplication();
        mRes = getResources();
        mObserver = new MmsDBObserver();
        mH = new StaticWallpaperHandler();
        mContext.getContentResolver().registerContentObserver(MessageSummary.CONTENT_URI, true, mObserver);
        update();
        mWallpaperManager = (WallpaperManager) getSystemService(WALLPAPER_SERVICE);
        mThread = new HandlerThread("Wallpaper", Process.THREAD_PRIORITY_FOREGROUND);
        mThread.start();
        setCallbackLooper(mThread.getLooper());
        dumpCurrentThread("Wallpaper OnCreate");
        Log.i(TAG, "mThread = " + mThread.getId() + " ###");
        //Message m = Message.obtain();
        //m.what = MSG_IMAGE_UPDATE;
        //mH.sendMessageDelayed(m, 1000);
        mSmsHeader1 = mContext.getString(R.string.sms_header1);
        mSmsHeader2 = mContext.getString(R.string.sms_header2);
        mSmsHeader3 = mContext.getString(R.string.sms_header3);
        mSmsSender1 = mContext.getString(R.string.sms_sender1);
        mSmsSender2 = mContext.getString(R.string.sms_sender2);
        mTest = mContext.getString(R.string.test);
        mTextColor = 0xFFFF0000;
        mTextSize = 36;//mRes.getDimensionPixelSize(36);//unit is dip
        mTextBgLeft = 0;
        mTextBgTop = 200;
        mTextBgWidth = 480;
        mTextBgHeight = 400;
        mTextPaint.setColor(mTextColor);
        mTextPaint.setTextSize(mTextSize);
        mTextVSpacing = 10;

    }

    public Engine onCreateEngine() {
       DrawableEngine e =  new DrawableEngine();
       mMsgHandler = e.mH;
       return e;
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        mThread.quit();
        mContext.getContentResolver().unregisterContentObserver(mObserver);
    }
    private String[] getNameNoDup(String[] names)
    {
        ArrayList<String> newnames = new ArrayList<String>();
        for(int i = 0 ; i < names.length ; i++)
        {
            String name = names[i];
            boolean found = false;
            for(int j = 0 ; j < newnames.size() ; j++)
            {
                if(newnames.get(j).equals(name))
                {
                    found = true;
                    break;
                }
            }
            if(!found)
            {
                newnames.add(name);
            }
        }
        String[] ret = new String[newnames.size()];
        for(int i = 0 ; i < ret.length ; i++)
        {
            ret[i] = newnames.get(i);
        }
        return ret;
    }

    public String getDisplayAddress(Context context, String address) {
        if (address == null) {
            return "";
        }

        if (mLocalNumber == null) {
            TelephonyManager telephonyManager = TelephonyManager.getDefault();

            if(telephonyManager != null) {
                mLocalNumber = telephonyManager.getLine1Number();
            }
        }
        if (PhoneNumberUtils.compare(address, mLocalNumber)) {
            return mMeStr;
        }

        String[] values = address.split(";");
        if (address.indexOf(",") != -1) {
            values = address.split(",");
        }
        String result = "";
        for (int i = 0; i < values.length; i++) 
        {
            if (values[i].length() > 0) 
            {
                try {
                    Cursor cursor = context.getContentResolver().query(
                        Uri.withAppendedPath(Contacts.Phones.CONTENT_FILTER_URL, values[i]),
                        new String[]{ Contacts.Phones.NAME }, null, null, null);
                    if (cursor != null) 
                    {
                        if (cursor.moveToFirst()) 
                        {
                            result = result + ";" + cursor.getString(0);
                            cursor.close();
                            continue;
                        }
                        cursor.close();
                    }
                
                // sim card
                    Cursor cur = context.getContentResolver().query(
                            Uri.withAppendedPath(ContactsExt.SimCardContacts.CONTENT_URI_NUMBER_FILTER, values[i]),
                            new String[]{ContactsExt.SimCardContacts.NAME}, null, null, null);
                    if (cur != null) {
                        if (cur.moveToFirst()) {
                            result = result + ";" + cur.getString(0);
                            cur.close();
                            continue;
                        }
                        cur.close();
                    }
                } catch (IllegalStateException ex) {
                    ex.printStackTrace();
                } catch (Exception e) {
                    Log.e(TAG, "Exception " + e.getMessage());
                }
                //default
                result = result + ";" + values[i];
            }
        }

        if (result.length() > 0) {
            // Skip the first ';'
            return result.substring(1);
        }
        return result;
    }

    private String getDisplayAddressName(String number)
    {
		return getDisplayAddress(mContext, number).replace(';', ',');
	}

    class DrawableEngine extends Engine {
        private final Object mLock = new Object();
        private WallpaperObserver mReceiver;
        Drawable mBackground;
        float mXOffset;
        float mYOffset;
        String mTsStr = "TSTS";
        public Handler mH
        = new Handler() {
            public void handleMessage(Message msg)
            {
                switch(msg.what)
                {
                    case MSG_SMS_UPDATE:
                        {
                            dumpCurrentThread("MSG_SMS_UPDATE");
                            drawFrame();

                        }
                        break;
                    case MSG_IMAGE_UPDATE:
                        {
                            Random r = new Random();
                            int randomNum = r.nextInt();
                            int i = 0;
                            if(randomNum > 0)
                            {
                                i = randomNum % 3;
                            }
                            Log.i(TAG, "### i =" + i  + " ###");
                            mTsStr = mTsString[i];
                            drawFrame();
                        }
                        break;
                    default:
                        break;
                }
            }
        };
        class WallpaperObserver extends BroadcastReceiver {
            public void onReceive(Context context, Intent intent) {
                updateWallpaper();
                drawFrame();
                // Assume we are the only one using the wallpaper in this
                // process, and force a GC now to release the old wallpaper.
                System.gc();
            }
        }

        @Override
        public void onCreate(SurfaceHolder surfaceHolder) {
            super.onCreate(surfaceHolder);
            IntentFilter filter = new IntentFilter(Intent.ACTION_WALLPAPER_CHANGED);
            mReceiver = new WallpaperObserver();
            registerReceiver(mReceiver, filter);
            updateWallpaper();
            surfaceHolder.setSizeFromLayout();
            dumpCurrentThread("Engine onCreate");
        }

        @Override
        public void onDestroy() {
            super.onDestroy();
            unregisterReceiver(mReceiver);
        }

        @Override
        public void onVisibilityChanged(boolean visible) {
            drawFrame();
        }
        
        @Override
        public void onTouchEvent(MotionEvent event) {
            super.onTouchEvent(event);
        }

        @Override
        public void onOffsetsChanged(float xOffset, float yOffset,
                float xOffsetStep, float yOffsetStep,
                int xPixels, int yPixels) {
            mXOffset = xOffset;
            mYOffset = yOffset;
            drawFrame();
        }

        @Override
        public void onSurfaceChanged(SurfaceHolder holder, int format, int width, int height) {
            super.onSurfaceChanged(holder, format, width, height);
            drawFrame();
        }

        @Override
        public void onSurfaceCreated(SurfaceHolder holder) {
            super.onSurfaceCreated(holder);
        }

        @Override
        public void onSurfaceDestroyed(SurfaceHolder holder) {
            super.onSurfaceDestroyed(holder);
        }
        void drawTextBackground(Canvas canvas)
        {
            if(mBgDrawable == null)
            {
                Drawable d = getResources().getDrawable(R.drawable.popup_full_dark);
                mBgDrawable = d;
            }
            Rect r = new Rect();
            boolean haspadding = mBgDrawable.getPadding(r);
            if(haspadding)
            {
                Log.i(TAG, "## " + r.left + ", " + r.top + ", " + r.right + ", " + r.bottom);
                mPaddingLeft = r.left;
                mPaddingRight = r.right;
                mPaddingTop = r.top;
                mPaddingBottom = r.bottom;
            }
            else
            {
                Log.i(TAG, "no padding");
            }
            mBgDrawable.setBounds(mTextBgLeft, mTextBgTop, mTextBgLeft + mTextBgWidth, mTextBgTop + mTextBgHeight);
            mBgDrawable.draw(canvas);
        }
        Layout getTextLayout(String str, TextPaint tp, int boundWidth)
        {
            return new StaticLayout(str, tp, boundWidth, Layout.Alignment.ALIGN_NORMAL, 1, 0, true);
        }
        int getTextWidth(String str, TextPaint tp)
        {
            return (int)(Layout.getDesiredWidth(str, tp) + 0.5);
        }
        int getTextHeight(String str)
        {
            Layout l = getTextLayout(str, mTextPaint, mTextBgWidth);
            return l.getHeight();
        }
        void drawText(Canvas canvas)
        {
            Log.i(TAG, "### unread msg = " + mMmsData.count + " ####");
            for(int i = 0 ; i < mMmsData.count ; i++)
            {
                Log.i(TAG, "### name = " + mMmsData.nameArray[i] + " ###");
            }
            if(mMmsData.transferedNameArray == null)
            {
                String[] names = getNameNoDup(mMmsData.nameArray);
                mMmsData.transferedNameArray = new String[names.length];
                for(int i = 0 ; i < names.length ; i++)
                {
                    String str = getDisplayAddressName(names[i]);
                    mMmsData.transferedNameArray[i] = str;
                }
            }
            int startx = mTextBgLeft + mPaddingLeft + 2;
            int starty = mTextBgTop + mPaddingTop;
            String strHeader;
            if(mMmsData.count > 1)
            {
                strHeader = mSmsHeader1 + Integer.toString(mMmsData.count) + mSmsHeader3;
            }
            else
            {
                strHeader = mSmsHeader1 + Integer.toString(mMmsData.count) + mSmsHeader2;
            }
            int textHeight = getTextHeight(strHeader);
            starty += textHeight;
            canvas.drawText(strHeader, startx, starty, mTextPaint);
            //String[] names = getNameNoDup(mMmsData.nameArray);
            starty += mTextVSpacing; 
            String strSender = null;
            if(mMmsData.transferedNameArray.length > 1)
                strSender = mSmsSender2;
            else
                strSender = mSmsSender1;
            textHeight = getTextHeight(strSender);
            starty += textHeight;
            canvas.drawText(strSender, startx, starty , mTextPaint);
            starty += mTextVSpacing;
            int bottom = mTextBgHeight + mTextBgTop - mPaddingBottom;
            Log.i(TAG, "bottom = " + bottom);
            int size = 0;
            int tmpY = starty;
            for(int i = 0 ; i < mMmsData.transferedNameArray.length ; i++)
            {
                String str = mMmsData.transferedNameArray[i];//getDisplayAddressName(names[i]);
                textHeight = getTextHeight(str);
                tmpY += textHeight;
                Log.i(TAG, "tmpY = " + tmpY);
                //canvas.drawText(str, startx, starty, mTextPaint);
                if(tmpY <= bottom)
                    size++;
                else
                    break;
                tmpY += mTextVSpacing;
            }
            boolean needEllipse = false;
            if(size > 0 && size < mMmsData.transferedNameArray.length)
            {
                size--;
                needEllipse = true;
            }
            for(int i = 0 ; i < size ; i++)
            {
                String str = mMmsData.transferedNameArray[i];//getDisplayAddressName(names[i]);
                textHeight = getTextHeight(str);
                starty += textHeight;
                canvas.drawText(str, startx, starty, mTextPaint);
            }
            if(needEllipse)
            {
                String str = "......";
                textHeight = getTextHeight(str);
                starty += textHeight;
                canvas.drawText(str, startx, starty, mTextPaint);
            }
            canvas.drawText(mTest, 0, (800 * 3/ 4) , mTextPaint);
        }
        void drawFrame() {
            SurfaceHolder sh = getSurfaceHolder();
            Canvas c = sh.lockCanvas();
            if (c != null) {
                final Rect frame = sh.getSurfaceFrame();
                synchronized (mLock) {
                    final Drawable background = mBackground;
                    final int dw = frame.width();
                    final int dh = frame.height();
                    final int bw = background != null ? background.getIntrinsicWidth() : 0;
                    final int bh = background != null ? background.getIntrinsicHeight() : 0;
                    final int availw = dw-bw;
                    final int availh = dh-bh;
                    int xPixels = availw < 0 ? (int)(availw*mXOffset+.5f) : (availw/2);
                    int yPixels = availh < 0 ? (int)(availh*mYOffset+.5f) : (availh/2);

                    c.translate(xPixels, yPixels);
                    if (availw<0 || availh<0) {
                        c.save(Canvas.CLIP_SAVE_FLAG);
                        c.clipRect(0, 0, bw, bh, Op.DIFFERENCE);
                        c.drawColor(0xff000000);
                        c.restore();
                    }
                    if (background != null) {
                        background.draw(c);
                    }
                    Log.i("StaticWallpaper", "#### xPixels " + xPixels + ", yPixels " + yPixels);
                    if(mMmsData != null && (mMmsData.count > 0))
                    {
                        c.translate(-xPixels, -yPixels);
                        drawTextBackground(c);
                        drawText(c);

                    }
                }
                sh.unlockCanvasAndPost(c);
            }
        }

        void updateWallpaper() {
            synchronized (mLock) {
            	try {
                    mBackground = mWallpaperManager.getFastDrawable();
                } catch (RuntimeException e) {
                    Log.w("ImageWallpaper", "Unable to load wallpaper!", e);
                }
            }
        }
    }
}
