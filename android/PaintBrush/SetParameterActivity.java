package com.example.hellojni;
import android.app.Activity;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;
import android.util.Log;
import android.content.Intent;
import android.widget.ImageView;
import android.graphics.Bitmap;
import android.widget.Spinner;
import android.widget.ArrayAdapter;  
import android.widget.AdapterView;  
import android.widget.AdapterView.OnItemSelectedListener;  
import android.os.Parcelable;
import android.widget.EditText;
public class SetParameterActivity extends Activity
{
	@Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.parameter);
        Button okButton = (Button)findViewById(R.id.ok);
        Button cancelButton = (Button)findViewById(R.id.cancel);
        Intent intent = getIntent();
        Parcelable[] pbp = intent.getParcelableArrayExtra("param_array");
        initPaintBrushParam(pbp);
        okButton.setClickable(true);
        cancelButton.setClickable(true);
        okButton.setOnClickListener(new View.OnClickListener() {
			
		@Override
		public void onClick(View v) {
				// TODO Auto-generated method stub
				Intent intent = new Intent();
                				
			}
		});
        cancelButton.setOnClickListener(new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				// TODO Auto-generated method stub
			    finish();	
			}
		});
        mIndexSpinner = (Spinner)findViewById(R.id.index_spinner);
        mIndexAdapter = new ArrayAdapter<String>(this,android.R.layout.simple_spinner_item, mData);  
        
        mIndexAdapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);  
        mIndexSpinner.setAdapter(mIndexAdapter);  
        mIndexSpinner.setOnItemSelectedListener(new SpinnerSelectedListener());  
        
        setOrientTypeSpinner();
        setSizeTypeSpinner();
        setPlaceTypeSpinner();
        setColorTypeSpinner();
        setBgTypeSpinner();
    }
	void initPaintBrushParam(Parcelable[] pbp)
	{
		mPaintBrushParamArray = new PaintBrushParam[pbp.length];
		for(int i = 0 ; i < pbp.length ; i++)
		{
			mPaintBrushParamArray[i] = (PaintBrushParam)pbp[i];
		}
	}
	void setOrientTypeSpinner()
	{
		int[] orientData = {R.string.orient_value, R.string.orient_radius, R.string.orient_random,
				             R.string.orient_radial, R.string.orient_flowing, R.string.orient_hue,
				             R.string.orient_adaptive, R.string.orient_manual};
		String[] orientDataStr = new String[orientData.length];
		for(int i = 0 ; i < orientDataStr.length; i++)
		{
            orientDataStr[i] = getResources().getText(orientData[i]).toString();			
 		}
		Spinner orientTypeSpinner = (Spinner)findViewById(R.id.orient_type_input);
		ArrayAdapter<String> adapter = new ArrayAdapter<String>(this, android.R.layout.simple_spinner_item, orientDataStr);
		adapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);  
        orientTypeSpinner.setAdapter(adapter);  
        orientTypeSpinner.setOnItemSelectedListener(new OnItemSelectedListener(){
        	public void onItemSelected(AdapterView<?> arg0, View arg1, int arg2,  
                    long arg3) {  
        		Log.i(TAG, "### orient type = " + arg2 + " ####");
        	    mPaintBrushParamsCache.orient_type = arg2;
        	}
        	public void onNothingSelected(AdapterView<?> arg0) {  
            }  
        });  
	}
	void setSizeTypeSpinner()
	{
		int[] sizeTypeData = {R.string.size_type_value, R.string.size_type_radius, R.string.size_type_random,
	             R.string.size_type_radial, R.string.size_type_flowing, R.string.size_type_hue,
	             R.string.size_type_adaptive, R.string.size_type_manual};
        String[] sizeTypeDataStr = new String[sizeTypeData.length];
        for(int i = 0 ; i < sizeTypeDataStr.length; i++)
        {
            sizeTypeDataStr[i] = getResources().getText(sizeTypeData[i]).toString();			
        }
        Spinner sizeTypeSpinner = (Spinner)findViewById(R.id.size_type_input);
        ArrayAdapter<String> adapter = new ArrayAdapter<String>(this, android.R.layout.simple_spinner_item, sizeTypeDataStr);
        adapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);  
        sizeTypeSpinner.setAdapter(adapter);  
        sizeTypeSpinner.setOnItemSelectedListener(new OnItemSelectedListener(){
            public void onItemSelected(AdapterView<?> arg0, View arg1, int arg2,  
               long arg3) {  
	                Log.i(TAG, "### size type = " + arg2 + " ####");
                    mPaintBrushParamsCache.size_type = arg2;
            }
            public void onNothingSelected(AdapterView<?> arg0) {  
            }  
        });  
	}
	void setPlaceTypeSpinner()
	{
		int[] placeTypeData = {R.string.place_type_random, R.string.place_type_even_dist};
       String[] placeTypeDataStr = new String[placeTypeData.length];
       for(int i = 0 ; i < placeTypeDataStr.length; i++)
       {
           placeTypeDataStr[i] = getResources().getText(placeTypeData[i]).toString();			
       }
       Spinner sizeTypeSpinner = (Spinner)findViewById(R.id.size_type_input);
       ArrayAdapter<String> adapter = new ArrayAdapter<String>(this, android.R.layout.simple_spinner_item, placeTypeDataStr);
       adapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);  
       sizeTypeSpinner.setAdapter(adapter);  
       sizeTypeSpinner.setOnItemSelectedListener(new OnItemSelectedListener(){
           public void onItemSelected(AdapterView<?> arg0, View arg1, int arg2,  
              long arg3) {  
	                Log.i(TAG, "### place type = " + arg2 + " ####");
                   mPaintBrushParamsCache.place_type = arg2;
           }
           public void onNothingSelected(AdapterView<?> arg0) {  
           }  
       });  
	}
	void setColorTypeSpinner()
	{
		int[] colorTypeData = {R.string.color_type_brush_avg, R.string.color_type_brush_center};
	       String[] colorTypeDataStr = new String[colorTypeData.length];
	       for(int i = 0 ; i < colorTypeDataStr.length; i++)
	       {
	           colorTypeDataStr[i] = getResources().getText(colorTypeData[i]).toString();			
	       }
	       Spinner colorTypeSpinner = (Spinner)findViewById(R.id.size_type_input);
	       ArrayAdapter<String> adapter = new ArrayAdapter<String>(this, android.R.layout.simple_spinner_item, colorTypeDataStr);
	       adapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);  
	       colorTypeSpinner.setAdapter(adapter);  
	       colorTypeSpinner.setOnItemSelectedListener(new OnItemSelectedListener(){
	           public void onItemSelected(AdapterView<?> arg0, View arg1, int arg2,  
	              long arg3) {  
		                Log.i(TAG, "### color type = " + arg2 + " ####");
	                   mPaintBrushParamsCache.color_type = arg2;
	           }
	           public void onNothingSelected(AdapterView<?> arg0) {  
	           }  
	       });  		
	}
	void setBgTypeSpinner()
	{
		int[] bgTypeData = {R.string.bg_type_solid, R.string.bg_type_keep_original, R.string.bg_type_from_paper,
				R.string.bg_type_transparent};
	       String[] bgTypeDataStr = new String[bgTypeData.length];
	       for(int i = 0 ; i < bgTypeDataStr.length; i++)
	       {
	           bgTypeDataStr[i] = getResources().getText(bgTypeData[i]).toString();			
	       }
	       Spinner bgTypeSpinner = (Spinner)findViewById(R.id.size_type_input);
	       ArrayAdapter<String> adapter = new ArrayAdapter<String>(this, android.R.layout.simple_spinner_item, bgTypeDataStr);
	       adapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);  
	       bgTypeSpinner.setAdapter(adapter);  
	       bgTypeSpinner.setOnItemSelectedListener(new OnItemSelectedListener(){
	           public void onItemSelected(AdapterView<?> arg0, View arg1, int arg2,  
	              long arg3) {  
		                Log.i(TAG, "### bg type = " + arg2 + " ####");
	                   mPaintBrushParamsCache.bg_type = arg2;
	           }
	           public void onNothingSelected(AdapterView<?> arg0) {  
	           }  
	       });  			
	}
	class SpinnerSelectedListener implements OnItemSelectedListener{  
		  
        public void onItemSelected(AdapterView<?> arg0, View arg1, int arg2,  
                long arg3) {  
            String str = mData[arg2];
            Log.i("setparameter", str);
            mCurrentIndex = Integer.parseInt(str); 
            PaintBrushParam pbp = mPaintBrushParamArray[mCurrentIndex];
            setTextValue(pbp);
        }  
  
        public void onNothingSelected(AdapterView<?> arg0) {  
        }  
    }  
	void setTextValue(PaintBrushParam pbp)
	{
		Spinner orientType = (Spinner)findViewById(R.id.orient_type_input);
		orientType.setSelection(pbp.orient_type);
		
		EditText orientNum = (EditText)findViewById(R.id.orient_num_input);
		orientNum.setText(Integer.toString(pbp.orient_num));
		
		
	}
	private Spinner mIndexSpinner;
	private ArrayAdapter<String> mIndexAdapter;
	private static final String[] mData = {"1", "2", "3", "4", "5", "6", "7", "8", "9", "10"};
	private int mCurrentIndex;
	private PaintBrushParam mPaintBrushParamsCache = new PaintBrushParam();
	private PaintBrushParam[] mPaintBrushParamArray;// = new PaintBrushParam[10];
	private final static String TAG = "SetParameter";
}
