package com.example.hellojni;
import android.os.Parcelable;
import android.os.Parcel;
public class PaintBrushParam implements Parcelable
{
    int orient_type;
    int orient_num;
    float orient_first;
    float orient_last;
    int size_num;
    float size_first;
    float size_last;
    int size_type;
    int bg_type;
    int place_type;
    float brush_density;
    
    float paper_scale;
    float paper_relief;
    float brush_relief;
    int color_type;
    
    int drawing_speed;
    int wait_time;
    public PaintBrushParam()
    {
    	
    }
    public PaintBrushParam(int ot, int on, float of, float ol, int sn, 
    		               float sf, float sl, int st, int bt, int p, float bd,
    		               float ps, float pr, float br, int ct, int ds, int wt)
    {
    	orient_type = ot;
    	orient_num = on;
    	orient_first = of;
    	orient_last = ol;
        size_num = sn;
        size_first = sf;
        size_last = sl;
        size_type = st;
        bg_type = bt;
        place_type = p;
        brush_density = bd;
        paper_scale = ps;
        paper_relief = pr;
        brush_relief = br;
        color_type = ct;
        drawing_speed = ds;
        wait_time = wt;
    }
   public int describeContents()
   {
	   return 0;
   }
    
    public void writeToParcel(Parcel dest, int flags)
    {
    	dest.writeInt(orient_type);
    	dest.writeInt(orient_num);
    	dest.writeFloat(orient_first);
    	dest.writeFloat(orient_last);
    	dest.writeInt(size_num);
    	dest.writeFloat(size_first);
    	dest.writeFloat(size_last);
    	dest.writeInt(size_type);
    	dest.writeInt(bg_type);
    	dest.writeInt(place_type);
    	dest.writeFloat(brush_density);
    	dest.writeFloat(paper_scale);
    	dest.writeFloat(paper_relief);
    	dest.writeFloat(brush_relief);
    	dest.writeInt(color_type);
    	dest.writeInt(drawing_speed);
    	dest.writeInt(wait_time);
    }

    public final static  Parcelable.Creator<PaintBrushParam> CREATOR = new Parcelable.Creator<PaintBrushParam>() {
        public PaintBrushParam createFromParcel(Parcel source)
        {
        	PaintBrushParam pbp = new PaintBrushParam();
        	pbp.orient_type = source.readInt();
        	pbp.orient_num = source.readInt();
        	pbp.orient_first = source.readFloat();
        	pbp.orient_last = source.readFloat();
        	pbp.size_num = source.readInt();
        	pbp.size_first = source.readFloat();
        	pbp.size_last = source.readFloat();
        	pbp.size_type = source.readInt();
        	pbp.bg_type = source.readInt();
        	pbp.place_type = source.readInt();
        	pbp.brush_density = source.readFloat();
        	pbp.paper_scale = source.readFloat();
        	pbp.paper_relief = source.readFloat();
        	pbp.brush_relief = source.readFloat();
        	pbp.color_type = source.readInt();
        	pbp.drawing_speed = source.readInt();
        	pbp.wait_time = source.readInt();
        	return pbp;
        }
        
        public PaintBrushParam[] newArray(int size)
        {
        	PaintBrushParam[] pbp = new PaintBrushParam[size];
        	return pbp;
        }
    };
}
