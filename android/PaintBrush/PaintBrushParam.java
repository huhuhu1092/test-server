package com.example.hellojni;

public class PaintBrushParam
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
    public PaintBrushParam()
    {
    	
    }
    public PaintBrushParam(int ot, int on, float of, float ol, int sn, 
    		               float sf, float sl, int st, int bt, int p, float bd)
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
    }
}
