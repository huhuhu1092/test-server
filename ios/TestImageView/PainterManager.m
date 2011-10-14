//
//  PainterManager.m
//  TestImageView
//
//  Created by 陈勇 on 11-10-13.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import "PainterManager.h"
@implementation PainterParam
@synthesize orient_type;
@synthesize orient_num;
@synthesize  orient_first;
@synthesize  orient_last;
@synthesize size_num;
@synthesize size_first;
@synthesize size_last;
@synthesize size_type;
@synthesize bg_type;
@synthesize place_type;
@synthesize brush_density;
@synthesize paper_scale;
@synthesize paper_relief;
@synthesize brush_relief;
@synthesize color_type;
@synthesize drawing_speed;
@synthesize wait_time;
- (id)init
{
    self = [super init];
    return self;
}
- (void)setParam:(int)ot :(int)on :(float)of :(float)ol :(int)sn :(float)sf :(float)sl :(int)st
                :(int)bt :(int)pt :(float)bd :(float)ps :(float)pr :(float)br :(int)ct :(int)ds :(int)wt
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
    place_type = pt;
    brush_density = bd;
    paper_scale = ps;
    paper_relief = pr;
    brush_relief = br;
    drawing_speed = ds;
    wait_time = wt;
    color_type = ct;
}
@end

@implementation PainterManager
@synthesize paramArray;
- (void)initParams
{
    PainterParam* p1 = [[PainterParam alloc] init];
    [p1 setParam:6 :8 :120 :60 :6 :179 :224 :4 :2 :1 :20 :30 :0 :0 :0 :100 :20];
    PainterParam* p2 = [[PainterParam alloc] init];
    [p2 setParam:6 :8 :45 :180 :6 :148 :184 :4 :2 :0 :10 :30 :0 :0 :0 :500 :20];
    
    PainterParam* p3 = [[PainterParam alloc] init];
    [p3 setParam:6 :8 :45 :180 :6 :120 :148 :4 :2 :0 :10 :30 :0 :0 :0 :500 :10];
    
    PainterParam* p4 = [[PainterParam alloc] init];
    [p4 setParam:6 :8 :45 :180 :6 :95 :116 :4 :2 :0 :10 :30 :0 :0 :0 :500 :10];
    
    PainterParam* p5 = [[PainterParam alloc] init];
    [p5 setParam:6 :8 :45 :180 :6 :73 :88 :4 :2 :0 :10 :30 :0 :0 :0 :500 :0];
    
    PainterParam* p6 = [[PainterParam alloc] init];
    [p6 setParam:6 :8 :45 :180 :6 :54 :64 :4 :2 :0 :10 :30 :0 :0 :0 :500 :0];
    
    PainterParam* p7 = [[PainterParam alloc] init];
    [p7 setParam:6 :8 :45 :180 :6 :38 :44 :4 :2 :0 :10 :30 :0 :0 :0 :1000 :0];
    
    PainterParam* p8 = [[PainterParam alloc] init];
    [p8 setParam:4 :8 :45 :180 :4 :25 :28 :4 :2 :0 :10 :30 :0 :0 :0 :1000 :0];
    
    PainterParam* p9 = [[PainterParam alloc] init];
    [p9 setParam:6 :8 :45 :180 :2 :15 :16 :4 :2 :0 :10 :30 :0 :0 :0 :1000 :0];
    
    PainterParam* p10 = [[PainterParam alloc] init];
    [p10 setParam:6 :8 :45 :180 :1 :8 :8 :4 :2 :0 :20 :30 :0 :0 :0 :1000 :0];
    
    paramArray = [[NSArray alloc] initWithObjects:p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, nil];
}
- (id)init
{
    self = [super init];
    if (self) {
        // Initialization code here.
        [self initParams];
        for(int i = 0 ; i < PARAM_NUM ; i++)
        {
            drawingState[i] = 1;
        }
    }
    
    return self;
}
- (void)dealloc
{
    [paramArray release];
    [super dealloc];
}
- (void)setDrawingState:(int)index :(int)s
{
    if(index >= 0 && index < PARAM_NUM)
    {
        drawingState[index] = s;
    }
}
- (int)drawingState:(int)index
{
    return drawingState[index];
}
@end
