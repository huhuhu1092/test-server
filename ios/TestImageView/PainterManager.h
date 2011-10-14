//
//  PainterManager.h
//  TestImageView
//
//  Created by 陈勇 on 11-10-13.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
enum {
    PARAM_NUM = 10
};
@interface PainterParam : NSObject
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
}
@property (nonatomic) int orient_type;
@property (nonatomic) int orient_num;
@property (nonatomic) float orient_first;
@property (nonatomic) float orient_last;
@property (nonatomic) int size_num;
@property (nonatomic) float size_first;
@property (nonatomic) float size_last;
@property (nonatomic) int size_type;
@property (nonatomic) int bg_type;
@property (nonatomic) int place_type;
@property (nonatomic) float brush_density;
@property (nonatomic) float paper_scale;
@property (nonatomic) float paper_relief;
@property (nonatomic) float brush_relief;
@property (nonatomic) int color_type;
@property (nonatomic) int drawing_speed;
@property (nonatomic) int wait_time;
- (void)setParam:(int)ot :(int)on :(float)of :(float)ol :(int)sn :(float)sf :(float)sl :(int)st
                :(int)bt :(int)pt :(float)bd :(float)ps :(float)pr :(float)br :(int)ct :(int)ds :(int)wt;
@end
@interface PainterManager : NSObject
{
    NSArray* paramArray;
    int drawingState[PARAM_NUM];
}
@property (nonatomic, readonly) NSArray* paramArray;
- (void)setDrawingState:(int)index :(int)s;
- (int)drawingState:(int)index;
@end
