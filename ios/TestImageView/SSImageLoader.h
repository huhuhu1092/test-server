//
//  SSImageLoader.h
//  TestImageView
//
//  Created by 陈勇 on 11-9-27.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
struct ppm;
struct PainterProperty
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
    
    unsigned char brush_img[128];
    unsigned char paper_img[128];
    
    int drawing_speed;
    int wait_time;
};
extern void initPainterProperty(struct PainterProperty* p);
@interface SSImageLoader : NSObject
{
    id setImageCallback;
    struct PainterProperty painterProperty;
}
- (void)createFromPhotoLib:(NSString*)photoName;
- (void)createFromFileName:(NSString*)fileName;
- (void)loadPhotoLib;
- (void)setPainterProperty:(struct PainterProperty)p;
- (struct PainterProperty)painterProperty;
+ (CGImageRef)createCGImage:(struct ppm*) p;
@property (nonatomic, retain) id setImageCallback;
@end
