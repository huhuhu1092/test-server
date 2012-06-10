//
//  SEResDefine.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-1-29.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEResDefine.h"
@interface FontPropertyData : NSObject
{
    enum FONT_STYLE style;
    enum FONT_SIZE size;
    NSDictionary* font;
}
@property (nonatomic, assign) enum FONT_STYLE style;
@property (nonatomic, assign) enum FONT_SIZE size;
@property (nonatomic, retain) NSDictionary* font;
@end
@implementation FontPropertyData
@synthesize style;
@synthesize size;
@synthesize font;

@end
////////
@implementation SEResLoader
- (id) init
{
    self = [super init];
    if(self)
    {
        NSString* path = [[NSBundle mainBundle] pathForResource:@"ViewProperty" ofType:@"plist"];
        mResDict = [[NSDictionary alloc] initWithContentsOfFile:path];
    }
    return self;
}
- (void) dealloc
{
    [mResDict release];
    [super dealloc];
}

- (UIImage*) getImage: (NSString*) key
{
    NSString* name = [mResDict objectForKey:key];
    return [UIImage imageNamed:name];    
}
- (int) getInt : (NSString*)key
{
    NSNumber* num = [mResDict objectForKey:key];
    return [num intValue];
}
- (int) getFloat : (NSString*) key
{
    NSNumber* num = [mResDict objectForKey:key];
    return [num floatValue];
}
@end
@implementation SEFontLoader

- (id) init
{
    self = [super init];
    if(self)
    {
        mFontArray = [NSMutableArray array];
        [mFontArray retain];
        NSString* path_normal_normal = [[NSBundle mainBundle] pathForResource:@"Font_Normal_Normal" ofType:@"plist"];
        NSString* path_normal_big = [[NSBundle mainBundle] pathForResource:@"Font_Normal_Big" ofType:@"plist"];
        NSDictionary* font1 = [[NSDictionary alloc] initWithContentsOfFile:path_normal_normal];
        NSDictionary* font2 = [[NSDictionary alloc] initWithContentsOfFile:path_normal_big];
        FontPropertyData* fpd1 =[[FontPropertyData alloc] init];
        fpd1.style = FONT_NORMALSTYLE;
        fpd1.size = FONT_NORMAL_SIZE;
        fpd1.font = font1;
        [mFontArray addObject:fpd1];
        [fpd1 release];
        [font1 release];
        
        
        FontPropertyData* fpd2 = [[FontPropertyData alloc] init];
        fpd2.style = FONT_NORMALSTYLE;
        fpd2.size = FONT_BIG_SIZE;
        fpd2.font = font2;
        [mFontArray addObject:fpd2];
        [fpd2 release];
        [font2 release];
        
    }
    return self;
}
- (void)dealloc
{
    [mFontArray release];
    [super dealloc];
}
- (UIImage*) getImage: (NSString*) key style: (enum FONT_STYLE) style size : (enum FONT_SIZE)size
{
    for(FontPropertyData* fpd in mFontArray)
    {
        if(fpd.style == style && fpd.size == size)
        {
            NSString* name = [fpd.font objectForKey:key];
            return [UIImage imageNamed:name];
        }
    }
    return nil;
}
@end
