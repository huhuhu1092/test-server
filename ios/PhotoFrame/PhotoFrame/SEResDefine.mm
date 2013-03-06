//
//  SEResDefine.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-1-29.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEResDefine.h"
#import "SEUtil.h"
////////////////////
////////////////////
@interface FontPropertyData : NSObject
{
    int  style;
    enum FONT_SIZE size;
    NSMutableDictionary* font;
}
@property (nonatomic, assign) int style;
@property (nonatomic, assign) enum FONT_SIZE size;
@property (nonatomic, retain) NSMutableDictionary* font;
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
    NSString* prefixname = nil;
    int version = [SEUtil getSystemVersion];
    if([key isEqualToString: @"MusicPickerBackground"])
    {
        NSLog(@"#########");
    }
    if(version <= 4)
    {
        NSString* prefix = @"4x";
        prefixname = [prefix stringByAppendingFormat:@"_%@", name];
    }
    if(prefixname != nil)
    {
        UIImage* image = [UIImage imageNamed:prefixname];
        if(image == nil)
        {
            image = [UIImage imageNamed:name];
        }
        return image;
    }
    else
    {
        return [UIImage imageNamed:name];    
    }
}
- (UIImage*) getImage: (NSString*) key withCapInset: (UIEdgeInsets) inset
{
    UIImage* image = [self getImage:key];
    return [SEUtil imageWithCap:image top:inset.top bottom:inset.bottom left:inset.left right:inset.right];
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
/*
- (void) initFontStyle: (FONT_STYLE) style
{
    NSString* path_normal = nil;
    NSString* path_big = nil;
    switch (style) {
        case FONT_NORMALSTYLE:
        {
            path_normal = [[NSBundle mainBundle] pathForResource:@"Font_Normal_Normal" ofType:@"plist"];
            path_big = [[NSBundle mainBundle] pathForResource:@"Font_Normal_Big" ofType:@"plist"];
        }
            break;
        case FONT_HANDSTYLE:
        {
            path_normal = [[NSBundle mainBundle] pathForResource:@"Font_HandStyle_Normal" ofType:@"plist"];
            path_big = [[NSBundle mainBundle] pathForResource:@"Font_HandStyle_Big" ofType:@"plist"];
        }
            break;
        case FONT_003:
        {
            path_normal = [[NSBundle mainBundle] pathForResource:@"Font003_Normal" ofType:@"plist"];
            path_big = [[NSBundle mainBundle] pathForResource:@"Font003_Big" ofType:@"plist"];
        }
            break;
        default:
            break;
    }
    if(path_normal == nil || path_big == nil)
    {
        NSLog(@"can not find font path : %d", style);
        return;
    }
    
    NSMutableDictionary* font1 = [[NSMutableDictionary alloc] initWithContentsOfFile:path_normal];
    NSMutableDictionary* font2 = [[NSMutableDictionary alloc] initWithContentsOfFile:path_big];
    FontPropertyData* fpd1 =[[FontPropertyData alloc] init];
    fpd1.style = style;
    fpd1.size = FONT_NORMAL_SIZE;
    fpd1.font = font1;
    [mFontArray addObject:fpd1];
    [fpd1 release];
    [font1 release];
    
    
    FontPropertyData* fpd2 = [[FontPropertyData alloc] init];
    fpd2.style = style;
    fpd2.size = FONT_BIG_SIZE;
    fpd2.font = font2;
    [mFontArray addObject:fpd2];
    [fpd2 release];
    [font2 release];
}
 */
- (int) getFontNum
{
    return [mFontArray count];
}
- (NSArray*) getAllFontStyles
{
    NSMutableArray* array = [NSMutableArray array];
    for(int i = 0 ; i < mFontArray.count ; i++)
    {
        FontPropertyData* fpd = [mFontArray objectAtIndex:i];
        NSNumber* ns = [NSNumber numberWithInt:fpd.style];
        [array addObject:ns];
    }
    return array;
}
- (UIImage*) getImage:(NSString*) key fontIndex: (int)index
{
    if(index < 0 || index >= mFontArray.count)
        return nil;
    FontPropertyData* fpd = [mFontArray objectAtIndex:index];
    NSString* name = [fpd.font objectForKey:key];
    return [UIImage imageNamed:name];
}
- (FontPropertyData*) getFontPropertyByStyle: (int)s
{
    for(int i = 0 ; i < mFontArray.count ; i++)
    {
        FontPropertyData* fpd = [mFontArray objectAtIndex:i];
        if(fpd.style == s)
            return fpd;
    }
    return nil;
}
- (NSDictionary*) defineFontKeys
{
    NSMutableDictionary* dir = [NSMutableDictionary dictionary];
    [dir setValue:@"0" forKey:@"0"];
    [dir setValue:@"1" forKey:@"1"];
    [dir setValue:@"2" forKey:@"2"];
    [dir setValue:@"3" forKey:@"3"];
    [dir setValue:@"4" forKey:@"4"];
    [dir setValue:@"5" forKey:@"5"];
    [dir setValue:@"6" forKey:@"6"];
    [dir setValue:@"7" forKey:@"7"];
    [dir setValue:@"8" forKey:@"8"];
    [dir setValue:@"9" forKey:@"9"];
    [dir setValue:@"am" forKey:@"am"];
    [dir setValue:@"pm" forKey:@"pm]"];
    [dir setValue:@":" forKey:@"pp"];
    [dir setValue:@"/" forKey:@"xd"];
    return dir;
}
- (void) printAllFont
{
    for(int i = 0 ; i < mFontArray.count ; i++)
    {
        FontPropertyData* fpd = [mFontArray objectAtIndex:i];
        NSLog(@"font style = %d", fpd.style);
        NSArray* keys = [fpd.font allKeys];
        for(int j = 0 ;  j < keys.count ; j++)
        {
            NSString* key = [keys objectAtIndex:j];
            NSString* str = [fpd.font valueForKey:key];
            NSLog(@"key = %@, str = %@" , key, str);
        }
    }
}
- (NSArray*) getNameComponents: (NSString*) name
{
    NSArray* parts = [name componentsSeparatedByString:@"."];
    NSString* firstName = [parts objectAtIndex:0];
    return [firstName componentsSeparatedByString:@"_"];
}
- (void) initFont
{
    /*
    NSString* homeDir = NSHomeDirectory();
    NSArray* appDir = NSSearchPathForDirectoriesInDomains(NSApplicationSupportDirectory, NSUserDomainMask, YES);
    NSString* str = [appDir objectAtIndex:0];
    NSLog(@"str = %@", str);
    
    NSString* locationDir = [[NSBundle mainBundle] resourcePath];//[[NSBundle mainBundle] pathForResource:@"Impatfont_001_0" ofType:@"png"];
   */ 
    NSDate* start = [NSDate date];
    NSArray* paths = [[NSBundle mainBundle] pathsForResourcesOfType:@".png" inDirectory:nil];
    NSDate* end = [NSDate date];
    NSTimeInterval timeInterv = [end timeIntervalSinceDate:start];
    //NSLog(@"## time = %f ##", timeInterv);
    NSDictionary* fontKeyDefine = [self defineFontKeys];
    for(int i = 0 ; i < paths.count ; i++)
    {
        //NSLog(@"path %d : %@", i, [paths objectAtIndex:i]);
        NSString* name = [paths objectAtIndex:i];
        NSArray* nameArray = [name componentsSeparatedByString:@"/"];
        name = [nameArray lastObject];
        NSRange range = [name rangeOfString:@"Impatfont_"];
        if(range.location == 0  && range.length == 10)
        {
            //NSLog(@"name = %@", name);
            NSArray* comp = [self getNameComponents: name];//[name componentsSeparatedByString:@"_"];
            if(comp.count != 3)
                continue;
            //NSLog(@"comp count = %d", comp.count);
            NSString* styleStr = [comp objectAtIndex:1];
            int v = [styleStr intValue];
            //NSLog(@"v = %d", v);
            FontPropertyData* fpd = [self getFontPropertyByStyle:v];
            if(fpd == nil)
            {
                fpd = [[FontPropertyData alloc] init];
                fpd.style = v;
                fpd.font = [NSMutableDictionary dictionary];
                fpd.size = FONT_NORMAL_SIZE;
                [mFontArray addObject:fpd];
                [fpd release];
            }
            NSString* keyStr = [comp objectAtIndex:2];
            NSString* key = [fontKeyDefine valueForKey:keyStr];
            if(key != nil)
                [fpd.font setValue:name forKey:key];
        }
    }
    //[self printAllFont];
}
- (id) init
{
    self = [super init];
    if(self)
    {
        mFontArray = [NSMutableArray array];
        [mFontArray retain];
        [self initFont];
        //[self initFontStyle:FONT_NORMALSTYLE];
        //[self initFontStyle:FONT_HANDSTYLE];
        //[self initFontStyle:FONT_003];

    }
    return self;
}
- (void)dealloc
{
    [mFontArray release];
    [super dealloc];
}
- (UIImage*) getImage: (NSString*) key style: (int) style size : (enum FONT_SIZE)size
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
+ (NSArray*) getFeedChar
{
    NSArray* ret = [NSArray arrayWithObjects:[NSNumber numberWithUnsignedChar:'P'], [NSNumber numberWithUnsignedChar:'h'], [NSNumber numberWithUnsignedChar:'o'], [NSNumber numberWithUnsignedChar:'T'], [NSNumber numberWithUnsignedChar:'O'],[NSNumber numberWithUnsignedChar:'F'], [NSNumber numberWithUnsignedChar:'r'], [NSNumber numberWithUnsignedChar:'A'], [NSNumber numberWithUnsignedChar:'m'], [NSNumber numberWithUnsignedChar:'E'], nil];
    return ret;
}
@end
