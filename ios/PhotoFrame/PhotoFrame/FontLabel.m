//
//  FontLabel.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-9-20.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import "FontLabel.h"
@implementation ZFont
@synthesize fontFamily;
@synthesize fontName;
@synthesize font;
- (void) dealloc
{
    [fontFamily release];
    [fontName release];
    [font release];
    [super dealloc];
}
@end
@implementation FontLabel
- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) 
    {
        zFont = [[ZFont alloc] init];
        zFont.font = [UIFont systemFontOfSize:20];
        zFont.fontName = zFont.font.fontName;
        zFont.fontFamily = zFont.font.familyName;
        self.font = zFont.font;
        self.backgroundColor = [UIColor clearColor];
    }
    return self;
}
- (void) setZFont:(ZFont *)font
{
    [zFont release];
    zFont = [font retain];
    self.font = zFont.font;
}
- (ZFont*) getZFont
{
    return zFont;
}
- (id)initWithFrame:(CGRect)frame fontName:(NSString *)fontName pointSize:(CGFloat)pointSize
{
    self = [super initWithFrame:frame];
    if(self)
    {
        zFont = [[ZFont alloc] init];
        zFont.font = [UIFont fontWithName:fontName size:pointSize];
        zFont.fontName = zFont.font.fontName;
        zFont.fontFamily = zFont.font.familyName;
        self.font = zFont.font;
        self.backgroundColor = [UIColor clearColor];
    }
    return self;
}
- (id)initWithFrame:(CGRect)frame zFont:(ZFont *)font
{
    self = [super initWithFrame:frame];
    if(self)
    {
        zFont = [font retain];
        self.font = zFont.font;
        self.backgroundColor = [UIColor clearColor];
    }
    return self;
}
- (void) dealloc
{
    [zFont release];
    [super dealloc];
}
@end
static FontManager* gFontManger = nil;
@implementation FontManager

+ (FontManager*) sharedManager
{
    NSThread* current = [NSThread currentThread];
    NSThread* mainThread = [NSThread mainThread];
    assert(current == mainThread);
    if(gFontManger == nil)
    {
        gFontManger = [[FontManager alloc] init];
    }
    return gFontManger;
}
- (ZFont*) zFontWithName: (NSString*)fontName pointSize: (CGFloat) fontSize
{
    ZFont* font = [[ZFont alloc] init];
    font.fontName = fontName;
    font.font = [UIFont fontWithName:fontName size:fontSize];
    font.fontFamily = font.font.familyName;
    [font autorelease];
    return font;
}
@end
