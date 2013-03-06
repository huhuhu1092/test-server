//
//  FontLabel.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-9-20.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
@interface ZFont : NSObject
{
    NSString* fontFamily;
    NSString* fontName;
    UIFont* font;
}
@property (nonatomic, retain) UIFont* font;
@property (nonatomic, retain) NSString* fontFamily;
@property (nonatomic, retain) NSString* fontName;
@end
@interface FontLabel : UILabel
{
    ZFont* zFont;
}
- (id)initWithFrame:(CGRect)frame fontName:(NSString *)fontName pointSize:(CGFloat)pointSize;
- (id)initWithFrame:(CGRect)frame zFont:(ZFont *)font;
- (void) setZFont:(ZFont *)font;
- (ZFont*) getZFont;
@end
@interface FontManager : NSObject
+ (FontManager*) sharedManager;
- (ZFont*) zFontWithName: (NSString*)fontName pointSize: (CGFloat) fontSize;
@end
