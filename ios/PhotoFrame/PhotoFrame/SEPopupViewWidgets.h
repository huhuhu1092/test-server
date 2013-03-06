//
//  SEOptionsViewWidgets.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-9-21.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
@class FontLabel;
//////////////////
@interface SEPopupImageView : UIImageView
{
    UIImageView* imageView;
}
@property (nonatomic, readonly) UIImageView* imageView;
- (void) setImageViewSize: (CGSize)s;
@end
/////

/////
@interface SEPopupButton : UIView 
{
    UIButton* button;
    FontLabel* buttonText;
}
@property (nonatomic, readonly) UIButton* button;
@property (nonatomic, readonly) FontLabel* buttonText;
- (void) setButtonBackground: (NSString*) normalStr : (NSString*) highlightedStr;
@end
///
@interface SEPopupImageTextLabel : UIView
{
    UIImageView* mBackground;
    UIImageView* mTextImage;
}
- (void) setText: (NSString*)text;
- (void) setBackground: (NSString*) imageName;
@end
////////
@interface SEPopupLabel : UIView
{
    FontLabel* label;
    UIImageView* background;
}
@property (nonatomic, assign) NSString* text;
@property (nonatomic, readonly) FontLabel* label;
@property (nonatomic, readonly) UIImageView* background;
- (void) setTextCenter: (BOOL)b;
- (void) setAppearance;
@end
//////
@interface SETextImageButton : UIView 
{
    UIButton* button;
    UIImageView* indicateView;
    UIImageView* textImageView;
    BOOL mUseScaleTextImage;
}
- (void) setTextImage:(NSString*)textImageStr indicateImage: (NSString*)indicateImageStr alignment: (UITextAlignment) alignment;
- (void) setButtonHandler: (id) target action: (SEL)action;
- (void) setButtonBackground: (NSString*)normal select: (NSString*)select;
- (void) setScaleTextImage: (BOOL) b;
@end
