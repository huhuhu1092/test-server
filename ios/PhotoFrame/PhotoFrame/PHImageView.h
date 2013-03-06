//
//  PHImageView.h
//  PhotoFrame
//
//  Created by 陈勇 on 11-11-10.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
@interface MyClipRect : NSObject
{
@public
    CGRect clipRect;
}
@property (nonatomic) CGRect clipRect;
@end
@class SEViewNavigator;
@interface PHImageView : UIView
{
    NSArray* clippingRectList;
    UIImage* image;
    NSMutableArray* pointArrayList;
    CGFloat lineWidth;
    UIImageView* left;
    UIImageView* right;
    UIImageView* top;
    UIImageView* bottom;
    SEViewNavigator* mViewNav;
    BOOL animFinished[4];
    BOOL needDrawFrame;
    UIImage* blockImage;
    BOOL mRotateScreen;
    UIImage* mSignatureImage;
}

@property (nonatomic, assign) SEViewNavigator* mViewNav;
@property (nonatomic, assign) BOOL mRotateScreen;
@property (nonatomic, retain) UIImage* image;
- (void)drawRect:(CGRect)rect;
- (void) clearClippingList;
- (void) setClipRectList: (const MyClipRect**) clipRect count:(int)size;
- (void) setPoints:(NSArray*)points;
- (void) playFrameAnim;
- (void) paintSignatureImage: (CGImageRef)imageRef frame: (CGRect)frame;
- (CGImageRef) createSignatureImage: (CGImageRef)imageRef frame: (CGRect) frame;
@end
