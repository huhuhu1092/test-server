//
//  SEMainDisplay.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-2-15.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "SEProtocalDefine.h"
@class SEResLoader;
@class PHImageView;
enum TOOLBAR_BUTTON_TYPE {PLAY_PAUSE, MUSIC_SELECT, IMAGE_SELECT, OPTION, PREVIEW, MUSIC_IMAGE_LIST, INVALID_TOOLBAR_BUTTON, TOOLBAR_BUTTON_NUM};
@interface SEMainDisplayToolBarView : UIImageView 
{
    UIButton* mButtons[TOOLBAR_BUTTON_NUM];
    SEResLoader* mResLoader;
    id mToolBarButtonHandleTarget;
    SEL mToolBarButtonHandleAction;
}
- (void)setClickHandleTarget:(id)target withAction:(SEL)action;
- (id) initWithFrame:(CGRect)frame withResLoader:(SEResLoader*) resLoader;
@end
@interface SEImageRect : NSObject {
    int startx;
    int endx;
    int starty;
    int endy;
    int block;
}
@property (nonatomic, assign) int startx;
@property (nonatomic, assign) int endx;
@property (nonatomic, assign) int starty;
@property (nonatomic, assign) int endy;
@property (nonatomic, assign) int block;
@end
@interface SEDateTimeView : UIView 
{
    UIImage* mDataImage;
    SEResLoader* mResLoader;
    NSMutableArray* mImageRectArray;
}
- (id) initWithFrame:(CGRect)frame withResLoader:(SEResLoader*) resLoader;
@end
@interface SEMainDisplay : UIView <SEAdjustContentView>
{
    PHImageView* mMainDispalyImageView;
    SEMainDisplayToolBarView* mToolBarView;
    SEDateTimeView* mDateTimeView;
    SEResLoader* mResLoader;
    CGRect mFrame;
    BOOL mToolBarHidden;

}
- (void) setToolBarButtonHandleTarget: (id) target withAction:(SEL)action;
- (id) initWithResLoader: (SEResLoader*)resLoader withFrame:(CGRect)frame;
- (void) loadView;
- (void)tapHandler: (UITapGestureRecognizer*)tapGes;
@end
