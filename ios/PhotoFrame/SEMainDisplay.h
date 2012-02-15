//
//  SEMainDisplay.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-2-15.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
@class SEResLoader;
@class PHImageView;
enum TOOLBAR_BUTTON_TYPE {OPTION, PLAY_PAUSE, PREVIEW, TOOLBAR_BUTTON_NUM};
@interface SEMainDisplayToolBarView : UIView
{
    UIButton* mButtons[TOOLBAR_BUTTON_NUM];
    SEResLoader* mResLoader;
}
@end
@interface SEMainDisplay : UIViewController
{
    UIView* mRootView;
    PHImageView* mMainDispalyImageView;
    SEMainDisplayToolBarView* mToolBarView;
    SEResLoader* mResLoader;
    CGRect mFrame;
    BOOL mToolBarHidden;
}
- (id) initWithResLoader: (SEResLoader*)resLoader withFrame:(CGRect)frame;
- (void)tapHandler: (UITapGestureRecognizer*)tapGes;
@end
