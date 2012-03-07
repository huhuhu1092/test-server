//
//  SESignatureView.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-2-28.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//


#import "SEFixedView.h"
@class SEDrawTouchView;
@class SEUIScrollView;
@class SEViewNavigator;
@class SESignaturePopupViewController;
@class SEResLoader;
@class SEUIImageView;
@class UserInfo;
@interface SESignatureView : SEFixedView <UIAlertViewDelegate>
{
@private
    SEDrawTouchView* mDrawView;
    SEUIScrollView* mScrollView;
    UIButton* mAddButton;
    UIButton* mDeleteButton;
    SEViewNavigator* mViewNav;
    NSString* mCurrentSignatureName;
    NSString* mSavedSignatureName;
    UIPopoverController* mPopup;
    SESignaturePopupViewController* mSigViewController;
    SEResLoader* mResLoader;
    //SEUIImageView* mCurrentSelectedSignatureImageView;
    int mCurrentSeq;
    int mSignatureImageWidth;
    int mSignatureImageHeight;
    int mAlertType;
    CGPoint mCurrentPoint;
    UIImage* mSelectedSignatureImage;
    UIImage* mNormalSignatureImage;
}
@property (nonatomic, readonly) int mCurrentSeq;
@property (nonatomic, readonly) UIImage* mSelectedSignatureImage;
@property (nonatomic, readonly) UIImage* mNormalSignatureImage;
@property (nonatomic, assign) SEResLoader* mResLoader;
@property (nonatomic, retain) SESignaturePopupViewController* mSigViewController;
@property (nonatomic, assign) SEViewNavigator* mViewNav;
- (void) initData;
- (void) saveContext:(UserInfo*) userInfo;
@end
