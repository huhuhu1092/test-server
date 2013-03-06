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
@class SEDrawTouchPannel;
@class FontLabel;
@interface SESignatureButton : UIView 
{
    UIButton* button;
    UIImageView* indicateView;
    UIImageView* textImageView;
}
- (void) setTextImage:(NSString*)textImageStr indicateImage: (NSString*)indicateImageStr alignment: (UITextAlignment) alignment;
- (void) setButtonHandler: (id) target action: (SEL)action;
- (void) setButtonBackground: (NSString*)normal select: (NSString*)select;
@end
@class SEPopupLabel;
@interface SEScrollViewItem : UIView
{
    UIImageView* bg;
    SEDrawTouchView* content;
    //UITextField* textField;
    SEPopupLabel* textLabel;
    SEViewNavigator* mViewNav;
}
@property (nonatomic, assign) SEViewNavigator* mViewNav;
@property (nonatomic, readonly) UIImageView* bg;
@property (nonatomic, readonly) SEDrawTouchView* content;
@property (nonatomic, readonly) SEPopupLabel* textLabel;
- (id) initWithFrame:(CGRect)frame withViewNav:viewNav;
@end
@class SEColorPad;
@class SEColorPadController;
@class SEColorPadCell;
@class SEStrokeView;
@class SEUIProgressView;
@interface SESignatureView : SEFixedView <UIAlertViewDelegate>
{
@private
    IBOutlet SEDrawTouchPannel* mDrawView;
    IBOutlet UIScrollView* mScrollView;
    IBOutlet SESignatureButton* mAddButton;
    IBOutlet SESignatureButton* mDeleteButton;
    IBOutlet SESignatureButton* mEditButton;
    IBOutlet UIImageView* mSignatureTitleBg;
    IBOutlet UIImageView* mSignatureBg;
    IBOutlet UIImageView* mAddDeleteBg;
    IBOutlet UIImageView* mBg;
    IBOutlet UIImageView* mScrollViewBg;
    //for edit
    UIView* mEditPannel;
    UIImageView* mLeftBg;
    UIImageView* mRightBg;
    UIImageView* mEditBg;
    SEColorPadCell* mColorPad;
    UIImageView* mColorPadBg;
    UISlider* mStokeLineWidth;
    UIImageView* mStokeStyleBg;
    FontLabel* mLeftTimeLabel;

    //UIImageView* mStokeStyle;
    SEStrokeView* mStokeStyle;
    SESignatureButton* mSave;
    SESignatureButton* mClearButton;
    SESignatureButton* mBack;
    SEViewNavigator* mViewNav;
    NSString* mCurrentSignatureName;
    NSString* mSavedSignatureName;
    //UIPopoverController* mPopup;
    SESignaturePopupViewController* mSigViewController;
    SEResLoader* mResLoader;
    int mCurrentSeq;
    int mSignatureImageWidth;
    int mSignatureImageHeight;
    int mAlertType;
    CGPoint mCurrentPoint;
    UIImage* mScrollViewNormalItemImage;
    UIImage* mScrollViewSelectedItemImage;
    CGRect mEditButtonRect;
    CGRect mAddDeleteBgRect;
    UIImageView* mAllBackground;
    SEColorPad* mColorPadPopupView;
    //SEColorPadController* mColorPadController;
    UIImageView* mStrokeImagePreviewBg;
    UIView* mColorPadPopupBgView;
    int mCurrentMode;//edit mode or view mode
    float mLineWidthRatio;
    
    int mDrawingConsumeTime;//milisecond unit
    int mDrawingTotalTime;
    
    int mTouchStartTime;//milisecond
    //time for signature
    //int mSignatureTime;
    
    UIImageView* mInkImageView;
    SEUIProgressView* mInkProgressView;
}
@property (nonatomic, retain) IBOutlet UIImageView* mScrollViewBg;
@property (nonatomic, retain) IBOutlet UIImageView* mBg;
@property (nonatomic, retain) IBOutlet SEDrawTouchPannel* mDrawView;
@property (nonatomic, retain) IBOutlet UIScrollView* mScrollView;
@property (nonatomic, retain) IBOutlet SESignatureButton* mAddButton;
@property (nonatomic, retain) IBOutlet SESignatureButton* mDeleteButton;
@property (nonatomic, retain) IBOutlet SESignatureButton* mEditButton;
@property (nonatomic, retain) IBOutlet UIImageView* mSignatureTitleBg;
@property (nonatomic, retain) IBOutlet UIImageView* mSignatureBg;
@property (nonatomic, retain) IBOutlet UIImageView* mAddDeleteBg;
@property (nonatomic, readonly) int mCurrentSeq;
@property (nonatomic, retain) UIImage* mScrollViewNormalItemImage;
@property (nonatomic, retain) UIImage* mScrollViewSelectedItemImage;
@property (nonatomic, assign) SEResLoader* mResLoader;
@property (nonatomic, assign) SESignaturePopupViewController* mSigViewController;
@property (nonatomic, assign) SEViewNavigator* mViewNav;
- (void) initData;
- (void) saveContext:(UserInfo*) userInfo;
+ (NSArray*) getFeedChar;
@end
