//
//  SEOptionsView.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-2-22.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "SEProtocalDefine.h"
#import "SEFixedView.h"
#import "SEViewNavigator.h"
#import "FontLabel.h"
@class SEResLoader;
@class SEOptionLeftLabel;
@class SEUIProgressView;
@class SEUISwitch;
@class SEProduct;
@class SEDrawTouchPannel;
enum OPTION_LEFT_BAR_TYPE {PLAY_SETTING, WIDGET_SETTING, USER_INFOR_SETTING, APP_STORE_SETTING,  ABOUT_SETTING, HELP_SETTING, INVALID_LEFT_BAR_VIEW, OPTION_LEFT_BAR_NUM};
enum LOCK_TYPE {LOCK_BUY, LOCK_ACHIEVE, LOCK_LEVELUP};

//////////////////
@interface SEOptionsImageView : UIImageView
{
    UIImageView* imageView;
}
@property (nonatomic, readonly) UIImageView* imageView;
- (void) setImageViewSize: (CGSize)s;
@end
/////
@interface SEOptionsRadioButton : NSObject
{
    NSMutableArray* mButtonView;// it is a UIView but not UIButton
    NSMutableArray* mButtonViewValue;
    NSNumber* mCurrentValue;
    NSString* mNormalResStr;
    NSString* mSelectedResStr;
    id mValueChangeHandler;
    SEL mValueChangeAction;
}
+ (id) createRadioButton: (NSArray*) views values: (NSArray*)values normal: (NSString*)normalStr selected: (NSString*)selectedStr currentValue: (int) value;
- (void) setCurrentValue: (int)value;
- (void) setValueChangeHandler: (id)handler action: (SEL)action;
@end
///////
@interface SEOptionsLockView : UIView
{
    UIImageView* lockbg;
    UIImageView* lock;
    FontLabel* textLabel;
}
- (void) setLock: (BOOL) b type: (LOCK_TYPE) lockType;
- (void) setText: (NSString*) text;
- (void) setText:(NSString *)text lines: (int)lineNum;
- (void) setLockStartPoint: (CGPoint)p;
- (id) initWithFrame: (CGRect) rect lockSize: (CGSize) size;
@end
/////
@interface SEOptionsButton : UIView 
{
    UIButton* button;
    FontLabel* buttonText;
}
@property (nonatomic, readonly) UIButton* button;
@property (nonatomic, readonly) FontLabel* buttonText;
@end
///
@interface SEOptionsStoreItem : UIView
{
    UIImageView* background;
    UIImageView* content;
    UIImageView* discountImageView;
    FontLabel* label;
    UIButton* button;
    FontLabel* buttonText;
    FontLabel* buttonText2;
    FontLabel* dateLabelView;
    FontLabel* updatePriceFailText;
    SEOptionsLockView* lockView;
    UIImageView* buiedView;
    NSString* productId;
    int productType;
    int touchState;
    CGPoint beginPoint;
    id target;
    SEL action;
    BOOL bSelected;
    UIView* drawLineView;
    UIImageView* connectionImageView;
    NSTimer* connectionTimer;
    float connectionViewAngle;
    BOOL hasUpdatePrice;
}
@property (nonatomic, assign) BOOL hasUpdatePrice;
@property (nonatomic, assign) int productType;
@property (nonatomic, retain) NSString* productId;
@property (nonatomic, readonly) FontLabel* label;
@property (nonatomic, readonly) UIImageView* content;
@property (nonatomic, readonly) UIImageView* background;
@property (nonatomic, readonly) UIButton* button;
@property (nonatomic, readonly) FontLabel* buttonText;
@property (nonatomic, readonly) FontLabel* buttonText2;
@property (nonatomic, readonly) UIImageView* connectionImageView;
- (void) setLock: (BOOL) b type:(LOCK_TYPE)t;
- (BOOL) isLocked;
- (void) setBuied: (BOOL) b;
- (BOOL) isBuied;
- (BOOL) isSelected;
- (void) setSelected: (BOOL) b;
- (void) setTarget: (id) t action: (SEL)a;
- (void) setContentImage: (UIImage*)image;
- (void) setDiscountImage: (UIImage*)image;
- (void) setUpdatePriceFailed: (NSString*)text;
- (id) initWithFrame:(CGRect)frame product: (SEProduct*)product;
- (void) startConnection;
- (void) endConnection;
- (void) setExpiredDate :(NSString*)str;
@end
////////
//
//use tag to indicate which style label is create
//1 : indicate horizonal label
// 2 : indicate vertical label
@interface SEOptionsStoreTwoLabelItem : SEOptionsStoreItem
{
    //UIImageView* background;
    //UIImageView* content;
    //FontLabel* label1;
    FontLabel* label2;
    //UIButton* button;
    //FontLabel* buttonText;
    //SEOptionsLockView* lockView;
    //UIImageView* buiedView;
}
//@property (nonatomic, readonly) UIImageView* background;
//@property (nonatomic, readonly) UIImageView* content;
//@property (nonatomic, readonly) FontLabel* label1;
@property (nonatomic, readonly) FontLabel* label2;
//@property (nonatomic, readonly) UIButton* button;
//@property (nonatomic, readonly) FontLabel* buttonText;
//- (void) setLock: (BOOL) b type: (LOCK_TYPE) t;
//- (void) setBuied: (BOOL) b;
//- (BOOL) isSelected;
@end
////////
@interface SEOptionsLabel : UIView
{
    FontLabel* label;
    UIImageView* background;
}
@property (nonatomic, assign) NSString* text;
@property (nonatomic, readonly) FontLabel* label;
@property (nonatomic, readonly) UIImageView* background;
- (void) setTextCenter: (BOOL)b;
@end
///////////////////////
@class SESlider;
@interface SEOptionsSlider : UIView
{
    SESlider* slider;
    SEOptionsLockView* lockView;
}
@property (nonatomic, readonly) UISlider* slider;
- (void) setLock: (BOOL) l lockType: (LOCK_TYPE) lockType;
@end
///////////////////////
@class FontLabel;
@interface SESelectScrollChildView : UIView
{
    UIImageView* mShellImageView;
    UIImageView* mContentView;
    FontLabel* mText;
    UIImageView* mLockImageView;
    UIImageView* mLockBgView;
    SEViewNavigator* mViewNav;
    BOOL mHideShell;
    CGSize mContentSize;
}
@property (nonatomic, assign) CGSize mContentSize;
@property (nonatomic, assign) SEViewNavigator* mViewNav;
@property (nonatomic, assign) UIImage* image;
@property (nonatomic, readonly) UIImageView* mLockImageView;
@property (nonatomic, readonly) UIImageView* mLockBgView;
@property (nonatomic, readonly) FontLabel* mText;
- (void) hideShell: (BOOL) b;
- (BOOL) isShellHidden;
- (BOOL) isLocked;
- (void) setLock : (BOOL) b lockType:(LOCK_TYPE)lockType;
- (id) initWithViewNav: (SEViewNavigator*)viewNav;
- (CGSize) getContentSize;
- (void) setImageCenter : (CGSize)size;
- (void )changeChildFrame ;
- (void) changeContentFrame: (CGSize)s : (CGSize) contentSize;
@end
/////
@interface SESelectScrollView : UIView
{
    UIImageView* mBgView;
    UIScrollView* mScrollView;
    UIImageView* mMaskView;
    SEViewNavigator* mViewNav;
    UIView* mContentParent;
    float startx;
    
    SEOptionsLockView* mLockView;
}

@property (nonatomic, readonly) UIScrollView* mScrollView;
@property (nonatomic, retain) UIView* mContentParent;
- (void) initView: (SEViewNavigator*)viewNav;
- (void) setData;
- (SESelectScrollChildView*) addChildItem: (UIImage*) contentImage viewNav:(SEViewNavigator*)viewNav text: (NSString*)text;
- (void) setContentSize;
- (void) setSelected: (SESelectScrollChildView*)v;
- (void) setLock: (BOOL) l lockType: (LOCK_TYPE) lockType;
- (void) clearAllChild;
@end
////////////////////
/*
@class SEBrushScrollViewImpl;
@interface SEBrushScrollView : UIView
{
    UIImage* mBgImage;
    SEBrushScrollViewImpl* mScrollView;
    SEViewNavigator* mViewNav;
}
@property (nonatomic, readonly) UIScrollView* mScrollView;
@property (nonatomic, assign) SEViewNavigator* mViewNav;
- (void) initScrollView: (SEViewNavigator*)viewNav;
- (void) clearSelectedBg;
@end
 */
/////////////////////
//////////////
@class SEOptionsView;
@interface SEOptionsSettingViewController : NSObject
{
    SEViewNavigator* mViewNav;
    UIView* mView;
    SEOptionsView* mOptionsView;
}
- (UIView*) getView;
- (void) removeFromParent : (UIView*)parent;
- (void) addViewToParent: (UIView*) parent;
- (void) setData;
- (void) setImage: (UIImageView*)imageView name: (NSString*) name;
- (float) getHeight;
- (void) update;

@property (nonatomic, assign) SEViewNavigator* mViewNav;
@property (nonatomic, assign) SEOptionsView* mOptionsView;
@end
@interface SEOptionsPlaySetting : SEOptionsSettingViewController
{
    enum PLAY_MODE {SEQUENCE, RANDOM, INTEL};// if change this define, please change code in PainterManager.mm too.
    enum BRUSH_MODE {USE_SELECT, EVERY_IMAGE, EVERY_TIME};
    IBOutlet SEOptionsLabel* mDrawSettingTitle;
    IBOutlet SEOptionsImageView* mApple;
    IBOutlet SEOptionsImageView* mDrawSettingBg;
    IBOutlet SEOptionsLabel* mQualityTitle;
    IBOutlet SEOptionsLabel* mTimesTitle;
    IBOutlet SEOptionsSlider* mQualitySlider;
    IBOutlet SEOptionsSlider* mTimesSlider;
    IBOutlet SEOptionsLabel* mQualityValue;
    IBOutlet SEOptionsLabel* mTimesValue;
    IBOutlet SEOptionsLabel* mWaitingTimeTitle;
    IBOutlet SEOptionsLabel* mSecondValue;
    IBOutlet SEOptionsLabel* mSecondLabel;
    IBOutlet SEOptionsSlider* mWaitingTimeSlider;
    IBOutlet SEOptionsImageView* mWaitingTimeBg;
    IBOutlet SEOptionsLabel* mPlayModeTitle;
    IBOutlet SEOptionsImageView* mPlayModeBg;
    
    IBOutlet SEOptionsLabel* mPlaySequenceTitle;
    IBOutlet SEOptionsLabel* mPlayRandomTitle;
    IBOutlet SEOptionsLabel* mPlayIntelTitle;
    IBOutlet SEOptionsImageView* mPlaySequence;
    IBOutlet SEOptionsImageView* mPlayRandom;
    IBOutlet SEOptionsImageView* mPlayIntelligence;
    
    IBOutlet SEOptionsLabel* mDrawBrushDefaultTitle;
    IBOutlet SEOptionsLabel* mDrawBrushEveryImageTitle;
    IBOutlet SEOptionsLabel* mDrawBrushEveryTimeTitle;
    IBOutlet SEOptionsImageView* mDrawBrushDefault;
    IBOutlet SEOptionsImageView* mDrawBrushEveryImage;
    IBOutlet SEOptionsImageView* mDrawBrushEveryTime;
    
    IBOutlet SEOptionsLabel* mBrushTitle;
    IBOutlet SESelectScrollView* mBrushScrollView;
    IBOutlet SEOptionsSlider* mAngleAdjust;
    IBOutlet SEOptionsSlider* mTransparentAdjust;
    IBOutlet SEOptionsLabel* mAngleLabel;
    IBOutlet SEOptionsLabel* mTransparentLabel;
    IBOutlet SEOptionsLabel* mAngleAdjustValue;
    IBOutlet SEOptionsLabel* mTransparentValue;
    IBOutlet SEOptionsImageView* mBrushBg;
    
    IBOutlet SEOptionsLabel* mImageSizeFilterTitleLabel;
    IBOutlet SEOptionsLabel* mImageSizeFilterTitle;
    
    IBOutlet SEOptionsLabel* mRotateLockTitle;
    IBOutlet SEUISwitch* mRotateLockSwitch;
    
    IBOutlet SEUISwitch* mImageSizeFilterSwitch;
    IBOutlet SEOptionsImageView* mImageSizeFilterBg;
    
    IBOutlet SEOptionsLabel* mDensityLabel;
    IBOutlet SEOptionsLabel* mDensityNum;
    IBOutlet SEOptionsSlider* mDensitySlider;
    
    IBOutlet SEOptionsLabel* mEdgeDetectLabel;
    IBOutlet SEOptionsLabel* mEdgeDetectValue;
    IBOutlet SEOptionsSlider* mEdgeDetectSlider;
    
    IBOutlet UIImageView* mBrushSetingBg;
    NSString* mCurrentQuality;
    NSString* mCurrentTimes;
    PLAY_MODE mPlayMode; // 0: sequnce, 1: random, 2: intelligence
    int mSecond;
    SEOptionsLockView* mLockViewForDrawing;
    SEOptionsLockView* mLockViewForBrushSetting;
    SEOptionsLockView* mLockViewForBrush;
    SEOptionsRadioButton* mPlayModeRadioButton;
    SEOptionsRadioButton* mDrawBrushModeRadioButton;
}
@property (nonatomic, retain) IBOutlet UIImageView* mBrushSetingBg;
@property (nonatomic, retain) IBOutlet SEOptionsLabel* mDrawBrushDefaultTitle;
@property (nonatomic, retain) IBOutlet SEOptionsLabel* mDrawBrushEveryImageTitle;
@property (nonatomic, retain) IBOutlet SEOptionsLabel* mDrawBrushEveryTimeTitle;
@property (nonatomic, retain) IBOutlet SEOptionsImageView* mDrawBrushDefault;
@property (nonatomic, retain) IBOutlet SEOptionsImageView* mDrawBrushEveryImage;
@property (nonatomic, retain) IBOutlet SEOptionsImageView* mDrawBrushEveryTime;

@property (nonatomic, retain) IBOutlet SEOptionsLabel* mRotateLockTitle;
@property (nonatomic, retain) IBOutlet SEUISwitch* mRotateLockSwitch;
@property (nonatomic, retain) IBOutlet SEOptionsLabel* mDensityLabel;
@property (nonatomic, retain) IBOutlet SEOptionsLabel* mDensityNum;
@property (nonatomic, retain) IBOutlet SEOptionsSlider* mDensitySlider;

@property (nonatomic, retain) IBOutlet SEOptionsLabel* mEdgeDetectLabel;
@property (nonatomic, retain) IBOutlet SEOptionsLabel* mEdgeDetectValue;
@property (nonatomic, retain) IBOutlet SEOptionsSlider* mEdgeDetectSlider;

@property (nonatomic, retain) IBOutlet SEOptionsLabel* mImageSizeFilterTitleLabel;
@property (nonatomic, retain) IBOutlet SEOptionsLabel* mImageSizeFilterTitle;
@property (nonatomic, retain) IBOutlet SEUISwitch* mImageSizeFilterSwitch;
@property (nonatomic, retain) IBOutlet SEOptionsImageView* mImageSizeFilterBg;
@property (nonatomic, retain) NSString* mCurrentQuality;
@property (nonatomic, retain) NSString* mCurrentTimes;
@property (nonatomic, retain) IBOutlet SEOptionsImageView* mBrushBg;
@property (nonatomic, retain) SEOptionsLabel* mAngleAdjustValue;
@property (nonatomic, retain) SEOptionsLabel* mTransparentValue;
@property (nonatomic, retain) SEOptionsImageView* mWaitingTimeBg;
@property (nonatomic, retain) SEOptionsImageView* mDrawingSettingBg;
@property (nonatomic, retain)  SEOptionsLabel* mDrawSettingTitle;
@property (nonatomic, retain)  SEOptionsImageView* mApple;
@property (nonatomic, retain)  SEOptionsLabel* mQualityTitle;
@property (nonatomic, retain)  SEOptionsLabel* mTimesTitle;
@property (nonatomic, retain)  SEOptionsSlider* mQualitySlider;
@property (nonatomic, retain)  SEOptionsSlider* mTimesSlider;
@property (nonatomic, retain)  SEOptionsLabel* mQualityValue;
@property (nonatomic, retain)  SEOptionsLabel* mTimesValue;
@property (nonatomic, retain)  SEOptionsLabel* mWaitingTimeTitle;
@property (nonatomic, retain)  SEOptionsLabel* mSecondValue;
@property (nonatomic, retain)  SEOptionsLabel* mSecondLabel;
@property (nonatomic, retain)  SEOptionsSlider* mWaitingTimeSlider;
@property (nonatomic, retain)  SEOptionsLabel* mPlayModeTitle;
@property (nonatomic, retain)  SEOptionsImageView* mPlayModeBg;
@property (nonatomic, retain)  SEOptionsLabel* mPlaySequenceTitle;
@property (nonatomic, retain)  SEOptionsLabel* mPlayRandomTitle;
@property (nonatomic, retain)  SEOptionsLabel* mPlayIntelTitle;
@property (nonatomic, retain)  SEOptionsImageView* mPlaySequence;
@property (nonatomic, retain)  SEOptionsImageView* mPlayRandom;
@property (nonatomic, retain)  SEOptionsImageView* mPlayIntelligence;
@property (nonatomic, retain)  SEOptionsLabel* mBrushTitle;
@property (nonatomic, retain)  SESelectScrollView* mBrushScrollView;
@property (nonatomic, retain)  SEOptionsSlider* mAngleAdjust;
@property (nonatomic, retain)  SEOptionsSlider* mTransparentAdjust;
@property (nonatomic, retain)  SEOptionsLabel* mAngleLabel;
@property (nonatomic, retain)  SEOptionsLabel* mTransparentLabel;
@end

/////////
@class SEDrawTouchView;
@class SETextImageButton;
@interface SEOptionsWidgetSetting : SEOptionsSettingViewController
{
    IBOutlet SEOptionsLabel* mTimeLabel;
    IBOutlet SEOptionsLabel* mTimeViewTitle;
    IBOutlet SEOptionsImageView* mTimeViewDisplayBg;
    IBOutlet SEUISwitch* mTimeDisplaySwitch;
    IBOutlet SESelectScrollView* mTimeStyleSelectScrollView;
    IBOutlet SEOptionsLabel* mPowerTitle;
    IBOutlet SEOptionsLabel* mPowerViewTitle;
    IBOutlet SEOptionsLabel* mBigIconTitle;
    IBOutlet SEUISwitch* mPowerViewSwitch;
    IBOutlet SEUISwitch* mBigIconSwitch;
    IBOutlet SEOptionsImageView* mPowerBg;
    
    IBOutlet SEOptionsLabel* mSignatureTitle;
    IBOutlet SEOptionsLabel* mSignatureViewText;
    IBOutlet SEOptionsLabel* mAutoColorText;
    IBOutlet SEOptionsLabel* mSignatureSizeText;
    IBOutlet SEOptionsLabel* mSignatureSizeResult;
    IBOutlet UISlider* mSigSizeSlider;
    IBOutlet SEUISwitch* mSigViewSwitch;
    IBOutlet SEUISwitch* mAutoColorSwitch;
    IBOutlet SEDrawTouchPannel* mDrawView;
    IBOutlet UIImageView* mSignatureBg;
    IBOutlet UIImageView* mPreviewBg;
    
    IBOutlet SETextImageButton* mSignatureEditButton;
    
    IBOutlet SEOptionsLabel* mPowerViewSize;
    IBOutlet UISlider* mPowerViewSizeSlider;
    SEOptionsLockView* mLockViewForSignatureEdit;
}

@property (nonatomic, retain) IBOutlet UISlider* mPowerViewSizeSlider;
@property (nonatomic, retain) IBOutlet SEOptionsLabel* mPowerViewSize;
@property (nonatomic, retain) IBOutlet SETextImageButton* mSignatureEditButton;
@property (nonatomic, retain) IBOutlet UIImageView* mPreviewBg;
@property (nonatomic, retain) IBOutlet SEOptionsImageView* mPowerBg;
@property (nonatomic, retain) IBOutlet SEOptionsLabel* mTimeLabel;
@property (nonatomic, retain) IBOutlet SEOptionsLabel* mTimeViewTitle;
@property (nonatomic, retain) IBOutlet SEOptionsImageView* mTimeViewDisplayBg;
@property (nonatomic, retain) IBOutlet SEUISwitch* mTimeDisplaySwitch;
@property (nonatomic, retain) IBOutlet SESelectScrollView* mTimeStyleSelectScrollView;
@property (nonatomic, retain) IBOutlet SEOptionsLabel* mPowerTitle;
@property (nonatomic, retain) IBOutlet SEOptionsLabel* mPowerViewTitle;
@property (nonatomic, retain) IBOutlet SEOptionsLabel* mBigIconTitle;
@property (nonatomic, retain) IBOutlet SEUISwitch* mPowerViewSwitch;
@property (nonatomic, retain) IBOutlet SEUISwitch* mBigIconSwitch;
///////////
@property (nonatomic, retain) IBOutlet SEOptionsLabel* mSignatureTitle;
@property (nonatomic, retain) IBOutlet SEOptionsLabel* mSignatureViewText;
@property (nonatomic, retain) IBOutlet SEOptionsLabel* mAutoColorText;
@property (nonatomic, retain) IBOutlet SEOptionsLabel* mSignatureSizeText;
@property (nonatomic, retain) IBOutlet SEOptionsLabel* mSignatureSizeResult;
@property (nonatomic, retain) IBOutlet UISlider* mSigSizeSlider;
@property (nonatomic, retain) IBOutlet SEUISwitch* mSigViewSwitch;
@property (nonatomic, retain) IBOutlet SEUISwitch* mAutoColorSwitch;
@property (nonatomic, retain) IBOutlet SEDrawTouchPannel* mDrawView;
@property (nonatomic, retain) IBOutlet UIImageView* mSignatureBg;
/////
- (void) setTimeViewShow: (NSNumber*) bOk;
- (void) setPowerViewShow: (NSNumber*) bOk;
- (void) setPowerBigIcon: (NSNumber*) bOk;
- (void) setLightViewShow: (NSNumber*)bOk;
//- (void) setAutoControlLight: (NSNumber*)bOk;
- (void) setSignatureShow: (NSNumber*)bOk;
- (void) setSignatureAutoColor: (NSNumber*)bOk;
@end
/////
@class SEOptionsUserGetView;
@interface SEOptionsUserInfoSetting : SEOptionsSettingViewController
{
    enum USER_GET_TYPE {IMAGE_GET, MUSIC_GET, USER_GET_TYPE_NUM};
    enum USER_NEXT_GET_TYPE {IMAGE_NEXT_GET, MUSIC_NEXT_GET, BRUSH_NEXT_GET, TIMEFONT_NEXT_GET, NEXT_GET_TYPE_NUM};
    IBOutlet UIImageView* mLevelProgessBarBg;
    IBOutlet SEUIProgressView* mLevelProgressView;
    IBOutlet UIImageView* mUserGetBg;
    IBOutlet UIImageView* mUserNextGetBg;
    IBOutlet UIScrollView* mUserNextGetScrollView;
    IBOutlet UIScrollView* mAchiveScrollView;
    ////
    IBOutlet SEOptionsLabel* mLevelTitle;
    IBOutlet UIImageView* mLevelBg;
    IBOutlet UIImageView* mLevelTextBg;
    IBOutlet FontLabel* mLevelValue;
    IBOutlet FontLabel* mLevelText;
    IBOutlet SEOptionsImageView* mLevelPlusIcon;
    IBOutlet SEOptionsLabel* mNextLevelGetTitle;
    IBOutlet SEOptionsLabel* mAchieveTitle;
    
    IBOutlet FontLabel* mLevelPointView;
    SEOptionsUserGetView* mUserGetProgressView[USER_GET_TYPE_NUM];
    UIView* mNextGetContentParent;
    int mNextGetNum[NEXT_GET_TYPE_NUM];
    UIView* mAchievementParentView;
    SEUIProgressView* mAchievementProcessView[USER_MEDAL_COUNT][MEDAL_LEVEL_COUNT];
    UIImageView* mAchievementIconBgView[USER_MEDAL_COUNT][MEDAL_LEVEL_COUNT];
    UIImageView* mAchievementIconFgView[USER_MEDAL_COUNT][MEDAL_LEVEL_COUNT];
    FontLabel* mAchievementPointView[USER_MEDAL_COUNT][MEDAL_LEVEL_COUNT];
    float myHeight;
    
    SEOptionsLockView* mLevelLockView;
    SEOptionsLockView* mArchieveLockView;
}
@property (nonatomic, retain) IBOutlet FontLabel* mLevelPointView;
@property (nonatomic, retain) IBOutlet UIImageView* mLevelProgessBarBg;
@property (nonatomic, retain) IBOutlet SEUIProgressView* mLevelProgressView;
@property (nonatomic, retain) IBOutlet UIImageView* mUserGetBg;
@property (nonatomic, retain) IBOutlet UIImageView* mUserNextGetBg;
@property (nonatomic, retain) IBOutlet UIScrollView* mUserNextGetScrollView;
@property (nonatomic, retain) IBOutlet UIScrollView* mAchiveScrollView;
////
@property (nonatomic, retain) IBOutlet SEOptionsLabel* mLevelTitle;
@property (nonatomic, retain) IBOutlet UIImageView* mLevelBg;
@property (nonatomic, retain) IBOutlet UIImageView* mLevelTextBg;
@property (nonatomic, retain) IBOutlet FontLabel* mLevelValue;
@property (nonatomic, retain) IBOutlet FontLabel* mLevelText;
@property (nonatomic, retain) IBOutlet SEOptionsImageView* mLevelPlusIcon;
@property (nonatomic, retain) IBOutlet SEOptionsLabel* mNextLevelGetTitle;
@property (nonatomic, retain) IBOutlet SEOptionsLabel* mAchieveTitle;
-(void) updateInfo;
@end
//////
/*
@interface SEOptionsSignatureSetting : SEOptionsSettingViewController
{
    //IBOutlet UIImageView* mBg;
    IBOutlet UIImageView* mSigTitleBg;
    IBOutlet UIImageView* mSigBg;
    IBOutlet UIImageView* mSigViewTextBg;
    IBOutlet UIImageView* mAutoColorTextBg;
    IBOutlet UIImageView* mSigSizeTextBg;
    IBOutlet UIImageView* mSigSizeResultTextBg;
    IBOutlet UISlider* mSigSizeSlider;
    IBOutlet UIImageView* mPreviewBg;
    IBOutlet SEUISwitch* mSigViewSwitch;
    IBOutlet SEUISwitch* mAutoColorSwitch;
    
    IBOutlet FontLabel* mSignatureTitleLabel;
    IBOutlet FontLabel* mSigViewTextLabel;
    IBOutlet FontLabel* mAutoColorTextLabel;
    IBOutlet FontLabel* mSigSizeTextLabel;
    IBOutlet FontLabel* mSigResultLabel;
    
    IBOutlet SEDrawTouchPannel* mDrawView;
    float myHeight;
}
@property (nonatomic, retain) IBOutlet SEDrawTouchPannel* mDrawView;
@property (nonatomic, retain) IBOutlet FontLabel* mSignatureTitleLabel;
@property (nonatomic, retain) IBOutlet FontLabel* mSigViewTextLabel;
@property (nonatomic, retain) IBOutlet FontLabel* mAutoColorTextLabel;
@property (nonatomic, retain) IBOutlet FontLabel* mSigSizeTextLabel;
@property (nonatomic, retain) IBOutlet FontLabel* mSigResultLabel;

@property (nonatomic, retain) IBOutlet SEUISwitch* mSigViewSwitch;
@property (nonatomic, retain) IBOutlet SEUISwitch* mAutoColorSwitch;
//@property (nonatomic, retain) IBOutlet UIImageView* mBg;
@property (nonatomic, retain) IBOutlet UIImageView* mSigTitleBg;
@property (nonatomic, retain) IBOutlet UIImageView* mSigBg;
@property (nonatomic, retain) IBOutlet UIImageView* mSigViewTextBg;
@property (nonatomic, retain) IBOutlet UIImageView* mAutoColorTextBg;
@property (nonatomic, retain) IBOutlet UIImageView* mSigSizeTextBg;
@property (nonatomic, retain) IBOutlet UIImageView* mSigSizeResultTextBg;
@property (nonatomic, retain) IBOutlet UISlider* mSigSizeSlider;
@property (nonatomic, retain) IBOutlet UIImageView* mPreviewBg;
- (void) setSignatureShow: (NSNumber*)bOk;
- (void) setSignatureAutoColor: (NSNumber*)bOk;

@end
 */
/////
@class SEProductManager;
@interface SEOptionsAboutSetting : SEOptionsSettingViewController
{
    //IBOutlet UIImageView* mBg;
    IBOutlet UIImageView* mAboutTitleBg;
    IBOutlet UIImageView* mAboutBg;
    IBOutlet UIImageView* mBrand;
    IBOutlet UIImageView* mSuggestTitleBg;
    IBOutlet UIImageView* mSuggestBg;
    IBOutlet UIImageView* mStatusBg;
    IBOutlet UIButton* mSubmit;
    IBOutlet UITextView* mTextView;
    IBOutlet FontLabel* mCopyRightText;
    IBOutlet SEOptionsButton* mSubmitButton;
    IBOutlet FontLabel* mAboutTitle;
    IBOutlet FontLabel* mSuggestTitle;
    IBOutlet FontLabel* mThanksTitle;
    
    IBOutlet UIImageView* mDeveloperBg;
    IBOutlet UITextView* mDeveloperText;
    IBOutlet UIButton* mFacebookButton;
    IBOutlet FontLabel* mFacebookText;
    
    IBOutlet FontLabel* mAppVersionText;
    //for test
    IBOutlet UILabel* mDownloadLabel;
    IBOutlet UIButton* mDownloadButton;
    IBOutlet UIButton* mClearDataButton;
}
@property (nonatomic, retain) IBOutlet UIButton* mClearDataButton;
@property (nonatomic, retain) IBOutlet FontLabel* mAppVersionText;
@property (nonatomic, retain) IBOutlet FontLabel* mFacebookText;
@property (nonatomic, retain) IBOutlet UIButton* mFacebookButton;
@property (nonatomic, retain) IBOutlet UIImageView* mDeveloperBg;
@property (nonatomic, retain) IBOutlet UITextView* mDeveloperText;
@property (nonatomic, retain) IBOutlet FontLabel* mThanksTitle;
@property (nonatomic, retain) IBOutlet FontLabel* mAboutTitle;
@property (nonatomic, retain) IBOutlet FontLabel* mSuggestTitle;
@property (nonatomic, retain) IBOutlet SEOptionsButton* mSubmitButton;
@property (nonatomic, retain) IBOutlet FontLabel* mCopyRightText;
@property (nonatomic, retain) IBOutlet UILabel* mDownloadLabel;
@property (nonatomic, retain) IBOutlet UIButton* mDownloadButton;
@property (nonatomic, retain) IBOutlet UITextView* mTextView;
//@property (nonatomic, retain) IBOutlet UIImageView* mBg;
@property (nonatomic, retain) IBOutlet UIImageView* mAboutTitleBg;
@property (nonatomic, retain) IBOutlet UIImageView* mAboutBg;
@property (nonatomic, retain) IBOutlet UIImageView* mBrand;
@property (nonatomic, retain) IBOutlet UIImageView* mSuggestTitleBg;
@property (nonatomic, retain) IBOutlet UIImageView* mSuggestBg;
@property (nonatomic, retain) IBOutlet UIImageView* mStatusBg;
@property (nonatomic, retain) IBOutlet UIButton* mSubmit;
- (IBAction) downloadHandler:(id)sender;
- (IBAction) facebookURLButtonHandler: (UIButton*)button;
- (IBAction) clearData:(id)sender;
+ (NSArray*) getFeedChar;
@end
//////
/*
@class SEInAppPurchaseTransactionObserver;
@interface SEOptionsAppStoreSetting : SEOptionsSettingViewController
{
    IBOutlet SEOptionsLabel* mPlayLabel;
    IBOutlet SEOptionsStoreTwoLabelItem* mlevelUpToMaxItem;
    IBOutlet SEOptionsStoreTwoLabelItem* mAddImageNumberItem;
    IBOutlet SEOptionsStoreTwoLabelItem* mAddMusicNumberItem;
    IBOutlet SEOptionsLabel* mBrushLabel;
    IBOutlet SEOptionsLabel* mBasicLabel;
    IBOutlet SEOptionsStoreItem* mBasicItem;
    IBOutlet SEOptionsStoreItem* mBrushFunctionItem;
    NSMutableArray* mBrushList;
    
    IBOutlet SEOptionsLabel* mTotalLabel;
    IBOutlet SEOptionsLabel* mSupportLabel;
    IBOutlet SEOptionsButton* mSubmitButton;
    IBOutlet SEOptionsLabel* mTotalPriceLabel;
    IBOutlet UIImageView* mTotalBlank;
    
    float mTotalPrice;
    NSMutableArray* mProductIdArray;
    SEInAppPurchaseTransactionObserver* mProductTransaction;
}
@property (nonatomic, retain) IBOutlet SEOptionsStoreItem* mBrushFunctionItem;
@property (nonatomic, retain) IBOutlet SEOptionsLabel* mBasicLabel;
@property (nonatomic, retain) IBOutlet SEOptionsStoreItem* mBasicItem;
@property (nonatomic, retain) IBOutlet SEOptionsLabel* mPlayLabel;
@property (nonatomic, retain) IBOutlet SEOptionsStoreTwoLabelItem* mlevelUpToMaxItem;
@property (nonatomic, retain) IBOutlet SEOptionsStoreTwoLabelItem* mAddImageNumberItem;
@property (nonatomic, retain) IBOutlet SEOptionsStoreTwoLabelItem* mAddMusicNumberItem;
@property (nonatomic, retain) IBOutlet SEOptionsLabel* mBrushLabel;

//@property (nonatomic, retain) NSMutableArray* mBrushList;

@property (nonatomic, retain) IBOutlet SEOptionsLabel* mTotalLabel;
@property (nonatomic, retain) IBOutlet UIImageView* mTotalBlank;
@property (nonatomic, retain) IBOutlet SEOptionsLabel* mSupportLabel;
@property (nonatomic, retain) IBOutlet SEOptionsButton* mSubmitButton;
@property (nonatomic, retain) IBOutlet SEOptionsLabel* mTotalPriceLabel;
@end
 */
///////////
@interface SEOptionsAppStoreSetting2 : SEOptionsSettingViewController
{
    NSMutableArray* mStoreItems;
    SEOptionsLabel* mTotalLabel;
    SEOptionsImageView* mTotalBlank;
    SEOptionsLabel* mTotalPriceLabel;
    SEOptionsLabel* mSupportLabel;
    SEOptionsButton* mSubmitButton;
    float mTotalPrice;
    NSMutableArray* mFunctionHandlerArray;
    SEProductManager* mProductManager;
    int mConnectionTime;
    NSTimer* mConnectionTimer;
}
- (void) selectStoreItem: (int) productType;
@end
///////
@interface SEHelpCategoryBlock : NSObject
{
    NSString* categoryName;
    NSMutableArray* details;
}
@property (nonatomic, retain) NSString* categoryName;
- (void) addDetail: (NSString*)detail;
- (NSArray*) getDetails;
- (BOOL) hasDetail: (NSString*)detail;
@end
@interface SEHelpCategory : NSObject
{
    //array of help category block
    NSMutableArray* mCategoryArray;
}
- (NSArray*) getAllCategoryName;
- (NSArray*) getCategoryDetail: (NSString*) categoryName;
- (SEHelpCategoryBlock*) getCategoryBlock: (NSString*)categoryName;
- (void) print;
@end
//////
@class SEOptionsHelpSetting;
@interface SEOptionsHelpCategoryView : UIView
{
    
    SEOptionsHelpSetting* mOptionsHelp;
}
@property (nonatomic, assign) SEOptionsHelpSetting* mOptionsHelp;
- (id) initWithFrame:(CGRect)frame category: (SEHelpCategory*)category;
@end
/////////
@interface SEOptionsHelpButton : UIView
{
    UIButton* mButton;
    UIImageView* mLeftArrow;
    UIImageView* mRightArrow;
    FontLabel* mText;
    id mTarget;
    SEL mLeftArrowHandler;
    SEL mRightArrowHandler;
}
- (void) setBackgroundImage: (NSString*) normal : (NSString*)highlighted;
- (void) setButtonHandler: (id)target : (SEL)action;
- (void) setText: (NSString*)str  textColor: (UIColor*)color;
- (void) setArrowHandlerTarget: (id)target leftArrowAction: (SEL)leftAction rightArrowAction: (SEL)rightAction;
- (id) initWithFrame:(CGRect)frame withArrow: (BOOL) hasArrow;
@end
///////////////
@interface SEOptionsHelpDetailView : UIView <UIScrollViewDelegate>
{
    UIScrollView* mContentScrollView;
    SEOptionsHelpButton* mCloseButton;
    SEOptionsHelpButton* mNextButton;
    FontLabel* mLabel;
    UIPageControl* mPageControl;
    SEOptionsHelpSetting* mOptionsHelp;
    UIImageView* mBackgroundView;
}
@property (nonatomic, assign) SEOptionsHelpSetting* mOptionsHelp;
- (void) setDetails: (NSArray*)details;
@end
//////
@interface SEOptionsHelpSetting : SEOptionsSettingViewController
{
    SEOptionsHelpCategoryView* mCategoryView;
    SEOptionsHelpDetailView* mDetialView;
    SEHelpCategory* mHelpCatgory;
}
- (void) hideCategoryView;
- (void) hideDetailView;
- (void) setDetails: (NSString*)categoryName;
@end
///////
@interface SEOptionsInvalidSetting : SEOptionsSettingViewController

@end
//////////
@interface SEOptionsView : SEFixedView
{
    SEOptionLeftLabel* mLeftViewBars[OPTION_LEFT_BAR_NUM];
    SEOptionsSettingViewController* mRightViewController[OPTION_LEFT_BAR_NUM]; // right views
    SEViewNavigator* mViewNav;
    float mLeftVMargin;
    OPTION_LEFT_BAR_TYPE mCurrentLeftBar;
    UIImage* mRightBackground;
    UIImage* mLeftBackground;
    UIImageView* mLeftView;
    UIScrollView* mRightView;
    UIView* mRightContentParent;
    UIImageView* mRightBackgroundView;
    SEOptionsLockView* mLockView;
}
- (id) initWithFrame:(CGRect)frame byViewNav: (SEViewNavigator*)viewNav;
- (void) initData;
- (void) setCurrentBarView: (OPTION_LEFT_BAR_TYPE) barType;
- (void) updateUserUpgradeInfo;
- (void) setAppStoreItemSelected: (int) productType;
@end
