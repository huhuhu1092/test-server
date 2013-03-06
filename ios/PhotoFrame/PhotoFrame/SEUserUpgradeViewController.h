//
//  SEUserUpdateViewController.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-7-7.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>

@class SEUIProgressView;
@class SEViewNavigator;
@class FontLabel;
@interface SELeveUpViewController : NSObject
{
    IBOutlet UIImageView* mLevelBg;
    IBOutlet UIImageView* mLevelIconBg;
    IBOutlet UIImageView* mLevelIcon;
    IBOutlet UIImageView* mFromLevelBg;
    IBOutlet UIImageView* mToLevelBg;
    IBOutlet UIImageView* mArrow;
    IBOutlet UIImageView* mProgressViewBg;
    IBOutlet SEUIProgressView* mProgressView;
    IBOutlet UIScrollView* mScrollView;
    IBOutlet FontLabel* mLevelFromLabel;
    IBOutlet FontLabel* mLevelToLabel;
    SEViewNavigator* mViewNav;
    UIView* mContentParent;
    CGFloat startx;
}
@property (nonatomic, retain) IBOutlet UIImageView* mLevelIconBg;
@property (nonatomic, retain) IBOutlet UIImageView* mLevelIcon;
@property (nonatomic, assign) IBOutlet SEViewNavigator* mViewNav;
@property (nonatomic, retain) IBOutlet UIImageView* mLevelBg;
@property (nonatomic, retain) IBOutlet UIImageView* mFromLevelBg;
@property (nonatomic, retain) IBOutlet UIImageView* mToLevelBg;
@property (nonatomic, retain) IBOutlet UIImageView* mArrow;
@property (nonatomic, retain) IBOutlet UIImageView* mProgressViewBg;
@property (nonatomic, retain) IBOutlet SEUIProgressView* mProgressView;
@property (nonatomic, retain) IBOutlet UIScrollView* mScrollView;
@property (nonatomic, retain) IBOutlet FontLabel* mLevelFromLabel;
@property (nonatomic, retain) IBOutlet FontLabel* mLevelToLabel;
@property (nonatomic, readonly) UIView* mContentParent;
- (void) initView;
- (void) addImageLabel: (NSString*) str type: (int)type color : (UIColor*)textColor : (NSArray*) contents;
- (void) setFromLevel: (int) level;
- (void) setTolevel: (int) level;
- (void) setProgressViewPercent: (double) percent;
- (void) setImageNum: (int) num;
- (void) setMusicNum: (int) num;
- (void) setBrushNum: (int) num : (NSArray*)brushArray;
- (void) setTimeFontNum: (int) num : (NSArray*)timeFontArray;
@end

@interface SEAchieveViewController : NSObject
{
    IBOutlet UIImageView* mAchieveIcon;
    IBOutlet UIImageView* mAchieveIconBg;
    IBOutlet UIImageView* mAchieveBg;
    IBOutlet UIImageView* mAchieveDetailBg;
    IBOutlet UIScrollView* mAchieveDetailScrollView;
    IBOutlet FontLabel* mAchieveDesc;
    SEViewNavigator* mViewNav;
    UIView* mContentParent;
    CGFloat startx;
}
@property (nonatomic, assign) CGFloat startx;
@property (nonatomic, assign) SEViewNavigator* mViewNav;
//@property (nonatomic, retain) IBOutlet UIImageView* mAchieveTitleBg;
@property (nonatomic, retain) IBOutlet FontLabel* mAchieveDesc;
@property (nonatomic, retain) IBOutlet UIImageView* mAchieveIconBg;
@property (nonatomic, retain) IBOutlet UIImageView* mAchieveIcon;
@property (nonatomic, retain) IBOutlet UIImageView* mAchieveBg;
@property (nonatomic, retain) IBOutlet UIImageView* mAchieveDetailBg;
@property (nonatomic, retain) IBOutlet UIScrollView* mAchieveDetailScrollView;
@property (nonatomic, readonly) UIView* mContentParent;
- (void) initView;
- (void) setAchieve:(UIImage*) achiveImage imageNum: (int) imageNum musicNum : (int) musicNum brushNum: (int) brushNum: (int) timeFontNum : (int) expNum;
@end

@interface SEUserUpgradeTestController : NSObject
{
    IBOutlet UITextField* mShareImageNum;
    IBOutlet UITextField* mDrawImageNum;
    IBOutlet UITextField* mPlayMusicNum;
    IBOutlet UITextField* mReviewNum;
    
    IBOutlet UIButton* mShareImageButton;
    IBOutlet UIButton* mDrawImageButton;
    IBOutlet UIButton* mPlayMusicButton;
    IBOutlet UIButton* mReviewButton;
    IBOutlet UITextField* mExpNum;
    IBOutlet UITextView* mContentData;
    
    IBOutlet UITextField* mBrushSeq;
    IBOutlet UITextField* mTimeStyleSeq;
    IBOutlet UILabel* mDownloadLabel;
    SEViewNavigator* mViewNav;
}
@property (nonatomic, retain) IBOutlet UILabel* mDownloadLabel;
@property (nonatomic, retain) IBOutlet UITextField* mBrushSeq;
@property (nonatomic, retain) IBOutlet UITextField* mTimeStyleSeq;
@property (nonatomic, retain) IBOutlet UITextView* mContentData;
@property (nonatomic, retain) IBOutlet UITextField* mExpNum;
@property (nonatomic, retain) IBOutlet UITextField* mShareImageNum;
@property (nonatomic, retain) IBOutlet UITextField* mDrawImageNum;
@property (nonatomic, retain) IBOutlet UITextField* mPlayMusicNum;
@property (nonatomic, retain) IBOutlet UITextField* mReviewNum;

@property (nonatomic, retain) IBOutlet UIButton* mShareImageButton;
@property (nonatomic, retain) IBOutlet UIButton* mDrawImageButton;
@property (nonatomic, retain) IBOutlet UIButton* mPlayMusicButton;
@property (nonatomic, retain) IBOutlet UIButton* mReviewButton; 
- (UIView*) getView;
- (IBAction) shareImageHandler:(id)sender;
- (IBAction) playMusicHandler:(id)sender;
- (IBAction) reviewHandler:(id)sender;
- (IBAction) drawImageHandler:(id)sender;
- (IBAction) addExp:(id)sender;
- (IBAction) clearAllData:(id)sender;
- (IBAction) okButton:(id)sender;
- (IBAction) buyMaxLevel:(id)sender;
- (IBAction) buyBasicSetting:(id)sender;
- (IBAction) buyBrushSetting:(id)sender;
- (IBAction) buyBrush:(id)sender;
- (IBAction) buyTimeStyle:(id)sender;
- (IBAction) shouldRotate:(id)sender;
- (IBAction) batterLevelChangeTest:(id)sender;
- (IBAction) showNotificationTest:(id)sender;
- (IBAction) restoreProduct:(id)sender;
@end

@class SETextImageButton;
@interface SEUserUpgradeViewController : NSObject
{
    SEViewNavigator* mViewNav;
    //CGFloat starty;
}
//@property (nonatomic, assign) int mUpgradeType;
//@property (nonatomic, readonly) SELeveUpViewController* mLevelUpViewController;
//@property (nonatomic, readonly) SEAchieveViewController* mAchieveViewController;
//@property (nonatomic, readonly) UIView* mLevelUpView;
//@property (nonatomic, readonly) UIView* mAchieveView;
//@property (nonatomic, retain) IBOutlet SETextImageButton* mOkButton;
//@property (nonatomic, retain) IBOutlet UIImageView* mTopBg;
//@property (nonatomic, retain) IBOutlet UIImageView* mBottomBg;
//@property (nonatomic, retain) IBOutlet UIScrollView* mContentScrollView;
@property (nonatomic, assign) SEViewNavigator* mViewNav;
//- (void) initView;
- (UIView*) getView;
- (UIView*) getTestPopup;
//- (void) addToParent: (UIView*)parent;
//- (void) update;
//- (void) clearAll;
//- (void) setFromLevel: (int) level;
//- (void) setTolevel: (int) level;
//- (void) setProgressViewPercent: (double) percent;
//- (void) setImageNum: (int) num;
//- (void) setMusicNum: (int) num;
//- (void) setBrushNum: (int) num;
//- (void) setTimeFontNum: (int) num;
//- (void) addAchieve:(UIImage*) achiveImage imageNum: (int) imageNum musicNum : (int) musicNum brushNum: (int) brushNum;
- (IBAction)okButtonHandler:(id)sender;
@end
