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
@class SEViewNavigator;
@class SEResLoader;
@class SEOptionLeftLabel;
enum OPTION_LEFT_BAR_TYPE {MAIN_SETTING, USER_INFO, ISSUE_REPORT, INVALID_LEFT_BAR_VIEW, OPTION_LEFT_BAR_NUM};
//////////////////
@interface SEOptionsRightViewController : NSObject
{
    SEViewNavigator* mViewNav;
}
@property (nonatomic, assign) SEViewNavigator* mViewNav;
- (void) addViewsToParent: (UIView*) parent;
- (void) removeViewsFromParent;
- (void) createViews: (NSString*) nibName frame: (CGRect) frame;
@end
/////////
@interface SEIssueReportController : SEOptionsRightViewController<SEIssueReportDelegate>
{
    IBOutlet UITextField* title;
    IBOutlet UILabel* titleStatic;
    IBOutlet UILabel* descriptionStatic;
    IBOutlet UITextView* description;
    IBOutlet UIButton* sendButton;
    IBOutlet UILabel* outputLabel;
    NSMutableData* mRecvData;
}
@property (nonatomic, retain) IBOutlet UILabel* outputLabel;
@property (nonatomic, retain) IBOutlet UILabel* titleStatic;
@property (nonatomic, retain) IBOutlet UILabel* descriptionStatic;
@property (nonatomic, retain) IBOutlet UIButton* sendButton;
@property (nonatomic, retain) IBOutlet UITextField* title;
@property (nonatomic, retain) IBOutlet UITextView* description;
- (IBAction)sendAction :(id)sender;
@end
///////
@interface SEOptionsUserUpgradeInfoController : SEOptionsRightViewController
{
@private
    UIScrollView* mUserInfoScrollView;
}
@end
///////
@interface SEOptionsMainSettingController : SEOptionsRightViewController
{
@private
    IBOutlet UILabel* mQualityLabel;
    IBOutlet UILabel* mTimesLabel;
    IBOutlet UIImageView* mSampleView;
    IBOutlet UILabel* mCurrentQuality;
    IBOutlet UILabel* mCurrentTimes;
    IBOutlet UISlider* mQualitySlider;
    IBOutlet UISlider* mTimesSlider;
    IBOutlet UIButton* mSignature;
    IBOutlet UIImageView* mImageBackground;
    IBOutlet UIButton* mChangeConfig;
    IBOutlet UILabel* mDownloadIndicate;
}
@property (nonatomic, retain) IBOutlet UILabel* mDownloadIndicate;
@property (nonatomic, retain) IBOutlet UIButton* mChangeConfig;
@property (nonatomic, retain) IBOutlet UIImageView* mImageBackground;
@property (nonatomic, retain) IBOutlet UILabel* mQualityLabel;
@property (nonatomic, retain) IBOutlet UILabel* mTimesLabel;
@property (nonatomic, retain) IBOutlet UIImageView* mSampleView;
@property (nonatomic, retain) IBOutlet UILabel* mCurrentQuality;
@property (nonatomic, retain) IBOutlet UILabel* mCurrentTimes;
@property (nonatomic, retain) IBOutlet UISlider* mQualitySlider;
@property (nonatomic, retain) IBOutlet UISlider* mTimesSlider;
@property (nonatomic, retain) IBOutlet UIButton* mSignature;
- (IBAction)qualitySliderHandler:(UISlider*)sender;
- (IBAction)timesSliderHandler:(UISlider*)sender;
- (IBAction)signatureButtonHandler:(id)sender;
- (IBAction)changeConfigHandler:(id)sender;
- (void) setSampleImageWithQuality:(int)percent withResLoader:(SEResLoader*)resLoader;

@end
//////////////
//////////
@interface SEOptionsView : SEFixedView
{
    SEOptionLeftLabel* mLeftViewBars[OPTION_LEFT_BAR_NUM];
    SEOptionsRightViewController* mRightViewController[OPTION_LEFT_BAR_NUM]; // right views
    SEViewNavigator* mViewNav;
    SEResLoader* mResLoader;
    float mLeftVMargin;
    OPTION_LEFT_BAR_TYPE mCurrentLeftBar;
    UIImage* mRightBackground;
    UIImage* mLeftBackground;
}
- (id) initWithFrame:(CGRect)frame byViewNav: (SEViewNavigator*)viewNav withResLoader: (SEResLoader*)resLoader;
- (void) initData;
- (void) setCurrentBarView: (OPTION_LEFT_BAR_TYPE) barType;
@end
