//
//  SEOptionsView.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-2-22.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "SEProtocalDefine.h"
@class SEViewNavigator;
@class SEResLoader;
@class SEOptionLeftLabel;
enum OPTION_LEFT_BAR_TYPE {MAIN_SETTING, USER_INFO, INVALID_LEFT_BAR_VIEW, OPTION_LEFT_BAR_NUM};
///////
@interface SEOptionsRightViewCreator : NSObject 
{

}
- (NSArray*) createViews :(UIView*) parent withResLoader: (SEResLoader*) resLoader;
@end
@interface SEOptionsMainSettingCreator : SEOptionsRightViewCreator 
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
    SEResLoader* mResLoader;
    SEViewNavigator* mViewNav;
}
@property (nonatomic, assign) SEViewNavigator* mViewNav;
@property (nonatomic, assign) SEResLoader* mResLoader;
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
- (void) setSampleImageWithQuality:(int)percent withResLoader:(SEResLoader*)resLoader;
@end
//////////////
//////////
@interface SEOptionsView : UIView <SEAdjustContentView>
{
    SEOptionLeftLabel* mLeftViewBars[OPTION_LEFT_BAR_NUM];
    NSArray* mRightViews[OPTION_LEFT_BAR_NUM]; // right views is an arrary of view arrays
    SEViewNavigator* mViewNav;
    SEResLoader* mResLoader;
    NSArray* mRightViewsCreator;
    float mLeftVMargin;
    OPTION_LEFT_BAR_TYPE mCurrentLeftBar;
    UIImage* mBackground;
}
- (id) initWithFrame:(CGRect)frame byViewNav: (SEViewNavigator*)viewNav withResLoader: (SEResLoader*)resLoader;
- (void) initData;
//barStrings is the left bar's string array, we will use them to create left label bar
- (void) initLeftView: (NSArray*)barStrings withRightViewCreator: (NSArray*)rightViewCreator;
- (void) setCurrentLeftBar: (OPTION_LEFT_BAR_TYPE) barType;

@end
