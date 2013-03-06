//
//  SEUserUpgrate.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-3-22.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "SEViewNavigator.h"
//if you change this enum , please change the define in file userinfometadata.txt
enum TASK_OPERATION_TYPE {USE, DRAW, SHARE, MUSIC, BUY, ALL_OPERATION};
struct SEAchieveData
{
    int expNum;
    int imageListNumDelta;
    int musicListNumDelta;
    int brushNumDelta;
    int timeFontDelta;
};
struct SEAchieveTaskData
{
    int taskPoint;
    int opPoint[ALL_OPERATION];
    /*
    int useNum;
    int drawNum;
    int shareNum;
    int commentNum;
    int musicNum;
     */
};
@class SEUserUpdateViewController;
@interface SEUserData : NSObject 
{
@private
    int level;
    int expNum;
    int imageListNum;
    int musicListNum;
    int brushNum;
    int effect;
    int timeFontNum;
}
@property (nonatomic, assign) int timeFontNum;
@property (nonatomic, assign) int level;
@property (nonatomic, assign) int expNum;
@property (nonatomic, assign) int imageListNum;
@property (nonatomic, assign) int musicListNum;
@property (nonatomic, assign) int brushNum;
@property (nonatomic, assign) int effect;
@end
enum TIME_INTERVAL_UNIT {ONE_DAY, FIVE_MINUTE, ONE_MINUTE, MINUTE_20, ONE_HOUR,INVALID_TIME_INTERVAL};
enum UPGRADE_INFO_TYPE {ACHIEVE_ONLY, LEVEL_UP_ONLY, ACHIEVE_LEVE_UP, NO_UPGRADE};
@interface SEUserUpgrade : NSObject
{
    NSMutableArray* mUserMetaData;
    SEViewNavigator* mViewNav;
    BOOL mLevelUpgrate;
    int mFromLevel;
    int mToLevel;
    BOOL mAchievementUpgrade[USER_MEDAL_COUNT];
    enum TIME_INTERVAL_UNIT mCurrentTimeIntervalUnit;
    int mDrawOneTimeExp;// exp num gotten when draw one image
    int mShareOneTimeExp; //exp num gotten when share one image
    int mCommentOneTimeExp; //exp num gotten when comment one time;
    int mMaxFinishedImageNum;
    int mMaxNewPersonPointNum;
    int mMaxFinishedMusicNum;
    int mMaxShareNum;
    int mMaxCommentNum;
    NSMutableArray* mAchieveDataArray; //when achieve one medal , you can get some data
    NSMutableArray* mAchieveTaskDataArray; // how to achieve one medal;
    NSMutableArray* mAchieveTaskUnitArray;// every task's unit
}
@property (nonatomic, assign) enum TIME_INTERVAL_UNIT mCurrentTimeIntervalUnit;
@property (nonatomic, assign) SEViewNavigator* mViewNav;
- (void) calculatePresentOnDuty;
- (void) calculateNewPerson;
- (void) shareOneImage;
- (void) finishOneImageDrawing;
- (void) finishOneMusicPlay;
- (void) finishOneComment;
- (SEUserData*) getUserData: (int)level;
- (void) getMedalPercent: (int)medalType : (int) medal : (float*)outPercent : (float*)outPercentArray;
- (void) getMedalPointValue: (int)medalType : (int)medal : (int*)currentPointValue : (int*)totalPointValue : (int*)currentPointValueArray : (int*) totalPointValueArray;
//- (void) setupUserUpgradeInfo: (SEUserUpdateViewController*) contorller;
- (float) getCurrentLevelPercent;
- (int) getCurrentMedal: (int) medalType;
- (void) dump;
- (SEUserData*) getMaxUserData;
- (BOOL) canUserUpgrade;
- (enum UPGRADE_INFO_TYPE) getCurrentUpgradeType;
- (SEAchieveData) getAchieveData: (int)achieveType medal:(int)medal;
- (UIImage*) getAchieveMedalImage: (int)achieveType medal: (int)medal;
- (void) setTest;
- (void) addExp : (int)num;
- (void) clearAllData;
- (NSString*) getAllData;
- (NSString*) getAchieveDescription: (int)achieveType medal: (int)medal;
- (int) getImageDeltaNumByAchieve: (int)achieveType medal: (int)medal;
- (int) getMusicDeltaNumByAchieve: (int)achieveType medal: (int)medal;
- (int) getAllAchieveDeltaImageNum;
- (int) getAllAchieveDeltaMusicNum;
// -1 is the invalid level num
- (int) getLevelFromExpNum: (int)expNum;
//return is array of empty or an array with all brushid at current achievement
- (NSArray*) getAllBrushIDByUserCurrentAchieve;
- (NSArray*) getAllBrushIDByUserCurrentLevel;
- (NSArray*) getAllTimeFontByCurrentAchieve;
- (NSArray*) getAllTimeFontByCurrentLevel;
- (NSArray*) getBrushIDBetweenTwoLevel: (int)level1 : (int)level2;
- (NSArray*) getTimeFontBetweenTwoLevel: (int) level1 : (int) level2;
- (void) setToMaxLevel;
+ (NSArray*) getFeedChar;
/////
- (void) testSuite;
@end
