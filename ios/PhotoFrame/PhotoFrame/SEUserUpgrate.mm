//
//  SEUserUpgrate.mm
//  PhotoFrame
//
//  Created by 陈勇 on 12-3-22.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEUserUpgrate.h"
#import "UserInfo.h"
#import "SEUserUpgradeViewController.h"
#import "SEUserDefaultManager.h"
#import "SEResDefine.h"
#import "UpgradeInfo.h"
#import "SEConfigFileParser.h"
#import "SEUserDefaultManager.h"
#import "PhotoFrameAppDelegate.h"
#import "SESystemDataManager.h"
#import "SEUtil.h"
////////
#define EXP_DEFINE @"EXP_DEFINE"
#define ACHIEVE_DEFINE @"ACHIEVE_DEFINE"
#define ACHIEVE_TASK @"ACHIEVE_TASK"
#define ACHIEVE_TASK_OPERATION_UNIT @"ACHIEVE_TASK_OPERATION_UNIT"
#define EXP_GET @"EXP_GET"
///////////////////////////////////
/*
static int gPresentOnDutyPoint[3] = {10, 20, 30};
static int gNewPersonPoint[3] = {50, 150, 300};

static int gSharePoint[3] = {100, 400, 1500};
static int gFansPoint[3] = {400, 1110, 3800};
static const int DRAWING_ACCUMU_POINT1 = 500;
static const int DRAWING_ACCUMU_POINT2 = 1000;
static const int DRAWING_ACCUMU_POINT3 = 8500;

static const int FANS_DRAWING_ACCUMU_POINT1 = 100;
static const int FANS_DRAWING_ACCUMU_POINT2 = 500;
static const int FANS_DRAWING_ACCUMU_POINT3 = 2500;

static const int FANS_SHARE_ACCUMU_POINT1 = 10;
static const int FANS_SHARE_ACCUMU_POINT2 = 50;
static const int FANS_SHARE_ACCUMU_POINT3 = 200;

static const int FANS_MUSIC_ACCUMU_POINT1 = 20;
static const int FANS_MUSIC_ACCUMU_POINT2 = 100;
static const int FANS_MUSIC_ACCUMU_POINT3 = 250;

static const int FANS_COMMENT_POINT1 = 1;

static const int EXP_POINT_DRAW_ONE_IMAGE = 20;
static const int EXP_POINT_SHARE_ONE_TIME = 50;
static const int EXP_POINT_COMMENT_ONE_TIME = 5000;

struct FansPointData
{
    int drawingPoint;
    int sharePoint;
    int musicPoint;
    int commentPoint;
};
FansPointData gFansData[] = {
    {FANS_DRAWING_ACCUMU_POINT1, FANS_SHARE_ACCUMU_POINT1, FANS_MUSIC_ACCUMU_POINT1, FANS_COMMENT_POINT1},
    {FANS_DRAWING_ACCUMU_POINT2, FANS_SHARE_ACCUMU_POINT2, FANS_MUSIC_ACCUMU_POINT2, FANS_COMMENT_POINT1},
    {FANS_DRAWING_ACCUMU_POINT3, FANS_SHARE_ACCUMU_POINT3, FANS_MUSIC_ACCUMU_POINT3, FANS_COMMENT_POINT1}
};
static int getFansMedal(int drawingPoint, int sharePoint, int musicPoint, int commentPoint)
{
    int count = sizeof(gFansData) / sizeof(FansPointData);
    for(int i = 0 ; i < count ; i++)
    {
        if(gFansData[i].drawingPoint == drawingPoint && 
           gFansData[i].sharePoint == sharePoint * 10 &&
           gFansData[i].musicPoint == musicPoint * 5 &&
           gFansData[i].commentPoint == commentPoint * 100)
            return i;
    }
    return INVALID_MEDAL_LEVEL;
}
////////////////////////////////

SEAchieveData gAchieveAllData[] = {
    {1000, 10, 2, 0, 0},
    {2000, 10, 2, 0, 0},
    {3000, 10, 2, 6, 7},
    {1000, 10, 2, 0, 0},
    {2000, 10, 2, 0, 0},
    {3000, 10, 2, 7, 0},
    {1000, 10, 2, 0, 0},
    {2000, 10, 2, 0, 0},
    {3000, 10, 2, 8, 0},
    {1000, 10, 2, 0, 0},
    {2000, 10, 2, 0, 0},
    {3000, 10, 2, 9, 0},
    {1000, 10, 2, 0, 0},
    {2000, 10, 2, 0, 0},
    {3000, 10, 2, 10, 0}
};
*/
NSString* gAchieveDescriptionString[] = 
{
    @"invalid present on duty medal",
    @"YOU GOT PERFECT ATTENDANCE BRONZE AWARD",
    @"YOU GOT PERFECT ATTENDANCE SILVER AWARD",
    @"YOU GOT PERFECT ATTENDANCE GOLD AWARD",
    
    @"invalid new person medal",
    @"YOU GOT ROOKIE BRONZE AWARD",
    @"YOU GOT ROOKIE SILVER AWARD",
    @"YOU GOT ROOKIE GOLD AWARD",
    
    @"invalid drawing medal",
    @"YOU GOT PAINTING BRONZE AWARD",
    @"YOU GOT PAINTING SILVER AWARD",
    @"YOU GOT PAINTING GOLD AWARD",
    
    @"invalid share medal",
    @"YOU GOT HAPPY SHARING BRONZE AWARD",
    @"YOU GOT HAPPY SHARING SILVER AWARD",
    @"YOU GOT HAPPY SHARING GOLD AWARD",
    
    @"invalid fans medal",
    @"YOU GOT LOYAL USER BRONZE AWARD",
    @"YOU GOT LOYAL USER SILVER AWARD",
    @"YOU GOT LOYAL USER GOLD AWARD",
};
/////////////////////////////

@implementation SEUserData
@synthesize level;
@synthesize expNum;
@synthesize imageListNum;
@synthesize musicListNum;
@synthesize brushNum;
@synthesize effect;
@synthesize timeFontNum;
@end
////
@interface SEUserUpgrade (Private)
- (int) maxExpPointNum;
- (void) addExpPoint: (int) point;
- (void) upgradeLevel : (int) oldExpNum : (int)currentExpNum;
- (int) getNextLevelIndex: (int) currentExpNum;
- (int) getAchieveLevelByTaskData: (int) medalType : (SEAchieveTaskData)taskData;
- (SEAchieveTaskData) getAchieveTaskData: (int)medalType : (int)medalLevel;
- (void) initTaskData: (SEAchieveTaskData*)td;
@end

//////////////////////////////////////
@implementation SEUserUpgrade (Private)
- (void) initTaskData: (SEAchieveTaskData*)td
{
    td->opPoint[USE] = 0;
    td->opPoint[DRAW] = 0;
    td->opPoint[MUSIC] = 0;
    td->opPoint[SHARE] = 0;
    td->opPoint[BUY] = 0;
}
- (SEAchieveTaskData) getAchieveTaskData: (int)medalType : (int)medalLevel
{
    int index= medalType * MEDAL_LEVEL_COUNT + medalLevel;
    NSValue* v = [mAchieveTaskDataArray objectAtIndex:index];
    SEAchieveTaskData td;
    [v getValue:&td];
    return td;
}
- (int) getAchieveLevelByTaskData: (int) medalType : (SEAchieveTaskData)taskData
{
    int startIndex = medalType * MEDAL_LEVEL_COUNT;
    int retIndex = INVALID_MEDAL_LEVEL;
    for(int i = startIndex + 3; i >= startIndex ; i--)
    {
        NSValue* v = [mAchieveTaskDataArray objectAtIndex:i];
        SEAchieveTaskData currentTask;
        [v getValue:&currentTask];
        BOOL ok = YES;
        for(int op = USE ; op < ALL_OPERATION; op++)
        {
            int opValue = currentTask.opPoint[op];
            int inValue = taskData.opPoint[op];
            if(inValue < opValue)
            {
                ok = NO;
                break;
            }
        }
        if(ok)
        {
            retIndex = i - startIndex;
            break;
        }
    }
    return retIndex;
}
- (int) getNextLevelIndex: (int) currentExpNum
{
    int currentLevelIndex = -1;
    for(int i = mUserMetaData.count - 1 ; i >= 0; i--)
    {
        SEUserData* userData = [mUserMetaData objectAtIndex:i];
        if(currentExpNum >= userData.expNum)
        {
            currentLevelIndex = i;
            break;
        }
    }
    assert(currentLevelIndex != -1);
    if(currentLevelIndex == mUserMetaData.count - 1)
    {
        return currentLevelIndex;
    }
    else 
    {
        return currentLevelIndex + 1;
    }
}
- (void) insertLevelUpgradeInfoToCoreData
{
    [mViewNav addUpgradeInfoToCoreData:mFromLevel toLevel:mToLevel achievementType:INVALID_ACHIEVE medal:INVALID_MEDAL_LEVEL];        
}
- (BOOL) isLevelInUpgradeInfoCoreData: (int)fromLevel :(int)toLevel
{
    NSArray* upgradeInfoArray = [mViewNav getUpgradeInfoArray];
    for(int i = 0 ; i < upgradeInfoArray.count ; i++)
    {
        UpgradeInfo* upgradeInfo = [upgradeInfoArray objectAtIndex:i];
        if([[upgradeInfo fromlevel] intValue] == fromLevel && [[upgradeInfo tolevel] intValue] == toLevel)
        {
            return YES;
        }
    }
    return NO;
}

- (void) upgradeLevel: (int) oldExpNum : (int)currentExpNum
{
    int oldLevel = [self getLevelFromExpNum:oldExpNum];
    int currentLevel = [self getLevelFromExpNum:currentExpNum];
    if(oldLevel == -1 || currentLevel == -1)
    {
        NSLog(@"can not find the level\n");
        assert(0);
        return;
    }
    if(oldLevel == currentLevel)
        return;
    mFromLevel = oldLevel;
    mToLevel = currentLevel;
    if([self isLevelInUpgradeInfoCoreData:mFromLevel :mToLevel] == NO)
    {
        mLevelUpgrate = YES;
        [self insertLevelUpgradeInfoToCoreData];
        [mViewNav notificationShow];
    }
}

- (void) addExpPoint: (int) point
{
    //UserInfo* userInfo = [mViewNav getUserInfo];
    int count = [mViewNav.mSystemDataManager.exppointnum intValue];
    if(count < [self maxExpPointNum])
    {
        int oldExpNum = [mViewNav.mSystemDataManager.exppointnum intValue];
        mViewNav.mSystemDataManager.exppointnum = [NSNumber numberWithInt:(oldExpNum + point)];
        [self upgradeLevel : oldExpNum: [mViewNav.mSystemDataManager.exppointnum intValue]];
    }
}


- (int) maxExpPointNum
{
    SEUserData* userData = [mUserMetaData lastObject];
    return userData.expNum;
}
@end
@implementation SEUserUpgrade
@synthesize mCurrentTimeIntervalUnit;
@synthesize mViewNav;
- (void) insertAchieveToCoreData: (int)achieveType medal: (int)medal
{
    [mViewNav addUpgradeInfoToCoreData:INVALID_LEVEL toLevel:INVALID_LEVEL achievementType:achieveType medal:medal];
}
- (enum UPGRADE_INFO_TYPE) getCurrentUpgradeType
{
    BOOL achieveUpgrade = NO;
    for(int i = 0 ; i < USER_MEDAL_COUNT ; i++)
    {
        if(mAchievementUpgrade[i] == YES)
        {
            achieveUpgrade = YES;
            break;
        }
    }
    if(mLevelUpgrate == YES && achieveUpgrade == YES)
        return ACHIEVE_LEVE_UP;
    else if(mLevelUpgrate == YES && achieveUpgrade == NO)
        return LEVEL_UP_ONLY;
    else if(mLevelUpgrate == NO && achieveUpgrade == YES)
        return ACHIEVE_ONLY;
    else 
    {
        return NO_UPGRADE;
    }
}
- (void) testTimeInterval
{
    NSCalendar* gregorian = [[[NSCalendar alloc] initWithCalendarIdentifier:NSGregorianCalendar] autorelease];
    NSDateComponents* dateComponents = [[[NSDateComponents alloc] init] autorelease];
    [dateComponents setYear:2012];
    [dateComponents setMonth:10];
    [dateComponents setDay:20];
    [dateComponents setHour:10];
    [dateComponents setMinute:20];
    
    NSDate* firstDate = [gregorian dateFromComponents:dateComponents];
    
    [dateComponents setYear:2012];
    [dateComponents setMonth:10];
    [dateComponents setDay:20];
    [dateComponents setHour:20];
    [dateComponents setHour:20];
    
    NSDate* secondDate = [gregorian dateFromComponents:dateComponents];
    NSUInteger unitFlag = NSDayCalendarUnit;
    NSUInteger unitFlag2 = NSHourCalendarUnit;
    NSDateComponents* intervalDay = [gregorian components:unitFlag fromDate:firstDate toDate:secondDate options:0];
    NSUInteger n = [intervalDay day];
    NSLog(@"inverval day = %d", n);
    
    NSDateComponents* intervalHour = [gregorian components:unitFlag2 fromDate:firstDate toDate:secondDate options:0];
    n = [intervalHour hour];
    NSLog(@"interval hour = %d", n);
    
    NSDateComponents* intervalMinute = [gregorian components:NSMinuteCalendarUnit fromDate:firstDate toDate:secondDate options:0];
    n = [intervalMinute minute];
    NSLog(@"interval minute = %d", n);
}
- (void) configFileHandler: (NSArray*) data
{
    NSString* category = [data objectAtIndex:0];
    NSArray* tokens = [data objectAtIndex:1];
    if([category isEqualToString:EXP_DEFINE])
    {
        assert(tokens.count == 8);
        SEUserData* userData = [[SEUserData alloc] init];
        userData.level = [[tokens objectAtIndex:1] intValue];
        userData.expNum = [[tokens objectAtIndex: 2] intValue];
        userData.imageListNum = [[tokens objectAtIndex: 3] intValue];
        userData.brushNum = [[tokens objectAtIndex: 5]  intValue];
        userData.musicListNum = [[tokens objectAtIndex: 6]  intValue];
        NSString* fontEffect = [tokens objectAtIndex: 7];
        userData.timeFontNum = [fontEffect intValue];
        [mUserMetaData addObject:userData];
        [userData release];
            SEUserData* ud = userData;
            NSLog(@"meta user data level = %d, expNum = %d", ud.level, ud.expNum);
            NSLog(@"image list num = %d, music list num = %d, brushNum = %d", ud.imageListNum, ud.musicListNum, ud.brushNum);
        NSLog(@"exp data load end");
    }
    else if([category isEqualToString:ACHIEVE_DEFINE])
    {
        assert(tokens.count == 5);
        SEAchieveData ad;
        ad.expNum = [[tokens objectAtIndex:0] intValue];
        ad.imageListNumDelta = [[tokens objectAtIndex:1] intValue];
        ad.brushNumDelta = [[tokens objectAtIndex:2] intValue];
        ad.musicListNumDelta = [[tokens objectAtIndex:3] intValue];
        ad.timeFontDelta = [[tokens objectAtIndex:4] intValue];
        NSValue* v = [NSValue valueWithBytes:&ad objCType:@encode(SEAchieveData)];
        [mAchieveDataArray addObject:v];
        NSLog(@"achieve data expNum = %d, imageListNumData = %d, brushNum = %d, musicListNum = %d, timeFontDelta = %d", ad.expNum, ad.imageListNumDelta, ad.brushNumDelta, ad.musicListNumDelta, ad.timeFontDelta);
    }
    else if([category isEqualToString: ACHIEVE_TASK])
    {
        assert(tokens.count == 6);
        SEAchieveTaskData td;
        td.taskPoint = [[tokens objectAtIndex:0] intValue];
        td.opPoint[USE] = [[tokens objectAtIndex:1] intValue];
        td.opPoint[DRAW] = [[tokens objectAtIndex:2] intValue];
        td.opPoint[SHARE] = [[tokens objectAtIndex:3] intValue];
        td.opPoint[MUSIC]= [[tokens objectAtIndex:4] intValue];
        td.opPoint[BUY] = [[tokens objectAtIndex:5] intValue];
        NSValue* v = [NSValue valueWithBytes:&td objCType:@encode(SEAchieveTaskData)];
        [mAchieveTaskDataArray addObject:v];
        NSLog(@"achieve task USE = %d, DRAW = %d, SHARE = %d, MUSIC = %d, COMMENT = %d", td.opPoint[USE], td.opPoint[DRAW], td.opPoint[SHARE], td.opPoint[MUSIC], td.opPoint[BUY]);
    }
    else if([category isEqualToString:ACHIEVE_TASK_OPERATION_UNIT])
    {
        assert(tokens.count == 5);
        for(int i = 0 ; i < ALL_OPERATION ; i++)
        {
            [mAchieveTaskUnitArray addObject:[NSNumber numberWithInt: [[tokens objectAtIndex:i] intValue]]];
            NSLog(@"unit = %@", [tokens objectAtIndex:i]);
        }
    }
    else if([category isEqualToString:EXP_GET])
    {
        mDrawOneTimeExp = [[tokens objectAtIndex:0] intValue];
        mShareOneTimeExp = [[tokens objectAtIndex:1] intValue];
        mCommentOneTimeExp = [[tokens objectAtIndex:2] intValue];
        NSLog(@"draw one time exp = %d ", mDrawOneTimeExp);
        NSLog(@"share one time exp = %d", mShareOneTimeExp);
        NSLog(@"comment one time exp = %d", mCommentOneTimeExp);
    }
}
- (id)init
{
    self = [super init];
    if (self) 
    {
        mAchieveDataArray = [[NSMutableArray array] retain];
        mAchieveTaskDataArray = [[NSMutableArray array] retain];
        mAchieveTaskUnitArray = [[NSMutableArray array] retain];
        mUserMetaData = [[NSMutableArray array] retain];
        SEConfigFileParser* parser = [[SEConfigFileParser alloc] init];
        NSArray* configArray = [NSArray arrayWithObjects:EXP_DEFINE, ACHIEVE_DEFINE, ACHIEVE_TASK, ACHIEVE_TASK_OPERATION_UNIT, EXP_GET,nil];
        [parser setBlockDefine:configArray];
        [parser setTarget:self action:@selector(configFileHandler:)];
        [parser parse: @"userinfometadata.txt"];
        [parser release];
        /*
        int* expnum = (int*)malloc(sizeof(int) * mUserMetaData.count);
        for(int i = 0 ; i < mUserMetaData.count ; i++)
        {
            expnum[i] = 0;
        }
        for(int i = 1 ; i < mUserMetaData.count ; i++)
        {
            int expNum = 0;
            for(int j = 0 ; j <= i ; j++)
            {
                SEUserData* ud = [mUserMetaData objectAtIndex:j];
                expNum += ud.expNum;
            }
            //SEUserData* ud = [mUserMetaData objectAtIndex:i];
            //ud.expNum = expNum;
            expnum[i] = expNum;
            NSLog(@"level %d expnum = %d", i, expnum[i]);
        }
        for(int i = 0 ; i < mUserMetaData.count ; i++)
        {
            SEUserData* ud =  [mUserMetaData objectAtIndex:i];
            ud.expNum = expnum[i];
        }
        free(expnum);
         */
        mCurrentTimeIntervalUnit = ONE_DAY;
        mMaxFinishedImageNum = 20000;
        mMaxNewPersonPointNum = 20000;
        mMaxFinishedMusicNum = 20000;
        mMaxShareNum = 20000;
        mMaxCommentNum = 20000;
    }
    return self;
}
- (void)dealloc
{
    [mUserMetaData release];
    [mAchieveDataArray release];
    [mAchieveTaskDataArray release];
    [mAchieveTaskUnitArray release];
    [super dealloc];
}
- (BOOL) canUserUpgrade
{
    BOOL ok = [SEUserDefaultManager isFunctionOK:USERINFO_FUNC];
    return ok;
}
- (int) upgradeUserMedalLevel: (UserInfo*)userInfo : (int) currentMedalLevel
{
    int newMedalLevel = currentMedalLevel + 1;
    if(newMedalLevel > GOLD_MEDAL)
        newMedalLevel = GOLD_MEDAL;
    return newMedalLevel;
}
- (void)upgradePresentOnDuty : (UserInfo*)userInfo
{
    if([self canUserUpgrade] == NO)
        return;
    int currentMedal = [mViewNav.mSystemDataManager.presentondutymedal intValue];
    SEAchieveTaskData td;
    [self initTaskData: (&td)];
    td.opPoint[USE] = [mViewNav.mSystemDataManager.presentondutypoint intValue];
    int newMedal = [self getAchieveLevelByTaskData:PRESENTONDUTY_MEDAL :td];
    BOOL upgradeOK = NO;
    if(currentMedal < newMedal)
    {
        mViewNav.mSystemDataManager.presentondutymedal = [NSNumber numberWithInt:newMedal];
        int expNum = 0;
        for(int i = currentMedal + 1 ; i <= newMedal ; i++)
        {
            SEAchieveData ad = [self getAchieveData:PRESENTONDUTY_MEDAL medal:i];
            expNum += ad.expNum;
        }
        [self addExpPoint:expNum];
        upgradeOK = YES;
    }
    if(upgradeOK)
    {
        mAchievementUpgrade[PRESENTONDUTY_MEDAL] = YES;
        [self insertAchieveToCoreData: PRESENTONDUTY_MEDAL medal:[mViewNav.mSystemDataManager.presentondutymedal intValue]];
        [mViewNav notificationShow];
    }
}
- (void) addOneForPresentOnDutyPoint: (UserInfo*)userInfo
{
    SEAchieveTaskData td = [self getAchieveTaskData:PRESENTONDUTY_MEDAL :GOLD_MEDAL];
    int currentPoint = [mViewNav.mSystemDataManager.presentondutypoint intValue];
    if(currentPoint < td.opPoint[USE])
    {
        mViewNav.mSystemDataManager.presentondutypoint = [NSNumber numberWithInt: currentPoint + 1];
    }
}


- (NSDate*) getDateByUnitType: (NSDate*) date unitType: (TIME_INTERVAL_UNIT)unitType
{
    NSCalendar *gregorian = [[[NSCalendar alloc]
                              initWithCalendarIdentifier:NSGregorianCalendar] autorelease];
    NSUInteger unitFlags = 0;
    switch (unitType) {
        case ONE_DAY:
        {
            unitFlags = NSDayCalendarUnit | NSMonthCalendarUnit | NSYearCalendarUnit;
            NSDateComponents* currentComponent = [gregorian components:unitFlags fromDate:date];
            NSInteger currentDay = [currentComponent day];
            NSInteger currentYear = [currentComponent year];
            NSInteger currentMonth = [currentComponent month];
            
            currentComponent = [[[NSDateComponents alloc] init] autorelease];
            [currentComponent setYear:currentYear];
            [currentComponent setMonth:currentMonth];
            [currentComponent setDay:currentDay];
            NSDate* newCurrentDate = [gregorian dateFromComponents:currentComponent];
            return newCurrentDate;
        }
            break;
        case ONE_HOUR:
        {
            unitFlags = NSHourCalendarUnit | NSDayCalendarUnit | NSMonthCalendarUnit | NSYearCalendarUnit;
            NSDateComponents* currentComponent = [gregorian components:unitFlags fromDate:date];
            NSInteger currentDay = [currentComponent day];
            NSInteger currentYear = [currentComponent year];
            NSInteger currentMonth = [currentComponent month];
            NSInteger currentHour = [currentComponent hour];
            
            currentComponent = [[[NSDateComponents alloc] init] autorelease];
            [currentComponent setYear:currentYear];
            [currentComponent setMonth:currentMonth];
            [currentComponent setDay:currentDay];
            [currentComponent setHour:currentHour];
            NSDate* newCurrentDate = [gregorian dateFromComponents:currentComponent];
            return newCurrentDate;

        }
            break;
        case FIVE_MINUTE:
        case ONE_MINUTE:
        case MINUTE_20:
        {
            unitFlags = NSMinuteCalendarUnit | NSHourCalendarUnit | NSDayCalendarUnit | NSMonthCalendarUnit | NSYearCalendarUnit;
            NSDateComponents* currentComponent = [gregorian components:unitFlags fromDate:date];
            NSInteger currentDay = [currentComponent day];
            NSInteger currentYear = [currentComponent year];
            NSInteger currentMonth = [currentComponent month];
            NSInteger currentHour = [currentComponent hour];
            NSInteger currentMinute = [currentComponent minute];
            
            currentComponent = [[[NSDateComponents alloc] init] autorelease];
            [currentComponent setYear:currentYear];
            [currentComponent setMonth:currentMonth];
            [currentComponent setDay:currentDay];
            [currentComponent setHour:currentHour];
            [currentComponent setMinute:currentMinute];
            NSDate* newCurrentDate = [gregorian dateFromComponents:currentComponent];
            return newCurrentDate;
        }
            break;
        default:
            break;
    }
    return nil;
}
- (int) getTimeIntervalUnit: (NSDate*)currentDate lastDate: (NSDate*)lastDate unit: (TIME_INTERVAL_UNIT)unitType;
{
    assert(currentDate != nil && lastDate != nil && unitType != INVALID_TIME_INTERVAL);
    //NSDate* newCurrentDate = [self getDateByUnitType:currentDate unitType:unitType];
    //NSDate* newLastDate = [self getDateByUnitType:lastDate unitType:unitType];
    //NSTimeInterval timeDiff = [newCurrentDate timeIntervalSinceDate:newLastDate];
    NSCalendar *gregorian = [[[NSCalendar alloc]
                             initWithCalendarIdentifier:NSGregorianCalendar] autorelease];
    //int unit = 0;
    int unitDiff = 0;
    switch (unitType) {
        case ONE_DAY:
        {
            NSUInteger unitFlags = NSDayCalendarUnit;
            NSDateComponents* dateComponents = [gregorian components:unitFlags fromDate:lastDate toDate:currentDate options:0];
            unitDiff = [dateComponents day];
            NSLog(@"day = %d", unitDiff);
        }
            break;
        case ONE_HOUR:
            //unit = 60 * 60;
        {
            NSUInteger unitFlags = NSHourCalendarUnit;
            NSDateComponents* dateComponents = [gregorian components:unitFlags fromDate:lastDate toDate:currentDate options:0];
            unitDiff = [dateComponents hour];
            NSLog(@"hour = %d", unitDiff);
        }
            break;
        case FIVE_MINUTE:
        {
            NSUInteger unitFlags = NSMinuteCalendarUnit;
            NSDateComponents* dateComponents = [gregorian components:unitFlags fromDate:lastDate toDate:currentDate options:0];
            unitDiff = [dateComponents minute];
            NSLog(@"minute = %d", unitDiff);
            unitDiff = unitDiff / 5;
        }
            break;
        case ONE_MINUTE:
        {
            NSUInteger unitFlags = NSMinuteCalendarUnit;
            NSDateComponents* dateComponents = [gregorian components:unitFlags fromDate:lastDate toDate:currentDate options:0];
            unitDiff = [dateComponents minute];
            NSLog(@"minute = %d", unitDiff);
        }
            break;
        case MINUTE_20:
        {
            NSUInteger unitFlags = NSMinuteCalendarUnit;
            NSDateComponents* dateComponents = [gregorian components:unitFlags fromDate:lastDate toDate:currentDate options:0];
            unitDiff = [dateComponents minute];
            NSLog(@"minute = %d", unitDiff);
            unitDiff /= 20;
        }
            break;
        default:
            break;
    }
    //assert(unit != 0 && newCurrentDate != nil && newLastDate != nil);
    //int unitDiff = (int)(timeDiff / unit);
    NSLog(@"unitDiff = %d", unitDiff);
    return unitDiff;
}
- (BOOL) isAppropriateForNewPersonTime: (NSDate*)currentDate lastDate: (NSDate*)lastDate
{
    TIME_INTERVAL_UNIT unitType = mCurrentTimeIntervalUnit;
    int timeIntervalUnit = [self getTimeIntervalUnit:currentDate lastDate:lastDate unit:unitType];
    return timeIntervalUnit == 0;
}
- (BOOL) isAppropriatePresentOnDutyTime: (NSDate*)currentDate lastDate: (NSDate*) lastDate
{

    TIME_INTERVAL_UNIT unitType = mCurrentTimeIntervalUnit;
    int timeIntervalUnit = [self getTimeIntervalUnit:currentDate lastDate:lastDate unit:unitType];
    return timeIntervalUnit == 1;
}
- (BOOL) isExeceedPresendOnDutyTime: (NSDate*)currentDate lastDate: (NSDate*)lastDate
{
    TIME_INTERVAL_UNIT unitType = mCurrentTimeIntervalUnit;
    int timeIntervalUnit = [self getTimeIntervalUnit:currentDate lastDate:lastDate unit:unitType];
    return timeIntervalUnit > 1;
}
- (int) presendOnDutyTimeToTimes : (NSDate*)currentDate lastDate: (NSDate*)lastDate
{
    TIME_INTERVAL_UNIT unitType = mCurrentTimeIntervalUnit;
    int timeIntervalUnit = [self getTimeIntervalUnit:currentDate lastDate:lastDate unit:unitType];
    return timeIntervalUnit - 1;
}
- (void) calculatePresentOnDuty
{
    if([self canUserUpgrade] == NO)
        return;
    UserInfo* userInfo = [mViewNav getUserInfo];
    if(userInfo.applaststartdate == nil)
    {
        userInfo.applaststartdate = [NSDate date];
    }
    else
    {
        NSDate* currentDate = [NSDate date];
        NSDate* lastDate = userInfo.applaststartdate;
        if([self isAppropriatePresentOnDutyTime:currentDate lastDate:lastDate])
        {
            [self addOneForPresentOnDutyPoint:userInfo];
            [self upgradePresentOnDuty: userInfo];
            userInfo.applaststartdate = currentDate;
        }
        else if([self isExeceedPresendOnDutyTime:currentDate lastDate:lastDate])
        {
            int times = [self presendOnDutyTimeToTimes:currentDate lastDate:lastDate];
            for(int i = 0 ; i < times ; i++)
            {
                mViewNav.mSystemDataManager.presentondutypoint = [NSNumber numberWithInt:[mViewNav.mSystemDataManager.presentondutypoint intValue] - 1];
                if([mViewNav.mSystemDataManager.presentondutypoint intValue] < 0)
                {
                    mViewNav.mSystemDataManager.presentondutypoint = [NSNumber numberWithInt:0];
                }
            }
            userInfo.applaststartdate = currentDate;
        }
        
    }
}

- (int) getSharePoint:(UserInfo*)userInfo
{
    return [mViewNav.mSystemDataManager.sharepoint intValue];
    /*
    if([userInfo.sharemedal intValue] == INVALID_MEDAL_LEVEL)
        point = [userInfo.sharepoint intValue];
    else
    {
        for(int i = COPPER_MEDAL ; i <= GOLD_MEDAL ; i++)
        {
            point += gSharePoint[i];
        }
        point += [userInfo.sharepoint intValue];
    }
    return point;
     */
}
- (void) calculateFansPoint
{
    if([self canUserUpgrade] == NO)
        return;
    UserInfo* userInfo = [mViewNav getUserInfo];
    SEProductManager* productManager = [PhotoFrameAppDelegate getProductManager];
    int drawPoint = [mViewNav.mSystemDataManager.finishedimagenum intValue];
    int sharePoint = [self getSharePoint:userInfo];
    int musicPoint = [mViewNav.mSystemDataManager.finishedmusicnum intValue];
    int buyPoint = [productManager getUserBuiedProductNum];
    SEAchieveTaskData taskdata;
    taskdata.opPoint[USE] = 0;
    taskdata.opPoint[DRAW] = drawPoint;
    taskdata.opPoint[MUSIC] = musicPoint;
    taskdata.opPoint[SHARE] = sharePoint;
    taskdata.opPoint[BUY] = buyPoint;
    int currentFansMedal = [mViewNav.mSystemDataManager.fansmedal intValue];
    int medal = [self getAchieveLevelByTaskData:FANS_MEDAL :taskdata];
    if(currentFansMedal < medal)
    {
        mViewNav.mSystemDataManager.fansmedal = [NSNumber numberWithInt: medal];
        int expNum = 0;
        for(int i = currentFansMedal + 1 ; i <= medal ; i++)
        {
            SEAchieveData ad = [self getAchieveData:FANS_MEDAL medal:i];
            expNum += ad.expNum;
        }
        [self addExpPoint:expNum];
        mAchievementUpgrade[FANS_MEDAL] = YES;
        [mViewNav notificationShow];
        [self insertAchieveToCoreData:FANS_MEDAL medal:medal];
        [mViewNav saveCoreDataContext];
    }
    
}
- (void) addOneForSharePoint: (UserInfo*)userInfo
{
    int currentSharePoint = [mViewNav.mSystemDataManager.sharepoint intValue];
    if(currentSharePoint < mMaxShareNum)
    {
        mViewNav.mSystemDataManager.sharepoint = [NSNumber numberWithInt:currentSharePoint + 1];
    }
}
- (void) upgradeShareMedal: (UserInfo*)userInfo
{
    if([self canUserUpgrade] == NO)
        return;
    BOOL upgradeOK = NO;
    int currentMedal = [mViewNav.mSystemDataManager.sharemedal intValue];
    SEAchieveTaskData td;
    [self initTaskData: (&td)];
    td.opPoint[SHARE] = [mViewNav.mSystemDataManager.sharepoint intValue];
    int newMedal = [self getAchieveLevelByTaskData:SHARE_MEDAL :td];
    if(currentMedal < newMedal)
    {
        mViewNav.mSystemDataManager.sharemedal = [NSNumber numberWithInt:newMedal];
        int expNum = 0;
        for(int i = currentMedal + 1 ; i <= newMedal ; i++)
        {
            SEAchieveData ad = [self getAchieveData:SHARE_MEDAL medal:i];
            expNum += ad.expNum;
        }
        [self addExpPoint:expNum];
        upgradeOK = YES;
    }
    if(upgradeOK)
    {
        [self insertAchieveToCoreData:SHARE_MEDAL medal:[mViewNav.mSystemDataManager.sharemedal intValue]];
        mAchievementUpgrade[SHARE_MEDAL] = YES;
        [mViewNav notificationShow];
    }
    
}
- (void) shareOneImage
{
    if([self canUserUpgrade] == NO)
        return;
    UserInfo* userInfo = [mViewNav getUserInfo];
    [self addOneForSharePoint:userInfo];
    [self upgradeShareMedal: userInfo];
    [self calculateFansPoint];
    [self addExpPoint:mShareOneTimeExp];
}
- (void) upgradeNewPersonMedal : (UserInfo*)userInfo;
{
    if([self canUserUpgrade] == NO)
        return;
    BOOL upgradeOK = NO;
    int currentMedal = [mViewNav.mSystemDataManager.newpersonmedal intValue];
    SEAchieveTaskData td;
    [self initTaskData: (&td)];
    td.opPoint[DRAW] = [mViewNav.mSystemDataManager.newpersonpoint intValue];
    int newMedal = [self getAchieveLevelByTaskData:NEWPERSON_MEDAL :td];
    if(currentMedal < newMedal)
    {
        mViewNav.mSystemDataManager.newpersonmedal = [NSNumber numberWithInt:newMedal];
        int expNum = 0;
        for(int i = currentMedal + 1 ; i <= newMedal ; i++)
        {
            SEAchieveData ad = [self getAchieveData:NEWPERSON_MEDAL medal:i];
            expNum += ad.expNum;
        }
        [self addExpPoint:expNum];
        upgradeOK = YES;
    }
    if(upgradeOK)
    {
        mAchievementUpgrade[NEWPERSON_MEDAL] = YES;
        [self insertAchieveToCoreData:NEWPERSON_MEDAL medal:[mViewNav.mSystemDataManager.newpersonmedal intValue]];
        [mViewNav notificationShow];
    }
}
-(void) addOneToFinishedImage:(UserInfo*)userInfo
{
    int count = [mViewNav.mSystemDataManager.finishedimagenum intValue];
    if(count < mMaxFinishedImageNum)
    {
        mViewNav.mSystemDataManager.finishedimagenum = [NSNumber numberWithInt:[mViewNav.mSystemDataManager.finishedimagenum intValue] + 1];
    }
    
}
- (void) addOneToNewPersonPoint: (UserInfo*)userInfo
{
    int currentNewPersonPoint = [mViewNav.mSystemDataManager.newpersonpoint intValue];
    if(currentNewPersonPoint < mMaxNewPersonPointNum)
    {
        mViewNav.mSystemDataManager.newpersonpoint = [NSNumber numberWithInt:currentNewPersonPoint + 1];
    }
}
- (void) calculateDrawingAchieve
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    BOOL upgradeOK = NO;
    int currentMedal = [mViewNav.mSystemDataManager.drawingmedal intValue];
    SEAchieveTaskData td;
    [self initTaskData:&td];
    td.opPoint[DRAW] = [mViewNav.mSystemDataManager.finishedimagenum intValue];
    int newMedal = [self getAchieveLevelByTaskData:DRAWING_MEDAL :td];
    if(currentMedal < newMedal)
    {
        mViewNav.mSystemDataManager.drawingmedal = [NSNumber numberWithInt:newMedal];
        int expNum = 0;
        for(int i = currentMedal + 1 ; i <= newMedal ; i++)
        {
            SEAchieveData ad = [self getAchieveData:DRAWING_MEDAL medal:i];
            expNum += ad.expNum;
        }
        [self addExpPoint:expNum];
        upgradeOK = YES;
    }
    if(upgradeOK)
    {
        mAchievementUpgrade[DRAWING_MEDAL] = YES;
        [self insertAchieveToCoreData:DRAWING_MEDAL medal:[mViewNav.mSystemDataManager.drawingmedal intValue]];
        [mViewNav notificationShow];
    }
}
- (void) calculateNewPerson
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    if(userInfo.lastfinishoneimagetime == nil)
    {
        userInfo.lastfinishoneimagetime = [NSDate date];
    }
    else
    {
        NSDate* currentDate = [NSDate date];
        NSDate* lastDate = userInfo.lastfinishoneimagetime;
        if([self isAppropriateForNewPersonTime:currentDate lastDate:lastDate])
        {
            [self upgradeNewPersonMedal: userInfo];
        }
        else
        {
            //userInfo.drawimagetime = [NSNumber numberWithInt:0];
            mViewNav.mSystemDataManager.newpersonpoint = [NSNumber numberWithInt:0];
        }
        userInfo.lastfinishoneimagetime = currentDate;
    }
}

- (void) finishOneImageDrawing
{
    if([self canUserUpgrade] == NO)
        return;
    UserInfo* userInfo = [mViewNav getUserInfo];
    [self addOneToFinishedImage:userInfo];
    [self addOneToNewPersonPoint:userInfo];
    [self calculateNewPerson];
    [self calculateDrawingAchieve];
    [self calculateFansPoint];
    [self addExpPoint:mDrawOneTimeExp];
    //[self dump];
}
- (void) finishOneMusicPlay
{
    if([self canUserUpgrade] == NO)
        return;
    UserInfo* userInfo = [mViewNav getUserInfo];
    int musicPlayNum = [mViewNav.mSystemDataManager.finishedmusicnum intValue];
    if(musicPlayNum < mMaxFinishedMusicNum)
    {
        mViewNav.mSystemDataManager.finishedmusicnum = [NSNumber numberWithInt:[mViewNav.mSystemDataManager.finishedmusicnum intValue] + 1];
        [self calculateFansPoint];
    }
    //[self dump];
}
- (void) finishOneComment
{
    if([self canUserUpgrade] == NO)
        return;
    UserInfo* userInfo = [mViewNav getUserInfo];
    int currentCommentNum = [mViewNav.mSystemDataManager.finishedcommentnum intValue];
    if(currentCommentNum < mMaxCommentNum)
    {
        mViewNav.mSystemDataManager.finishedcommentnum = [NSNumber numberWithInt: currentCommentNum + 1];
    }
    [self calculateFansPoint];
    [self addExpPoint:mCommentOneTimeExp];
    [self dump];
}
////////////////////////////////////
- (SEUserData*) getUserData: (int)level
{
    for(SEUserData* userData in mUserMetaData)
    {
        if(userData.level == level)
            return userData;
    }
    return nil;
}
- (SEUserData*) getMaxUserData
{
    int index = mUserMetaData.count - 1;
    return [mUserMetaData objectAtIndex:index];
}

- (float) getCurrentLevelPercent
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    int expNum = [mViewNav.mSystemDataManager.exppointnum intValue];
    int nextLevelIndex = [self getNextLevelIndex:expNum];
    int currentLevel = [self getLevelFromExpNum:expNum];
    SEUserData* userData = [mUserMetaData objectAtIndex: nextLevelIndex];
    SEUserData* currentUserData =  [self getUserData:currentLevel];
    int nextExpNum = userData.expNum;
    if(nextExpNum == 0)
        return 0;
    if(userData.expNum == currentUserData.expNum)
        return 1;
    float ret = ((float)expNum - currentUserData.expNum) / (nextExpNum - currentUserData.expNum);
    if(ret > 1)
    {
        return 1;
    }
    else 
    {
        return (float)ret;
    }
}
- (void) getMedalPointValue: (int)medalType : (int)medal : (int*)currentPointValue : (int*)totalPointValue : (int*)currentPointValueArray: (int*) totalPointValueArray
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    SEAchieveTaskData td = [self getAchieveTaskData:medalType :medal];
    //SEAchieveData achieveData = [self getAchieveData:medalType medal:medal];
    SEAchieveTaskData currentTd;
    [self initTaskData:&currentTd];
    NSNumber* useUnitNum = [mAchieveTaskUnitArray objectAtIndex:USE];
    NSNumber* drawUnitNum = [mAchieveTaskUnitArray objectAtIndex:DRAW];
    NSNumber* musicUnitNum = [mAchieveTaskUnitArray objectAtIndex:MUSIC];
    NSNumber* shareUnitNum = [mAchieveTaskUnitArray objectAtIndex:SHARE];
    NSNumber* buyUnitNum = [mAchieveTaskUnitArray objectAtIndex:BUY];
    int useUnit = [useUnitNum intValue];
    int drawUnit = [drawUnitNum intValue];
    int musicUnit = [musicUnitNum intValue];
    int shareUnit = [shareUnitNum intValue];
    int buyUnit = [buyUnitNum intValue];
    int units[] = {useUnit, drawUnit, musicUnit, shareUnit, buyUnit};
    for(int i = USE ; i < ALL_OPERATION ; i++)
    {
        td.opPoint[i] *= units[i];
    }
    int currentMedal = -1;
    switch (medalType) 
    {
        case NEWPERSON_MEDAL:
        {
            int newPersonPoint = [mViewNav.mSystemDataManager.newpersonpoint intValue] * drawUnit;
            currentTd.opPoint[DRAW] = newPersonPoint;
            currentMedal = [mViewNav.mSystemDataManager.newpersonmedal intValue];
        }
            break;
        case PRESENTONDUTY_MEDAL:
        {
            int presentOnDutyPoint = [mViewNav.mSystemDataManager.presentondutypoint intValue] * useUnit;
            currentTd.opPoint[USE] = presentOnDutyPoint;
            currentMedal = [mViewNav.mSystemDataManager.presentondutymedal intValue];
        }
            break;
        case DRAWING_MEDAL:
        {
            int drawPoint = [mViewNav.mSystemDataManager.finishedimagenum intValue] * drawUnit;
            currentTd.opPoint[DRAW] = drawPoint;
            currentMedal = [mViewNav.mSystemDataManager.drawingmedal intValue];
        }
            break;
        case SHARE_MEDAL:
        {
            
            int sharePoint = [mViewNav.mSystemDataManager.sharepoint intValue] * shareUnit;
            currentTd.opPoint[SHARE] = sharePoint;
            currentMedal = [mViewNav.mSystemDataManager.sharemedal intValue];
        }
            break;
        case FANS_MEDAL:
        {
            
            int drawPoint = [mViewNav.mSystemDataManager.finishedimagenum intValue] * drawUnit;
            int sharePoint = [self getSharePoint:userInfo] * shareUnit;
            int musicPoint = [mViewNav.mSystemDataManager.finishedmusicnum intValue] * musicUnit;
            SEProductManager* productManager = [PhotoFrameAppDelegate getProductManager];
            //int commentPoint = ([userInfo.finishedcommentnum intValue] > 0) * commentUnit;
            int buyPoint = [productManager getUserBuiedProductNum] * buyUnit;
            currentTd.opPoint[DRAW] = drawPoint;
            currentTd.opPoint[SHARE] = sharePoint;
            currentTd.opPoint[MUSIC] = musicPoint;
            currentTd.opPoint[BUY] = buyPoint;
            currentMedal = [mViewNav.mSystemDataManager.fansmedal intValue];
        }
            break;
        default:
            break;
    }
    NSLog(@"currentMedal = %d", currentMedal);
    if(medal <= currentMedal)
    {
        *currentPointValue = td.taskPoint;
        *totalPointValue = td.taskPoint;
        if(medalType == FANS_MEDAL)
        {
            int opType[] = {DRAW, SHARE, MUSIC, BUY};
            for(int i = 0 ; i < 4 ; i++)
            {
                int type = opType[i];
                currentPointValueArray[i] = td.opPoint[type];
                totalPointValueArray[i] = td.opPoint[type];
            }
        }
    }
    else
    {
        int totalPoint = 0;
        for(int i  = USE ; i < ALL_OPERATION ; i++)
        {
            if(currentTd.opPoint[i] < td.opPoint[i])
                totalPoint += currentTd.opPoint[i];
            else {
                totalPoint += td.opPoint[i];
            }
        }
        if(totalPoint <= td.taskPoint)
        {
            *currentPointValue = totalPoint;
        }
        else 
        {
            *currentPointValue = td.taskPoint;
        }
        *totalPointValue = td.taskPoint;
        if(medalType == FANS_MEDAL)
        {
            int opType[] = {DRAW, SHARE, MUSIC, BUY};
            for(int i = 0 ; i < 4 ; i++)
            {
                int type = opType[i];
                totalPointValueArray[i] = td.opPoint[type];
                if(currentTd.opPoint[type] > td.opPoint[type])
                {
                    currentPointValueArray[i] = td.opPoint[type];
                }
                else
                {
                    currentPointValueArray[i] = currentTd.opPoint[type];
                }
            }
        }
    }
}
- (void) getMedalPercent: (int)medalType : (int) medal: (float*)outPercent: (float*)outPercentArray
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    //int finishedImageNum = [userInfo.finishedimagenum intValue];
    int currentPoint = 0;
    int totalPoint = 0;
    int currentPointArray[4];
    int totalPointArray[4];
    for(int i = 0 ; i < 4 ; i++)
    {
        currentPointArray[i] = totalPointArray[i] = 1;
    }
    [self getMedalPointValue:medalType :medal :&currentPoint :&totalPoint: currentPointArray: totalPointArray];
    NSLog(@"medalType = %d, medal = %d", medalType, medal);
    NSLog(@"currentpoint = %d, totalPoint = %d", currentPoint, totalPoint);
    assert(currentPoint <= totalPoint);
    *outPercent = currentPoint / (float)totalPoint;
    for(int i = 0 ; i < 4 ; i++)
    {
        outPercentArray[i] = currentPointArray[i] / (float)totalPointArray[i];
    }
}
- (UIImage*) getAchieveMedalImage: (int)achieveType medal: (int)medal
{
    UIImage* image = nil;
    switch (achieveType) 
    {
        case NEWPERSON_MEDAL:
        {
            switch (medal) {
                case COPPER_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradeNewPersonCopper"];
                    break;
                case SILVER_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradeNewPersonSilver"];
                    break;
                case GOLD_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradeNewPersonGold"];
                    break;
                default:
                    break;
            }
        }        
            break;
        case PRESENTONDUTY_MEDAL:
        {
            switch (medal) {
                case COPPER_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradePresentOnDutyCopper"];
                    break;
                case SILVER_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradePresentOnDutySilver"];
                    break;
                case GOLD_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradePresentOnDutyGold"];
                    break;
                default:
                    break;
            }
        }
            break;
        case DRAWING_MEDAL:
        {
            switch (medal) {
                case COPPER_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradeDrawingCopper"];
                    break;
                case SILVER_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradeDrawingSilver"];
                    break;
                case GOLD_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradeDrawingGold"];
                    break;
                default:
                    break;
            }
        }
            break;
        case SHARE_MEDAL:
        {
            switch (medal) {
                case COPPER_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradeShareCopper"];
                    break;
                case SILVER_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradeShareSilver"];
                    break;
                case GOLD_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradeShareGold"];
                    break;
                default:
                    break;
            }
        }
            break;
        case FANS_MEDAL:
        {
            switch (medal) {
                case COPPER_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradeFansCopper"];
                    break;
                case SILVER_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradeFansSilver"];
                    break;
                case GOLD_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradeFansGold"];
                    break;
                default:
                    break;
            }
        }
            break;
        default:
            break;
    }
    return image;

}
- (UIImage*) getCurrentAchieveMedal: (int)medalType
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    UIImage* image = nil;
    switch (medalType) 
    {
        case NEWPERSON_MEDAL:
        {
            int medal = [mViewNav.mSystemDataManager.newpersonmedal intValue];
            switch (medal) {
                case COPPER_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradeNewPersonCopper"];
                    break;
                case SILVER_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradeNewPersonSilver"];
                    break;
                case GOLD_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradeNewPersonGold"];
                    break;
                default:
                    break;
            }
        }        
            break;
        case PRESENTONDUTY_MEDAL:
        {
            int medal = [mViewNav.mSystemDataManager.presentondutymedal intValue];
            switch (medal) {
                case COPPER_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradePresentOnDutyCopper"];
                    break;
                case SILVER_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradePresentOnDutySilver"];
                    break;
                case GOLD_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradePresentOnDutyGold"];
                    break;
                default:
                    break;
            }
        }
            break;
        case DRAWING_MEDAL:
        {
            int medal = [mViewNav.mSystemDataManager.drawingmedal intValue];
            switch (medal) {
                case COPPER_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradeDrawingCopper"];
                    break;
                case SILVER_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradeDrawingSilver"];
                    break;
                case GOLD_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradeDrawingGold"];
                    break;
                default:
                    break;
            }
        }
            break;
        case SHARE_MEDAL:
        {
            int medal = [mViewNav.mSystemDataManager.sharemedal intValue];
            switch (medal) {
                case COPPER_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradeShareCopper"];
                    break;
                case SILVER_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradeShareSilver"];
                    break;
                case GOLD_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradeShareGold"];
                    break;
                default:
                    break;
            }
        }
            break;
        case FANS_MEDAL:
        {
            int medal = [mViewNav.mSystemDataManager.fansmedal intValue];
            switch (medal) {
                case COPPER_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradeFansCopper"];
                    break;
                case SILVER_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradeFansSilver"];
                    break;
                case GOLD_MEDAL:
                    image = [mViewNav.mResLoader getImage:@"UserUpgradeFansGold"];
                    break;
                default:
                    break;
            }
        }
            break;
        default:
            break;
    }
    return image;
}
- (int) getCurrentMedal: (int)type
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    int medal = -1;
    switch (type) {
        case PRESENTONDUTY_MEDAL:
            medal = [mViewNav.mSystemDataManager.presentondutymedal intValue];
            break;
        case NEWPERSON_MEDAL:
            medal = [mViewNav.mSystemDataManager.newpersonmedal intValue];
            break;
        case DRAWING_MEDAL:
            medal = [mViewNav.mSystemDataManager.drawingmedal intValue];
            break;
        case SHARE_MEDAL:
            medal = [mViewNav.mSystemDataManager.sharemedal intValue];
            break;
        case FANS_MEDAL:
            medal = [mViewNav.mSystemDataManager.fansmedal intValue];
            break;
        default:
            break;
    }
    return medal;
}
- (void) setTest
{
    mLevelUpgrate = YES;
    mFromLevel = 1;
    mToLevel = 2;
    mAchievementUpgrade[NEWPERSON_MEDAL] = YES;
    UserInfo* userInfo = [mViewNav getUserInfo];
    mViewNav.mSystemDataManager.newpersonmedal = [NSNumber numberWithInt:1];
}
- (SEAchieveData) getAchieveData: (int)achieveType medal:(int)medal
{
    assert(achieveType >= 0 && achieveType < USER_MEDAL_COUNT);
    assert(medal >= 0 && medal < MEDAL_LEVEL_COUNT);
    NSValue* v = [mAchieveDataArray objectAtIndex:achieveType * MEDAL_LEVEL_COUNT + medal];
    SEAchieveData data;
    [v getValue:&data];
    return data;
}

- (void) clearAllData
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    mViewNav.mSystemDataManager.presentondutymedal = [NSNumber numberWithInt:INVALID_MEDAL_LEVEL];
    mViewNav.mSystemDataManager.newpersonmedal = [NSNumber numberWithInt:INVALID_MEDAL_LEVEL];
    mViewNav.mSystemDataManager.drawingmedal = [NSNumber numberWithInt:INVALID_MEDAL_LEVEL];
    mViewNav.mSystemDataManager.sharemedal = [NSNumber numberWithInt:INVALID_MEDAL_LEVEL];
    mViewNav.mSystemDataManager.fansmedal = [NSNumber numberWithInt:INVALID_MEDAL_LEVEL];
    
    mViewNav.mSystemDataManager.presentondutypoint = [NSNumber numberWithInt:0];
    mViewNav.mSystemDataManager.newpersonpoint = [NSNumber numberWithInt:0];
    mViewNav.mSystemDataManager.drawingpoint = [NSNumber numberWithInt:0];
    mViewNav.mSystemDataManager.sharepoint = [NSNumber numberWithInt:0];
    mViewNav.mSystemDataManager.fanspoint = [NSNumber numberWithInt:0];
    
    mViewNav.mSystemDataManager.finishedimagenum = [NSNumber numberWithInt:0];
    mViewNav.mSystemDataManager.finishedmusicnum = [NSNumber numberWithInt:0];
    mViewNav.mSystemDataManager.finishedcommentnum = [NSNumber numberWithInt:0];
    mViewNav.mSystemDataManager.exppointnum = [NSNumber numberWithInt:0];
    [userInfo removeUpgradelist:userInfo.upgradelist];
    //NSLog(@"upgradelist= %@ count = %d", userInfo.upgradelist, userInfo.upgradelist.count);
    [mViewNav saveCoreDataContext];
}
- (NSString*) getAllData
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    NSString* s1 = [NSString stringWithFormat:@"present on duty medal = %d\n", [mViewNav.mSystemDataManager.presentondutymedal intValue]];
    NSString* s2 = [NSString stringWithFormat: @"new person medal = %d\n", [mViewNav.mSystemDataManager.newpersonmedal intValue]];
    NSString* s3 = [NSString stringWithFormat: @"drawing medal = %d\n", [mViewNav.mSystemDataManager.drawingmedal intValue]];
    NSString* s4 = [NSString stringWithFormat: @"share medal = %d\n", [mViewNav.mSystemDataManager.sharemedal intValue]];
    NSString* s5 = [NSString stringWithFormat: @"fans medal = %d\n", [mViewNav.mSystemDataManager.fansmedal intValue]];

    NSString* s6 = [NSString stringWithFormat: @"present on duty point = %d\n", [mViewNav.mSystemDataManager.presentondutypoint intValue]];
    NSString* s7 = [NSString stringWithFormat: @"new person point = %d\n", [mViewNav.mSystemDataManager.newpersonpoint intValue]];
    NSString* s8 = [NSString stringWithFormat: @"drawing point = %d\n", [mViewNav.mSystemDataManager.drawingpoint intValue]];
    NSString* s9 = [NSString stringWithFormat: @"share point = %d\n", [mViewNav.mSystemDataManager.sharepoint intValue]];
    NSString* s10 = [NSString stringWithFormat: @"fans point = %d\n", [mViewNav.mSystemDataManager.fanspoint intValue]];
    NSString* s11 = [NSString stringWithFormat: @"finished image = %d\n", [mViewNav.mSystemDataManager.finishedimagenum intValue]];
    NSString* s12 = [NSString stringWithFormat: @"exp num = %d\n", [mViewNav.mSystemDataManager.exppointnum intValue]];

    NSString* s13 = [NSString stringWithFormat:@"finished music point = %d\n", [mViewNav.mSystemDataManager.finishedmusicnum intValue]];
    NSString* s14 = [NSString stringWithFormat:@"finished comment point = %d\n", [mViewNav.mSystemDataManager.finishedcommentnum intValue]];
    NSString* array[] = {s1, s2, s3, s4, s5, s6,s7,s8,s9,s10,s11,s12, s13, s14};
    NSString* str = [NSString stringWithFormat:@"data:\n"];
    for(int i = 0 ; i < 14 ; i++)
    {
        str = [str stringByAppendingFormat:@"%@", array[i]];
    }
    return str;
}
- (void) dump
{
    return;
    /*
    UserInfo* userInfo = [mViewNav getUserInfo];
    NSLog(@"###########");
    NSLog(@"present on duty medal = %d", [userInfo.presentondutymedal intValue]);
    NSLog(@"new person medal = %d", [userInfo.newpersonmedal intValue]);
    NSLog(@"drawing medal = %d", [userInfo.drawingmedal intValue]);
    NSLog(@"share medal = %d", [userInfo.sharemedal intValue]);
    NSLog(@"fans medal = %d", [userInfo.fansmedal intValue]);
    NSLog(@"###########");
    NSLog(@"present on duty point = %d", [userInfo.presentondutypoint intValue]);
    NSLog(@"new person point = %d", [userInfo.newpersonpoint intValue]);
    NSLog(@"drawing point = %d", [userInfo.drawingpoint intValue]);
    NSLog(@"share point = %d", [userInfo.sharepoint intValue]);
    NSLog(@"fans point = %d", [userInfo.fanspoint intValue]);
    NSLog(@"######");
    NSLog(@"finished image = %d", [userInfo.finishedimagenum intValue]);
    NSLog(@"exp num = %d", [userInfo.exppointnum intValue]);
    NSLog(@"#######");
     */
}
- (void) setToMaxLevel
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    int currentExpNum = [mViewNav.mSystemDataManager.exppointnum intValue];
    SEUserData* userData = [self getMaxUserData];
    [self addExp:userData.expNum - currentExpNum];
}
- (void) addExp : (int)num
{
    [self addExpPoint:num];
}
- (NSString*) getAchieveDescription: (int)achieveType medal: (int)medal
{
    return gAchieveDescriptionString[achieveType * MEDAL_LEVEL_COUNT + medal];
}    
- (int) getImageDeltaNumByAchieve: (int)achieveType medal: (int)medal
{
    if(achieveType == INVALID_ACHIEVE)
        return 0;
    if(medal == INVALID_MEDAL_LEVEL)
        return 0;
    int startIndex = achieveType * MEDAL_LEVEL_COUNT;
    int num = 0;
    for(int i = 0 ; i <= medal ; i++)
    {
        SEAchieveData data = [self getAchieveData:achieveType medal:i];//gAchieveAllData[startIndex + i];
        num += data.imageListNumDelta;
    }
    return num;
}
- (int) getMusicDeltaNumByAchieve: (int)achieveType medal: (int)medal
{
    if(achieveType == INVALID_ACHIEVE)
        return 0;
    if(medal == INVALID_MEDAL_LEVEL)
        return 0;
    int startIndex = achieveType * MEDAL_LEVEL_COUNT;
    int num = 0;
    for(int i = 0 ; i <= medal ; i++)
    {
        SEAchieveData data = [self getAchieveData:achieveType medal:i];//gAchieveAllData[startIndex + i];
        num += data.musicListNumDelta;
    }
    return num;
}
- (NSArray*) getAllAchieveMedalValue
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    //int medal[] = {[userInfo.presentondutymedal intValue], [userInfo.newpersonmedal intValue], [userInfo.drawingmedal intValue], [userInfo.sharemedal intValue], [userInfo.fansmedal intValue]};
    return [NSArray arrayWithObjects:mViewNav.mSystemDataManager.presentondutymedal, mViewNav.mSystemDataManager.newpersonmedal, mViewNav.mSystemDataManager.drawingmedal, mViewNav.mSystemDataManager.sharemedal, mViewNav.mSystemDataManager.fansmedal, nil];
}
- (int) getAllAchieveDeltaImageNum
{
    //UserInfo* userInfo = [mViewNav getUserInfo];
    //int medal[] = {[userInfo.presentondutymedal intValue], [userInfo.newpersonmedal intValue], [userInfo.drawingmedal intValue], [userInfo.sharemedal intValue], [userInfo.fansmedal intValue]};
    NSArray* medalArray = [self getAllAchieveMedalValue];
    int totalNum = 0;
    for(int i = 0 ; i < USER_MEDAL_COUNT ; i++)
    {
        int medal = [[medalArray objectAtIndex:i] intValue];
        int num = [self getImageDeltaNumByAchieve:i medal:medal];
        totalNum += num;
    }
    return totalNum;
}
- (int) getAllAchieveDeltaMusicNum
{
    //UserInfo* userInfo = [mViewNav getUserInfo];
    //int medal[] = {[userInfo.presentondutymedal intValue], [userInfo.newpersonmedal intValue], [userInfo.drawingmedal intValue], [userInfo.sharemedal intValue], [userInfo.fansmedal intValue]};
    NSArray* medalArray = [self getAllAchieveMedalValue];
    int totalNum = 0;
    for(int i = 0 ; i < USER_MEDAL_COUNT ; i++)
    {
        int medal = [[medalArray objectAtIndex:i] intValue];
        int num = [self getMusicDeltaNumByAchieve:i medal:medal];
        totalNum += num;
    }
    return totalNum;
}
// -1 is the invalid level num
//
- (int) getLevelFromExpNum: (int)expNum
{
    for(int i = mUserMetaData.count - 1 ; i >= 0 ;i--)
    {
        SEUserData* userData = [mUserMetaData objectAtIndex:i];
        if(expNum >= userData.expNum)
            return userData.level;
    }
    assert(0);
    return -1;
}
enum {GAIN_BRUSH, GAIN_TIME_FONT};
- (NSArray*) getUserGainByAchieve: (int)achieveType medal: (int)medal gainType:(int) gainType
{
    NSArray* array = [NSArray array];
    if(achieveType == INVALID_ACHIEVE)
        return array;
    if(medal == INVALID_MEDAL_LEVEL)
        return array;
    //for(int i = 0 ; i <= achieveType ; i++)
    //{
        for(int j = 0 ; j <= medal ; j++)
        {
            SEAchieveData data = [self getAchieveData:achieveType medal:j];
            switch (gainType) {
                case GAIN_BRUSH:
                {
                    if(data.brushNumDelta > 0)
                    {
                        array = [array arrayByAddingObject:[NSNumber numberWithInt:data.brushNumDelta]];
                    }
                }
                    break;
                case GAIN_TIME_FONT:
                {
                    if(data.timeFontDelta > 0)
                    {
                        array = [array arrayByAddingObject:[NSNumber numberWithInt:data.timeFontDelta]];
                    }
                }
                    break;
                default:
                    break;
            }

        }
    //}
    return array;
}
- (NSArray*) getTimeFontIDByAchieve: (int)achieveType medal: (int)medal
{
    return [self getUserGainByAchieve:achieveType medal:medal gainType:GAIN_TIME_FONT];
}
- (NSArray*) getBrushIDByAchieve:(int)achieveType medal: (int)medal
{
    return [self getUserGainByAchieve:achieveType medal:medal gainType:GAIN_BRUSH];
    /*
    NSArray* array = [NSArray array];
    if(achieveType == INVALID_ACHIEVE)
        return array;
    if(medal == INVALID_MEDAL_LEVEL)
        return array;
    for(int i = 0 ; i <= achieveType ; i++)
    {
        for(int j = 0 ; j <= medal ; j++)
        {
            SEAchieveData data = gAchieveAllData[i * MEDAL_LEVEL_COUNT + j];
            if(data.brushNumDelta > 0)
            {
                array = [array arrayByAddingObject:[NSNumber numberWithInt:data.brushNumDelta]];
            }
        }
    }
    return array;
     */
}
- (NSArray*) getAllBrushIDByUserCurrentAchieve
{
    NSArray* medalArray = [self getAllAchieveMedalValue];
    NSArray* retArray = [NSArray array];
    for(int i = 0 ; i < medalArray.count ; i++)
    {
        int medal = [[medalArray objectAtIndex:i] intValue];
        NSArray* newArray = [self getBrushIDByAchieve:i medal:medal];
        retArray = [retArray arrayByAddingObjectsFromArray:newArray];
    }
    return retArray;
}
- (NSArray*) getUserGainBetweenLevel: (int)level1 : (int)level2 gainType: (int)gainType
{
    assert(level1 <= level2);
    NSArray* retArray = [NSArray array];
    for (int i = 0 ; i < mUserMetaData.count; i++) 
    {
        SEUserData* userData = [mUserMetaData objectAtIndex:i];
        if(userData.level <= level2 && userData.level >= level1)
        {
            switch (gainType) {
                case GAIN_BRUSH:
                {
                    if(userData.brushNum > 0)
                    {
                        retArray = [retArray arrayByAddingObject:[NSNumber numberWithInt:userData.brushNum]];
                    }
                }
                    break;
                case GAIN_TIME_FONT:
                {
                    if(userData.timeFontNum > 0)
                    {
                        retArray = [retArray arrayByAddingObject:[NSNumber numberWithInt:userData.timeFontNum]];
                    }
                }
                    break;
                default:
                    break;
            }
            
        }
    }
    return retArray;    
}
- (NSArray*) getUserGainByUserCurrentLevel: (int)gainType
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    int level = [self getLevelFromExpNum:[mViewNav.mSystemDataManager.exppointnum intValue]];
    NSArray* retArray = [NSArray array];
    for (int i = 0 ; i < mUserMetaData.count; i++) 
    {
        SEUserData* userData = [mUserMetaData objectAtIndex:i];
        NSLog(@"level = %d", userData.level);
        if(userData.level <= level)
        {
            switch (gainType) {
                case GAIN_BRUSH:
                {
                    if(userData.brushNum > 0)
                    {
                        retArray = [retArray arrayByAddingObject:[NSNumber numberWithInt:userData.brushNum]];
                    }
                }
                    break;
                case GAIN_TIME_FONT:
                {
                    NSLog(@"time font num = %d", userData.timeFontNum);
                    if(userData.timeFontNum > 0)
                    {
                        retArray = [retArray arrayByAddingObject:[NSNumber numberWithInt:userData.timeFontNum]];
                    }
                }
                    break;
                default:
                    break;
            }

        }
    }
    return retArray;
}
- (NSArray*) getAllBrushIDByUserCurrentLevel
{
    return [self getUserGainByUserCurrentLevel:GAIN_BRUSH];
    /*
    UserInfo* userInfo = [mViewNav getUserInfo];
    int level = [self getLevelFromExpNum:[userInfo.exppointnum intValue]];
    NSArray* retArray = [NSArray array];
    for (int i = 0 ; i < mUserMetaData.count; i++) 
    {
        SEUserData* userData = [mUserMetaData objectAtIndex:i];
        if(userData.level <= level)
        {
            if(userData.brushNum > 0)
            {
                retArray = [retArray arrayByAddingObject:[NSNumber numberWithInt:userData.brushNum]];
            }
        }
    }
    return retArray;
     */
}
- (NSArray*) getAllTimeFontByCurrentAchieve
{
    NSArray* medalArray = [self getAllAchieveMedalValue];
    NSArray* retArray = [NSArray array];
    for(int i = 0 ; i < USER_MEDAL_COUNT ; i++)
    {
        int medal = [[medalArray objectAtIndex:i] intValue];
        NSArray* newArray = [self getTimeFontIDByAchieve:i medal:medal];
        retArray = [retArray arrayByAddingObjectsFromArray:newArray];
    }
    return retArray;
}
- (NSArray*) getAllTimeFontByCurrentLevel
{
    return [self getUserGainByUserCurrentLevel:GAIN_TIME_FONT];
}
- (NSArray*) getBrushIDBetweenTwoLevel: (int)level1 : (int)level2
{
    return [self getUserGainBetweenLevel:level1 :level2 gainType:GAIN_BRUSH];
}
- (NSArray*) getTimeFontBetweenTwoLevel: (int) level1 : (int) level2
{
    return [self getUserGainBetweenLevel:level1 :level2 gainType:GAIN_TIME_FONT];
}
+ (NSArray*) getFeedChar
{
    NSArray* ret = [NSArray arrayWithObject:[NSNumber numberWithUnsignedChar:'m']];
    return ret;
}
/////////////////////////////
- (void) test_AddExp
{
    /*
    [self clearAllData];
    UserInfo* userInfo = [mViewNav getUserInfo];
    int expNum[] = { 0,450,950,1550,2250,3050,3950,4950,6050,7250,8550,9950,11450,13050,14750,16550,18450,20450,22550,24750,27050,29450,31950,34550,37250,40050,42950,45950,49050,52250};
    assert([userInfo.exppointnum doubleValue] == 0);
    [self addExp:450];
    assert([userInfo.exppointnum intValue] == [[mUserMetaData lastObject] expNum]);
    [self addExp:1000];
    assert([userInfo.exppointnum intValue] == [[mUserMetaData lastObject] expNum]);
     */
    NSLog(@"test_AddExp ok");
}
- (void) test_addOneToFinishedImage
{
    [self clearAllData];
    UserInfo* userInfo = [mViewNav getUserInfo];
    [self addOneToFinishedImage:userInfo];
    assert([mViewNav.mSystemDataManager.finishedimagenum intValue] == 1);
    for(int i = 0 ; i <= mMaxFinishedImageNum ; i++)
    {
        [self addOneToFinishedImage:userInfo];
    }
    assert([mViewNav.mSystemDataManager.finishedimagenum intValue] == mMaxFinishedImageNum);
    NSLog(@"test_addOneToFnishedImage ok");
}
-(void) test_addOneToNewPersonPoint
{
    [self clearAllData];
    UserInfo* userInfo = [mViewNav getUserInfo];
    [self addOneToNewPersonPoint:userInfo];
    assert([mViewNav.mSystemDataManager.newpersonpoint intValue] == 1);
    for(int i = 0 ; i <= mMaxNewPersonPointNum ; i++)
    {
        [self addOneToNewPersonPoint:userInfo];
    }
    assert([mViewNav.mSystemDataManager.newpersonpoint intValue] == mMaxNewPersonPointNum);

    NSLog(@"test_addOneToNewPersonPoint ok");
}
- (void) test_levelUp
{
    [self clearAllData];
    [self addExpPoint:450];
    assert(mFromLevel == 1 && mToLevel == 2);
    [self addExpPoint:950];
    assert(mFromLevel == 2 && mToLevel == 3);
    [self clearAllData];
    [self addExpPoint:5200];
    assert(mFromLevel == 1 && mToLevel == 5);
    [self clearAllData];
    [self addExpPoint:5000];
    assert(mFromLevel == 1 && mToLevel == 4);
    [self clearAllData];
    [self addExpPoint:581450];
    assert(mFromLevel == 1 && mToLevel == 30);
    [self addExpPoint:5666];
    assert(mFromLevel == 1 && mToLevel == 30);
    [self clearAllData];
    [self addExpPoint:560000];
    assert(mFromLevel == 1 && mToLevel == 29);
    [self clearAllData];
    [self addExpPoint:590000];
    assert(mFromLevel == 1 && mToLevel == 30);
    NSLog(@"test_levelUp ok");
}
- (void) test_NewPersonAchieve
{
    [self clearAllData];
    BOOL ok = [SEUserDefaultManager isFunctionOK:USERINFO_FUNC];
    [SEUserDefaultManager setFunction:USERINFO_FUNC value:YES];
    UserInfo* userInfo = [mViewNav getUserInfo];
    for(int i = 0; i < 50 ; i++)
    {
        [self addOneToNewPersonPoint:userInfo];
    }
    [self upgradeNewPersonMedal:userInfo];
    assert([mViewNav.mSystemDataManager.newpersonmedal intValue] == COPPER_MEDAL);
    
    [self clearAllData];
    for(int i = 0; i < 240 ; i++)
    {
        [self addOneToNewPersonPoint:userInfo];
    }
    [self upgradeNewPersonMedal:userInfo];
    assert([mViewNav.mSystemDataManager.newpersonmedal intValue] == SILVER_MEDAL);
    
    [self clearAllData];
    for(int i = 0 ; i < 600; i++)
    {
        [self addOneToNewPersonPoint:userInfo];
    }
    [self upgradeNewPersonMedal:userInfo];
    assert([mViewNav.mSystemDataManager.newpersonmedal intValue] == GOLD_MEDAL);
    NSLog(@"test_achieve ok");
}
- (void) test_DrawingAchieve
{
    [self clearAllData];
    UserInfo* userInfo = [mViewNav getUserInfo];
    for(int i = 0 ; i < 500; i++)
    {
        [self addOneToFinishedImage:userInfo];
    }
    [self calculateDrawingAchieve];
    assert([mViewNav.mSystemDataManager.drawingmedal intValue] == COPPER_MEDAL);
    
    [self clearAllData];
    for(int i = 0 ; i < 1500; i++)
    {
        [self addOneToFinishedImage:userInfo];
    }
    [self calculateDrawingAchieve];
    assert([mViewNav.mSystemDataManager.drawingmedal intValue] == SILVER_MEDAL);
    
    [self clearAllData];
    for(int i = 0 ; i < 10000; i++)
    {
        [self addOneToFinishedImage:userInfo];
    }
    [self calculateDrawingAchieve];
    assert([mViewNav.mSystemDataManager.drawingmedal intValue] == GOLD_MEDAL);
    NSLog(@"test_DrawingAchieve ok");
    
}
- (void) test_ShareAchieve
{
    [self clearAllData];
    UserInfo* userInfo = [mViewNav getUserInfo];
    for(int i = 0 ; i < 10 ; i++)
    {
        [self addOneForSharePoint:userInfo];
    }
    [self upgradeShareMedal:userInfo];
    assert([mViewNav.mSystemDataManager.sharemedal intValue] == COPPER_MEDAL);
    
    [self clearAllData];
    for(int i = 0 ; i < 50 ; i++)
    {
        [self addOneForSharePoint:userInfo];
    }
    [self upgradeShareMedal:userInfo];
    assert([mViewNav.mSystemDataManager.sharemedal intValue] == SILVER_MEDAL);
    
    [self clearAllData];
    for(int i = 0 ; i < 200 ; i++)
    {
        [self addOneForSharePoint:userInfo];
    }
    [self upgradeShareMedal:userInfo];
    assert([mViewNav.mSystemDataManager.sharemedal intValue] == GOLD_MEDAL);
    
    
    [self clearAllData];
    for(int i = 0 ; i < 20 ; i++)
    {
        [self addOneForSharePoint:userInfo];
    }
    [self upgradeShareMedal:userInfo];
    assert([mViewNav.mSystemDataManager.sharemedal intValue] == COPPER_MEDAL);
    
    [self clearAllData];
    for(int i = 0 ; i < 5 ; i++)
    {
        [self addOneForSharePoint:userInfo];
    }
    [self upgradeShareMedal:userInfo];
    assert([mViewNav.mSystemDataManager.sharemedal intValue] == INVALID_MEDAL_LEVEL);
    
    [self clearAllData];
    for(int i = 0 ; i < 70 ; i++)
    {
        [self addOneForSharePoint:userInfo];
    }
    [self upgradeShareMedal:userInfo];
    assert([mViewNav.mSystemDataManager.sharemedal intValue] == SILVER_MEDAL);
    
    [self clearAllData];
    for(int i = 0 ; i < 250 ; i++)
    {
        [self addOneForSharePoint:userInfo];
    }
    [self upgradeShareMedal:userInfo];
    assert([mViewNav.mSystemDataManager.sharemedal intValue] == GOLD_MEDAL);
    NSLog(@"test_ShareAchieve ok");
    
}
- (void) test_FansAchieve
{
    [self clearAllData];
    UserInfo* userInfo = [mViewNav getUserInfo];
    assert([mViewNav.mSystemDataManager.fansmedal intValue] == INVALID_MEDAL_LEVEL );
    mViewNav.mSystemDataManager.finishedimagenum  = [NSNumber numberWithInt: 100];
    mViewNav.mSystemDataManager.sharepoint = [NSNumber numberWithInt:10];
    mViewNav.mSystemDataManager.finishedmusicnum = [NSNumber numberWithInt:20];
    mViewNav.mSystemDataManager.finishedcommentnum = [NSNumber numberWithInt:1];
    [self calculateFansPoint];
    assert([mViewNav.mSystemDataManager.fansmedal intValue] == COPPER_MEDAL);
    
    [self clearAllData];
    assert([mViewNav.mSystemDataManager.fansmedal intValue] == INVALID_MEDAL_LEVEL );
    mViewNav.mSystemDataManager.finishedimagenum  = [NSNumber numberWithInt: 500];
    mViewNav.mSystemDataManager.sharepoint = [NSNumber numberWithInt:50];
    mViewNav.mSystemDataManager.finishedmusicnum = [NSNumber numberWithInt:100];
    mViewNav.mSystemDataManager.finishedcommentnum = [NSNumber numberWithInt:1];
    [self calculateFansPoint];
    assert([mViewNav.mSystemDataManager.fansmedal intValue] == SILVER_MEDAL);
    
    [self clearAllData];
    assert([mViewNav.mSystemDataManager.fansmedal intValue] == INVALID_MEDAL_LEVEL );
    mViewNav.mSystemDataManager.finishedimagenum  = [NSNumber numberWithInt: 2500];
    mViewNav.mSystemDataManager.sharepoint = [NSNumber numberWithInt:200];
    mViewNav.mSystemDataManager.finishedmusicnum = [NSNumber numberWithInt:250];
    mViewNav.mSystemDataManager.finishedcommentnum = [NSNumber numberWithInt:1];
    [self calculateFansPoint];
    assert([mViewNav.mSystemDataManager.fansmedal intValue] == GOLD_MEDAL);
    
    [self clearAllData];
    assert([mViewNav.mSystemDataManager.fansmedal intValue] == INVALID_MEDAL_LEVEL );
    mViewNav.mSystemDataManager.finishedimagenum  = [NSNumber numberWithInt: 600];
    mViewNav.mSystemDataManager.sharepoint = [NSNumber numberWithInt:50];
    mViewNav.mSystemDataManager.finishedmusicnum = [NSNumber numberWithInt:50];
    mViewNav.mSystemDataManager.finishedcommentnum = [NSNumber numberWithInt:1];
    [self calculateFansPoint];
    assert([mViewNav.mSystemDataManager.fansmedal intValue] == COPPER_MEDAL);
    
    [self clearAllData];
    assert([mViewNav.mSystemDataManager.fansmedal intValue] == INVALID_MEDAL_LEVEL );
    mViewNav.mSystemDataManager.finishedimagenum  = [NSNumber numberWithInt: 500];
    mViewNav.mSystemDataManager.sharepoint = [NSNumber numberWithInt:50];
    mViewNav.mSystemDataManager.finishedmusicnum = [NSNumber numberWithInt:100];
    mViewNav.mSystemDataManager.finishedcommentnum = [NSNumber numberWithInt:0];
    [self calculateFansPoint];
    assert([mViewNav.mSystemDataManager.fansmedal intValue] == INVALID_MEDAL_LEVEL);
    
    
    NSLog(@"test_FansAchieve ok");
}
- (void) test_getLevelPoint
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    [self clearAllData];
    int medalType = FANS_MEDAL;
    int medal = COPPER_MEDAL;
    int currentPoint = 0;
    int totalPoint = 0;
    int currentPointArray[4];
    int totalPointArray[4];
    mViewNav.mSystemDataManager.sharepoint = [NSNumber numberWithInt:20];
    [self getMedalPointValue:medalType :medal :&currentPoint :&totalPoint: currentPointArray: totalPointArray];
    NSLog(@"currentpoint = %d", currentPoint);
    NSLog(@"totalpoint = %d", totalPoint);
    assert(currentPoint == 200);
    assert(totalPoint == 400);
    
    [self clearAllData];
    medalType = PRESENTONDUTY_MEDAL;
    medal = COPPER_MEDAL;
    mViewNav.mSystemDataManager.presentondutypoint = [NSNumber numberWithInt:10];
    mViewNav.mSystemDataManager.presentondutymedal = [NSNumber numberWithInt:INVALID_MEDAL_LEVEL];
    [self getMedalPointValue:medalType :medal :&currentPoint :&totalPoint : currentPointArray:totalPointArray];
    assert(currentPoint == 10);
    assert(totalPoint == 10);
    
    [self clearAllData];
    medalType = PRESENTONDUTY_MEDAL;
    medal = COPPER_MEDAL;
    mViewNav.mSystemDataManager.presentondutypoint = [NSNumber numberWithInt:15];
    mViewNav.mSystemDataManager.presentondutymedal = [NSNumber numberWithInt:COPPER_MEDAL];
    [self getMedalPointValue:medalType :medal :&currentPoint :&totalPoint:currentPointArray:totalPointArray];
    assert(currentPoint == 10);
    assert(totalPoint == 10);
    
    [self clearAllData];
    medalType = PRESENTONDUTY_MEDAL;
    medal = SILVER_MEDAL;
    mViewNav.mSystemDataManager.presentondutypoint = [NSNumber numberWithInt:15];
    mViewNav.mSystemDataManager.presentondutymedal = [NSNumber numberWithInt:COPPER_MEDAL];
    [self getMedalPointValue:medalType :medal :&currentPoint :&totalPoint:currentPointArray:totalPointArray];
    assert(currentPoint == 15);
    assert(totalPoint == 30);
    
    [self clearAllData];
    medalType = PRESENTONDUTY_MEDAL;
    medal = GOLD_MEDAL;
    mViewNav.mSystemDataManager.presentondutypoint = [NSNumber numberWithInt:15];
    mViewNav.mSystemDataManager.presentondutymedal = [NSNumber numberWithInt:COPPER_MEDAL];
    [self getMedalPointValue:medalType :medal :&currentPoint :&totalPoint:currentPointArray:totalPointArray];
    assert(currentPoint == 15);
    assert(totalPoint == 60);
    
    [self clearAllData];
    medalType = PRESENTONDUTY_MEDAL;
    medal = GOLD_MEDAL;
    mViewNav.mSystemDataManager.presentondutymedal = [NSNumber numberWithInt:GOLD_MEDAL];
    mViewNav.mSystemDataManager.presentondutypoint = [NSNumber numberWithInt:65];
    [self getMedalPointValue:medalType :medal :&currentPoint :&totalPoint:currentPointArray:totalPointArray];
    assert(currentPoint == 60);
    assert(totalPoint == 60);
    NSLog(@"test_getLevelPoint ok");
}
- (void) test_getImageDeltaNumByAchieve
{
    [self clearAllData];
    int medalType = PRESENTONDUTY_MEDAL;
    int medal = COPPER_MEDAL;
    int num = [self getImageDeltaNumByAchieve:medalType medal:medal];
    assert(num == 10);
    
    [self clearAllData];
    medalType = PRESENTONDUTY_MEDAL;
    medal = SILVER_MEDAL;
    num = [self getImageDeltaNumByAchieve:medalType medal:medal];
    assert(num == 20);
    
    [self clearAllData];
    medalType = PRESENTONDUTY_MEDAL;
    medal = GOLD_MEDAL;
    num = [self getImageDeltaNumByAchieve:medalType medal:medal];
    assert(num == 30);
    NSLog(@"test_getImageDeltaNumByAchieve ok");
}
- (void) test_getMusicDeltaNumByAchieve
{
    [self clearAllData];
    int medalType = PRESENTONDUTY_MEDAL;
    int medal = COPPER_MEDAL;
    int num = [self getMusicDeltaNumByAchieve:medalType medal:medal];
    assert(num == 2);
    
    NSLog(@"test_getMusicDeltaNumByAchieve ok");
}
- (void) test_setFansPoint
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    mViewNav.mSystemDataManager.finishedimagenum  = [NSNumber numberWithInt: 600];
    mViewNav.mSystemDataManager.sharepoint = [NSNumber numberWithInt:50];
    mViewNav.mSystemDataManager.finishedmusicnum = [NSNumber numberWithInt:50];
    mViewNav.mSystemDataManager.finishedcommentnum = [NSNumber numberWithInt:1];
}
- (void) testSuite
{
    BOOL ok = [SEUserDefaultManager isFunctionOK:USERINFO_FUNC];
    BOOL basic = [SEUserDefaultManager isFunctionOK:BASIC_FUNC];
    [SEUserDefaultManager setFunction:BASIC_FUNC value:YES];
    [SEUserDefaultManager setFunction:USERINFO_FUNC value:YES];
    [self test_AddExp];
    [self test_addOneToFinishedImage];
    [self test_levelUp];
    [self test_NewPersonAchieve];
    [self test_DrawingAchieve];
    [self test_ShareAchieve];
    [self test_addOneToNewPersonPoint];
    [self test_FansAchieve];
    [self test_getLevelPoint];
    [self test_getImageDeltaNumByAchieve];
    [self test_getMusicDeltaNumByAchieve];
    [self clearAllData];
    [mViewNav notificationHide];
    //[self test_setFansPoint];
    [SEUserDefaultManager setFunction:USERINFO_FUNC value:ok];
    [SEUserDefaultManager setFunction:BASIC_FUNC value:basic];
}
@end
