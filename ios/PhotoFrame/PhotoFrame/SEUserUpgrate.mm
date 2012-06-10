//
//  SEUserUpgrate.mm
//  PhotoFrame
//
//  Created by 陈勇 on 12-3-22.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEUserUpgrate.h"
#import "SEViewNavigator.h"
#import "UserInfo.h"
static int gPresentOnDutyPoint[3] = {10, 20, 30};
static int gNewPersonPoint[3] = {50, 150, 300};
//static int gDrawingAccumuPoint[3] = {500, 1000, 8500};
static int gSharePoint[3] = {100, 400, 1500};
static const int DRAWING_ACCUMU_POINT1 = 500;
static const int DRAWING_ACCUMU_POINT2 = 1500;
static const int DRAWING_ACCUMU_POINT3 = 10000;

static const int FANS_DRAWING_ACCUMU_POINT1 = 100;
static const int FANS_DRAWING_ACCUMU_POINT2 = 500;
static const int FANS_DRAWING_ACCUMU_POINT3 = 2500;

static const int FANS_SHARE_ACCUMU_POINT1 = 10;
static const int FANS_SHARE_ACCUMU_POINT2 = 50;
static const int FANS_SHARE_ACCUMU_POINT3 =200;

static const int FANS_MUSIC_ACCUMU_POINT1 = 20;
static const int FANS_MUSIC_ACCUMU_POINT2 = 100;
static const int FANS_MUSIC_ACCUMU_POINT3 = 250;

static const int FANS_COMMENT_POINT1 = 1;

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
static int getFansLevel(int drawingPoint, int sharePoint, int musicPoint, int commentPoint)
{
    int count = sizeof(gFansData) / sizeof(FansPointData);
    for(int i = 0 ; i < count ; i++)
    {
        if(gFansData[i].drawingPoint == drawingPoint && 
           gFansData[i].sharePoint == sharePoint &&
           gFansData[i].musicPoint == musicPoint &&
           gFansData[i].commentPoint == commentPoint)
            return i;
    }
    return INVALID_MEDAL_LEVEL;
}
/////////////////////////////

@implementation SEUserData
@synthesize level;
@synthesize expNum;
@synthesize imageListNum;
@synthesize musicListNum;
@synthesize brushNum;
@synthesize effect;

@end
////
@interface SEUserUpgrade (Private)
- (NSString*)readDataFile:(NSString*)fileName;
- (void) parseString: (NSString*)str;
- (BOOL)whitespaceLine: (NSString*)line;
@end

//////////////////////////////////////
@implementation SEUserUpgrade (Private)

- (NSString*)readDataFile:(NSString*)fileName
{
    NSArray* fileNameArray = [fileName componentsSeparatedByString:@"."];
    if([fileNameArray count] != 2)
    {
        return NO;
    }
    NSString* s = [fileNameArray objectAtIndex:0];
    NSString* ext = [fileNameArray objectAtIndex:1];
    NSString* filePath = [[NSBundle mainBundle] pathForResource:s ofType:ext];
    NSString* dataContent = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
    return dataContent;
}
- (BOOL)whitespaceLine: (NSString*)line
{
    NSCharacterSet* cs = [NSCharacterSet whitespaceAndNewlineCharacterSet];
    NSUInteger i;
    for(i = 0 ; i < [line length]; i++)
    {
        unichar c = [line characterAtIndex:i];
        BOOL b = [cs characterIsMember:c];
        if(!b)
            return false;
    }
    return true;
}

- (void) parseString: (NSString*)dataContent
{
    [mUserMetaData release];
    mUserMetaData = [NSArray array];
    NSArray* dataLines = [dataContent componentsSeparatedByString:@"\n"];
    NSUInteger i;
    for(i = 0 ; i < [dataLines count] ; i++)
    {
        NSString* line = [dataLines objectAtIndex:i];
        if([self whitespaceLine:line])
            continue;
        NSCharacterSet* cs = [NSCharacterSet whitespaceAndNewlineCharacterSet];
        NSArray* tokenArray = [line componentsSeparatedByCharactersInSet:cs];
        assert(tokenArray.count == 8);
        NSString** tokens = (NSString**)malloc(sizeof(NSString*) * tokenArray.count);
        for(NSUInteger j = 0 ; j < [tokenArray count] ; j++)
        {
            NSString* s = [tokenArray objectAtIndex:j];
            NSString* tok = [s stringByTrimmingCharactersInSet:cs];
            tokens[j] = tok;
        }
        SEUserData* userData = [[SEUserData alloc] init];
        userData.level = [tokens[1] intValue];
        userData.expNum = [tokens[2] intValue];
        userData.imageListNum = [tokens[3] intValue];
        userData.brushNum = [tokens[5] intValue];
        userData.musicListNum = [tokens[6] intValue];
        userData.effect = [tokens[7] intValue];
        free(tokens);
        mUserMetaData = [mUserMetaData arrayByAddingObject:userData];
        [userData release];
    }
    [mUserMetaData retain];
}

@end
@implementation SEUserUpgrade

@synthesize mViewNav;
- (id)init
{
    self = [super init];
    if (self) 
    {
        // Initialization code here.
        NSString* dataContent = [self readDataFile:@"userinfometadata.txt"];
        if(dataContent)
        {
            [self parseString:dataContent];
        }
    }
    return self;
}
- (void)dealloc
{

    [mUserMetaData release];
    [super dealloc];
}
- (double) toDay: (NSTimeInterval) timeInterval
{
    return timeInterval / (24 * 60 * 60);
}
- (void)upgradePresentOnDuty : (UserInfo*)userInfo
{
    if([userInfo.presentondutymedal intValue] == INVALID_MEDAL_LEVEL)
        userInfo.presentondutymedal = [NSNumber numberWithInt: COPPER_MEDAL];
    else
        userInfo.presentondutymedal = [NSNumber numberWithInt:[userInfo.presentondutymedal intValue] + 1];
    userInfo.presentondutypoint = 0;
}
- (void) addOneForPresentOnDutyPoint: (UserInfo*)userInfo
{
    userInfo.presentondutypoint = [NSNumber numberWithInt: [userInfo.presentondutypoint intValue] + 1];
    
}
- (void) startApplication
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    if(userInfo.applaststartdate == nil)
    {
        userInfo.applaststartdate = [NSDate date];
    }
    else
    {
        NSDate* currentDate = [NSDate date];
        NSTimeInterval timeInterval = [currentDate timeIntervalSinceDate:userInfo.applaststartdate];
        double day = [self toDay:timeInterval];
        if(day >= 1 && day <= 2)
        {
            
            int medalPointIndex = GOLD_MEDAL + 1;
            if([userInfo.presentondutymedal intValue] == INVALID_MEDAL_LEVEL)
            {
                [self addOneForPresentOnDutyPoint: userInfo];
                medalPointIndex = COPPER_MEDAL;
            }
            else if([userInfo.presentondutymedal intValue] < GOLD_MEDAL && [userInfo.presentondutymedal intValue] >= COPPER_MEDAL)
            {
                [self addOneForPresentOnDutyPoint: userInfo];
                medalPointIndex = [userInfo.presentondutymedal intValue] + 1;
            }
            if(medalPointIndex <= GOLD_MEDAL && [userInfo.presentondutypoint intValue] == gPresentOnDutyPoint[medalPointIndex])
            {
                [self upgradePresentOnDuty: userInfo];
            }
        }
        else if(day > 2)
        {
            int times = (int)day;
            
            for(int i = 0 ; i < times ; i++)
            {
                userInfo.presentondutypoint = [NSNumber numberWithInt:[userInfo.presentondutypoint intValue] - 1];
                if([userInfo.presentondutypoint intValue] < 0)
                {
                    userInfo.presentondutypoint = [NSNumber numberWithInt:0];
                }
            }
        }
        userInfo.applaststartdate = currentDate;
    }
}

- (int) getSharePoint:(UserInfo*)userInfo
{
    int point = 0;
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
}
- (void) calculateFansPoint
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    int drawPoint = [userInfo.finishedimagenum intValue];
    int sharePoint = [self getSharePoint:userInfo];
    int musicPoint = [userInfo.finishedmusicnum intValue];
    int commentPoit = [userInfo.finishedcommentnum intValue] > 0;
    int level = getFansLevel(drawPoint, sharePoint, musicPoint, commentPoit);
    if(level != INVALID_MEDAL_LEVEL)
    {
        userInfo.fansmedal = [NSNumber numberWithInt:level];
    }
}
- (void) addOneForSharePoint: (UserInfo*)userInfo
{
    userInfo.sharepoint = [NSNumber numberWithInt:[userInfo.sharepoint intValue] + 1];
}
- (void) upgradeShareMedal: (UserInfo*)userInfo
{
    if([userInfo.sharemedal intValue] == INVALID_MEDAL_LEVEL)
    {
        userInfo.sharemedal = [NSNumber numberWithInt:COPPER_MEDAL];
    }
    else
    {
        userInfo.sharemedal = [NSNumber numberWithInt:[userInfo.sharemedal intValue] + 1];
    }
    userInfo.sharepoint = [NSNumber numberWithInt:0];
}
- (void) shareOneImage
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    int shareIndex = GOLD_MEDAL + 1;
    if([userInfo.sharemedal intValue] == INVALID_MEDAL_LEVEL)
    {
        shareIndex = COPPER_MEDAL;
        [self addOneForSharePoint:userInfo];
    }
    else if([userInfo.sharemedal intValue] >= COPPER_MEDAL &&
            [userInfo.sharemedal intValue] < GOLD_MEDAL)
    {
        shareIndex = [userInfo.sharemedal intValue] + 1;
        [self addOneForSharePoint:userInfo];
    }
    if(shareIndex <= GOLD_MEDAL && [userInfo.sharepoint intValue] == gSharePoint[shareIndex])
    {
        [self upgradeShareMedal: userInfo];
    }
    [self calculateFansPoint];
}
- (void) upgradeNewPersonMedal : (UserInfo*)userInfo;
{
    if([userInfo.newpersonmedal intValue] == INVALID_MEDAL_LEVEL)
    {
        userInfo.newpersonmedal = [NSNumber numberWithInt:COPPER_MEDAL];
    }
    else
    {
        userInfo.newpersonmedal = [NSNumber numberWithInt:[userInfo.newpersonmedal intValue] + 1];
    }
    userInfo.newpersonpoint = [NSNumber numberWithInt:0];
}
-(void) addOneToFinishedImage:(UserInfo*)userInfo
{
    userInfo.finishedimagenum = [NSNumber numberWithInt:[userInfo.finishedimagenum intValue] + 1];
    
}
- (void) addOneToNewPersonPoint: (UserInfo*)userInfo
{
    userInfo.newpersonpoint = [NSNumber numberWithInt:[userInfo.newpersonpoint intValue] + 1];
}
- (void) finishOneImageDrawing
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    if(userInfo.lastfinishoneimagetime == nil)
    {
        userInfo.lastfinishoneimagetime = [NSDate date];
        [self addOneToFinishedImage:userInfo];
        [self addOneToNewPersonPoint:userInfo];
    }
    else
    {
        [self addOneToFinishedImage:userInfo];
        [self addOneToNewPersonPoint:userInfo];
        NSDate* currentDate = [NSDate date];
        NSTimeInterval timeInterval = [currentDate timeIntervalSinceDate:userInfo.lastfinishoneimagetime];
        double day = [self toDay:timeInterval];
        if(day <= 1)
        {
            int newPersonIndex = GOLD_MEDAL + 1;
            if([userInfo.newpersonmedal intValue] == INVALID_MEDAL_LEVEL)
            {
                newPersonIndex = COPPER_MEDAL;
            }
            else if([userInfo.newpersonmedal intValue] >= COPPER_MEDAL && [userInfo.newpersonmedal intValue] < GOLD_MEDAL)
            {
                newPersonIndex = [userInfo.newpersonmedal intValue] + 1;
            }
            if(newPersonIndex <= GOLD_MEDAL && [userInfo.newpersonpoint intValue] == gNewPersonPoint[newPersonIndex])
            {
                [self upgradeNewPersonMedal: userInfo];
            }
        }
        else
        {
            userInfo.newpersonpoint = [NSNumber numberWithInt:0];
        }
        userInfo.lastfinishoneimagetime = currentDate;
    }
    if([userInfo.finishedimagenum intValue] == DRAWING_ACCUMU_POINT1)
    {
        userInfo.drawingmedal = [NSNumber numberWithInt:COPPER_MEDAL];
    }
    else if([userInfo.finishedimagenum intValue] == DRAWING_ACCUMU_POINT2)
    {
        userInfo.drawingmedal = [NSNumber numberWithInt:SILVER_MEDAL];
    }
    else if([userInfo.finishedimagenum intValue] == DRAWING_ACCUMU_POINT3)
    {
        userInfo.drawingmedal = [NSNumber numberWithInt:GOLD_MEDAL];
    }
    [self calculateFansPoint];
}
- (void) finishOneMusicPlay
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    int musicPlayNum = [userInfo.finishedmusicnum intValue];
    if(musicPlayNum <= 250)
    {
        userInfo.finishedmusicnum = [NSNumber numberWithInt:[userInfo.finishedmusicnum intValue] + 1];
        [self calculateFansPoint];
    }
}
- (void) finishOneComment
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    userInfo.finishedcommentnum = [NSNumber numberWithInt:[userInfo.finishedcommentnum intValue] + 1];
    [self calculateFansPoint];
}
- (SEUserData*) getUserData: (int)level
{
    for(SEUserData* userData in mUserMetaData)
    {
        if(userData.level == level)
            return userData;
    }
    return nil;
}
@end
