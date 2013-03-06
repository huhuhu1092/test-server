//
//  SESystemDataManager.m
//  PhotoFrame
//
//  Created by 陈勇 on 13-2-21.
//
//

#import "SESystemDataManager.h"
#import "SEViewNavigator.h"
#import "PhotoFrameAppDelegate.h"
#import "SEKeyChainHelper.h"
#import "UserInfo.h"
#define SET_DEFAULT_VALUE(userInfo, key, v) { \
    NSData* data = [self createDefaultData:v]; \
    [userInfo setValue:data forKey:key]; \
}
#define SET_INT_VALUE(prop,v) { \
    int n = [v intValue]; \
    NSData* data = [SEKeyChainHelper createMyStringFromIntValue:n]; \
    UserInfo* userInfo = [self getUserInfo]; \
    userInfo.prop = data; \
}
#define GET_INT_VALUE(prop) {\
    UserInfo* userInfo = [self getUserInfo]; \
    NSData* data = userInfo.prop; \
    int v = 0; \
    BOOL ret = [SEKeyChainHelper getIntValueFromString:data : &v]; \
    NSNumber* outV = nil; \
    if(ret) \
    {  \
        outV = [NSNumber numberWithInt: v]; \
    } \
    else \
    { \
        outV = nil; \
    } \
    return outV;\
}
@implementation SESystemDataManager
@dynamic brushdensity;
@dynamic  brushedgedetect;
@dynamic  currentangle;
@dynamic  currentbrushid;
@dynamic  currentbrushtransparent;
@dynamic  currentimagequality;
@dynamic  currentimagetimes;
@dynamic  drawingmedal;
@dynamic  drawingpoint;
@dynamic  exppointnum;
@dynamic  fansmedal;
@dynamic  fanspoint;
@dynamic  finishedcommentnum;
@dynamic  finishedimagenum;
@dynamic  finishedmusicnum;
@dynamic  level;
@dynamic  newpersonmedal;
@dynamic  newpersonpoint;
@dynamic  presentondutymedal;
@dynamic  presentondutypoint;
@dynamic  shareimagenum;
@dynamic  sharemedal;
@dynamic  sharepoint;
@dynamic  timetextstyle;
- (UserInfo*) getUserInfo
{
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    return [viewNav getUserInfo];
}

- (NSNumber*) brushdensity
{
    GET_INT_VALUE(brushdensity);
    /*
    UserInfo* userInfo = [self getUserInfo];
    NSData* data = userInfo.brushdensity;
    int v = 0;
    BOOL ret = [SEKeyChainHelper getIntValueFromString:data : &v];
    if(ret)
    {
        return [NSNumber numberWithInt: v];
    }
    else
        return nil;
     */
}
- (void) setBrushdensity:(NSNumber*) v
{
    SET_INT_VALUE(brushdensity, v);
    /*
    int n = [v intValue];
    NSData* data = [SEKeyChainHelper createMyStringFromIntValue:n];
    UserInfo* userInfo = [self getUserInfo];
    userInfo.brushdensity = data;
     */
}
- (void) setBrushedgedetect: (NSNumber*)v
{
    SET_INT_VALUE(brushedgedetect, v);
}
- (NSNumber*) brushedgedetect
{
    
    GET_INT_VALUE(brushedgedetect);
}
- (void)  setCurrentangle: (NSNumber*)v
{
    SET_INT_VALUE(currentangle, v);
}
- (NSNumber*) currentangle
{
    GET_INT_VALUE(currentangle);
}
- (void)  setCurrentbrushid: (NSNumber*)v
{
    SET_INT_VALUE(currentbrushid, v);
}
- (NSNumber*) currentbrushid
{
    GET_INT_VALUE(currentbrushid);
}
- (void)  setCurrentbrushtransparent: (NSNumber*)v
{
    SET_INT_VALUE(currentbrushtransparent, v);
}
- (NSNumber*) currentbrushtransparent
{
    GET_INT_VALUE(currentbrushtransparent);
}
- (void)  setCurrentimagequality: (NSNumber*)v
{
    SET_INT_VALUE(currentimagequality, v);
}
- (NSNumber*) currentimagequality
{
    GET_INT_VALUE(currentimagequality);
}
- (void)  setCurrentimagetimes : (NSNumber*)v
{
    SET_INT_VALUE(currentimagetimes, v);
}
- (NSNumber*) currentimagetimes
{
    GET_INT_VALUE(currentimagetimes);
}
/*
- (void)  setDrawimagetime: (NSNumber*)v
{
    SET_INT_VALUE(drawimagetime, v);
}
- (NSNumber*) getDrawimagetime
{
    GET_INT_VALUE(drawimagetime);
}
 */
- (void)  setDrawingmedal: (NSNumber*)v
{
    SET_INT_VALUE(drawingmedal, v);
}
- (NSNumber*) drawingmedal
{
    GET_INT_VALUE(drawingmedal);
}
- (void) setDrawingpoint: (NSNumber*)v
{
    SET_INT_VALUE(drawingpoint, v);
}
- (NSNumber*) drawingpoint
{
    GET_INT_VALUE(drawingpoint);
}
- (void)  setExppointnum: (NSNumber*)v
{
    SET_INT_VALUE(exppointnum, v);
}
- (NSNumber*) exppointnum
{
    GET_INT_VALUE(exppointnum);
}
- (void)  setFansmedal: (NSNumber*)v
{
    SET_INT_VALUE(fansmedal, v);
}
- (NSNumber*) fansmedal
{
    GET_INT_VALUE(fansmedal);
}
- (void)  setFanspoint: (NSNumber*)v
{
    SET_INT_VALUE(fanspoint, v);
}
- (NSNumber*) fanspoint
{
    GET_INT_VALUE(fanspoint);
}
- (void)  setFinishedcommentnum: (NSNumber*)v
{
    SET_INT_VALUE(finishedcommentnum, v);
}
- (NSNumber*) finishedcommentnum
{
    GET_INT_VALUE(finishedcommentnum);
}
- (void)  setFinishedimagenum: (NSNumber*)v
{
    SET_INT_VALUE(finishedimagenum, v);
}
- (NSNumber*) finishedimagenum
{
    GET_INT_VALUE(finishedimagenum);
}
- (void)  setFinishedmusicnum: (NSNumber*)v
{
    SET_INT_VALUE(finishedmusicnum, v);
}
- (NSNumber*) finishedmusicnum
{
    GET_INT_VALUE(finishedmusicnum);
}
- (void)  setLevel: (NSNumber*)v
{
    SET_INT_VALUE(level, v);
}
- (NSNumber*) level
{
    GET_INT_VALUE(level);
}
- (void)  setNewpersonmedal: (NSNumber*)v
{
    SET_INT_VALUE(newpersonmedal, v);
}
- (NSNumber*) newpersonmedal
{
    GET_INT_VALUE(newpersonmedal);
}
- (void)  setNewpersonpoint: (NSNumber*)v
{
    SET_INT_VALUE(newpersonpoint, v);
}
- (NSNumber*) newpersonpoint
{
    GET_INT_VALUE(newpersonpoint);
}
- (void)  setPresentondutymedal: (NSNumber*)v
{
    SET_INT_VALUE(presentondutymedal, v);
}
- (NSNumber*) presentondutymedal
{
    GET_INT_VALUE(presentondutymedal);
}
- (void)  setPresentondutypoint: (NSNumber*)v
{
    SET_INT_VALUE(presentondutypoint, v);
}
- (NSNumber*) presentondutypoint
{
    GET_INT_VALUE(presentondutypoint);
}
- (void)  setShareimagenum: (NSNumber*)v
{
    SET_INT_VALUE(shareimagenum, v);
}
- (NSNumber*) shareimagenum
{
    GET_INT_VALUE(shareimagenum);
}
- (void)  setSharemedal: (NSNumber*)v
{
    SET_INT_VALUE(sharemedal, v);
}
- (NSNumber*) sharemedal
{
    GET_INT_VALUE(sharemedal);
}
- (void)  setSharepoint: (NSNumber*)v
{
    SET_INT_VALUE(sharepoint, v);
}
- (NSNumber*) sharepoint
{
    GET_INT_VALUE(sharepoint);
}
- (void)  setTimetextstyle: (NSNumber*)v
{
    SET_INT_VALUE(timetextstyle, v);
}
- (NSNumber*) timetextstyle
{
    GET_INT_VALUE(timetextstyle);
}

- (NSData*) createDefaultData: (int)v
{
    NSData* data = [SEKeyChainHelper createMyStringFromIntValue:v];
    return data;
}
- (void) initData : (NSManagedObject*) userInfoObject
{
    SET_DEFAULT_VALUE(userInfoObject,  @"brushdensity", 6) ;
    SET_DEFAULT_VALUE(userInfoObject,  @"brushedgedetect", 2);
    SET_DEFAULT_VALUE(userInfoObject,  @"currentangle", 21);
    SET_DEFAULT_VALUE(userInfoObject,  @"currentbrushid", 1);
    SET_DEFAULT_VALUE(userInfoObject,  @"currentbrushtransparent", 1);
    SET_DEFAULT_VALUE(userInfoObject,  @"drawingmedal", 0);
    SET_DEFAULT_VALUE(userInfoObject,  @"drawingpoint", 0);
    SET_DEFAULT_VALUE(userInfoObject,  @"exppointnum", 0);
    SET_DEFAULT_VALUE(userInfoObject,  @"fansmedal", 0);
    SET_DEFAULT_VALUE(userInfoObject,  @"fanspoint", 0);
    SET_DEFAULT_VALUE(userInfoObject,  @"finishedcommentnum", 0);
    SET_DEFAULT_VALUE(userInfoObject,  @"finishedimagenum", 0);
    SET_DEFAULT_VALUE(userInfoObject,  @"finishedmusicnum", 0);
    SET_DEFAULT_VALUE(userInfoObject,  @"newpersonmedal", 0);
    SET_DEFAULT_VALUE(userInfoObject,  @"newpersonpoint", 0);
    SET_DEFAULT_VALUE(userInfoObject,  @"presentondutymedal", 0);
    SET_DEFAULT_VALUE(userInfoObject,  @"presentondutypoint", 0);
    SET_DEFAULT_VALUE(userInfoObject,  @"shareimagenum", 0);
    SET_DEFAULT_VALUE(userInfoObject,  @"sharemedal", 0);
    SET_DEFAULT_VALUE(userInfoObject,  @"sharepoint", 0);
    SET_DEFAULT_VALUE(userInfoObject,  @"timetextstyle", 1);
}
@end
