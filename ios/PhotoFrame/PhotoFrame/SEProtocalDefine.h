//
//  SEProtocalDefine.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-2-16.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
@class SEContentViewContainer;
@protocol SEAdjustContentView <NSObject>

- (BOOL) canAdjust;
- (void) relayout;
- (void) update;
@end

@protocol SENextImageDisplay <NSObject>

- (void) displayNextImage;

@end

@protocol SEContainerAnimationHandler <NSObject>

- (void) handleAnimEnd: (BOOL) stopInMid withDirection: (int) direct leftContainer: (SEContentViewContainer*)leftContainer rightContainer: (SEContentViewContainer*)rightContianer;

@end
/*
@protocol SESaveImage <NSObject>

- (void) saveImageToCoreData: (UIImage*)uiImage urlName:(NSString*)url urlDate: (NSDate*)date index:(int)index;
- (void) saveImageThumbnailToCoreData:(UIImage*)uiImage urlName:(NSString*)url urlDate: (NSDate*)date index: (int)index;
- (void) saveImageAndThumbnailToCoreData:(UIImage*)uiImage urlName:(NSString*)url urlDate: (NSDate*)date index: (int)index;;
@end
 */
////
@protocol SELoadedImageHandler <NSObject>
- (void) handleImage; //run in main thread
- (void) setImage: (UIImage*)image; //run in thread which load image
- (void) preHandleImage; // run in thread which load image
@end
//this protocol is used when you want to share you image with weibo, twitter or facebook, etc.
@protocol SEShareImage <NSObject>
- (void) share;
- (void) addImage: (UIImage*) image;
- (void) removeImage: (UIImage*)image;
@end

@protocol SEIssueReportDelegate <NSObject>

- (void) setStatusMessage: (NSString*) msg;

@end
//////
