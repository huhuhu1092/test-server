//
//  SEDataUploadManager.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-5-18.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "SEProtocalDefine.h"
enum SE_DATA_UPLOAD_TYPE {WEIBO_TYPE, HTTP_TYPE};
enum SE_DATA_UPLOAD_STATE {NO_UPLOAD, UPLOADING, UPLOAD_OK, UPLOAD_FAILED, PENDDING};
@class SEViewNavigator;
@class SEDataUploadManager;
@interface SEDataForUpload : NSObject
{
    int mReuploadTimes;
    int mCurrentReuploadTimes;
}
@property (nonatomic, assign) int mCurrentReuploadTimes;
@property (nonatomic, assign) int mReuploadTimes;
- (BOOL) doUpload;
- (void) destroy;
- (void) saveToCoreData;
- (void) wrapperOfUpload;
- (BOOL) isEqualToData: (SEDataForUpload*) data;
@end
@interface SEWeiboData : SEDataForUpload
{
    NSData* data;
    
}
@property (nonatomic, retain) NSData* data;
@end
//////
@interface SEIssueReportData : SEDataForUpload
{
    NSString* title;
    NSString* description;
    NSTimeInterval dateInterval;
    NSString* deviceName;
    NSObject<SEIssueReportDelegate>* delegate;
@private
    NSMutableData* mRecvData;
    int statusCode;
    SEViewNavigator* mViewNav;
    
}
@property (nonatomic, retain) NSString* title;
@property (nonatomic, retain) NSString* description;
@property (nonatomic, retain) NSString* deviceName;
@property (nonatomic, assign) NSTimeInterval dateInterval;
@property (nonatomic, retain) NSObject<SEIssueReportDelegate>* delegate;
@property (nonatomic, assign) SEViewNavigator* mViewNav;
@end
@interface SEDataUploadManager : NSObject
{
    NSMutableArray* mDataQueue;
    enum SE_DATA_UPLOAD_STATE mCurrentUploadState;
}
@property (nonatomic, readonly) enum SE_DATA_UPLOAD_STATE mCurrentUploadState;
- (void) upload: (SEDataForUpload*)data;
//- (void) sendOneComment: (NSString*)text;
@end

@interface SECommentSender : NSObject
{
    NSString* mContent;
    id mFinishedTarget;
    SEL mFinishedAction;
    NSMutableData* mRecvData;
    int mStatusCode;
    UILabel* mOutTextLabel;
}
- (id) initWithComment: (NSString*)text label: (UILabel*)label;
- (void) sendWith: (id) target finishedAction: (SEL)action;
- (void) setOutputText: (NSString*) text;
@end
