//
//  SEDataUploadManager.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-5-18.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEDataUploadManager.h"
#import "SEUtil.h"
#import "SEViewNavigator.h"
//////////
@interface SEDataUploadManager (Private)
- (void) setCurrentState: (enum SE_DATA_UPLOAD_STATE) state;
- (void) justChangeState: (enum SE_DATA_UPLOAD_STATE) state;
@end
@implementation SEDataUploadManager (Private)
- (void) justChangeState:(enum SE_DATA_UPLOAD_STATE)state
{
    mCurrentUploadState = state;
}
- (BOOL) uploadRestData
{
    BOOL ret = NO;
    while(mDataQueue.count > 0 && ret == NO)
    {
        SEDataForUpload* first = [mDataQueue objectAtIndex:0];
        ret = [first doUpload];
        if(ret == NO)
        {
            [mDataQueue removeObject:first];
        }
    }
    return ret;
}
- (void) setCurrentState: (enum SE_DATA_UPLOAD_STATE) state
{
    if(mDataQueue.count == 0)
    {
        mCurrentUploadState = NO_UPLOAD;
        return;
    }
    if(mCurrentUploadState == state)
        return;
    if(mCurrentUploadState == UPLOADING && state == UPLOAD_OK)
    {
        SEDataForUpload* first = [mDataQueue objectAtIndex:0];
        [mDataQueue removeObject:first];
        BOOL ret = [self uploadRestData];
        if(ret == YES)
        {
            mCurrentUploadState = UPLOADING; 
        }
        else {
            mCurrentUploadState = NO_UPLOAD;
        }
    }
    else if(mCurrentUploadState == UPLOADING && state == UPLOAD_FAILED)
    {
        SEDataForUpload* first = [mDataQueue objectAtIndex:0];
        if(first.mCurrentReuploadTimes < first.mReuploadTimes)
        {
            first.mCurrentReuploadTimes += 1;
            [first performSelector:@selector(wrapperOfUpload) withObject:nil afterDelay:5];
            mCurrentUploadState = PENDDING;
        }
        else 
        {
            NSLog(@" save first to core data ");
            [first saveToCoreData];
            [mDataQueue removeObject:first];
            BOOL ret = [self uploadRestData];
            if(ret == YES)
            {
                mCurrentUploadState = UPLOADING;
            }
            else {
                mCurrentUploadState = NO_UPLOAD;
            }
        }
    }
}


@end
/////////////////////////////////
@implementation SEDataForUpload
@synthesize mCurrentReuploadTimes;
@synthesize mReuploadTimes;
- (id) init
{
    self = [super init];
    if(self)
    {
        mReuploadTimes = 3;
    }
    return self;
}
- (BOOL) doUpload
{
    return NO;
}
- (BOOL) isEqualToData: (SEDataForUpload*) data
{
    return NO;
}
- (void) destroy
{}
- (void) saveToCoreData
{}
- (void) wrapperOfUpload
{
    
}
@end

@implementation SEIssueReportData
@synthesize title;
@synthesize description;
@synthesize deviceName;
@synthesize dateInterval;
@synthesize delegate;
@synthesize mViewNav;
- (void)dealloc
{
    [title release];
    [description release];
    [deviceName release];
    [delegate release];
    [mRecvData release];
    [super dealloc];
}
- (id) init
{
    self = [super init];
    if(self)
    {
        mRecvData = [NSMutableData data];
        [mRecvData retain];
    }
    return self;
}
- (BOOL) isEqualToData:(SEDataForUpload *)data
{
    Class selfClass = [self class];
    Class dataClass = [data class];
    if(selfClass != dataClass)
        return NO;
    SEIssueReportData* realData = (SEIssueReportData*)data;
    if([realData.title isEqualToString:self.title] && [realData.description isEqualToString:self.description])
        return YES;
    else {
        return NO;
    }
}
- (void)saveToCoreData
{
    [mViewNav saveIssueReport:deviceName :dateInterval :title :description];
}
- (void) wrapperOfUpload
{
    SEDataUploadManager* dataUploadManager = [mViewNav getDataUploadManager];
    BOOL ret = [self doUpload];
    if(ret == YES)
    {
        [dataUploadManager justChangeState:UPLOADING];
    }
    else 
    {
        [dataUploadManager justChangeState:NO_UPLOAD];    
    }
}
- (BOOL)doUpload
{
    if([SEUtil reachabilityWithLocalWifi] == NO || [SEUtil reachabilityForHostName:@"http://mobilefly.sinaapp.com"] == NO)
    {
        [delegate setStatusMessage: @"error: wifi network is not ready"];
        return NO;
    }
    NSString* strURL = @"http://mobilefly.sinaapp.com/sendmessage.php";
    NSURL* url = [NSURL URLWithString:strURL];
    NSMutableURLRequest* req = [NSMutableURLRequest requestWithURL:url];
    [req setHTTPMethod:@"POST"];
    //NSString* contentType = @"application/x-www-form-urlencoded";
    //[req setValue:contentType forHTTPHeaderField:@"Content-Type"];
    NSMutableData* postBody = [NSMutableData data];
    NSDate* date = [NSDate date];
    NSTimeInterval timeInterv = [date timeIntervalSince1970];
    [postBody appendData:[[NSString stringWithFormat:@"devname=%@&", deviceName] dataUsingEncoding:NSUTF8StringEncoding]];
    [postBody appendData:[[NSString stringWithFormat:@"date=%f&", timeInterv] dataUsingEncoding:NSUTF8StringEncoding]];
    [postBody appendData:[[NSString stringWithFormat:@"title=%@&", title] dataUsingEncoding:NSUTF8StringEncoding]];
    [postBody appendData:[[NSString stringWithFormat:@"description=%@", description] dataUsingEncoding:NSUTF8StringEncoding]];
    [req setHTTPBody:postBody];
    //NSHTTPURLResponse* response = nil;
    //NSError* error = nil;
    //NSData* returnData = [NSURLConnection sendSynchronousRequest:req returningResponse:&response error:&error];
    NSURLConnection* conn = [NSURLConnection connectionWithRequest:req delegate:self];
    [conn retain];
    return YES;
    //int statusCode = response.statusCode;
    //NSString* str = [[[NSString alloc] initWithData:returnData encoding:NSUTF8StringEncoding] autorelease];
    //NSLog(@"return str = %@", str);
    /*
    if([str isEqualToString:@"OK"] && statusCode == 200 && error == nil)
    {
        outputLabel.text =  @"send message ok";
    }
    else 
    {
        outputLabel.text = @"send message failed";
    }
*/
}
- (void)connection:(NSURLConnection *)connection didReceiveResponse:(NSURLResponse *)response
{
    NSLog(@"start response");
    NSThread* currentThread = [NSThread currentThread];
    NSThread* mainThread = [NSThread mainThread];
    assert(currentThread == mainThread);
    if([response isMemberOfClass:[NSHTTPURLResponse class]])
    {
        statusCode = ((NSHTTPURLResponse*)response).statusCode;
    }
}
- (void)connection:(NSURLConnection *)connection didReceiveData:(NSData *)data
{
    NSLog(@"receive data");
    NSThread* currentThread = [NSThread currentThread];
    NSThread* mainThread = [NSThread mainThread];
    assert(currentThread == mainThread);
    [mRecvData appendData:data];
}
- (void)connection:(NSURLConnection *)connection didFailWithError:(NSError *)error
{
    NSLog(@"error response");
    [connection release];
    NSThread* currentThread = [NSThread currentThread];
    NSThread* mainThread = [NSThread mainThread];
    assert(currentThread == mainThread);
    SEDataUploadManager* dataUploadManager = [mViewNav getDataUploadManager];
    [dataUploadManager setCurrentState:UPLOAD_FAILED];
}
- (void)connectionDidFinishLoading:(NSURLConnection *)connection
{
    NSLog(@"finished connection\n");
    NSThread* currentThread = [NSThread currentThread];
    NSThread* mainThread = [NSThread mainThread];
    assert(currentThread == mainThread);
    int len = [mRecvData length];
    const char* bytes = (const char*)[mRecvData bytes];
    char* data = (char*)malloc(len + 1);
    memset(data, 0, len + 1);
    memcpy(data, bytes, len);
    NSString* str = [NSString stringWithCString:data encoding:NSUTF8StringEncoding];
    NSLog(@"%@", str);
    [connection release];
    free(data);
    if([str isEqualToString:@"OK"] && statusCode == 200)
    {
        [delegate setStatusMessage:@"send message ok"];
    }
    else
    {
        [delegate setStatusMessage:@"send message failed"];
    }
    SEDataUploadManager* dataUploadManager = [mViewNav getDataUploadManager];
    [dataUploadManager setCurrentState:UPLOAD_OK];
}  

@end
//////////////////////////////////
@implementation SEDataUploadManager
@synthesize mCurrentUploadState;
- (void) dealloc
{
    [mDataQueue release];
    [super dealloc];
}
- (id) init
{
    self = [super init];
    if(self)
    {
        mDataQueue = [NSMutableArray array];
        [mDataQueue retain];
    }
    return self;
}
- (BOOL) isDataExist: (SEDataForUpload*)data
{
    for(SEDataForUpload* d in mDataQueue)
    {
        if([d isEqualToData:data])
            return YES;
    }
    return NO;
}
- (void) upload:(SEDataForUpload *)data
{
    if([self isDataExist:data] == NO)
    {
        [mDataQueue addObject:data];
        assert(mCurrentUploadState != UPLOAD_OK && mCurrentUploadState != UPLOAD_FAILED);
        if(mCurrentUploadState == NO_UPLOAD)
        {
            BOOL ret = [data doUpload];
            if(ret == NO)
            {
                [mDataQueue removeObject:data];
            }
            else 
            {
                mCurrentUploadState = UPLOADING;
            }
        }
    }
    else {
        NSLog(@"## send message has been in sending queue ##");
    }
}
@end
