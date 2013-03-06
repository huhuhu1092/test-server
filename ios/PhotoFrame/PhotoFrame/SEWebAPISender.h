//
//  SEWebAPISender.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-10-13.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>

@protocol SEWebAPISenderDelegate <NSObject>

- (void) sendOK: (NSData*) recvData;
- (void) sendError: (NSError*)error;

@end
@interface SENameValue : NSObject
{
    NSString* name;
    NSString* value;
}
@property (nonatomic, retain) NSString* name;
@property (nonatomic, retain) NSString* value;
@end
@interface SEWebAPISender : NSObject
{
    NSMutableData* mRecvData;
    NSMutableArray* mPropertyArray;
    NSMutableArray* mHeaderArray;
    NSString* mUrl;
    NSString* mApi;
    NSObject <SEWebAPISenderDelegate>* mDelegate;
}
@property (nonatomic, retain) NSString* mUrl;
@property (nonatomic, retain) NSString* mApi;
@property (nonatomic, assign) NSObject <SEWebAPISenderDelegate>* mDelegate;
//must use sequence: createCommand, setProperty, setProperty, ..., sendCommandByPost
- (void) createCommand : (NSString*) urlName api:(NSString*)api;
- (void) setHeader: (NSString*) headerName value: (NSString*)value;
- (void) setProperty: (NSString*) name value: (NSString*)value;
- (void) sendCommandByPost;
@end
@class SEViewNavigator;
@interface SEGooglePlusAccessor : NSObject <SEWebAPISenderDelegate, UIWebViewDelegate>
{
    enum GOOGLEPLUS_STATE {GET_ACCESS_TOKEN, REFRESH_TOKEN, NO_GOOGLEPLUS_STATE};
    NSString* googlePlusClientID;
    NSString* googlePlusClientSecret;
    SEViewNavigator* mViewNav;
    SEWebAPISender* mWebSender;
    GOOGLEPLUS_STATE mCurrentState;
}
- (void) access;

@end
