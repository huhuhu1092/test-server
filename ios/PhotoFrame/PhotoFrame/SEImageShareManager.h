//
//  SEImageShareManager.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-5-15.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "OAuthController.h"
#import "SEProtocalDefine.h"
@class OAuthEngine;
@class WeiboClient;
@class SEViewNavigator;
@interface SEImageShareManager : NSObject<SEShareImage>
{
@protected
    NSMutableArray* mShareImageArray;
}
@end
@interface SEWeiboClientData : NSObject
{
    WeiboClient* weiboClient;
    NSMutableArray* statuses;
}
@property (nonatomic, assign) WeiboClient* weiboClient;
@property (nonatomic, retain) NSMutableArray* statuses;
+ (SEWeiboClientData*) createWeiboClient: (id)target action:(SEL)action engine:(OAuthEngine*)engine;
@end
@interface SEWeiboImageShare : SEImageShareManager<OAuthControllerDelegate>
{
    OAuthEngine				*mWeiboEngine;
	NSMutableArray* mWeiboClientArray;//array of SEWeiboClientData
    SEViewNavigator* mViewNav;
    NSMutableData* mRecvData;
}
@property (nonatomic, assign) SEViewNavigator* mViewNav;
@end
