//
//  SEImageShareManager.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-5-15.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEImageShareManager.h"
#import "OAuthEngine.h"
#import "WeiboClient.h"
#import "SEViewNavigator.h"
#define kOAuthConsumerKey	@"440606512"
#define kOAuthConsumerSecret	@"72dfe0879a2e730ecc36574a4fefd467"


@implementation SEImageShareManager
- (id) init
{
    self = [super init];
    if(self)
    {
        mShareImageArray = [NSMutableArray array];
        mShareImageArray = [mShareImageArray retain];
    }
    return self;
}
- (void) addImage:(UIImage *)image
{
    [mShareImageArray addObject:image];
}
- (void) share
{}
- (void)removeImage:(UIImage *)image
{
    [mShareImageArray removeObject:image];
}
- (void) dealloc
{
    [mShareImageArray release];
    [super dealloc];
}
@end

@implementation SEWeiboClientData
@synthesize weiboClient;
@synthesize statuses;
-(void) dealloc
{
    [weiboClient release];
    [statuses release];
    [super dealloc];
}
+ (SEWeiboClientData*)createWeiboClient:(id)target action:(SEL)action engine:(OAuthEngine *)engine
{
    WeiboClient* weiboClient = [[WeiboClient alloc] initWithTarget:target 
											   engine:engine
											   action:action];
	[weiboClient getFollowedTimelineSinceID:0 
							 startingAtPage:0 count:100];
    SEWeiboClientData* data = [[SEWeiboClientData alloc] init];
    data->weiboClient = weiboClient;
    data->statuses = [[NSMutableArray array] retain];
    [data autorelease];
    return data;
}

@end

@implementation SEWeiboImageShare
@synthesize mViewNav;
- (id) init
{
    self = [super init];
    if(self)
    {
        mWeiboEngine = [[OAuthEngine alloc] initOAuthWithDelegate: self];
		mWeiboEngine.consumerKey = kOAuthConsumerKey;
		mWeiboEngine.consumerSecret = kOAuthConsumerSecret;
        
        mWeiboClientArray = [NSMutableArray array];
        mWeiboClientArray = [mWeiboClientArray retain];
        mRecvData = [NSMutableData data];
        [mRecvData retain];
    }
    return self;
}
- (void)dealloc
{
    [mWeiboEngine release];
    [mWeiboClientArray release];
    [mRecvData release];
    [super dealloc];
}
#pragma mark OAuthEngineDelegate
- (void) storeCachedOAuthData: (NSString *) data forUsername: (NSString *) username {
	NSUserDefaults			*defaults = [NSUserDefaults standardUserDefaults];
	
	[defaults setObject: data forKey: @"authData"];
	[defaults synchronize];
}

- (NSString *) cachedOAuthDataForUsername: (NSString *) username {
	return [[NSUserDefaults standardUserDefaults] objectForKey: @"authData"];
}

- (void)removeCachedOAuthDataForUsername:(NSString *) username{
	NSUserDefaults			*defaults = [NSUserDefaults standardUserDefaults];
	
	[defaults removeObjectForKey: @"authData"];
	[defaults synchronize];
}
//=============================================================================================================================
#pragma mark OAuthSinaWeiboControllerDelegate
- (void) OAuthController: (OAuthController *) controller authenticatedWithUsername: (NSString *) username {
	NSLog(@"Authenicated for %@", username);
	[self share];
}

- (void) OAuthControllerFailed: (OAuthController *) controller {
	NSLog(@"Authentication Failed!");
    if (controller) 
		[mViewNav presentModalViewController: controller animated: YES];
	
}

- (void) OAuthControllerCanceled: (OAuthController *) controller {
	NSLog(@"Authentication Canceled.");
	
}
- (void)openAuthenticateView {
	[self removeCachedOAuthDataForUsername:mWeiboEngine.username];
	[mWeiboEngine signOut];
	UIViewController *controller = [OAuthController controllerToEnterCredentialsWithEngine: mWeiboEngine delegate: self];
	
	if (controller) 
		[mViewNav presentModalViewController: controller animated: YES];
}
- (void) resetWeiboClient: (WeiboClient*)sender
{
    SEWeiboClientData* deleteObj = nil;
    for(SEWeiboClientData* weiboClientData in mWeiboClientArray)
    {
        if(weiboClientData.weiboClient == sender)
        {
            weiboClientData.weiboClient = nil;
            deleteObj = weiboClientData;
            break;
        }
    }
    [mWeiboClientArray removeObject:deleteObj];
}
- (void) setWeiboClientStatus:(WeiboClient*)sender statues: (NSArray*)ary
{
    NSMutableArray* statuses = [NSMutableArray array];
    for (int i = [ary count] - 1; i >= 0; --i)
    {
		NSDictionary *dic = (NSDictionary*)[ary objectAtIndex:i];
		if (![dic isKindOfClass:[NSDictionary class]]) {
			continue;
		}
		Status* sts = [Status statusWithJsonDictionary:[ary objectAtIndex:i]];
		[statuses insertObject:sts atIndex:0];
	}	
    for(SEWeiboClientData* weiboClientData in mWeiboClientArray)
    {
        if(weiboClientData.weiboClient == sender)
        {
            weiboClientData.statuses = statuses;
        }
    }
}
- (void)timelineDidReceive:(WeiboClient*)sender obj:(NSObject*)obj
{
	NSLog(@"begin timelineDidReceive");
    if (sender.hasError) {
		NSLog(@"timelineDidReceive error!!!, errorMessage:%@, errordetail:%@"
			  , sender.errorMessage, sender.errorDetail);
		[sender alert];
        if (sender.statusCode == 401) {
            [self openAuthenticateView];
        }
    }
    if (obj == nil || ![obj isKindOfClass:[NSArray class]]) 
    {
        //[self resetWeiboClient: sender];
        return;
    }
	NSArray *ary = (NSArray*)obj;  
	//[self setWeiboClientStatus:sender statues: ary];
	//[self resetWeiboClient: sender];
}

- (void) shareWithWeiboClient
{
    for(UIImage* image in mShareImageArray)
    {
        WeiboClient* client = [[WeiboClient alloc] initWithTarget:self engine:[OAuthEngine currentOAuthEngine] action:@selector(timelineDidReceive:obj:)];
        NSData* data = UIImageJPEGRepresentation(image, 1.0);
        data = [data retain];
        NSString* s = @"aatest";
        s = [s retain];
        [client upload:data status:s];
    }
}

- (void)share
{

    UIViewController *controller = [OAuthController controllerToEnterCredentialsWithEngine: mWeiboEngine delegate: self];
    
    if (controller) 
        [mViewNav presentModalViewController: controller animated: YES];
    else {
        NSLog(@"Authenicated for %@..", mWeiboEngine.username);
        [OAuthEngine setCurrentOAuthEngine:mWeiboEngine];
        [self shareWithWeiboClient];
    }
}
@end