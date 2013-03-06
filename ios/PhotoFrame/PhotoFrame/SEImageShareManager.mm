//
//  SEImageShareManager.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-5-15.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEImageShareManager.h"
//#import "OAuthEngine.h"
//#import "WeiboClient.h"
#import "SEViewNavigator.h"
#import "SA_OAuthTwitterEngine.h"

//#define kOAuthConsumerKey	@"440606512"
//#define kOAuthConsumerSecret	@"72dfe0879a2e730ecc36574a4fefd467"

#define kOAuthConsumerKey				@"3oBm0t8FFMca3mjl6EQQuQ"		//REPLACE ME
#define kOAuthConsumerSecret			@"L1ge1p2p8U7RkBTJGTGZEDr9hrn8Kd1C5l9XBbLWq"		//REPLACE ME

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
/*
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
 */
///////////

@implementation SETwitterImageShare
- (void) handleResponse: (id) response
{
    
    NSLog(@"handle response : r");
    [response release];
}
- (void) performTwitter
{
    NSLog(@"perform twitter");
    ACAccountType* twitterType = [mStore accountTypeWithAccountTypeIdentifier:ACAccountTypeIdentifierTwitter];
    NSArray* accounts = [mStore accountsWithAccountType:twitterType];
    for(ACAccount* account in accounts)
    {
        NSLog(@"account username : %@", [account username]);
        NSLog(@"account description : %@", [account description]);
    }
    NSURL* url = [NSURL URLWithString:@"https://upload.twitter.com/1/statuses/update_with_media.json"];
    UIImage* image = [mShareImageArray objectAtIndex:0];//[UIImage imageNamed:@"info_botton_001.png"];
    NSData* imageData = UIImageJPEGRepresentation(image, 1.0);
    //[imageData retain];
    //NSDictionary* dic = [NSDictionary dictionaryWithObjectsAndKeys:imageData, @"media[]",@"test1", @"status", nil];
    
    TWRequest* request = [[TWRequest alloc] initWithURL:url parameters:nil requestMethod:TWRequestMethodPOST];
    ACAccount* account = [accounts objectAtIndex:0];
    [request setAccount:account];
    [request addMultiPartData:imageData withName:@"media[]" type:@"multipart/form-data"];
    NSString *status = @"From TheSpeedSun PhotoFrame";
    [request addMultiPartData:[status dataUsingEncoding:NSUTF8StringEncoding] 
                     withName:@"status" 
                         type:@"multipart/form-data"];
    [request performRequestWithHandler:^(NSData *responseData, NSHTTPURLResponse *urlResponse, NSError *error) 
    {
        NSError* jsonError = nil;
        NSLog(@"url response status = %d ", urlResponse.statusCode);
        if(responseData == nil)
        {
            NSLog(@"## has no response data ##");
            return;
        }
        id response = [NSJSONSerialization JSONObjectWithData:responseData options:0 error:&jsonError];
        if(jsonError == nil)
        {
            NSLog(@" response ok ");
            [response retain];
            [self performSelectorOnMainThread:@selector(handleResponse:) withObject:response waitUntilDone:NO];
        }
        else
        {
            NSLog(@"response error");
        }
        
        
    }];
    /*
     
    TWTweetComposeViewController* tweet = [[TWTweetComposeViewController alloc] init];
    if(![tweet setInitialText:@"iOS 5 Core framework"])
    {
        NSLog(@"Unable to add text");
    }
    UIImage* image = [UIImage imageNamed:@"info_botton_001.png"];
    if(![tweet addImage:image])
    {
        NSLog(@"Unable to add image");
    }
    TWTweetComposeViewControllerCompletionHandler handler = ^(TWTweetComposeViewControllerResult result)
    {
        switch (result) {
            case TWTweetComposeViewControllerResultCancelled:
                NSLog(@"tweet canceled");
                break;
            case TWTweetComposeViewControllerResultDone:
                NSLog(@"tweet complete");
                break;
            default:
                break;
        }
        [tweet dismissViewControllerAnimated:YES completion:nil];
    };
    [tweet setCompletionHandler:handler];
    [mViewNav presentViewController:tweet animated:YES completion:nil];
     */
}
- (id) initWithViewNav: (SEViewNavigator*)viewNav
{
    self = [super init];
    if(self)
    {
        mViewNav = viewNav;
        mStore = [[ACAccountStore alloc] init];
    }
    return self;
}
- (void) dealloc
{
    [mStore release];
    [_engine release];
    [super dealloc];
}
- (void)share
{
    /*
    ACAccountType* twitterType = [mStore accountTypeWithAccountTypeIdentifier:ACAccountTypeIdentifierTwitter];
    [mStore requestAccessToAccountsWithType:twitterType withCompletionHandler:^(BOOL granted, NSError *error) {
        if(granted)
        {
            NSLog(@"can access twitter acount");
            [self performSelectorOnMainThread:@selector(performTwitter) withObject:nil waitUntilDone:NO];
        }
        else
        {
            NSLog(@"can not access twitter account : %@", [error localizedDescription]);    
        }
        
    }];
     */
    if (_engine) return;
	_engine = [[SA_OAuthTwitterEngine alloc] initOAuthWithDelegate: self];
	_engine.consumerKey = kOAuthConsumerKey;
	_engine.consumerSecret = kOAuthConsumerSecret;
	
	UIViewController			*controller = [SA_OAuthTwitterController controllerToEnterCredentialsWithTwitterEngine: _engine delegate: self];
	
	if (controller) 
		[mViewNav presentModalViewController: controller animated: YES];
	else {
		[_engine sendUpdate: [NSString stringWithFormat: @"Already Updated. %@", [NSDate date]]];
        //NSString* sss = [_engine getPublicTimeline];
        //NSString* sss = [_engine sendUpdate: @"test API"];
        //NSLog(@"ss = %@", sss);
	}
}
- (void) storeCachedTwitterOAuthData: (NSString *) data forUsername: (NSString *) username {
	NSUserDefaults			*defaults = [NSUserDefaults standardUserDefaults];
    
	[defaults setObject: data forKey: @"authData"];
	[defaults synchronize];
}

- (NSString *) cachedTwitterOAuthDataForUsername: (NSString *) username {
	return [[NSUserDefaults standardUserDefaults] objectForKey: @"authData"];
}

//=============================================================================================================================
#pragma mark SA_OAuthTwitterControllerDelegate
- (void) OAuthTwitterController: (SA_OAuthTwitterController *) controller authenticatedWithUsername: (NSString *) username {
	NSLog(@"Authenicated for %@", username);
}

- (void) OAuthTwitterControllerFailed: (SA_OAuthTwitterController *) controller {
	NSLog(@"Authentication Failed!");
}

- (void) OAuthTwitterControllerCanceled: (SA_OAuthTwitterController *) controller {
	NSLog(@"Authentication Canceled.");
    NSString* sss = [_engine testService];
    NSLog(@"sss = %@" , sss);
}

//=============================================================================================================================
#pragma mark TwitterEngineDelegate
- (void) requestSucceeded: (NSString *) requestIdentifier {
	NSLog(@"Request %@ succeeded", requestIdentifier);
}

- (void) requestFailed: (NSString *) requestIdentifier withError: (NSError *) error {
	NSLog(@"Request %@ failed with error: %@", requestIdentifier, error);
}



@end