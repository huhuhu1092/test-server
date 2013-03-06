//
//  SEViewNavigator.mm
//  PhotoFrame
//
//  Created by 陈勇 on 12-1-6.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEViewNavigator.h"
#import "SEUIScrollView.h"
#import "SEResDefine.h"
#import "SEUtil.h"
#import "SelectedImage.h"
#import "UserInfo.h"
//#import "SEUITableView.h"
#import "SEMainDisplay.h"
#import "PainterManager.h"
#import "SEImageMusicListView.h"
#import "SEOptionsView.h"
#import "SEMusicPickerView.h"
#import "SESignatureView.h"
#import "Signature.h"
#import "MusicList.h"
#import "ImageList.h"
#import "SelectedImage.h"
#import "SelectedMusic.h"
#import "SESignaturePreview.h"
#import "SEPageScrollView.h"
#import "SEUserUpgrate.h"
#import "SE3DPreview.h"
#import "SEOperationView.h"
#import "SEPopupViewWidgets.h"
#import "FinishedImage.h"
#import "SEUserUpgradeViewController.h"
#import "SEDataUploadManager.h"
#import "SEDrawTouchView.h"
#import "PhotoFrameAppDelegate.h"
#import "SEMusicImageListPopup.h"
#import "SESystemConfig.h"
#import "SEFrameImageView.h"
#import "SEPopupViewWidgets.h"
#import "SEPopupView.h"
#import "UpgradeInfo.h"
#import "SEImageAsyncLoader.h"
#import "SEProtocalDefine.h"
#import "SEUIProgressView.h"
#import "SEWebAPISender.h"
#import "SEDrawingStateManager.h"
#import "SEDrawCircleView.h"
#import "SEUserDefaultManager.h"
#import "SESystemDataManager.h"
#import "SEKeyChainHelper.h"
#import "SEUISwitch.h"
//for twitter
#import "OAuthConsumer.h"
#import "MTJSON.h"
//end
//#import "SEWebServiceManager.h"
#import "GData.h"
#import "GDataFeedPhotoAlbum.h"
#import "GDataFeedPhoto.h"
//#import "GooglePlusShare.h"
//#import "GooglePlusSignInButton.h"
#import <QuartzCore/CALayer.h>
#import <MediaPlayer/MediaPlayer.h>
#import <AssetsLibrary/AssetsLibrary.h>
/// for weibo
#import "SEImageShareManager.h"
//#import "OAuthEngine.h"
/////////
#define DBG_VIEW_BG(view, color) view.backgroundColor = [UIColor color];
#define SHARE_IMAGE_NUM 5
static void setLabelFont(FontLabel* fontLabel, NSString* text, UIColor* color ,NSString* fontName, CGFloat fontSize)
{
    [fontLabel setZFont:[[FontManager sharedManager] zFontWithName:fontName pointSize:fontSize]];
    fontLabel.text = text;
    fontLabel.backgroundColor = [UIColor clearColor];
    fontLabel.textColor = color;
}
static NSArray* getMusicInMusicList(MusicList* musicList)
{
    NSSet* selectedMusic = musicList.selectedmusic;
    NSArray* retArray = [selectedMusic allObjects];
    retArray = [retArray sortedArrayUsingComparator:^NSComparisonResult(id obj1, id obj2) {
        SelectedMusic* sm1 = (SelectedMusic*)obj1;
        SelectedMusic* sm2 = (SelectedMusic*)obj2;
        return [sm1.seq compare:sm2.seq];
    }];
    return retArray;
}
static void showLoadingViewForShare()
{
    [[PhotoFrameAppDelegate getViewNavigator] showLoadingView];
    SELoadingView* loadingView = [[PhotoFrameAppDelegate getViewNavigator] getLoadingView];
    [loadingView setStage:LOADING_STAGE3];

}
/////////////
@interface SESelectedImageCheckData : NSObject
{
    NSArray* photoUrlArray;
    NSMutableArray* photoExistsArray;
    ALAssetsLibrary* assetLib;
}
@property (nonatomic, retain) ALAssetsLibrary* assetLib;
@property (nonatomic, retain) NSArray* photoUrlArray;
@property (nonatomic, retain) NSMutableArray* photoExistsArray;

@end
@implementation SESelectedImageCheckData
@synthesize photoExistsArray;
@synthesize photoUrlArray;
@synthesize assetLib;
- (void) dealloc
{
    [photoExistsArray release];
    [assetLib release];
    [photoUrlArray release];
    [super dealloc];
}

@end

///////////
@class SEWebService;
@protocol SEWebServiceDelegate <NSObject> 
- (void) handleSignInError: (SEWebService*) webService;
- (void) handleSignInOK : (SEWebService*) webService;
- (void) handleUploadError: (SEWebService*) webService;
- (void) handleUploadOK: (SEWebService*) webService;
- (void) handleOtherError: (SEWebService*)webService;
- (void) handleAuthTestError: (SEWebService*)webService;
- (void) handleAuthTestOK: (SEWebService*)webService;
- (void) handleSignOut: (SEWebService*)webService;
- (void) handleRequestAccessToken: (SEWebService*)webService;
- (void) handleSigningIn: (SEWebService*)webService;
- (void) handleUploadProgress: (unsigned long long) totalCount : (unsigned long long) deliverCount;
- (void) handleMessage: (NSString*)msg;
- (BOOL) isImageNeedScale;
- (void) setErrorNum: (int) errorNum;
- (CGSize) scaledImageSize;
//- (int) getUploadImageCount;
//- (NSData*) getUploadImage: (int) index;
//- (NSString*) getInputUserName;
//- (NSString*) getInputUserPassword;
@end
////
///////////
@interface SEWebService : NSObject
{
    NSObject <SEWebServiceDelegate>* delegate;
    BOOL isFromPhotoLib;
}
@property (nonatomic, retain) NSObject <SEWebServiceDelegate>* delegate;
- (BOOL) isServiceSignIn;
- (void) signIn;
- (void) signOut;
- (void) authTest;
- (void) uploadPhoto: (NSData*)photoData : (NSString*)photoName;
- (BOOL) isPhotoFromPhotoLib;
- (void) setPhotoFromPhotoLib: (BOOL) b;
@end
@implementation SEWebService

@synthesize delegate;
- (BOOL) isServiceSignIn
{
    return NO;
}
- (void) setPhotoFromPhotoLib:(BOOL)b
{
    isFromPhotoLib = b;
}
- (BOOL) isPhotoFromPhotoLib
{
    return isFromPhotoLib;
}
- (void) uploadPhoto: (NSData*)photoData : (NSString*)photoName
{}
- (void) authTest
{}
- (void) signIn
{}
- (void) signOut
{}
- (void) dealloc
{
    [delegate release];
    [super dealloc];
}
@end
///////
@implementation SEDialogView
//@synthesize mContentView;
- (void) dealloc
{
    [super dealloc];
}
- (UIView*) getContentView
{
    return mContentView;
}
- (void) setContentView:(UIView *)v
{
    [mContentView removeFromSuperview];
    if(v)
    {
        CGRect frame = self.frame;
        v.frame = CGRectMake((frame.size.width - v.frame.size.width) / 2, 40, v.frame.size.width, v.frame.size.height);
        [self addSubview:v];
    }
    mContentView = v;
}
@end
//////////////
@interface SEWebServiceManager : NSObject
{
    enum WEBSERVICE_TYPE {PICASA, TWITTER, INVALID_WEBSERVICE, WEBSERVICE_NUM};
    NSMutableArray* mWebServiceArray;
}
- (BOOL) isServiceSignIn: (enum WEBSERVICE_TYPE) webServiceType;
- (void) signIn: (enum WEBSERVICE_TYPE) webServiceType;
- (void) signOut: (enum WEBSERVICE_TYPE) webServiceType;
@end

@implementation SEWebServiceManager
- (BOOL) isServiceSignIn: (enum WEBSERVICE_TYPE) webServiceType
{
    return NO;
}
- (void) signIn: (enum WEBSERVICE_TYPE) webServiceType
{}
- (void) signOut: (enum WEBSERVICE_TYPE) webServiceType
{}

@end
//////
@interface SEImageLoaderForUploadImage : SEImageAsyncLoadHandler
{
    UIImage* mCurrentImage;
    SEWebService* mWebService;
}
@property (nonatomic, retain) SEWebService* mWebService;
@end
@implementation SEImageLoaderForUploadImage
@synthesize mWebService;
- (void) dealloc
{
    [mCurrentImage release];
    [mWebService release];
    [super dealloc];
}
- (void) setImage:(UIImage *)image
{
    mCurrentImage = [image retain];
}
- (void) preHandleImage
{}
- (void) handleImage
{
    NSData* photoData = nil;
    NSObject<SEWebServiceDelegate>* delegate = mWebService.delegate;
    if([delegate isImageNeedScale])
    {
        CGSize s = [delegate scaledImageSize];
        CGSize dstS = [SEUtil computeFitSize:CGSizeMake(mCurrentImage.size.width, mCurrentImage.size.height) toDst:s];
        NSLog(@"dst s = %f, %f", dstS.width, dstS.height);
        UIImage* image = [SEUtil drawImage:mCurrentImage toSize:dstS];
        BOOL isDisplayText = [SEUserDefaultManager isFunctionOK:BASIC_FUNC] == NO;
        if(isDisplayText)
        {
            NSString* str = [SESystemConfig getITuneConnetionStr];
            UIImage* textImage = [SEUtil drawTextOnImage:image text:str color:[UIColor whiteColor] textSize:[SESystemConfig getShareStrFontSize] x:0 y:0];
            photoData = UIImageJPEGRepresentation(textImage, 1.0);
        }
        else
        {
            photoData = UIImageJPEGRepresentation(image, 1.0);
        }
    }
    else
    {
        BOOL isDisplayText = [SEUserDefaultManager isFunctionOK:BASIC_FUNC] == NO;
        if(isDisplayText)
        {
            NSString* str = [SESystemConfig getITuneConnetionStr];
            UIImage* textImage = [SEUtil drawTextOnImage:mCurrentImage text:str color:[UIColor whiteColor] textSize:[SESystemConfig getShareStrFontSize] x:0 y:0];
            photoData = UIImageJPEGRepresentation(textImage, 1.0);
        }
        else 
        {
            photoData = UIImageJPEGRepresentation(mCurrentImage, 1.0);
        }
    }
    NSDate* date = [NSDate date];
    //NSTimeInterval time = [date timeIntervalSince1970];
    NSUInteger uflag;
    uflag = NSDayCalendarUnit | NSMonthCalendarUnit | NSYearCalendarUnit;
    NSCalendar* calendar = [NSCalendar currentCalendar];
    NSDateComponents* component = [calendar components:uflag fromDate:date];
    NSInteger currentDay = [component day];
    NSInteger currentMonth = [component month];
    NSInteger currentYear = [component year];
    NSString* dateStr = [NSString stringWithFormat:@"%d-%d-%d", currentYear, currentMonth, currentDay];//[date description];
    NSString* photoName = [NSString stringWithFormat:@"PrivatePainterShare_%@", dateStr];
    NSLog(@"photoName = %@", photoName);
    [mWebService uploadPhoto:photoData :photoName];
}

@end
////////
/////////////
#define gTwitterRequestTokenURL @"https://api.twitter.com/oauth/request_token"
#define gTwitterAuthorizeURL @"https://api.twitter.com/oauth/authorize"
#define gTwitterAccessTokenURL @"https://api.twitter.com/oauth/access_token"
#define gTwitterConsumerKey @"3oBm0t8FFMca3mjl6EQQuQ"
#define gTwitterConsumerSecret @"L1ge1p2p8U7RkBTJGTGZEDr9hrn8Kd1C5l9XBbLWqKM"
#define gTwitterAuthKey @"UserTwitterKey"
///////////////////
/*
@interface SETwitterWebServiceDelegate : NSObject <SEWebServiceDelegate >
{
    int mCurrentImageIndex;
    NSArray* mImageURLArray;
    int mImageWidth;
    int mImageHeight;
}
@property (nonatomic, assign) int mCurrentImageIndex;
@property (nonatomic, retain) NSArray* mImageURLArray;
@property (nonatomic, assign) int mImageWidth;
@property (nonatomic, assign) int mImageHeight;
@end
@implementation SETwitterWebServiceDelegate
@synthesize mCurrentImageIndex;
@synthesize mImageURLArray;
@synthesize mImageWidth;
@synthesize mImageHeight;
- (void) dealloc
{
    [mImageURLArray release];
    [super dealloc];
}
- (void) handleSignInError: (SEWebService*) webService
{
    NSLog(@"twitter signin error");
}
- (void) handleSignInOK : (SEWebService*) webService
{
    NSLog(@"twitter sign in ok");
}
- (void) loadImage: (int) imageIndex : (SEWebService*)webService
{
    SEImageLoaderForUploadImage* loader = [[SEImageLoaderForUploadImage alloc] init];
    loader.mWebService = webService;
    SEImageAsyncLoader* asyncLoader = [[SEImageAsyncLoader alloc] init];
    asyncLoader.mViewNav = [PhotoFrameAppDelegate getViewNavigator];
    ALAssetsLibrary* lib = [[[ALAssetsLibrary alloc] init] autorelease];
    [asyncLoader setAssetLibOwn:lib];
    SEPageImageURL* imageURL = [mImageURLArray objectAtIndex:imageIndex];
    NSURL* url = imageURL.url;
    NSString* date = imageURL.urlDate;
    [asyncLoader loadCoreDataFullRepresentation:url date: date withHandler:loader];
}
- (void) handleUploadError: (SEWebService*) webService
{
    NSLog(@"twitter upload error");
}
- (void) handleUploadOK: (SEWebService*) webService
{
    NSLog(@"twitter upload ok");
    mCurrentImageIndex++;
    if(mCurrentImageIndex < mImageURLArray.count)
    {
        float v = mCurrentImageIndex / (float)mImageURLArray.count;
        SELoadingView* loadingView = [[PhotoFrameAppDelegate getViewNavigator] getLoadingView];
        [loadingView setPercent:v];
        [self loadImage:mCurrentImageIndex :webService];    
    }
    else
    {
        SELoadingView* loadingView = [[PhotoFrameAppDelegate getViewNavigator] getLoadingView];
        [loadingView setPercent:1];
        [[PhotoFrameAppDelegate getViewNavigator] hideLoadingView];
    }
}
- (void) handleOtherError: (SEWebService*)webService
{
    NSLog(@"twitter other error");
}
- (void) handleAuthTestError: (SEWebService*)webService
{
    NSLog(@"twitter auth test error");
}
- (void) handleAuthTestOK: (SEWebService*)webService
{
    NSLog(@"twitter auth test ok");
    [[PhotoFrameAppDelegate getViewNavigator] hideLoadingView];
    if(mImageURLArray.count > 0)
    {
        [[PhotoFrameAppDelegate getViewNavigator] showLoadingProgressView];
        mCurrentImageIndex = 0;
        [self loadImage:mCurrentImageIndex :webService];
    }
}
- (void) handleUploadProgress: (unsigned long long) totalCount : (unsigned long long) deliverCount
{
    NSLog(@"handle upload progress");
}

@end
 */
///////////
@class SEShareViewItem;
@interface SEGooglePicasaServiceDelegate : NSObject <SEWebServiceDelegate>
{
    enum {JUST_NO_OP, JUST_SIGNIN, JUST_SHARE};
    enum {NO_ERROR, NO_ENOUGH_ROOM_ERROR};
    int mCurrentImageIndex;
    NSArray* mCurrentImageURLArray;
    SEShareViewItem* mParentView;
    int mImageWidth;
    int mImageHeight;
    BOOL mIsScaleImage;
    int mUploadDelayTime;
    int mOpType;
    int mErrorNum;
}
@property (nonatomic, assign) int mOpType;
@property (nonatomic, assign) int mUploadDelayTime;
@property (nonatomic, assign) BOOL mIsScaleImage;
@property (nonatomic, assign) int mImageWidth;
@property (nonatomic, assign) int mImageHeight;
@property (nonatomic, retain) NSArray* mCurrentImageURLArray;
@property (nonatomic, assign) int mCurrentImageIndex;
@property (nonatomic, assign) SEShareViewItem* mParentView;
@end
/////////////////
@interface SETwitterWebService : SEWebService <UIWebViewDelegate>
{
    UIView* mTwitterSignInView;
    NSString	*mConsumerSecret;
	NSString	*mConsumerKey;
	NSURL		*mRequestTokenURL;
	NSURL		*mAccessTokenURL;
	NSURL		*mAuthorizeURL;
    
	NSString	*mPin;
    
@private
	OAConsumer	*mConsumer;
	OAToken		*mRequestToken;
	OAToken		*mAccessToken; 
    NSString* mUserName;
    UIView* mView;
    UIWebView* mWebView;
    UIButton* mCancelButton;
    UILabel* mInfoLabel;
    NSString* mUserAccessTokenString;
    NSData* mPhotoData;
    NSString* mPhotoName;
    BOOL mSignInWithDataUpload;
    BOOL mCancelWebAuth;
    int mReconnectTimes;
    int mCurrentReconnectTime;
    /////////////////////////
    BOOL mAccessTokenRequestEnd;
    BOOL mRequestTokenEnd;
    BOOL mWebLoadEnd;
}
@property (nonatomic, assign) BOOL mSignInWithDataUpload;
@property (nonatomic, retain) NSData* mPhotoData;
@property (nonatomic, retain) NSString* mPhotoName;
@property (nonatomic, retain) NSString* mUserName;
@property (nonatomic, retain) OAConsumer* mConsumer;
@property (nonatomic, retain) OAToken* mRequestToken;
@property (nonatomic, retain) OAToken* mAccessToken;
@property (nonatomic, retain) NSString* mConsumerSecret;
@property (nonatomic, retain) NSString* mConsumerKey;
@property (nonatomic, retain) NSURL* mRequestTokenURL;
@property (nonatomic, retain) NSURL		*mAccessTokenURL;
@property (nonatomic, retain) NSURL		*mAuthorizeURL;
@property (nonatomic, retain) NSString	*mPin;
@end
@implementation SETwitterWebService
@synthesize mSignInWithDataUpload;
@synthesize mUserName;
@synthesize mConsumer;
@synthesize mRequestToken;
@synthesize mAccessToken;
@synthesize mConsumerKey;
@synthesize mConsumerSecret;
@synthesize mRequestTokenURL;
@synthesize mAuthorizeURL;
@synthesize mAccessTokenURL;
@synthesize mPin;
@synthesize mPhotoData;
@synthesize mPhotoName;
- (id) init
{
    self = [super init];
    if(self)
    {
        NSLog(@"twiter service init");
        self.mRequestTokenURL = [NSURL URLWithString: gTwitterRequestTokenURL];
		self.mAccessTokenURL = [NSURL URLWithString: gTwitterAccessTokenURL];
		self.mAuthorizeURL = [NSURL URLWithString: gTwitterAuthorizeURL];
        self.mConsumerKey = [NSString stringWithFormat:@"%@", gTwitterConsumerKey];
        self.mConsumerSecret = [NSString stringWithFormat:@"%@", gTwitterConsumerSecret];
        self.mConsumer = [[[OAConsumer alloc] initWithKey: self.mConsumerKey secret: self.mConsumerSecret] autorelease];
        NSString* s = [[NSUserDefaults standardUserDefaults] objectForKey:gTwitterAuthKey];
        NSLog(@"s = %@", s);
        if(s)
        {
            self.mAccessToken = [[[OAToken alloc] initWithHTTPResponseBody:s] autorelease];
        }
        mReconnectTimes = 1;
        //////////
        mAccessTokenRequestEnd = YES;
        mRequestTokenEnd = YES;
        mWebLoadEnd = YES;
    }
    return self;
}
- (void) dealloc
{
    NSLog(@"twitter webservice dealloc");
    [mRequestTokenURL release];
    [mAccessTokenURL release];
    [mAuthorizeURL release];
    [mPin release];
    [mConsumerKey release];
    [mConsumerSecret release];
    [mConsumer release];
    [mPhotoData release];
    [mPhotoName release];
    [super dealloc];
}
- (BOOL) OAuthSetup 
{
	return mConsumer != nil;
}
- (BOOL) isAllRequestEnd
{
    return mRequestTokenEnd && mAccessTokenRequestEnd && mWebLoadEnd && mCancelWebAuth;
}
- (BOOL) isAuthorized 
{	
	if (mAccessToken.key && mAccessToken.secret) 
    {
        return YES;
    }
    else 
    {
        return NO;
    }
}
- (void) outhTicketFailed: (OAServiceTicket *) ticket data: (NSData *) data 
{
    mRequestTokenEnd = NO;
    /*
    if([self isAllRequestEnd])
    {
        [self release];
    }
     */
    if(mCancelWebAuth)
    {
        [self release];
        return;
    }
    NSHTTPURLResponse* r = (NSHTTPURLResponse*)ticket.response;
    NSLog(@"service ticket failed = %d", r.statusCode);
    [mView removeFromSuperview];
    mView = nil;
    if(mSignInWithDataUpload)
    {
        [delegate handleUploadError:self];
    }
    else
    {
        [delegate handleSignInError:self];
    }
    [self release];
}
- (void) requestURL: (NSURL *) url token: (OAToken *) token onSuccess: (SEL) success onFail: (SEL) fail 
{
    OAMutableURLRequest				*request = [[[OAMutableURLRequest alloc] initWithURL: url consumer: self.mConsumer token:token realm:nil signatureProvider: nil] autorelease];
	if (!request) 
    {
        return;
    }

    [request setHTTPMethod: @"POST"];
    
    
	[self retain];
    NSLog(@"reques  url and self retain : %d", [self retainCount]);
    OADataFetcher				*fetcher = [[[OADataFetcher alloc] init] autorelease];	
    [fetcher fetchDataWithRequest: request delegate: self didFinishSelector: success didFailSelector: fail];
}

//A request token is used to eventually generate an access token
- (void) requestRequestToken 
{
    mRequestTokenEnd = NO;
	[self requestURL: self.mRequestTokenURL token: nil onSuccess: @selector(setRequestToken:withData:) onFail: @selector(outhTicketFailed:data:)];
}

- (void) setRequestToken: (OAServiceTicket *) ticket withData: (NSData *) data
{
    mRequestTokenEnd = YES;
    /*
    if([self isAllRequestEnd])
    {
        [self release];
    }
     */
    if(mCancelWebAuth)
    {
        [self release];
        return;
    }
    [self release];
    NSLog(@"setRequestToken = %d", [self retainCount]);
	if (!ticket.didSucceed || !data) 
    {
        if(mSignInWithDataUpload)
        {
            [delegate handleUploadError:self];
        }
        else
        {
            [delegate handleSignInError:self];
        }
        return;
    }
    if(mCancelWebAuth)
    {
        if(mSignInWithDataUpload)
        {
            [delegate handleUploadError:self];
        }
        else
        {
            [delegate handleSignInError:self];
        }
        return;
    }
	NSString *dataString = [[[NSString alloc] initWithData: data encoding: NSUTF8StringEncoding] autorelease];
	if (!dataString) 
    {
        if(mSignInWithDataUpload)
        {
            [delegate handleUploadError:self];
        }
        else
        {
            [delegate handleSignInError:self];
        }
        return;
    }

	self.mRequestToken = [[[OAToken alloc] initWithHTTPResponseBody:dataString] autorelease];
	NSURLRequest* request = [self authorizeURLRequest];
    //[self retain];
    [mWebView loadRequest: request];
    [[NSNotificationCenter defaultCenter] addObserver: self selector: @selector(pasteboardChanged:) name: UIPasteboardChangedNotification object: nil];
	//if (self.pin.length) mRequestToken.pin = self.pin;
}
- (NSString *) extractUsernameFromHTTPBody: (NSString *) body
{
	if (!body) return nil;
	
	NSArray					*tuples = [body componentsSeparatedByString: @"&"];
	if (tuples.count < 1) return nil;
	
	for (NSString *tuple in tuples) {
		NSArray *keyValueArray = [tuple componentsSeparatedByString: @"="];
		
		if (keyValueArray.count == 2) {
			NSString				*key = [keyValueArray objectAtIndex: 0];
			NSString				*value = [keyValueArray objectAtIndex: 1];
			
			if ([key isEqualToString:@"screen_name"]) return value;
		}
	}
	
	return nil;
}
- (void) setAccessToken: (OAServiceTicket *) ticket withData: (NSData *) data 
{
    mAccessTokenRequestEnd = YES;
    /*
    if([self isAllRequestEnd])
    {
        [self release];
    }
     */
    if(mCancelWebAuth)
    {
        [[PhotoFrameAppDelegate getViewNavigator] hideLoadingView];
        [delegate handleSignInError:self];
        [self release];
        return;
    }
    [self release];
	if (!ticket.didSucceed || !data) 
    {
        [[PhotoFrameAppDelegate getViewNavigator] hideLoadingView];
        [delegate handleSignInError:self];
        NSLog(@"setAccessToken disSucced error: %d", [self retainCount]);
        return;
    }
	
	NSString *dataString = [[[NSString alloc] initWithData: data encoding: NSUTF8StringEncoding] autorelease];
	if (!dataString) 
    {
        //[self release];
        NSLog(@"setAccessToken dataString == nil : %d", [self retainCount]);
        [[PhotoFrameAppDelegate getViewNavigator] hideLoadingView];
        [delegate handleSignInError:self];
        return;
    }
    NSLog(@"dataString = %@", dataString);
	if (self.mPin.length && [dataString rangeOfString: @"oauth_verifier"].location == NSNotFound) 
        dataString = [dataString stringByAppendingFormat: @"&oauth_verifier=%@", self.mPin];
	
	NSString				*username = [self extractUsernameFromHTTPBody:dataString];
    
	if (username.length > 0) 
    {
        self.mUserName = username;
	}
	
	//[_accessToken release];
	self.mAccessToken = [[[OAToken alloc] initWithHTTPResponseBody:dataString] autorelease];
    [[NSUserDefaults standardUserDefaults] setObject:dataString forKey:gTwitterAuthKey];
    [[NSUserDefaults standardUserDefaults] synchronize];
    [delegate handleSignInOK:self];
    if(mSignInWithDataUpload)
    {
        mSignInWithDataUpload = NO;
        [self authTest];
    }
    else
    {
        [[PhotoFrameAppDelegate getViewNavigator] hideLoadingView];
    }
    //[self release];
}
- (void) outhTicketAccessFailed: (OAServiceTicket *) ticket data: (NSData *) data 
{
    mAccessTokenRequestEnd = YES;
    /*
    if([self isAllRequestEnd])
    {
        [self release];
    }
     */
    if(mCancelWebAuth)
    {
        [self release];
        return;
    }
    NSHTTPURLResponse* r = (NSHTTPURLResponse*)ticket.response;
    NSLog(@"service ticket failed = %d", r.statusCode);
    [mView removeFromSuperview];
    mView = nil;
    if(mSignInWithDataUpload)
    {
        [delegate handleUploadError:self];
    }
    else
    {
        [delegate handleSignInError:self];
    }
    [self release];
}

//this is what we eventually want
- (void) requestAccessToken 
{
    mAccessTokenRequestEnd = NO;
	[self requestURL: self.mAccessTokenURL token: mRequestToken onSuccess: @selector(setAccessToken:withData:) onFail: @selector(outhTicketAccessFailed:data:)];
}
//This generates a URL request that can be passed to a UIWebView. It will open a page in which the user must enter their Twitter creds to validate
- (NSURLRequest *) authorizeURLRequest 
{
	if (!mRequestToken.key && mRequestToken.secret) 
        return nil;	// we need a valid request token to generate the URL
    
	OAMutableURLRequest			*request = [[[OAMutableURLRequest alloc] initWithURL: self.mAuthorizeURL consumer: nil token: mRequestToken realm: nil signatureProvider: nil] autorelease];	
    NSLog(@"oauth_token = %@", mRequestToken.key);
	[request setParameters: [NSArray arrayWithObject: [[[OARequestParameter alloc] initWithName: @"oauth_token" value: mRequestToken.key] autorelease]]];	
	return request;
}
- (BOOL) oauthtwitter_isNumeric: (NSString*) str 
{
	const char				*raw = (const char *) [str UTF8String];
	
	for (int i = 0; i < strlen(raw); i++) {
		if (raw[i] < '0' || raw[i] > '9') return NO;
	}
	return YES;
}
- (void) gotPin: (NSString *) pin 
{
	self.mPin = pin;
    mRequestToken.pin = pin;
    [mView removeFromSuperview];
    mView = nil;
    showLoadingViewForShare();
    [delegate handleRequestAccessToken:self];
	[self requestAccessToken];

}
- (void) pasteboardChanged: (NSNotification *) note
{
	UIPasteboard					*pb = [UIPasteboard generalPasteboard];
	
	if ([note.userInfo objectForKey: UIPasteboardChangedTypesAddedKey] == nil) return;		//no meaningful change
	
	NSString						*copied = pb.string;
	
	if (copied.length != 7 || ![self oauthtwitter_isNumeric:copied]) 
        return;
	
	[self gotPin: copied];
}
- (void) cancelButtonHandler: (UIButton*)sender
{
    NSLog(@"cancel button click ");
    mCancelWebAuth = YES;
    /*
    if([self isAllRequestEnd] && mView != nil)
    {
        [self release];
        
    }
     */
    if(mWebLoadEnd)
    {
        [mView removeFromSuperview];
        mView = nil;
    }
}
- (void) realSignIn
{
    UIView* authView = [[[NSBundle mainBundle] loadNibNamed:@"SignInView" owner:nil options:nil] lastObject];
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    NSLog(@"## authView = %f, %f", authView.frame.size.width, authView.frame.size.height);
    authView.frame = CGRectMake((viewNav.mViewPortWidth - authView.frame.size.width)/  2, (viewNav.mViewPortHeight - authView.frame.size.height) / 2, authView.frame.size.width, authView.frame.size.height);

    UIWebView* webView = (UIWebView*)[authView viewWithTag:101];
    mCancelButton = (UIButton*)[authView viewWithTag:102];
    [mCancelButton addTarget:self action:@selector(cancelButtonHandler:) forControlEvents:UIControlEventTouchUpInside];
    mInfoLabel = (UILabel*)[authView viewWithTag:103];
    
    [viewNav.mRootView addSubview:authView];
    mTwitterSignInView = authView;
    mWebView = webView;
    mWebView.delegate = self;
    mInfoLabel.text = @"Loading Authenticated Information";
    [self requestRequestToken];
    mView = authView;
    mCancelWebAuth = NO;
}
- (void) signIn
{
    mSignInWithDataUpload = NO;
    //[self retain];
    [self realSignIn];
}
- (void) signOut
{
    [[NSUserDefaults standardUserDefaults] removeObjectForKey:gTwitterAuthKey];
    [[NSUserDefaults standardUserDefaults] synchronize];
    self.mAccessToken = nil;
    [delegate handleSignOut:self];

}
- (BOOL) isServiceSignIn
{
    return [self isAuthorized];
}
/*
- (void) requestURL: (NSURL *) url token: (OAToken *) token onSuccess: (SEL) success onFail: (SEL) fail 
{
    OAMutableURLRequest				*request = [[[OAMutableURLRequest alloc] initWithURL: url consumer: self.mConsumer token:token realm:nil signatureProvider: nil] autorelease];
	if (!request) return;
    
    [request setHTTPMethod: @"POST"];
	
    OADataFetcher				*fetcher = [[[OADataFetcher alloc] init] autorelease];	
    [fetcher fetchDataWithRequest: request delegate: self didFinishSelector: success didFailSelector: fail];
}
 */
- (void) setConfig : (OAServiceTicket *) ticket withData: (NSData *) data 
{
    if(ticket.didSucceed == NO || data == nil)
    {
        [[PhotoFrameAppDelegate getViewNavigator] hideLoadingView];
        [delegate handleUploadError:self];
        return;
    }
    NSString* dataString = [[[NSString alloc ] initWithData: data encoding: NSUTF8StringEncoding] autorelease];
    NSLog(@"data String = %@", dataString);
    NSArray* objectArray = [MTJSON MTJSONStringToObject:dataString];
    NSDictionary* dict = [objectArray objectAtIndex:0];
    NSString* photoSizeLimit = [dict objectForKey:@"photo_size_limit"];
    NSLog(@"photo size limit = %@", photoSizeLimit);
    NSDictionary* photosize = [dict objectForKey:@"photo_sizes"];
    NSDictionary* medium = [photosize objectForKey:@"medium"];
    NSString* hstr = [medium objectForKey:@"h"];
    NSString* wstr = [medium objectForKey:@"w"];
    NSLog(@"hstr = %@, wstr = %@", hstr, wstr);
    SEGooglePicasaServiceDelegate* currentDelegate = (SEGooglePicasaServiceDelegate*)self.delegate;
    currentDelegate.mImageWidth = [wstr intValue];
    currentDelegate.mImageHeight = [hstr intValue];
    mCurrentReconnectTime = 0;
    [delegate handleAuthTestOK:self];
}
- (void) configError: (OAServiceTicket *) ticket withData: (NSData *) data
{
    NSLog(@"network test error");
    [delegate handleUploadError:self];
    [[PhotoFrameAppDelegate getViewNavigator] hideLoadingView];
}

- (void) uploadHandler: (OAServiceTicket *) ticket withData: (NSData *) data
{
    if(ticket.didSucceed == NO || data == nil)
    {
        NSHTTPURLResponse* response = (NSHTTPURLResponse*)ticket.response;
        NSLog(@"response code = %d", response.statusCode);
        NSLog(@"data = %@", data);
        [delegate handleUploadError:self];
        [[PhotoFrameAppDelegate getViewNavigator] hideLoadingView];
        return;
    }
    [delegate handleUploadOK:self];
}
- (void) uploadHandlerError: (OAServiceTicket *) ticket withData: (NSData *) data
{
    NSHTTPURLResponse* response = (NSHTTPURLResponse*)ticket.response;
    NSLog(@"response code = %d", response.statusCode);
    NSLog(@"data = %@", data);
    mCurrentReconnectTime++;
    if(mCurrentReconnectTime < mReconnectTimes)
    {
        [self uploadPhoto:mPhotoData :mPhotoName];
    }
    else
    {
        [[PhotoFrameAppDelegate getViewNavigator] hideLoadingView];
        [delegate handleUploadError:self];
    }
}
- (NSString *)URLEncodedString: (NSString*) srcStr
{
    NSString *result = (NSString *)CFURLCreateStringByAddingPercentEscapes(kCFAllocatorDefault,
                                                                           (CFStringRef)srcStr,
                                                                           NULL,
																		   CFSTR("!*'();:@&=+$,/?%#[]"),
                                                                           kCFStringEncodingUTF8);
    [result autorelease];
	return result;
}

- (NSString*)URLDecodedString: (NSString*)srcStr
{
	NSString *result = (NSString *)CFURLCreateStringByReplacingPercentEscapesUsingEncoding(kCFAllocatorDefault,
																						   (CFStringRef)srcStr,
																						   CFSTR(""),
																						   kCFStringEncodingUTF8);
    [result autorelease];
	return result;	
}

- (void) uploadPhoto:(NSData *)photoData :(NSString *)photoName
{
    self.mPhotoData = photoData;
    self.mPhotoName = photoName;
    NSString* configPath = @"https://api.twitter.com/1.1/statuses/update_with_media.json";
    //NSString* configPath = @"https://api.twitter.com/1.1/statuses/update.json";
    NSURL* url = [NSURL URLWithString:configPath];
    OAMutableURLRequest* request = [[[OAMutableURLRequest alloc] initWithURL:url consumer:self.mConsumer token:mAccessToken realm:nil signatureProvider:nil] autorelease];
    [request setHTTPMethod: @"POST"];
    [request setHTTPShouldHandleCookies:NO];
    /*
    NSString* test = [NSString stringWithFormat:@"status=%@", @"test"];
    NSMutableData* outData = [NSMutableData data];
    [outData appendData:[[self URLEncodedString:test] dataUsingEncoding:NSUTF8StringEncoding]];
    [request setHTTPBody:outData];
     */
    
    //NSString* photoName = mPhotoName;
    //NSData* photoData = mPhotoData;
    NSString* contentType = @"multipart/form-data; boundary=";
    NSString* boundary = @"------------------------7d4a6d158c9";
    contentType = [NSString stringWithFormat:@"%@%@", contentType, boundary];
    [request setValue:contentType forHTTPHeaderField:@"Content-Type"];
    NSString* multipartFirst = @"--";
    NSString* first = [NSString stringWithFormat:@"%@%@\r\n", multipartFirst, boundary];
    NSString* part1 = @"Content-Disposition: form-data; name=\"status\"\r\n\r\n";
    NSString* part1Content = @" \r\n";
    
    NSString* second = [NSString stringWithFormat:@"%@%@\r\n", multipartFirst, boundary];
    NSString* contentType2 = @"Content-Type: image/jpeg\r\n";
    NSString* contentDisposition = [NSString stringWithFormat:@"Content-Disposition: form-data; name=\"media[]\"; filename=\"%@.jpg\"\r\n\r\n", photoName];
    
    NSString* end = [NSString stringWithFormat:@"\r\n%@%@%@\r\n", multipartFirst, boundary, multipartFirst];
    
    NSMutableData* outData = [NSMutableData data];
    [outData appendData:[first dataUsingEncoding:NSUTF8StringEncoding] ];
    [outData appendData:[part1 dataUsingEncoding:NSUTF8StringEncoding]];
    [outData appendData:[part1Content dataUsingEncoding:NSUTF8StringEncoding]];
    [outData appendData:[second dataUsingEncoding:NSUTF8StringEncoding]];
    [outData appendData:[contentType2 dataUsingEncoding:NSUTF8StringEncoding]];
    [outData appendData: [contentDisposition dataUsingEncoding:NSUTF8StringEncoding]];
    //NSString* testStr = [[NSString alloc] initWithData:outData encoding:NSUTF8StringEncoding];
    //NSLog(@"test str = %@", testStr);
    [outData appendData:photoData];
    NSLog(@"photoData size = %d", photoData.length);
    [outData appendData:[end dataUsingEncoding:NSUTF8StringEncoding]];
    [request setHTTPBody:outData];
    //NSString* lenStr = [NSString stringWithFormat:@"%d", outData.length];
    //[request setValue:lenStr forHTTPHeaderField:@"Content-Length"];
    
    OADataFetcher				*fetcher = [[[OADataFetcher alloc] init] autorelease];	
    [fetcher fetchDataWithRequest: request delegate: self didFinishSelector: @selector(uploadHandler:withData:) didFailSelector: @selector(uploadHandlerError:withData:)];
    //[[PhotoFrameAppDelegate getViewNavigator] showLoadingTextView];
}
- (void) authTest
{
    if([self isServiceSignIn] == NO)
    {
        mSignInWithDataUpload = YES;
        [self realSignIn];
    }
    else 
    {
        NSString* configPath = @"https://api.twitter.com/1.1/help/configuration.json";
        NSURL* url = [NSURL URLWithString:configPath];
        OAMutableURLRequest* request = [[[OAMutableURLRequest alloc] initWithURL:url consumer:self.mConsumer token:mAccessToken realm:nil signatureProvider:nil] autorelease];
        
        OADataFetcher				*fetcher = [[[OADataFetcher alloc] init] autorelease];	
        [fetcher fetchDataWithRequest: request delegate: self didFinishSelector: @selector(setConfig:withData:) didFailSelector: @selector(configError:withData:)];
        [[PhotoFrameAppDelegate getViewNavigator] showLoadingView];
        SELoadingView* loadingView = [[PhotoFrameAppDelegate getViewNavigator] getLoadingView];
        [loadingView setStage:LOADING_STAGE3];
    }
}
- (NSString*) extractPin: (NSString*) htmlStr
{
    NSRange rangeStart = [htmlStr rangeOfString:@"<code>"];
    if(rangeStart.location == NSNotFound)
    {
        return nil;
    }
    NSRange rangeEnd = [htmlStr rangeOfString:@"</code>"];
    if(rangeEnd.location == NSNotFound)
        return nil;
    NSString* subStr = [htmlStr substringToIndex:rangeEnd.location];
    rangeStart = [subStr rangeOfString:@">"];
    if(rangeStart.location == NSNotFound)
        return nil;
    subStr = [subStr substringFromIndex:rangeStart.location + 1];
    return subStr;
}
- (NSString*) catchPattern: (NSString*) str before: (NSString*)before after: (NSString*)after
{
    NSRange rangeFirst = [str rangeOfString:before];
    if(rangeFirst.location == NSNotFound)
        return nil;
    NSRange rangeLast = [str rangeOfString:after];
    if(rangeLast.location == NSNotFound)
        return nil;
    NSRange range;
    range.location = rangeFirst.location + before.length;
    range.length = rangeLast.location - range.location;
    NSString* newStr = [str substringWithRange:range];
    return newStr;
}
- (void) webViewDidFinishLoad: (UIWebView *) webView
{
    mWebLoadEnd = YES;

    if(mCancelWebAuth)
    {
        NSLog(@"button cancel click");
        [self  release];
        return;
    }
    [self release];
    NSLog(@"web view finished");
    NSString* str = [webView stringByEvaluatingJavaScriptFromString:@"document.body.innerHTML"];
    if(str == nil || str.length == 0)
    {
        //[self release];
        return;
    }
    //NSLog(@"str = %@", str);
    NSString* pin = [self catchPattern:str before:@"<kbd aria-labelledby=\"code-desc\"><code>" after: @"</code></kbd>"];
    if(pin)
        [self gotPin:pin];
    //[self release];
    NSLog(@"webview finish : %d", [self retainCount]);
}
- (void) webViewDidStartLoad: (UIWebView *) webView 
{
    NSLog(@"start load");
    mWebLoadEnd = NO;
    [self retain];
}
- (void)webView:(UIWebView *)webView didFailLoadWithError:(NSError *)error
{
    NSLog(@"load web view error");
    mWebLoadEnd = YES;
    [self release];
}

- (BOOL) webView: (UIWebView *) webView shouldStartLoadWithRequest: (NSURLRequest *) request 
{
    return YES;
}


@end
//////////
@interface SEGooglePicasaService : SEWebService
{
    //enum GOOGLE_SERVICE_STATE {GOOGLE_NO_ACTION, GOOGLE_SIGN_IN, GOOGLE_UPLOAD_IMAGE};
    GDataFeedPhotoUser *mUserAlbumFeed; // user feed of album entries
    GDataServiceTicket *mAlbumFetchTicket;
    NSError *mAlbumFetchError;
    NSString *mAlbumImageURLString;
    
    GDataFeedPhotoAlbum *mAlbumPhotosFeed; // album feed of photo entries
    GDataServiceTicket *mPhotosFetchTicket;
    NSError *mPhotosFetchError;
    NSString *mPhotoImageURLString;
    int mCurrentLoadImageIndex;
    
    unsigned long long mLastProgressDeliveredCount;
    unsigned long long mLastProgressTotalCount;
    
    NSData* mPhotoData;
    NSString* mPhotoName;
    NSString* mInputUserName;
    NSString* mInputPassword;
    
    BOOL mSaveUserName;
    //GOOGLE_SERVICE_STATE mCurrentGoogleServiceState;
    //BOOL mbAuthed;
    //BOOL mDoAuthTest;
}
@property (nonatomic, retain) NSData* mPhotoData;
@property (nonatomic, retain) NSString* mPhotoName;
@property (nonatomic, retain) NSString* mInputUserName;
@property (nonatomic, retain) NSString* mInputPassword;
@end
#define PICASA_USER_NAME_KEY @"PicasaUserName"
#define PICASA_USER_PASSWORD_KEY @"PicasaUserPassword"
#define PICASA_SAVE_USERNAME_SIGN_KEY @"PicasaSaveUserName"
@implementation SEGooglePicasaService
@synthesize mPhotoData;
@synthesize mPhotoName;
@synthesize mInputPassword;
@synthesize mInputUserName;
- (void) dealloc
{
    NSLog(@"google picasa web service dealloc");
    [mPhotoName release];
    [mPhotoData release];
    [mInputPassword release];
    [mInputUserName release];
    [super dealloc];
}
- (id) init
{
    self = [super init];
    if(self)
    {
        NSLog(@"google picasa service init");
        //mbAuthed = YES;
    }
    return self;
}
- (NSString*) getPicasaUserName
{
    NSData* data = [[NSUserDefaults standardUserDefaults] objectForKey:PICASA_USER_NAME_KEY];
    NSString* s = nil;
    BOOL ret = [SEKeyChainHelper getStringValueFromString:data :&s];
    if(ret)
        return s;
    else
        return nil;

}
- (BOOL) getSaveUserNameSign
{
    NSNumber* b = [[NSUserDefaults standardUserDefaults] objectForKey:PICASA_SAVE_USERNAME_SIGN_KEY];
    if(b)
    {
        return [b boolValue];
    }
    else
        return NO;
}
- (NSString*) getPicasaUserPassword
{
    return [[NSUserDefaults standardUserDefaults] objectForKey:PICASA_USER_PASSWORD_KEY];
}
- (NSString*) getNameFromEmailType: (NSString*)str
{
    NSArray* strArray = [str componentsSeparatedByString:@"@"];
    if(strArray.count > 0)
        return [strArray objectAtIndex:0];
    else
        return nil;
}
- (void) setSaveUserNameSign: (BOOL)b
{
    NSNumber* num = [NSNumber numberWithBool:b];
    [[NSUserDefaults standardUserDefaults] setObject:num forKey:PICASA_SAVE_USERNAME_SIGN_KEY];
}
- (void) setPicasaUserName: (NSString*)name
{
    NSString* emailNamePart = [self getNameFromEmailType:name];

    if(mSaveUserName)
    {
        if(emailNamePart != nil)
        {
            NSLog(@"set user name = %@", emailNamePart);
            NSData* encryptData = [SEKeyChainHelper createMyStringFromStringValue:emailNamePart];
            [[NSUserDefaults standardUserDefaults] setObject:encryptData forKey:PICASA_USER_NAME_KEY];
            [[NSUserDefaults standardUserDefaults] synchronize];
        }
    }
    /*
    else
    {
        NSString* oldName = [self getPicasaUserName];
        NSLog(@"oldName = %@", oldName);
        NSLog(@"emailNamePart = %@", emailNamePart);
        if(emailNamePart)
        {
            if([oldName isEqualToString:emailNamePart])
            {
                [[NSUserDefaults standardUserDefaults] removeObjectForKey:PICASA_USER_NAME_KEY];
                [[NSUserDefaults standardUserDefaults] synchronize];
            }
        }
    }
     */
}
- (void) setPicasaUserPassword: (NSString*)key
{
    [[NSUserDefaults standardUserDefaults] setObject:key forKey:PICASA_USER_PASSWORD_KEY];
    [[NSUserDefaults standardUserDefaults] synchronize];
}
- (GDataFeedPhotoUser *)albumFeed {
    return mUserAlbumFeed; 
}

- (void)setAlbumFeed:(GDataFeedPhotoUser *)feed {
    [mUserAlbumFeed autorelease];
    mUserAlbumFeed = [feed retain];
}

- (NSError *)albumFetchError {
    return mAlbumFetchError; 
}

- (void)setAlbumFetchError:(NSError *)error {
    [mAlbumFetchError release];
    mAlbumFetchError = [error retain];
}

- (GDataServiceTicket *)albumFetchTicket {
    return mAlbumFetchTicket; 
}

- (void)setAlbumFetchTicket:(GDataServiceTicket *)ticket {
    [mAlbumFetchTicket release];
    mAlbumFetchTicket = [ticket retain];
}

- (NSString *)albumImageURLString {
    return mAlbumImageURLString;
}

- (void)setAlbumImageURLString:(NSString *)str {
    [mAlbumImageURLString autorelease];
    mAlbumImageURLString = [str copy];
}

- (GDataFeedPhotoAlbum *)photoFeed {
    return mAlbumPhotosFeed; 
}

- (void)setPhotoFeed:(GDataFeedPhotoAlbum *)feed {
    [mAlbumPhotosFeed autorelease];
    mAlbumPhotosFeed = [feed retain];
}

- (NSError *)photoFetchError {
    return mPhotosFetchError; 
}

- (void)setPhotoFetchError:(NSError *)error {
    [mPhotosFetchError release];
    mPhotosFetchError = [error retain];
}

- (GDataServiceTicket *)photoFetchTicket {
    return mPhotosFetchTicket; 
}

- (void)setPhotoFetchTicket:(GDataServiceTicket *)ticket {
    [mPhotosFetchTicket release];
    mPhotosFetchTicket = [ticket retain];
}

- (NSString *)photoImageURLString {
    return mPhotoImageURLString;
}

- (void)setPhotoImageURLString:(NSString *)str {
    [mPhotoImageURLString autorelease];
    mPhotoImageURLString = [str copy];
}

- (GDataServiceGooglePhotos *)googlePhotosService: (NSString*)username password: (NSString*)password {
    
    static GDataServiceGooglePhotos* service = nil;
    
    if (!service)
    {
        service = [[GDataServiceGooglePhotos alloc] init];
        
        [service setShouldCacheResponseData:YES];
        [service setServiceShouldFollowNextLinks:YES];
    }
    
    // update the username/password each time the service is requested
    //NSString *username = [self getPicasaUserName];
    //NSString *password = [self getPicasaUserPassword];
    if ([username length] && [password length])
    {
        [service setUserCredentialsWithUsername:username
                                       password:password];
    } 
    else
    {
        [service setUserCredentialsWithUsername:nil
                                       password:nil];
    }
    
    return service;
}
/*
- (void)albumListFetchTicket:(GDataServiceTicket *)ticket
            finishedWithFeed:(GDataFeedPhotoUser *)feed
                       error:(NSError *)error {
    [self setAlbumFeed:feed];
    [self setAlbumFetchError:error];
    [self setAlbumFetchTicket:nil];

    if(mAlbumFetchError)
    {
        NSDictionary* userinfo = [mAlbumFetchError userInfo];
        NSLog(@"code = %d", [mAlbumFetchError code]);
        NSArray* keys = [userinfo allKeys];
        BOOL logInError = NO;
        for(NSString* key in keys)
        {
            NSString* value = [userinfo objectForKey:key];
            NSLog(@"## key = %@ #", key);
            NSLog(@"## value = %@", [userinfo objectForKey:key]);
            if([key isEqualToString:@"Error"] && [value isEqualToString:@"BadAuthentication"])
            {
                logInError = YES;
                break;
            }
        }
        mbAuthed = NO;
        if(logInError)
        {
            if(mDoAuthTest)
            {
                [delegate handleAuthTestError:self];
            }
            else
            {
                [delegate handleSignInError:self];
            }

        }
        else 
        {
            [delegate handleOtherError:self];
        }
    }
    else
    {
        if(mbAuthed == NO)
        {
            [self setPicasaUserName:mInputUserName];
            [self setPicasaUserPassword:mInputPassword];
            mbAuthed = YES;
        }
        if(mDoAuthTest)
        {
            [delegate handleAuthTestOK:self];
        }
        else
        {
            [delegate handleSignInOK:self];
        }
    }
    if(mDoAuthTest)
        mDoAuthTest = NO;
}
 */
- (void) signInAlbumListFetchTicket:(GDataServiceTicket *)ticket
            finishedWithFeed:(GDataFeedPhotoUser *)feed
                       error:(NSError *)error 
{
    [self setAlbumFeed:feed];
    [self setAlbumFetchError:error];
    [self setAlbumFetchTicket:nil];
    
    if(mAlbumFetchError)
    {
        NSDictionary* userinfo = [mAlbumFetchError userInfo];
        NSLog(@"code = %d", [mAlbumFetchError code]);
        NSArray* keys = [userinfo allKeys];
        BOOL logInError = NO;
        for(NSString* key in keys)
        {
            NSString* value = [userinfo objectForKey:key];
            NSLog(@"## key = %@ #", key);
            NSLog(@"## value = %@", [userinfo objectForKey:key]);
            if([key isEqualToString:@"Error"] && [value isEqualToString:@"BadAuthentication"])
            {
                logInError = YES;
                break;
            }
        }
        [delegate handleSignInError:self];
    }
    else
    {
        //[self setPicasaUserName:mInputUserName];
        [delegate handleSignInOK:self];
    }
}
- (void) realSignIn
{
    [self setAlbumFeed:nil];
    [self setAlbumFetchError:nil];
    [self setAlbumFetchTicket:nil];
    
    [self setPhotoFeed:nil];
    [self setPhotoFetchError:nil];
    [self setPhotoFetchTicket:nil];
    NSString* username = self.mInputUserName;
    NSString* password = self.mInputPassword;
    GDataServiceGooglePhotos *service = [self googlePhotosService: username password:password];
    GDataServiceTicket *ticket;
    NSURL *feedURL = [GDataServiceGooglePhotos photoFeedURLForUserID:username
                                                             albumID:nil
                                                           albumName:nil
                                                             photoID:nil
                                                                kind:nil
                                                              access:nil];
    ticket = [service fetchFeedWithURL:feedURL
                              delegate:self
                     didFinishSelector:@selector(signInAlbumListFetchTicket:finishedWithFeed:error:)];
    [self setAlbumFetchTicket:ticket];
    [delegate handleSigningIn:self];
}
- (void)uploadImageAlbumListFetchTicket:(GDataServiceTicket *)ticket
            finishedWithFeed:(GDataFeedPhotoUser *)feed
                       error:(NSError *)error 
{
    [self setAlbumFeed:feed];
    [self setAlbumFetchError:error];
    [self setAlbumFetchTicket:nil];
    
    if(mAlbumFetchError)
    {
        NSDictionary* userinfo = [mAlbumFetchError userInfo];
        NSLog(@"code = %d", [mAlbumFetchError code]);
        NSArray* keys = [userinfo allKeys];
        BOOL logInError = NO;
        for(NSString* key in keys)
        {
            NSString* value = [userinfo objectForKey:key];
            NSLog(@"## key = %@ #", key);
            NSLog(@"## value = %@", [userinfo objectForKey:key]);
            if([key isEqualToString:@"Error"] && [value isEqualToString:@"BadAuthentication"])
            {
                logInError = YES;
                break;
            }
        }
        if(logInError)
        {
            [delegate handleAuthTestError:self];
        }
        else 
        {
            [delegate handleUploadError:self];
        }
    }
    else
    {
        [delegate handleAuthTestOK:self];
    }

}
- (void)realUploadImage
{
    [self setAlbumFeed:nil];
    [self setAlbumFetchError:nil];
    [self setAlbumFetchTicket:nil];
    
    [self setPhotoFeed:nil];
    [self setPhotoFetchError:nil];
    [self setPhotoFetchTicket:nil];
    
    NSString* username = mInputUserName;
    NSString* password = mInputPassword;
    NSLog(@"fetchAllAlbum : username = %@", username);
    GDataServiceGooglePhotos *service = [self googlePhotosService: username password:password];
    GDataServiceTicket *ticket;
    
    NSURL *feedURL = [GDataServiceGooglePhotos photoFeedURLForUserID:username
                                                             albumID:nil
                                                           albumName:nil
                                                             photoID:nil
                                                                kind:nil
                                                              access:nil];
    ticket = [service fetchFeedWithURL:feedURL
                              delegate:self
                     didFinishSelector:@selector(uploadImageAlbumListFetchTicket:finishedWithFeed:error:)];
    [self setAlbumFetchTicket:ticket];
}
// get the album selected in the top list, or nil if none
- (GDataEntryPhotoAlbum *)selectedAlbum {
    
    NSArray *albums = [mUserAlbumFeed entries];
    if ([albums count] > 0)
    {
        GDataEntryPhotoAlbum *album = [albums objectAtIndex:0];
        return album;
    }
    return nil;
}
- (void)ticket:(GDataServiceTicket *)ticket
hasDeliveredByteCount:(unsigned long long)numberOfBytesRead
ofTotalByteCount:(unsigned long long)dataLength
{
    
    mLastProgressDeliveredCount = numberOfBytesRead;
    mLastProgressTotalCount = dataLength;
    NSLog(@"## total size = %llu, delivered size = %llu", mLastProgressTotalCount, mLastProgressDeliveredCount);
    [delegate handleUploadProgress:dataLength :numberOfBytesRead];
}
// photo add callback
- (void)addPhotoTicket:(GDataServiceTicket *)ticket
     finishedWithEntry:(GDataEntryPhoto *)photoEntry
                 error:(NSError *)error {
    
    if (error == nil) 
    {
        [self setPicasaUserName:self.mInputUserName];
        [delegate handleUploadOK:self];
    }
    else 
    {
        // upload failed
        NSLog(@"## upload error = %@", [error description]);
        [delegate handleUploadError:self];
    }
    
}
- (void)uploadPhotoAtPath:(NSData*)photoData
                    path : (NSString*)photoPath
                uploadURL:(NSURL *)uploadURL 
{
    NSString* photoName = [photoPath lastPathComponent];
    if (photoData) 
    {
        
        // make a new entry for the photo
        GDataEntryPhoto *newEntry = [GDataEntryPhoto photoEntry];
        
        // set a title, description, and timestamp
        [newEntry setTitleWithString:photoName];
        [newEntry setPhotoDescriptionWithString:photoPath];
        [newEntry setTimestamp:[GDataPhotoTimestamp timestampWithDate:[NSDate date]]];
        
        // attach the NSData and set the MIME type for the photo
        [newEntry setPhotoData:photoData];
        
        NSString *mimeType = [GDataUtilities MIMETypeForFileAtPath:photoPath
                                                   defaultMIMEType:@"image/jpeg"];
        [newEntry setPhotoMIMEType:mimeType];
        
        // the slug is just the upload file's filename
        [newEntry setUploadSlug:photoName];
        
        NSString* username = mInputUserName;
        NSString* password = mInputPassword;
        // make service tickets call back into our upload progress selector
        GDataServiceGooglePhotos *service = [self googlePhotosService: username password:password];
        
        SEL progressSel = @selector(ticket:hasDeliveredByteCount:ofTotalByteCount:);
        [service setServiceUploadProgressSelector:progressSel];
        
        // insert the entry into the album feed
        GDataServiceTicket *ticket;
        ticket = [service fetchEntryByInsertingEntry:newEntry
                                          forFeedURL:uploadURL
                                            delegate:self
                                   didFinishSelector:@selector(addPhotoTicket:finishedWithEntry:error:)];
        
        // no need for future tickets to monitor progress
        [service setServiceUploadProgressSelector:nil];
        
    }
    else 
    {
        [delegate handleUploadError:self];
    }
}

// photo list fetch callback
- (void)photosTicket:(GDataServiceTicket *)ticket
    finishedWithFeed:(GDataFeedPhotoAlbum *)feed
               error:(NSError *)error 
{
    
    [self setPhotoFeed:feed];
    [self setPhotoFetchError:error];
    [self setPhotoFetchTicket:nil];
    
    //at this place upload image to picasa
    NSData* photoData = mPhotoData;//[delegate getUploadImage:mCurrentLoadImageIndex];
    //NSTimeInterval time = [[NSDate date] timeIntervalSince1970];
    NSString* photoName = mPhotoName;//[NSString stringWithFormat:@"TheSpeedSun_%f", time];
    GDataFeedPhotoAlbum *albumFeedOfPhotos = [self photoFeed];
    NSURL *uploadURL = [[albumFeedOfPhotos uploadLink] URL];
    NSLog(@"## uploadURL = %@", uploadURL);
    if(uploadURL != nil)
    {
        [self uploadPhotoAtPath:photoData path:photoName uploadURL:uploadURL];
    }
    else
    {
        [delegate handleUploadError:self];
    }
}
- (void) uploadPhoto: (NSData*)photoData : (NSString*)photoName
{
    self.mPhotoData = photoData;
    self.mPhotoName = photoName;
    [self uploadPhotoToSelectedAlbum];
}
// album creation callback
- (void)createAlbumTicket:(GDataServiceTicket *)ticket
        finishedWithEntry:(GDataEntryPhotoAlbum *)entry
                    error:(NSError *)error {
    if (error == nil)
    {
        NSLog(@"new album create");    
        [self authTest];
    } 
    else 
    {
        NSLog(@"create album error");
        [delegate handleUploadError:self];
    }
}
- (void)createAnAlbum 
{
    NSDate* date = [NSDate date];
    //NSTimeInterval time = [date timeIntervalSince1970];
    NSUInteger uflag;
    uflag = NSDayCalendarUnit | NSMonthCalendarUnit | NSYearCalendarUnit;
    NSCalendar* calendar = [NSCalendar currentCalendar];
    NSDateComponents* component = [calendar components:uflag fromDate:date];
    NSInteger currentDay = [component day];
    NSInteger currentMonth = [component month];
    NSInteger currentYear = [component year];
    NSString* dateStr = [NSString stringWithFormat:@"%d-%d-%d", currentYear, currentMonth, currentDay];
    NSString *albumName = dateStr;
    if ([albumName length] > 0) {
        
        NSString *description = [NSString stringWithFormat:@"Created %@",
                                 [NSDate date]];
        
        BOOL doCreateUnlisted = NO;//([mCreateAlbumUnlistedCheckbox state] == NSOnState);
        NSString *access = (doCreateUnlisted ? kGDataPhotoAccessPrivate : kGDataPhotoAccessPublic);
        
        GDataEntryPhotoAlbum *newAlbum = [GDataEntryPhotoAlbum albumEntry];
        [newAlbum setTitleWithString:albumName];
        [newAlbum setPhotoDescriptionWithString:description];
        [newAlbum setAccess:access];
        
        NSURL *postLink = [[mUserAlbumFeed postLink] URL];
        NSString* username = mInputUserName;
        NSString* password = mInputPassword;
        GDataServiceGooglePhotos *service = [self googlePhotosService: username password:password];
        
        [service fetchEntryByInsertingEntry:newAlbum
                                 forFeedURL:postLink
                                   delegate:self
                          didFinishSelector:@selector(createAlbumTicket:finishedWithEntry:error:)];
    }
}
- (void) uploadPhotoToSelectedAlbum
{
    GDataEntryPhotoAlbum *album = [self selectedAlbum];
    if (album)
    {
        // fetch the photos feed
        //mCurrentLoadImageIndex = 0;
        mLastProgressTotalCount = 0;
        mLastProgressDeliveredCount = 0;
        NSURL *feedURL = [[album feedLink] URL];
        if (feedURL) 
        {
            [self setPhotoFeed:nil];
            [self setPhotoFetchError:nil];
            [self setPhotoFetchTicket:nil];
            NSString* username = mInputUserName;
            NSString* password = mInputPassword;
            GDataServiceGooglePhotos *service = [self googlePhotosService: username password:password];
            GDataServiceTicket *ticket;
            ticket = [service fetchFeedWithURL:feedURL
                                      delegate:self
                             didFinishSelector:@selector(photosTicket:finishedWithFeed:error:)];
            [self setPhotoFetchTicket:ticket];
            
        }
    }
    else 
    {
        [self createAnAlbum];
    }
}


- (BOOL) isServiceSignIn
{
    /*
    NSString* userName = [self getPicasaUserName];
    NSString* password = [self getPicasaUserPassword];
    if(userName.length > 0 && password.length > 0)
    {
        return YES;
    }
    else 
    {
        return NO;
    }
     */
    return NO;
}
- (void) handleButtonCancel: (UIButton*)sender
{
    NSLog(@"handle button cancel");
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    [viewNav dismissDialogView];
}
- (void) handleButtonOK: (UIButton*)sender
{
    NSLog(@"handle button ok");
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    SEDialogView* dialog = [viewNav getDialogView];
    UIView* v = [dialog getContentView];
    UITextField* userNameView = (UITextField*)[ v viewWithTag:101];
    UITextField* passwordView = (UITextField*)[v viewWithTag:102];
    passwordView.secureTextEntry = YES;
    NSString* username = [NSString stringWithFormat:@"%@%@", userNameView.text, @"@gmail.com"];
    self.mInputUserName = username;
    self.mInputPassword = passwordView.text;
    //[self setPicasaUserName:userNameView.text];
    //[self setPicasaUserPassword:passwordView.text];
    [self realSignIn];
    //[viewNav getLoadingView]
    [viewNav dismissDialogView];
    /*
    [viewNav showLoadingView];
    SELoadingView* loadingView = [[PhotoFrameAppDelegate getViewNavigator] getLoadingView];
    [loadingView setStage:LOADING_STAGE3];
     */
    showLoadingViewForShare();
}
- (void) setLoginDialogHandler: (UIView*)dialog
{
    UIView* v1 = [dialog viewWithTag:103];
    UIView* v2 = [dialog viewWithTag:104];
    SETextImageButton* cancelButton = (SETextImageButton*)[dialog viewWithTag:103];
    SETextImageButton * okButton = (SETextImageButton*)[dialog viewWithTag:104];
    [cancelButton setButtonHandler:self action:@selector(handleButtonCancel:)];
    [okButton setButtonHandler:self action:@selector(handleButtonOK:)];
    [cancelButton setButtonBackground:@"ShareButtonNormal" select:@"ShareButtonH"];
    [okButton setButtonBackground:@"ShareButtonNormal" select:@"ShareButtonH"];
    [cancelButton setTextImage:@"cancel" indicateImage:nil alignment:UITextAlignmentCenter];
    [okButton setTextImage:@"ok" indicateImage:nil alignment:UITextAlignmentCenter];
    /*
    SEPopupButton* cancelButton = (SEPopupButton*)[dialog viewWithTag:103];
    SEPopupButton* okButton = (SEPopupButton*)[dialog viewWithTag:104];
    [okButton setButtonBackground:@"ShareButtonNormal" :@"ShareButtonH"];
    [okButton.button addTarget:self action:@selector(handleButtonOK:) forControlEvents:UIControlEventTouchUpInside];
    
    [cancelButton setButtonBackground:@"ShareButtonNormal" :@"ShareButtonH"];
    [cancelButton.button addTarget:self action:@selector(handleButtonCancel:) forControlEvents:UIControlEventTouchUpInside];
    setLabelFont(okButton.buttonText, @"Ok", [UIColor blackColor], [SESystemConfig getFontName], 20);
    setLabelFont(cancelButton.buttonText, @"Cancel", [UIColor blackColor], [SESystemConfig getFontName], 20);
    */
}
- (void) authTest
{
    [self realUploadImage];
    /*
    if([self isServiceSignIn])
    {
       
    }
    else
    {
        [self signIn];
    }
     */
}
- (void) saveUserNameHandler: (NSNumber*) bOk
{
    mSaveUserName = [bOk boolValue];
    [self setSaveUserNameSign:mSaveUserName];
    if(mSaveUserName == NO)
    {
        [[NSUserDefaults standardUserDefaults] removeObjectForKey:PICASA_USER_NAME_KEY];
        [[NSUserDefaults standardUserDefaults] synchronize];
    }
}
- (void) setViewBackground: (UIView*)v
{
    v.backgroundColor = [UIColor clearColor];
    UIImageView* bgView = (UIImageView*)[v viewWithTag:107];
    assert([bgView isKindOfClass:[UIImageView class]]);
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    UIImage* image = [viewNav.mResLoader getImage:@"ShareImageBackgroundTop"];
    bgView.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
    
    UIImageView* groupBg = (UIImageView*)[v viewWithTag:108];
    assert([groupBg isKindOfClass:[UIImageView class]]);
    image = [viewNav.mResLoader getImage:@"ShareImageURLBg"];
    groupBg.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
    
    SEOptionsLabel* userName = (SEOptionsLabel*)[v viewWithTag:105];
    assert([userName isKindOfClass:[SEOptionsLabel class]]);
    SEOptionsLabel* password = (SEOptionsLabel*)[v viewWithTag:106];
    assert([password isKindOfClass:[SEOptionsLabel class]]);
    image = [viewNav.mResLoader getImage:@"OptionsPlaySettingDrawQualityNumBg"];
    userName.background.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
    image = [viewNav.mResLoader getImage:@"OptionsPlaySettingDrawQualityNumBg"];
    password.background.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
    
    userName.label.backgroundColor = [UIColor clearColor];
    [userName.label setZFont:[[FontManager sharedManager] zFontWithName:[SESystemConfig getFontName]    pointSize:25]];
    userName.label.textColor = [UIColor blackColor];
    userName.label.text = @"User Name";
    userName.label.textAlignment = UITextAlignmentCenter;
    
    password.label.backgroundColor = [UIColor clearColor];
    [password.label setZFont:[[FontManager sharedManager] zFontWithName:[SESystemConfig getFontName]    pointSize:25]];
    password.label.textColor = [UIColor blackColor];
    password.label.textAlignment = UITextAlignmentCenter;
    password.label.text = @"Password";
    
    SEUISwitch* saveSwitch = (SEUISwitch*)[v viewWithTag:109];
    assert([saveSwitch isKindOfClass:[SEUISwitch class]]);
    [saveSwitch initImageView:[PhotoFrameAppDelegate getViewNavigator]];
    [saveSwitch setTarget:self action:@selector(saveUserNameHandler:)];
    mSaveUserName = [self getSaveUserNameSign];
    [saveSwitch setOn:mSaveUserName animated:NO];
    
    FontLabel* label = (FontLabel*)[v viewWithTag:110];
    assert([label isKindOfClass:[FontLabel class]]);
    [label setZFont:[[FontManager sharedManager] zFontWithName:[SESystemConfig getFontName] pointSize:25]];
    label.textColor = [UIColor colorWithRed:54.0/255 green:54.0/255 blue:54.0/255 alpha:1];
    
    FontLabel* gmailLabel = (FontLabel*)[v viewWithTag:111];
    assert([gmailLabel isKindOfClass:[FontLabel class]]);
    [gmailLabel setZFont:[[FontManager sharedManager] zFontWithName:[SESystemConfig getFontName] pointSize:25]];
    gmailLabel.textColor = [UIColor colorWithRed:54.0/255 green:54.0/255 blue:54.0/255 alpha:1];
    
    NSString* name = [self getPicasaUserName];
    if(name != nil && [name isEqualToString:@""] == NO)
    {
        UITextField* textFiled = (UITextField*)[v viewWithTag:101];
        assert([textFiled isKindOfClass:[UITextField class]]);
        textFiled.text = name;
    }
}
- (void) signIn
{
    UIView* v = [[[NSBundle mainBundle] loadNibNamed:@"PicasaLogin" owner:nil options:nil] lastObject];
    [self setViewBackground: v];
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    SEDialogView* dialog = [viewNav createDialogView];
    if(dialog)
    {
        [dialog setContentView:v];
        [viewNav showDialogView];
        [self setLoginDialogHandler: v];
    }
}
- (void) signOut
{
    [self setPicasaUserName:@""];
    [self setPicasaUserPassword:@""];
    [delegate handleSignOut:self];
}

@end
//////////////

///////////
@implementation SEDimView


@end
/////////////
@implementation SEUIRootView
//@synthesize imageView;
/*
- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"root touches began count = %u", [touches count]);
    
}
- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"root touches move count = %u", [touches count]);

    
}
- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"root touches end count = %u", [touches count]);
}
- (void)touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"root touch cancel");
}
*/
@end
/////////
static BOOL isHintFrame(CGRect hintFrame, CGRect frame)
{
    if(hintFrame.size.width != frame.size.width || hintFrame.size.height != frame.size.height)
        return NO;
    else
        return YES;
}
////////////////////

struct ViewRelation
{
    VIEW_TYPE curr;
    VIEW_TYPE prev;
    VIEW_TYPE next;
};

static enum VIEW_RELATION_TYPE gViewRelationType = TYPE1;
struct ViewRelation gViewRelation[] = {
    {MAIN_DISPLAY, INVALID_VIEW, INVALID_VIEW},
    {IMAGE_PICKER, MAIN_DISPLAY, SELECTED_IMAGE_VIEW},
    {SELECTED_IMAGE_VIEW, IMAGE_PICKER, MAIN_DISPLAY_MIRROR},
    {MUSIC_PICKER, MAIN_DISPLAY, SELECTED_MUSIC_VIEW},
    {SELECTED_MUSIC_VIEW, MUSIC_PICKER, MAIN_DISPLAY_MIRROR},
    {OPTIONS, MAIN_DISPLAY, INVALID_VIEW},
    {MUSIC_IMAGE_LIST_ATTACH, MAIN_DISPLAY, INVALID_VIEW},
    //{SIGNATURE_PREVIEW, OPTIONS, INVALID_VIEW},
    {SIGNATURE_VIEW, OPTIONS, INVALID_VIEW},
    
    {PREVIEW_3D, MAIN_DISPLAY, INVALID_VIEW}
    
}; 
struct ViewRelation gViewRelation2[] = {
    {MUSIC_IMAGE_LIST_ATTACH, MAIN_DISPLAY, INVALID_VIEW},
    {MUSIC_PICKER, MUSIC_IMAGE_LIST_ATTACH, SELECTED_MUSIC_VIEW},
    {IMAGE_PICKER, MUSIC_IMAGE_LIST_ATTACH, SELECTED_IMAGE_VIEW},
    {SELECTED_IMAGE_VIEW, IMAGE_PICKER, INVALID_VIEW},
    {SELECTED_MUSIC_VIEW, MUSIC_PICKER, INVALID_VIEW}
};
static VIEW_TYPE getPrevView(VIEW_TYPE curr)
{
    if(gViewRelationType == TYPE1)
    {
        int count = sizeof(gViewRelation) / sizeof(ViewRelation);
        for(int i = 0 ; i < count ; i++)
        {
            if(gViewRelation[i].curr == curr)
                return gViewRelation[i].prev;
        }
        return INVALID_VIEW;
    }
    else if(gViewRelationType == TYPE2)
    {
        int count = sizeof(gViewRelation2) / sizeof(ViewRelation);
        for(int i = 0 ; i < count ; i++)
        {
            if(gViewRelation2[i].curr == curr)
                return gViewRelation2[i].prev;
        }
        return INVALID_VIEW;
    }
    else 
        return INVALID_VIEW;
}
static VIEW_TYPE getNextView(VIEW_TYPE curr)
{
    if(gViewRelationType == TYPE1)
    {
        int count = sizeof(gViewRelation) / sizeof(ViewRelation);
        for(int i = 0 ; i < count ; i++)
        {
            if(gViewRelation[i].curr == curr)
                return gViewRelation[i].next;
        }
        return INVALID_VIEW; 
    }
    else if(gViewRelationType == TYPE2)
    {
        int count = sizeof(gViewRelation2) / sizeof(ViewRelation);
        for(int i = 0 ; i < count ; i++)
        {
            if(gViewRelation2[i].curr == curr)
                return gViewRelation2[i].next;
        }
        return INVALID_VIEW; 
    }
    else
        return INVALID_VIEW;
    
}
///

static BarViewType gBarViewType[] = {
    {IMAGE_PICKER, SELECTED_IMAGE_VIEW, YES, YES, YES},
    {MAIN_DISPLAY, IMAGE_PICKER, YES, NO, NO},
    {MAIN_DISPLAY, OPTIONS, YES, NO, NO},
    {MAIN_DISPLAY, PREVIEW_3D, NO, NO, NO},
    {MAIN_DISPLAY, MUSIC_PICKER, YES, NO, NO},
    {MAIN_DISPLAY, MUSIC_IMAGE_LIST_ATTACH, YES, NO, NO},
    {MUSIC_PICKER, SELECTED_MUSIC_VIEW, YES, YES, YES},
    {OPTIONS, SIGNATURE_VIEW, YES, NO, NO},
    {SIGNATURE_PREVIEW, SIGNATURE_VIEW, YES, NO, NO},
    {SELECTED_IMAGE_VIEW, MAIN_DISPLAY_MIRROR, NO, YES, NO},
    {SELECTED_MUSIC_VIEW, MAIN_DISPLAY_MIRROR, NO, YES, NO},
    //
    {MUSIC_IMAGE_LIST_ATTACH, IMAGE_PICKER, YES, NO, NO},
    {MUSIC_IMAGE_LIST_ATTACH, MUSIC_PICKER, YES, NO, NO}
}; 
static BOOL isBarViewTypeEqual(BarViewType bv1, BarViewType bv2)
{
    return bv1.leftView == bv2.leftView && bv1.rightView == bv2.rightView;
}
static int getBarViewTypeCount()
{
    return sizeof(gBarViewType) / sizeof(BarViewType);
}
static BarViewType getBarViewType(VIEW_TYPE v1, VIEW_TYPE v2)
{
    int count = sizeof(gBarViewType) / sizeof(BarViewType);
    for(int i = 0 ; i < count ; i++)
    {
        BarViewType bvt = gBarViewType[i];
        if(bvt.leftView == v1 && bvt.rightView == v2)
            return bvt;
    }
    BarViewType bvt;
    bvt.leftView = INVALID_VIEW;
    bvt.rightView = INVALID_VIEW;
    return bvt;
}
static BarViewType getBarViewTypeWithLeftView(VIEW_TYPE leftV)
{
    int count = sizeof(gBarViewType) / sizeof(BarViewType);
    for(int i = 0 ; i < count ; i++)
    {
        BarViewType bvt = gBarViewType[i];
        if(bvt.leftView == leftV)
            return bvt;
    }
    BarViewType bvt;
    bvt.leftView = INVALID_VIEW;
    bvt.rightView = INVALID_VIEW;
    return bvt;
}
static BarViewType getBarViewTypeWithRightView(VIEW_TYPE rightV)
{
    int count = sizeof(gBarViewType) / sizeof(BarViewType);
    for(int i = 0 ; i < count ; i++)
    {
        BarViewType bvt = gBarViewType[i];
        if(bvt.rightView == rightV)
            return bvt;
    }
    BarViewType bvt;
    bvt.leftView = INVALID_VIEW;
    bvt.rightView = INVALID_VIEW;
    return bvt;
}
static BOOL isBarViewTypeValid(BarViewType bvt)
{
    return bvt.leftView != INVALID_VIEW || bvt.rightView != INVALID_VIEW;
}

/////////
VIEW_TYPE gViewSequenceMainDisplayOnly[] = {MAIN_DISPLAY};
VIEW_TYPE gViewSequenceImagePicker[] = {MAIN_DISPLAY, IMAGE_PICKER, SELECTED_IMAGE_VIEW, MAIN_DISPLAY_MIRROR};
VIEW_TYPE gViewSequenceMusicPicker[] = {MAIN_DISPLAY, MUSIC_PICKER, SELECTED_MUSIC_VIEW, MAIN_DISPLAY_MIRROR};
VIEW_TYPE gViewSequenceOption[] = {MAIN_DISPLAY, OPTIONS};
VIEW_TYPE gViewSequenceMusicImageListAttach[] = {MAIN_DISPLAY, MUSIC_IMAGE_LIST_ATTACH};
VIEW_TYPE gViewSequenceOptionImagePicker[] = {OPTIONS, IMAGE_PICKER, SELECTED_IMAGE_VIEW};
VIEW_TYPE gViewSequenceOptionMusicPicker[] = {OPTIONS, MUSIC_PICKER};
//VIEW_TYPE gViewSequenceOptionsSignaturePreview[] = {OPTIONS, SIGNATURE_PREVIEW};
VIEW_TYPE gViewSequenceOptionsSignature[] = {OPTIONS, SIGNATURE_VIEW};

VIEW_TYPE gViewSequenceSignatureViewPreview[] = {SIGNATURE_PREVIEW, SIGNATURE_VIEW};
VIEW_TYPE gViewSequenceMusicListAttachImagePicker[] = {MUSIC_IMAGE_LIST_ATTACH, IMAGE_PICKER, SELECTED_IMAGE_VIEW};
VIEW_TYPE gViewSequenceMusicListAttachMusicPicker[] = {MUSIC_IMAGE_LIST_ATTACH, MUSIC_PICKER, SELECTED_MUSIC_VIEW};
VIEW_TYPE gViewSequencePreview3DOnly[] = {MAIN_DISPLAY, PREVIEW_3D};
struct ViewSeqProperty
{
    VIEW_TYPE* viewseq;
    VIEW_SEQ_TYPE type;
    int count;
};
ViewSeqProperty gViewSeqProps[] = {
    {gViewSequenceMainDisplayOnly, MAIN_DISPLAY_ONLY, sizeof(gViewSequenceMainDisplayOnly) / sizeof(VIEW_TYPE)},
    {gViewSequenceImagePicker, MAIN_DISPLAY_IMAGE_PICKER, sizeof(gViewSequenceImagePicker) / sizeof(VIEW_TYPE)},
    {gViewSequenceMusicPicker, MAIN_DISPLAY_MUSIC_PICKER, sizeof(gViewSequenceMusicPicker) / sizeof(VIEW_TYPE)},
    {gViewSequenceMusicImageListAttach, MAIN_DISPLAY_MUSIC_IMAGE_LIST_ATTACH, sizeof(gViewSequenceMusicImageListAttach) / sizeof(VIEW_TYPE)},
    {gViewSequenceOption, MAIN_DISPLAY_OPTIONS, sizeof(gViewSequenceOption) / sizeof(VIEW_TYPE)},
    {gViewSequenceOptionImagePicker, OPTIONS_IMAGE_PICKER, sizeof(gViewSequenceOptionImagePicker)/ sizeof(VIEW_TYPE)},
    {gViewSequenceOptionMusicPicker, OPTIONS_MUSIC_PICKER, sizeof(gViewSequenceOptionMusicPicker)/sizeof(VIEW_TYPE)},
    //{gViewSequenceOptionsSignaturePreview, OPTIONS_SIGNATURE, sizeof(gViewSequenceOptionsSignaturePreview) / sizeof(VIEW_TYPE)},
    {gViewSequenceOptionsSignature, OPTIONS_SIGNATURE, sizeof(gViewSequenceOptionsSignature) / sizeof(VIEW_TYPE)},
    //{gViewSequenceSignatureViewPreview, SIGNATURE_SIGNATURE_PREVIEW, sizeof(gViewSequenceSignatureViewPreview) / sizeof(VIEW_TYPE)},
    {gViewSequenceMusicListAttachImagePicker, MUSIC_IMAGE_LIST_ATTACH_IMAGE_PICKER, sizeof(gViewSequenceMusicListAttachImagePicker) / sizeof(VIEW_TYPE)},
    {gViewSequenceMusicListAttachMusicPicker, MUSIC_IMAGE_LIST_ATTACH_MUSIC_PICKER, sizeof(gViewSequenceMusicListAttachMusicPicker) / sizeof(VIEW_TYPE)},
    {gViewSequencePreview3DOnly, PREVIEW_3D_ONLY, sizeof(gViewSequencePreview3DOnly) / sizeof(VIEW_TYPE)}
};
static int getViewIndexInSequence(ViewSeqProperty vsp, VIEW_TYPE vp)
{
    for(int i = 0 ; i < vsp.count ; i++)
    {
        if(vp == vsp.viewseq[i])
            return i;
    }
    return -1;
}
static ViewSeqProperty getViewSeqProperty(VIEW_SEQ_TYPE type)
{
    int count = sizeof(gViewSeqProps) / sizeof(ViewSeqProperty);
    for(int i = 0 ; i < count ; i++)
    {
        if(gViewSeqProps[i].type == type)
            return gViewSeqProps[i];
    }
    ViewSeqProperty vsp;
    vsp.type = INVALID_SEQ_TYPE;
    vsp.viewseq = NULL;
    vsp.count = 0;
    return vsp;
}
static int getViewIndexInSequence(VIEW_SEQ_TYPE seqType, VIEW_TYPE vp)
{
    ViewSeqProperty vsp = getViewSeqProperty(seqType);
    if(vsp.type != INVALID_SEQ_TYPE)
    {
        return getViewIndexInSequence(vsp, vp);
    }
    else
        return -1;
}
static BOOL isValidViewSeqProp(ViewSeqProperty vsp)
{
    return  vsp.type != INVALID_SEQ_TYPE;
}
////////
struct ViewMirrorTypeData
{
    VIEW_TYPE origType;
    VIEW_TYPE mirrorType;
};
ViewMirrorTypeData gViewMirrorTypeData[] ={
    {MAIN_DISPLAY, MAIN_DISPLAY_MIRROR}
};
static VIEW_TYPE getMirrorType(VIEW_TYPE vp)
{
    int count = sizeof(gViewMirrorTypeData) / sizeof(ViewMirrorTypeData);
    for(int i = 0 ;i < count ; i++)
    {
        if(gViewMirrorTypeData[i].origType == vp)
            return gViewMirrorTypeData[i].mirrorType;
        else if(gViewMirrorTypeData[i].mirrorType == vp)
            return gViewMirrorTypeData[i].origType;
    }
    return INVALID_VIEW;
}
///////////
struct ViewLayoutData
{
    VIEW_TYPE vp;
    BOOL fullScreen;
};
ViewLayoutData gViewLayoutData[] = {
    {PREVIEW_3D, YES},
    {MAIN_DISPLAY, YES},
    {MAIN_DISPLAY_MIRROR, YES}
};
static BOOL isViewLayoutFullScreen(VIEW_TYPE vp)
{
    int count = sizeof(gViewLayoutData) / sizeof(ViewLayoutData);
    for(int i = 0 ; i < count ; i++)
    {
        if(gViewLayoutData[i].vp == vp)
            return gViewLayoutData[i].fullScreen;
    }
    return NO;
}
///////////////
struct ViewBarBackground
{
    VIEW_TYPE vp;
    VIEW_TYPE vpNext;
    NSString* key;
};
ViewBarBackground gViewBarBackground[] = {
    {MAIN_DISPLAY, IMAGE_PICKER, @"MainDisplayImagePickerBarBackground"},
    {IMAGE_PICKER, SELECTED_IMAGE_VIEW,@"ImagePickerSelectedImageViewBarBackground"},
    {SELECTED_IMAGE_VIEW, MAIN_DISPLAY_MIRROR,@"SelectedImageViewMainDisplayBarBackground"}, 
    {MAIN_DISPLAY, MUSIC_PICKER, @"MainDisplayMusicPickerBarBackground"},
    {MUSIC_PICKER, SELECTED_MUSIC_VIEW, @"MusicPickerSelectedMusicViewBarBackground"},
    {SELECTED_MUSIC_VIEW, MAIN_DISPLAY_MIRROR, @"SelectedMusicViewMainDisplayBarBackground"},
    {MAIN_DISPLAY, OPTIONS, @"OptionBarBackground"},
    {MAIN_DISPLAY, MUSIC_IMAGE_LIST_ATTACH, @"MainDisplayMusicImageAttachBarBackground"},
    {MUSIC_IMAGE_LIST_ATTACH, IMAGE_PICKER, @"MusicImageAttachImagePickerBarBackground"},
    {MUSIC_IMAGE_LIST_ATTACH, MUSIC_PICKER, @"MusicImageAttachMusicPickerBarBackground"},
    {OPTIONS, SIGNATURE_VIEW, @"OptionsSignatureViewBarBg"},
    /////
    {MAIN_DISPLAY, SIGNATURE_VIEW, @"MainDisplayBarBackground"},
    {MAIN_DISPLAY, SIGNATURE_PREVIEW, @"MainDisplayBarBackground"}
};
static NSString* getViewBarBackgroundKey(VIEW_TYPE vp, VIEW_TYPE vpNext)
{
    int count = sizeof(gViewBarBackground) / sizeof(ViewBarBackground);
    for(int i = 0 ; i < count ; i++)
    {
        if(vp == gViewBarBackground[i].vp && vpNext == gViewBarBackground[i].vpNext)
            return gViewBarBackground[i].key;
    }
    return nil;
}
////////

@implementation SELoadingButton
@synthesize buttonText;
@synthesize button;
- (void) createChild:(CGRect)frame
{
    button = [UIButton buttonWithType:UIButtonTypeCustom];
    button.frame = CGRectMake(0, 0, frame.size.width, frame.size.height);
    UIImage* normalImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"AppStoreButtonNormalBg"];
    normalImage = [SEUtil imageWithCap:normalImage top:0.1 bottom:0.9 left:0.1 right:0.9];
    //UIImage* selectImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"AppStoreButtonSelectBg"];
    UIImage* selectImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"OptionsStoreItemSelectedBg"];
    selectImage = [SEUtil imageWithCap:selectImage top:0.1 bottom:0.9 left:0.1 right:0.9];
    [button setBackgroundImage:normalImage forState:UIControlStateNormal];
    [button setBackgroundImage:selectImage forState:UIControlStateHighlighted];
    [self addSubview:button];
    
    buttonText = [[FontLabel alloc] initWithFrame:CGRectMake(0, 0, frame.size.width, frame.size.height) fontName:[SESystemConfig getFontName] pointSize:30];
    [self addSubview:buttonText];
    [buttonText release];
    buttonText.textAlignment = UITextAlignmentCenter;
    self.backgroundColor = [UIColor clearColor];
}
- (id) initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if(self)
    {
        [self createChild:frame];
    }
    return self;
}
- (id) initWithCoder:(NSCoder *)aDecoder
{
    self = [super initWithCoder:aDecoder];
    if(self)
    {
        CGRect frame = self.frame;
        [self createChild:frame];
    }
    return self;
}



@end
///////
@implementation SELoadingView
@synthesize mCurrentStage;
- (id) initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if(self)
    {
        //backgroundView = [[UIImageView alloc] initWithFrame:frame];
        self.backgroundColor = [UIColor blackColor];
        self.alpha = 0.5;
        UIImage* image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"LoadingIcon"];
        CGSize s = CGSizeMake(image.size.width / 2, image.size.height/2);
        CGRect r = CGRectMake((frame.size.width - s.width) / 2, (frame.size.height - s.height) / 2, s.width, s.height);
        iconView = [[UIImageView alloc] initWithFrame:r];
        iconView.image = image;
        [self addSubview:iconView];
        [iconView release];
        
    
        stageView = [[UIImageView alloc] initWithFrame:r];
        [self addSubview:stageView];
        [stageView release];
        mCurrentStage = NO_LOADING_STAGE;
    }
    return self;
}
- (void) setPercent: (float) v
{
    if(v > 1)
        v = 1;
    if(v < 0)
        v = 0;
    [mProgressView setPercent:v];
}
- (void) setText: (NSString*) text
{
    mTextLabel.label.text = text;
}

- (BOOL) isUseProgressView
{
    return mProgressView != nil;
}
- (BOOL) isUseTextView
{
    return mTextLabel != nil;
}
- (SELoadingButton*) getExitButton
{
    return mButtonExit;
}
- (SELoadingButton*) getContinueButton
{
    return mButtonContinue;
}
- (void) showButton : (int) type
{
    [mTextLabel2 removeFromSuperview];
    mTextLabel2 = nil;
    CGRect r = CGRectMake(iconView.frame.origin.x - 250, mTextLabel.frame.origin.y, iconView.frame.size.width + 500, 180);
    mTextLabel.frame = r;
    mTextLabel.label.frame = CGRectMake(0, 0, r.size.width, r.size.height);
    //mTextLabel.backgroundColor = [UIColor redColor];
    mTextLabel.label.lineBreakMode = UILineBreakModeWordWrap;
    mTextLabel.label.numberOfLines = 3;
    //mTextLabel.label.textAlignment = UITextAlignmentLeft;
    if(mButtonContinue == nil && mButtonExit == nil)
    {
        if(type == SE_LOADINVIEW_TWO_BUTTON)
        {
            CGRect rect1, rect2;
            CGFloat y = mTextLabel.frame.origin.y + mTextLabel.frame.size.height;
            rect1 = CGRectMake(iconView.frame.origin.x, y, 150, 60);
            rect2 = CGRectMake(rect1.origin.x + rect1.size.width + 30, y, 150, 60);
            mButtonContinue = [[SELoadingButton alloc] initWithFrame:rect1];
            [self addSubview:mButtonContinue];
            [mButtonContinue release];
            
            mButtonExit = [[SELoadingButton alloc] initWithFrame:rect2];
            [self addSubview:mButtonExit];
            [mButtonExit release];
        }
        else if(type == SE_LOADINGVIEW_ONE_BUTTON)
        {
            CGRect rect1;
            CGFloat y = mTextLabel.frame.origin.y + mTextLabel.frame.size.height;
            CGFloat x = iconView.frame.origin.x + (iconView.frame.size.width - 150) / 2;
            rect1 = CGRectMake(x, y, 150, 60);
            mButtonExit = [[SELoadingButton alloc] initWithFrame:rect1];
            [self addSubview:mButtonExit];
            [mButtonExit release];
        }
    }
}
- (void) useTextView
{
    stageView.hidden = YES;
    if(mTextLabel == nil)
    {
        //CGRect r = iconView.frame;
        CGSize textSize = CGSizeMake(iconView.frame.size.width + 130, 61);
        
        CGRect r = CGRectMake((self.frame.size.width - textSize.width)/ 2, iconView.frame.origin.y + iconView.frame.size.height + 10, textSize.width, textSize.height);
        mTextLabel = [[SEOptionsLabel alloc] initWithFrame:r];
        [mTextLabel setTextCenter:YES];
        [self addSubview:mTextLabel];
        [mTextLabel release];
        mTextLabel.label.textAlignment = UITextAlignmentCenter;
        setLabelFont(mTextLabel.label, @"", [UIColor whiteColor], [SESystemConfig getFontName], 30);
        
        mTextLabel2 = [[SEOptionsLabel alloc] initWithFrame:CGRectMake(r.origin.x, mTextLabel.frame.origin.y + mTextLabel.frame.size.height + 10, r.size.width, r.size.height)];
        [mTextLabel2 setTextCenter:YES];
        [self addSubview:mTextLabel2];
        [mTextLabel2 release];
        mTextLabel2.label.textAlignment = UITextAlignmentCenter;
        setLabelFont(mTextLabel2.label, @"", [UIColor whiteColor], [SESystemConfig getFontName], 30);
    }
}
- (void) setText2: (NSString*)text
{
    mTextLabel2.label.text = text;
}
- (void) useProgressView
{
    stageView.hidden = YES;
    if(mProgressView == nil)
    {
        SEUIProgressView* progressView = [[SEUIProgressView  alloc] initWithFrame:CGRectMake(iconView.frame.origin.x, iconView.frame.origin.y + iconView.frame.size.height + 20, iconView.frame.size.width, 60)];
        [self addSubview:progressView];
        [progressView initData:[PhotoFrameAppDelegate getViewNavigator]];
        [progressView release];
        mProgressView = progressView;
    }
}
- (void) showStageView
{
    stageView.hidden = NO;
}
- (void) setStage: (enum LOADING_STAGE_TYPE) stage
{
    UIImage* image = nil;
    if(stage == mCurrentStage)
        return;
    mCurrentStage = stage;
    NSLog(@"stage = %d", stage);
    switch (stage) {
        case LOADING_STAGE1:
        {
            image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"LoadingIconStage1"];

        }
            break;
        case LOADING_STAGE2:
        {
            image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"LoadingIconStage2"];
        }
            break;
        case LOADING_STAGE3:
        {
            image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"LoadingIconStage3"];
        }
            break;
        case NO_LOADING_STAGE:
        {
            image = nil;
        }
            break;
        default:
            break;
    }
    stageView.image = image;
    [stageView setNeedsDisplay];
}
- (void) setIconViewTransform: (CGAffineTransform) transform
{
    iconView.transform = transform;
    [iconView setNeedsDisplay];
    //stageView.transform = transform;
    //[stageView setNeedsDisplay];
}
@end
/////////////
enum {LEFT_PART, RIGHT_PART};
/////
@implementation SEImageListProperty
@synthesize imageCount;
@synthesize name;
@synthesize firstURLString;
- (void)dealloc
{
    [firstURLString release];
    [name release];
    [super dealloc];
}
@end
@implementation SEMusicListProperty

@synthesize musicCount;
@synthesize name;
@synthesize firstURLString;
- (void) dealloc
{
    [name release];
    [firstURLString release];
    [super dealloc];
}
@end
////////
@implementation SEContentContainerLabel


- (BOOL) canAdjust
{
    return FALSE;
}

- (void) relayout
{}
- (void) update
{}
@end
/////////
@implementation SEContentViewContainer
@synthesize mHintRect;
@synthesize mType;
@synthesize mViewNav;
@synthesize mBarBackgroundKey;
@synthesize mBackgroundImage;
@synthesize mStatusLabelRightX;
@synthesize mTitleLabelMidX;
@synthesize mStatusLabelMidX;
@synthesize mTitleLabelLeftX;
@synthesize mStatusLabelWidth;
@synthesize mStatusLabelHeight;
@synthesize mStatusLabelXRelativeToBar;
@synthesize mTitleLabelXRelativeToBar;
- (void) stop
{
    /*
    UIView<SEAdjustContentView>* content = [self contentView];
    if(content == nil)
        return;
    if([content respondsToSelector:@selector(stopAsync:::)])
    {
        
    }
    else
    {
        [content removeFromSuperview];
    }
     */
}
- (void) removeContentViewLater: (id) obj
{
    UIView* v = (UIView*)obj;
    [v release];
}
- (void) createTitleLabel : (CGRect)frame fontName: (NSString*)fontName fontSize: (CGFloat)fontSize  textColor:(UIColor*)color 
{
    if(mTitleLabel == nil)
    {
        SEContentContainerLabel* labelName = [[[SEContentContainerLabel alloc] initWithFrame:frame fontName:fontName pointSize:fontSize] autorelease];
        labelName.textColor = color;
        labelName.backgroundColor = [UIColor clearColor];
        [self addSubview:labelName];
        mTitleLabel = labelName;
    }
    else {
        mTitleLabel.frame = frame;
    }
}
- (void) createStatusLabel: (CGRect)frame fontName: (NSString*)fontName fontSize: (CGFloat)fontSize  textColor:(UIColor*)color
{
    if(mStatusLabel == nil)
    {
        SEContentContainerLabel* statusLabel = [[[SEContentContainerLabel alloc] initWithFrame:frame fontName:fontName pointSize:fontSize] autorelease];
        statusLabel.backgroundColor = [UIColor clearColor];
        statusLabel.textColor = color;
        [self addSubview:statusLabel];
        mStatusLabel = statusLabel;
    }
    else
    {
        mStatusLabel.frame = frame;    
    }
}
- (void) setTitleText: (NSString*) str
{
    mTitleLabel.text = str;
}
- (void) setStatusText: (NSString*)str
{
    mStatusLabel.text = str;
}
- (SEContentContainerLabel*) getTitleLabel
{
    return mTitleLabel;
}
- (SEContentContainerLabel*) getStatusLabel
{
    return mStatusLabel;
}
- (void) removeContentView
{
    UIView<SEAdjustContentView>* content = [self contentView];
    if(content == nil)
        return;
    Class cl = [SEPageUIScrollView class];
    if([content isMemberOfClass:cl])
    {
        SEPageUIScrollView* scrollView = (SEPageUIScrollView*)content;
        [scrollView stopLoadImage];
        mViewNav.mSelectedImage = [NSArray array];
        mViewNav.mSelectedImageViewIndex = [NSArray array];
        mViewNav.mSelectedPhotoURL = [NSArray array];
        mStatusLabel.text = @"";
    }
    [content removeFromSuperview];
    [mTitleLabel removeFromSuperview];
    [mStatusLabel removeFromSuperview];
    mTitleLabel = nil;
    mStatusLabel = nil;
}
- (void) drawRect:(CGRect)rect
{
    if(mBackgroundImage)
    {
        if(mBackgroundImage.size.width == rect.size.width && mBackgroundImage.size.height == rect.size.height)
            [mBackgroundImage drawInRect:rect];
        else
        {
            UIImage* image = [SEUtil drawImage:mBackgroundImage inRect:CGRectMake(0, 0, rect.size.width, rect.size.height)];
            [image drawInRect:rect];
        }
    }
}
- (BOOL) hasContent
{
    if(self.subviews.count == 0)
        return NO;
    int count = 0;
    for(UIView* v in self.subviews)
    {
        if([v isMemberOfClass:[SEContentContainerLabel class]] == NO)
        {
            count++;
        }
    }
    return count > 0;
}
- (void) setTitleStatusFrameToOrigin
{
    
    mTitleLabel.frame = CGRectMake(mTitleLabelLeftX, mTitleLabel.frame.origin.y, mTitleLabel.frame.size.width, mTitleLabel.frame.size.height);
    mStatusLabel.frame = CGRectMake(mStatusLabelRightX, mStatusLabel.frame.origin.y , mStatusLabel.frame.size.width, mStatusLabel.frame.size.height);
}
- (void) updateContent
{
    UIView<SEAdjustContentView>* content = [self contentView];
    [self setTitleStatusFrameToOrigin];
    if(content)
        [content update];
}
- (void) adjustContentViewLayout: (float) contentViewWidth part: (int)partType barStopInMid:(BOOL)bStopInMid
{
    UIView<SEAdjustContentView>* contentView = [self contentView];
    if([contentView isMemberOfClass:[SEPageUIScrollView class]])
    {
        //SEPageUIScrollView* pageScrollView = (SEPageUIScrollView*)contentView;
        if(bStopInMid)
        {
            [self decreaseContent:contentViewWidth part:partType];
        }
    }
    else if([contentView respondsToSelector:@selector(handleWhenStop:)])
    {
        [contentView handleWhenStop:bStopInMid];
    }
    else
    {
        CGRect frame = contentView.frame;
        if(contentViewWidth == frame.size.width)
            return;
        if(contentViewWidth != self.frame.size.width)
        {
            if(mType == IMAGE_PICKER)
            {
                contentView.frame = CGRectMake(self.frame.size.width - contentViewWidth, frame.origin.y, contentViewWidth, frame.size.height);
            }
            else if(mType == SELECTED_IMAGE_VIEW)
            {
                contentView.frame = CGRectMake(0, 0, contentViewWidth, frame.size.height);
            }
        }
        else
        {
            contentView.frame = self.bounds;
        }
        if([contentView isMemberOfClass:[SEUIScrollView class]])
        {
            SEUIScrollView* scrollView = (SEUIScrollView*)contentView;
            scrollView.mViewWidth = contentViewWidth;
        }
    }
}
- (UIView<SEAdjustContentView>*) contentView
{
    if([self.subviews count] > 0)
    {
        NSArray* subviews = self.subviews;
        for(int i = 0 ; i < subviews.count ; i++)
        {
            UIView* v = [subviews objectAtIndex:i];
            if([v isMemberOfClass:[SEContentContainerLabel class]] == NO)
                return [self.subviews objectAtIndex:i];
        }
        return nil;
    }
    else
        return nil;
}
- (void) saveContext : (NSArray*)userInfoArray
{
    switch (mType) 
    {
        case IMAGE_PICKER:
        {}
            break;
        case SELECTED_IMAGE_VIEW:
        {
        }
            break;
        case MUSIC_PICKER:
        {
        }
            break;
        case SIGNATURE_PREVIEW:
        {
            SESignaturePreview* sigPreView = (SESignaturePreview*)[self contentView];
            int site = sigPreView.mCurrentSite;
            int size = sigPreView.mCurrentSize;
            UserInfo* userInfo = [userInfoArray objectAtIndex:0];
            userInfo.currentsignaturesite = [NSNumber numberWithInt: site];
            userInfo.currentsignaturesize = [NSNumber numberWithInt: size];
        }
            break;
        case SIGNATURE_VIEW:
        {
            SESignatureView* sigView = (SESignatureView*)[self contentView];
            UserInfo* userInfo = [userInfoArray objectAtIndex:0];
            [sigView saveContext:userInfo];
        }
            break;
        default:
            break;
    }
}

- (BOOL) canStopInMid
{
    return YES;
}
- (id) initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if(self)
    {
        mStatusLabelWidth = 63;
        mStatusLabelHeight = 63;
        mTitleLabelLeftX = 0;
        mStatusLabelRightX = self.frame.size.width - mStatusLabelWidth;
        float barWidth = [[PhotoFrameAppDelegate getViewNavigator] mBarWidth];
        float viewPortWidth = [[PhotoFrameAppDelegate getViewNavigator] mViewPortWidth];
        float d = (viewPortWidth - barWidth) / 2 - mStatusLabelWidth - barWidth;
        mTitleLabelMidX = mStatusLabelRightX - d;
        mStatusLabelMidX = d;

    }
    return self;
}
- (void) initContainer
{

}
- (void)dealloc
{
    [mBackgroundImage release];
    [mBarBackgroundKey release];
    [super dealloc];
}
- (void) expandContent:(float) contentWidth part: (int)partType
{
    UIView<SEAdjustContentView>* v = [self contentView];
    if([v isMemberOfClass:[SEPageUIScrollView class]])
    {
        SEPageUIScrollView* pageScrollView = (SEPageUIScrollView*)v;
        if([pageScrollView.mName isEqualToString:@"image_picker"])
        {
            [pageScrollView changeContentToMultipleColumn:contentWidth movePart:LEFT_PAGE];
        }
        else if([pageScrollView.mName isEqualToString:@"image_selected_view"])
        {
            [pageScrollView changeContentToMultipleColumn:contentWidth movePart:RIGHT_PAGE];
        }
    }    
}
- (void) decreaseContent: (float) contentWidth part: (int)partType
{
    UIView<SEAdjustContentView>* v = [self contentView];
    if([v isMemberOfClass:[SEPageUIScrollView class]])
    {
        SEPageUIScrollView* pageScrollView = (SEPageUIScrollView*)v;
        if([pageScrollView.mName isEqualToString:@"image_picker"])
        {
            [pageScrollView changeContentToSingleColumn:contentWidth movePart:LEFT_PAGE];
        }
        else if([pageScrollView.mName isEqualToString:@"image_selected_view"])
        {
            [pageScrollView changeContentToSingleColumn:contentWidth movePart:RIGHT_PAGE];
        }
    }
}
- (BOOL) isContentAnimEnd
{
    UIView* v = [self contentView];
    if([v isMemberOfClass:[SEPageUIScrollView class]] )
    {
        SEPageUIScrollView* scrollView = (SEPageUIScrollView*)v;
        return scrollView.mIsAnimEnd && scrollView.mIsScrolling == NO;
    }
    else
        return YES;
}
@end

//////////////
#define FIRST_CONTENTVIEW_WIDTH 0
#define MID_CONTENTVIEW_WIDTH 1024 / 2

@implementation SEBarView
@synthesize mHintRect;
@synthesize mLeftContentContainer;
@synthesize mRightContentContainer;
@synthesize mViewNav;
@synthesize mBarViewType;
@synthesize mCanStopInMid;
@synthesize mResLoader;
@synthesize mContainerAnimHandler;
@synthesize mStayOnMid;
@synthesize mIsBarAnimationEnd;

- (void)drawRect:(CGRect)rect
{
    if(mLeftContentContainer.mType == SELECTED_IMAGE_VIEW)
    {
        NSLog(@"## selected image view left ###");
    }
    if(mLeftContentContainer.mBackgroundImage)
    {
        CGSize s = CGSizeMake(self.frame.size.width, self.frame.size.height);
        UIGraphicsBeginImageContext(s);
        CGFloat imageWidth = mLeftContentContainer.mBackgroundImage.size.width;
        //CGFloat imageHeight = mLeftContentContainer.mBackgroundImage.size.height;
        [mLeftContentContainer.mBackgroundImage drawAtPoint:CGPointMake(s.width - imageWidth, 0)];
        UIImage* image = UIGraphicsGetImageFromCurrentImageContext();
        UIGraphicsEndImageContext();
        [image drawInRect:rect];
    }
}
- (void) setArrowVisibility
{
    if(mLeftContentContainer.mType == MAIN_DISPLAY && mRightContentContainer.mType == OPTIONS)
    {
        mRightArrowButton.hidden = YES;
    }
    if(mLeftContentContainer.mType == MAIN_DISPLAY && mRightContentContainer.mType == MUSIC_IMAGE_LIST_ATTACH)
    {
        mRightArrowButton.hidden = YES;
    }
    if(mLeftContentContainer.mType == OPTIONS && mRightContentContainer.mType ==SIGNATURE_VIEW)
    {
        mRightArrowButton.hidden = YES;
    }
}
- (void) createChildView: (CGRect)frame
{
    CGRect r ;
    UIImage* rightArrowImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"BarViewRightArrow"];
    r.origin.x = (frame.size.width - rightArrowImage.size.width) / 2;
    r.origin.y = frame.size.height - frame.size.height / 3 + 5;
    r.size.width = rightArrowImage.size.width;
    r.size.height = rightArrowImage.size.height;
    //mArrowView = [[UIImageView alloc] initWithFrame:r];
    mRightArrowButton = [UIButton buttonWithType:UIButtonTypeCustom];
    mRightArrowButton.frame = r;
    UIImage* rightArrowImageH = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"BarViewRightArrowH"];
    [mRightArrowButton setBackgroundImage:rightArrowImage forState:UIControlStateNormal];
    [mRightArrowButton setBackgroundImage:rightArrowImageH forState:UIControlStateHighlighted];
    [self addSubview:mRightArrowButton];
    [mRightArrowButton addTarget:self action:@selector(rightArrowButtonHandler:) forControlEvents:UIControlEventTouchUpInside];
    
    r.origin.y += rightArrowImage.size.height - 13;
    mLeftArrowButton = [UIButton buttonWithType:UIButtonTypeCustom];
    mLeftArrowButton.frame = r;
    UIImage* leftArrowImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"BarViewLeftArrow"];
    UIImage* leftArrowImageH = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"BarViewLeftArrowH"];
    [mLeftArrowButton setBackgroundImage:leftArrowImage forState:UIControlStateNormal];
    [mLeftArrowButton setBackgroundImage:leftArrowImageH forState:UIControlStateHighlighted];
    [self addSubview:mLeftArrowButton];
    [mLeftArrowButton addTarget:self action:@selector(leftArrowButtonHandler:) forControlEvents:UIControlEventTouchUpInside];
    [self setArrowVisibility];
}
- (id) initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if(self)
    {
        [self createChildView:frame];
    }
    return self;
}
- (void) initBackground
{
    CGRect bounds = self.bounds;
    NSString* bgKey = getViewBarBackgroundKey(mLeftContentContainer.mType, mRightContentContainer.mType);
    //NSString* leftBgKey = mLeftContentContainer.mBarBackgroundKey;
    //NSString* rightBgKey = mRightContentContainer.mBarBackgroundKey;
    UIImage* leftImage = [mResLoader getImage:bgKey];
    //UIImage* rightImage = [mResLoader getImage:rightBgKey];
    mLeftImageView = [[UIImageView alloc] initWithFrame:bounds];
    //mRightImageView = [[UIImageView alloc] initWithFrame:bounds];
    [self addSubview:mLeftImageView];
    //[self addSubview:mRightImageView];
    [mLeftImageView release];
    //[mRightImageView release];
    mLeftImageView.image = leftImage;
    [self setArrowVisibility];
    //mRightImageView.image = rightImage;
    //self.backgroundColor = [UIColor clearColor];
    
    //CGAffineTransform reverse = CGAffineTransformMakeScale(-1, -1);
    //mLeftImageView.transform = reverse;
}
- (BAR_LOC_TYPE) barLocationType
{
    if(mLeftContentContainer.mType == mViewNav.prevView && !mStayOnMid )
        return LEFT_BAR;
    else if(mRightContentContainer.mType == mViewNav.nextView && !mStayOnMid)
        return RIGHT_BAR;
    else if(mStayOnMid)
    {
        return MID_BAR;
    }
    else
    {
        return INVALID_BAR;
    }
}
- (void) calculateTitleStatusRelativeCoord
{
    if([mLeftContentContainer getTitleLabel] == nil || [mLeftContentContainer getStatusLabel] == nil || [mRightContentContainer getTitleLabel] == nil || [mRightContentContainer getStatusLabel] == nil)
        return;
    CGPoint barStartPointInRootView = [self convertPoint:CGPointMake(0, 0) toView:mViewNav.mRootView];
    CGPoint barEndPointInRootView = [self convertPoint:CGPointMake(self.frame.size.width, 0) toView:mViewNav.mRootView];
    CGPoint leftStatusStartPoinInRootView = [[mLeftContentContainer getStatusLabel] convertPoint:CGPointMake(0, 0) toView:mViewNav.mRootView];
    CGPoint rightTitleStartPointInRootView = [[mRightContentContainer getTitleLabel] convertPoint:CGPointMake(0, 0) toView:mViewNav.mRootView];
    mLeftContentContainer.mStatusLabelXRelativeToBar = barStartPointInRootView.x - leftStatusStartPoinInRootView.x;
    mRightContentContainer.mTitleLabelXRelativeToBar = barStartPointInRootView.x - rightTitleStartPointInRootView.x;
}
- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"bar view touches began, mIsBarAnimationEnd = %d, leftContainer = %d, rightContainer = %d, bar type = %d, %d", mIsBarAnimationEnd, [mLeftContentContainer isContentAnimEnd], [mRightContentContainer isContentAnimEnd], mBarViewType.leftView, mBarViewType.rightView);
    if(mIsBarAnimationEnd == NO || [mLeftContentContainer isContentAnimEnd] == NO || [mRightContentContainer isContentAnimEnd] == NO )
        return;
    BAR_LOC_TYPE barLocType = [self barLocationType];
    if(barLocType == INVALID_BAR)
        return;
    mTouchStart = YES;
    mOrig = [[touches anyObject] locationInView:mViewNav.mRootView];
    
    mFirstMove = YES;
    if(barLocType == LEFT_BAR)
    {
        mLeftContentContainer.hidden = NO;
        [mViewNav addContentToContentContainer:mLeftContentContainer.mType];
        UIView* v = [mLeftContentContainer getTitleLabel];
        v.frame = CGRectMake(mLeftContentContainer.mTitleLabelMidX, v.frame.origin.y, v.frame.size.width, v.frame.size.height);
    }
    else if(barLocType == RIGHT_BAR)
    {
        mRightContentContainer.hidden = NO;
        [mViewNav addContentToContentContainer:mRightContentContainer.mType];
        UIView* v = [mRightContentContainer getStatusLabel];
        v.frame = CGRectMake(mRightContentContainer.mStatusLabelMidX, v.frame.origin.y, v.frame.size.width, v.frame.size.height);
    }
    else if(barLocType == MID_BAR)
    {
        NSLog(@"touch at mid bar\n");
        
    }
    else
    {
        NSLog(@"bar type error\n");
        assert(0);
    }
}
- (void) setContainerTitleStatusLabelFrame: (CGFloat) deltax
{
    CGPoint barStartPointInRootView = [self convertPoint:CGPointMake(deltax, 0) toView:mViewNav.mRootView];
    CGPoint barEndPointInRootView = [self convertPoint:CGPointMake(mViewNav.mBarWidth + deltax, 0) toView:mViewNav.mRootView];
    CGFloat midBarStartPointInRootView = (mViewNav.mViewPortWidth - mViewNav.mBarWidth) / 2;
    
    if(barStartPointInRootView.x > midBarStartPointInRootView)
    {
        CGPoint p = [mViewNav.mRootView convertPoint:CGPointMake(mViewNav.mBarWidth + deltax, 0) toView:mLeftContentContainer];
        UIView* leftTitleLabel = [mLeftContentContainer getTitleLabel];
        leftTitleLabel.frame = CGRectMake(p.x, leftTitleLabel.frame.origin.y, leftTitleLabel.frame.size.width, leftTitleLabel.frame.size.height);
    }
    if((mViewNav.mViewPortWidth - barEndPointInRootView.x) > midBarStartPointInRootView)
    {
        CGPoint p = [mViewNav.mRootView convertPoint:CGPointMake(mViewNav.mViewPortWidth - mRightContentContainer.mStatusLabelWidth - mViewNav.mBarWidth + deltax, 0) toView:mRightContentContainer];
        UIView* rightStatusLabel = [mRightContentContainer getStatusLabel];
        rightStatusLabel.frame = CGRectMake(p.x, rightStatusLabel.frame.origin.y, rightStatusLabel.frame.size.width, rightStatusLabel.frame.size.height);
    }
}
- (void) moveContainerTitleStatusLabel: (CGFloat) deltax
{
    if([mLeftContentContainer getTitleLabel] == nil || [mLeftContentContainer getStatusLabel] == nil || [mRightContentContainer getTitleLabel] == nil || [mRightContentContainer getStatusLabel] == nil)
        return;
    [self setContainerTitleStatusLabelFrame:0];

}
- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event
{
    //NSLog(@"bar view touches move count = %u", [touches count]);
    if(mTouchStart == NO)
        return;
    CGPoint loc = [[touches anyObject] locationInView:mViewNav.mRootView];
    CGFloat deltax = loc.x - mOrig.x;
    if(deltax > 0)
    {
        mDirect = MOVE_RIGHT;
    }
    else if(deltax < 0)
    {
        mDirect = MOVE_LEFT;
    }
    CGPoint p = [self convertPoint:CGPointMake(0, 0) toView:mViewNav.mRootView];
    float currentx = p.x + deltax;
    float currentend = p.x + self.frame.size.width + deltax;
    if(currentx < 0)
    {
        deltax = -p.x;
    }
    if(currentend > mViewNav.mViewPortWidth)
    {
        deltax = mViewNav.mViewPortWidth - p.x - self.frame.size.width;
    }
    if(fabsf(deltax) < 5 && mFirstMove)
        return;
    mOrig = loc;
    CGPoint c = mViewNav.mContentContainerParent.center;
    c.x += deltax;
    NSLog(@"deltax = %f", deltax);
    mViewNav.mContentContainerParent.center = c;
    BAR_LOC_TYPE barLocType = [self barLocationType];
    if(barLocType == MID_BAR && mFirstMove)
    {
        NSLog(@"expand page \n");
        [mLeftContentContainer expandContent: (mViewNav.mViewPortWidth - mViewNav.mBarWidth) / 2 part:LEFT_PART];
        [mRightContentContainer expandContent:(mViewNav.mViewPortWidth - mViewNav.mBarWidth) / 2 part:RIGHT_PART];
    }

    mFirstMove = NO;
    [self moveContainerTitleStatusLabel:deltax];
}
- (CGPoint) pointInScreen: (CGPoint) point
{
    return [self convertPoint:point toView:mViewNav.mRootView];
}
- (void) handleContentContainerWhenTouchEndAtLeft: (BOOL)stopInMid
{
    if(stopInMid)
    {
        self.mStayOnMid = YES;
        float contentWidth = (mViewNav.mViewPortWidth - mViewNav.mBarWidth) / 2;
        if([mLeftContentContainer.contentView canAdjust])
        {
            [mLeftContentContainer adjustContentViewLayout:contentWidth part:LEFT_PART barStopInMid:YES];
        }
        if([mRightContentContainer.contentView canAdjust])
        {
            [mRightContentContainer adjustContentViewLayout:contentWidth part:RIGHT_PART barStopInMid:YES];
        }
        
    }
    else
    {
        self.mStayOnMid = NO;
        if([mRightContentContainer.contentView canAdjust])
        {
            [mRightContentContainer adjustContentViewLayout:mRightContentContainer.frame.size.width part:RIGHT_PART barStopInMid:NO];
        }
        VIEW_TYPE realType = getMirrorType(mRightContentContainer.mType);
        NSLog(@"### realType = %d ####", realType);
        if(mBarViewType.canSeenAsLeftBar == NO)
        {
           // VIEW_TYPE currViewType = mRightContentContainer.mType;
            assert(realType != INVALID_VIEW);
            VIEW_SEQ_TYPE seqType = [mViewNav popViewSeqType];
            NSLog(@"### popuped seqType = %d ####", seqType);
            [mViewNav moveToView:seqType :realType hasAnimation:NO isPush:NO];
        }
        else
        {
            if(realType != INVALID_VIEW)
            {
                VIEW_SEQ_TYPE seqType = [mViewNav popViewSeqType];
                NSLog(@"### popuped seqType = %d ####", seqType);
                [mViewNav moveToView:seqType :realType hasAnimation:NO isPush:NO];
            }
            else
            {
                if(mViewNav.mCurrView != mRightContentContainer.mType)
                {
                    //[mViewNav setPrevCurrView: mViewNav.mCurrView];
                }
                [mViewNav setCurrentView:mRightContentContainer.mType isAnimation:NO];
            }
        }
        
    }
    
}
- (void) handleContentContainerWhenTouchEndAtRight: (BOOL)stopInMid
{
    if(stopInMid)
    {
        self.mStayOnMid = YES;
        float contentWidth = (mViewNav.mViewPortWidth - mViewNav.mBarWidth) / 2;
        if([mLeftContentContainer.contentView canAdjust])
        {
            [mLeftContentContainer adjustContentViewLayout:contentWidth part:LEFT_PART barStopInMid:YES];
        }
        if([mRightContentContainer.contentView canAdjust])
        {
            [mRightContentContainer adjustContentViewLayout:contentWidth part:RIGHT_PART barStopInMid:YES];
        }
        
    }
    else
    {
        self.mStayOnMid = NO;
        if([mLeftContentContainer.contentView canAdjust])
        {
            [mLeftContentContainer adjustContentViewLayout:mLeftContentContainer.frame.size.width part:LEFT_PART barStopInMid:NO];
        }
        if(mBarViewType.canSeenAsRightBar == NO)
        {
            VIEW_SEQ_TYPE seqType = [mViewNav popViewSeqType];
            
            [mViewNav moveToView:seqType :mLeftContentContainer.mType hasAnimation:NO isPush:NO];
        }
        else
        {
            if(mViewNav.mCurrView != mRightContentContainer.mType)
            {
                //[mViewNav setPrevCurrView: mViewNav.mCurrView];
            }
            [mViewNav setCurrentView:mLeftContentContainer.mType isAnimation:NO];
        }
        
        [mContainerAnimHandler handleAnimEnd:stopInMid withDirection:mDirect leftContainer:mLeftContentContainer rightContainer:mRightContentContainer];
        //mRightContentContainer.hidden = YES;
    }

}
- (CGRect) createLeftTitleRect: (MOVE_DIRECT)direct deltax: (float)deltax stopInMid: (BOOL) stopInMid
{
    UIView* titleLabel = [mLeftContentContainer getTitleLabel];
    if(direct == MOVE_LEFT)
    {
        float x = titleLabel.frame.origin.x + deltax;
        if(x > mLeftContentContainer.mTitleLabelMidX )
            x = mLeftContentContainer.mTitleLabelMidX;
        return CGRectMake(x, titleLabel.frame.origin.y, titleLabel.frame.size.width, titleLabel.frame.size.height);
    }
    else if(direct == MOVE_RIGHT)
    {
        float x = titleLabel.frame.origin.x - deltax;
        if(stopInMid)
        {
            x = titleLabel.frame.origin.x;
        }
        else 
        {
            
            if(x < mLeftContentContainer.mTitleLabelLeftX)
                x = mLeftContentContainer.mTitleLabelLeftX;
        }
        return CGRectMake(x, titleLabel.frame.origin.y, titleLabel.frame.size.width, titleLabel.frame.size.height);
    }
    else 
    {
        return titleLabel.frame;
    }
}
- (CGRect) createRightStatusRect: (MOVE_DIRECT)direct deltax: (float)deltax stopInMid:(BOOL)stopInMid
{
    UIView* statusLabel = [mRightContentContainer getStatusLabel];
    if(direct == MOVE_RIGHT)
    {
        float x = statusLabel.frame.origin.x - deltax;
        if(x < mRightContentContainer.mStatusLabelMidX)
            x = mRightContentContainer.mStatusLabelMidX;
        return CGRectMake(x, statusLabel.frame.origin.y, statusLabel.frame.size.width, statusLabel.frame.size.height);
    }
    else if(direct == MOVE_LEFT)
    {
        float x = statusLabel.frame.origin.x + deltax;
        if(stopInMid)
        {
            x = statusLabel.frame.origin.x;
        }
        else
        {
            if(x > mRightContentContainer.mStatusLabelRightX)
                x = mRightContentContainer.mStatusLabelRightX;
        }
        return CGRectMake(x, statusLabel.frame.origin.y, statusLabel.frame.size.width, statusLabel.frame.size.height);
    }
    else {
        return statusLabel.frame;
    }

}
- (void) leftArrowButtonHandler: (UIButton*)sender
{
    NSLog(@"left arrow button handler");
    BAR_LOC_TYPE barLocType = [self barLocationType];
    NSLog(@"barloc type = %d", barLocType);
    BOOL bHandle = NO;
    BOOL stopInMid = NO;
    float dist = 0;
    BarViewType leftBarViewType = getBarViewTypeWithRightView(mLeftContentContainer.mType);
    switch (barLocType) 
    {
        case LEFT_BAR:
        {
            bHandle = YES;
            stopInMid = self.mCanStopInMid;
            dist = mViewNav.mViewPortWidth / 2 - mViewNav.mBarWidth / 2;
            if(stopInMid == NO)
            {
                if(mBarViewType.canSeenAsRightBar)
                {
                    dist = mViewNav.mViewPortWidth - mViewNav.mBarWidth;
                }
                else
                {
                    dist = mViewNav.mViewPortWidth;
                }
            }
            
        }
            break;
        case RIGHT_BAR:
        {
            if(leftBarViewType.leftView != INVALID_VIEW && leftBarViewType.rightView != INVALID_VIEW)
            {
                SEBarView* leftBarView = [mViewNav getBarView: leftBarViewType.leftView: leftBarViewType.rightView];
                if(mLeftContentContainer.frame.size.width == (mViewNav.mViewPortWidth - 2 * mViewNav.mBarWidth))
                {
                    [leftBarView leftArrowButtonHandler:nil];
                }
            }

        }
            break;
        case MID_BAR:
        {
            bHandle = YES;
            stopInMid = NO;
            dist = mViewNav.mViewPortWidth / 2 - mViewNav.mBarWidth / 2;
        }
            break;
        default:
            break;
    }
    if(bHandle)
    {
        if(barLocType == LEFT_BAR)
        {
            mLeftContentContainer.hidden = NO;
            [mViewNav addContentToContentContainer:mLeftContentContainer.mType];
            UIView* v = [mLeftContentContainer getTitleLabel];
            v.frame = CGRectMake(mLeftContentContainer.mTitleLabelMidX, v.frame.origin.y, v.frame.size.width, v.frame.size.height);
        }
        else if(barLocType == RIGHT_BAR)
        {
            mRightContentContainer.hidden = NO;
            [mViewNav addContentToContentContainer:mRightContentContainer.mType];
            UIView* v = [mRightContentContainer getStatusLabel];
            v.frame = CGRectMake(mRightContentContainer.mStatusLabelMidX, v.frame.origin.y, v.frame.size.width, v.frame.size.height);
        }
        if(barLocType == MID_BAR)
        {
            NSLog(@"expand page \n");
            [mLeftContentContainer expandContent: (mViewNav.mViewPortWidth - mViewNav.mBarWidth) / 2 part:LEFT_PART];
            [mRightContentContainer expandContent:(mViewNav.mViewPortWidth - mViewNav.mBarWidth) / 2 part:RIGHT_PART];
        }
        

        UIView* leftTitle = [mLeftContentContainer getTitleLabel];
        UIView* rightStatus = [mRightContentContainer getStatusLabel];
        CGPoint p = mViewNav.mContentContainerParent.center;
        p.x += dist;
        CGRect titleRect = [self createLeftTitleRect:MOVE_LEFT deltax:dist stopInMid:stopInMid];
        CGRect statusRect = [self createRightStatusRect:MOVE_LEFT deltax:dist stopInMid:stopInMid] ;
        void (^animBlock) (void) = ^{
            mViewNav.mContentContainerParent.center = p;
            leftTitle.frame = titleRect;
            rightStatus.frame = statusRect;
        };
        void (^animEnd) (BOOL) = ^(BOOL f)
        {
            [self handleContentContainerWhenTouchEndAtRight:stopInMid];
            [mContainerAnimHandler handleAnimEnd:stopInMid withDirection:MOVE_RIGHT leftContainer:mLeftContentContainer rightContainer:mRightContentContainer];
            mIsBarAnimationEnd = YES;
            self.userInteractionEnabled = YES;
        };
        self.userInteractionEnabled = NO;
        mIsBarAnimationEnd = NO;
        [UIView animateWithDuration:0.2 delay: 0 options: UIViewAnimationOptionCurveLinear animations:animBlock completion:animEnd];
    }
}
- (void) rightArrowButtonHandler: (UIButton*)sender
{
    NSLog(@"right arrow button handler");
    BAR_LOC_TYPE barLocType = [self barLocationType];
    NSLog(@"barloc type = %d", barLocType);
    BOOL bHandle = NO;
    BOOL stopInMid = NO;
    float dist = 0;
    BarViewType rightBarViewType = getBarViewTypeWithLeftView(mRightContentContainer.mType);
    switch (barLocType) {
        case LEFT_BAR:
        {
            if(rightBarViewType.leftView != INVALID_VIEW && rightBarViewType.rightView != INVALID_VIEW)
            {
                SEBarView* barView = [mViewNav getBarView:rightBarViewType.leftView: rightBarViewType.rightView];
                if(mRightContentContainer.frame.size.width == (mViewNav.mViewPortWidth - 2 * mViewNav.mBarWidth))
                    [barView rightArrowButtonHandler:nil];
            }
            
         }
            break;
        case RIGHT_BAR:
        {
            bHandle = YES;
            stopInMid = self.mCanStopInMid;
            dist = mViewNav.mViewPortWidth / 2 - mViewNav.mBarWidth / 2;
            if(stopInMid == NO)
            {
                if(mBarViewType.canSeenAsLeftBar)
                {
                    dist = mViewNav.mViewPortWidth - mViewNav.mBarWidth;
                }
                else
                {
                    dist = mViewNav.mViewPortWidth;
                }
            }

        }
            break;
        case MID_BAR:
        {
            bHandle = YES;
            stopInMid = NO;
            dist = mViewNav.mViewPortWidth / 2 - mViewNav.mBarWidth / 2;
        }
            break;
        default:
            break;
    }
    if(bHandle)
    {
        if(barLocType == LEFT_BAR)
        {
            mLeftContentContainer.hidden = NO;
            [mViewNav addContentToContentContainer:mLeftContentContainer.mType];
            UIView* v = [mLeftContentContainer getTitleLabel];
            v.frame = CGRectMake(mLeftContentContainer.mTitleLabelMidX, v.frame.origin.y, v.frame.size.width, v.frame.size.height);
        }
        else if(barLocType == RIGHT_BAR)
        {
            mRightContentContainer.hidden = NO;
            [mViewNav addContentToContentContainer:mRightContentContainer.mType];
            UIView* v = [mRightContentContainer getStatusLabel];
            v.frame = CGRectMake(mRightContentContainer.mStatusLabelMidX, v.frame.origin.y, v.frame.size.width, v.frame.size.height);
        }

        if(barLocType == MID_BAR)
        {
            NSLog(@"expand page \n");
            [mLeftContentContainer expandContent: (mViewNav.mViewPortWidth - mViewNav.mBarWidth) / 2 part:LEFT_PART];
            [mRightContentContainer expandContent:(mViewNav.mViewPortWidth - mViewNav.mBarWidth) / 2 part:RIGHT_PART];
        }
        CGPoint p = mViewNav.mContentContainerParent.center;
        p.x -= dist;
        
        UIView* leftTitle = [mLeftContentContainer getTitleLabel];
        UIView* rightStatus = [mRightContentContainer getStatusLabel];
        CGRect titleRect = [self createLeftTitleRect:MOVE_LEFT deltax:dist stopInMid:stopInMid];
        CGRect statusRect = [self createRightStatusRect:MOVE_LEFT deltax:dist stopInMid:stopInMid];                
        void (^animBlock) (void) = ^{
            mViewNav.mContentContainerParent.center = p;
            
            leftTitle.frame = titleRect;
            rightStatus.frame = statusRect;
            //[self setContainerTitleStatusLabelFrame:dist];
        };
        void (^animEnd) (BOOL) = ^(BOOL f){
            [self handleContentContainerWhenTouchEndAtLeft:stopInMid];
            mIsBarAnimationEnd = YES;
            self.userInteractionEnabled = YES;
        };
        self.userInteractionEnabled = NO;
        mIsBarAnimationEnd = NO;
        [UIView animateWithDuration:0.2 delay: 0
                            options: UIViewAnimationOptionCurveLinear animations:animBlock
                         completion:animEnd];
    }
    
}

- (void) barUpHandler
{
    CGPoint currBarStartPoint = [self pointInScreen:CGPointMake(0, 0)];
    BOOL stopInMid = NO;
    [mViewNav clearPlacedView];
    switch (mDirect)
    {
        case MOVE_LEFT:
        {
            float dist = 0;
            if(self.mCanStopInMid)
            {
                if(currBarStartPoint.x > MID_CONTENTVIEW_WIDTH && currBarStartPoint.x < mViewNav.mViewPortWidth)
                {
                    dist = currBarStartPoint.x + mViewNav.mBarWidth / 2 - MID_CONTENTVIEW_WIDTH;
                    stopInMid = YES;
                }
                else if(currBarStartPoint.x < MID_CONTENTVIEW_WIDTH && currBarStartPoint.x > 0)
                {
                    dist = currBarStartPoint.x;
                }
                else if(currBarStartPoint.x == MID_CONTENTVIEW_WIDTH)
                {
                    dist = mViewNav.mBarWidth / 2;
                    stopInMid = YES;
                }
            }
            else
            {
                if(mBarViewType.canSeenAsLeftBar)
                    dist = currBarStartPoint.x;
                else
                    dist = currBarStartPoint.x + mViewNav.mBarWidth;
            }
            NSLog(@"move left dist = %f", dist);
            if(dist != 0)
            {
                CGPoint p = mViewNav.mContentContainerParent.center;
                p.x -= dist;
                /*
                UIView* leftTitle = [mLeftContentContainer getTitleLabel];
                //UIView* rightStatus = [mRightContentContainer getStatusLabel];
                CGRect titleRect = CGRectMake(leftTitle.frame.origin.x + dist, leftTitle.frame.origin.y, leftTitle.frame.size.width, leftTitle.frame.size.height);
                //CGRect statusRect = CGRectMake(rightStatus.frame.origin.x - dist, rightStatus.frame.origin.y, rightStatus.frame.size.width, rightStatus.frame.size.height);
                */
                UIView* leftTitle = [mLeftContentContainer getTitleLabel];
                UIView* rightStatus = [mRightContentContainer getStatusLabel];
                CGRect titleRect = [self createLeftTitleRect:mDirect deltax:dist stopInMid:stopInMid];
                CGRect statusRect = [self createRightStatusRect:mDirect deltax:dist stopInMid:stopInMid] ;
                void (^animBlock) (void) = ^{
                    mViewNav.mContentContainerParent.center = p;
                    leftTitle.frame = titleRect;
                    rightStatus.frame = statusRect;
                    //[self setContainerTitleStatusLabelFrame:-dist];
                };
                void (^animEnd) (BOOL) = ^(BOOL f)
                {
                    [self handleContentContainerWhenTouchEndAtLeft:stopInMid];
                    [mContainerAnimHandler handleAnimEnd:stopInMid withDirection:mDirect leftContainer:mLeftContentContainer rightContainer:mRightContentContainer];
                    mIsBarAnimationEnd = YES;
                    self.userInteractionEnabled = YES;
                };
                self.userInteractionEnabled = NO;
                mIsBarAnimationEnd = NO;
                [UIView animateWithDuration:0.2 delay: 0 options: UIViewAnimationOptionCurveLinear animations:animBlock completion:animEnd];
                
            }
            else
            {
                NSLog(@"warning : ################## dist = 0   ##################################");
                [self handleContentContainerWhenTouchEndAtLeft:stopInMid];
            }
            
        }
            break;
        case MOVE_RIGHT:
        {
            float dist = 0;
            if(self.mCanStopInMid)
            {
                if(currBarStartPoint.x > FIRST_CONTENTVIEW_WIDTH && currBarStartPoint.x < MID_CONTENTVIEW_WIDTH)
                {
                    dist = MID_CONTENTVIEW_WIDTH - (currBarStartPoint.x + mViewNav.mBarWidth / 2);
                    stopInMid = YES;
                }
                else if(currBarStartPoint.x > MID_CONTENTVIEW_WIDTH && (currBarStartPoint.x + mViewNav.mBarWidth)<= mViewNav.mViewPortWidth)
                {
                    dist = mViewNav.mViewPortWidth - (currBarStartPoint.x + mViewNav.mBarWidth);
                }
                else if(currBarStartPoint.x == MID_CONTENTVIEW_WIDTH)
                {
                    dist = mViewNav.mBarWidth / 2;
                    stopInMid = YES;
                }
            }
            else
            {
                if(mBarViewType.canSeenAsRightBar)
                    dist = mViewNav.mViewPortWidth - (currBarStartPoint.x + mViewNav.mBarWidth);
                else
                    dist = mViewNav.mViewPortWidth - currBarStartPoint.x;
            }
            NSLog(@"move right dist = %f", dist);
            if(dist != 0)
            {
                
                CGPoint p = mViewNav.mContentContainerParent.center;
                p.x += dist;
                
                
                UIView* leftTitle = [mLeftContentContainer getTitleLabel];
                UIView* rightStatus = [mRightContentContainer getStatusLabel];
                CGRect titleRect = [self createLeftTitleRect:mDirect deltax:dist stopInMid:stopInMid];
                CGRect statusRect = [self createRightStatusRect:mDirect deltax:dist stopInMid:stopInMid];                
                void (^animBlock) (void) = ^{
                    mViewNav.mContentContainerParent.center = p;
                    
                    leftTitle.frame = titleRect;
                    rightStatus.frame = statusRect;
                    //[self setContainerTitleStatusLabelFrame:dist];
                };
                void (^animEnd) (BOOL) = ^(BOOL f){
                    [self handleContentContainerWhenTouchEndAtRight:stopInMid];
                    mIsBarAnimationEnd = YES;
                    self.userInteractionEnabled = YES;
                };
                self.userInteractionEnabled = NO;
                mIsBarAnimationEnd = NO;
                [UIView animateWithDuration:0.2 delay: 0
                                    options: UIViewAnimationOptionCurveLinear animations:animBlock
                                 completion:animEnd];
            }
            else
            {
                NSLog(@"warning: ################## dist = 0  #######################");
                [self handleContentContainerWhenTouchEndAtRight:stopInMid];
            }
        }
            break;
        case NO_MOVE:
        {
            NSLog(@"################ NO_MOVE ##########");
        }
            
            break;
        default:
            break;
    }
    
}

- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"bar view touches end count = %u", [touches count]);
    if(mTouchStart == NO)
        return;
    mTouchStart = NO;
    
    [self barUpHandler];
}
- (void)touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"bar view touch cancel");
    if(mTouchStart == NO)
        return;
    mTouchStart = NO;
    [self barUpHandler];
    
}
@end
////////////////////
@interface SEViewNavigator (Private)
//- (void) resetSharedImageArray;
//- (void) addSharedImage:(UIImage*)image;
//- (void) loadShareInterface;
//- (void)popupOperationViewGroup;
//- (void) disappearOperationViewGroup;
- (void)createFloatView: (SEPageHitProperty)hp scrollView:(SEPageUIScrollView*)currView;
- (void) setCurrentContentContainerFrame;
- (CGRect) calculateContentContainerFrame: (VIEW_TYPE)vp;
- (UIView*) loadViewFromNib: (NSString*)name;
- (void) setupRootView;
- (UIView<SEAdjustContentView>*) createImagePickerView: (CGRect)rect;
- (UIView<SEAdjustContentView>*) createImageSelectedView : (CGRect) rect;
- (UIView<SEAdjustContentView>*) createMusicPickerView: (CGRect) rect;
- (UIView<SEAdjustContentView>*) createOptionsView: (CGRect) rect;
- (UIView<SEAdjustContentView>*) createSignatureView:(CGRect) rect;
- (UIView<SEAdjustContentView>*) createSignaturePreview: (CGRect) rect;
- (UIView<SEAdjustContentView>*) createMusicImageListAttachView: (CGRect)rect;
- (UIView<SEAdjustContentView>*) createSelectedMusicView: (CGRect) rect;
- (UIView<SEAdjustContentView>*) createPreview3D: (CGRect)rect;
- (UIImage*) getDefaultImage;
- (void) initContentContainer;
- (void) initBarView;
- (void) initContentContainerParent;
- (SEBarView*) getBarView: (VIEW_TYPE)leftView :(VIEW_TYPE)rightView;
- (void) setCurrentViewSequenceFrame;
- (void) handleToolBarButtonClick: (TOOLBAR_BUTTON_TYPE*) typePtr;
- (void) playImage;
- (void) playMusic;
- (MPMediaItem*) getMediaItem: (NSArray*)mediaArray title: (NSString*)title artist: (NSString*) artist album: (NSString*)album;
- (void) setRootViewTransform;
- (void) createDefaultSelectedImageAndMusic;
- (void) setPhotoURLToCoreData:(NSMutableArray *)photoURLArray imageListName: (NSString*)imageListName;
- (void) setSelectedMusicToCoreData: (NSArray*) musicArray musicListName: (NSString*) musicListName;
- (void) getDataFromPhotoLib;
- (void) finishGetImageFromPhotoLib: (NSMutableArray*)data;
- (void) setPrevCurrView: (VIEW_TYPE)vp;
@property (nonatomic, retain) NSString* mDrawingImageList;

@end

///////////////////

@implementation SEUIFloatView
@synthesize p;
@synthesize origC;
@dynamic image;
@dynamic backgroundImage;
- (void) setCount: (int)n
{
    NSString* str = [NSString stringWithFormat:@"%d", n];
    mLabel.text = str;
}
- (id) init
{
    self = [super init];
    if(self)
    {
        self.multipleTouchEnabled = NO;
    }
    return self;
}
- (void) createChild: (CGRect) frame
{
    mBackgroundView = [[UIImageView alloc] initWithFrame:CGRectMake(0,0, frame.size.width, frame.size.height)];
    [self addSubview:mBackgroundView];
    [mBackgroundView release];
    
    mForegroundView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 40, frame.size.width, 100)];
    [self addSubview:mForegroundView];
    [mForegroundView release];
    mForegroundView.contentMode = UIViewContentModeCenter;
    mForegroundView.clipsToBounds = YES;
    
    mLabel = [[UILabel alloc] initWithFrame:CGRectMake(0, 0, 100, 40)];
    mLabel.textAlignment = UITextAlignmentCenter;
    mLabel.backgroundColor = [UIColor clearColor];
    [self addSubview:mLabel];
    [mLabel release];
}
- (id) initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if(self)
    {
        [self createChild:frame];
    }
    return self;
}
- (void) setImage:(UIImage *)image
{
    mForegroundView.image = image;
}
- (UIImage*) getImage
{
    return mForegroundView.image;
}
- (void) setBackgroundImage:(UIImage *)backgroundImage
{
    mBackgroundView.image = backgroundImage;
}
- (UIImage*) getBackgroundImage
{
    return mBackgroundView.image;
}
@end
////////////////////

////////////////////////////////////////////////////
@interface SEImageViewLoader : SEImageAsyncLoadHandler
{
    UIImage* currImage;
    UIView* imageView;
    SEViewNavigator* viewNav;
    CGSize destSize;
    id mAfterHandler;
    SEL mAfterAction;
}
@property (nonatomic, retain) UIView* imageView;
@property (nonatomic, assign) SEViewNavigator* viewNav;
@property (nonatomic, assign) CGSize destSize;
- (void) setAfterImageLoadTarget: (id) target action: (SEL) action;
@end
@implementation SEImageViewLoader
@synthesize imageView;
@synthesize viewNav;
@synthesize destSize;
- (void) setAfterImageLoadTarget:(id)target action:(SEL)action
{
    mAfterHandler = target;
    mAfterAction = action;
}
- (void) setImage:(UIImage *)image
{
    [currImage release];
    currImage = [image retain];
    
}
- (void) preHandleImage
{

}
- (void) dealloc
{
    [currImage release];
    [imageView release];
    [super dealloc];
}
- (void) handleImage
{
    if([imageView respondsToSelector:@selector(setImage:)])
    {
        //NSLog(@"image width = %f, height = %f", currImage.size.width, currImage.size.height);
        //NSLog(@"current image = %@", currImage);
        if(destSize.height != 0 && destSize.width != 0)
        {
            CGSize srcSize = CGSizeMake(currImage.size.width, currImage.size.height);
            CGSize actualSize = [SEUtil computeFitSize:srcSize toDst:destSize];
            BOOL isDisplayText = [SEUserDefaultManager isFunctionOK:BASIC_FUNC] == NO;
            UIImage* textImage = currImage;
            if(isDisplayText)
            {
                NSString* str = [SESystemConfig getITuneConnetionStr];
                textImage = [SEUtil drawTextOnImage:currImage text:str color:[UIColor whiteColor] textSize:[SESystemConfig getShareStrFontSize] x:0 y:0];
            }
            UIGraphicsBeginImageContext(actualSize);
            [textImage drawInRect:CGRectMake(0, 0, actualSize.width, actualSize.height)];
            UIImage* retimage = UIGraphicsGetImageFromCurrentImageContext();
            UIGraphicsEndImageContext();
            [imageView setImage: retimage];
        }
        else
        {
            [imageView setImage:currImage];
        }
    }
    [mAfterHandler performSelectorOnMainThread:mAfterAction withObject:nil waitUntilDone:NO];
    //[viewNav addShareImage:currImage];
}
@end
///////////
@interface SEImageViewLoaderContent : NSObject
{
    UIView* imageView;
    CGSize destSize;
    NSURL* url;
    VIEW_TYPE viewType;
    NSString* urlDate;
}
@property (nonatomic, retain) UIView* imageView;
@property (nonatomic, retain) NSURL* url;
@property (nonatomic, assign) CGSize destSize;
@property (nonatomic, assign) VIEW_TYPE viewType;
@property (nonatomic, retain) NSString* urlDate;
@end
@implementation SEImageViewLoaderContent
@synthesize url;
@synthesize imageView;
@synthesize destSize;
@synthesize viewType;
@synthesize urlDate;
- (void) dealloc
{
    [imageView release];
    [url release];
    [urlDate release];
    [super dealloc];
}

@end
/////////
@interface SEImageViewListLoader : NSObject
{
    NSMutableArray* imageViewLoaderContentList;
    int currentIndex;
}
@property (nonatomic, retain) NSMutableArray* imageViewLoaderContentList;
@property (nonatomic, assign) int currentIndex;
- (void) load;
@end
@implementation SEImageViewListLoader
@synthesize imageViewLoaderContentList;
@synthesize currentIndex;
- (void) dealloc
{
    [imageViewLoaderContentList release];
    [super dealloc];
}
- (void) load
{
    if(currentIndex < imageViewLoaderContentList.count)
    {
        SEImageViewLoaderContent* content = [imageViewLoaderContentList objectAtIndex:currentIndex];
        SEImageViewLoader* imageViewLoader = [[SEImageViewLoader alloc] init];
        imageViewLoader.imageView = content.imageView;
        imageViewLoader.viewNav = [PhotoFrameAppDelegate getViewNavigator];
        imageViewLoader.destSize = content.destSize;
        ALAssetsLibrary* lib = [[[ALAssetsLibrary alloc] init] autorelease];
        [imageViewLoader setAssetLibOwn:lib];
        [imageViewLoader setAfterImageLoadTarget:self action:@selector(afterImageLoad)];
        if(content.viewType == IMAGE_PICKER)
        {
            if(content.destSize.width > 0 && content.destSize.height > 0)
            {
                [imageViewLoader loadFullRepresentation:content.url];
            }
            else
            {
                [imageViewLoader loadImageFromPhotoLib:content.url size:CGSizeMake(content.imageView.frame.size.width - 20, content.imageView.frame.size.height - 20)];    
            }
        }
        else
        {
            if(content.destSize.width > 0 && content.destSize.height > 0)
            {
                [imageViewLoader loadCoreDataFullRepresentation:content.url date:content.urlDate];
            }
            else
            {
                [imageViewLoader loadCoreDataImage:content.url date:content.urlDate size:CGSizeMake(content.imageView.frame.size.width - 20, content.imageView.frame.size.height - 20)]; 
            }
        }

    }
    else 
    {
        [self release];
    }
}
- (void) afterImageLoad
{
    currentIndex++;
    [self load];
}
@end
//////
@interface SEShareImageOperationViewHandler : NSObject <SEOperationHandler>//, GooglePlusShareDelegate> 
{
    SEViewNavigator* mViewNav;
    UIImageView* mBigImageView;
    SEFrameImageView* mCurrentImageView;
    SEContentViewContainer* mOperationContainer;
    NSArray* mSelectedImageViewIndex;
    NSMutableArray* mImageViewForNotShare;
    UIView* mView;
    SEPopupLabel* mMessageView;
}
- (void) indicateImageHasNotShared: (NSArray*) imageURLArray;
- (void) indicateAllImageHasShared: (BOOL) allShareOK;
//- (void) setUploadMessage;
- (void) setMessage:(NSString*)msg data: (NSArray*)data;
- (void) setTextWithShareState: (BOOL) hasNoShared;
@end
@interface SEShareViewItem : UIView
{
    enum SHARE_WAY {SHARE_BY_TWITTER, SHARE_BY_PICASA, SHARE_WAY_NUM};
    SEPAGE_SCROLLVIEW_TYPE mResourceScrollViewType;
    BOOL mSelected_;
    SEPopupButton* mLoginButton;
    UIImageView* mImageBgView;
    id mTarget;
    SEL mAction;
    id mBackgroundTarget;
    SEL mBackgroundAction;
    SHARE_WAY mShareWay;
    SEWebService* mWebService;
    //int mCurrentImageURLIndex;
    //NSArray* mCurrentImageURLArray;
    SEShareImageOperationViewHandler* mParent;
    NSString* mStateText;
}
@property (nonatomic, assign) SEPAGE_SCROLLVIEW_TYPE mResourceScrollViewType;
@property (nonatomic, retain) NSString* mStateText;
@property (nonatomic, assign) SEShareImageOperationViewHandler* mParent;
@property (nonatomic, assign) BOOL mSelected;
//- (void) setImage: (NSString*)str;
- (void) setButtonTarget: (id)target action: (SEL)action;
- (void) setButtonText: (NSString*)text;
- (SHARE_WAY) getShareWay;
- (void) shareImage: (NSArray*)urlArray;
- (void) setBackgroundTarget: (id)target action: (SEL) action;
- (void) changeTextByAuth;
- (NSArray*) getUnsharedImageArray;
- (void) indicateUploadError: (NSString*)str;
- (void) indicateUploadOK: (NSString*)str;
- (void) indicateAuthError: (NSString*)str;
- (void) indicateOtherError: (NSString*) str;
- (void) indicateSignInOK : (NSString*)str;
- (void) indicateSignOutOK : (NSString*)str;
- (void) indicateSignInError: (NSString*)str;
- (void) indicateMessage: (NSString*)msg;
- (BOOL) hasStartShare;
- (NSString*) getShareWayName;
//- (void) setShareWay: (SHARE_WAY)way;
@end

/////////////////////
@implementation SEGooglePicasaServiceDelegate
@synthesize mOpType;
@synthesize mCurrentImageIndex;
@synthesize mCurrentImageURLArray;
@synthesize mParentView;
@synthesize mImageHeight;
@synthesize mImageWidth;
@synthesize mIsScaleImage;
@synthesize mUploadDelayTime;
- (void) dealloc
{
    [mCurrentImageURLArray release];
    [super dealloc];
}
- (id) init
{
    self = [super init];
    if(self)
    {
        mOpType = JUST_NO_OP;
    }
    return self;
}

- (BOOL) isImageNeedScale
{
    return mIsScaleImage;
}
- (CGSize) scaledImageSize
{
    return CGSizeMake(mImageWidth, mImageHeight);
}
- (void) handleSigningIn: (SEWebService*)webService
{
    [mParentView indicateMessage:@"signing_in"];
}
- (void) handleSignOut: (SEWebService*)webService
{
    [mParentView changeTextByAuth];
    [mParentView indicateSignOutOK:@"signout_ok"];
}
- (void) handleAuthTestError:(SEWebService *)webService
{
    NSLog(@"auth test error");
    [[PhotoFrameAppDelegate getViewNavigator] hideLoadingView];
    [webService signIn];
}
- (void) loadImage: (int) imageIndex : (SEWebService*)webService
{
    SEImageLoaderForUploadImage* loader = [[SEImageLoaderForUploadImage alloc] init];
    loader.mWebService = webService;
    //SEImageAsyncLoader* asyncLoader = [[SEImageAsyncLoader alloc] init];
    //asyncLoader.mViewNav = [PhotoFrameAppDelegate getViewNavigator];
    ALAssetsLibrary* lib = [[[ALAssetsLibrary alloc] init] autorelease];
    [loader setAssetLibOwn:lib];
    SEPageImageURL* imageURL = [mCurrentImageURLArray objectAtIndex:imageIndex];
    NSURL* url = imageURL.url;
    NSString* date = imageURL.urlDate;
    if([webService isPhotoFromPhotoLib])
    {
        //[asyncLoader loadFullRepresentation:url withHandler:loader];
        [loader loadFullRepresentation:url];
    }
    else 
    {
        //[asyncLoader loadCoreDataFullRepresentation:url date: date withHandler:loader];
        [loader loadCoreDataFullRepresentation:url date:date];
    }
}
- (void) setErrorNum: (int) errorNum
{
    mErrorNum = errorNum;
}
- (void) handleRequestAccessToken: (SEWebService*)webService
{
    [mParentView indicateMessage:@"request_access_token"];
}
- (void) handleAuthTestOK:(SEWebService *)webService
{
    NSLog(@"auth test Ok");
    [[PhotoFrameAppDelegate getViewNavigator] hideLoadingView];
    if(mCurrentImageURLArray.count > 0)
    {
        //[[PhotoFrameAppDelegate getViewNavigator] showLoadingTextView];
        //mCurrentImageIndex = 0;
        showLoadingViewForShare();
        SELoadingView* loadingView = [[PhotoFrameAppDelegate getViewNavigator] getLoadingView];
        [loadingView useTextView];
        [loadingView showStageView];
        [loadingView setText:@"Upload Processing ......"];
        [loadingView setText2:[NSString stringWithFormat:@"%d / %d  pieces", mCurrentImageIndex, mCurrentImageURLArray.count]];
        [self loadImage:mCurrentImageIndex :webService];
    }
}
- (void) handleSignInError: (SEWebService*) webService
{
    NSLog(@"sign in error");  
    [[PhotoFrameAppDelegate getViewNavigator] hideLoadingView];
    [mParentView indicateSignInError:@"signin_error"];
}
- (void) handleSignInOK : (SEWebService*) webService
{
    NSLog(@"sign in ok");
    [[PhotoFrameAppDelegate getViewNavigator] dismissDialogView];
    //[mParentView setButtonText:@"SignOut"];
    [mParentView indicateSignInOK:@"signin_ok"];
    id t = webService.delegate;
    SEGooglePicasaServiceDelegate* delegate = nil;
    if([t isMemberOfClass:[SEGooglePicasaServiceDelegate class]])
    {
        delegate = t;
    }
    if(delegate.mOpType == JUST_SHARE)
    {
        [webService authTest];
    }
    else
    {
        [[PhotoFrameAppDelegate getViewNavigator] hideLoadingView];
    }
}
- (void) handleUploadError: (SEWebService*) webService
{
    NSLog(@"upload error");
    [[PhotoFrameAppDelegate getViewNavigator] hideLoadingView];
    if(mErrorNum != NO_ENOUGH_ROOM_ERROR)
    {
        [mParentView indicateUploadError:@"upload_error"];
    }
    else
    {
        mErrorNum = 0;
        [mParentView indicateUploadError:@"upload_error_no_room"];
    }
}
- (void) loadImageDelay: (SEWebService*)webService
{
    [self loadImage:mCurrentImageIndex :webService];
}
- (void) delayHandleUploadOk
{
    [[PhotoFrameAppDelegate getViewNavigator] hideLoadingView];
    [mParentView indicateUploadOK:@"upload_ok"];
}
- (void) handleUploadOK: (SEWebService*) webService
{
    NSLog(@"upload ok");
    [[PhotoFrameAppDelegate getViewNavigator] finishShareOneImage];
    mCurrentImageIndex++;
    SELoadingView* loadingView = [[PhotoFrameAppDelegate getViewNavigator] getLoadingView];
    [loadingView setText:@"Upload Processing ......"];
    [loadingView setText2:[NSString stringWithFormat:@"%d / %d   pieces", mCurrentImageIndex, mCurrentImageURLArray.count]];
    if(mCurrentImageIndex < mCurrentImageURLArray.count)
    {
        //SELoadingView* loadingView = [[PhotoFrameAppDelegate getViewNavigator] getLoadingView];
        //[loadingView setText:[NSString stringWithFormat:@"%d / %d", (mCurrentImageIndex + 1), mCurrentImageURLArray.count]];
        //[self loadImage:mCurrentImageIndex :webService];    
        [self performSelector:@selector(loadImageDelay:) withObject:webService afterDelay:mUploadDelayTime];
        if(mUploadDelayTime > 0)
        {
            [[PhotoFrameAppDelegate getViewNavigator] showLoadingView];
        }
    }
    else
    {
        [self performSelector:@selector(delayHandleUploadOk) withObject:nil afterDelay:3];
    }
}
- (void)handleUploadProgress:(unsigned long long)totalCount :(unsigned long long)deliverCount
{
    /*
    double v = mCurrentImageIndex / (double) mCurrentImageURLArray.count;
    v += deliverCount / (double) totalCount;
    if(v > 1)
        v = 1;
    SELoadingView* loadingView = [[PhotoFrameAppDelegate getViewNavigator] getLoadingView];
    [loadingView setPercent:v];
     */
}
- (void) handleOtherError: (SEWebService*)webService
{
    NSLog(@"other error");
    [[PhotoFrameAppDelegate getViewNavigator] hideLoadingView];
    [mParentView indicateOtherError:@"other_error"];
}
- (void) handleMessage:(NSString *)msg
{
    
}
@end

@implementation SEShareViewItem
//@dynamic  mSelected;
@synthesize mParent;
@synthesize mStateText;
@synthesize mResourceScrollViewType;
@dynamic mSelected;
- (void) dealloc
{
    [mWebService release];
    [mStateText release];
    [super dealloc];
}

- (void) setBackgroundTarget:(id)target action:(SEL)action
{
    mBackgroundTarget = target;
    mBackgroundAction = action;
}
- (UIImage*) getBgImage
{
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    switch (mShareWay) 
    {
        case SHARE_BY_TWITTER:
        {
            if(!mSelected_)
            {
                return [viewNav.mResLoader getImage:@"TwitterButtonImage"];
            }
            else 
            {
                return [viewNav.mResLoader getImage:@"TwitterButtonImageH"];
            }
        }
            break;
        case SHARE_BY_PICASA:
        {
            if(!mSelected_)
            {
                return [viewNav.mResLoader getImage:@"PicasaButtonImage"];
            }
            else 
            {
                return [viewNav.mResLoader getImage:@"PicasaButtonImageH"];
            }
        }
            break;
        default:
            break;
    }
    return nil;
}

- (BOOL) mSelected
{
    return mSelected_;
}

- (void) setMSelected:(BOOL)s
{
    if(mSelected_ != s)
    {
        mSelected_ = s;
        UIImage* bg = [self getBgImage];
        mImageBgView.image = bg;
    }
}
- (void) setButtonText: (NSString*)text
{
    mLoginButton.buttonText.text = text;
}
- (SHARE_WAY) getShareWay
{
    return mShareWay;
}
- (void) setShareWay: (SHARE_WAY)way
{
    if(mShareWay != way)
    {
        mShareWay = way;
    }
}
- (void) setButtonTarget:(id)target action:(SEL)action
{
    mTarget = target;
    mAction = action;
}
- (SEWebService*) getWebService
{
    switch (mShareWay) {
        case SHARE_BY_PICASA:
        {
            if(mWebService == nil)
            {
                mWebService = [[SEGooglePicasaService alloc] init];
                SEGooglePicasaServiceDelegate* delegate = [[SEGooglePicasaServiceDelegate alloc] init];
                mWebService.delegate = delegate;
                [delegate release];
                delegate.mParentView = self;
                [mWebService setPhotoFromPhotoLib:mResourceScrollViewType == PHOTOLIB_SCROLLVIEW];
            }
        }
            break;
        case SHARE_BY_TWITTER:
        {
            if(mWebService == nil)
            {
                mWebService = [[SETwitterWebService alloc] init];
                SEGooglePicasaServiceDelegate* delegate = [[[SEGooglePicasaServiceDelegate alloc] init] autorelease];
                mWebService.delegate = delegate;
                [mWebService setPhotoFromPhotoLib: mResourceScrollViewType == PHOTOLIB_SCROLLVIEW];
                delegate.mParentView = self;
                delegate.mUploadDelayTime = 5;
            }
        }
            break;
        default:
            break;
    }
    return mWebService;
}
- (void) indicateUploadError: (NSString*)str
{
    NSArray* unsharedImageArray = [self getUnsharedImageArray];
    //[mParent indicateImageHasNotShared:unsharedImageArray];
    [mParent setMessage: str data: unsharedImageArray];
}

- (void) indicateAuthError: (NSString*)str
{
    if(mShareWay == SHARE_BY_PICASA)
    {
        NSArray* unsharedImageArray = [self getUnsharedImageArray];
        [mParent setMessage: @"auth_error" data: unsharedImageArray];
    }
    else
    {
        [mParent setMessage: @"auth_error" data: nil];
    }
}
- (void) indicateOtherError: (NSString*) str
{
    if(mShareWay == SHARE_BY_PICASA)
    {
        NSArray* unsharedImageArray = [self getUnsharedImageArray];
        [mParent setMessage: @"other_error" data: unsharedImageArray];
    }
    else
    {
        [mParent setMessage: @"other_error" data:nil];
    }
}
- (void) indicateSignInOK : (NSString*)str
{
    if(mShareWay == SHARE_BY_TWITTER)
    {
        [self changeTextByAuth];
    }
    [mParent setMessage:@"signin_ok" data:nil];
}
- (void) indicateSignOutOK : (NSString*)str
{
    [mParent setMessage:@"signout_ok" data:nil];
}
- (void) indicateUploadOK: (NSString*)str
{
    [mParent setMessage:@"upload_ok" data: nil];
}
- (void) indicateSignInError: (NSString*)str
{
    if(mShareWay == SHARE_BY_PICASA)
    {
         NSArray* unsharedImageArray = [self getUnsharedImageArray];
        [mParent setMessage:@"signin_error" data:unsharedImageArray];
    }
    else
    {
        [mParent setMessage:@"signin_error" data:nil];
    }
}
- (NSString*) getShareWayName
{
    switch (mShareWay) {
        case SHARE_BY_PICASA:
            return @"Google Picasa";
            break;
        case SHARE_BY_TWITTER:
            return @"Twitter";
            break;
        default:
            break;
    }
    return @"";
}
- (void) indicateMessage: (NSString*)msg
{
    [mParent setMessage:msg data:nil];
}
- (NSArray*) getUnsharedImageArray
{
    SEGooglePicasaServiceDelegate* delegate = (SEGooglePicasaServiceDelegate*)mWebService.delegate;
    NSMutableArray* allImages = [NSMutableArray array];
    if(delegate.mCurrentImageIndex < delegate.mCurrentImageURLArray.count)
    {
        for(int i = delegate.mCurrentImageIndex ; i < delegate.mCurrentImageURLArray.count ; i++)
        {
            SEPageImageURL* url = [delegate.mCurrentImageURLArray objectAtIndex:i];
            [allImages addObject:url];
        }
    }
    NSLog(@"unupload image count = %d", allImages.count);
    return allImages;
}
- (void) changeTextByAuth
{
    SEWebService* webService = [self getWebService];
    if([webService isServiceSignIn])
    {
        [self setButtonText:@"SignOut"];
    }
    else
    {
        [self setButtonText:@"SignIn"];
    }
}
- (void) indicateImageHasNotShared: (NSArray*) imageURLArray
{
    
}
- (BOOL) hasStartShare
{
    SEWebService* webService = [self getWebService];
    switch (mShareWay)
    {
        case SHARE_BY_PICASA:
        {
            SEGooglePicasaServiceDelegate* delegate = (SEGooglePicasaServiceDelegate*)webService.delegate;
            return delegate.mCurrentImageURLArray.count > 0;
        }
            break;
        case SHARE_BY_TWITTER:
        {
            SEGooglePicasaServiceDelegate* delegate = (SEGooglePicasaServiceDelegate*)webService.delegate;
            return delegate.mCurrentImageURLArray.count > 0;
        }
            break;
        default:
            break;
    }
    return NO;
}
- (void) shareImage : (NSArray*)urlArray
{
    //[mCurrentImageURLArray release];
    //mCurrentImageURLArray = [urlArray retain];
    //mCurrentImageURLArray = 0;
    SEWebService* webService = [self getWebService];
    switch (mShareWay) {
        case SHARE_BY_PICASA:
        {
            SEGooglePicasaServiceDelegate* delegate = (SEGooglePicasaServiceDelegate*)webService.delegate;
            if(delegate.mCurrentImageURLArray.count > 0 && delegate.mCurrentImageIndex < delegate.mCurrentImageURLArray.count)
            {
            }
            else
            {
                delegate.mCurrentImageIndex = 0;
                delegate.mCurrentImageURLArray = urlArray;
            }
            delegate.mOpType = JUST_SHARE;
            //[webService authTest];
            [webService signIn];
        }
            break;
        case SHARE_BY_TWITTER:
        {
            SEGooglePicasaServiceDelegate* delegate = (SEGooglePicasaServiceDelegate*)webService.delegate;
            if(delegate.mCurrentImageURLArray.count > 0 && delegate.mCurrentImageIndex < delegate.mCurrentImageURLArray.count)
            {
            }
            else
            {
                delegate.mCurrentImageIndex = 0;
                delegate.mCurrentImageURLArray = urlArray;
            }
            delegate.mIsScaleImage = YES;
            //delegate.mParentView = self;
            [webService authTest];
        }
            break;
        default:
            break;
    }
}
- (void) buttonAction: (UIButton*)sender
{
    SEWebService* webSercie = [self getWebService];
    if([webSercie isServiceSignIn] == NO)
    {
        SEGooglePicasaServiceDelegate* delegate = nil;
        id t = webSercie.delegate;
        if([t isMemberOfClass:[SEGooglePicasaService class]])
        {
            delegate = t;
        }
        delegate.mOpType = JUST_SIGNIN;
        [mWebService signIn];
    }
    else 
    {
        [mWebService signOut];
    }
    [mTarget performSelector:mAction withObject:self];
}
- (void) handleBgTap: (UITapGestureRecognizer*)tap
{
    [mBackgroundTarget performSelector:mBackgroundAction withObject:tap];
}
- (void) createChild: (CGRect)frame
{
    UIImage* image = [self getBgImage];
    NSLog(@"bg image size = %f, %f", image.size.width, image.size.height);
    UIImageView* imageView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, image.size.width, image.size.height)];
    [self addSubview:imageView];
    [imageView release];
    mImageBgView = imageView;
    mImageBgView.image = image;
    imageView.userInteractionEnabled = YES;
    UITapGestureRecognizer* tap = [[[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(handleBgTap:)] autorelease];
    [imageView addGestureRecognizer:tap];

    
    float buttonWidth = image.size.width * 0.5;
    float buttonHeight = image.size.height - 16;
    float startx = self.frame.size.width - 10 - buttonWidth;
    float starty = (image.size.height - buttonHeight) / 2;
    NSLog(@"button height = %f, image height = %f", buttonHeight, image.size.height);
    SEPopupButton* button = [[SEPopupButton alloc] initWithFrame:CGRectMake(startx, starty, buttonWidth, buttonHeight)];
    [button.button addTarget:self action:@selector(buttonAction:) forControlEvents:UIControlEventTouchUpInside];
    [self addSubview:button];
    [button release];
    [button setButtonBackground:@"SignInButtonBg" :@"SignInButtonBgH"];
    //button.buttonText.text = @"SignIn";
    setLabelFont(button.buttonText, @"SignIn", [UIColor blackColor], [SESystemConfig getFontName], 20);
    mLoginButton = button;
    button.buttonText.alpha = 0.5;
    if(mShareWay == SHARE_BY_PICASA)
    {
        button.hidden = YES;
    }
    
    self.frame = CGRectMake(frame.origin.x, frame.origin.y, image.size.width, image.size.height);
}
/*
- (void) setImage:(NSString *)str
{
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    mImageBgView.image = [viewNav.mResLoader getImage:str];
}
 */
- (id) initWithFrame:(CGRect)frame shareWay: (SHARE_WAY)shareWay
{
    self = [super initWithFrame:frame];
    if(self)
    {
        mShareWay = shareWay;
        [self createChild: frame];
    }
    return self;
}
@end
////////
@interface SEURLListScrollView : UIView
{
    UIImageView* backgroundView;
    UIScrollView* scrollView;
}
@property (nonatomic, readonly) UIImageView* backgroundView;
@property (nonatomic, readonly) UIScrollView* scrollView;
- (void) initView;
@end
@implementation SEURLListScrollView
@synthesize backgroundView;
@synthesize scrollView;
/*
- (void) drawRect:(CGRect)rect
{
    [image drawInRect:CGRectMake(0, 0, self.frame.size.width, self.frame.size.height)];
}
- (void) dealloc
{
    [image release];
    [super dealloc];
}
 */
- (void) initView
{
    backgroundView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, self.frame.size.width, self.frame.size.height)];
    scrollView = [[UIScrollView alloc] initWithFrame:CGRectMake(0, 0, self.frame.size.width, self.frame.size.height)];
    [self addSubview:backgroundView];
    [self addSubview:scrollView];
    [backgroundView release];
    [scrollView release];
    scrollView.backgroundColor = [UIColor clearColor];
}
@end
/////////////
@interface SEURLView : UIView 
{
    UIImageView* background;
    UIImageView* foreground;
}
@property (nonatomic, retain) UIImageView* background;
@property (nonatomic, retain) UIImageView* foreground;
@end
@implementation SEURLView

@synthesize background;
@synthesize foreground;
- (void) dealloc
{
    [super dealloc];
}
- (id) initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if(self)
    {
        background = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, frame.size.width, frame.size.height)];
        [self addSubview:background];
        [background release];
        
        foreground = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, frame.size.width, frame.size.height)];
        [self addSubview:foreground];
        [foreground release];
    }
    return self;
}
@end
////////
@interface SEHasSelectedImageView : UIImageView
{
    UIImageView* mForeground;
    UIImage* mFgImage;
}
- (void) setForegroundImage: (UIImage*) image;
- (void) setSelected: (BOOL) b;
- (BOOL) isSelected;
@end
@implementation SEHasSelectedImageView

- (void) createChild: (CGRect)frame
{
    mForeground = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, frame.size.width, frame.size.height)];
    [self addSubview:mForeground];
    [mForeground release];
}
- (id) initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if(self)
    {
        [self createChild:frame];
    }
    return self;
}
- (id) initWithCoder:(NSCoder *)aDecoder
{
    self = [super initWithCoder:aDecoder];
    if(self)
    {
        [self createChild:self.frame];
    }
    return self;
}
- (void) setForegroundImage:(UIImage *)image
{
    [mFgImage release];
    mFgImage = [image retain];
}
- (void) setSelected: (BOOL) b
{
    if(b)
    {
        mForeground.image = mFgImage;
    }
    else {
        mForeground.image = nil;
    }
    
}
- (BOOL) isSelected
{
    return mForeground.image != nil;
}
- (void) dealloc
{
    [mFgImage release];
    [super dealloc];
}
@end
/////////

//#define SHARE_IMAGE_NUM 5
@implementation SEShareImageOperationViewHandler
- (id) init
{
    self = [super init];
    if(self)
    {
        mViewNav = [PhotoFrameAppDelegate getViewNavigator];
        mImageViewForNotShare = [[NSMutableArray array] retain];
        for(int i = 0 ; i < SHARE_IMAGE_NUM ; i++)
        {
            UIImageView* imageView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, 10, 10)];
            [mImageViewForNotShare addObject:imageView];
            [imageView release];
        }
    }
    return self;
}
- (void) dealloc
{
    [mImageViewForNotShare release];
    [super dealloc];
}
- (SEPAGE_SCROLLVIEW_TYPE) getScrollViewType
{
    if(mOperationContainer.mType == SELECTED_IMAGE_VIEW)
    {
        return COREDATA_SCROLLVIEW;
    }
    else if(mOperationContainer.mType == IMAGE_PICKER)
    {
        return PHOTOLIB_SCROLLVIEW;
    }
    else
    {
        return INVALID_SCROLLVIEW;
    }
}
- (SEPageUIScrollView*) getScrollView
{
    SEPageUIScrollView* scrollView = nil;
    if(mOperationContainer.mType == SELECTED_IMAGE_VIEW)
    {
        scrollView = (SEPageUIScrollView*)[mOperationContainer contentView];
    }
    else if(mOperationContainer.mType == IMAGE_PICKER)
    {
        scrollView = (SEPageUIScrollView*)[mOperationContainer contentView];
    }
    return scrollView;
}
- (void) clear
{
    mBigImageView = nil;
    [mSelectedImageViewIndex release];
}
- (SEShareViewItem*) getSelectedShareItem
{
    SEPopupView* popup = [mViewNav getPopupView];
    SEURLListScrollView* urlListView = (SEURLListScrollView*)[popup.mContentView viewWithTag:107];
    for(UIView* child in urlListView.subviews)
    {
        if([child isMemberOfClass:[SEShareViewItem class]])
        {
            SEShareViewItem* v = (SEShareViewItem*)child;
            if(v.mSelected)
            {
                return v;
            }
        }
    }
    return nil;
}

- (UIImageView*) getUnsharedSignImageView: (int)i
{
    UIImageView* imageView = (UIImageView*)[mView viewWithTag:i + 108];
    UIImageView* imageViewForSign = [mImageViewForNotShare objectAtIndex:i];
    if(imageViewForSign.superview != mView)
    {
        CGRect rect = imageView.frame;
        rect.origin.x = imageView.frame.origin.x + 0.75 * imageView.frame.size.width;
        rect.origin.y = imageView.frame.origin.y + 0.75 * imageView.frame.size.height;
        rect.size.width = 0.25 * imageView.frame.size.width;
        rect.size.height = 0.25 * imageView.frame.size.height;
        imageViewForSign.frame = rect;
        [mView addSubview:imageViewForSign];
    }
    return imageViewForSign;
}

- (void) setMessage:(NSString *)msg data: (NSArray*)data
{
    SEShareViewItem* selectedItem = [self getSelectedShareItem];
    NSString* shareWayName = [selectedItem getShareWayName];
    if([msg isEqualToString:@"signin_ok"])
    {
        NSString* str = [NSString stringWithFormat:@"%@ %@",  @"Sign In Ok for", shareWayName];
        mMessageView.label.text = str;
        selectedItem.mStateText = [NSString stringWithFormat:@"%@", str];
    }
    else if([msg isEqualToString:@"signin_error"])
    {
        NSString* str = [NSString stringWithFormat:@"%@ %@", @"Sign In Error for", shareWayName];
        mMessageView.label.text = str;
        selectedItem.mStateText = [NSString stringWithFormat:@"%@", str];
        if(data.count > 0)
        {
            [self indicateImageHasNotShared:data];
            [self setTextWithShareState: YES];
        }
    }
    else if([msg isEqualToString:@"signing_in"])
    {
        NSString* str = [NSString stringWithFormat:@"%@ %@", @"SigningIn for", shareWayName];
        mMessageView.label.text = str;
        selectedItem.mStateText = [NSString stringWithFormat:@"%@", str];
    }
    else if([msg isEqualToString:@"signout_ok"])
    {
        NSString* str = [NSString stringWithFormat:@"%@ %@", @"Sign Out OK for", shareWayName];
        mMessageView.label.text = str;
        selectedItem.mStateText = [NSString stringWithFormat:@"%@", str];
    }
    else if([msg isEqualToString:@"upload_error"])
    {
        NSString* str = [NSString stringWithFormat:@"%@ %@", @"Upload Image Error for", shareWayName];
        mMessageView.label.text = str;
        if(data.count > 0)
            [self indicateImageHasNotShared:data];
        selectedItem.mStateText = [NSString stringWithFormat:@"%@", str];
        [self setTextWithShareState: YES];
    }
    else if([msg isEqualToString:@"upload_error_no_room"])
    {
        NSString* str = [NSString stringWithFormat:@"%@ %@", @"No enough room in ", shareWayName ];
        mMessageView.label.text = str;
        if(data.count > 0)
            [self indicateImageHasNotShared:data];
        selectedItem.mStateText = [NSString stringWithFormat:@"%@", str];
        [self setTextWithShareState: YES];
    }
    else if([msg isEqualToString:@"upload_ok"])
    {
        NSString* str = [NSString stringWithFormat:@"%@ %@", @"Upload Image OK for", shareWayName ];
        mMessageView.label.text = str;
        [self indicateAllImageHasShared:YES];
        selectedItem.mStateText = [NSString stringWithFormat:@"%@", str];
    }
    else if([msg isEqualToString:@"other_error"])
    {
        NSString* str = @"Network Error";
        mMessageView.label.text = str;
        selectedItem.mStateText = [NSString stringWithFormat:@"%@", str];
        if(data.count > 0)
        {
            [self indicateImageHasNotShared:data];
            [self setTextWithShareState: YES];
        }
    }
    else if([msg isEqualToString:@"request_access_token"])
    {
        NSString* str = [NSString stringWithFormat:@"%@ %@", @" Requesting Access Token", shareWayName];
        mMessageView.label.text = str;
        selectedItem.mStateText = [NSString stringWithFormat:@"%@", str];
    }
}
- (void) indicateAllImageHasShared : (BOOL) allShareOK
{
    if(allShareOK)
    {
        int num = mSelectedImageViewIndex.count;
        if(num > SHARE_IMAGE_NUM)
            num = SHARE_IMAGE_NUM;
        for(int i = 0 ; i < num ; i++)
        {
            UIImageView* imageView = [self getUnsharedSignImageView:i];
            imageView.image = [mViewNav.mResLoader getImage:@"ShareImageYes"];
            //[image removeFromSuperview];
        }
    }
    else
    {
        int num = mSelectedImageViewIndex.count;
        if(num > SHARE_IMAGE_NUM)
            num = SHARE_IMAGE_NUM;
        for(int i = 0 ; i < num ; i++)
        {
            UIImageView* imageView = [self getUnsharedSignImageView:i];
            imageView.image = nil;
        }
    }
    
}
- (void) indicateImageHasNotShared: (NSArray*) imageURLArray
{
    SEPageUIScrollView* scrollView = [self getScrollView];
    int num = mSelectedImageViewIndex.count;
    if(num > SHARE_IMAGE_NUM)
        num = SHARE_IMAGE_NUM;
    for(int i = 0 ; i < imageURLArray.count ; i++)
    {
        SEPageImageURL* url = [imageURLArray objectAtIndex:i];
        NSLog(@"Unupload image url= %@", url.url);
        
        for(int j = 0 ; j < num ; j++)
        {
            int index = [[mSelectedImageViewIndex objectAtIndex:j] intValue];
            NSLog(@"j = %d", j);
            SEPageImageURL* selectedImageURL = [scrollView getImageURL:index];
            if([[url.url absoluteString] isEqualToString:[selectedImageURL.url absoluteString]])
            {
                NSLog(@"unupload image url = %@", url.url);
                UIImageView* imageUnsharedSign = [self getUnsharedSignImageView:j];
                NSLog(@"imageUnsharedSign = %@", imageUnsharedSign);
                imageUnsharedSign.image = [mViewNav.mResLoader getImage:@"ShareImageNotOK"];
            }
        }
    }
    for(int j = 0 ; j < num ; j++)
    {
        int index = [[mSelectedImageViewIndex objectAtIndex:j] intValue];
        SEPageImageURL* url = [scrollView getImageURL:index];
        BOOL found = NO;
        for(int i = 0 ; i < imageURLArray.count ; i++)
        {
            SEPageImageURL* notShareURL = [imageURLArray objectAtIndex:i];
            if([[url.url absoluteString] isEqualToString:[notShareURL.url absoluteString]])
            {
                found = YES;
                break;
            }
        }
        if(found == NO)
        {
            UIImageView* imageUnsharedSign = [self getUnsharedSignImageView:j];
            imageUnsharedSign.image = [mViewNav.mResLoader getImage:@"ShareImageYes"];
        }
    }
}
- (void)handleShareCancel: (id)sender
{
    [self clear];
    [mViewNav dismissPopup];
    //self release
    [self release];
    //end
}
- (void) handleShareOk: (id)sender
{
    NSLog(@"share image");
    SEShareViewItem* viewItem = [self getSelectedShareItem];
    if(viewItem)
    {
        SEPageUIScrollView* scrollView = [self getScrollView]; 
        NSArray* urlArray = [NSArray array];
        int num = mSelectedImageViewIndex.count;
        if(num > SHARE_IMAGE_NUM)
            num = SHARE_IMAGE_NUM;
        for(int i = 0 ; i < num ; i++)
        {
            int index = [[mSelectedImageViewIndex objectAtIndex:i] intValue];
            SEPageImageURL* url = [scrollView getImageURL:index];
            SEPageImageURL* newUrl = [url clone];
            urlArray = [urlArray arrayByAddingObject:newUrl];
        }
        [viewItem shareImage:urlArray];
    }

}

- (UIView*)createShareView
{
    CGSize bgTopSize = CGSizeMake(1024 / 2.0, 768  / 2.0);
    UIImage* bgBottom = [mViewNav.mResLoader getImage:@"ShareImageBackgroundBottom"];
    CGSize bgBottomSize = CGSizeMake(bgBottom.size.width, bgBottom.size.height);
    UIView* background = [[UIView alloc] initWithFrame:CGRectMake(0, 0, bgTopSize.width, bgTopSize.height + bgBottomSize.height)];
    background.backgroundColor = [UIColor clearColor];
    UIImageView* bgTopView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, bgTopSize.width, bgTopSize.height)];
    UIImage* bgTop = [mViewNav.mResLoader getImage:@"ShareImageBackgroundTop"];
    bgTop = [SEUtil imageWithCap:bgTop top:0.1 bottom:0.9 left:0.1 right:0.9];
    bgTopView.image = bgTop;
    
    [background addSubview:bgTopView];
    [bgTopView release];
    
    UIImageView* bgBottomView = [[UIImageView alloc] initWithFrame:CGRectMake(0, bgTopSize.height, bgBottomSize.width, bgBottomSize.height)];
    bgBottom = [SEUtil imageWithCap:bgBottom top:0.1 bottom:0.9 left:0.1 right:0.9];
    bgBottomView.image = bgBottom;
    [background addSubview:bgBottomView];
    [bgBottomView release];
    float leftPadding = 10;
    float topPadding = 10;
    CGSize urlListSize = CGSizeMake(100, 50);
    float smallImageListHeight = 50;
    UIImageView* bigImageView = [[UIImageView alloc] initWithFrame:CGRectMake(leftPadding, topPadding, bgTopSize.width - urlListSize.width, bgTopSize.height - smallImageListHeight)];
    
}
- (void) handleTwitter: (UITapGestureRecognizer*)ges
{
    NSLog(@"## handle twitter ##");
}
- (void) handleGooglePlus: (UITapGestureRecognizer*)ges
{
    NSLog(@"hangleGooglePlus");
    /*
    SEWebServiceManager* webServiceManager = mViewNav.mWebServiceManager;
    if([webServiceManager isServiceSignIn:GOOGLE_PLUS] == NO)
    {
        [webServiceManager signIn:GOOGLE_PLUS];
    }
     */
}
- (void)finishedSharing:(BOOL)shared
{
    NSLog(@"share finished ");
}

- (void) handleBgTap: (UITapGestureRecognizer*)tap
{
    switch (tap.state) {
        case UIGestureRecognizerStateBegan:
        {
            NSLog(@"tap begin");
        }
            break;
        case UIGestureRecognizerStateChanged:
        {
            NSLog(@"tap changed");
        }
            break;
        case UIGestureRecognizerStateEnded:
        {
            NSLog(@"tap end");
            
            
        }
            break;
        case UIGestureRecognizerStateCancelled:
        {
            NSLog(@"tap cancel");
        }
            break;
        default:
            break;
    }
    SEPopupView* popup = [mViewNav getPopupView];
    SEURLListScrollView* urlListView = (SEURLListScrollView*)[popup.mContentView viewWithTag:107];
    UIView* selectedItem = tap.view.superview;// the taped view is mImageBgView in SEShareViewItem
    for(UIView* child in urlListView.subviews)
    {
        if([child isMemberOfClass:[SEShareViewItem class]])
        {
            SEShareViewItem* v = (SEShareViewItem*)child;
            if(child == selectedItem)
            {
                v.mSelected = YES;
            }
            else
            {
                v.mSelected = NO;
            }
        }
    }
    SEShareViewItem* shareItem = (SEShareViewItem*)selectedItem;
    NSArray* notSharedImage = [shareItem getUnsharedImageArray];
    mMessageView.label.text = shareItem.mStateText;
    if(notSharedImage.count > 0)
    {
        [self indicateImageHasNotShared:notSharedImage];
        [self setTextWithShareState: YES];
    }
    else
    {
        if([shareItem hasStartShare])
        {
            [self indicateAllImageHasShared: YES];  
            [self setTextWithShareState: NO];
        }
        else
        {
            [self indicateAllImageHasShared:NO];
            [self setTextWithShareState:NO];
        }
    }
}

- (void) setBackgroundForShareView: (UIView*)v
{
    UIImageView* bgTopView = (UIImageView*)[v viewWithTag:101];
    UIImage* bgTop = [mViewNav.mResLoader getImage:@"ShareImageBackgroundTop"];
    bgTop = [SEUtil imageWithCap:bgTop top:0.1 bottom:0.9 left:0.1 right:0.9];
    bgTopView.image = bgTop;
    
    UIImageView* bgBottomView = (UIImageView*)[v viewWithTag:102];
    UIImage* bgBottom = [mViewNav.mResLoader getImage:@"ShareImageBackgroundBottom"];
    bgBottom = [SEUtil imageWithCapInset: bgBottom top:16 bottom:16 left:103 right:103];
    bgBottomView.image = bgBottom;
    
    UIImageView* bgBigImageView = (UIImageView*)[v viewWithTag:105];
    UIImage* bgBigImage = [mViewNav.mResLoader getImage:@"ShareImageBigImageBg"];
    bgBigImage = [SEUtil imageWithCap:bgBigImage top:0.1 bottom:0.1 left:0.1 right:0.1];
    bgBigImageView.image = bgBigImage;
    
    UIImageView* bgSmallImageView = (UIImageView*)[v viewWithTag:106];
    UIImage* bgSmallImage = [mViewNav.mResLoader getImage:@"ShareImageSmallImageBg"];
    bgSmallImage = [SEUtil imageWithCap:bgSmallImage top:0.1 bottom:0.1 left:0.1 right:0.1];
    bgSmallImageView.image = bgSmallImage;
    
    SEURLListScrollView* urlListView = (SEURLListScrollView*)[v viewWithTag:107];
    [urlListView initView];
    urlListView.backgroundColor = [UIColor clearColor];
    UIImage* urlListImage = [mViewNav.mResLoader getImage:@"ShareImageURLListBg"];
    urlListImage = [SEUtil imageWithCap:urlListImage top:0.1 bottom:0.9 left:0.1 right:0.9];
    urlListView.backgroundView.image = urlListImage;
    float startx = 2;
    float starty = 4;
    float width = urlListView.frame.size.width;
    float height = 50;
    UIImage* urlImage = [mViewNav.mResLoader getImage:@"ShareImageURLBg"];
    urlImage = [SEUtil imageWithCap:urlImage top:0.1 bottom:0.1 left:0.1 right:0.1];
    
    mMessageView = (SEPopupLabel*)[v viewWithTag:210];
    mMessageView.background.image = [SEUtil imageWithCap:[mViewNav.mResLoader getImage:@"ShareImageMsgBg"] top:0.1 bottom:0.9 left:0.1 right:0.9];;
    setLabelFont(mMessageView.label, @"", [UIColor blackColor], [SESystemConfig getFontName], 30);
    
    
    //UIImage* twitterImage = [mViewNav.mResLoader getImage:@"TwitterButtonImage"];
    //UIImage* googlePlusImage = [mViewNav.mResLoader getImage:@"Pica"];
    SEL shareSiteHandler[2] = {@selector(handleTwitter:), @selector(handleGooglePlus:)};
    //UIImage* shareSiteImage[2] = {twitterImage, googlePlusImage};
    float itemWidth = 10; 
    float itemHeight = 10;
    for(int i = SHARE_BY_TWITTER ; i < SHARE_WAY_NUM; i++)
    {
        SEShareViewItem* viewItem = [[SEShareViewItem alloc] initWithFrame:CGRectMake(startx, starty, width, height) shareWay:(SHARE_WAY)i];
        if(i == SHARE_BY_TWITTER)
        {
            viewItem.mSelected = YES;
        }
        viewItem.mParent = self;
        viewItem.mResourceScrollViewType = [self getScrollViewType ];
        [urlListView addSubview:viewItem];
        viewItem.userInteractionEnabled = YES;
        [viewItem release];
        [viewItem setButtonTarget:self action:shareSiteHandler[i]];
        [viewItem setBackgroundTarget:self action:@selector(handleBgTap:)];
        [viewItem changeTextByAuth];
        //viewItem.mShareWay = i;
        starty += viewItem.frame.size.height;
        itemWidth = viewItem.frame.size.width;
        itemHeight = viewItem.frame.size.height;
    }
    UIImageView* facebookImageVeiw = [[UIImageView alloc] initWithFrame:CGRectMake(startx, starty, itemWidth, itemHeight)];
    facebookImageVeiw.image = [mViewNav.mResLoader getImage:@"FacebookLogo"];
    [urlListView addSubview:facebookImageVeiw];
    [facebookImageVeiw release];
    starty += itemHeight;
    
    urlListView.scrollView.contentSize = CGSizeMake(width, starty);
}
- (void) imageViewTapHandler: (UITapGestureRecognizer*)ges
{
    NSLog(@"tap image view");
    SEFrameImageView* imageView = (SEFrameImageView*)ges.view;
    if(imageView == mCurrentImageView)
        return;
    imageView.highlighted = YES;
    mCurrentImageView.highlighted = NO;
    mCurrentImageView = imageView;
    int i = imageView.tag - 108;
    int index = [[mSelectedImageViewIndex objectAtIndex:i] intValue];
    
    SEPageUIScrollView* scrollView = [self getScrollView];
    SEPageImageURL* url = [scrollView getImageURL:index];
    
    SEImageViewLoader* imageViewLoader = [[SEImageViewLoader alloc] init];
    imageViewLoader.imageView = mBigImageView;
    imageViewLoader.viewNav = mViewNav;
    imageViewLoader.destSize = CGSizeMake(mBigImageView.frame.size.width, mBigImageView.frame.size.height);
    //SEImageAsyncLoader* asyncLoader = [[SEImageAsyncLoader alloc] init];
    //asyncLoader.mViewNav = [PhotoFrameAppDelegate getViewNavigator];
    ALAssetsLibrary* lib = [[[ALAssetsLibrary alloc] init] autorelease];
    [imageViewLoader setAssetLibOwn:lib];
    if(mOperationContainer.mType == IMAGE_PICKER)
    {
        [imageViewLoader loadFullRepresentation:url.url];
    }
    else
    {
        [imageViewLoader loadCoreDataFullRepresentation:url.url date:url.urlDate];
        //[imageViewLoader loadCoreDataImageWithRatio:url.url date:url.urlDate size:CGSizeMake(mBigImageView.frame.size.width, mBigImageView.frame.size.height)];
    }
    //[scrollView loadImageFromPhotoLib:url size:CGSizeMake(mBigImageView.frame.size.width - 10, mBigImageView.frame.size.height - 10) withHandler:imageViewLoader];
}
- (void) setTextWithShareState: (BOOL) hasNoShared
{
    SETextImageButton * okButton = (SETextImageButton*)[mView viewWithTag:104];
    if(hasNoShared)
    {
        [okButton setTextImage:@"retry" indicateImage:nil alignment:UITextAlignmentCenter];
    }
    else
    {
        [okButton setTextImage:@"share" indicateImage:nil alignment:UITextAlignmentCenter];
    }
}
- (void)handleShareOp
{
    //[mViewNav resetSharedImageArray];
    SEPageUIScrollView* scrollView = [self getScrollView];;

    UIView* v = [[[NSBundle mainBundle] loadNibNamed:@"UserShareView" owner:self options:nil] lastObject];
    mView = v;
    v.userInteractionEnabled = YES;
    v.backgroundColor = [UIColor clearColor];
    [self setBackgroundForShareView: v];
    int tags[] = {108, 109, 110, 111, 112};
    int imageNum = [mSelectedImageViewIndex count];
    imageNum = MIN(SHARE_IMAGE_NUM, imageNum);
    for(int i = 0 ; i < SHARE_IMAGE_NUM ; i++)
    {
        SEFrameImageView* imageView = (SEFrameImageView*)[v viewWithTag:tags[i]];
        imageView.userInteractionEnabled = YES;
        imageView.backgroundColor = [UIColor clearColor];
    }
    UIImageView* imageView = (UIImageView*)[v viewWithTag:113];
    mBigImageView = imageView;
    imageView.contentMode = UIViewContentModeCenter;
    

    /*
    SEImageViewLoader* imageViewLoader = [[SEImageViewLoader alloc] init];
    imageViewLoader.imageView = imageView;
    imageViewLoader.viewNav = mViewNav;
    imageViewLoader.destSize = CGSizeMake(mBigImageView.frame.size.width, mBigImageView.frame.size.height);
    ALAssetsLibrary* lib = [[[ALAssetsLibrary alloc] init] autorelease];
    [imageViewLoader setAssetLibOwn:lib];
    if(mOperationContainer.mType == IMAGE_PICKER)
    {
        [imageViewLoader loadFullRepresentation:url.url];
    }
    else
    {
        [imageViewLoader loadCoreDataFullRepresentation:url.url date:url.urlDate];
    }
    */
    NSNumber* num = [mSelectedImageViewIndex objectAtIndex:0];
    SEPageImageURL* url = [scrollView getImageURL:[num intValue]];

    NSMutableArray* imageLoaderList = [NSMutableArray array];
    SEImageViewLoaderContent* bigViewContent = [[SEImageViewLoaderContent alloc] init];
    bigViewContent.imageView = mBigImageView;
    bigViewContent.destSize = CGSizeMake(mBigImageView.frame.size.width, mBigImageView.frame.size.height);
    bigViewContent.url = url.url;
    bigViewContent.urlDate = url.urlDate;
    bigViewContent.viewType = mOperationContainer.mType;
    [imageLoaderList addObject:bigViewContent];
    [bigViewContent release];

    UIImage* frameImage = [mViewNav.mResLoader getImage:@"ImagePickerFrameImage"];
    UIImage* hFrameImage = [mViewNav.mResLoader getImage:@"PageScrollViewImageViewHighlighted"];
    for(int i = 0 ; i < imageNum ; i++)
    {
        int tag = tags[i];
        
        SEFrameImageView* imageView = (SEFrameImageView*)[v viewWithTag:tag];
        imageView.frameImage = frameImage;
        imageView.highlightedFrameImage = hFrameImage;
        UITapGestureRecognizer* ges = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(imageViewTapHandler:)];
        [imageView addGestureRecognizer:ges];
        [ges release];
        
        int index = [[mSelectedImageViewIndex objectAtIndex:i] intValue];
        SEPageImageURL* url = [scrollView getImageURL:index];
        /*
        SEImageViewLoader* imageViewLoader = [[SEImageViewLoader alloc] init];
        imageViewLoader.imageView = imageView;
        imageViewLoader.viewNav = mViewNav;
        ALAssetsLibrary* lib = [[[ALAssetsLibrary alloc] init] autorelease];
        [imageViewLoader setAssetLibOwn:lib];
        
        if(mOperationContainer.mType == IMAGE_PICKER)
        {
            [imageViewLoader loadImageFromPhotoLib:url.url size:CGSizeMake(imageView.frame.size.width - 20, imageView.frame.size.height - 20)];
        }
        else
        {
            [imageViewLoader loadCoreDataImage:url.url date:url.urlDate size:CGSizeMake(imageView.frame.size.width - 20, imageView.frame.size.height - 20)];    
        }
        */
        SEImageViewLoaderContent* viewContent = [[SEImageViewLoaderContent alloc] init];
        viewContent.imageView = imageView;
        viewContent.destSize = CGSizeMake(0, 0);
        viewContent.url = url.url;
        viewContent.urlDate = url.urlDate;
        viewContent.viewType = mOperationContainer.mType;
        [imageLoaderList addObject:viewContent];
        [viewContent release];

        if(i == 0)
        {
            imageView.highlighted = YES;
            mCurrentImageView = imageView;
        }
        else 
        {
            imageView.highlighted = NO;
        }
    }
    
    SEImageViewListLoader* viewListLoader = [[SEImageViewListLoader alloc] init];
    viewListLoader.imageViewLoaderContentList = imageLoaderList;
    viewListLoader.currentIndex = 0;
    [viewListLoader load];
    
    SETextImageButton* cancelButton = (SETextImageButton*)[v viewWithTag:103];
    SETextImageButton * okButton = (SETextImageButton*)[v viewWithTag:104];
    [cancelButton setButtonHandler:self action:@selector(handleShareCancel:)];
    [okButton setButtonHandler:self action:@selector(handleShareOk:)];
    /*
    [cancelButton setButtonBackground:@"ShareButtonNormalImage" select:@"ShareButtonHImage"];
    [okButton setButtonBackground:@"ShareButtonNormalImage" select:@"ShareButtonHImage"];
    */
    [cancelButton setButtonBackground:@"ShareButtonNormal" select:@"ShareButtonH"];
    [okButton setButtonBackground:@"ShareButtonNormal" select:@"ShareButtonH"];
    [cancelButton setTextImage:@"cancel" indicateImage:nil alignment:UITextAlignmentCenter];
    [okButton setTextImage:@"share" indicateImage:nil alignment:UITextAlignmentCenter];
    /*
    [cancelButton addTarget:self action:@selector(handleShareCancel:) forControlEvents:UIControlEventTouchUpInside];
    [okButton addTarget:self action:@selector(handleShareOk:) forControlEvents:UIControlEventTouchUpInside];
     */
    [self retain]; // self will be released when OperationView remove from super view. so we should retain it ,and use it by later.
    [mViewNav popupView: v];
}
- (void)handleOperation:(NSString*)op view:(SEOperationView *)opView
{
    mOperationContainer = mViewNav.mOperationContainer;
    mSelectedImageViewIndex = [[NSArray arrayWithArray:mViewNav.mSelectedImageViewIndex] retain];
    if(mViewNav.mCurrentFloatViewType == FLOATVIEW_IMAGE)
    {
        if([op isEqualToString:@"share_op"]) 
        {
            NSLog(@"share operation\n");
            [self handleShareOp];
        }
    }
}
@end
@interface SEShareDeleteImageOperationViewHandler : SEShareImageOperationViewHandler  

@end
@implementation SEShareDeleteImageOperationViewHandler
- (id) init
{
    self = [super init];
    if(self)
    {
    }
    return self;
}
- (void) handleDeleteOp
{
    if(mViewNav.mOperationContainer.mType == SELECTED_IMAGE_VIEW)
    {
        SEPageUIScrollView* scrollView = (SEPageUIScrollView*)[mViewNav.mOperationContainer contentView];
        NSMutableArray* selectedIndex = mViewNav.mSelectedImageViewIndex;
        for(NSNumber* n in selectedIndex)
        {
            SEPageUIImageView* v = [scrollView imageView:[n intValue]];
            v.highlighted = NO;
        }
        [scrollView removeFromPhotoURLAsset:mViewNav.mSelectedImageViewIndex];
    }
}
- (void) handleOperation:(NSString *)op view:(SEOperationView *)opView
{
    if([op isEqualToString:@"delete_op"])
    {
        NSLog(@"delete operation\n");
        [self handleDeleteOp];
    }
    else 
    {
        [super handleOperation:op view:opView];
    }
}
@end

@implementation SEViewNavigator (Private)
- (void) setPrevCurrView: (VIEW_TYPE)vp
{
    mPrevCurrView = vp;
}
- (NSString*) mDrawingImageList
{
    return mDrawingImageList;
}
- (void) finishGetImageFromPhotoLib: (NSMutableArray*)data
{
    ALAssetsLibrary* lib = [data objectAtIndex:0];
    NSMutableArray* photoArray = [data objectAtIndex:1];
    //to do some thing about image url
    NSMutableArray* imageArray = [NSMutableArray array];
    if(photoArray.count > 5)
    {
        for(int i = 0 ; i < 5 ; i++)
        {
            [imageArray addObject:[photoArray objectAtIndex:i]];
        }
    }
    else
    {
        for(int i = 0 ; i < photoArray.count; i++)
        {
            [imageArray addObject:[photoArray objectAtIndex:i]];
        }
    }
    assert(imageArray.count <= 5);
    NSMutableArray* photoURLArray = [NSMutableArray array];
    for(int i = 0; i < imageArray.count ; i++)
    {
        SEPageImageURL* url = [[[SEPageImageURL alloc] init] autorelease];
        SEPhotoLibData* data = [imageArray objectAtIndex:i];
        url.url = data.url;
        url.urlDate = data.urlDate;
        url.origWidth = data.width;
        url.origHeight = data.height;
        url.orientation = data.orient;
        url.type = SEPAGE_PHOTO_LIB_URL;
        [photoURLArray addObject:url];
    }
    [self setPhotoURLToCoreData:photoURLArray imageListName:[SESystemConfig getDefaultImageListName]];
    // end
    [data release];
    [lib release];
    [photoArray release];
}
- (void) getDataFromPhotoLib
{
    ALAssetsLibrary* lib = [[ALAssetsLibrary alloc] init];
    NSMutableArray* photoArray = [[NSMutableArray array] retain];
    NSMutableArray* types = [NSMutableArray array];
    
    [types addObject:[NSNumber numberWithInt:ALAssetsGroupLibrary]];
    [SEUtil getImagesFromPhotoLib:lib assetType:types photoContainer:photoArray finishedTarget:self finishedAction:@selector(finishGetImageFromPhotoLib:)];
}
- (void) createDefaultSelectedImageAndMusic
{
    [self getDataFromPhotoLib];
    NSArray* defaultMusicArray = [SEUtil getMusicFromMusicLib];
    NSMutableArray* itemPropertyArray = [NSMutableArray array];
    int num = defaultMusicArray.count;
    if(num > 5)
        num = 5;
    for(int i = 0 ; i < num ; i++)
    {
        SEMusicItemProperty* item = [[[SEMusicItemProperty alloc] init] autorelease];
        SEMusicLibData* data = [ defaultMusicArray objectAtIndex:i];
        item.title = data.title;
        item.artist = data.artist;
        item.album = data.album;
        [itemPropertyArray addObject:item];
    }
    [self setSelectedMusicToCoreData:itemPropertyArray musicListName:[SESystemConfig getDefaultMusicListName]];
    [self attachImageToMusic:[SESystemConfig getDefaultImageListName] imageListName:[SESystemConfig getDefaultMusicListName]];
}
- (void) setPhotoURLToCoreData:(NSMutableArray *)photoURLArray imageListName: (NSString*)imageListName
{
    ImageList* il = [self getImageListByName:imageListName];
    [self removeAllSelectedImageInImageList:imageListName];
    NSArray* selectedImageArray = [self getSelectedImageArrayByName:mCurrentLoadedImageListName];
    assert(selectedImageArray.count == 0);
    for(int i = 0 ; i < photoURLArray.count ; i++)
    {
        SEPageImageURL* imageURL = [photoURLArray objectAtIndex:i];
        assert(imageURL != nil);
        SelectedImage* si = (SelectedImage*)[self newObjectByEntityName:@"SelectedImage"];
        si.seq = [NSNumber numberWithInt:i];
        assert(imageURL.url != nil || imageURL.filepath != nil);
        si.url = [imageURL.url absoluteString];
        si.filepath = [imageURL.filepath absoluteString];
        si.urldate = imageURL.urlDate;
        si.width = [NSNumber numberWithInt:(int)imageURL.origWidth];
        si.height = [NSNumber numberWithInt:(int)imageURL.origHeight];
        si.urltype = [NSNumber numberWithInt:imageURL.type];
        si.orientation = [NSNumber numberWithInt:imageURL.orientation];
        NSLog(@"save url width = %f, height = %f, oreint = %d", imageURL.origWidth, imageURL.origHeight, imageURL.orientation);
        [il addSelectedimageObject:si];
    }
    [self saveCoreDataContext];
}
- (void) setSelectedMusicToCoreData: (NSArray*) musicArray musicListName: (NSString*) musicListName
{
    MusicList* musicList = [self getMusicListByName:musicListName];
    NSSet* selectedMusic = musicList.selectedmusic;
    [musicList removeSelectedmusic:selectedMusic];
    for(int i = 0 ; i < musicArray.count ; i++)
    {
        SEMusicItemProperty* item = [musicArray objectAtIndex:i];
        SelectedMusic* sm = (SelectedMusic*)[self newObjectByEntityName:@"SelectedMusic"];
        sm.seq = [NSNumber numberWithInt:i];
        sm.title = item.title;
        sm.album = item.album;
        sm.singer = item.artist;
        [musicList addSelectedmusicObject:sm];
    }
    [self saveCoreDataContext];
}

- (void) setMDrawingImageList:(NSString *)d
{
    [mDrawingImageList release];
    mDrawingImageList = [d retain];
}
- (MPMediaItem*) getMediaItem: (NSArray*)mediaArray title:(NSString*)title artist: (NSString*) artist album: (NSString*)album
{
    for(MPMediaItemCollection* collection in mediaArray)
    {
        for(MPMediaItem* item in [collection items])
        {
            NSString* tmpTitle  = [item valueForKey:MPMediaItemPropertyTitle];
            NSString* tmpAlbum = [item valueForKey:MPMediaItemPropertyAlbumArtist];
            NSString* tmpArtist = [item valueForKey:MPMediaItemPropertyArtist];
            if(tmpArtist == nil)
                tmpArtist = @"unknown";
            if(tmpAlbum == nil)
                tmpAlbum = @"unknown";
            if([tmpTitle isEqualToString:title] && [tmpAlbum isEqualToString:album] && [tmpArtist isEqualToString:artist])
                return item;
        }
    }
    return nil;
}
- (void) stopMusic
{
    
    MPMusicPlayerController* player = [MPMusicPlayerController applicationMusicPlayer];
    [player stop];
}
- (void) resumeMusic
{
    UserInfo* userInfo = [self getUserInfo];
    NSString* imageListName = userInfo.currentimagelist;
    MusicList* musicList = [self getMusicListByImageList:imageListName];
    NSLog(@"#### imageListName = %@ ######\n", imageListName);
    if(mPrevCurrView == PREVIEW_3D)
    {
        [self stopMusic];
        if(mDrawingStateManager.mCurrentPlayState == PLAY_STATE)
            [self playMusic];
    }
    else
    {
        if(musicList && mDrawingStateManager.mCurrentPlayState == PLAY_STATE)
        {
            if([musicList.name isEqualToString:mCurrentPlayMusicListName])
            {
                MPMusicPlayerController* player = [MPMusicPlayerController applicationMusicPlayer];
                [player play];
            }
            else 
            {
                [self playMusic];
            }
        }
    }
}
- (void) musicItemChanged: (NSNotification*) n
{
    NSLog(@"musicItem changed");
    MPMusicPlayerController* player = [MPMusicPlayerController applicationMusicPlayer];
    if([n object] == player && mCurrView == MAIN_DISPLAY)
    {
        NSString* title = [player.nowPlayingItem valueForProperty:MPMediaItemPropertyTitle];
        NSLog(@"current music title = %@", title);
        if(title && !mMusicObservFirst)
        {
            NSLog(@"finish one music");
            [self finishOneMusicPlay];
        }
        mMusicObservFirst = NO;
    }
}
- (void) playMusic
{
    UserInfo* userInfo = [self getUserInfo];
    NSString* imageListName = userInfo.currentimagelist;
    MusicList* musicList  = [self getMusicListByImageList:imageListName];
    NSLog(@"#### imageListName = %@ ######\n", imageListName);
    if(musicList)
    {
        //NSSet* selectedMusic = musicList.selectedmusic;
        NSArray* selectedMusic = getMusicInMusicList(musicList);
        NSMutableArray* itemArray = [NSMutableArray array];
        for(SelectedMusic* sm in selectedMusic)
        {
            NSLog(@"sm seq = %d, sm title = %@, artist = %@, album = %@", [sm.seq intValue], sm.title, sm.singer, sm.album);

            NSArray* items = [SEUtil findMediaItemByTitle:sm.title aritst:sm.singer album:sm.album];
            if(items.count > 0)
            {
                MPMediaItem* item = [items objectAtIndex:0];
                [itemArray addObject:item];
            }
        }
        NSLog(@"#### music array count = %d ########\n", itemArray.count);
        if(itemArray.count > 0)
        {
            MPMediaItemCollection* queue = [MPMediaItemCollection collectionWithItems:itemArray];
            if(queue)
            {
                MPMusicPlayerController* player = [MPMusicPlayerController applicationMusicPlayer];
                
                [player setQueueWithItemCollection:queue];
                [player beginGeneratingPlaybackNotifications];
                [[NSNotificationCenter defaultCenter] removeObserver:self name:MPMusicPlayerControllerNowPlayingItemDidChangeNotification object:nil];
                [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(musicItemChanged:) name:MPMusicPlayerControllerNowPlayingItemDidChangeNotification object:nil];
                mMusicObservFirst = YES;
                UserInfo* userInfo = [self getUserInfo];
                int sequenceMode = [userInfo.imageplaymode intValue];
                if(sequenceMode == 0)
                {
                    player.shuffleMode = MPMusicShuffleModeDefault;
                }
                else
                {
                    player.shuffleMode = MPMusicShuffleModeSongs;
                }
                player.repeatMode =  MPMusicRepeatModeAll;
                [player play];
            }
        }
    }
}
- (void) pauseMusic
{
    UserInfo* userInfo = [self getUserInfo];
    NSString* imageListName = userInfo.currentimagelist;
    MusicList* musicList  = [self getMusicListByImageList:imageListName];
    NSLog(@"#### imageListName = %@ ######\n", imageListName);
    if(musicList)
    {
        mCurrentPlayMusicListName = musicList.name;
        MPMusicPlayerController* player = [MPMusicPlayerController applicationMusicPlayer];
        [player pause];
    }
}

- (void) pauseImagePlay
{
    SEContentViewContainer* c = mViewArray[MAIN_DISPLAY];
    SEMainDisplay* d = (SEMainDisplay*)[c contentView];
    if(mPainterManager.imageArray != nil)
    {
        [d setPlayIcon];
        [mPainterManager pauseDrawing];
    }
}
- (void) stopCurrentImageDraw
{
    [mPainterManager stopDrawImage];
}
- (void) endImageDraw
{
    DRAW_IMAGE_STATE currentDrawImageState = [self getDrawImageState];
    switch (currentDrawImageState)
    {
        case INIT_DRAW_IMAGE_STATE:
        {
            
        }
            break;
            
        case START_DRAW_IMAGE:
        {
            [self stopCurrentImageDraw];
            //[self setDrawImageState:STOPPING_DRAWING_IMAGE];
        }
            break;
        case PAUSE_DRAW_IMAGE:
        {
            //[self stopCurrentImageDraw];
            //[self setDrawImageState:STOPPING_DRAWING_IMAGE];
        }
            break;
        case STOPPING_DRAWING_IMAGE:
        {
            
        }
            break;
        case STOP_DRAW_IMAGE:
        {}
            break;
        case START_DRAW_IMAGE_PENDING:
        {}
            break;
        default:
            break;
    }
}
- (void)playImage
{
    SEContentViewContainer* c = mViewArray[MAIN_DISPLAY];
    SEMainDisplay* d = (SEMainDisplay*)[c contentView];
    DRAW_IMAGE_STATE currentDrawImageState = [self getDrawImageState];
    //if(currentDrawImageState == INIT_DRAW_IMAGE_STATE)
    switch (currentDrawImageState)
    {
        case INIT_DRAW_IMAGE_STATE:
        {
            if(mPainterManager.imageArray == nil)
            {
                [self displayNextImage];
            }
            else
            {
                [mPainterManager startDrawing];    
            }
            [d setPaurseIcon];
            [self setDrawImageState:START_DRAW_IMAGE];
        }
            break;
        
        case START_DRAW_IMAGE:
        {
            if(mPainterManager.imageArray != nil)
            {
                [mPainterManager pauseDrawing];
                [d setPlayIcon];
            }
            [self setDrawImageState:INIT_DRAW_IMAGE_STATE];
        }
            break;
        case PAUSE_DRAW_IMAGE:
        {
            
            
        }
            break;
        case STOPPING_DRAWING_IMAGE:
        {}
            break;
        case STOP_DRAW_IMAGE:
        {}
            break;
        case START_DRAW_IMAGE_PENDING:
        {}
            break;
        default:
            break;
    }
    /*
    if(mPainterManager.isPause)
    {
        if(mPainterManager.imageArray == nil)
        {
            [self displayNextImage];
        }
        else
        {
            [mPainterManager startDrawing];
        }
        [d setPaurseIcon];
    }
    else
    {
        if(mPainterManager.imageArray != nil)
        {
            [d setPlayIcon];
            [mPainterManager pauseDrawing];
        }
    }
     */
}
/*
- (void) testUserUpgradeThread
{
    NSLog(@"test user upgrade thread start");
    while(YES)
    {
        
        NSLog(@"sleep start");
        sleep(5000);
        NSLog(@"sleep end");
        [self performSelectorOnMainThread:@selector(finishOneImageDrawing) withObject:nil waitUntilDone:NO];
    }
}
- (void) testForUserUpgrade
{
    [NSThread detachNewThreadSelector:@selector(testUserUpgradeThread) toTarget:self withObject:nil];    
}
 */
- (void) handleToolBarButtonClick: (TOOLBAR_BUTTON_TYPE*) typePtr
{
    TOOLBAR_BUTTON_TYPE type = *typePtr;
    //[self notificationShow];
    //return;
    switch (type) 
    {
    case PLAY_PAUSE:
    {
        NSLog(@"play drawing\n");
        [mDrawingStateManager pressPlaypauseButton];
    }
    break;
    case MUSIC_SELECT:
    {
        NSLog(@"music select\n");
        //[self createDefaultSelectedImageAndMusic];
        //[self drawSignatureAnim];
        //break;
        /*
        static BOOL show = NO;
        if(show == NO)
        {
            [self playDrawFrameShowAnim];
            show = YES;
        }
        else 
        {
            [self playDrawFrameHideAnim];
            show = NO;
        }
        */
        
        
        [mDrawingStateManager moveToOtherView: MUSIC_PICKER];
        UserInfo* userInfo = [self getUserInfo];
        if(userInfo.currentmusiclist != nil)
        {
            self.mCurrentLoadedMusicListName = userInfo.currentmusiclist;
            [self setViewRelationType:TYPE1];
            mMoveViewFromMainDisplay = YES;
            [self moveToView:MAIN_DISPLAY_MUSIC_PICKER :MUSIC_PICKER hasAnimation:YES isPush:YES];
        }
        
    }
    break;
    case IMAGE_SELECT:
    {
        NSLog(@"image select\n");
        //[self pauseImagePlay];
        //[mPainterManager stopDrawImage];
        [mDrawingStateManager moveToOtherView: IMAGE_PICKER];
        //[self pauseMusic];
        UserInfo* userInfo = [self getUserInfo];
        if(userInfo.currentimagelist != nil)
        {
            self.mCurrentLoadedImageListName = userInfo.currentimagelist ;
            [self setViewRelationType:TYPE1];
            mMoveViewFromMainDisplay = YES;
            [self moveToView:MAIN_DISPLAY_IMAGE_PICKER :IMAGE_PICKER hasAnimation:YES isPush:YES];
        }
    }
    break;
    case OPTION:
    {
        NSLog(@"option select\n");
        //for test
        /*
        for(int i = 0 ; i < 28 ; i++)
        {
            [[self getUserUpgrade] finishOneImageDrawing];
        }
         */
        //end
        [mDrawingStateManager moveToOtherView: OPTIONS];
        [self setViewRelationType:TYPE1];
        mMoveViewFromMainDisplay = YES;
        [self moveToView:MAIN_DISPLAY_OPTIONS :OPTIONS hasAnimation:YES isPush:YES];
    }
        break;
    case PREVIEW:
    {
        NSLog(@"preview select\n");
        [mDrawingStateManager moveToOtherView: PREVIEW_3D];
        SE3DPreview* v = (SE3DPreview*)[self createPreview3D:CGRectMake(0, 0, mViewPortWidth, mViewPortHeight)];
        mPrevCurrView = MAIN_DISPLAY;
        mCurrView = PREVIEW_3D;
        //v.mDecorateView.image = image;
        //v.mScreenImage = image;
        [mRootView addSubview:v];
        UIImage* midImage = [UIImage imageNamed:@"painterbackground003_side.png"];
        float leftViewWidth = 1024;
        UIImageView* leftView = [[UIImageView alloc] initWithFrame:CGRectMake(-leftViewWidth, 0, leftViewWidth, midImage.size.height)];
        [mRootView addSubview:leftView];
        [leftView release];
        v.mLeftView = leftView;
        leftView.image = midImage;
        leftView.transform = CGAffineTransformMakeRotation(3.1415926);
        //[v release];
        [v startTransitionAnim];
        //[self setViewRelationType:TYPE1];
        //[self moveToView:PREVIEW_3D_ONLY :PREVIEW_3D hasAnimation:YES isPush:YES];
    }        
        break;
    case MUSIC_IMAGE_LIST:
        {
            NSLog(@"music image list select\n");
            //[self pauseImagePlay];
            [mDrawingStateManager moveToOtherView: MUSIC_IMAGE_LIST_ATTACH];
            [self pauseMusic];
            [self setViewRelationType:TYPE1];
            mMoveViewFromMainDisplay = YES;
            [self moveToView:MAIN_DISPLAY_MUSIC_IMAGE_LIST_ATTACH :MUSIC_IMAGE_LIST_ATTACH hasAnimation:YES isPush:YES];
        }     
        break;
     default:
     break;
     }
}
- (void) setCurrentViewSequenceFrame
{
    ViewSeqProperty vsp = getViewSeqProperty(mCurrentViewSeqType);
    for(int i = 0 ; i < vsp.count; i++)
    {
        VIEW_TYPE vp = vsp.viewseq[i];
        SEContentViewContainer* vc = mViewArray[vp];
        vc.frame = vc.mHintRect;
        vc.hidden = NO;
    }
    for(int i = 0 ; i < vsp.count - 1 ; i++)
    {
        VIEW_TYPE vp  = vsp.viewseq[i];
        VIEW_TYPE vpNext = vsp.viewseq[i + 1];
        SEBarView* barView = [self getBarView:vp  :vpNext];
        barView.frame = barView.mHintRect;
        barView.hidden = NO;
    }
}
- (SEBarView*) getBarView: (VIEW_TYPE)leftView :(VIEW_TYPE)rightView
{
    
    BarViewType bvt = getBarViewType(leftView, rightView);
    if(isBarViewTypeValid(bvt))
    {
        for(SEBarView* v in mBarViewArray)
        {
            if(isBarViewTypeEqual(bvt, v.mBarViewType))
                return v;
        }
        return nil;
    }
    else
    {
        return nil;
    }
}
- (CGRect) calculateContentContainerFrame: (VIEW_TYPE)vp
{
    VIEW_TYPE prev = getPrevView(vp);
    VIEW_TYPE next = getNextView(vp);
    float contentWidth = mViewPortWidth;
    if(isViewLayoutFullScreen(vp) == NO)
    {
        if(prev != INVALID_VIEW)
            contentWidth -= mBarWidth;
        if(next != INVALID_VIEW)
            contentWidth -= mBarWidth;
    }
    CGRect frame = CGRectMake(0, 0, contentWidth, mViewPortHeight);
    return frame;
}
- (void) initContentContainer
{
    for(int i = 0 ; i < VIEW_NUM ; i++)
    {
        if(i != INVALID_VIEW)
        {
            VIEW_TYPE vp = (VIEW_TYPE)i;
            CGRect frame = [self calculateContentContainerFrame:vp];
            SEContentViewContainer* viewContainer = [[SEContentViewContainer alloc] initWithFrame:frame];
            DBG_VIEW_BG(viewContainer, redColor);
            viewContainer.mHintRect = frame;
            viewContainer.mViewNav = self;
            viewContainer.mType = vp;
            mViewArray[i] = viewContainer;
        }
    }
}
- (void) initBarView
{
    mBarViewArray = [NSMutableArray array];
    [mBarViewArray retain];
    int count = getBarViewTypeCount();
    for(int i = 0 ; i < count ; i++)
    {
        CGRect frame = CGRectMake(0, 0, mBarWidth, mViewPortHeight);
        SEBarView* barView = [[SEBarView alloc] initWithFrame:frame];
        barView.mIsBarAnimationEnd = YES;
        barView.multipleTouchEnabled = NO;
        barView.mResLoader = mResLoader;
        barView.mHintRect = frame;
        barView.mContainerAnimHandler = self;
        BarViewType bvt = gBarViewType[i];
        barView.mBarViewType = bvt;
        barView.mCanStopInMid = bvt.canStopInMid;
        barView.mLeftContentContainer = mViewArray[bvt.leftView];
        barView.mRightContentContainer = mViewArray[bvt.rightView];
        barView.mViewNav = self;
        [barView initBackground];
        [mBarViewArray addObject:barView];
    }
}
- (void)removeAllViewFromContentContainerParent
{
    for(UIView* v in mContentContainerParent.subviews)
    {
        [v removeFromSuperview];
    }
}
/*
 *before invoke this function. we must first adjust viewContainer's frame to appropiate size
 */
- (void) initContentContainerParent
{
    [self removeAllViewFromContentContainerParent];
    ViewSeqProperty currViewSeqProp = getViewSeqProperty(mCurrentViewSeqType);
    
    NSLog(@"#### viewsequence property count = %d ####", currViewSeqProp.count);
    NSLog(@"##### seq type = %d #####", currViewSeqProp.type);
    if(!isValidViewSeqProp(currViewSeqProp))
        return;
    VIEW_TYPE firstViewType = currViewSeqProp.viewseq[0];
    SEBarView* firstBarView = nil;
    if(firstViewType != MAIN_DISPLAY)
    {
        VIEW_TYPE vp = MAIN_DISPLAY;
        VIEW_TYPE vpNext = firstViewType;
        firstBarView = [self getBarView:vp :vpNext];
    }
    int count = currViewSeqProp.count;
    int totalWidth = 0;
    if(firstBarView != nil)
        totalWidth += mBarWidth;
    for(int i = 0 ; i < count - 1; i++)
    {
        VIEW_TYPE vp = currViewSeqProp.viewseq[i];
        SEContentViewContainer* containerView = mViewArray[vp];
        totalWidth += containerView.mHintRect.size.width;
        totalWidth += mBarWidth;
    }
    SEContentViewContainer* cv = mViewArray[currViewSeqProp.viewseq[count - 1]];
    totalWidth += cv.mHintRect.size.width;
    mContentContainerParent.frame = CGRectMake(0, 0, totalWidth, mViewPortHeight);
    float startx = 0;
    if(firstBarView != nil)
    {
        startx = mBarWidth;
        firstBarView.frame = CGRectMake(0, 0, mBarWidth, mViewPortHeight);
        [mContentContainerParent addSubview:firstBarView];
    }
    for(int i = 0 ; i < count - 1 ; i++)
    {
        VIEW_TYPE vp = currViewSeqProp.viewseq[i];
        VIEW_TYPE vpNext = currViewSeqProp.viewseq[i + 1];
        SEContentViewContainer* viewContainer = mViewArray[vp];
        SEBarView* barView = [self getBarView:vp : vpNext];
        viewContainer.frame = CGRectMake(startx, 0, viewContainer.frame.size.width, viewContainer.frame.size.height);
        startx += viewContainer.frame.size.width;
        barView.frame = CGRectMake(startx, 0, barView.frame.size.width, barView.frame.size.height);
        startx += barView.frame.size.width;
        [mContentContainerParent addSubview:viewContainer];
        [mContentContainerParent addSubview:barView];
    }
    VIEW_TYPE vp = currViewSeqProp.viewseq[count - 1];
    //if(count == 1 || (count > 1 && vp != currViewSeqProp.viewseq[0]))
    {
        SEContentViewContainer* viewContainer = mViewArray[vp];
        viewContainer.frame = CGRectMake(startx, 0, viewContainer.frame.size.width, viewContainer.frame.size.height);
        [mContentContainerParent addSubview:viewContainer];
    }
}

- (UIImage*) getDefaultImage
{
    return [UIImage imageNamed:@"image_background_001.png"];
}
- (UIView<SEAdjustContentView>*) createSelectedMusicView: (CGRect) rect
{
    SESelectedMusicView* view = (SESelectedMusicView*)[[[NSBundle mainBundle] loadNibNamed:@"MusicSelectedView" owner:self options:nil] lastObject];
    view.frame = rect;
    view.mViewNav = self;
    [view initData];
    return view;
}
- (UIView<SEAdjustContentView>*) createMusicPickerView: (CGRect) rect
{
    SEMusicPickerView* view  = (SEMusicPickerView*)[[[NSBundle mainBundle] loadNibNamed:@"MusicPicker" owner:self options:nil] lastObject];
    view.mViewNav = self;
    view.frame = rect;
    [view initData];
    return view;
}
- (UIView<SEAdjustContentView>*) createMusicImageListAttachView: (CGRect)r
{
    UIImage* image = [mResLoader getImage:@"MusicImageAttachBackground"];
    SEMusicImageListView* miv = (SEMusicImageListView*)[[[NSBundle mainBundle] loadNibNamed:@"MusicImageListView" owner:self options:nil] lastObject];
    miv.mBackground = image;
    miv.mResLoader = mResLoader;
    miv.mViewNav = self;
    [miv initData];
    
    return miv;
}
- (UIView<SEAdjustContentView>*) createSignaturePreview: (CGRect) rect
{
    SESignaturePreview* view = (SESignaturePreview*)[[[NSBundle mainBundle] loadNibNamed:@"SignaturePreview" owner:self options:nil] lastObject];
    view.mViewNav = self;
    [view initData];
    return view;
}
- (UIView<SEAdjustContentView>*) createSignatureView:(CGRect) rect
{
    SESignatureView* view  = [[SESignatureView alloc] initWithFrame:rect];
    [[[NSBundle mainBundle] loadNibNamed:@"NewSignatureView" owner:view options:nil] lastObject];
    view.backgroundColor = [UIColor clearColor];
    view.mViewNav = self;
    view.mResLoader = mResLoader;
    [view initData];
    //[v release];
    return view;
}
- (UIView<SEAdjustContentView>*) createOptionsView: (CGRect) rect
{
    SEOptionsView* view = [[SEOptionsView alloc] initWithFrame:rect byViewNav:self];
    [view initData];
    [view setCurrentBarView:PLAY_SETTING];
    [view autorelease];
    return view;
}
- (UIView<SEAdjustContentView>*) createMainDisplayView : (CGRect) rect
{
    SEMainDisplay* mainDisplay = [[SEMainDisplay alloc] initWithResLoader:mResLoader withFrame:rect];
    mainDisplay.mViewNav = self;
    [mainDisplay loadView];
    [mainDisplay setToolBarButtonHandleTarget:self withAction:@selector(handleToolBarButtonClick:)];
    [mainDisplay autorelease];
    return mainDisplay;
}
- (UIImage*) createPickerViewBackground: (CGRect) rect
{
    UIImage* image = [mResLoader getImage:@"ImagePickerViewBackground"];
    CGFloat w = image.size.width;
    CGFloat h = image.size.height;
    UIImage* ret = nil;
    UIGraphicsBeginImageContext(rect.size);
    [image drawAtPoint:CGPointMake(-rect.origin.x, -rect.origin.y)];
    ret = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    return ret;
}
- (UIImage*) createSelectViewBackground : (CGRect) rect
{
    UIImage* image = [mResLoader getImage:@"ImageSelectedViewBackground"];;
    UIImage* ret = nil;
    UIGraphicsBeginImageContext(rect.size);
    [image drawAtPoint:CGPointMake(-rect.origin.x, -rect.origin.y)];
    ret = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    return ret;
}

- (void)createFloatView: (SEPageHitProperty)hp scrollView:(SEPageUIScrollView*)currView
{
    CGRect r = [hp.imageView convertRect:hp.rect toView:mRootView];
    r = CGRectMake(r.origin.x, r.origin.y - 40, 100, 140);
    SEPageUIImageView* imageView = hp.imageView;
    mFloatView = [[SEUIFloatView alloc] initWithFrame:r];
    //mFloatView.backgroundColor = [UIColor greenColor];
    mFloatView.backgroundColor = [UIColor clearColor];
    mFloatView.contentMode = UIViewContentModeCenter;
    mFloatView.clipsToBounds = YES;
    mFloatView.origC = mFloatView.center;
    [mRootView addSubview:mFloatView];
    [mFloatView release];
    [mRootView bringSubviewToFront:mFloatView];
    mFloatView.image = imageView.image;
    mFloatView.backgroundImage = [mResLoader getImage:@"ImageFloatViewBg"];
    imageView.highlighted = YES;
    NSArray* highlightedView = [currView getHighlightedImageView];
    NSLog(@"selected count = %d", highlightedView.count);
    NSLog(@"select index = %d", hp.index);
    self.mSelectedPhotoURL = [NSMutableArray array];
    //[mSelectedPhotoURL retain];
    self.mSelectedImage = [NSMutableArray array];
    //[mSelectedImage retain];
    self.mSelectedImageViewIndex = [NSMutableArray array];
    /*
    if([highlightedView containsObject:imageView] == NO)
    {
        SEPageImageURL* url = [currView getImageURL:hp.index];
        NSLog(@"selected url = %@", url);
        if(url.url)
        {
            SEPageURLID* urlID = [SEPageURLID create:url.url : url.urlDate : url.origWidth : url.origHeight : url.orientation];
            [mSelectedPhotoURL addObject:urlID];
        }
        [mSelectedImageViewIndex addObject:[NSNumber numberWithInt:hp.index]];
        [mSelectedImage addObject:imageView.image];
    }
     */
    for (SEPageUIImageView* v in highlightedView) 
    {
        int index = [currView getIndexForImageView:v];
        if(index != -1)
        {
            SEPageImageURL* url = [currView getImageURL:index];
            NSLog(@"selected url = %@", url);
            if(url.url)
            {
                SEPageURLID* urlID = [SEPageURLID create:url.url :url.urlDate : url.origWidth :url.origHeight : url.orientation];
                [mSelectedPhotoURL addObject:urlID];
            }
        }
        [mSelectedImageViewIndex addObject:[NSNumber numberWithInt:index]];
        [mSelectedImage addObject:v.image];
    }
    [mFloatView setCount:mSelectedImageViewIndex.count];
    [currView disableAllGestures];
    mPlacedViewIndex = SE_INVALID_IMAGE_INDEX;
}
/*
- (void) longPressGestureHandler: (UILongPressGestureRecognizer*)ges
{
    NSLog(@"long press\n");
    UIView* v = ges.view;
    if(v == mViewArray[IMAGE_PICKER])
    {
        CGPoint p = [ges locationInView:v];
        NSLog(@"p = %f, %f", p.x, p.y);
        SEPageUIScrollView* imagePickerView = (SEPageUIScrollView*)v;
        SEPageHitProperty hp = [imagePickerView hitRect:p];
        if(hp.imageView)
        {
            [self createFloatView:hp scrollView:imagePickerView];
        }
    }
}
 */
- (UIView<SEAdjustContentView>*) createImagePickerView : (CGRect) rect
{
    rect = CGRectMake(0, 63, rect.size.width, rect.size.height - 63);
    SEResLoader* resLoader = mResLoader;
    SEPageUIScrollView* scrollView = [[SEPageUIScrollView alloc] init];
    /*
    UILongPressGestureRecognizer* longGes = [[UILongPressGestureRecognizer alloc] initWithTarget:self action:@selector(longPressGestureHandler:)];
    [scrollView addGestureRecognizer:longGes];
    [longGes release];
     */
    scrollView.mScrollViewType = PHOTOLIB_SCROLLVIEW;
    scrollView.multipleTouchEnabled = NO;
    //scrollView.mGetImageURLInMainThread = YES;
    scrollView.mResLoader = resLoader;
    scrollView.mName = @"image_picker";
    scrollView.mFrameImage = [mResLoader getImage:@"ImagePickerFrameImage"];
    scrollView.mHighlightedFrameImage = [mResLoader getImage:@"PageScrollViewImageViewHighlighted"];
    scrollView.mHandleMultiTouchDelegate = self;
    scrollView.pagingEnabled = YES;
    UIImage* image = [mResLoader getImage:@"ImagePickerPageViewBackground"];
    //image = [SEUtil imageWithCap: image top:0.1 bottom:0.9 left:0.1 right:0.9];
    //CGContextRef context = UIGraphicsBeginImageContext(CGSizeMake(436, 705));
    
    scrollView.mPageBackground = image;
    scrollView.mBackgroundImage = [self createPickerViewBackground:rect];
    scrollView.backgroundColor = [UIColor whiteColor];
    scrollView.frame = rect;
    scrollView.mPageCol = 4;
    scrollView.mPageRow = 6;
    scrollView.mLeftPadding = 7;
    scrollView.mTopPadding = 16;
    scrollView.mDefaultImage = [self getDefaultImage];
    scrollView.mPhotoWidth = [resLoader getInt: @"PhotoWidth"];
    scrollView.mPhotoHeight = [resLoader getInt:@"PhotoHeight"];
    scrollView.mVMargin = [resLoader getInt:@"VMargin"];
    scrollView.mHMargin = [resLoader getInt:@"HMargin"];
    scrollView.mViewNavigator = self;
    scrollView.mLongPressHandler = self;
    scrollView.mCanTouchResponse = YES;
    
    [scrollView initState];
    [scrollView initPhotoLibUrl];
    [scrollView autorelease];
    scrollView.backgroundColor = [UIColor blackColor];
    return scrollView;
}
- (UIView<SEAdjustContentView>*) createImageSelectedView : (CGRect) rect
{
    rect = CGRectMake(0, 63, rect.size.width, rect.size.height - 63);
    SEResLoader* resLoader = mResLoader;
    SEPageUIScrollView* scrollView= [[SEPageUIScrollView alloc] init];
    scrollView.mScrollViewType = COREDATA_SCROLLVIEW;
    scrollView.pagingEnabled = YES;
    scrollView.multipleTouchEnabled = NO;
    scrollView.mResLoader = resLoader;
    scrollView.mName = @"image_selected_view";
    scrollView.mHandleMultiTouchDelegate = self;
    scrollView.mFrameImage = [mResLoader getImage:@"ImagePickerFrameImage"];
    scrollView.mHighlightedFrameImage = [mResLoader getImage:@"PageScrollViewImageViewHighlighted"];
    scrollView.mBackgroundImage = [self createSelectViewBackground:rect];
    scrollView.mPageBackground = [mResLoader getImage:@"SelectedImageViewPageViewBackground"];
    scrollView.frame = rect;
    //scrollView.backgroundColor = [UIColor redColor];
    scrollView.mPageCol = 4;
    scrollView.mPageRow = 6;
    
    scrollView.mLeftPadding = 7;
    scrollView.mTopPadding = 16;
    scrollView.mPhotoWidth = [resLoader getInt:@"PhotoWidth"];
    scrollView.mPhotoHeight = [resLoader getInt:@"PhotoHeight"];
    scrollView.mVMargin = [resLoader getInt:@"VMargin"];
    scrollView.mHMargin = [resLoader getInt:@"HMargin"];
    scrollView.mViewNavigator = self;
    scrollView.mDefaultImage = [self getDefaultImage];
    scrollView.mImageListName = mCurrentLoadedImageListName;
    scrollView.mLongPressHandler = self;
    [scrollView initState];
    [scrollView initPhotoLibUrl];
    [scrollView createContent];
    [scrollView setNeedsDisplay];
    [scrollView autorelease];
    scrollView.backgroundColor = [UIColor blackColor];
    return scrollView;
}
- (UIView<SEAdjustContentView>*) createPreview3D: (CGRect)rect
{
    SE3DPreview* preview = [[SE3DPreview alloc] initWithFrame:rect withViewNav:self];
    [preview initData];
    [preview autorelease];
    return preview;
}
- (UIView*) loadViewFromNib: (NSString*)name
{
    NSBundle* mainBundle = [NSBundle mainBundle];
    UIView* view = [[mainBundle loadNibNamed:name owner:self options:NULL] lastObject];
    return view;
    
}

- (void) setRootViewTransform
{
   
    if(UIInterfaceOrientationIsPortrait(self.interfaceOrientation))
    {
        /*
        CGAffineTransform t1 = CGAffineTransformMakeTranslation(mViewPortHeight, 0);
        CGAffineTransform t2 = CGAffineTransformTranslate(t1, -mViewPortHeight / 2, - mViewPortWidth / 2);
        CGAffineTransform rotate = CGAffineTransformRotate(t2, 90 * 3.1415926 / 180.0);
        CGAffineTransform t3 = CGAffineTransformTranslate(rotate, mViewPortHeight / 2, mViewPortWidth / 2);
        mRootView.transform = t3;
         */
        
        //mRootView.frame = CGRectMake(0, 0, mViewPortWidth, mViewPortHeight);
        CGAffineTransform t1 = CGAffineTransformMakeTranslation(mViewPortHeight, 0);
        CGAffineTransform t2 = CGAffineTransformTranslate(t1, -mViewPortWidth / 2, - mViewPortHeight / 2);
        CGAffineTransform rotate = CGAffineTransformRotate(t2, 90 * 3.1415926 / 180.0);
        CGAffineTransform t3 = CGAffineTransformTranslate(rotate, mViewPortWidth / 2, mViewPortHeight/ 2);
        mRootView.transform = t3;
         
        
    }
    else
    {
        mRootView.transform = CGAffineTransformIdentity;
        mRootView.frame = CGRectMake(0, 0, mViewPortWidth, mViewPortHeight);
    }
}
- (void) setupRootView
{
    UIView* ppView = nil;
    if(mRootView == nil)
    {
        UIInterfaceOrientation o = self.interfaceOrientation;
        NSLog(@"loadview o = %d ", o);
        mRootView = [[SEUIRootView alloc] initWithFrame:CGRectMake(0, 0, mViewPortWidth, mViewPortHeight)];
        //mRootView = [[SEUIRootView alloc] init];
        [self setRootViewTransform];
        ppView = [[SEUIRootView alloc] initWithFrame:CGRectMake(0, 0, mViewPortWidth, mViewPortHeight)];
        UIImageView* imageView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, ppView.frame.size.width, ppView.frame.size.height)];
        imageView.image = [SESystemConfig getDefaultSelectedImage];
        [ppView addSubview:imageView];
        [imageView release];
        imageView.autoresizingMask = UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight | UIViewAutoresizingFlexibleBottomMargin | UIViewAutoresizingFlexibleLeftMargin | UIViewAutoresizingFlexibleRightMargin | UIViewAutoresizingFlexibleTopMargin;
        imageView.contentMode = UIViewContentModeScaleAspectFill;
        [ppView addSubview:mRootView];
        ppView.backgroundColor = [UIColor yellowColor];
    }
    DBG_VIEW_BG(mRootView, greenColor);
    [self removeAllViewFromRoot];
    mContentContainerParent = [[UIView alloc] init];
    [mRootView addSubview:mContentContainerParent];
    [mContentContainerParent release];
    DBG_VIEW_BG(mContentContainerParent, blueColor);
    self.view = ppView;//mRootView;
    [ppView release];
    

}
- (void) setCurrentContentContainerFrame
{
    ViewSeqProperty currViewSeqProp = getViewSeqProperty(mCurrentViewSeqType);
    for(int i = 0 ; i < currViewSeqProp.count ; i++)
    {
        VIEW_TYPE vp = currViewSeqProp.viewseq[i];
        mViewArray[vp].mHintRect = [self calculateContentContainerFrame:vp];
        mViewArray[vp].frame = [self calculateContentContainerFrame:vp];
    }
}

/*
- (void) popupMusicOperationView
{
    mMusicOperationViewGroup = [[SEOperationViewGroup alloc] init];
    mMusicOperationViewGroup.mOperationHandler = mOperationHandler;
    CGRect frame = [mMusicOperationViewGroup calculateFrame];
    mMusicOperationViewGroup.frame = CGRectMake(mViewPortWidth - frame.size.width, 0, frame.size.width, frame.size.height);
    [mMusicOperationViewGroup initData];
    [mRootView addSubview:mMusicOperationViewGroup];
    [mMusicOperationViewGroup release];
    mMusicOperationViewGroup.backgroundColor = [UIColor clearColor];//[UIColor redColor];
}
*/
@end
//////////////////////////////////
@implementation SEViewNavigator
@synthesize mCurrentOrientation;
@synthesize  mNewConfig;
@synthesize mResLoader;
@synthesize managedObjectContext;
@synthesize mUserInfoProperty;
@synthesize mViewPortWidth;
@synthesize mViewPortHeight;
@synthesize mCurrView;
@synthesize mBarWidth;
@synthesize mRootView;
@synthesize mContentContainerParent;
@synthesize mFontLoader;
@synthesize mOperationContainer;
@synthesize mSelectedImageViewIndex;
@synthesize mMusicFloatView;
@synthesize persistentStoreCoordinator;
@synthesize mCurrentLoadedImageListName;
@synthesize mCurrentLoadedMusicListName;
@synthesize mCurrentFloatViewType;
@synthesize mCurrentSystemBright;
@synthesize mSelectedImage;
@synthesize mSelectedPhotoURL;
@synthesize mSignatureAutoColor;
@synthesize mWebServiceManager;
@synthesize mCurrentDownloadingFileName;
@synthesize mSystemDataManager;
@synthesize mMusicPlayListChange;
@synthesize mStartLaunch;
- (float) getBarWidth
{
    //return 64;
    UIImage* leftImage = [mResLoader getImage:@"LeftBarBg"];
    return leftImage.size.width;
}
- (id) initWithResLoader: (SEResLoader*) resLoader
{
    self = [super init];
    if(self)
    {
        mCurrView = INVALID_VIEW;
        mPrevCurrView = INVALID_VIEW;
        mCurrentViewSeqType = MAIN_DISPLAY_ONLY;
        mResLoader = resLoader;
        mFontLoader = [[SEFontLoader alloc] init];
        mBarWidth = [self getBarWidth];
        mUserUpgradeInfo = [[SEUserUpgrade alloc] init];
        mUserUpgradeInfo.mViewNav = self;
        mViewSeqTypeStack = [NSMutableArray array];
        [mViewSeqTypeStack retain];
        mSystemDataManager = [[SESystemDataManager alloc] init];
        //[mUserUpgradeInfo calculatePresentOnDuty];
        
        //SEWeiboImageShare* share = [[SEWeiboImageShare alloc] init];
        //SETwitterImageShare* share = [[SETwitterImageShare alloc] initWithViewNav:self];
        //share.mViewNav = self;
        //mShareImage = share;
        mDataUploadManager = [[SEDataUploadManager alloc] init];
        //mWebServiceManager = [[SEWebServiceManager alloc] init];
        [self startUpgradeTimer];
        mDrawingStateManager = [[SEDrawingStateManager alloc] init];
        mStartLaunch = YES;
        /*
        MPMusicPlayerController* player = [MPMusicPlayerController applicationMusicPlayer];
        [player beginGeneratingPlaybackNotifications];
        [[NSNotificationCenter defaultCenter] removeObserver:self name:MPMusicPlayerControllerNowPlayingItemDidChangeNotification object:nil];
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(musicItemChanged:) name:MPMusicPlayerControllerNowPlayingItemDidChangeNotification object:nil];
         */
    }
    return self;
}
- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)didReceiveMemoryWarning
{
    // Releases the view if it doesn't have a superview.
    [super didReceiveMemoryWarning];
    
    // Release any cached data, images, etc that aren't in use.
}

#pragma mark - View lifecycle

-(void) initPainterManager
{
    mPainterManager.bgWidth = 1024;
    mPainterManager.bgHeight =768;
}
- (void) addTestView
{
    SEDrawCircleView* v = [[SEDrawCircleView alloc] initWithFrame:CGRectMake(0, 0, 1024, 768)];
    v.backgroundColor = [UIColor whiteColor];
    [mRootView addSubview:v];
    [v release];
}
// Implement loadView to create a view hierarchy programmatically, without using a nib.
- (void)loadView
{
    mPainterManager = [PainterManager painterManager];
    mPainterManager.mViewNav = self;
    [self initPainterManager];
    [self setupRootView];
    [self initContentContainer];
    [self initBarView];
    [self setCurrentViewSequenceFrame];
    [self initContentContainerParent];
    [self setCurrentView:MAIN_DISPLAY isAnimation:NO];
    //for test
    //[self showFirstNumView];
    //[self showSecondNumView];
    //end
    //[self addTestView];
    //[self showLogView];
    //[self getDataFromPhotoLib];
}
- (void) viewDidLayoutSubviews
{
    [super viewDidLayoutSubviews];
    //mViewLoaded = YES;
}
- (void)viewWillAppear:(BOOL)animated
{
    [super viewWillAppear:animated];
}
/*
- (void) setStatusbarOrient
{
    //[[UIApplication sharedApplication] setStatusBarOrientation:UIInterfaceOrientationLandscapeLeft];
}
 */
/*
- (void) keyboardShow: (NSNotification*)n
{
    NSLog(@"keyboardShow");
    for(int j = 0 ; j < [[[UIApplication sharedApplication] windows] count]; j++)
    {
        UIWindow* tmpWindow = [[[UIApplication sharedApplication] windows] objectAtIndex:j];
        NSLog(@"window = %@", tmpWindow);
        UIView* keyboard;
        NSLog(@"window root view = %@", [tmpWindow.rootViewController.view description]);
        for(int i = 0 ; i < [tmpWindow.rootViewController.view.subviews count] ; i++)
        {
            keyboard = [tmpWindow.subviews objectAtIndex:i];
            NSLog(@"view description = %@", [keyboard description]);
            if([[keyboard description] hasPrefix:@"<UIPeripheral"] == YES)
            {
                NSLog(@"key board frame = %@", NSStringFromCGRect(keyboard.frame));
            }
        }
    }
    [self performSelectorOnMainThread:@selector(setStatusbarOrient) withObject:nil waitUntilDone:NO];
}
 */
- (void) viewDidAppear:(BOOL)animated
{
    [super viewDidAppear:animated];
    NSLog(@"viewDidAppear");
    mViewLoaded = YES;
    //[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardShow:) name:UIKeyboardWillShowNotification object:nil];
}
/*
// Implement viewDidLoad to do additional setup after loading the view, typically from a nib.
- (void)viewDidLoad
{
    [super viewDidLoad];
}
*/

- (void)viewDidUnload
{
    [super viewDidUnload];
    // Release any retained subviews of the main view.
    // e.g. self.myOutlet = nil;
}
- (void) setShouldRotate: (BOOL) b
{
    //mIsShouldRotate = b;
    UserInfo* userInfo = [self getUserInfo];
    userInfo.rotatescreen = [NSNumber numberWithBool:b];
}
- (BOOL) isShouldRotate
{
    //return mIsShouldRotate;
    UserInfo* userInfo = [self getUserInfo];
    return [userInfo.rotatescreen boolValue];
}
//static BOOL startRRR = YES;
- (BOOL)shouldAutorotate
{
    /*
    if(mStartLaunch)
    {
        mStartLaunch = NO;
        return YES;
    }
    else
        return [self isShouldRotate];
     */
    UserInfo* userInfo = [self getUserInfo];
    BOOL shouldRotate = [userInfo.rotatescreen boolValue];
    
    if(mViewLoaded == NO)
        return YES;
    if(mCurrView == SIGNATURE_VIEW || mCurrView == MUSIC_IMAGE_LIST_ATTACH || mCurrView == OPTIONS || mCurrView == OPTIONS_MIRROR)
        return YES;
    
    if(mCurrView == MAIN_DISPLAY || mCurrView == INVALID_VIEW)
    {
        if(shouldRotate)
        {
            return YES;
        }
        else
        {
            return NO;
        }
    }
    else
    {
        return NO;
    }

}
- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{

    NSLog(@"should rotate to %d ", interfaceOrientation);
    UserInfo* userInfo = [self getUserInfo];
    BOOL shouldRotate = [userInfo.rotatescreen boolValue];

    if(mViewLoaded == NO)
        return YES;
    if(mCurrView == SIGNATURE_VIEW || mCurrView == MUSIC_IMAGE_LIST_ATTACH || mCurrView == OPTIONS || mCurrView == OPTIONS_MIRROR)
        return YES;
        
    if(mCurrView == MAIN_DISPLAY || mCurrView == INVALID_VIEW)
    {
        if(shouldRotate)
        {
            return YES;
        }
        else 
        {
            if(self.interfaceOrientation == interfaceOrientation)
            {
                return YES;
            }
            else
            {
                return NO;
            }
        }
    }
    else 
    {
        return NO;
    }
    //return (interfaceOrientation == UIInterfaceOrientationLandscapeLeft) || (interfaceOrientation == UIInterfaceOrientationLandscapeRight);

}
- (void) redisplayAll: (UIView*)v
{
    if(v.subviews.count == 0)
    {
        [v setNeedsDisplay];
    }
    else
    {
        [v setNeedsDisplay];
        for(UIView* child in v.subviews)
        {
            [self redisplayAll: child];
        }
    }
}
- (void) willRotateToInterfaceOrientation:(UIInterfaceOrientation)toInterfaceOrientation duration:(NSTimeInterval)duration
{
    [super willRotateToInterfaceOrientation:toInterfaceOrientation duration:duration];
    mRootView.hidden = YES;
}
- (void)willAnimateRotationToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation duration:(NSTimeInterval)duration
{
    NSLog(@"animation rotate");
    [super willAnimateRotationToInterfaceOrientation:interfaceOrientation duration:duration];
}
- (void)realRotate
{
    //NSLog(@"real rotate rotated root view layout: %f, %f, %f, %f", mRootView.frame.origin.x, mRootView.frame.origin.y, mRootView.frame.size.width, mRootView.frame.size.height);
    [self setRootViewTransform];
    if(mCurrView == MAIN_DISPLAY)
    {
        SEContentViewContainer* c = mViewArray[MAIN_DISPLAY];
        [c updateContent];
    }
    [self redisplayAll: mRootView];
    NSArray* upgradeInfoArray = [self getUpgradeInfoArray];
    if(upgradeInfoArray.count > 0)
    {
        [self notificationShow];
    }
}
- (void) didRotateFromInterfaceOrientation:(UIInterfaceOrientation)fromInterfaceOrientation
{
    //NSLog(@"## did rotate from interface orientation: %d  to %d ##", fromInterfaceOrientation, self.interfaceOrientation);
    //NSLog(@" did rotate : root view layout: %f, %f, %f, %f", mRootView.frame.origin.x, mRootView.frame.origin.y, mRootView.frame.size.width, mRootView.frame.size.height);
    mRootView.hidden = NO;
    //[self performSelectorOnMainThread:@selector(realRotate) withObject:nil waitUntilDone:NO];
    [self realRotate];
}
- (enum VIEW_TYPE) getPrevView
{
    return getPrevView(mCurrView);
}
- (enum VIEW_TYPE) getNextView
{
    return getNextView(mCurrView);
}

- (void) dealloc
{
    [mFontLoader release];
    for(int i = 0 ; i < VIEW_NUM ; i++)
    {
        [mViewArray[i] release];
    }
    for(int i = 0 ; i < [mBarViewArray count]; i++)
    {
        [[mBarViewArray objectAtIndex:i] release];
    }
    [mUserUpgradeInfo release];
    [mRootView release];
    [managedObjectContext release];
    [mUserInfoProperty release];
    [mShareImage release];
    [mDataUploadManager release];
    [mWebServiceManager release];
    [persistentStoreCoordinator release];
    [mNotificationView release];
    [mUserUpdateView release];
    [mLogView release];
    [mSignatureAutoColor release];
    [mCurrentDownloadingFileName release];
    [mDrawingStateManager release];
    [mSystemDataManager release];
    [super dealloc];
}

- (SEContentViewContainer*) getContainer : (VIEW_TYPE) vp;
{
    if(vp == INVALID_VIEW)
        return nil;
    return mViewArray[vp];
}
- (void) addContentToContentContainer: (VIEW_TYPE) vp
{
    if(vp == INVALID_VIEW)
        return;
    SEContentViewContainer* viewContainer = [self getContainer:vp];
    if([viewContainer hasContent])
    {
        [viewContainer updateContent];
        return;
    }
    else if(getMirrorType(vp) != INVALID_VIEW)
    {
        VIEW_TYPE mirrorType = getMirrorType(vp);
        SEContentViewContainer* mirrorViewContainer = [self getContainer:mirrorType];
        if([mirrorViewContainer hasContent])
        {
            UIView* contentView = [mirrorViewContainer contentView];
            [contentView retain];
            [contentView removeFromSuperview];
            [viewContainer addSubview:contentView];
            [contentView release];
            [viewContainer updateContent];
            return;
        }
    }
    CGRect frame = viewContainer.bounds;
    UIView* view = [self createContentView:vp withFrame:frame];
    [viewContainer addSubview:view];
    [viewContainer initContainer];
}
- (BOOL) isSelectedImageViewMid
{
    SEContentViewContainer* container = mViewArray[SELECTED_IMAGE_VIEW];
    UIView* v = container.contentView;
    float w = (self.mViewPortWidth - self.mBarWidth) / 2;
    if(v.frame.size.width == w)
        return true;
    else
        return false;
}
- (SEBarView*) getRightBarView: (VIEW_TYPE)currType
{
    for(int i = 0 ; i < mBarViewArray.count  ; i++)
    {
        SEBarView* v = [mBarViewArray objectAtIndex:i];
        if(v.mBarViewType.leftView == currType)
            return v;
    }
    return nil;
}
- (void)updateBarView
{
    for(UIView* v in mContentContainerParent.subviews)
    {
        if([v isMemberOfClass:[SEBarView class]])
        {
            [v setNeedsDisplay];
        }
    }
}
- (void) changeCurrentViewRelationType: (VIEW_TYPE) vp
{
    if(gViewRelationType == TYPE2 && vp == MUSIC_IMAGE_LIST_ATTACH)
        gViewRelationType = TYPE1;
}
- (CGFloat) getDeltax: (UIView*)currentView
{
    CGFloat total = 0;
    for(UIView* v in mContentContainerParent.subviews)
    {
        if(v != currentView)
            total += v.frame.size.width;
        else
            break;
    }
    return total;
}
- (void) restoreFromPreview3D
{
    mPrevCurrView = PREVIEW_3D;
    mCurrView = MAIN_DISPLAY;
    [mDrawingStateManager moveFromOtherViewBack: PREVIEW_3D];
}
- (void) afterSetCurrentView
{
    if(mPrevCurrView == MUSIC_IMAGE_LIST_ATTACH)
    {
        SEContentViewContainer* c = mViewArray[mPrevCurrView];
        SEMusicImageListView* lv = (SEMusicImageListView*)[c contentView];
        [lv pauseMusic];
        MPMusicPlayerController* player = [MPMusicPlayerController applicationMusicPlayer];
        [player stop];
    }
    if(mPrevCurrView == IMAGE_PICKER || mPrevCurrView == SELECTED_IMAGE_VIEW)
    {
        SEContentViewContainer* c = mViewArray[mPrevCurrView];
        SEPageUIScrollView* scrollView = (SEPageUIScrollView*)[c contentView];
        [scrollView stopLoadImage];
    }
    if(mCurrView == MAIN_DISPLAY)   
    {
        for(int i = 0 ; i < VIEW_NUM; i++)
        {
            SEContentViewContainer* c = mViewArray[i];
            if(i != MAIN_DISPLAY)
            {
                [c removeContentView];
                //[c stop];
            }
        }
        [self saveCoreDataContext];
        mMoveFromOtherView = YES;
        MPMusicPlayerController* player = [MPMusicPlayerController applicationMusicPlayer];
        [player stop];
        [mDrawingStateManager moveFromOtherViewBack: mPrevCurrView];
    }
    if(mCurrView == MUSIC_IMAGE_LIST_ATTACH)
    {
        SEContentViewContainer* selectedImage = mViewArray[SELECTED_IMAGE_VIEW];
        SEContentViewContainer* selectedMusic = mViewArray[SELECTED_MUSIC_VIEW];
        [selectedImage removeContentView];
        [selectedMusic removeContentView];
    }
    if(mCurrView == IMAGE_PICKER || mCurrView == SELECTED_IMAGE_VIEW)
    {
        SEContentViewContainer* c = mViewArray[mCurrView];
        SEPageUIScrollView* scrollView = (SEPageUIScrollView*)[c contentView];
        [scrollView startLoadImage];
    }
    if(mMoveViewFromMainDisplay && ( mCurrView == IMAGE_PICKER || mCurrView == MUSIC_PICKER))
    {
        SEContentViewContainer* leftContentContainer = nil;
        SEContentViewContainer* rightContentContainer = nil;
        if(mCurrView == IMAGE_PICKER)
        {
            leftContentContainer = mViewArray[IMAGE_PICKER];
            rightContentContainer = mViewArray[SELECTED_IMAGE_VIEW];
        }
        else 
        {
            leftContentContainer = mViewArray[MUSIC_PICKER];
            rightContentContainer = mViewArray[SELECTED_MUSIC_VIEW];
        }
        float contentWidth = (mViewPortWidth - mBarWidth) / 2;
        if([leftContentContainer.contentView canAdjust])
        {
            [leftContentContainer adjustContentViewLayout:contentWidth part:LEFT_PART barStopInMid:YES];
        }
        if([rightContentContainer.contentView canAdjust])
        {
            [rightContentContainer adjustContentViewLayout:contentWidth part:RIGHT_PART barStopInMid:YES];
        }
        SEBarView* barView = [self getBarView:leftContentContainer.mType :rightContentContainer.mType];
        barView.mStayOnMid = YES;
        NSArray* titleStatusView = [self getContainerTitleStatusView:mCurrView];
        UIView* titleView = [titleStatusView objectAtIndex:0];
        UIView* statusView = [titleStatusView objectAtIndex:1];
        CGRect r1, r2;
        NSArray* titleStatusRectArray = nil;
        titleStatusRectArray = [self getContainerTitleStatusRect:mCurrView];
        if(titleView != nil)
        {
            r1 = [[titleStatusRectArray objectAtIndex:0] CGRectValue];
        }
        if(statusView)
        {
            r2 = [[titleStatusRectArray objectAtIndex:1] CGRectValue];
        }
        titleView.frame = r1;
        statusView.frame = r2;
        //[leftContentContainer expandContent: (mViewPortWidth - mBarWidth) / 2 part:LEFT_PART];
        //[rightContentContainer expandContent:(mViewPortWidth - mBarWidth) / 2 part:RIGHT_PART];
    }
    mMoveViewFromMainDisplay = NO;
}
- (void) setRightContainer: (VIEW_TYPE)viewType
{
    if(viewType == IMAGE_PICKER)
        [self addContentToContentContainer:SELECTED_IMAGE_VIEW];
    else if(viewType == MUSIC_PICKER)
        [self addContentToContentContainer:SELECTED_MUSIC_VIEW];
    SEContentViewContainer* viewContainer = [self getContainer:viewType];
    UIView* v = [viewContainer getStatusLabel];
    v.frame = CGRectMake(viewContainer.mStatusLabelMidX, v.frame.origin.y, v.frame.size.width, v.frame.size.height);
}
- (NSArray*) getContainerTitleStatusView: (VIEW_TYPE)vp
{
    SEContentViewContainer* leftViewContainer = nil;
    SEContentViewContainer* rightViewContainer = nil;
    if(vp == IMAGE_PICKER)
    {
        leftViewContainer = [self getContainer:IMAGE_PICKER];
        rightViewContainer = [self getContainer:SELECTED_IMAGE_VIEW];
    }
    else if(vp == MUSIC_PICKER)
    {
        leftViewContainer = [self getContainer:MUSIC_PICKER];
        rightViewContainer = [self getContainer:SELECTED_MUSIC_VIEW];
    }
    else 
    {
        return nil;
    }
    UIView* titleView = [leftViewContainer getTitleLabel];
    UIView* statusView = [rightViewContainer getStatusLabel];
    return [NSArray arrayWithObjects:titleView , statusView, nil];
}
- (NSArray*) getContainerTitleStatusRect: (VIEW_TYPE)vp
{
    SEContentViewContainer* leftViewContainer = nil;
    SEContentViewContainer* rightViewContainer = nil;
    if(vp == IMAGE_PICKER)
    {
        leftViewContainer = [self getContainer:IMAGE_PICKER];
        rightViewContainer = [self getContainer:SELECTED_IMAGE_VIEW];
    }
    else if(vp == MUSIC_PICKER)
    {
        leftViewContainer = [self getContainer:MUSIC_PICKER];
        rightViewContainer = [self getContainer:SELECTED_MUSIC_VIEW];
    }
    else {
        return nil;
    }
    UIView* titleView = [leftViewContainer getTitleLabel];
    UIView* statusView = [rightViewContainer getStatusLabel];
    
    UIView* titleParent = [titleView superview];
    UIView* statusParent = [statusView superview];
    CGPoint titleScreenPoint = [titleView convertPoint:CGPointMake(0, 0) toView:mRootView];
    CGPoint titleP = [mRootView convertPoint:CGPointMake(mBarWidth, titleScreenPoint.y) toView:titleParent];
    CGPoint statusP = [mRootView convertPoint:CGPointMake(mViewPortWidth - mBarWidth, titleScreenPoint.y) toView:statusParent];
    CGRect r1 = CGRectMake(titleP.x, titleP.y, titleView.frame.size.width, titleView.frame.size.height);
    CGRect r2 = CGRectMake(statusP.x, statusP.y, statusView.frame.size.width, statusView.frame.size.height);
    NSArray* array = [NSArray arrayWithObjects:[NSValue valueWithCGRect:r1], [NSValue valueWithCGRect:r2] , nil];
    //titleView.frame = r1;
    //statusView.frame = r2;
    return array;
}
/*
 when set current view, it will display current view and left bar or right bar. it will not create prev view
     or next view.
 pre view and next view will be created when user touch the left bar or right bar
 */
- (void)setCurrentView: (enum VIEW_TYPE) view_type isAnimation: (BOOL)bAnim
{
    if(mCurrView == view_type)
        return;
    //mPrevCurrView = mCurrView;
    [self changeCurrentViewRelationType:view_type];
    mCurrentLeftBarView = nil;
    mCurrentRightBarView = nil;
    mCurrView = view_type;
    mCurrentLeftBarView = [self getBarView:self.prevView :mCurrView];
    mCurrentRightBarView = [self getBarView:mCurrView :self.nextView];
    SEContentViewContainer* currentContainer = [self getContainer:mCurrView];
    currentContainer.hidden = NO;
    currentContainer.backgroundColor = [UIColor blackColor];//[UIColor redColor];
    ViewSeqProperty currViewSeqProp = getViewSeqProperty(mCurrentViewSeqType);
    int viewIndex = getViewIndexInSequence(currViewSeqProp, mCurrView);
    if(viewIndex != -1)
    {
        float deltax = [self getDeltax:currentContainer];
        if(deltax > 0 && isViewLayoutFullScreen(mCurrView) == NO)
            deltax -= mBarWidth;
        NSArray* titleStatusRectArray = nil;
        if(mMoveViewFromMainDisplay && (currentContainer.mType == IMAGE_PICKER || currentContainer.mType == MUSIC_PICKER))
        {
            deltax += mBarWidth + (mViewPortWidth - 3 * mBarWidth) / 2;
            [self setRightContainer: currentContainer.mType];
            //titleStatusRectArray = [self getContainerTitleStatusRect:mCurrView];
        }
        CGRect frame = mContentContainerParent.frame;
        mContentContainerParent.frame = CGRectMake(-deltax, 0, frame.size.width, frame.size.height);
        
        [self addContentToContentContainer: mCurrView];
        if(bAnim)
        {
            SEContentViewContainer* prevContainer = [self getContainer:self.prevView];
            prevContainer.hidden = NO;
            SEContentViewContainer* nextContainer = [self getContainer:self.nextView];
            nextContainer.hidden = NO;
            frame = mContentContainerParent.frame;
            mContentContainerParent.frame = CGRectMake(0, 0, frame.size.width, frame.size.height);
            /*
            NSArray* titleStatusView = [self getContainerTitleStatusView:mCurrView];
            UIView* titleView = [titleStatusView objectAtIndex:0];
            UIView* statusView = [titleStatusView objectAtIndex:1];
            CGRect r1, r2;
            if(titleView != nil)
            {
                r1 = [[titleStatusRectArray objectAtIndex:0] CGRectValue];
            }
            if(statusView)
            {
                r2 = [[titleStatusRectArray objectAtIndex:1] CGRectValue];
            }
             */
            void (^animBlock) (void) = ^{
                mContentContainerParent.frame = frame;
            };
            void (^animEnd) (BOOL) = ^(BOOL) 
            {
                [self afterSetCurrentView];
            };
            [UIView animateWithDuration:0.5 animations:animBlock completion:animEnd];
        }
        else
        {
            //SEContentViewContainer* prevContainer = [self getContainer:self.prevView];
            //prevContainer.hidden = YES;
            //SEContentViewContainer* nextContainer = [self getContainer:self.nextView];
            //nextContainer.hidden = YES;
            [self afterSetCurrentView];
        }
        [self updateBarView];
    }
    else
    {
        //SEContentViewContainer* prevContainer = [self getContainer:self.prevView];
        //prevContainer.hidden = YES;
        //SEContentViewContainer* nextContainer = [self getContainer:self.nextView];
        //nextContainer.hidden = YES;
        [self afterSetCurrentView];
    }
}
- (void) removeAllViewFromRoot
{
    NSArray* subviews = mRootView.subviews;
    for(UIView* v in subviews)
    {
        [v removeFromSuperview];
    }
}
- (NSArray*) fetchDefaultSelectedImage
{
    NSFetchRequest* fetchRequest = [[NSFetchRequest alloc] init];
    NSEntityDescription* entity = [NSEntityDescription entityForName:@"SelectedImage" inManagedObjectContext:self.managedObjectContext];
    [fetchRequest setEntity:entity];
    NSError* error = nil;
    NSArray* data = [self.managedObjectContext executeFetchRequest:fetchRequest error:&error];
    if(error)
    {
        NSLog(@"Unresolved error when init data : %@", [error userInfo]);
        abort();
    }
    [fetchRequest release];
    NSLog(@"selected image num = %d", data.count);
    NSArray* retArray = [NSArray array];
    for(int i = 0 ; i < data.count ; i++)
    {
        SelectedImage* si = [data objectAtIndex:i];
        NSLog(@"si = %@", si.url);
        if([si.url isEqualToString:[SESystemConfig getDefaultSelectedImageURL]])
        {
            retArray = [retArray arrayByAddingObject:si];
        }
    }
    assert(retArray.count <= 1);
    return retArray;
}
- (NSArray*) fetchUserInfo
{
    NSFetchRequest* fetchRequest = [[NSFetchRequest alloc] init];
    NSEntityDescription* entity = [NSEntityDescription entityForName:@"UserInfo" inManagedObjectContext:self.managedObjectContext];
    [fetchRequest setEntity:entity];
    NSError* error = nil;
    NSArray* userInfoObjects = [self.managedObjectContext executeFetchRequest:fetchRequest error:&error];
    if(error)
    {
        NSLog(@"Unresolved error when init data : %@", [error userInfo]);
        abort();
    }
    [fetchRequest release];
    return userInfoObjects;
}
- (void) initState
{
    if([mUserInfoProperty count] > 1)
        return;
}
- (NSData*) getDefaultSignature
{

    NSString *filePath = [[NSBundle mainBundle] pathForResource:@"signature" ofType:@"dat"];
    NSLog(@"signature filepath = %@", filePath);
    NSData* data = [NSData dataWithContentsOfFile:filePath];
    return data;
}
- (void) initData
{
    NSArray* userInfoObjects = [self fetchUserInfo];
    NSUInteger count = [userInfoObjects count];
    if(count == 0)
    {
        NSEntityDescription* selectedImageEntity = [NSEntityDescription entityForName:@"SelectedImage" inManagedObjectContext:self.managedObjectContext];
        NSEntityDescription* imageListEntity = [NSEntityDescription entityForName:@"ImageList" inManagedObjectContext:self.managedObjectContext];
        NSEntityDescription* musicListEntity = [NSEntityDescription entityForName:@"MusicList" inManagedObjectContext:self.managedObjectContext];
        NSEntityDescription* userInfoEntity = [NSEntityDescription entityForName:@"UserInfo" inManagedObjectContext:self.managedObjectContext];
        NSEntityDescription* signatureEntity = [NSEntityDescription entityForName:@"Signature" inManagedObjectContext:self.managedObjectContext];
        NSManagedObject* newUserInfo = [NSEntityDescription insertNewObjectForEntityForName:[userInfoEntity name] inManagedObjectContext:self.managedObjectContext];
        NSNumber* level = [NSNumber numberWithInt:1];
        NSData* levelData = [SEKeyChainHelper createMyStringFromIntValue:1];
        [newUserInfo setValue:levelData forKey:@"level"];
        SEUserData* userData = [mUserUpgradeInfo getUserData:[level intValue]];
        int levelImageNum = userData.imageListNum;
        NSNumber* num = nil;
        int quality = [mResLoader getInt:@"InitQuality"];
        num = [NSNumber numberWithInt:quality];
        NSData* qualityData = [SEKeyChainHelper createMyStringFromIntValue:quality];
        [newUserInfo setValue:qualityData forKey:@"currentimagequality"];
        int times = [mResLoader getInt:@"InitTimes"];
        num = [NSNumber numberWithInt:times];
        NSData* timesData = [SEKeyChainHelper createMyStringFromIntValue:times];
        [newUserInfo setValue:timesData forKey:@"currentimagetimes"];
        [newUserInfo setValue:[SESystemConfig getDefaultImageListName] forKey:@"currentimagelist"];
        [newUserInfo setValue:[SESystemConfig getDefaultMusicListName] forKey:@"currentmusiclist"];
        [newUserInfo setValue:[NSNumber numberWithInt:0] forKey:@"currentsignaturesite"];
        [newUserInfo setValue:[NSNumber numberWithInt:0] forKey:@"currentsignaturesize"];
        [newUserInfo setValue:[NSNumber numberWithInt:0] forKey:@"currentsignature"];
        NSManagedObject* imageList = [NSEntityDescription insertNewObjectForEntityForName:[imageListEntity name] inManagedObjectContext:self.managedObjectContext];
        [imageList setValue:[SESystemConfig getDefaultImageListName] forKey:@"name"];
        [imageList setValue:[NSNumber numberWithInt:0] forKey:@"seq"];
        NSMutableSet* imageListSet = [newUserInfo valueForKey:@"imagelist"];
        [imageListSet addObject:imageList];

        NSManagedObject* musicList = [NSEntityDescription insertNewObjectForEntityForName:[musicListEntity name] inManagedObjectContext:self.managedObjectContext];
        [musicList setValue:[SESystemConfig getDefaultMusicListName] forKey:@"name"];
        [musicList setValue:[NSNumber numberWithInt:0] forKey:@"seq"];
        NSMutableSet* musicListSet = [newUserInfo valueForKey:@"musiclist"];
        [musicListSet addObject:musicList];
        ////// init signature
        NSMutableSet* signatureList = [newUserInfo valueForKey:@"signaturelist"];
        NSManagedObject* signature = [NSEntityDescription insertNewObjectForEntityForName:[signatureEntity name] inManagedObjectContext:self.managedObjectContext];
        [signature setValue:[NSNumber numberWithInt:0] forKey:@"seq"];
        [signatureList addObject:signature];
        [signature setValue:@"default" forKey:@"name"];
        NSData* data = [self getDefaultSignature];
        [signature setValue:data forKey:@"data"];
        SelectedImage* defalutSelectedImage = [NSEntityDescription insertNewObjectForEntityForName:[selectedImageEntity name] inManagedObjectContext:self.managedObjectContext];
        defalutSelectedImage.url = [SESystemConfig getDefaultSelectedImageURL];
        defalutSelectedImage.urldate = [[NSDate date] description];
        NSLog(@"description = %@", defalutSelectedImage.urldate);
        defalutSelectedImage.width = [NSNumber numberWithInt:1024];
        defalutSelectedImage.height = [NSNumber numberWithInt:768];
        defalutSelectedImage.orientation = [NSNumber numberWithInt:UIImageOrientationUp];
        [mSystemDataManager initData:newUserInfo];
        NSError* error = nil;
        if(![self.managedObjectContext save:&error])
        {
            NSLog(@"intialize user info error: %@", [error userInfo]);
            abort();
        }
    }
    else
    {
        mUserInfoProperty = userInfoObjects;
    }
    if(count == 0)
    {
        mUserInfoProperty = [self fetchUserInfo];
        
    }
    [mUserInfoProperty retain];
    [self initState];
    if(count == 0)
    {
        [self createDefaultSelectedImageAndMusic];
    }
    //debug
    /*
    for(NSUInteger i = 0 ; i < [mUserInfoProperty count] ; i++)
    {
        NSManagedObject* ui = [mUserInfoProperty objectAtIndex:i];
        NSSet* imageListSet = [ui valueForKey:@"imagelist"];
        NSSet* musicListSet = [ui valueForKey:@"musiclist"];
        NSEnumerator* it = [imageListSet objectEnumerator];
        NSManagedObject* imageList = nil;
        while((imageList = [it nextObject]) != nil)
        {
            NSString* name = [imageList valueForKey:@"name"];
            NSLog(@"name = %@", name);
            NSSet* selectedImageSet = [imageList valueForKey:@"selectedimage"];
            NSEnumerator* itSelectedImage = [selectedImageSet objectEnumerator];
            NSManagedObject* si = nil;
            while((si = [itSelectedImage nextObject]) != nil)
            {
                NSLog(@"seq = %@", [si valueForKey:@"seq"]);
                NSLog(@"url = %@", [si valueForKey:@"url"]);
                NSLog(@"urldate = %@", [si valueForKey:@"urldate"]);
                NSLog(@"filepath = %@", [si valueForKey:@"filepath"]);
            }
        }
        /////
        it = [musicListSet objectEnumerator];
        NSManagedObject* musicList = nil;
        while((musicList = [it nextObject]) != nil)
        {
            NSSet* selectedMusicSet = [musicList valueForKey:@"selectedmusic"];
            NSEnumerator* itSelectedMusic = [selectedMusicSet objectEnumerator];
            NSManagedObject* mi = nil;
            while((mi = [itSelectedMusic nextObject]) != nil)
            {
            }
        }
        
    }
     */
    //end
}
- (SelectedImage*) getSelectedImageByUrl : (NSString*) urlString andDate: (NSString*)date
{
    NSArray* selectedImageArray = [self getUserImageProperty];
    for(SelectedImage* i in selectedImageArray)
    {
        if([i.url isEqualToString:urlString] && [i.urldate isEqualToString:date])
            return i;
    }
    return nil;
}
- (SelectedImage*) getSelectedImageProperty: (int)index
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* imageList = userInfo.imagelist;
    NSString* currentImageList = userInfo.currentimagelist;
    for(ImageList* il in imageList)
    {
        if([il.name isEqualToString:currentImageList])
        {
            NSSet* selectedImageSet = il.selectedimage;
            for(SelectedImage* si in selectedImageSet)
            {
                if([si.seq isEqualToNumber:[NSNumber numberWithInt:index]])
                {
                    return si;
                }
            }
        }
    }
    return nil;
}
// levelImageNum : current level's image list num
// selecteImageSet.num : current image num in image list
// if levelImageNum > selectedImageSet.num we will add the additional selected image to image list
// else do nothing
- (void) addSelectedImageToImageListByNeeded : (ImageList*)il
{
    
    int levelImageNum = [self getCurrentLevelImageNum];
    NSSet* selectedImageSet = il.selectedimage;
    NSEntityDescription* selectedImageEntity = [NSEntityDescription entityForName:@"SelectedImage" inManagedObjectContext:self.managedObjectContext];
    if(levelImageNum > selectedImageSet.count)
    {
        //int count = levelImageNum - selectedImageSet.count;
        int startIndex = selectedImageSet.count;
        for(int i = startIndex ; i < levelImageNum ; i++)
        {
            SelectedImage* si = [NSEntityDescription insertNewObjectForEntityForName:[selectedImageEntity name] inManagedObjectContext:self.managedObjectContext];
            NSNumber* seq = [NSNumber numberWithInt:i];
            si.seq = seq;
            [il addSelectedimageObject:si];
        }
    }
}
- (void) fetchAllSelectedImage
{
    NSEntityDescription* entity = [NSEntityDescription entityForName:@"SelectedImage" inManagedObjectContext:self.managedObjectContext];
    NSFetchRequest* fetchReq = [[NSFetchRequest alloc] init];
    [fetchReq setEntity:entity];
    NSError* error = nil;
    NSArray* data = [self.managedObjectContext executeFetchRequest:fetchReq error:&error];
    if(error)
    {
        NSLog(@"Unresolved error when init data : %@", [error userInfo]);
        abort();
    }
    [fetchReq release];
    NSLog(@"selected image num = %d", data.count);
    for(int i = 0 ; i < data.count ; i++)
    {
        SelectedImage* si = [data objectAtIndex:i];
        NSLog(@"si.seq = %d, si.url = %@", [si.seq intValue], si.url);
    }
}
- (NSArray*) getSelectedImageArrayByName: (NSString*)name
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* imageListSet = userInfo.imagelist;
    NSString* currentImageList = name;
    for(ImageList* il in imageListSet)
    {
        if([il.name isEqualToString:currentImageList])
        {
            //[self addSelectedImageToImageListByNeeded:il];
            NSSet* selectedImageSet = il.selectedimage;
            NSArray* newArray = [selectedImageSet allObjects];
            newArray = [newArray sortedArrayUsingComparator:^NSComparisonResult(id obj1, id obj2) {
                SelectedImage* mo1 = (SelectedImage*)obj1;
                SelectedImage* mo2 = (SelectedImage*)obj2;
                NSNumber* left = mo1.seq;
                NSNumber* right = mo2.seq;
                return [left compare:right];
            }];
            return newArray;
        }
    }
    return nil;
}
- (void) removeAllSelectedImageInImageList:(NSString*) ilName
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* imageListSet = userInfo.imagelist;
    NSString* currentImageList = ilName;
    for(ImageList* il in imageListSet)
    {
        if([il.name isEqualToString:currentImageList])
        {
            NSSet* selectedImageSet = il.selectedimage;
            [il removeSelectedimage:selectedImageSet];
        }
    }
}
- (NSArray*) getSortedSelectedImageArrayBySeq: (ImageList*) il
{
    NSSet* selectedImageSet = il.selectedimage;
    if(selectedImageSet == nil || selectedImageSet.count == 0)
        return nil;
    NSArray* array = [selectedImageSet allObjects];
    array = [array sortedArrayUsingComparator:^NSComparisonResult(id obj1, id obj2) 
             {
                 SelectedImage* mo1 = (SelectedImage*)obj1;
                 SelectedImage* mo2 = (SelectedImage*)obj2;
                 NSNumber* left = mo1.seq;
                 NSNumber* right = mo2.seq;
                 return [left compare:right];
             }];
    return array;
}
- (SelectedImage*) getNoneNullSelectedImage: (ImageList*)il :(int) n
{
    NSArray* array = [self getSortedSelectedImageArrayBySeq:il];
    int index = 0;
    SelectedImage* currentSi = nil;
    while (index <= n)
    {
        int i;
        for(i = 0 ; i < array.count ; i++)
        {
            SelectedImage* si = [array objectAtIndex:i];
            if(si.url != nil)
            {
                currentSi = si;
                index++;
                break;
            }
        }
        if(i == array.count && currentSi == nil)
        {
            break;
        }
    }
    return currentSi;
}
- (NSArray*) getUserImageProperty
{
    UserInfo* userInfo = [self getUserInfo];
    NSString* currentImageList = userInfo.currentimagelist;
    NSArray* retArray = [self getSelectedImageArrayByName:currentImageList];
    return retArray;
}
- (void) saveContext
{
    for(int i = 0 ;i < VIEW_NUM ; i++)
    {
        SEContentViewContainer* c = mViewArray[i];
        [c saveContext: mUserInfoProperty];
    }
    [self saveCoreDataContext];
}
- (void) saveCoreDataContext
{
    NSError* error = nil;
    if(self.managedObjectContext != nil)
    {
        BOOL changed = [self.managedObjectContext hasChanges];
        if(changed && ![self.managedObjectContext save:&error])
        {
            NSLog(@"Unresolved error %@, %@", error , [error userInfo]);
            abort();
        }
    }
}
- (UIView<SEAdjustContentView>*) createContentView: (VIEW_TYPE) viewType withFrame: (CGRect)r
{
    if(viewType != INVALID_VIEW)
    {
        UIView<SEAdjustContentView>* view = nil;
        SEContentViewContainer* container = nil;
        UIImage* image = nil;
        switch (viewType) {
            case MAIN_DISPLAY:
                view = [self createMainDisplayView:r];
                container = mViewArray[MAIN_DISPLAY];
                container.mBackgroundImage = [mResLoader getImage:@"MainDisplayBg"];
                break;
            case IMAGE_PICKER:
            {
                view = [self createImagePickerView:r];
                container = mViewArray[IMAGE_PICKER];
                image = [mResLoader getImage:@"ImagePickerViewBackground"];
                container.mBackgroundImage = image;
                [container createTitleLabel:CGRectMake(container.mTitleLabelLeftX, 0, 300, 63) fontName:[SESystemConfig getFontName] fontSize:30 textColor:[UIColor whiteColor]];
                [container createStatusLabel:CGRectMake(container.mStatusLabelRightX, 0, container.mStatusLabelWidth, container.mStatusLabelHeight) fontName:[SESystemConfig getFontName] fontSize:30 textColor:[UIColor whiteColor]];
                NSLog(@"picker status label x = %f", container.mStatusLabelRightX);
                [container setTitleText:@"IMAGE LIBRARY"];
                [container setStatusText:@""];
            }
                break;
            case SELECTED_IMAGE_VIEW:
            {
                view = [self createImageSelectedView:r];
                container = mViewArray[SELECTED_IMAGE_VIEW];
                image = [mResLoader getImage:@"ImageSelectedViewBackground"];
                container.mBackgroundImage = image;
                [container setNeedsDisplay];
            
                [container createTitleLabel:CGRectMake(container.mTitleLabelLeftX, 0, 300, 63) fontName:[SESystemConfig getFontName] fontSize:30 textColor:[UIColor whiteColor]];
                [container createStatusLabel:CGRectMake(container.mStatusLabelRightX, 0, container.mStatusLabelWidth, container.mStatusLabelHeight) fontName:[SESystemConfig getFontName] fontSize:30 textColor:[UIColor whiteColor]];
                NSLog(@"select status label x = %f", container.mStatusLabelRightX);
                [container setTitleText:mCurrentLoadedImageListName];
                [container setStatusText:@""];
            }
                break;
            case MUSIC_PICKER:
                view = [self createMusicPickerView:r];
                container = mViewArray[MUSIC_PICKER];
                image = [mResLoader getImage:@"MusicPickerBackground"];
                container.mBackgroundImage = image;
                
                [container createTitleLabel:CGRectMake(container.mTitleLabelLeftX, 0, 300, 63) fontName:[SESystemConfig getFontName] fontSize:30 textColor:[UIColor whiteColor]];
                [container createStatusLabel:CGRectMake(container.mStatusLabelRightX, 0, container.mStatusLabelWidth, container.mStatusLabelHeight) fontName:[SESystemConfig getFontName] fontSize:30 textColor:[UIColor whiteColor]];
                NSLog(@"picker music status label x = %f", container.mStatusLabelRightX);
                [container setTitleText:@"MUSIC LIBRARY"];
                [container setStatusText:@""];
                
                break;
            case SELECTED_MUSIC_VIEW:
                view = [self createSelectedMusicView:r];
                container = mViewArray[SELECTED_MUSIC_VIEW];
                image = [mResLoader getImage:@"SelectedMusicViewBackground"];
                container.mBackgroundImage = image;
                
                [container createTitleLabel:CGRectMake(container.mTitleLabelLeftX, 0, 300, 63) fontName:[SESystemConfig getFontName] fontSize:30 textColor:[UIColor whiteColor]];
                [container createStatusLabel:CGRectMake(container.mStatusLabelRightX, 0, container.mStatusLabelWidth, container.mStatusLabelHeight) fontName:[SESystemConfig getFontName] fontSize:30 textColor:[UIColor whiteColor]];
                NSLog(@"select music status label x = %f", container.mStatusLabelRightX);
                [container setTitleText:mCurrentLoadedMusicListName];
                [container setStatusText:@""];
                break;
            case MUSIC_IMAGE_LIST_ATTACH:
                view = [self createMusicImageListAttachView:r];
                container = mViewArray[MUSIC_IMAGE_LIST_ATTACH];
                container.mBackgroundImage = [mResLoader getImage:@"MusicImageAttachBackground"];
                break;
            case OPTIONS:
                view = [self createOptionsView:r];
                break;
            case SIGNATURE_VIEW:
                view = [self createSignatureView:r];
                break;
            case SIGNATURE_PREVIEW:
                view = [self createSignaturePreview:r];
                break;
            case PREVIEW_3D:
                view = [self createPreview3D:r];
                image = [UIImage imageNamed:@"3DScreen.png"];
                container = mViewArray[PREVIEW_3D];
                container.mBackgroundImage = image;
                break;
            default:
                break;
        }
        container.backgroundColor = [UIColor blackColor];
        return view;
    }
    else
        return nil;
}
- (void) addSettingView: (UIView*)view
{
    [mRootView addSubview:view];
}
- (BOOL) isFloatViewShow
{
    return mFloatView != nil;
}
- (SEPageUIImageView*) getCurrentHitedPageUIImageView
{
    return mCurrentPageUIImageView;
}
- (void) highlightPlacedView: (SEPageUIImageView*)imageView
{
    if(imageView == nil)
    {
        if(mCurrentPageUIImageView.mIsAnimation == NO)
        {
            mCurrentPageUIImageView.frame = mCurrentPageUIImageView.mOriginRect;
        }
        mCurrentPageUIImageView = nil;
        return;
    }
    if([imageView isDefaultImage])
    {
        //mCurrentPageUIImageView.frame = mCurrentPageUIImageViewRect;
        if(mCurrentPageUIImageView.mIsAnimation == NO)
        {
            mCurrentPageUIImageView.frame = mCurrentPageUIImageView.mOriginRect;
        }
        mCurrentPageUIImageView = imageView;
        return;
    }
    if(mCurrentPageUIImageView == imageView)
    {
        return;
    }

    if(mCurrentPageUIImageView.mIsAnimation == NO)
    {
        mCurrentPageUIImageView.frame = mCurrentPageUIImageView.mOriginRect;    
    }
    mCurrentPageUIImageView = imageView;

    //mCurrentPageUIImageViewRect = mCurrentPageUIImageView.frame;
    //mCurrentPageUIImageView.mOriginRect = mCurrentPageUIImageView.frame;
    if(imageView.mIsAnimation == NO)
    {
        CGPoint center = imageView.center;
        CGPoint p = CGPointMake(center.x + 20 , center.y);
        void (^animBlock) (void) = ^{
            imageView.center = p;
        };
        void (^animComplete) (BOOL) = ^(BOOL) 
        {
            imageView.mIsAnimation = NO;
            SEPageUIImageView* currentPageImageView = [self getCurrentHitedPageUIImageView];
            if(imageView != currentPageImageView)
            {
                imageView.frame = imageView.mOriginRect;
            }
        };
        int opts = UIViewAnimationOptionCurveLinear | UIViewAnimationOptionAllowUserInteraction;
        [UIView animateWithDuration:0.2 delay:0 options:opts animations:animBlock completion:animComplete];
        imageView.mIsAnimation = YES;
        //[UIView animateWithDuration:0.2 animations:animBlock completion:animComplete];
    }
}
- (void) normalizePlacedView: (SEPageUIImageView*)imageView
{
    //imageView.backgroundColor = [UIColor clearColor];
    //imageView.alpha = 1.0;
    //[imageView setNeedsDisplay];
}

- (void) moveFloatViewToPoint : (CGPoint)p
{
    if(mFloatView)
    {
        SEContentViewContainer* sc = mViewArray[SELECTED_IMAGE_VIEW];
        SEPageUIScrollView* selectedScrollView = (SEPageUIScrollView*)[sc contentView];
        mFloatView.center = p;
        CGPoint c = mFloatView.center;
        c = [mRootView convertPoint:c toView:selectedScrollView];
        
        SEPageHitProperty hp;
        hp.index = SE_INVALID_IMAGE_INDEX;
        hp.imageView = nil;
        hp.rect = CGRectMake(0, 0, 0, 0);
        if(selectedScrollView != nil)
            hp = [selectedScrollView hitRect:c];
        //NSLog(@"current selected index = %d\n",hp.index);
        if(mPlacedViewIndex == SE_INVALID_IMAGE_INDEX && hp.index != SE_INVALID_IMAGE_INDEX)
        {
            mPlacedViewIndex = hp.index;
            [self highlightPlacedView:hp.imageView];
        }
        else if (mPlacedViewIndex != SE_INVALID_IMAGE_INDEX && hp.index != SE_INVALID_IMAGE_INDEX)
        {
            SEPageUIImageView* currentPlacedView = [selectedScrollView imageView:mPlacedViewIndex];
            [self normalizePlacedView:currentPlacedView];
            [self highlightPlacedView:hp.imageView];
            mPlacedViewIndex = hp.index;
        }
        else if(mPlacedViewIndex != SE_INVALID_IMAGE_INDEX && hp.index == SE_INVALID_IMAGE_INDEX)
        {
            SEPageUIImageView* currentPlacedView = [selectedScrollView imageView:mPlacedViewIndex];
            [self normalizePlacedView:currentPlacedView];
            [self highlightPlacedView:nil];
            mPlacedViewIndex = SE_INVALID_IMAGE_INDEX;
        }
        else
        {
            mPlacedViewIndex = hp.index;
        }
        [self intersectWithOperationView];
    }
    else
    {
        NSLog(@"error: float view is nil");
    }
}
- (void)touchBegin:(NSArray *)points1 : (NSArray*)points2 withPointDelta: (CGPoint)deltap withScrollView:(SEPageUIScrollView *)scrollView
{
    SEContentViewContainer* imagePicker = mViewArray[IMAGE_PICKER];
    NSValue* v = [points1 lastObject];
    CGPoint p = [v CGPointValue];
    NSLog(@"p = %f, %f", p.x, p.y);
    if(imagePicker.contentView == scrollView && mFloatView == nil)
    {
        SEPageUIScrollView* currView = scrollView;
        SEPageHitProperty hp = [currView hitRect:p];
        if(hp.imageView != nil)
        {
            CGRect r = [hp.imageView convertRect:hp.rect toView:mRootView];
            SEPageUIImageView* imageView = hp.imageView;
            mFloatView = [[SEUIFloatView alloc] initWithFrame:r];
            mFloatView.backgroundColor = [UIColor greenColor];
            mFloatView.contentMode = UIViewContentModeCenter;
            mFloatView.clipsToBounds = YES;
            mFloatView.image = imageView.image;
            //mFloatView.p = p;
            mFloatView.origC = mFloatView.center;
            [mRootView addSubview:mFloatView];
            [mFloatView release];
            NSLog(@"select index = %d", hp.index);
            self.mSelectedPhotoURL = [NSMutableArray array];
            //[mSelectedPhotoURL retain];
            [mSelectedPhotoURL addObject:[currView getImageURL:hp.index]];
            NSLog(@"selected url = %@", mSelectedPhotoURL);
            [currView disableAllGestures];
        }
        mPlacedViewIndex = SE_INVALID_IMAGE_INDEX;
    }
}
- (void) touchMove:(NSArray *)points1 : (NSArray*)points2 withPointDelta: (CGPoint)deltap withScrollView:(SEPageUIScrollView *)scrollView
{
    SEContentViewContainer* imagePicker = mViewArray[IMAGE_PICKER];
    if(scrollView == imagePicker.contentView)
    {
        if([self isFloatViewShow])
        {
            CGPoint p;
            CGPoint c = mFloatView.center;
            p.x = c.x + deltap.x;
            p.y = c.y + deltap.y;
            NSLog(@"touch in scrollview : p = %f, %f", p.x, p.y);
            [self moveFloatViewToPoint:p];
        }
    }
}
- (void)saveImageURL: (SEPageImageURL*)imageURL withSeq:(int) seq
{
    SelectedImage* si = [self getSelectedImageProperty:seq];
    if(imageURL.url)
    {
        NSString* str = [imageURL.url absoluteString];
        si.url = str;
        si.urldate = imageURL.urlDate;
    }
    if(imageURL.filepath)
    {
        NSString* str = [imageURL.filepath absoluteString];
        si.filepath = str;
    }
}
- (void) addImageToSelectedImageScrollView: (NSMutableArray*) urlArray : (NSMutableArray*)imageArray
{
    assert(mPlacedViewIndex != SE_INVALID_IMAGE_INDEX);
    SEContentViewContainer* selectedContainer = mViewArray[SELECTED_IMAGE_VIEW];
    SEPageUIScrollView* selectedScrollView = (SEPageUIScrollView*)selectedContainer.contentView;
    [selectedScrollView insertURLToPhotoAsset:mPlacedViewIndex url:urlArray image: imageArray];
}
- (void) touchEnd:(NSArray *)points1 : (NSArray*)points2 withPointDelta: (CGPoint)deltap withScrollView:(SEPageUIScrollView *)scrollView
{
    SEContentViewContainer* imagePicker = mViewArray[IMAGE_PICKER];
    if(imagePicker.contentView == scrollView)
    {
        if(mPlacedViewIndex != SE_INVALID_IMAGE_INDEX)
        {
            SEContentViewContainer* selectedContainer = mViewArray[SELECTED_IMAGE_VIEW];
            SEPageUIScrollView* selectedScrollView = (SEPageUIScrollView*)selectedContainer.contentView;
            //[self addImageToSelectedImageScrollView:mSelectedPhotoURL];
            SEPageUIImageView* imageView = [selectedScrollView imageView:mPlacedViewIndex];
            [self normalizePlacedView:imageView];
            mPlacedViewIndex = SE_INVALID_IMAGE_INDEX;
            //[mSelectedPhotoURL release];
            self.mSelectedPhotoURL = nil;
        }
        mSelectedPhotoURL = nil;
        if(mFloatView)
        {
            [mFloatView removeFromSuperview];
            mFloatView = nil; 
            [scrollView restoreGesture];
        }
    }
}
- (int) getImageListCount
{
    NSManagedObject* userInfo = [mUserInfoProperty objectAtIndex:0];
    NSSet* imageListSet = [userInfo valueForKey:@"imagelist"];
    return [imageListSet count];
}
- (int) getMusicListCount
{
    NSManagedObject* userInfo = [mUserInfoProperty objectAtIndex:0];
    NSSet* musicListSet = [userInfo valueForKey:@"musiclist"];
    return [musicListSet count];
}
- (int) getImageCount: (NSSet*) selectedImageSet
{
    NSEnumerator* it = [selectedImageSet objectEnumerator];
    SelectedImage* si = nil;
    int count = 0;
    while ((si = [it nextObject]) != nil) 
    {
        if(si.url != nil || si.filepath != nil) 
            count++;
    }
    return count;
}

- (int) getCurrentLevelImageNum
{
    UserInfo* userInfo = [self getUserInfo];
    int expNum = [self.mSystemDataManager.exppointnum intValue];
    int level = [mUserUpgradeInfo getLevelFromExpNum:expNum];
    SEUserData* userData = [mUserUpgradeInfo getUserData:level];
    int levelImageNum = userData.imageListNum;
    int imageDeltaNum = [mUserUpgradeInfo getAllAchieveDeltaImageNum];
    return levelImageNum + imageDeltaNum;
}
- (int) getCurrentLevelMusicNum
{
    UserInfo* userInfo = [self getUserInfo];
    int expNum = [self.mSystemDataManager.exppointnum intValue];
    int level = [mUserUpgradeInfo getLevelFromExpNum:expNum];
    SEUserData* userData = [mUserUpgradeInfo getUserData:level];
    int levelMusicNum = userData.musicListNum;
    int musicDeltaNum = [mUserUpgradeInfo getAllAchieveDeltaMusicNum];
    return levelMusicNum + musicDeltaNum;
}
- (NSArray*) getAllImageList
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* imageListSet = userInfo.imagelist;
    NSArray* imageListArray = [imageListSet allObjects];
    if(imageListArray)
    {
        imageListArray = [imageListArray sortedArrayUsingComparator:^NSComparisonResult(id obj1, id obj2) {
            ImageList* il1 = (ImageList*)obj1;
            ImageList* il2 = (ImageList*)obj2;
            return [il1.seq compare:il2.seq];
            
        }];
        return imageListArray;
    }
    else
        return nil;
    
}

- (NSArray*) getAllMusicList
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* musicListSet = userInfo.musiclist;
    NSArray* musicListArray = [musicListSet allObjects];
    if(musicListArray)
    {
        musicListArray = [musicListArray sortedArrayUsingComparator:^NSComparisonResult(id obj1, id obj2) {
            MusicList* ml1 = (MusicList*)obj1;
            MusicList* ml2 = (MusicList*)obj2;
            return [ml1.seq compare: ml2.seq];
        }];
    
        return musicListArray;
    }
    else
        return nil;
}
/*
- (NSArray*) getImageListProperty
{
    NSManagedObject* userInfo = [mUserInfoProperty objectAtIndex:0];
    NSSet* imageListSet = [userInfo valueForKey:@"imagelist"];
    NSEnumerator* it = [imageListSet objectEnumerator];
    NSManagedObject* imageList = nil;
    NSArray* imagePropertyArray = [NSArray array];
    while ((imageList = [it nextObject]) != nil) 
    {
        NSSet* siSet = [imageList valueForKey:@"selectedimage"];
        NSString* name = [imageList valueForKey:@"name"];
        SEImageListProperty* ip = [[SEImageListProperty alloc] init];
        ip.imageCount = [self getImageCount:siSet];
        ip.name = name;
        imagePropertyArray = [imagePropertyArray arrayByAddingObject:ip];
        [ip release];
    }
    return imagePropertyArray;
}
- (NSArray*) getMusicListProperty
{
    NSManagedObject* userInfo = [mUserInfoProperty objectAtIndex:0];
    NSSet* musicListSet = [userInfo valueForKey:@"musiclist"];
    NSEnumerator* it = [musicListSet objectEnumerator];
    NSManagedObject* musicList = nil;
    NSArray* musicPropertyArray = [NSArray array];
    while ((musicList = [it nextObject]) != nil) 
    {
        NSSet* miSet = [musicList valueForKey:@"selectedmusic"];
        NSString* name = [musicList valueForKey:@"name"];
        SEMusicListProperty* ip = [[SEMusicListProperty alloc] init];
        ip.musicCount = [miSet count];
        ip.name = name;
        musicPropertyArray = [musicPropertyArray arrayByAddingObject:ip];
        [ip release];
    }
    return musicPropertyArray;
}
 */
- (UserInfo*)getUserInfo
{
    if([mUserInfoProperty count] == 1)
        return [mUserInfoProperty objectAtIndex:0];
    else
        return nil;
}
- (void) setImageQuality:(int) q
{
    UserInfo* userInfo = [self getUserInfo];
    self.mSystemDataManager.currentimagequality = [NSNumber numberWithInt:q];
    //[self saveCoreDataContext];
}
- (void) setImageTimes: (int)t
{
    UserInfo* userInfo = [self getUserInfo];
    self.mSystemDataManager.currentimagetimes = [NSNumber numberWithInt:t];
    //[self saveCoreDataContext];
}
- (int) getImageQuality
{
    UserInfo* userInfo = [self getUserInfo];
    int ret = [self.mSystemDataManager.currentimagequality intValue];
    return ret;
}
- (int) getImageTimes
{
    UserInfo* userInfo = [self getUserInfo];
    int ret = [self.mSystemDataManager.currentimagetimes intValue];
    return ret;
}
- (NSArray*) getCurrentSelectedMusicArray
{
    NSArray* currentSelectedMusic = [NSArray array];
    UserInfo* userInfo = [mUserInfoProperty objectAtIndex:0];
    NSString* currentMusicList = mCurrentLoadedMusicListName;//userInfo.currentmusiclist;
    NSSet* musicListSet = [userInfo valueForKey:@"musiclist"];
    NSManagedObject* musicList = nil;
    NSEnumerator* itMusicList = [musicListSet objectEnumerator];
    while((musicList = [itMusicList nextObject]) != nil)
    {
        NSString* name = [musicList valueForKey:@"name"];
        if([name isEqualToString:currentMusicList])
        {
            NSSet* smSet = [musicList valueForKey:@"selectedmusic"];
            NSManagedObject* sm = nil;
            NSEnumerator* itSelectedMusic = [smSet objectEnumerator];
            while((sm = [itSelectedMusic nextObject]) != nil)
            {
                SEMusicItemProperty* item = [[SEMusicItemProperty alloc] init];
                NSString* title = [sm valueForKey:@"title"];
                NSString* singer = [sm valueForKey:@"singer"];
                NSString* album = [sm valueForKey:@"album"];
                NSNumber* seqNum = [sm valueForKey:@"seq"];
                item.title = title;
                item.artist = singer;
                item.album = album;
                item.seq = seqNum;
                currentSelectedMusic = [currentSelectedMusic arrayByAddingObject:item];
                [item release];
            }
        }
        
    }
    return currentSelectedMusic;
}
- (void) addSelectedMusicTitle: (NSString*)title aritist: (NSString*) artist album:(NSString*)album toMusicList: (NSString*) musicListName;
{
    MusicList* musicList = [self getMusicListByName:musicListName];
    SelectedMusic* selectedMusic = (SelectedMusic*)[self newObjectByEntityName:@"SelectedMusic"];
    selectedMusic.title = title;
    selectedMusic.singer = artist;
    selectedMusic.album = album;
    NSSet* selectMusicSet = musicList.selectedmusic;
    NSArray* selectMusicArray = [selectMusicSet allObjects];
    selectMusicArray = [selectMusicArray sortedArrayUsingComparator:^NSComparisonResult(id obj1, id obj2) {
        SelectedMusic* s1 = (SelectedMusic*)obj1;
        SelectedMusic* s2 = (SelectedMusic*)obj2;
        return [s1.seq compare:s2.seq];
    }];
    if(selectMusicArray.count == 0)
    {
        selectedMusic.seq = [NSNumber numberWithInt:0];
    }
    else
    {
        SelectedMusic* lastObj = [selectMusicArray lastObject];
        selectedMusic.seq = [NSNumber numberWithInt:[lastObj.seq intValue] + 1];
    }
    [musicList addSelectedmusicObject:selectedMusic];
}
- (BOOL) isHorizonPhoto: (float) width : (float)height : (int) o
{
    if(width > height)
    {
        if(o == UIImageOrientationDown || o == UIImageOrientationUp || o == UIImageOrientationUpMirrored || o == UIImageOrientationDownMirrored)
        {
            return YES;
        }
        else
        {
            return NO;        
        }
    }
    else
    {
        if(o == UIImageOrientationUp || o == UIImageOrientationDown || o == UIImageOrientationUpMirrored || o == UIImageOrientationDownMirrored)
        {
            return NO;
        }
        else 
        {
            return YES;
        }
    }    
}
#define IMAGE_HORIZON 1
#define IMAGE_VERTICAL 0
- (NSArray*) getImageByOrientation: (NSArray*) imageArray: (int) orient
{
    NSMutableArray* horizonArray = [NSMutableArray array];
    NSMutableArray* verticalArray = [NSMutableArray array];
    for(int i = 0 ; i < imageArray.count ; i++)
    {
        SelectedImage* si = [imageArray objectAtIndex:i];
        BOOL isH = [self isHorizonPhoto: [si.width intValue]: [si.height intValue]: [si.orientation intValue]];
        if(isH)
        {
            [horizonArray addObject:si];
        }
        else
        {
            [verticalArray addObject:si];
        }
    }
    if(orient == IMAGE_HORIZON)
    {
        return horizonArray;
    }
    else if(orient == IMAGE_VERTICAL)
    {
        return verticalArray;
    }
    else 
    {
        return nil;
    }
}
- (int) advanceIndex:(NSArray*)selectedImageArray: (int) currentIndex
{
    int index = currentIndex;
    index++;
    if(index >= selectedImageArray.count)
    {
        index = 0;
    }
    return index;
}
//not found return -1
- (int) getNextProperIndexFrom: (NSArray*) selectedImageArray: (int)currentImageIndex : (int) orient
{
    //int inputIndex = currentImageIndex;
    int nextIndex = [self advanceIndex:selectedImageArray :currentImageIndex];
    while (currentImageIndex != nextIndex)
    {
        SelectedImage* si = [selectedImageArray objectAtIndex:nextIndex];
        BOOL isH = [self isHorizonPhoto:[si.width intValue] : [si.height intValue] : [si.orientation intValue]];
        if(isH && orient == IMAGE_HORIZON)
        {
            return nextIndex;
        }
        else if(!isH && orient == IMAGE_VERTICAL)
        {
            return nextIndex;
        }
        else
        {
            nextIndex = [self advanceIndex:selectedImageArray : nextIndex];
        }
    }
    return -1;
}
- (BOOL) findSequenceModeImageIndex: (int)currentImageIndex: (NSArray*) selectedArray :(int*)outIndex
{
    UserInfo* userInfo = [self getUserInfo];
    BOOL isImageFilter = [userInfo.imagesizefilter boolValue];
    int inputIndex = currentImageIndex;
    if(isImageFilter == NO)
    {
        inputIndex++;
        int imageCount = [selectedArray count];
        if(inputIndex >= imageCount)
        {
            inputIndex = 0;
        }
        *outIndex = inputIndex;
        return YES;
    }
    else 
    {
        int nextIndex;
        if(UIInterfaceOrientationIsPortrait(self.interfaceOrientation))
        {
            nextIndex = [self getNextProperIndexFrom:selectedArray :inputIndex :IMAGE_VERTICAL];
        }
        else
        {
            nextIndex = [self getNextProperIndexFrom:selectedArray :inputIndex :IMAGE_HORIZON];
        }
        if(nextIndex == -1)
        {
            return NO;
        }
        else
        {
            *outIndex = nextIndex;
            return YES;
        }
    }
}
- (BOOL) findRandomModeImageIndex: (int)currentImageIndex: (NSArray*)selectedArray :(int*)outIndex
{
    UserInfo* userInfo = [self getUserInfo];
    BOOL isImageFilter = [userInfo.imagesizefilter boolValue];
    if(isImageFilter == NO)
    {
        int imageCount = selectedArray.count;
        int random = abs(rand() % imageCount);
        *outIndex = random;
        return YES;
    }
    else
    {
        NSArray* imageArray = nil;
        if(UIInterfaceOrientationIsPortrait(self.interfaceOrientation))
        {
            imageArray = [self getImageByOrientation:selectedArray :IMAGE_VERTICAL];
        }
        else
        {
            imageArray = [self getImageByOrientation:selectedArray :IMAGE_HORIZON];
        }
        if(imageArray.count == 0)
        {
            return NO;
        }
        else
        {
            int index = abs(rand() % imageArray.count);
            SelectedImage* si = [imageArray objectAtIndex:index];
            BOOL found = NO;
            int i;
            for(i = 0 ; i < selectedArray.count ; i++)
            {
                SelectedImage* tmp = [selectedArray objectAtIndex:i];
                if(tmp == si)
                {
                    found = YES;
                    break;
                }
            }
            assert(found == YES);
            *outIndex = i;
            return YES;
        }
    }
}
- (BOOL) imageHasDrawn: (NSArray*)selectedImageArray: (int)index
{
    SelectedImage* si = [selectedImageArray objectAtIndex:index];
    NSArray* drawnData = [self fetchFinishedImage:si.url urlDate:si.urldate];
    if(drawnData.count > 0)
        return YES;
    else 
    {
        return NO;
    }
}
- (NSArray*) findNotDrawnImageIndexArray: (NSArray*)selectedImageArray: (int)currentImageIndex
{
    int nextIndex = [self advanceIndex:selectedImageArray :currentImageIndex];
    NSMutableArray* array = [NSMutableArray array];
    while (currentImageIndex != nextIndex)
    {
        if([self imageHasDrawn:selectedImageArray :nextIndex] == NO)
        {
            [array addObject:[NSNumber numberWithInt:nextIndex]];
        }
        nextIndex = [self advanceIndex:selectedImageArray :nextIndex];
    }
    return array;
}
- (int) findPropertNotDrawnIndex: (NSArray*) selectedImageArray: (int) currentImageIndex
{
    NSArray* array = [self findNotDrawnImageIndexArray:selectedImageArray :currentImageIndex];
    if(array.count == 0)
        return -1;
    return [[array objectAtIndex:0] intValue];// can use other criterion
}
- (int) findProperNotDrawnIndexByOrientation: (NSArray*) selectedImageArray: (int) currentImageIndex
{
    NSArray* array = [self findNotDrawnImageIndexArray:selectedImageArray :currentImageIndex];
    if(array.count == 0)
        return -1;
    NSMutableArray* imageArray = [NSMutableArray array];
    for(int i = 0 ; i < array.count ; i++)
    {
        int index = [[array objectAtIndex:i] intValue];
        SelectedImage* si = [selectedImageArray objectAtIndex:index];
        [imageArray addObject:si];
    }
    NSArray* retImageArray = nil;
    if(UIInterfaceOrientationIsPortrait(self.interfaceOrientation))
    {
        retImageArray = [self getImageByOrientation:imageArray :IMAGE_VERTICAL];
    }
    else
    {
        retImageArray = [self getImageByOrientation:imageArray :IMAGE_HORIZON];
    }
    if(retImageArray.count == 0)
        return -1;
    SelectedImage* si = [retImageArray objectAtIndex:0];// can use other criterion
    BOOL found = NO;
    int i;
    for(i = 0 ; i < selectedImageArray.count ; i++)
    {
        SelectedImage* tmp = [selectedImageArray objectAtIndex:i];
        if(tmp == si)
        {
            found = YES;
            break;
        }
    }
    assert(found);
    return i;
}
- (BOOL) findIntelModeImageIndex: (int)currentImageIndex: (NSArray*)selectedArray :(int*)outIndex
{
    UserInfo* userInfo = [self getUserInfo];
    BOOL isImageFilter = [userInfo.imagesizefilter boolValue];
    if(isImageFilter == NO)
    {
        int index = [self findPropertNotDrawnIndex:selectedArray :currentImageIndex];
        if(index == -1)
        {
            return [self findSequenceModeImageIndex:currentImageIndex :selectedArray :outIndex];
        }
        else
        {
            *outIndex = index;
            return YES;
        }
    }
    else
    {
        int index = [self findProperNotDrawnIndexByOrientation:selectedArray :currentImageIndex];
        if(index == -1)
        {
            return [self findSequenceModeImageIndex:currentImageIndex :selectedArray :outIndex];
        }
        else
        {
            *outIndex = index;
            return YES;
        }
    }
}
- (BOOL) findNextProperImageIndex: (int)currentImageIndex: (NSArray*) selectedImageArray: (BOOL) imageArrayChanged: (int*)outIndex
{
    UserInfo* userInfo = [self getUserInfo];
    int sequenceMode = [userInfo.imageplaymode intValue];
    BOOL isImageFilter = [userInfo.imagesizefilter boolValue];
    BOOL found = NO;
    if(imageArrayChanged)
    {
        if(isImageFilter == NO)
        {
            *outIndex = currentImageIndex;
            return YES;
        }
        else
        {
            SelectedImage* si = [selectedImageArray objectAtIndex:currentImageIndex];
            BOOL isH = [self isHorizonPhoto:[si.width intValue] :[si.height intValue] :[si.orientation intValue]];
            if(UIInterfaceOrientationIsPortrait(self.interfaceOrientation))
            {
                if(!isH)
                {
                    *outIndex = currentImageIndex;
                    return YES;
                }
            }
            else
            {
                if(isH)
                {
                    *outIndex = currentImageIndex;
                    return YES;
                }
                
            }
        }
    }
    switch (sequenceMode) 
    {
        case 0:
        {
            found = [self findSequenceModeImageIndex:currentImageIndex :selectedImageArray: outIndex];
        }
            break;
        case 1:
        {
            found = [self findRandomModeImageIndex:currentImageIndex :selectedImageArray: outIndex];
        }
            break;
        case 2:
        {
            found = [self findIntelModeImageIndex:currentImageIndex :selectedImageArray: outIndex];
        }
            break;
        default:
            break;
    }
    if(!found)
    {
        SelectedImage* si = [selectedImageArray objectAtIndex:currentImageIndex];
        int orient = IMAGE_HORIZON;
        if(UIInterfaceOrientationIsPortrait(self.interfaceOrientation))
        {
            orient = IMAGE_VERTICAL;
        }
        BOOL isH = [self isHorizonPhoto:[si.width intValue] : [si.height intValue] : [si.orientation intValue]];
        if(isH && orient == IMAGE_HORIZON)
        {
            *outIndex = currentImageIndex;
            return YES;
        }
        else if(!isH && orient == IMAGE_VERTICAL)
        {
            *outIndex = currentImageIndex;
            return YES;
        }
        else
        {
            return NO;    
        }
    }
    else
    {
        return found;
    }
}
- (void) doActionAfterCheck: (NSArray*) data
{
    UserInfo* userInfo = [self getUserInfo];
    NSString* currentImageList = userInfo.currentimagelist;
    SESelectedImageCheckData* checkData = [data objectAtIndex:0];
    NSMutableArray* imageURLArray = [NSMutableArray array];
    for(int i = 0 ; i < checkData.photoUrlArray.count ; i++)
    {
        SelectedImage* si = [checkData.photoUrlArray objectAtIndex:i];
        NSLog(@"do action si size = %d, %d", [si.width intValue] , [si.height intValue]);
        BOOL exist = [[checkData.photoExistsArray objectAtIndex:i] boolValue];
        if(exist)
        {
            [imageURLArray addObject:si];
        }
    }
    int sequenceMode = [userInfo.imageplaymode intValue];
    BOOL useDefaultImage = NO;
    BOOL imageArrayChanged = NO;
    //we will use default selected image
    if(imageURLArray.count == 0)
    {
        [self showIndication:@"Not find image, show default image" justInMainDisplay:YES time:12];
        imageURLArray = [self fetchDefaultSelectedImage];
        useDefaultImage = YES;
    }
    int oldImageArrayCount = mPainterManager.imageArray.count;
    mPainterManager.imageArray = imageURLArray;
    if([mDrawingImageList isEqualToString:currentImageList])
    {
        if(oldImageArrayCount != imageURLArray.count)
        {
            NSLog(@"## warning: image array count changed ##");
            if(mPainterManager.currentImageIndex >= imageURLArray.count)
                mPainterManager.currentImageIndex = 0;
            imageArrayChanged = YES;
        }
        else
        {
            //maybe the image sequence has change
            imageArrayChanged = NO;
        }
        
    }
    else
    {
        mPainterManager.currentImageIndex = 0;
        if(oldImageArrayCount == 0 && sequenceMode == 1)
        {
            imageArrayChanged = NO;
        }
        else
        {
            imageArrayChanged = YES;
        }
    }
    if(mMoveFromOtherView)
    {
        imageArrayChanged = YES;
        mMoveFromOtherView = NO;
    }
    self.mDrawingImageList = currentImageList;
    if([imageURLArray count] > 0)
    {
        int quality = [self getImageQuality];
        int times = [self getImageTimes];
        [mPainterManager initPainterState:quality withTimes:times];
        if(useDefaultImage)
        {
            mPainterManager.currentImageIndex = 0;
            [mPainterManager nextDisplayStage];
        }
        else 
        {
            int imageIndex = 0;
            BOOL found = [self findNextProperImageIndex:[mPainterManager currentImageIndex] :imageURLArray : imageArrayChanged: &imageIndex];
            if(found)
            {
                mPainterManager.currentImageIndex = imageIndex;
                [self hideIndication];
            }
            else
            {
                mPainterManager.currentImageIndex = 0;
                imageURLArray = [self fetchDefaultSelectedImage];
                mPainterManager.imageArray = imageURLArray;
                [self showIndication:@"Not find proper image, show default image. Please rotate your screen or select the proper size image." justInMainDisplay:YES time:12];
            }
            [mPainterManager nextDisplayStage];
        }
    }

}
- (void) doCheck: (NSMutableArray*)data
{
    SESelectedImageCheckData* checkData = [data objectAtIndex:0];
    int currentIndex = [[data objectAtIndex:1] intValue];
    ALAssetsLibrary* lib = checkData.assetLib;
    if(currentIndex >= checkData.photoUrlArray.count)
    {
        [self doActionAfterCheck: data];
        [data release];
        return;
    }
    SelectedImage* si = [checkData.photoUrlArray objectAtIndex:currentIndex]; //[[checkData.photoUrlArray objectAtIndex:currentIndex] url];
    NSString* urlStr = si.url;
    NSURL* url = [[NSURL URLWithString:urlStr] retain];
    
    void (^handleAsset)(BOOL) = ^(BOOL isExist){
        [checkData.photoExistsArray replaceObjectAtIndex:currentIndex withObject:[NSNumber numberWithBool:isExist]];
        if(!isExist)
        {
            NSLog(@"url [%@] is not exist", url);
        }
        int newIndex = currentIndex + 1;
        [url release];
        [data replaceObjectAtIndex:1 withObject:[NSNumber numberWithInt:newIndex]];
        [self performSelectorOnMainThread:@selector(doCheck:) withObject:data waitUntilDone:NO];
    };
    ALAssetsLibraryAssetForURLResultBlock result = ^(ALAsset* asset)
    {
        BOOL isExist = NO;
        if(asset != NULL)
        {
            isExist = YES;
        }
        handleAsset(isExist);
    };
    ALAssetsLibraryAccessFailureBlock fail = ^(NSError* error)
    {
        NSLog(@"error");
        handleAsset(NO);    
    };
    [lib assetForURL:url resultBlock:result failureBlock:fail];
}
- (void) checkAllSelectedImage: (NSArray*) selectedImageArray
{
    NSMutableArray* data = [[NSMutableArray array] retain];
    SESelectedImageCheckData* checkData = [[SESelectedImageCheckData alloc] init];
    checkData.photoUrlArray = selectedImageArray;
    NSMutableArray* exits = [NSMutableArray array];
    for(int i = 0 ; i < selectedImageArray.count ; i++)
    {
        [exits addObject:[NSNumber numberWithBool:YES]];
    }
    checkData.photoExistsArray = exits;
    ALAssetsLibrary* lib = [[ALAssetsLibrary alloc] init];
    checkData.assetLib = lib;
    [lib release];
    [data addObject:checkData];
    [checkData release];
    [data addObject:[NSNumber numberWithInt:0]];
    [self doCheck:data];
}

- (void)displayNextImage
{
    UserInfo* userInfo = [self getUserInfo];
    NSString* currentImageList = userInfo.currentimagelist;
    NSArray* imagePropertyArray = [self getSelectedImageArrayByName:currentImageList];
    NSArray* imageURLArray = [NSArray array];
    for(SelectedImage* si in imagePropertyArray)
    {
        NSLog(@"si url = %@", si.url);
        NSLog(@"si. width = %@, %@", si.width, si.height);
        if(si.url != nil && [si.width intValue] > 0 && [si.height intValue] > 0)
        {
            imageURLArray = [imageURLArray arrayByAddingObject:si];
        }
    }
    [self checkAllSelectedImage:imageURLArray];
    return;
    /*
    int sequenceMode = [userInfo.imageplaymode intValue];
    BOOL useDefaultImage = NO;
    BOOL imageArrayChanged = NO;
    //we will use default selected image
    if(imageURLArray.count == 0)
    {
        [self showIndication:@"Not Find Image, it will show default image" justInMainDisplay:YES time:2];
        imageURLArray = [self fetchDefaultSelectedImage];
        useDefaultImage = YES;
    }
    int oldImageArrayCount = mPainterManager.imageArray.count;
    mPainterManager.imageArray = imageURLArray;
    if([mDrawingImageList isEqualToString:currentImageList])
    {
        if(oldImageArrayCount != imageURLArray.count)
        {
            NSLog(@"## warning: image array count changed ##");
            if(mPainterManager.currentImageIndex >= imageURLArray.count)
                mPainterManager.currentImageIndex = 0;
            imageArrayChanged = YES;
        }
        else
        {
            //maybe the image sequence has change
            imageArrayChanged = NO;
        }
        
    }
    else
    {
        mPainterManager.currentImageIndex = 0;
        if(oldImageArrayCount == 0 && sequenceMode == 1)
        {
            imageArrayChanged = NO;
        }
        else
        {
            imageArrayChanged = YES;
        }
    }
    if(mMoveFromOtherView)
    {
        imageArrayChanged = YES;
        mMoveFromOtherView = NO;
    }
    self.mDrawingImageList = currentImageList;
    if([imageURLArray count] > 0)
    {
        int quality = [self getImageQuality];
        int times = [self getImageTimes];
        [mPainterManager initPainterState:quality withTimes:times];
        if(useDefaultImage)
        {
            mPainterManager.currentImageIndex = 0;
            [mPainterManager nextDisplayStage];
        }
        else 
        {
            int imageIndex = 0;
            BOOL found = [self findNextProperImageIndex:[mPainterManager currentImageIndex] :imageURLArray : imageArrayChanged: &imageIndex];
            if(found)
            {
                mPainterManager.currentImageIndex = imageIndex;
            }
            else
            {
                mPainterManager.currentImageIndex = 0;
                imageURLArray = [self fetchDefaultSelectedImage];
                mPainterManager.imageArray = imageURLArray;
                [self showIndication:@"Not Find Proper Image, it will show default image" justInMainDisplay:YES time:2];
            }
            [mPainterManager nextDisplayStage];
        }
    }
     */
}
- (void) handleAnimEnd:(BOOL)stopInMid withDirection:(int)direct leftContainer:(SEContentViewContainer *)leftContainer rightContainer:(SEContentViewContainer *)rightContianer
{
    SEPageUIScrollView* leftScrollView = nil;
    if(leftContainer.mType == IMAGE_PICKER || leftContainer.mType == SELECTED_IMAGE_VIEW)
    {
        leftScrollView = (SEPageUIScrollView*)[leftContainer contentView];
    }
    
    SEPageUIScrollView* rightScrollView = nil;
    if(rightContianer.mType == IMAGE_PICKER || rightContianer.mType == SELECTED_IMAGE_VIEW)
    {
        rightScrollView =  (SEPageUIScrollView*)[rightContianer contentView];
    }
    if(stopInMid)
    {
        if([leftScrollView isStopLoadImage])
        {
            [leftScrollView startLoadImage];
        }
        if([rightScrollView isStopLoadImage])
        {
            [rightScrollView startLoadImage];
        }
        return;
    }
    if(direct == MOVE_LEFT)
    {
        [leftScrollView stopLoadImage];
    }
    else if(direct == MOVE_RIGHT)
    {
        [rightScrollView stopLoadImage];
    }
    [self saveContext];
}

- (void) moveToView:(VIEW_SEQ_TYPE)vst : (VIEW_TYPE)vp hasAnimation:(BOOL)isAnim isPush:(BOOL) bPush
{
    if(bPush == YES)
    {
        [self pushViewSeqType:mCurrentViewSeqType];
    }
    mCurrentViewSeqType = vst;
    [self setCurrentViewSequenceFrame];
    [self setCurrentContentContainerFrame];
    [self initContentContainerParent];
    mPrevCurrView = mCurrView;
    mCurrView = INVALID_VIEW;
    [self setCurrentView:vp isAnimation:isAnim];
    
}
- (VIEW_SEQ_TYPE) popViewSeqType
{
    NSNumber* v = [mViewSeqTypeStack lastObject];
    VIEW_SEQ_TYPE ret =  (VIEW_SEQ_TYPE)[v intValue];
    [mViewSeqTypeStack removeObject:v];
    mCurrentViewSeqType = ret;
    return ret;
}
- (void) pushViewSeqType:(VIEW_SEQ_TYPE)vst
{
    [mViewSeqTypeStack addObject:[NSNumber numberWithInt:vst]];
}
- (NSManagedObject*)newObjectByEntityName: (NSString*) entityName
{
    NSEntityDescription* entity = [NSEntityDescription entityForName:entityName inManagedObjectContext:self.managedObjectContext];
    NSManagedObject* obj = [NSEntityDescription insertNewObjectForEntityForName:[entity name] inManagedObjectContext:self.managedObjectContext];
    return obj;

}
- (Signature*) getCurrentSignature
{
    UserInfo* userInfo = [self getUserInfo];
    NSNumber* sigSeq = userInfo.currentsignature;
    NSSet* signatureList = userInfo.signaturelist;
    Signature* signature = nil;
    NSEnumerator* sigIt = [signatureList objectEnumerator];
    while((signature = [sigIt nextObject]) != nil)
    {
        NSNumber* seq = [signature valueForKey:@"seq"];
        if([seq isEqualToNumber:sigSeq])
        {
            return signature;
        }
    }
    return nil;
}
- (NSArray*) getAllSignatures
{
    
    NSArray* signatureArray = [NSArray array];
    UserInfo* userInfo = [self getUserInfo];
    NSSet* signatureList = userInfo.signaturelist;
    Signature* signature = nil;
    NSEnumerator* sigIt = [signatureList objectEnumerator];
    while((signature = [sigIt nextObject]) != nil)
    {
        signatureArray = [signatureArray arrayByAddingObject:signature];
    }
    signatureArray = [signatureArray sortedArrayUsingComparator:^NSComparisonResult(id obj1, id obj2) 
    {
        Signature* s1 = (Signature*)obj1;
        Signature* s2 = (Signature*) obj2;
        NSNumber* seq1 = s1.seq;
        NSNumber* seq2 = s2.seq;
        return [seq1 compare:seq2];
    }];
    return signatureArray;
}
- (NSArray*) getMusicAttachedImage: (NSString*)name
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* musicListSet = userInfo.musiclist;
    MusicList* musicList = nil;
    NSEnumerator* it = [musicListSet objectEnumerator];
    while ((musicList = [it nextObject]) != nil)
    {
        NSString* musicListName = musicList.name;
        if([musicListName isEqualToString:name])
        {
            NSSet* imageSet = musicList.attachimagelist;
            NSArray* imageArray = [imageSet allObjects];
            imageArray = [imageArray sortedArrayUsingComparator:^NSComparisonResult(id obj1, id obj2) {
                ImageList* list1 = (ImageList*)obj1;
                ImageList* list2 = (ImageList*)obj2;
                return [list1.seq compare:list2.seq];
            }];
            return imageArray;
        }
    }
    return nil;
}
- (void) attachImageToMusic: (NSString*)musicListName imageListName:(NSString*) imageListName
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* musicListSet = userInfo.musiclist;
    MusicList* ml = nil;
    NSEnumerator* itMusicList = [musicListSet objectEnumerator];
    while((ml = [itMusicList nextObject]) != nil)
    {
        if([ml.name isEqualToString:musicListName])
        {
            NSMutableSet* attachImageSet = (NSMutableSet*)ml.attachimagelist;
            for(ImageList* i in attachImageSet)
            {
                if([i.name isEqualToString:imageListName])
                    return;
            }
            ImageList* imageList = [self getImageListByName:imageListName];
            [ml addAttachimagelistObject:imageList];
        }
    }
}
- (ImageList*) getImageListByName: (NSString*)imageListName
{
    UserInfo* userInfo = [self getUserInfo];
    
    NSSet* imageListSet = userInfo.imagelist;
    ImageList* imageList = nil;
    NSEnumerator* it = [imageListSet objectEnumerator];
    while((imageList = [it nextObject]) != nil)
    {
        if([imageList.name isEqualToString:imageListName])
            return imageList;
    }
    return nil;
}
- (MusicList*) getMusicListByName: (NSString*)musicListName
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* musicListSet = userInfo.musiclist;
    NSEnumerator* it = [musicListSet objectEnumerator];
    MusicList* musicList = nil;
    while((musicList = [it nextObject]) != nil)
    {
        if([musicList.name isEqualToString:musicListName])
            return musicList;
    }
    return nil;
}

- (MusicList*) addMusicList:(NSString*) musicListName
{
    MusicList* ml = [self getMusicListByName:musicListName];
    if(ml)
    {
        return ml;
    }
    
    ml = (MusicList*)[self newObjectByEntityName:@"MusicList"];
    ml.name = musicListName;
    NSArray* musicListArray = [self getAllMusicList];
    NSLog(@"music list num = %d", musicListArray.count);
    if(musicListArray)
    {
        MusicList* lastObj = [musicListArray lastObject];
        if(lastObj == nil)
        {
            ml.seq = [NSNumber numberWithInt:0];
        }
        else
        {
            NSNumber* seq = [NSNumber numberWithInt:[lastObj.seq intValue] + 1];
            ml.seq = seq;
        }
    }
    else
        ml.seq = [NSNumber numberWithInt:0];
    UserInfo* userInfo = [self getUserInfo];
    [userInfo addMusiclistObject:ml];
    [self printAllMusicList];
    return ml;
}
- (ImageList*) addImageList: (NSString*) imageListName
{
    ImageList* il = [self getImageListByName:imageListName];
    if(il)
        return il;
    il = (ImageList*)[self newObjectByEntityName:@"ImageList"];
    il.name = imageListName;
    NSArray* imageListArray = [self getAllImageList];
    if(imageListArray)
    {
        ImageList* lastObj = [imageListArray lastObject];
        if(lastObj == nil)
        {
            il.seq = [NSNumber numberWithInt:0];
        }
        else
        {
            NSNumber* seq = [NSNumber numberWithInt:[lastObj.seq intValue] + 1];
            il.seq = seq;
        }
    }
    else
        il.seq = [NSNumber numberWithInt:0];
    UserInfo* userInfo = [self getUserInfo];
    [userInfo addImagelistObject:il];
    //NSMutableSet* imageListSet = (NSMutableSet*)userInfo.imagelist;
    //[imageListSet addObject:il];
    [self printAllImageList];
    return il;
}
- (MusicList*) getMusicListByImageList: (NSString*)imageListName
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* musicListSet = userInfo.musiclist;
    NSEnumerator* itMusicList = [musicListSet objectEnumerator];
    MusicList* ml = nil;
    while((ml = [itMusicList nextObject]) != nil)
    {
        NSSet* attachImage = ml.attachimagelist;
        NSEnumerator* itImageList = [attachImage objectEnumerator];
        ImageList* imageList = nil;
        while((imageList = [itImageList nextObject]) != nil)
        {
            if([imageList.name isEqualToString:imageListName])
                return ml;
        }
    }
    return nil;
}
- (NSArray*) getAttachedMusicListByImageListName: (NSString*)imageListName
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* musicListSet = userInfo.musiclist;
    MusicList* ml = nil;
    NSEnumerator* it = [musicListSet objectEnumerator];
    NSArray* array = [NSArray array];
    while((ml = [it nextObject]) != nil)
    {
        NSSet* attachImageList = ml.attachimagelist;
        NSEnumerator* itImageList = [attachImageList objectEnumerator];
        ImageList* imageList = nil;
        while((imageList = [itImageList nextObject]) != nil)
        {
            if([imageList.name isEqualToString:imageListName])
            {
                array = [array arrayByAddingObject:ml];
            }
        }
    }
    return array;
    
}
- (void) removeAttachedImageListFromMusicList: (NSString*) imageListName
{
    MusicList* ml = [self getMusicListByImageList:imageListName];
    if(ml)
    {
        ImageList* il = [self getImageListByName:imageListName];
        [ml removeAttachimagelistObject:il];
    }
}
- (void) removeImageListByName : (NSString*)imageListName
{
    ImageList* il = [self getImageListByName:imageListName];
    if(il == nil)
        return;
    UserInfo* userInfo = [self getUserInfo];
    NSArray* musicListArray = [self getAttachedMusicListByImageListName:imageListName];
    if(musicListArray)
    {
        for(MusicList* ml in musicListArray)
        {
            [ml removeAttachimagelistObject:il];
        }
    }
    [userInfo removeImagelistObject:il];
    NSArray* imageListArray = [self getAllImageList];
    for(int i = 0 ; i < imageListArray.count ; i++)
    {
        ImageList* mil = [imageListArray objectAtIndex:i];
        mil.seq = [NSNumber numberWithInt:i];
    }
}
- (void) printAllImageList
{
    NSArray* listArray = [self getAllImageList];
    for(int i = 0 ; i < listArray.count ; i++)
    {
        ImageList* il = [listArray objectAtIndex:i];
        NSLog(@"image list seq = %d , name = %@", [il.seq intValue], il.name);
    }
}
- (void) printAllMusicList
{
    //test
    NSArray* testMusicList = [self getAllMusicList];
    for(int i = 0 ; i < testMusicList.count ;i++)
    {
        MusicList* il = [testMusicList objectAtIndex:i];
        NSLog(@"music list seq = %d , name = %@", [il.seq intValue], il.name);
    }
    //end
}
- (void) removeMusicListByName : (NSString*)musicListName
{
    UserInfo* userInfo = [self getUserInfo];
    MusicList* musicList = [self getMusicListByName:musicListName];
    if(musicList)
    {
        NSNumber* seq = musicList.seq;
        [userInfo removeMusiclistObject:musicList];
        NSArray* allMusicList = [self getAllMusicList];
        //for(MusicList* ml in allMusicList)
        for(int i = 0 ; i < allMusicList.count ; i++)
        {
            MusicList* ml = [allMusicList objectAtIndex:i];
            ml.seq = [NSNumber numberWithInt: i];
        }
    }
    [self printAllMusicList];

}
- (void) removeAllAttachedImageFromMusicList: (NSString*)musicListName
{
    MusicList* musicList = [self getMusicListByName:musicListName];
    if(musicList)
    {
        NSArray* array = [musicList.attachimagelist allObjects];//[self getMusicAttachedImage:musicListName];
        for(ImageList* il in array)
        {
            [musicList removeAttachimagelistObject:il];
        }
        assert(musicList.attachimagelist.count == 0);
    }
    //[self printAllMusicList];
    //[self printAllImageList];
}
- (ImageList*) getImageListBySeq:(NSNumber*)seq
{
    UserInfo* userInfo = [self getUserInfo];
    
    NSSet* imageListSet = userInfo.imagelist;
    ImageList* imageList = nil;
    NSEnumerator* it = [imageListSet objectEnumerator];
    while((imageList = [it nextObject]) != nil)
    {
        if([imageList.seq isEqualToNumber:seq])
            return imageList;
    }
    return nil;    
}
- (MusicList*) getMusicListBySeq: (NSNumber*)seq
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* musicListSet = userInfo.musiclist;
    NSEnumerator* it = [musicListSet objectEnumerator];
    MusicList* musicList = nil;
    while((musicList = [it nextObject]) != nil)
    {
        if([musicList.seq isEqualToNumber:seq])
            return musicList;
    }
    return nil;
}
- (BOOL) musicListContainImageList: (MusicList*)musicList :(NSString*) imageListName
{
    NSSet* attachSet = musicList.attachimagelist;
    for(ImageList* il in attachSet)
    {
        if([il.name isEqualToString:imageListName])
            return YES;
    }
    return NO;
}
/*
- (int) getMaxSelectedImageCount
{
    UserInfo* userInfo = [self getUserInfo];
    int level = [mUserUpgradeInfo getLevelFromExpNum:[userInfo.exppointnum intValue]] ;
    SEUserData* userData = [mUserUpgradeInfo getUserData:level];
    if(userData)
    {
        return userData.imageListNum;    
    }
    else
        return 0;
}
 */
- (int) getMaxSelectedMusicCount
{
    UserInfo* userInfo = [self getUserInfo];
    int level = [mUserUpgradeInfo getLevelFromExpNum:[self.mSystemDataManager.exppointnum intValue]] ;
    SEUserData* userData = [mUserUpgradeInfo getUserData:level];
    if(userData)
    {
        return userData.musicListNum;
    }
    else
        return 0;
}
- (int) getCurrentAllSelectedImageCount
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* imageList = userInfo.imagelist;
    int count = 0;
    for(ImageList* il in imageList)
    {
        NSSet* siset = il.selectedimage;
        for(SelectedImage* si in siset)
        {
            if(si.url != nil || si.filepath != nil)
                count++;
        }
    }
    return count;
}
- (int) getCurrentAllSelectedMusicCount
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* musicList = userInfo.musiclist;
    int count = 0;
    for(MusicList* ml in musicList)
    {
        NSSet* smset = ml.selectedmusic;
        for(SelectedMusic* sm in smset)
        {
            if(sm.title != nil)
                count++;
        }
    }
    return count;    
}
///////////////////////
- (void) clearPlacedView
{
    //mPlacedView = nil;
    mPlacedViewIndex = SE_INVALID_IMAGE_INDEX;
}
- (void) setViewRelationType:(VIEW_RELATION_TYPE) type
{
    gViewRelationType = type;
}
- (void) longPressBegin:(CGPoint)p scrollView:(SEPageUIScrollView *)scrollView
{
    if(scrollView == [mViewArray[IMAGE_PICKER] contentView])
    {
        SEPageHitProperty hp = [scrollView hitRect:p];
        if(hp.imageView && [hp.imageView isDefaultImage] == NO)
        {
            mCurrentFloatViewType = FLOATVIEW_IMAGE;
            NSArray* operators = [NSArray arrayWithObjects:@"share_op",  nil];
            NSArray* shareImamges = [NSArray arrayWithObjects:@"OperationViewShareBackgroiund", @"OperationViewShareForeground", @"OperationViewShareBackgroundH", @"OperationViewShareForegroundH",nil];
            NSArray* resources = [NSArray arrayWithObjects:shareImamges, nil];
            SEOperationViewGroup* operationViewGroup = [[[SEOperationViewGroup alloc] initWithOperators: operators withImageResource:resources] autorelease];
            SEShareImageOperationViewHandler* handler = [[[SEShareImageOperationViewHandler alloc] init] autorelease];
            operationViewGroup.mOperationHandler = handler;
            [self popupOperationViewGroup: operationViewGroup];
            [self createFloatView:hp scrollView:scrollView];
            
        }
    }
    else if(scrollView == [mViewArray[SELECTED_IMAGE_VIEW] contentView])
    {
        SEPageHitProperty hp = [scrollView hitRect:p];
        if(hp.imageView && [hp.imageView isDefaultImage] == NO)
        {
            mCurrentFloatViewType = FLOATVIEW_IMAGE;
            NSArray* operators = [NSArray arrayWithObjects:@"share_op", @"delete_op", nil];
            NSArray* shareImamges = [NSArray arrayWithObjects:@"OperationViewShareBackgroiund", @"OperationViewShareForeground", @"OperationViewShareBackgroundH", @"OperationViewShareForegroundH",nil];
            NSArray* deleteImages = [NSArray arrayWithObjects:@"OperationViewDeleteBackground", @"OperationViewDeleteForeground", @"OperationViewDeleteBackgroundH", @"OperationViewDeleteForegroundH", nil];
            NSArray* resources = [NSArray arrayWithObjects:shareImamges, deleteImages, nil];
            SEOperationViewGroup* operationViewGroup = [[[SEOperationViewGroup alloc] initWithOperators: operators withImageResource:resources] autorelease];
            SEShareDeleteImageOperationViewHandler* handler = [[[SEShareDeleteImageOperationViewHandler alloc] init] autorelease];
            operationViewGroup.mOperationHandler = handler;
            [self popupOperationViewGroup:operationViewGroup];
            [self createFloatView:hp scrollView:scrollView];
        }
    }
}
- (void) longPressMove:(CGPoint)p scrollView:(SEPageUIScrollView *)scrollView
{
    if(scrollView == [mViewArray[IMAGE_PICKER] contentView])
    {
        if([self isFloatViewShow])
        {
            CGPoint currP = [scrollView convertPoint:p toView:mRootView];
            [self moveFloatViewToPoint:currP];
        }
    }
    else if(scrollView == [mViewArray[SELECTED_IMAGE_VIEW] contentView])
    {
        if([self isFloatViewShow])
        {
            CGPoint currP = [scrollView convertPoint:p toView:mRootView];
            [self moveFloatViewToPoint:currP];
        }
    }
}
- (void) clearRootView
{
    for(UIView* v in mRootView.subviews)
    {
        if([v isMemberOfClass:[SEPageUIImageView class]])
        {
            [v removeFromSuperview];
        }
    }
    mCurrentPageUIImageView.frame = mCurrentPageUIImageView.mOriginRect;
    mCurrentPageUIImageView = nil;
}
- (void) setSelectedImageNumToStatusLabel: (SEPageUIScrollView *)scrollView
{
    if(scrollView == [mViewArray[IMAGE_PICKER] contentView])
    {
        SEContentViewContainer* pickerContainer = mViewArray[IMAGE_PICKER];
        SEPageUIScrollView* pickerScrollView = (SEPageUIScrollView*)pickerContainer.contentView;
        NSArray* highlighted = [pickerScrollView getHighlightedImageView];
        if(highlighted.count > 0)
        {
            NSString* str = [NSString stringWithFormat:@"%d", highlighted.count];
            [pickerContainer setStatusText:str];
        }
        else {
            [pickerContainer setStatusText:@""];
        }
    }
    else if(scrollView == [mViewArray[SELECTED_IMAGE_VIEW] contentView])
    {
        SEContentViewContainer* selectedContainer = mViewArray[SELECTED_IMAGE_VIEW];
        SEPageUIScrollView* selectedScrollView = (SEPageUIScrollView*)selectedContainer.contentView;
        NSArray* highlited = [selectedScrollView getHighlightedImageView];
        if(highlited.count > 0)
        {
            NSString* str = [NSString stringWithFormat:@"%d", highlited.count];
            [selectedContainer setStatusText:str];
        }
        else 
        {
            [selectedContainer setStatusText:@""];
        }
    }
}
- (void) longPressEnd:(CGPoint)p scrollView:(SEPageUIScrollView *)scrollView
{
    [self clearRootView];
    if(scrollView == [mViewArray[IMAGE_PICKER] contentView])
    {
        if(mPlacedViewIndex != SE_INVALID_IMAGE_INDEX)
        {
            SEContentViewContainer* selectedContainer = mViewArray[SELECTED_IMAGE_VIEW];
            SEPageUIScrollView* selectedScrollView = (SEPageUIScrollView*)selectedContainer.contentView;
            [self addImageToSelectedImageScrollView:mSelectedPhotoURL : mSelectedImage];
            SEPageUIImageView* imageView = [selectedScrollView imageView:mPlacedViewIndex];
            [self normalizePlacedView:imageView];
            mPlacedViewIndex = SE_INVALID_IMAGE_INDEX;
            for(NSNumber* n in mSelectedImageViewIndex)
            {
                int index = [n intValue];
                SEPageUIImageView* imageView = [scrollView imageView:index];
                imageView.highlighted = NO;
            }
        }
        mOperationContainer = mViewArray[IMAGE_PICKER];
    }
    else if(scrollView == [mViewArray[SELECTED_IMAGE_VIEW] contentView])
    {
        if(mPlacedViewIndex != SE_INVALID_IMAGE_INDEX)
        {
            SEPageUIImageView* imageView = [scrollView imageView:mPlacedViewIndex];
            [self normalizePlacedView:imageView];
            [scrollView changeImageView: mSelectedImageViewIndex toPos: mPlacedViewIndex];
            for(NSNumber* n in mSelectedImageViewIndex)
            {
                int index = [n intValue];
                SEPageUIImageView* imageView = [scrollView imageView:index];
                imageView.highlighted = NO;
            }
            mPlacedViewIndex = SE_INVALID_IMAGE_INDEX;
        }
        mOperationContainer = mViewArray[SELECTED_IMAGE_VIEW];
        
    }
    if(mFloatView)
    {
        [mFloatView removeFromSuperview];
        mFloatView = nil;
        [scrollView enableAllGestures];
    }
    [self disappearOperationViewGroup];
    self.mSelectedImageViewIndex = nil;
    self.mSelectedPhotoURL = nil;
    self.mSelectedImage = nil;
    mOperationContainer = nil;
    mCurrentFloatViewType = NO_FLOATVIEW;
    [self setSelectedImageNumToStatusLabel : scrollView];
}
- (void) setImagePickerOrSelectedStatus: (SEPageUIScrollView *)scrollView
{
    if(scrollView.mScrollViewType == PHOTOLIB_SCROLLVIEW)
    {
        SEContentViewContainer* c = mViewArray[IMAGE_PICKER];
        FontLabel* statusLabel = [c getStatusLabel];
        NSArray* ha = [scrollView getHighlightedImageView];
        if(ha.count > 0)
        {
            statusLabel.text = [NSString stringWithFormat:@"%d", ha.count];
        }
        else {
            statusLabel.text = @"";
        }
    }
    else
    {
        SEContentViewContainer* c = mViewArray[SELECTED_IMAGE_VIEW];
        FontLabel* statusLabel = [c getStatusLabel];
        NSArray* ha = [scrollView getHighlightedImageView];
        if(ha.count > 0)
        {
            statusLabel.text = [NSString stringWithFormat:@"%d", ha.count];
        }
        else
        {
            statusLabel.text = @"";
        }
    }
}
- (void) clicked:(CGPoint)p scrollView:(SEPageUIScrollView *)scrollView
{
    SEPageHitProperty hp = [scrollView hitRect:p];
    if(hp.imageView)
    {
        if(hp.imageView.highlighted == NO)
        {
            hp.imageView.highlighted = YES;    
            [self setImagePickerOrSelectedStatus:scrollView];
        }
        else
        {
            hp.imageView.highlighted = NO;
            [self setImagePickerOrSelectedStatus:scrollView];
        }
    }
}
- (void) pressed:(CGPoint)p scrollView:(SEPageUIScrollView *)scrollView

{
    SEPageHitProperty hp = [scrollView hitRect:p];
    if(hp.imageView)
    {

    }
}
- (void) setSelectedMusicToCoreData: (NSArray*)musicArray
{
    //UserInfo* userInfo = [self getUserInfo];
    //MusicList* musicList = [self getMusicListByName:mCurrentLoadedMusicListName];//[self getMusicListByName:userInfo.currentmusiclist];
    [self setSelectedMusicToCoreData:musicArray musicListName:mCurrentLoadedMusicListName];
    /*
    NSSet* selectedMusic = musicList.selectedmusic;
    [musicList removeSelectedmusic:selectedMusic];
    for(int i = 0 ; i < musicArray.count ; i++)
    {
        SEMusicItemProperty* item = [musicArray objectAtIndex:i];
        SelectedMusic* sm = (SelectedMusic*)[self newObjectByEntityName:@"SelectedMusic"];
        sm.seq = [NSNumber numberWithInt:i];
        sm.title = item.title;
        sm.album = item.album;
        sm.singer = item.artist;
        [musicList addSelectedmusicObject:sm];
    }
    [self saveCoreDataContext];
     */
}
- (void)setPhotoURLToCoreData: (NSMutableArray*)photoURLArray
{
    //UserInfo* userInfo = [self getUserInfo];
    //NSArray* selectedImageArray = [self getSelectedImageArrayByName:mCurrentLoadedImageListName];
    [self setPhotoURLToCoreData:photoURLArray imageListName:mCurrentLoadedImageListName];
}
- (BOOL) operationEnable: (int)index
{
    return mOperationEnable[index];
}
- (SEPopupView*) getPopupView
{
    return mPopupView;
}
- (void) popupView:(UIView*)v
{
    mPopupView = [[SEPopupView alloc] initWithFrame:CGRectMake(0, 0, mViewPortWidth, mViewPortHeight)];
    mPopupView.mContentView = v;
    [mPopupView showAt:CGPointMake(0, 0) parent:mRootView];
}
- (void) dismissPopup
{
    [mPopupView dismiss];
    [mPopupView release];
    mPopupView = nil;
}
- (NSArray*) fetchFinishedImage: (NSString*)url urlDate: (NSString*)date managedObjectContext: (NSManagedObjectContext*)moc
{
    NSFetchRequest* fetchReq = [[NSFetchRequest alloc] init];
    NSEntityDescription* entity = [NSEntityDescription entityForName:@"FinishedImage" inManagedObjectContext:moc];
    [fetchReq setEntity:entity];
    NSPredicate* predicate = [NSPredicate predicateWithFormat:@"(url == %@) AND (urldate == %@)", url, date];
    //NSPredicate* predicate = [NSPredicate predicateWithFormat:@"url == %@", url];
    
    [fetchReq setPredicate:predicate];
    NSError* error = nil;
    NSArray* data = [moc executeFetchRequest:fetchReq error:&error];
    if(error)
    {
        NSLog(@"Unresolved error when init data : %@", [error userInfo]);
        abort();
    }
    [fetchReq release];
    return data;
}
- (NSArray*) fetchFinishedImage: (NSString*)url urlDate: (NSString*)date
{
    return [self fetchFinishedImage:url  urlDate:date managedObjectContext:self.managedObjectContext];
    /*
    NSFetchRequest* fetchReq = [[NSFetchRequest alloc] init];
    NSEntityDescription* entity = [NSEntityDescription entityForName:@"FinishedImage" inManagedObjectContext:self.managedObjectContext];
    [fetchReq setEntity:entity];
    NSPredicate* predicate = [NSPredicate predicateWithFormat:@"(url == %@) AND (urldate == %@)", url, date];
    //NSPredicate* predicate = [NSPredicate predicateWithFormat:@"url == %@", url];

    [fetchReq setPredicate:predicate];
    NSError* error = nil;
    NSArray* data = [self.managedObjectContext executeFetchRequest:fetchReq error:&error];
    if(error)
    {
        NSLog(@"Unresolved error when init data : %@", [error userInfo]);
        abort();
    }
    [fetchReq release];
    return data;
     */
}
- (void) removeImageFromCoreDate:(NSString*)url urlDate: (NSString*)date
{
    NSArray* data = [self fetchFinishedImage:url urlDate:date];
    assert(data.count == 1 || data.count == 0);
    if(data.count > 0)
    {
        FinishedImage* fi = [data objectAtIndex:0];
        [self.managedObjectContext deleteObject:fi];
    }
}
- (void) saveImageAndThumbnailToCoreData:(UIImage*)uiImage urlName:(NSString*)url urlDate: (NSString*)date orientation: (int)orientation index:(int)index
{
    NSArray* data = [self fetchFinishedImage:url urlDate:date];
    assert(data.count == 1 || data.count == 0);
    NSData* pngData = nil;
    NSData* thumbnailData = nil;
    //assert(orientation == UIImageOrientationUp);
    if(uiImage)
    {
        pngData = UIImagePNGRepresentation(uiImage);
        CGSize size = [self getThumbnailSize];
        UIImage* thumbnail = [SEUtil createThumbnail:uiImage thumbnailSize:size];
        UIImage* newthumb = [UIImage imageWithCGImage:[thumbnail CGImage] scale:thumbnail.scale orientation:(UIImageOrientation)orientation];
        thumbnailData = UIImagePNGRepresentation(newthumb);
    }
    
    if(data.count > 0)
    {
        FinishedImage* finishedImage = [data objectAtIndex:0];
        finishedImage.thumbnail = thumbnailData;
        finishedImage.data = pngData;
        finishedImage.orientation = [NSNumber numberWithInt: orientation];
    }
    else
    {
        NSManagedObject* obj = [self newObjectByEntityName:@"FinishedImage"];
        [obj setValue:url forKey:@"url"];
        [obj setValue:date forKey:@"urldate"];
        [obj setValue:thumbnailData forKey:@"thumbnail"];
        [obj setValue:pngData forKey:@"data"];
        [obj setValue:[NSNumber numberWithInt:orientation] forKey:@"orientation"];
    }
    [self saveCoreDataContext];    
}
- (void) saveImageThumbnailToCoreData:(UIImage*)uiImage urlName:(NSString*)url urlDate: (NSString*)date index:(int)index
{
    NSArray* data = [self fetchFinishedImage:url urlDate:date];
    assert(data.count == 1 || data.count == 0);
    NSData* pngData = nil;
    if(uiImage)
    {
        CGSize size = [self getThumbnailSize];
        UIImage* thumbnail = [SEUtil createThumbnail:uiImage thumbnailSize:size];
        pngData = UIImagePNGRepresentation(thumbnail);
    }
    
    if(data.count > 0)
    {
        FinishedImage* finishedImage = [data objectAtIndex:0];
        finishedImage.thumbnail = pngData;
    }
    else
    {
        NSManagedObject* obj = [self newObjectByEntityName:@"FinishedImage"];
        [obj setValue:url forKey:@"url"];
        [obj setValue:date forKey:@"urldate"];
        [obj setValue:pngData forKey:@"thumbnail"];
    }
    //use for test
    NSArray* paths = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES);
    NSString* docs = [paths objectAtIndex:0];
    NSString* strIndex = [NSString stringWithFormat:@"%d" , index];
    NSString* path = [docs stringByAppendingFormat:@"%@%@%@%@", @"/", @"ttestet_", strIndex, @".png"];
    const char* cpath = [path cStringUsingEncoding:NSASCIIStringEncoding];
    NSError* writeError = nil;
    BOOL ret = [pngData writeToFile:path options:NSDataWritingAtomic error:&writeError];
    if(ret == NO && writeError != nil)
    {
        NSLog(@" Error save image : %@", path);
    }
    //end
    [self saveCoreDataContext];
}
- (void) saveImageToCoreData: (UIImage*)uiImage urlName:(NSString*)url urlDate: (NSString*)date index:(int)index

{
    NSArray* data = [self fetchFinishedImage:url urlDate:date];
    assert(data.count == 1 || data.count == 0);
    NSData* pngData = nil;
    if(uiImage)
    {
        pngData = UIImagePNGRepresentation(uiImage);
    }
    /*
    //use for test
    NSArray* paths = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES);
    NSString* docs = [paths objectAtIndex:0];
    NSString* path = [docs stringByAppendingFormat:@"%@%@%@", @"/", @"ttestet", @".png"];
    const char* cpath = [path cStringUsingEncoding:NSASCIIStringEncoding];
    NSError* writeError = nil;
    BOOL ret = [pngData writeToFile:path options:NSDataWritingAtomic error:&writeError];
    if(ret == NO && writeError != nil)
    {
        NSLog(@" Error save image : %@", path);
    }
    //end
     */
    if(data.count > 0)
    {
        FinishedImage* finishedImage = [data objectAtIndex:0];
        finishedImage.data = pngData;
    }
    else
    {
        NSManagedObject* obj = [self newObjectByEntityName:@"FinishedImage"];
        [obj setValue:url forKey:@"url"];
        [obj setValue:date forKey:@"urldate"];
        [obj setValue:pngData forKey:@"data"];
    }
    [self saveCoreDataContext];
}
- (UIImage*) getThumbnailFromCoreData:(NSString*)url urlDate: (NSString*)date managedObjectContext: (NSManagedObjectContext*)moc

{
    if(url == nil || date == nil)
        return nil;
    if([SESystemConfig isDefaultSelectedImageURL:url])
    {
        UIImage* image = [SESystemConfig getDefaultSelectedImage];
        CGSize size = [self getThumbnailSize];
        UIImage* thumbnail = [SEUtil createThumbnail:image thumbnailSize:size];
        return thumbnail;
    }
    //NSLog(@"date = %@", date);
    //NSLog(@"url = %@", url);
    NSArray* data = [self fetchFinishedImage:url urlDate:date managedObjectContext:moc];
    //NSLog(@"thumb data = %@, %d", data, data.count);
    if(data == nil || data.count == 0)
        return nil;
    FinishedImage* fi = [data objectAtIndex:0];
    NSData* pngData = fi.thumbnail;
    UIImage* img = [UIImage imageWithData:pngData]; 
    UIImage* newImage = [UIImage imageWithCGImage:[img CGImage] scale:1.0 orientation:(UIImageOrientation)[fi.orientation intValue]];
    NSLog(@"# thumbnail orientation = %d ##", img.imageOrientation );
    return newImage;
}
- (UIImage*) getThumbnailFromCoreData:(NSString*)url urlDate: (NSString*)date
{
    return [self getThumbnailFromCoreData:url urlDate:date managedObjectContext:self.managedObjectContext];
}

- (UIImage*) getImageFromCoreData: (NSString*)url urlDate: (NSString*)date managedObjectContext: (NSManagedObjectContext*)moc
{
    if(url == nil || date == nil)
        return nil;
    //NSLog(@"date = %@", date);
    //NSLog(@"url = %@", url);
    NSArray* data = [self fetchFinishedImage:url urlDate:date managedObjectContext:moc];
    if(data == nil || data.count == 0)
        return nil;
    FinishedImage* fi = [data objectAtIndex:0];
    NSData* pngData = fi.data;
    if(pngData == nil)
        return nil;
    UIImageOrientation o = (UIImageOrientation)[fi.orientation intValue];
    NSLog(@"### image from core data orientation = %d ###", o);
    //return [UIImage imageWithData:pngData];
    UIImage* image = [UIImage imageWithData:pngData];
    UIImage* newImage = [UIImage imageWithCGImage:[image CGImage] scale:1.0 orientation:(UIImageOrientation)[fi.orientation intValue]];
    return newImage;
    //return image;
}
- (UIImage*) getImageFromCoreData: (NSString*)url urlDate: (NSString*)date
{
    if(url == nil || date == nil)
        return nil;
    //NSLog(@"date = %@", date);
    //NSLog(@"url = %@", url);
    NSArray* data = [self fetchFinishedImage:url urlDate:date];
    if(data == nil || data.count == 0)
        return nil;
    FinishedImage* fi = [data objectAtIndex:0];
    NSData* pngData = fi.data;
    //UIImageOrientation o = (UIImageOrientation)[fi.orientation intValue];
    return [UIImage imageWithData:pngData];
}
- (CGSize) getThumbnailSize
{
    return CGSizeMake(256, 256);
}
- (UIImage*) getImageFromPhotoLib : (NSString*)url urlDate: (NSString*)date
{
    return nil;
}
- (BOOL) isURLInPhotoURLs: (NSMutableArray*) photoURLs url:(NSURL*)url urlData : (NSString*)urlDate
{
    for(SEPageImageURL* imageURL in photoURLs)
    {
        if([imageURL.url isEqual:url] && [imageURL.urlDate isEqualToString:urlDate])
            return YES;
    }
    return NO;
}
- (void) setSelectedImageProperty: (NSString*)url urlDate: (NSString*)urlDate orientation: (int) orient image:(UIImage*)image
{
    SelectedImage* si =  [self getSelectedImageByUrl: url andDate:urlDate];
    si.orientation = [NSNumber numberWithInt:orient];
    si.width = [NSNumber numberWithInt: image.size.width];
    si.height = [NSNumber numberWithInt:image.size.height];
    NSLog(@"## %s, %d , width = %f, height = %f ##", __FUNCTION__, __LINE__, image.size.width, image.size.height);
}
- (void) removeSelectedImageNotInImageURLs:(NSMutableArray*)photoURLs
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* imageListSet = userInfo.imagelist;
    NSString* currentImageList = userInfo.currentimagelist;
    NSMutableArray* deleteSelectedImage = [NSMutableArray array];
    for(ImageList* il in imageListSet)
    {
        if([il.name isEqualToString:currentImageList])
        {
            NSSet* selectedImageSet = il.selectedimage;
            NSArray* newArray = [selectedImageSet allObjects];
            newArray = [newArray sortedArrayUsingComparator:^NSComparisonResult(id obj1, id obj2) {
                SelectedImage* mo1 = (SelectedImage*)obj1;
                SelectedImage* mo2 = (SelectedImage*)obj2;
                NSNumber* left = mo1.seq;
                NSNumber* right = mo2.seq;
                return [left compare:right];
            }];
            NSMutableArray* deleteArray = [NSMutableArray array];
            for(SelectedImage* si in newArray)
            {
                NSURL* url = [NSURL URLWithString:si.url];
                NSString* urldate = si.urldate;
                if(url != nil && [self isURLInPhotoURLs:photoURLs url:url urlData:urldate] == NO)
                {
                    [deleteArray addObject:si];
                    [deleteSelectedImage addObject:si];
                }
            }
            for(SelectedImage* si in deleteArray)
            {
                [il removeSelectedimageObject:si];
            }
        }
    }
    for(SelectedImage* si in deleteSelectedImage)
    {
        NSArray* data = [self fetchFinishedImage:si.url urlDate:si.urldate];
        if(data != nil && data.count > 0)
        {
            for(FinishedImage* fi in data)
            {
                [self.managedObjectContext deleteObject:fi];
            }
        }
    }
    [self saveCoreDataContext];
}
- (NSSet*) getUnsendIssueReport
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* issueReport = userInfo.issuereport;
    return issueReport;
}
- (void) saveIssueReport: (NSString*)devName : (double)date : (NSString*)title : (NSString*)descript
{
    UserInfo* userInfo = [self getUserInfo];
    IssueReport* isr = (IssueReport*)[self newObjectByEntityName:@"IssueReport"];
    [isr setValue:title forKey:@"title"];
    [isr setValue:[NSNumber numberWithDouble:date] forKey:@"date"];
    [isr setValue:descript forKey:@"descript"];
    [isr setValue:devName forKey:@"devicename"];
    [userInfo addIssuereportObject:isr];
    [self saveContext];
}
- (void) printSelectedImage
{
    NSArray* selectedImageArray = [self getSelectedImageArrayByName:mCurrentLoadedImageListName];
    NSLog(@"current image list name = %@", mCurrentLoadedImageListName);
    int i = 0; 
    for(SelectedImage* si in selectedImageArray)
    {
        if(si.url != nil)
        {
            NSLog(@" selected %d image: %@ , %@, %@ ##", i, si.url, si.urldate, si.urltype);
            i++;
        }
    }
}
- (void) printThumbnail
{
    NSFetchRequest* fetchReq = [[NSFetchRequest alloc] init];
    NSEntityDescription* entity = [NSEntityDescription entityForName:@"FinishedImage" inManagedObjectContext:self.managedObjectContext];
    [fetchReq setEntity:entity];
    NSError* error = nil;
    NSArray* data = [self.managedObjectContext executeFetchRequest:fetchReq error:&error];
    if(error)
    {
        NSLog(@"Unresolved error when init data : %@", [error userInfo]);
        abort();
    }
    [fetchReq release];
    int index = 0;
    for(FinishedImage* fi in data)
    {
        NSLog(@"## thumbnail %d: %@, %@", index, fi.url, fi.urldate);
        index++;
    }

}
/*
- (void) removeCurrentSelectedMusic
{
    SEContentViewContainer* container = mViewArray[SELECTED_MUSIC_VIEW];
    SESelectedMusicView* selectedMusicView = (SESelectedMusicView*)[container contentView];
    [selectedMusicView removeCurrentSelectedMusic];

}
 */
- (void) pauseMusicInMusicPicker: (UIView*)currentView
{
    SEContentViewContainer* container = mViewArray[SELECTED_MUSIC_VIEW];
    SESelectedMusicView* selectedMusicView = (SESelectedMusicView*)[container contentView];
    
    container = mViewArray[MUSIC_PICKER];
    SEMusicPickerView* pickerview = (SEMusicPickerView*)[container contentView];
    
    container = mViewArray[MUSIC_IMAGE_LIST_ATTACH];
    SEMusicImageListView* lv = (SEMusicImageListView*)[container contentView];
    [lv pauseMusic];
    if(currentView == selectedMusicView)
    {
        [pickerview pauseMusic];
    }
    if(currentView == pickerview)
    {
        [selectedMusicView pauseMusic];
    }
}
- (void) determineSelectedMusicRow
{
    if(mMusicFloatView == nil)
        return;
    SEContentViewContainer* container = mViewArray[SELECTED_MUSIC_VIEW];
    SESelectedMusicView* selectedMusicView = (SESelectedMusicView*)[container contentView];
    [selectedMusicView determineSelectedMusicRow: mMusicFloatView];
}
- (BOOL) addMusicToSelectedView: (NSArray*)items
{
    if(items == nil || items.count == 0)
        return NO;
    SEContentViewContainer* container = mViewArray[SELECTED_MUSIC_VIEW];
    SESelectedMusicView* selectedMusicView = (SESelectedMusicView*)[container contentView];
    return [selectedMusicView addNewMusic:items];
    
}
- (Signature*) getSignature: (NSNumber*)seq
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* signatureSet = userInfo.signaturelist;
    Signature* sig = nil;
    NSEnumerator* it = [signatureSet objectEnumerator];
    while((sig = [it nextObject]) != nil)
    {
        NSNumber* tmpSeq = sig.seq;
        if([tmpSeq isEqualToNumber:seq])
        {
            return sig;
        }
    }
    return nil;
}
- (SignaturePointData) getSignaturePointsWithSeqName:(NSString*)name
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* signatures = userInfo.signaturelist;
    Signature* sig = nil;
    NSEnumerator* it = [signatures objectEnumerator];
    int inputSeq = [name intValue];
    SignaturePointData sd;
    sd.points = [NSMutableArray array];
    sd.lineWidth = 0;
    while((sig = [it nextObject]) != nil)
    {
        if([sig.seq isEqualToNumber:[NSNumber numberWithInt:inputSeq]])
        {
            return [self getSignaturePoints:sig.seq];
        }
    }
    return sd;
}
- (void) updateImageInSelectedImageView
{
    SEContentViewContainer* container = mViewArray[SELECTED_IMAGE_VIEW];
    SEPageUIScrollView* selectedImageView = (SEPageUIScrollView*)[container contentView];
    [selectedImageView updateImageViewByScroll];
}
- (void) updateImageInImagePickerView
{
    SEContentViewContainer* container = mViewArray[IMAGE_PICKER];
    SEPageUIScrollView* selectedImageView = (SEPageUIScrollView*)[container contentView];
    [selectedImageView updateImageViewByScroll];
}
- (void) updateMusicInSelectedImageView
{
    SEContentViewContainer* container = mViewArray[SELECTED_MUSIC_VIEW];
    SESelectedMusicView* selectedMusicView = (SESelectedMusicView*)[container contentView];
    [selectedMusicView updateMusicView];
}
- (void) updateMusicInMusicPickerView
{
    SEContentViewContainer* container = mViewArray[MUSIC_PICKER];
    SEMusicPickerView* musicPicker = (SEMusicPickerView*)[container contentView];
    [musicPicker updateMusicView];
}
- (BOOL) isInMainDisplay
{
    return mCurrView == MAIN_DISPLAY;
}
- (void) finishSendComment: (NSNumber*) data
{
    NSLog(@"return code = %@", data);
    if([data intValue] == 1)
    {
        NSLog(@"finish one comment");
        [mCommentSender setOutputText:@"Suggestion Sent OK"];
        //[self finishOneComment];
    }
    else
    {
        NSLog(@"send comment error");
        [mCommentSender setOutputText:@"Suggestion Sent Error"];
    }
    [mCommentSender release];
    mCommentSender = nil;
    [self hideLoadingView];
}
- (void) sendComment: (NSString*)comment  label: (UILabel*) label
{
    [self showLoadingView];
    SECommentSender* sender = [[SECommentSender alloc] initWithComment:comment label:label];
    mCommentSender = sender;
    [sender sendWith:self finishedAction:@selector(finishSendComment:)];
}
- (SignaturePointData) getCurrentSignaturePoints
{
    UserInfo* userInfo = [self getUserInfo];
    return [self getSignaturePoints:userInfo.currentsignature];
}
- (SignaturePointData) getSignaturePoints:(NSNumber*)seq
{
    Signature* sig = [self getSignature:seq];
    NSData* data = sig.data;
    SignaturePointData sd;
    sd.points = [NSMutableArray array];
    sd.colors = [NSMutableArray array];
    if(data)
    {
        NSMutableArray* pointsArray = [NSMutableArray array];
        NSInputStream* input = [NSInputStream inputStreamWithData:data];
        [input open];
        int num;
        int ret = [input read:(uint8_t*)&num maxLength:sizeof(int)];
        assert(ret != -1);
        for(int i = 0 ; i  < num ; i++)
        {
            int pointNum;
            ret = [input read:(uint8_t *)&pointNum maxLength:sizeof(int)];
            assert(ret != -1);
            //pointNum = convertInt(pointNum);
            NSMutableArray* a = [NSMutableArray array];
            for(int j = 0 ; j < pointNum ; j++)
            {
                CGFloat x, y, lw;
                long time;
                long sec, usec;
                ret = [input read:(uint8_t *)&x maxLength:sizeof(CGFloat)];
                assert(ret != -1);
                ret = [input read:(uint8_t *)&y maxLength:sizeof(CGFloat)];
                assert(ret != -1);
                ret = [input read: (uint8_t*)&lw maxLength: sizeof(CGFloat)];
                assert(ret != -1);
                ret = [input read: (uint8_t*)&time maxLength:sizeof(long)];
                assert(ret != -1);
                ret = [input read: (uint8_t*)&sec maxLength:sizeof(long)];
                assert(ret != -1);
                ret = [input read: (uint8_t*)&usec maxLength:sizeof(long)];
                assert(ret != -1);
                SEDrawTouchPoint* point = [[SEDrawTouchPoint alloc] init];
                point.lineWidth = lw;
                point.point = CGPointMake(x, y);
                point.milliTime = time;
                point.sec = sec;
                point.usec = usec;
                [a addObject:point];
                [point release];
            }
            [pointsArray addObject:a];
        }
        for(int i = 0 ; i < num ; i++)
        {
            CGFloat r, g, b, a;
            ret = [input read:(uint8_t *)&r maxLength:sizeof(CGFloat)];
            assert(ret != -1);
            ret = [input read:(uint8_t *)&g maxLength:sizeof(CGFloat)];
            assert(ret != -1);
            ret = [input read:(uint8_t *)&b maxLength:sizeof(CGFloat)];
            assert(ret != -1);
            ret = [input read:(uint8_t *)&a maxLength:sizeof(CGFloat)];
            assert(ret != -1);
            UIColor* c = [UIColor colorWithRed:r green:g blue:b alpha:a];
            [sd.colors addObject:c];
        }
        [input close];
        sd.points = pointsArray;
        return sd;
    }
    else
        return sd;
    
}
- (void)writeDataToFile
{
    if(mRecvData == nil)
        return;
    if(mRecvData.length == 0)
    {
        [mRecvData release];
        return;
    }
    BOOL mydebug = YES;
    if(mydebug)
    {
        int len = mRecvData.length;
        const char* bytes = (const char*)[mRecvData bytes];
        char* data = (char*)malloc(len + 1);
        memset(data, 0, len + 1);
        memcpy(data, bytes, len);
        NSString* str = [NSString stringWithCString:data encoding:NSUTF8StringEncoding];
        NSLog(@"### get from internet : %@ ", str);
    }
    NSFileManager* fileManager = [NSFileManager defaultManager];
    NSArray* dir = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask,YES);
    NSString* directoryPath = [dir objectAtIndex:0];//[[dir objectAtIndex:0] stringByAppendingFormat:@"/config/"];
    NSURL* url = [NSURL fileURLWithPath:directoryPath isDirectory:YES];
    NSError* error = nil;
    BOOL succ = YES;
    //BOOL succ = [fileManager createDirectoryAtURL:url withIntermediateDirectories:YES attributes:nil error:&error];
    if(succ == YES)
    {
        NSString* filePath = [directoryPath stringByAppendingFormat:@"/%@", mCurrentDownloadingFileName];
        succ = [fileManager createFileAtPath:filePath contents:mRecvData attributes:nil];
        if(succ)
        {
            NSLog(@"write data to file OK");
            self.mNewConfig = YES;
            if(mLabel)
            {
                UILabel* label = (UILabel*)mLabel;
                label.text = @"OK";
            }
        }
        else 
        {
            NSLog(@"write data to file error");
            self.mNewConfig = NO;
            if(mLabel)
            {
                UILabel* label = (UILabel*)mLabel;
                label.text = @"ERROR";
            }
        }
    }
    [mRecvData release];
}
- (void) downloadParamConfig: (id) label textName: (NSString*)textName
{
    NSString* strURL = @"http://mobilefly.sinaapp.com/text_get_action.php";
    NSURL* url = [NSURL URLWithString:strURL];
    NSMutableURLRequest* req = [NSMutableURLRequest requestWithURL:url];
    [req setHTTPMethod:@"POST"];
    NSMutableData* postBody = [NSMutableData data];
    NSString* userName = @"aaa";
    if([textName isEqualToString:@"paramset_url.txt"] == NO)
        userName = @"ccc";
    [postBody appendData:[[NSString stringWithFormat:@"user=%@&", userName] dataUsingEncoding:NSUTF8StringEncoding]];
    [postBody appendData:[[NSString stringWithFormat:@"password=%@&", @"bbb"] dataUsingEncoding:NSUTF8StringEncoding]];
    [req setHTTPBody:postBody];
    NSURLConnection* conn = [NSURLConnection connectionWithRequest:req delegate:self];
    mRecvData = [NSMutableData data];
    mRecvData = [mRecvData retain];
    mLabel = label;
    self.mCurrentDownloadingFileName = textName;
    ((UILabel*)mLabel).text = @"downloading...";
    [conn retain];
}
- (void)connection:(NSURLConnection *)connection didReceiveResponse:(NSURLResponse *)response
{
    NSLog(@"start response");
    NSThread* currentThread = [NSThread currentThread];
    NSThread* mainThread = [NSThread mainThread];
    assert(currentThread == mainThread);
    if([response isMemberOfClass:[NSHTTPURLResponse class]])
    {
        int statusCode = ((NSHTTPURLResponse*)response).statusCode;
        NSLog(@"response status code = %d ", statusCode);
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
    if(mLabel)
    {
        UILabel* label = (UILabel*)mLabel;
        label.text = @"error";
    }
    [connection release];
}
- (void)connectionDidFinishLoading:(NSURLConnection *)connection
{
    NSLog(@"finished connection");
    [self writeDataToFile];
    if(mCurrentDownloadingFileName == @"paramset_url.txt")
    {
        if(mLabel)
        {
            UILabel* label = (UILabel*)mLabel;
            label.text = @"downloading param id ...";
        }
        [self downloadParamConfig:mLabel textName:@"paramid_url.txt"];
    }
    /*
    if(mLabel)
    {
        UILabel* label = (UILabel*)mLabel;
        label.text = @"download OK";
    }
     */
    [connection release];

}
- (void) showInRootView: (UIView*)v
{
    BOOL found = NO;
    for(UIView* view in mRootView.subviews)
    {
        if(view == v)
        {
            found = YES;
            break;
        }
    }
    if(found == NO)
    {
        [mRootView addSubview:v];
    }
}
- (void) hideFromRootView: (UIView*)v
{
    [v removeFromSuperview];
}

- (void) showUserUpgradeView
{
    UIView* userUpgradeView = [self getUserUpgradeView];
    if(userUpgradeView)
    {
        [self showInRootView:userUpgradeView];
    }
}
- (void) hideUserUpgradeView
{
    //[self hideFromRootView:mUserUpdateView];
    [mUserUpdateView removeFromSuperview];
    mUserUpdateView = nil;
    [mUserUpgradeViewController release];
    mUserUpgradeViewController = nil;
    mUpgradeViewShow = NO;
    NSArray* upgradeInfoArray = [self getUpgradeInfoArray];
    if(upgradeInfoArray.count > 0)
    {
        //[self notificationShow];
        [self performSelectorOnMainThread:@selector(notificationShow) withObject:nil waitUntilDone:NO];
    }
}

- (void) handleNotificationOk: (UITapGestureRecognizer*)ges
{
    NSLog(@"## handleNotificationOk ##");
    [self showUserUpgradeView];
    [self notificationHide];
    [self removeUpgradeInfo];
    mUpgradeViewShow = YES;
}
- (void) handleUserUpdateViewOK: (id)sender
{
    NSLog(@"hide userupgrade view");
    [self hideUserUpgradeView];
}
- (UIView*) getUserUpgradeView
{
    SEUserUpgradeViewController* controller = [[SEUserUpgradeViewController alloc] init];
    controller.mViewNav = self;
    mUserUpdateView = [controller getView];
    CGSize s = CGSizeMake(mUserUpdateView.frame.size.width, mUserUpdateView.frame.size.height);
    mUserUpdateView.frame = CGRectMake((mViewPortWidth - s.width) / 2, (mViewPortHeight - s.height) / 2, s.width, s.height);
    mUserUpgradeViewController  = controller;
    return mUserUpdateView;
}
- (void) setNotificationViewTransform: (CGSize) s view: (UIView*)notificationView
{
    SEContentViewContainer* c = mViewArray[MAIN_DISPLAY];
    SEMainDisplay* md = (SEMainDisplay*)[c contentView];
    float toolBarHeight = [md getToolBarHeight];
    if(UIInterfaceOrientationIsPortrait(self.interfaceOrientation))
    {
        notificationView.transform = CGAffineTransformIdentity;
        notificationView.frame = CGRectMake(0, 0, s.width, s.height);
        float w = s.width;
        float h = s.height;
        CGAffineTransform t1 = CGAffineTransformMakeTranslation(self.mViewPortWidth, 0);
        CGAffineTransform t2 = CGAffineTransformTranslate(t1, -w / 2, -h / 2);
        CGAffineTransform r1 = CGAffineTransformRotate(t2, -90 * 3.1415926 / 180.0f);
        CGAffineTransform t3 = CGAffineTransformTranslate(r1, -w / 2, -h / 2 - toolBarHeight);
        notificationView.transform = t3;
    }
    else
    {
        notificationView.transform = CGAffineTransformIdentity;
        notificationView.frame = CGRectMake(0, 0, s.width, s.height);
        notificationView.transform = CGAffineTransformMakeTranslation(mViewPortWidth - s.width, mViewPortHeight - s.height - toolBarHeight);
    }
}
- (UIView*) getNotificationView
{
    UIImage* bgImage = [mResLoader getImage:@"NotificationViewBackground1"];
    CGSize s = bgImage.size;
    if(mNotificationView == nil)
    {
        mNotificationView = [[UIView alloc] initWithFrame:CGRectMake(0, 0, s.width, s.height)];
        UIImageView* imageView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, s.width, s.height)];
        [mNotificationView addSubview:imageView];
        [imageView release];
        imageView.image = bgImage;
        mNotificationView.userInteractionEnabled = YES;
        imageView.userInteractionEnabled = YES;
        UITapGestureRecognizer* tap = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(handleNotificationOk:)];
        [imageView addGestureRecognizer:tap];
        [tap release];
    }
    [self setNotificationViewTransform:s view:mNotificationView];
    return mNotificationView;
}
- (void) notificationTimerUpdate: (NSTimer*)timer
{
    UIImage* bgImage1 = [mResLoader getImage:@"NotificationViewBackground1"];
    UIImage* bgImage2 = [mResLoader getImage:@"NotificationViewBackground2"];
    UIImageView* imageView = [mNotificationView.subviews objectAtIndex:0];
    if(imageView.image == bgImage1)
    {
        imageView.image = bgImage2;
    }
    else
    {
        imageView.image = bgImage1;    
    }
}
- (void) notificationShow
{
    if(mUpgradeViewShow == NO)
    {
        UIView* v = [self getNotificationView];
        [self showInRootView:v];
        [mNotificationViewTimer invalidate];
        mNotificationViewTimer = [NSTimer timerWithTimeInterval:0.3 target:self selector:@selector(notificationTimerUpdate:) userInfo:nil repeats:YES];
        [[NSRunLoop currentRunLoop] addTimer:mNotificationViewTimer forMode:NSDefaultRunLoopMode];
    }
}
- (void) notificationHide
{
    [mNotificationViewTimer invalidate];
    mNotificationViewTimer = nil;
    [mNotificationView removeFromSuperview];
}
- (void) finishShareOneImage
{
    [mUserUpgradeInfo shareOneImage];
}
- (void) finishOneImageDrawing
{
    [mUserUpgradeInfo finishOneImageDrawing];
}
- (void) finishOneMusicPlay
{
    [mUserUpgradeInfo finishOneMusicPlay];
}
- (void) finishOneComment
{
    [mUserUpgradeInfo finishOneComment];
}
- (void) getMedalPercent: (int) achieveType : (int)medal : (float*)outPercent : (float*)outPercentArray
{
    [mUserUpgradeInfo getMedalPercent:achieveType : medal: outPercent: outPercentArray];
}
- (float) getCurrentLevelPercent
{
    return [mUserUpgradeInfo getCurrentLevelPercent];
}
- (void) clearUserUpgrade
{
    UserInfo* userInfo = [self getUserInfo];
    self.mSystemDataManager.finishedimagenum = [NSNumber numberWithInt:0];
    [self saveCoreDataContext];
}
- (UIView*) getLogView
{
    if(mLogView == nil)
    {
        CGSize s = CGSizeMake(400, 300);
        mLogView = [[UIView alloc] initWithFrame:CGRectMake(1024 - s.width, 0, s.width, s.height)];
        UITextView* textView = [[UITextView alloc] initWithFrame:CGRectMake(0, 0, s.width, s.height)];
        [mLogView addSubview:textView];
        [textView release];
        mLogView.backgroundColor = [UIColor clearColor];
        textView.backgroundColor = [UIColor whiteColor];
        textView.textColor = [UIColor greenColor];
    }
    return mLogView;
}
- (UITextView*) getLogTextView
{
    UIView* v = [self getLogView];
    return [v.subviews objectAtIndex:0];
}
- (void) addLog: (NSString*)text
{
    //UITextView* textView = [self getLogTextView];
    //NSString* str = textView.text;
    //textView.text = [str stringByAppendingFormat:@"%@\n", text];
}
- (void) showLogView
{
    UIView* v = [self getLogView];
    [self showInRootView:v];
}
- (void) hideLogView
{
    UIView* v = [self getLogView];
    [v removeFromSuperview];
}
- (BOOL) isLogViewHide
{
    if(mLogView == nil)
        return YES;
    BOOL found = NO;
    for(UIView* v in mRootView.subviews)
    {
        if(v == mLogView)
        {
            found = YES;
            break;
        }
    }
    return found;
}
- (SEUserData*)getUserData: (int)level
{
    return [mUserUpgradeInfo getUserData:level];
}
- (int) getMaxUserLevel
{
    SEUserData* last = [mUserUpgradeInfo getMaxUserData];
    return last.level;
}
- (void) playMusicWithTitle: (NSString*)title artist: (NSString*) artist album: (NSString*)album
{
    //MPMediaQuery* query = [MPMediaQuery songsQuery];
    //NSArray* result = [query collections];
    //MPMediaItem* item = [self getMediaItem:result title:title artist:artist album:album];
    NSArray* items = [SEUtil findMediaItemByTitle:title aritst:artist album:album];
    if(items.count == 0)
    {
        NSLog(@"can not find this title song : %@", title);
        return;
    }
    NSMutableArray* itemArray = [NSMutableArray array];
    MPMediaItem* item = [items objectAtIndex:0];
    [itemArray addObject:item];
    MPMediaItemCollection* queue = [MPMediaItemCollection collectionWithItems:itemArray];
    if(queue)
    {
        MPMusicPlayerController* player = [MPMusicPlayerController applicationMusicPlayer];
        [player setQueueWithItemCollection:queue];
        player.shuffleMode = MPMusicShuffleModeSongs;
        [player play];
    }
}

- (SEDimView*) getDimView
{
    if(mDimView == nil)
    {
        mDimView = [[SEDimView alloc] initWithFrame:CGRectMake(0, 0, mViewPortWidth, mViewPortHeight)];
        [mRootView addSubview:mDimView];
        mDimView.backgroundColor = [UIColor blackColor];
        mDimView.alpha = 0.5;
        [mDimView release];
    }
    mDimView.userInteractionEnabled = NO;
    [mRootView bringSubviewToFront:mDimView];
    return mDimView;
    
}
- (void) setDimViewBrightness: (float)alpha
{
    [self getDimView];
    mDimView.alpha = alpha;
}
- (void) removeDimView
{
    [mDimView removeFromSuperview];
    mDimView = nil;
}
- (void) drawSignatureAnim
{
    SEContentViewContainer* c = mViewArray[MAIN_DISPLAY];
    SEMainDisplay* md = (SEMainDisplay*)[c contentView];
    [md startSignatureAnim];
}
- (CGRect) getSignatureViewRect
{
    SEContentViewContainer* c = mViewArray[MAIN_DISPLAY];
    SEMainDisplay* md = (SEMainDisplay*)[c contentView];
    return [md getSignatureRect];
}
- (CGImageRef) createSignatureImageWithPoints: (NSArray*)points colors: (NSArray*)colors
{
    SEContentViewContainer* c = mViewArray[MAIN_DISPLAY];
    SEMainDisplay* md = (SEMainDisplay*)[c contentView];
    return [md createSignatureImageWithPoints:points colors:colors];
}
- (void) signatureAnimEnd
{
    SEContentViewContainer* c = mViewArray[MAIN_DISPLAY];
    SEMainDisplay* md = (SEMainDisplay*)[c contentView];
    [md hideSignatureView];
    [mPainterManager promptNextImageDraw];
}
- (void) showFirstNumView
{
    if(mFirstNumView == nil)
    {
        mFirstNumView = [[UILabel alloc] initWithFrame:CGRectMake(20, 20, 100, 50)];
        [mRootView addSubview:mFirstNumView];
        [mFirstNumView release];
    }
    [mRootView bringSubviewToFront:mFirstNumView];
    mFirstNumView.hidden = NO;
}

- (void) showSecondNumView
{
    if(mSecondNumView == nil)
    {
        mSecondNumView = [[UILabel alloc] initWithFrame:CGRectMake(20, 80, 100, 50)];
        [mRootView addSubview:mSecondNumView];
        [mSecondNumView release];
    }
    [mRootView bringSubviewToFront:mSecondNumView];
    mSecondNumView.hidden = NO;

}
- (void) setFirstViewNum: (int) n
{
    NSString* str = [NSString stringWithFormat:@"%d", n];
    mFirstNumView.text = str;
}
- (void) setSecondViewNum: (int)n
{
    NSString* str = [NSString stringWithFormat:@"%d", n];
    mSecondNumView.text = str;
}
- (void)loaderTimerUpdate:(NSTimer*)timer
{
    mStartAngle += 30;
    if(mStartAngle >= 360)
    {
        mStartAngle = 0;
    }
    //[mLoadingView setStage:(LOADING_STAGE_TYPE)(rand() % 3)];
    [mLoadingView setIconViewTransform:CGAffineTransformMakeRotation(mStartAngle * 3.1415926 / 180.0)];
}

- (void) showLoadingView
{
    if(mLoadingView == nil)
    {
        mLoadingView = [[SELoadingView alloc] initWithFrame:CGRectMake(0, 0, mViewPortWidth, mViewPortHeight)];
        [mRootView addSubview:mLoadingView];
        [mLoadingView release];
    }
    [mRootView bringSubviewToFront:mLoadingView];
    mLoadingView.hidden = NO;
    mStartAngle = 0;
    mLoadingTimer = [[NSTimer timerWithTimeInterval:0.2 target:self selector:@selector(loaderTimerUpdate:) userInfo:nil repeats:YES] retain];
    [[NSRunLoop currentRunLoop] addTimer:mLoadingTimer forMode:NSDefaultRunLoopMode];
}
- (void) showLoadingTextView
{
    [self showLoadingView];
    [mLoadingView useTextView];
}
- (void) showLoadingProgressView
{
    [self showLoadingView];
    [mLoadingView useProgressView];
}
- (void) hideLoadingView
{
    [mLoadingTimer invalidate];
    [mLoadingTimer release];
    mLoadingTimer = nil;
    //mLoadingView.hidden = YES;
    [mLoadingView removeFromSuperview];
    mLoadingView = nil;
    
}
- (SELoadingView*) getLoadingView
{
    return mLoadingView;
}
- (int) getLoadingStage
{
    return mLoadingView.mCurrentStage;
    
}
- (void) setLoadingViewStage: (NSNumber*) stage
{
    [mLoadingView setStage:(LOADING_STAGE_TYPE)[stage intValue]];
}
- (void) createMusicImageAttachInputView
{
    if(mMusicImageAttachInputPopupView == nil)
    {
        mMusicImageAttachInputPopupView = [SEMusicImageListPopupView createFromNib: @"MusicImageListPopup"];
        [mRootView addSubview:mMusicImageAttachInputPopupView];
    }
}
- (void) showMusicImageAttachInputView
{
    mMusicImageAttachInputPopupView.hidden = NO;
    [mRootView bringSubviewToFront:mMusicImageAttachInputPopupView];
}
- (void) hideMusicImageAttachInputView
{
    //mMusicImageAttachInputPopupView.hidden = YES;
    [mMusicImageAttachInputPopupView removeFromSuperview];
    mMusicImageAttachInputPopupView = nil;
}
- (SEMusicImageListPopupView*) getMusicImageListPopupView
{
    return mMusicImageAttachInputPopupView;
}
- (void) createConfirmDlg: (id) target ok: (SEL)okOp cancel : (SEL) cancelOp
{
    if(mConfirmDlg == nil)
    {
        mConfirmDlg = [SEConfirmDlg createFromNib:@"ConfirmDlg" withType:OK_CANCEL];
        [mRootView addSubview:mConfirmDlg];
        [mConfirmDlg setHandler:target ok:okOp cancel:cancelOp];
    }
}
- (void) createConfirmDlg:(id)target ok:(SEL)okOp
{
    if(mConfirmDlg == nil)
    {
        mConfirmDlg = [SEConfirmDlg createFromNib:@"ConfirmDlg2" withType:OK];
        [mRootView addSubview:mConfirmDlg];
        [mConfirmDlg setHandler:target ok:okOp cancel:nil];
    }
}
- (void) showConfirmDlg
{
    [mRootView bringSubviewToFront:mConfirmDlg];
    mConfirmDlg.hidden = NO;
}
- (SEConfirmDlg*) getConfirmDlg
{
    return mConfirmDlg;
}
- (void) dismissConfirmDlg
{
    mConfirmDlg.hidden = YES;
    NSLog(@"confirm dlg count = %d", [mConfirmDlg retainCount]);
    [mConfirmDlg removeFromSuperview];
    mConfirmDlg = nil;
}
////////////
- (void) shareImage
{
    [mShareImage share];
}
- (void) addShareImage : (UIImage*) image
{
    [mShareImage addImage:image];
}
- (void) makeImageMusicPickerMoveToMid
{
    mMoveViewFromMainDisplay = YES;
}
- (SEDataUploadManager*) getDataUploadManager
{
    return mDataUploadManager;
}
- (BOOL) intersectWithOperationView
{
    CGRect r = mFloatView.frame;
    if(mCurrentFloatViewType == FLOATVIEW_MUSIC)
    {
        r = mMusicFloatView.frame;
    }
    if(mOperationViewGroup != nil)
    {
        r = [mRootView convertRect:r toView:mOperationViewGroup];
    }
    //NSLog(@"float view rect = %f, %f, %f, %f",r.origin.x, r.origin.y, r.size.width,r.size.height);
    NSLog(@"operation view = %@", mOperationViewGroup);
    return [mOperationViewGroup intersectOperationView:r];
}
- (void)popupOperationViewGroup : (SEOperationViewGroup*) operatoinViewGroup
{
    /*
    mOperationViewGroup = [[SEOperationViewGroup alloc] init];
    mOperationViewGroup.mViewNav = self;
    mOperationViewGroup.mOperationHandler = mOperationHandler;
    CGRect frame = [mOperationViewGroup calculateFrame];
     */
    CGRect frame = operatoinViewGroup.frame;
    mOperationViewGroup = operatoinViewGroup;
    mOperationViewGroup.frame = CGRectMake(mViewPortWidth - frame.size.width - 20, 0, frame.size.width, frame.size.height);
    [mRootView addSubview:mOperationViewGroup];
    mOperationViewGroup.backgroundColor = [UIColor clearColor];//[UIColor redColor];
}
- (void) disappearOperationViewGroup
{
    [mOperationViewGroup operate];
    [mOperationViewGroup removeFromSuperview];
    mOperationViewGroup = nil;
}
- (void) resetSelectedMusicViewCollisionedRow
{
    SEContentViewContainer* container = mViewArray[SELECTED_MUSIC_VIEW];
    SESelectedMusicView* selectedMusicView = (SESelectedMusicView*)[container contentView];
    [selectedMusicView resetCollisoinedRowRect];
}
/*
- (CGRect) getSignatureViewRect
{
    SEContentViewContainer* container = mViewArray[MAIN_DISPLAY];
    SEMainDisplay* m = (SEMainDisplay*)[container contentView];
    return [m getSignatureRect];
}
 */
- (int) getRemainingMusicCount
{
    int musicCount = [self getCurrentAllSelectedMusicCount];;
    int levelMusicNum = [self getCurrentLevelMusicNum];
    assert(levelMusicNum >= musicCount);
    return levelMusicNum - musicCount;
}
- (int) getRemainingImageCount
{
    int imageCount = [self getCurrentAllSelectedImageCount];
    int levelImageNum = [self getCurrentLevelImageNum];
    assert(levelImageNum >= imageCount);
    return levelImageNum - imageCount;
}
- (void) setSelectedMusicNumToMusicPicker : (NSString*) numStr
{
    SEContentViewContainer* c = mViewArray[MUSIC_PICKER];
    [c setStatusText:numStr];
}
- (void) setSelectedMusicNumToSelectedMusicView: (NSString*)numStr
{
    SEContentViewContainer* c = mViewArray[SELECTED_MUSIC_VIEW];
    [c setStatusText:numStr];
}
- (MPMediaItem*) findMediaItemInMusicPicker: (SEMusicItemProperty*)item
{
    SEContentViewContainer* c = mViewArray[MUSIC_PICKER];
    SEMusicPickerView* picker = (SEMusicPickerView*)[c contentView];
    return [picker findMediaItemByItemProperty:item];
}
- (int) getAllUpgradeInfoCount
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* upgradeInfo = userInfo.upgradelist;
    return upgradeInfo.count;
}
- (UpgradeInfo*) getUpgradeInfoBySeq:(int)seq
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* upgradeInfo = userInfo.upgradelist;
    for(UpgradeInfo* ug in upgradeInfo)
    {
        if([ug.seq intValue] == seq)
            return ug;
    }
    return nil;
}
- (NSString*) getAchievementDescription: (int)achieveType : (int)medal
{
    return [mUserUpgradeInfo getAchieveDescription:achieveType medal:medal];
}
- (void) addUpgradeInfoToCoreData: (int)fromLevel toLevel : (int)toLevel achievementType: (int)achieveType medal: (int)medal
{
    UserInfo* userInfo = [self getUserInfo];
    UpgradeInfo* upgradeInfo = (UpgradeInfo*)[self newObjectByEntityName:@"UpgradeInfo"];
    int seq = [self getAllUpgradeInfoCount];
    upgradeInfo.seq = [NSNumber numberWithInt:seq];
    if(fromLevel == INVALID_LEVEL)
    {
        upgradeInfo.fromlevel = nil;
    }
    else
    {
        upgradeInfo.fromlevel = [NSNumber numberWithInt:fromLevel];
    }
    if(toLevel == INVALID_LEVEL)
    {
        upgradeInfo.tolevel = nil;
    }
    else
    {
        upgradeInfo.tolevel = [NSNumber numberWithInt:toLevel];
    }
    if(achieveType == INVALID_ACHIEVE)
    {
        upgradeInfo.achievementtype = nil;
    }
    else
    {
        upgradeInfo.achievementtype = [NSNumber numberWithInt:achieveType];
    }
    if(medal == INVALID_MEDAL_LEVEL)
    {
        upgradeInfo.medal = nil;
    }
    else
    {
        upgradeInfo.medal = [NSNumber numberWithInt:medal];
    }
    [userInfo addUpgradelistObject:upgradeInfo];
    [self saveCoreDataContext];
}
- (void) removeUpgradeInfo
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* upgradeInfo = userInfo.upgradelist;
    [userInfo removeUpgradelist:upgradeInfo];
    [self saveCoreDataContext];
}
- (BOOL) assertUpgradeArrayOK: (NSArray*)array
{
    if(array.count == 0)
        return YES;
    for(int i = 0 ; i < array.count - 1 ; i++)
    {
        UpgradeInfo* up1 = [array objectAtIndex:i];
        UpgradeInfo* up2 = [array objectAtIndex:i + 1];
        if([up1.seq intValue] + 1 != [up2.seq intValue])
            return NO;
    }
    return YES;
}
- (NSArray*) getUpgradeInfoArray
{
    UserInfo* userInfo = [self getUserInfo];
    NSArray* upgradeInfoArray = [userInfo.upgradelist allObjects];
    NSArray* retArray = [upgradeInfoArray sortedArrayUsingComparator:^NSComparisonResult(id obj1, id obj2) {
        UpgradeInfo* up1 = (UpgradeInfo*)obj1;
        UpgradeInfo* up2 = (UpgradeInfo*)obj2;
        return [up1.seq compare:up2.seq];
    }];
    assert(retArray == nil || [self assertUpgradeArrayOK:retArray]);
    return  retArray;
}
- (SEUserUpgrade*) getUserUpgrade
{
    return mUserUpgradeInfo;
}
- (void) syncSelectedImageWithImageLib
{}
- (void) calculateUpgradeInfo
{
    [mUserUpgradeInfo calculatePresentOnDuty];
    [mUserUpgradeInfo calculateNewPerson];
}
- (void) upgradeInspect: (NSTimer*)timer
{
    NSLog(@"############ upgrade inspect #################");
    [mUserUpgradeInfo calculatePresentOnDuty];
    [mUserUpgradeInfo calculateNewPerson];
    //[mUserUpgradeInfo setTest];
    //[self handleNotificationOk:nil];
}
- (void) startUpgradeTimer
{
    NSTimeInterval time = 60 * 60;
    mUpgradeTimer = [NSTimer timerWithTimeInterval:time target:self selector:@selector(upgradeInspect:) userInfo:nil repeats:YES];
    [[NSRunLoop currentRunLoop] addTimer:mUpgradeTimer forMode:NSDefaultRunLoopMode];
    [mUserUpgradeInfo calculatePresentOnDuty];
    [mUserUpgradeInfo calculateNewPerson];
}
- (void) removeCurrentSelectMusic
{}
- (void) showWebView
{
    if(mWebView == nil)
    {
        mWebView = [[UIWebView alloc] initWithFrame:CGRectMake(0, 0, mViewPortWidth, mViewPortHeight)];
        [mRootView addSubview:mWebView];
        [mWebView release];
    }
    
}
- (UIWebView*) getWebView
{
    return mWebView;
}
- (void) dismissWebView
{
    [mWebView removeFromSuperview];
    mWebView = nil;
}
- (SEDialogView*)createDialogView
{
    if(mDialogView == nil)
    {
        SEDialogView* view = [[SEDialogView alloc] initWithFrame:CGRectMake(0, 0, mViewPortWidth, mViewPortHeight)];
        [mRootView addSubview:view];
        [view release];
        mDialogView = view;
    }
    return mDialogView;
}
- (void) showDialogView
{
    if(mDialogView)
    {
        mDialogView.hidden = NO;
    }
}
- (void) dismissDialogView
{
    [mDialogView removeFromSuperview];
    mDialogView = nil;
}
- (SEDialogView*) getDialogView
{
    return mDialogView;
}
- (void) showDrawingWaitingView
{
    SEContentViewContainer* c = mViewArray[MAIN_DISPLAY];
    SEMainDisplay* mainDisplay = (SEMainDisplay*)[c contentView];
    [mainDisplay startDrawingLoading];
}
- (void) hideDrawingWaitingView
{
    SEContentViewContainer* c = mViewArray[MAIN_DISPLAY];
    SEMainDisplay* mainDisplay = (SEMainDisplay*)[c contentView];
    [mainDisplay stopDrawingLoaing];
}
- (int) getDrawingLoadingStage
{
    SEContentViewContainer* c = mViewArray[MAIN_DISPLAY];
    SEMainDisplay* mainDisplay = (SEMainDisplay*)[c contentView];
    return [mainDisplay getDrawingLoadingStage];

}
- (void) setDrawingLoaingStage: (NSNumber*)i
{
    SEContentViewContainer* c = mViewArray[MAIN_DISPLAY];
    SEMainDisplay* mainDisplay = (SEMainDisplay*)[c contentView];
    [mainDisplay setDrawingLoadingStage:[i intValue]];
}
- (void) playDrawFrameShowAnim
{
    SEContentViewContainer* c = mViewArray[MAIN_DISPLAY];
    SEMainDisplay* mainDisplay = (SEMainDisplay*)[c contentView];
    [mainDisplay playFrameShowAnim];
}
- (void) playDrawFrameHideAnim
{
    SEContentViewContainer* c = mViewArray[MAIN_DISPLAY];
    SEMainDisplay* mainDisplay = (SEMainDisplay*)[c contentView];
    [mainDisplay playFrameHideAnim];
}
- (void) computeEnd
{
    [mDrawingStateManager computeEnd];
}
- (void) drawFinished
{
    [mDrawingStateManager drawFinished];
}
- (DRAW_IMAGE_STATE) getDrawImageState
{
    return mDrawImageState;
}
- (void) setDrawImageState: (DRAW_IMAGE_STATE)state
{
    mDrawImageState = state;
}
- (void) changePlayPauseIcon: (BOOL)b
{
    SEContentViewContainer* c = mViewArray[MAIN_DISPLAY];
    SEMainDisplay* d = (SEMainDisplay*)[c contentView];
    if(b)
    {
        [d setPlayIcon];
    }
    else 
    {
        [d setPaurseIcon];
    }
}
- (void) handleBrushBuied: (NSArray*)nameArray
{
    for(int i = 0 ; i < nameArray.count; i++)
    {
        NSString* brushName = [nameArray objectAtIndex:i];
        
    }
}
- (void) handleTimeBuied: (NSArray*)nameArray
{
    
}
- (void) playMusicByState
{
    [self playMusic];
}
- (BOOL) isInPlayState
{
    return mDrawingStateManager.mCurrentPlayState == PLAY_STATE;
}
- (void) resumeMusicByState
{
    [self resumeMusic];
    //[self playMusic];
}
- (void) stopMusicByState
{
    [self stopMusic];
}
- (void) pauseMusicByState
{
    [self pauseMusic];
}
- (void) showIndication: (NSString*)str justInMainDisplay: (BOOL) bInMainDisplay time: (int) seconds
{
    if(bInMainDisplay)
    {
        SEContentViewContainer* c = mViewArray[MAIN_DISPLAY];
        SEMainDisplay* d = (SEMainDisplay*)[c contentView];
        [d showIndicationView:str time:seconds];
    }
}
- (void) hideIndication
{
    SEContentViewContainer* c = mViewArray[MAIN_DISPLAY];
    SEMainDisplay* d = (SEMainDisplay*)[c contentView];
    [d hideIndicationView];
}
- (void) setBrushRandom: (NSNumber*)bOk
{
    mBrushRandom = [bOk boolValue];
}
- (BOOL) isBrushRandom
{
    return mBrushRandom;
}
- (void) restoreProduct
{
    SEProductManager* productManager = [PhotoFrameAppDelegate getProductManager];
    int num = [productManager getProductNum];
    NSArray* discountProduct = [productManager getAllDiscount];
    for(int i = 0 ; i < num ; i++)
    {
        SEProduct* product = [productManager getProductByIndex:i];
        BOOL isBasicSettingItem = product.func == BASIC_FUNC;
        [[NSUserDefaults standardUserDefaults] setValue:[NSNumber numberWithBool:NO] forKey: product.productId];
        if(isBasicSettingItem == NO)
        {
            [SEUserDefaultManager setFunction:product.func value:NO];
        }
    }
    for(SEProduct* product in discountProduct)
    {
        BOOL isBasicSettingItem = product.func == BASIC_FUNC;
        [[NSUserDefaults standardUserDefaults] setValue:[NSNumber numberWithBool:NO] forKey: product.productId];
        if(isBasicSettingItem == NO)
        {
            [SEUserDefaultManager setFunction:product.func value:NO];
        }
    }

}
///////// test
- (void) popupUpgradeInfoTest
{
    if(mUserUpgradeTestControler == nil)
    {
        mUserUpgradeTestControler = [[SEUserUpgradeTestController alloc] init];
        mUserUpgradePopupTestView = [mUserUpgradeTestControler getView];
    }
    [self showInRootView:mUserUpgradePopupTestView];
}
- (void) dismissPopupTest
{
    [mUserUpgradePopupTestView removeFromSuperview];
    mUserUpgradePopupTestView = nil;
    [mUserUpgradeTestControler release];
    mUserUpgradeTestControler = nil;
}
- (void) testSuite
{
    [mUserUpgradeInfo testSuite];
}
/////////
@end
