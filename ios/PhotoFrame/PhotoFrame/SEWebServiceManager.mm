//
//  SEWebServiceManager.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-10-16.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEWebServiceManager.h"
/*
#import "GTLServicePlus.h"
#import "PhotoFrameAppDelegate.h"
#import "SEViewNavigator.h"
#import "GTLPlusPerson.h"
#import "GTLPlusActivityFeed.h"
#import "GTMOAuth2SignIn.h"
#import "GTLQueryPlus.h"
#import "GTMOAuth2Authentication.h"
NSString *const kKeychainItemName = @"Photo Frame: Google Plus";
NSString* googlePlusClientID = @"401508939250.apps.googleusercontent.com";
NSString* googlePlusClientSecret = @"u8qSertmrpzmfscr72ODQxzO";
NSString* const kGTMOAuth2KeychainErrorDomain  = @"com.google.GTMOAuthKeychain";
NSString* const nGooglePlusViewControllerDismissed = @"GooglePlusViewControllerDismissed";
@interface SEWebService : NSObject
{}
- (BOOL) isSignIn;
- (void) signIn;
- (void) signOut;
@end
@implementation SEWebService

- (BOOL) isSignIn
{
    return NO;
}
- (void) signIn
{}
- (void) signOut
{}
@end
///////////////
@interface SETwitterWebService : SEWebService

@end
@implementation SETwitterWebService


@end
//////////
enum 
{
    kGTMOAuth2KeychainErrorBadArguments = -1301,
    kGTMOAuth2KeychainErrorNoPassword = -1302
};

@interface GTMOAuth2Keychain : NSObject

+ (GTMOAuth2Keychain *)defaultKeychain;

// OK to pass nil for the error parameter.
- (NSString *)passwordForService:(NSString *)service
                         account:(NSString *)account
                           error:(NSError **)error;

// OK to pass nil for the error parameter.
- (BOOL)removePasswordForService:(NSString *)service
                         account:(NSString *)account
                           error:(NSError **)error;

// OK to pass nil for the error parameter.
//
// accessibility should be one of the constants for kSecAttrAccessible
// such as kSecAttrAccessibleWhenUnlocked
- (BOOL)setPassword:(NSString *)password
         forService:(NSString *)service
      accessibility:(CFTypeRef)accessibility
            account:(NSString *)account
              error:(NSError **)error;

// For unit tests: allow setting a mock object
+ (void)setDefaultKeychain:(GTMOAuth2Keychain *)keychain;

@end
static NSString * const kGTMOAuth2AccountName = @"OAuth";
static GTMOAuth2Keychain* sDefaultKeychain = nil;
@implementation GTMOAuth2Keychain

+ (GTMOAuth2Keychain *)defaultKeychain {
    if (sDefaultKeychain == nil) {
        sDefaultKeychain = [[self alloc] init];
    }
    return sDefaultKeychain;
}


// For unit tests: allow setting a mock object
+ (void)setDefaultKeychain:(GTMOAuth2Keychain *)keychain {
    if (sDefaultKeychain != keychain) {
        [sDefaultKeychain release];
        sDefaultKeychain = [keychain retain];
    }
}

- (NSString *)keyForService:(NSString *)service account:(NSString *)account {
    return [NSString stringWithFormat:@"com.google.GTMOAuth.%@%@", service, account];
}

// The Keychain API isn't available on the iPhone simulator in SDKs before 3.0,
// so, on early simulators we use a fake API, that just writes, unencrypted, to
// NSUserDefaults.
#if TARGET_IPHONE_SIMULATOR && __IPHONE_OS_VERSION_MAX_ALLOWED < 30000
#pragma mark Simulator

// Simulator - just simulated, not secure.
- (NSString *)passwordForService:(NSString *)service account:(NSString *)account error:(NSError **)error {
    NSString *result = nil;
    if (0 < [service length] && 0 < [account length]) {
        NSUserDefaults *defaults = [NSUserDefaults standardUserDefaults];
        NSString *key = [self keyForService:service account:account];
        result = [defaults stringForKey:key];
        if (result == nil && error != NULL) {
            *error = [NSError errorWithDomain:kGTMOAuth2KeychainErrorDomain
                                         code:kGTMOAuth2KeychainErrorNoPassword
                                     userInfo:nil];
        }
    } else if (error != NULL) {
        *error = [NSError errorWithDomain:kGTMOAuth2KeychainErrorDomain
                                     code:kGTMOAuth2KeychainErrorBadArguments
                                 userInfo:nil];
    }
    return result;
    
}


// Simulator - just simulated, not secure.
- (BOOL)removePasswordForService:(NSString *)service account:(NSString *)account error:(NSError **)error {
    BOOL didSucceed = NO;
    if (0 < [service length] && 0 < [account length]) {
        NSUserDefaults *defaults = [NSUserDefaults standardUserDefaults];
        NSString *key = [self keyForService:service account:account];
        [defaults removeObjectForKey:key];
        [defaults synchronize];
    } else if (error != NULL) {
        *error = [NSError errorWithDomain:kGTMOAuth2KeychainErrorDomain
                                     code:kGTMOAuth2KeychainErrorBadArguments
                                 userInfo:nil];
    }
    return didSucceed;
}

// Simulator - just simulated, not secure.
- (BOOL)setPassword:(NSString *)password
         forService:(NSString *)service
      accessibility:(CFTypeRef)accessibility
            account:(NSString *)account
              error:(NSError **)error {
    BOOL didSucceed = NO;
    if (0 < [password length] && 0 < [service length] && 0 < [account length]) {
        NSUserDefaults *defaults = [NSUserDefaults standardUserDefaults];
        NSString *key = [self keyForService:service account:account];
        [defaults setObject:password forKey:key];
        [defaults synchronize];
        didSucceed = YES;
    } else if (error != NULL) {
        *error = [NSError errorWithDomain:kGTMOAuth2KeychainErrorDomain
                                     code:kGTMOAuth2KeychainErrorBadArguments
                                 userInfo:nil];
    }
    return didSucceed;
}

#else // ! TARGET_IPHONE_SIMULATOR
#pragma mark Device

+ (NSMutableDictionary *)keychainQueryForService:(NSString *)service account:(NSString *)account {
    NSMutableDictionary *query = [NSMutableDictionary dictionaryWithObjectsAndKeys:
                                  (id)kSecClassGenericPassword, (id)kSecClass,
                                  @"OAuth", (id)kSecAttrGeneric,
                                  account, (id)kSecAttrAccount,
                                  service, (id)kSecAttrService,
                                  nil];
    return query;
}

- (NSMutableDictionary *)keychainQueryForService:(NSString *)service account:(NSString *)account {
    return [[self class] keychainQueryForService:service account:account];
}



// iPhone
- (NSString *)passwordForService:(NSString *)service account:(NSString *)account error:(NSError **)error {
    OSStatus status = kGTMOAuth2KeychainErrorBadArguments;
    NSString *result = nil;
    if (0 < [service length] && 0 < [account length]) {
        CFDataRef passwordData = NULL;
        NSMutableDictionary *keychainQuery = [self keychainQueryForService:service account:account];
        [keychainQuery setObject:(id)kCFBooleanTrue forKey:(id)kSecReturnData];
        [keychainQuery setObject:(id)kSecMatchLimitOne forKey:(id)kSecMatchLimit];
        
        status = SecItemCopyMatching((CFDictionaryRef)keychainQuery,
                                     (CFTypeRef *)&passwordData);
        if (status == noErr && 0 < [(NSData *)passwordData length]) {
            result = [[[NSString alloc] initWithData:(NSData *)passwordData
                                            encoding:NSUTF8StringEncoding] autorelease];
        }
        if (passwordData != NULL) {
            CFRelease(passwordData);
        }
    }
    if (status != noErr && error != NULL) {
        *error = [NSError errorWithDomain:kGTMOAuth2KeychainErrorDomain
                                     code:status
                                 userInfo:nil];
    }
    return result;
}


// iPhone
- (BOOL)removePasswordForService:(NSString *)service account:(NSString *)account error:(NSError **)error {
    OSStatus status = kGTMOAuth2KeychainErrorBadArguments;
    if (0 < [service length] && 0 < [account length]) {
        NSMutableDictionary *keychainQuery = [self keychainQueryForService:service account:account];
        status = SecItemDelete((CFDictionaryRef)keychainQuery);
    }
    if (status != noErr && error != NULL) {
        *error = [NSError errorWithDomain:kGTMOAuth2KeychainErrorDomain
                                     code:status
                                 userInfo:nil];
    }
    return status == noErr;
}

// iPhone
- (BOOL)setPassword:(NSString *)password
         forService:(NSString *)service
      accessibility:(CFTypeRef)accessibility
            account:(NSString *)account
              error:(NSError **)error {
    OSStatus status = kGTMOAuth2KeychainErrorBadArguments;
    if (0 < [service length] && 0 < [account length]) {
        [self removePasswordForService:service account:account error:nil];
        if (0 < [password length]) {
            NSMutableDictionary *keychainQuery = [self keychainQueryForService:service account:account];
            NSData *passwordData = [password dataUsingEncoding:NSUTF8StringEncoding];
            [keychainQuery setObject:passwordData forKey:(id)kSecValueData];
            
            if (accessibility != NULL && &kSecAttrAccessible != NULL) {
                [keychainQuery setObject:(id)accessibility
                                  forKey:(id)kSecAttrAccessible];
            }
            status = SecItemAdd((CFDictionaryRef)keychainQuery, NULL);
        }
    }
    if (status != noErr && error != NULL) {
        *error = [NSError errorWithDomain:kGTMOAuth2KeychainErrorDomain
                                     code:status
                                 userInfo:nil];
    }
    return status == noErr;
}

#endif // ! TARGET_IPHONE_SIMULATOR

@end
///////
@interface SEGooglePlusViewController : NSObject <UIWebViewDelegate>
{
    IBOutlet UIWebView* mWebView;
    IBOutlet UIButton* mButton;
    IBOutlet UIView* mTopView;
    
    // The object responsible for the sign-in networking sequence; it holds
    // onto the authentication object as well.
    GTMOAuth2SignIn *signIn_;
    
    // the page request to load when awakeFromNib occurs
    NSURLRequest *request_;
    
    // The user we're calling back
    //
    // The delegate is retained only until the callback is invoked
    // or the sign-in is canceled
    id delegate_;
    SEL finishedSelector_;
    
#if NS_BLOCKS_AVAILABLE
    void (^completionBlock_)(SEGooglePlusViewController *, GTMOAuth2Authentication *, NSError *);
    
    void (^popViewBlock_)(void);
#endif
    
    NSString *keychainItemName_;
    CFTypeRef keychainItemAccessibility_;
    
    // if non-nil, the html string to be displayed immediately upon opening
    // of the web view
    NSString *initialHTMLString_;
    
    // if non-nil, the URL for which cookies will be deleted when the
    // browser view is dismissed
    NSURL *browserCookiesURL_;
    
    id userData_;
    NSMutableDictionary *properties_;
    
    // YES between sends of start and stop notifications
    BOOL hasNotifiedWebViewStartedLoading_;
    
    // To prevent us from calling our delegate's selector more than once.
    BOOL hasCalledFinished_;
    
    // Set in a webView callback.
    BOOL hasDoneFinalRedirect_;
}
@property (nonatomic, retain) IBOutlet UIWebView* mWebView;
@property (nonatomic, retain) IBOutlet UIButton* mButton;
@property (nonatomic, retain) IBOutlet UIView* mTopView;

// the application and service name to use for saving the auth tokens
// to the keychain
@property (nonatomic, copy) NSString *keychainItemName;

// the keychain item accessibility is a system constant for use
// with kSecAttrAccessible.
//
// Since it's a system constant, we do not need to retain it.
@property (nonatomic, assign) CFTypeRef keychainItemAccessibility;

// optional html string displayed immediately upon opening the web view
//
// This string is visible just until the sign-in web page loads, and
// may be used for a "Loading..." type of message or to set the
// initial view color
@property (nonatomic, copy) NSString *initialHTMLString;

// the underlying object to hold authentication tokens and authorize http
// requests
@property (nonatomic, retain, readonly) GTMOAuth2Authentication *authentication;

// the underlying object which performs the sign-in networking sequence
@property (nonatomic, retain, readonly) GTMOAuth2SignIn *signIn;

// if set, cookies are deleted for this URL when the view is hidden
//
// For Google sign-ins, this is set by default to https://google.com/accounts
// but it may be explicitly set to nil to disable clearing of browser cookies
@property (nonatomic, retain) NSURL *browserCookiesURL;


// userData is retained for the convenience of the caller
@property (nonatomic, retain) id userData;

// Stored property values are retained for the convenience of the caller
- (void)setProperty:(id)obj forKey:(NSString *)key;
- (id)propertyForKey:(NSString *)key;

@property (nonatomic, retain) NSDictionary *properties;
/////////////////////////////////////////
#if !GTM_OAUTH2_SKIP_GOOGLE_SUPPORT
+ (id)controllerWithScope:(NSString *)scope
                 clientID:(NSString *)clientID
             clientSecret:(NSString *)clientSecret
         keychainItemName:(NSString *)keychainItemName
                 delegate:(id)delegate
         finishedSelector:(SEL)finishedSelector;

- (id)initWithScope:(NSString *)scope
           clientID:(NSString *)clientID
       clientSecret:(NSString *)clientSecret
   keychainItemName:(NSString *)keychainItemName
           delegate:(id)delegate
   finishedSelector:(SEL)finishedSelector;

#if NS_BLOCKS_AVAILABLE
+ (id)controllerWithScope:(NSString *)scope
                 clientID:(NSString *)clientID
             clientSecret:(NSString *)clientSecret
         keychainItemName:(NSString *)keychainItemName
        completionHandler:(void (^)(SEGooglePlusViewController *viewController, GTMOAuth2Authentication *auth, NSError *error))handler;

- (id)initWithScope:(NSString *)scope
           clientID:(NSString *)clientID
       clientSecret:(NSString *)clientSecret
   keychainItemName:(NSString *)keychainItemName
  completionHandler:(void (^)(SEGooglePlusViewController *viewController, GTMOAuth2Authentication *auth, NSError *error))handler;
#endif
#endif

// Create a controller for authenticating to non-Google services, taking
//   explicit endpoint URLs and an authentication object
+ (id)controllerWithAuthentication:(GTMOAuth2Authentication *)auth
                  authorizationURL:(NSURL *)authorizationURL
                  keychainItemName:(NSString *)keychainItemName  // may be nil
                          delegate:(id)delegate
                  finishedSelector:(SEL)finishedSelector;

// This is the designated initializer
- (id)initWithAuthentication:(GTMOAuth2Authentication *)auth
            authorizationURL:(NSURL *)authorizationURL
            keychainItemName:(NSString *)keychainItemName
                    delegate:(id)delegate
            finishedSelector:(SEL)finishedSelector;

#if NS_BLOCKS_AVAILABLE
+ (id)controllerWithAuthentication:(GTMOAuth2Authentication *)auth
                  authorizationURL:(NSURL *)authorizationURL
                  keychainItemName:(NSString *)keychainItemName  // may be nil
                 completionHandler:(void (^)(SEGooglePlusViewController *viewController, GTMOAuth2Authentication *auth, NSError *error))handler;

- (id)initWithAuthentication:(GTMOAuth2Authentication *)auth
            authorizationURL:(NSURL *)authorizationURL
            keychainItemName:(NSString *)keychainItemName
           completionHandler:(void (^)(SEGooglePlusViewController *viewController, GTMOAuth2Authentication *auth, NSError *error))handler;
#endif
// apps may replace the sign-in class with their own subclass of it
+ (Class)signInClass;
+ (void)setSignInClass:(Class)theClass;

- (void)cancelSigningIn;

// revocation of an authorized token from Google
#if !GTM_OAUTH2_SKIP_GOOGLE_SUPPORT
+ (void)revokeTokenForGoogleAuthentication:(GTMOAuth2Authentication *)auth;
#endif

//
// Keychain
//

// create an authentication object for Google services from the access
// token and secret stored in the keychain; if no token is available, return
// an unauthorized auth object
#if !GTM_OAUTH2_SKIP_GOOGLE_SUPPORT
+ (GTMOAuth2Authentication *)authForGoogleFromKeychainForName:(NSString *)keychainItemName
                                                     clientID:(NSString *)clientID
                                                 clientSecret:(NSString *)clientSecret;
#endif

// add tokens from the keychain, if available, to the authentication object
//
// returns YES if the authentication object was authorized from the keychain
+ (BOOL)authorizeFromKeychainForName:(NSString *)keychainItemName
                      authentication:(GTMOAuth2Authentication *)auth;

// method for deleting the stored access token and secret, useful for "signing
// out"
+ (BOOL)removeAuthFromKeychainForName:(NSString *)keychainItemName;

// method for saving the stored access token and secret
+ (BOOL)saveParamsToKeychainForName:(NSString *)keychainItemName
                      accessibility:(CFTypeRef)accessibility
                     authentication:(GTMOAuth2Authentication *)auth;

// older version, defaults to kSecAttrAccessibleAfterFirstUnlockThisDeviceOnly
+ (BOOL)saveParamsToKeychainForName:(NSString *)keychainItemName
                     authentication:(GTMOAuth2Authentication *)auth;

//////////
- (void) loadView;
- (IBAction)buttonHandler:(id)sender;
- (void) dismiss;
@end
@interface SEGooglePlusViewController()
@property (nonatomic, copy) NSURLRequest *request;
//@property (nonatomic, copy) NSURLRequest *request;

- (void)signIn:(GTMOAuth2SignIn *)signIn displayRequest:(NSURLRequest *)request;
- (void)signIn:(GTMOAuth2SignIn *)signIn finishedWithAuth:(GTMOAuth2Authentication *)auth
         error:(NSError *)error;
- (void)clearBrowserCookies;
@end

////////////////////////////////////////////
@implementation SEGooglePlusViewController
@synthesize mWebView;
@synthesize mButton;
@synthesize mTopView;
@synthesize signIn = signIn_;
@synthesize authentication;
@synthesize keychainItemName = keychainItemName_;
@synthesize keychainItemAccessibility = keychainItemAccessibility_;
@synthesize initialHTMLString = initialHTMLString_;
@synthesize properties = properties_;
@synthesize userData = userData_;
@synthesize browserCookiesURL = browserCookiesURL_;
@synthesize request = request_;
- (void) dealloc
{
    [mWebView release];
    [mButton release];
    [mTopView release];
    [super dealloc];
}
- (void) popupErrorView
{
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    
}
- (void) loadView
{
    UIView* v = [[[NSBundle mainBundle] loadNibNamed:@"SignInView" owner:self options:nil] lastObject];
    assert(v == mTopView);
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    [viewNav.mRootView addSubview:mTopView];
    if (![signIn_ startSigningIn]) 
    {
        [self popupErrorView];
    }
}
- (void) dismiss
{
    [mTopView removeFromSuperview];
}
- (IBAction)buttonHandler:(id)sender
{
    [self dismiss];
    [self cancelSigningIn];
    NSNotificationCenter* center = [NSNotificationCenter defaultCenter];
    [center postNotificationName:nGooglePlusViewControllerDismissed object:nil userInfo:nil];
}
- (BOOL)shouldUseKeychain {
    NSString *name = self.keychainItemName;
    return ([name length] > 0);
}
#if !GTM_OAUTH2_SKIP_GOOGLE_SUPPORT
+ (id)controllerWithScope:(NSString *)scope
                 clientID:(NSString *)clientID
             clientSecret:(NSString *)clientSecret
         keychainItemName:(NSString *)keychainItemName
                 delegate:(id)delegate
         finishedSelector:(SEL)finishedSelector {
    return [[[self alloc] initWithScope:scope
                               clientID:clientID
                           clientSecret:clientSecret
                       keychainItemName:keychainItemName
                               delegate:delegate
                       finishedSelector:finishedSelector] autorelease];
}

- (id)initWithScope:(NSString *)scope
           clientID:(NSString *)clientID
       clientSecret:(NSString *)clientSecret
   keychainItemName:(NSString *)keychainItemName
           delegate:(id)delegate
   finishedSelector:(SEL)finishedSelector {
    // convenient entry point for Google authentication
    
    Class signInClass = [[self class] signInClass];
    
    GTMOAuth2Authentication *auth;
    auth = [signInClass standardGoogleAuthenticationForScope:scope
                                                    clientID:clientID
                                                clientSecret:clientSecret];
    NSURL *authorizationURL = [signInClass googleAuthorizationURL];
    return [self initWithAuthentication:auth
                       authorizationURL:authorizationURL
                       keychainItemName:keychainItemName
                               delegate:delegate
                       finishedSelector:finishedSelector];
}

#if NS_BLOCKS_AVAILABLE

+ (id)controllerWithScope:(NSString *)scope
                 clientID:(NSString *)clientID
             clientSecret:(NSString *)clientSecret
         keychainItemName:(NSString *)keychainItemName
        completionHandler:(void (^)(SEGooglePlusViewController *viewController, GTMOAuth2Authentication *auth, NSError *error))handler {
    return [[[self alloc] initWithScope:scope
                               clientID:clientID
                           clientSecret:clientSecret
                       keychainItemName:keychainItemName
                      completionHandler:handler] autorelease];
}

- (id)initWithScope:(NSString *)scope
           clientID:(NSString *)clientID
       clientSecret:(NSString *)clientSecret
   keychainItemName:(NSString *)keychainItemName
  completionHandler:(void (^)(SEGooglePlusViewController *viewController, GTMOAuth2Authentication *auth, NSError *error))handler {
    // convenient entry point for Google authentication
    
    Class signInClass = [[self class] signInClass];
    
    GTMOAuth2Authentication *auth;
    auth = [signInClass standardGoogleAuthenticationForScope:scope
                                                    clientID:clientID
                                                clientSecret:clientSecret];
    NSURL *authorizationURL = [signInClass googleAuthorizationURL];
    self = [self initWithAuthentication:auth
                       authorizationURL:authorizationURL
                       keychainItemName:keychainItemName
                               delegate:nil
                       finishedSelector:NULL];
    if (self) 
    {
        completionBlock_ = [handler copy];
    }
    return self;
}

#endif // NS_BLOCKS_AVAILABLE
#endif // !GTM_OAUTH2_SKIP_GOOGLE_SUPPORT

+ (id)controllerWithAuthentication:(GTMOAuth2Authentication *)auth
                  authorizationURL:(NSURL *)authorizationURL
                  keychainItemName:(NSString *)keychainItemName
                          delegate:(id)delegate
                  finishedSelector:(SEL)finishedSelector {
    return [[[self alloc] initWithAuthentication:auth
                                authorizationURL:authorizationURL
                                keychainItemName:keychainItemName
                                        delegate:delegate
                                finishedSelector:finishedSelector] autorelease];
}

- (id)initWithAuthentication:(GTMOAuth2Authentication *)auth
            authorizationURL:(NSURL *)authorizationURL
            keychainItemName:(NSString *)keychainItemName
                    delegate:(id)delegate
            finishedSelector:(SEL)finishedSelector {
    
    self = [super init];
    if (self != nil) {
        delegate_ = [delegate retain];
        finishedSelector_ = finishedSelector;
        
        Class signInClass = [[self class] signInClass];
        
        // use the supplied auth and OAuth endpoint URLs
        signIn_ = [[signInClass alloc] initWithAuthentication:auth
                                             authorizationURL:authorizationURL
                                                     delegate:self
                                           webRequestSelector:@selector(signIn:displayRequest:)
                                             finishedSelector:@selector(signIn:finishedWithAuth:error:)];
        
        // if the user is signing in to a Google service, we'll delete the
        // Google authentication browser cookies upon completion
        //
        // for other service domains, or to disable clearing of the cookies,
        // set the browserCookiesURL property explicitly
        NSString *authorizationHost = [signIn_.authorizationURL host];
        if ([authorizationHost hasSuffix:@".google.com"]) {
            NSString *urlStr = [NSString stringWithFormat:@"https://%@/",
                                authorizationHost];
            NSURL *cookiesURL = [NSURL URLWithString:urlStr];
            [self setBrowserCookiesURL:cookiesURL];
        }
        [self setKeychainItemName:keychainItemName];
    }
    return self;
}

#if NS_BLOCKS_AVAILABLE
+ (id)controllerWithAuthentication:(GTMOAuth2Authentication *)auth
                  authorizationURL:(NSURL *)authorizationURL
                  keychainItemName:(NSString *)keychainItemName
                 completionHandler:(void (^)(SEGooglePlusViewController *viewController, GTMOAuth2Authentication *auth, NSError *error))handler {
    return [[[self alloc] initWithAuthentication:auth
                                authorizationURL:authorizationURL
                                keychainItemName:keychainItemName
                               completionHandler:handler] autorelease];
}

- (id)initWithAuthentication:(GTMOAuth2Authentication *)auth
            authorizationURL:(NSURL *)authorizationURL
            keychainItemName:(NSString *)keychainItemName
           completionHandler:(void (^)(SEGooglePlusViewController *viewController, GTMOAuth2Authentication *auth, NSError *error))handler {
    // fall back to the non-blocks init
    self = [self initWithAuthentication:auth
                       authorizationURL:authorizationURL
                       keychainItemName:keychainItemName
                               delegate:nil
                       finishedSelector:NULL];
    if (self) {
        completionBlock_ = [handler copy];
    }
    return self;
}
#endif
+ (BOOL)authorizeFromKeychainForName:(NSString *)keychainItemName
                      authentication:(GTMOAuth2Authentication *)newAuth {
    newAuth.accessToken = nil;
    
    BOOL didGetTokens = NO;
    GTMOAuth2Keychain *keychain = [GTMOAuth2Keychain defaultKeychain];
    NSString *password = [keychain passwordForService:keychainItemName
                                              account:kGTMOAuth2AccountName
                                                error:nil];
    if (password != nil) {
        [newAuth setKeysForResponseString:password];
        didGetTokens = YES;
    }
    return didGetTokens;
}
#if !GTM_OAUTH2_SKIP_GOOGLE_SUPPORT
+ (GTMOAuth2Authentication *)authForGoogleFromKeychainForName:(NSString *)keychainItemName
                                                     clientID:(NSString *)clientID
                                                 clientSecret:(NSString *)clientSecret {
    Class signInClass = [self signInClass];
    NSURL *tokenURL = [signInClass googleTokenURL];
    NSString *redirectURI = [signInClass nativeClientRedirectURI];
    
    GTMOAuth2Authentication *auth;
    auth = [GTMOAuth2Authentication authenticationWithServiceProvider:kGTMOAuth2ServiceProviderGoogle
                                                             tokenURL:tokenURL
                                                          redirectURI:redirectURI
                                                             clientID:clientID
                                                         clientSecret:clientSecret];
    [[self class] authorizeFromKeychainForName:keychainItemName
                                authentication:auth];
    return auth;
}
#endif




+ (BOOL)removeAuthFromKeychainForName:(NSString *)keychainItemName {
    GTMOAuth2Keychain *keychain = [GTMOAuth2Keychain defaultKeychain];
    return [keychain removePasswordForService:keychainItemName
                                      account:kGTMOAuth2AccountName
                                        error:nil];
}
- (void)clearBrowserCookies {
    // if browserCookiesURL is non-nil, then get cookies for that URL
    // and delete them from the common application cookie storage
    NSURL *cookiesURL = [self browserCookiesURL];
    if (cookiesURL) {
        NSHTTPCookieStorage *cookieStorage;
        
        cookieStorage = [NSHTTPCookieStorage sharedHTTPCookieStorage];
        NSArray *cookies =  [cookieStorage cookiesForURL:cookiesURL];
        
        for (NSHTTPCookie *cookie in cookies) {
            [cookieStorage deleteCookie:cookie];
        }
    }
}
- (GTMOAuth2Authentication *)authentication {
    return self.signIn.authentication;
}
+ (BOOL)saveParamsToKeychainForName:(NSString *)keychainItemName
                     authentication:(GTMOAuth2Authentication *)auth {
    return [self saveParamsToKeychainForName:keychainItemName
                               accessibility:NULL
                              authentication:auth];
}

+ (BOOL)saveParamsToKeychainForName:(NSString *)keychainItemName
                      accessibility:(CFTypeRef)accessibility
                     authentication:(GTMOAuth2Authentication *)auth {
    [self removeAuthFromKeychainForName:keychainItemName];
    // don't save unless we have a token that can really authorize requests
    if (![auth canAuthorize]) return NO;
    
    if (accessibility == NULL
        && &kSecAttrAccessibleAfterFirstUnlockThisDeviceOnly != NULL) {
        accessibility = kSecAttrAccessibleAfterFirstUnlockThisDeviceOnly;
    }
    
    // make a response string containing the values we want to save
    NSString *password = [auth persistenceResponseString];
    GTMOAuth2Keychain *keychain = [GTMOAuth2Keychain defaultKeychain];
    return [keychain setPassword:password
                      forService:keychainItemName
                   accessibility:accessibility
                         account:kGTMOAuth2AccountName
                           error:nil];
}
#if !GTM_OAUTH2_SKIP_GOOGLE_SUPPORT
+ (void)revokeTokenForGoogleAuthentication:(GTMOAuth2Authentication *)auth {
    [[self signInClass] revokeTokenForGoogleAuthentication:auth];
}
#endif
static Class gSignInClass = Nil;

+ (Class)signInClass {
    if (gSignInClass == Nil) {
        gSignInClass = [GTMOAuth2SignIn class];
    }
    return gSignInClass;
}

+ (void)setSignInClass:(Class)theClass {
    gSignInClass = theClass;
}
- (void)cancelSigningIn 
{
    // The application has explicitly asked us to cancel signing in
    // (so no further callback is required)
    hasCalledFinished_ = YES;
    
    [delegate_ autorelease];
    delegate_ = nil;
    
#if NS_BLOCKS_AVAILABLE
    [completionBlock_ autorelease];
    completionBlock_ = nil;
#endif
    
    // The sign-in object's cancel method will close the window
    [signIn_ cancelSigningIn];
    hasDoneFinalRedirect_ = YES;
}
- (void)setProperty:(id)obj forKey:(NSString *)key {
    if (obj == nil) {
        // User passed in nil, so delete the property
        [properties_ removeObjectForKey:key];
    } else {
        // Be sure the property dictionary exists
        if (properties_ == nil) {
            [self setProperties:[NSMutableDictionary dictionary]];
        }
        [properties_ setObject:obj forKey:key];
    }
}

- (id)propertyForKey:(NSString *)key {
    id obj = [properties_ objectForKey:key];
    
    // Be sure the returned pointer has the life of the autorelease pool,
    // in case self is released immediately
    return [[obj retain] autorelease];
}

#pragma mark SignIn callbacks

- (void)signIn:(GTMOAuth2SignIn *)signIn displayRequest:(NSURLRequest *)request {
    // This is the signIn object's webRequest method, telling the controller
    // to either display the request in the webview, or if the request is nil,
    // to close the window.
    //
    // All web requests and all window closing goes through this routine
    
    if (request != nil) {
        const NSTimeInterval kJanuary2011 = 1293840000;
        BOOL isDateValid = ([[NSDate date] timeIntervalSince1970] > kJanuary2011);
        if (isDateValid) {
            // Display the request.
            self.request = request;
            // The app may prefer some html other than blank white to be displayed
            // before the sign-in web page loads.
            // The first fetch might be slow, so the client programmer may want
            // to show a local "loading" message.
            // On iOS 5+, UIWebView will ignore loadHTMLString: if it's followed by
            // a loadRequest: call, so if there is a "loading" message we defer
            // the loadRequest: until after after we've drawn the "loading" message.
            NSString *html = self.initialHTMLString;
            if ([html length] > 0) {
                [mWebView loadHTMLString:html baseURL:nil];
            } else {
                [mWebView loadRequest:request];
            }
        } else {
            // clock date is invalid, so signing in would fail with an unhelpful error
            // from the server. Warn the user in an html string showing a watch icon,
            // question mark, and the system date and time. Hopefully this will clue
            // in brighter users, or at least give them a clue when they report the
            // problem to developers.
            //
            // Even better is for apps to check the system clock and show some more
            // helpful, localized instructions for users; this is really a fallback.
            NSString *html = @"<html><body><div align=center><font size='7'>"
            @"&#x231A; ?<br><i>System Clock Incorrect</i><br>%@"
            @"</font></div></body></html>";
            NSString *errHTML = [NSString stringWithFormat:html, [NSDate date]];
            
            [mWebView loadHTMLString:errHTML baseURL:nil];
        }
    } else {
        // request was nil.
    }
}

- (void)signIn:(GTMOAuth2SignIn *)signIn finishedWithAuth:(GTMOAuth2Authentication *)auth
         error:(NSError *)error {
    if (!hasCalledFinished_) {
        hasCalledFinished_ = YES;
        
        if (error == nil) {
            if (self.shouldUseKeychain) {
                NSString *keychainItemName = self.keychainItemName;
                if (auth.canAuthorize) {
                    // save the auth params in the keychain
                    CFTypeRef accessibility = self.keychainItemAccessibility;
                    [[self class] saveParamsToKeychainForName:keychainItemName
                                                accessibility:accessibility
                                               authentication:auth];
                } else {
                    // remove the auth params from the keychain
                    [[self class] removeAuthFromKeychainForName:keychainItemName];
                }
            }
        }
        
        if (delegate_ && finishedSelector_) {
            SEL sel = finishedSelector_;
            NSMethodSignature *sig = [delegate_ methodSignatureForSelector:sel];
            NSInvocation *invocation = [NSInvocation invocationWithMethodSignature:sig];
            [invocation setSelector:sel];
            [invocation setTarget:delegate_];
            [invocation setArgument:&self atIndex:2];
            [invocation setArgument:&auth atIndex:3];
            [invocation setArgument:&error atIndex:4];
            [invocation invoke];
        }
        
        [delegate_ autorelease];
        delegate_ = nil;
        
#if NS_BLOCKS_AVAILABLE
        if (completionBlock_) {
            completionBlock_(self, auth, error);
            
            // release the block here to avoid a retain loop on the controller
            [completionBlock_ autorelease];
            completionBlock_ = nil;
        }
#endif
    }
}
- (void)notifyWithName:(NSString *)name
               webView:(UIWebView *)webView
                  kind:(NSString *)kind {
    BOOL isStarting = [name isEqual:kGTMOAuth2WebViewStartedLoading];
    if (hasNotifiedWebViewStartedLoading_ == isStarting) {
        // Duplicate notification
        //
        // UIWebView's delegate methods are so unbalanced that there's little
        // point trying to keep a count, as it could easily end up stuck greater
        // than zero.
        //
        // We don't really have a way to track the starts and stops of
        // subframe loads, too, as the webView in the notification is always
        // for the topmost request.
        return;
    }
    hasNotifiedWebViewStartedLoading_ = isStarting;
    
    // Notification for webview load starting and stopping
    NSDictionary *dict = [NSDictionary dictionaryWithObjectsAndKeys:
                          webView, kGTMOAuth2WebViewKey,
                          kind, kGTMOAuth2WebViewStopKindKey, // kind may be nil
                          nil];
    NSNotificationCenter *nc = [NSNotificationCenter defaultCenter];
    [nc postNotificationName:name
                      object:self
                    userInfo:dict];
}


- (BOOL)webView:(UIWebView *)webView
shouldStartLoadWithRequest:(NSURLRequest *)request
 navigationType:(UIWebViewNavigationType)navigationType {
    
    if (!hasDoneFinalRedirect_) {
        hasDoneFinalRedirect_ = [signIn_ requestRedirectedToRequest:request];
        if (hasDoneFinalRedirect_) {
            // signIn has told the view to close
            return NO;
        }
    }
    return YES;
}



- (void)webViewDidStartLoad:(UIWebView *)webView {
    [self notifyWithName:kGTMOAuth2WebViewStartedLoading
                 webView:webView
                    kind:nil];
}

- (void)webViewDidFinishLoad:(UIWebView *)webView {
    [self notifyWithName:kGTMOAuth2WebViewStoppedLoading
                 webView:webView
                    kind:kGTMOAuth2WebViewFinished];
    
    NSString *title = [webView stringByEvaluatingJavaScriptFromString:@"document.title"];
    if ([title length] > 0) {
        [signIn_ titleChanged:title];
    } else {
#if DEBUG
        // Verify that Javascript is enabled
        NSString *result = [webView stringByEvaluatingJavaScriptFromString:@"1+1"];
        NSAssert([result integerValue] == 2, @"GTMOAuth2: Javascript is required");
#endif
    }
    
    if (self.request && [self.initialHTMLString length] > 0) 
    {
        // The request was pending.
        [self setInitialHTMLString:nil];
        [mWebView loadRequest:self.request];
    } 
    else 
    {
        [signIn_ cookiesChanged:[NSHTTPCookieStorage sharedHTTPCookieStorage]];
    }
}

- (void)webView:(UIWebView *)webView didFailLoadWithError:(NSError *)error {
    [self notifyWithName:kGTMOAuth2WebViewStoppedLoading
                 webView:webView
                    kind:kGTMOAuth2WebViewFailed];
    
    // Tell the sign-in object that a load failed; if it was the authorization
    // URL, it will pop the view and return an error to the delegate.
    BOOL isUserInterruption = ([error code] == NSURLErrorCancelled
                               && [[error domain] isEqual:NSURLErrorDomain]);
    if (isUserInterruption) {
        // Ignore this error:
        // Users report that this error occurs when clicking too quickly on the
        // accept button, before the page has completely loaded.  Ignoring
        // this error seems to provide a better experience than does immediately
        // cancelling sign-in.
        //
        // This error also occurs whenever UIWebView is sent the stopLoading
        // message, so if we ever send that message intentionally, we need to
        // revisit this bypass.
        return;
    }
    
    [signIn_ loadFailedWithError:error];
}

@end


/////////////
@interface SEGooglePlusWebService : SEWebService
{
    NSError* profileFetchError;
    SEGooglePlusViewController* controller;
    
    GTLPlusPerson *userProfile_;
    GTLPlusActivityFeed *activityFeed_;
    GTLServiceTicket *profileTicket_;
    NSError *profileFetchError_;
    NSError *activityFeedFetchError_;
}
@property (readonly) GTLServicePlus* plusservice;
@end
@interface SEGooglePlusWebService ()
@property (nonatomic, retain) GTLPlusPerson *userProfile;
@property (nonatomic, retain) GTLPlusActivityFeed *activityFeed;
@property (nonatomic, retain) GTLServiceTicket *profileTicket;
@property (nonatomic, retain) NSError *profileFetchError;
@property (nonatomic, retain) NSError *activityFeedFetchError;
@end
@implementation SEGooglePlusWebService
@dynamic plusservice;
@synthesize profileFetchError = profileFetchError_;
@synthesize userProfile = userProfile_;
@synthesize activityFeed = activityFeed_;
@synthesize profileTicket = profileTicket_;
@synthesize activityFeedFetchError = activityFeedFetchError_;
- (void) googlePlusViewControllerDismissed: (NSNotification*)notification
{
    NSLog(@"google view controller dismissed");
    [controller release];
    controller = nil;
}
- (id) init
{
    self = [super init];
    if(self)
    {
        NSNotificationCenter* center = [NSNotificationCenter defaultCenter];
        [center addObserver:self selector:@selector(googlePlusViewControllerDismissed:) name:nGooglePlusViewControllerDismissed object:nil];
    }
    return self;
}
- (void) dealloc
{
    
}
- (GTLServicePlus *)plusService 
{
    
    static GTLServicePlus* service = nil;
    
    if (!service) {
        service = [[GTLServicePlus alloc] init];
        
        // Have the service object set tickets to retry temporary error conditions
        // automatically
        service.retryEnabled = YES;
        
        // Have the service object set tickets to automatically fetch additional
        // pages of feeds when the feed's maxResult value is less than the number
        // of items in the feed
        service.shouldFetchNextPages = YES;
    }
    return service;
}
- (NSString *)signedInUsername {
    GTMOAuth2Authentication *auth = self.plusService.authorizer;
    BOOL isSignedIn = auth.canAuthorize;
    if (isSignedIn) {
        return auth.userEmail;
    } else {
        return nil;
    }
}
- (BOOL) isSignIn
{
    NSString *name = [self signedInUsername];
    return (name != nil);
}
- (void) updateData
{
    NSString*  resultStr = @"####";
    if (userProfile_) {
        resultStr = [userProfile_ description];
    }
    NSLog(@"resultStr = %@", resultStr);
}
- (void)fetchUserProfile {
    self.userProfile = nil;
    self.activityFeed = nil;
    self.profileFetchError = nil;
    self.activityFeedFetchError = nil;
    
    // Make a batch for fetching both the user's profile and the activity feed
    GTLQueryPlus *profileQuery = [GTLQueryPlus queryForPeopleGetWithUserId:@"me"];
    profileQuery.completionBlock = ^(GTLServiceTicket *ticket, id object, NSError *error) {
        if (error == nil) {
            self.userProfile = object;
        } else {
            self.profileFetchError = error;
        }
    };
    NSString * const kGTLPlusCollectionPublic    = @"public";
    GTLQueryPlus *activitiesQuery = [GTLQueryPlus queryForActivitiesListWithUserId:@"me"
                                                                        collection:kGTLPlusCollectionPublic];
    // Set an appropriate page size when requesting the activity items
    activitiesQuery.maxResults = 100;
    activitiesQuery.completionBlock = ^(GTLServiceTicket *ticket, id object, NSError *error) {
        if (error == nil) {
            self.activityFeed = object;
        } else {
            self.activityFeedFetchError = error;
        }
    };
    
    GTLBatchQuery *batchQuery = [GTLBatchQuery batchQuery];
    [batchQuery addQuery:profileQuery];
    [batchQuery addQuery:activitiesQuery];
    
    GTLServicePlus *service = self.plusService;
    self.profileTicket = [service executeQuery:batchQuery
                             completionHandler:^(GTLServiceTicket *ticket,
                                                 id result, NSError *error) {
                                 // Callback
                                 //
                                 // For batch queries with successful execution,
                                 // the result is a GTLBatchResult object
                                 //
                                 // At this point, the query completion blocks
                                 // have already been called
                                 self.profileTicket = nil;
                                 
                                 [self updateData];
                             }];
    [self updateData];
}

- (void) signInDone
{
    NSLog(@"sign in done");
    [controller dismiss];
    [self fetchUserProfile];
    [controller release];
    controller = nil;
    
}
- (void)runSigninThenInvokeSelector:(SEL)signInDoneSel 
{
    NSString *scope = @"https://www.googleapis.com/auth/plus.me";
    controller =  [SEGooglePlusViewController controllerWithScope:(NSString *)scope
                                                                                         clientID:googlePlusClientID
                                                                                     clientSecret:googlePlusClientSecret
                                                                                 keychainItemName:(NSString *)kKeychainItemName
                                                                                completionHandler: ^(SEGooglePlusViewController* viewController, GTMOAuth2Authentication *auth, NSError *error)
                                                 {
                                                     if (error == nil)
                                                     {
                                                         self.plusService.authorizer = auth;
                                                         if (signInDoneSel) 
                                                         {
                                                             [self performSelector:signInDoneSel];
                                                         }
                                                     }
                                                     else 
                                                     {
                                                         self.profileFetchError = error;
                                                     }
                                                 }];
    [controller loadView];
    controller = [controller retain];
}
- (void) signIn
{
    if([self isSignIn])
        return;
    [self runSigninThenInvokeSelector:@selector(signInDone)];
}
- (void) signOut
{}

@end
///////////
@implementation SEWebServiceManager
- (id) init
{
    self = [super init];
    if(self)
    {
        mWebServiceArray = [[NSMutableArray alloc] initWithArray:[NSArray array]];
        SEWebService* ws = [[[SEGooglePlusWebService alloc] init] autorelease];
        [mWebServiceArray addObject:ws];
        ws = [[[SETwitterWebService alloc] init] autorelease];
        [mWebServiceArray addObject:ws];
    }
    return self;
}
- (void) dealloc
{
    [mWebServiceArray release];
    [super dealloc];
}
- (BOOL) isServiceSignIn: (enum WEBSERVICE_TYPE) webServiceType
{
    if(webServiceType < 0 || webServiceType >= WEBSERVICE_NUM)
        return NO;
    SEWebService* ws = [mWebServiceArray objectAtIndex:webServiceType];
    return [ws isSignIn];
}
- (void) signIn: (enum WEBSERVICE_TYPE) webServiceType
{
    if(webServiceType < 0 || webServiceType >= WEBSERVICE_NUM)
        return;
    SEWebService* ws = [mWebServiceArray objectAtIndex:webServiceType];
    [ws signIn];
}
- (void) signOut: (enum WEBSERVICE_TYPE) webServiceType
{
    if(webServiceType < 0 || webServiceType >= WEBSERVICE_NUM)
        return;
    SEWebService* ws = [mWebServiceArray objectAtIndex:webServiceType];
    [ws signOut];

}
@end
*/