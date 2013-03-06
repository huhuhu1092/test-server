//
//  SEWebAPISender.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-10-13.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEWebAPISender.h"
#import "SEViewNavigator.h"
#import "PhotoFrameAppDelegate.h"
#import "MTJSON.h"
//#import "yajl_parse.h"
//#import "yajl_gen.h"
/*
static int reformat_null(void * ctx)
{
    yajl_gen g = (yajl_gen) ctx;
    return yajl_gen_status_ok == yajl_gen_null(g);
}

static int reformat_boolean(void * ctx, int boolean)
{
    yajl_gen g = (yajl_gen) ctx;
    return yajl_gen_status_ok == yajl_gen_bool(g, boolean);
}

static int reformat_number(void * ctx, const char * s, size_t l)
{
    yajl_gen g = (yajl_gen) ctx;
    return yajl_gen_status_ok == yajl_gen_number(g, s, l);
}

static int reformat_string(void * ctx, const unsigned char * stringVal,
                           size_t stringLen)
{
    yajl_gen g = (yajl_gen) ctx;
    return yajl_gen_status_ok == yajl_gen_string(g, stringVal, stringLen);
}

static int reformat_map_key(void * ctx, const unsigned char * stringVal,
                            size_t stringLen)
{
    yajl_gen g = (yajl_gen) ctx;
    return yajl_gen_status_ok == yajl_gen_string(g, stringVal, stringLen);
}

static int reformat_start_map(void * ctx)
{
    yajl_gen g = (yajl_gen) ctx;
    return yajl_gen_status_ok == yajl_gen_map_open(g);
}


static int reformat_end_map(void * ctx)
{
    yajl_gen g = (yajl_gen) ctx;
    return yajl_gen_status_ok == yajl_gen_map_close(g);
}

static int reformat_start_array(void * ctx)
{
    yajl_gen g = (yajl_gen) ctx;
    return yajl_gen_status_ok == yajl_gen_array_open(g);
}

static int reformat_end_array(void * ctx)
{
    yajl_gen g = (yajl_gen) ctx;
    return yajl_gen_status_ok == yajl_gen_array_close(g);
}

static yajl_callbacks callbacks = {
    reformat_null,
    reformat_boolean,
    NULL,
    NULL,
    reformat_number,
    reformat_string,
    reformat_start_map,
    reformat_map_key,
    reformat_end_map,
    reformat_start_array,
    reformat_end_array
};
 */
@implementation SENameValue
@synthesize name;
@synthesize value;

- (void) dealloc
{
    [name release];
    [value release];
    [super dealloc];
}

@end
@implementation NSString (SEWebAPI)

- (NSString *)URLEncodedString 
{
    NSString *result = (NSString *)CFURLCreateStringByAddingPercentEscapes(kCFAllocatorDefault,
                                                                           (CFStringRef)self,
                                                                           NULL,
																		   CFSTR("!*'();:@&=+$,/?%#[]"),
                                                                           kCFStringEncodingUTF8);
    [result autorelease];
	return result;
}

- (NSString*)URLDecodedString
{
	NSString *result = (NSString *)CFURLCreateStringByReplacingPercentEscapesUsingEncoding(kCFAllocatorDefault,
																						   (CFStringRef)self,
																						   CFSTR(""),
																						   kCFStringEncodingUTF8);
    [result autorelease];
	return result;	
}

@end

@implementation SEWebAPISender
@synthesize mUrl;
@synthesize mApi;
@synthesize mDelegate;
- (void) createCommand : (NSString*) urlName api:(NSString*)api
{
    [mPropertyArray release];
    [mRecvData release];
    [mHeaderArray release];
    mPropertyArray = [[NSMutableArray alloc] initWithArray:[NSArray array]];
    mHeaderArray = [[NSMutableArray alloc] initWithArray:[NSArray array]];
    mRecvData = [[NSMutableData data] retain];
    self.mUrl = urlName;
    self.mApi = api;
}
- (void) setProperty: (NSString*) name value: (NSString*)value
{
    //[mPropertyDict setObject:value forKey:name];
    SENameValue* nv = [[SENameValue alloc] init];
    nv.name = name;
    nv.value = value;
    [mPropertyArray addObject:nv];
    [nv release];
}
- (void) setHeader: (NSString*) headerName value: (NSString*)value
{
    //[mHeaderDict setObject:value forKey:headerName];
    SENameValue* nv = [[SENameValue alloc] init];
    nv.name = headerName;
    nv.value = value;
    [mHeaderArray addObject:nv];
    [nv release];
}
- (void) sendCommandByPost
{
    NSURL* url = [NSURL URLWithString:mUrl];
    NSMutableURLRequest* req = [NSMutableURLRequest requestWithURL:url];
    [req setHTTPMethod:@"POST"];
    if(mPropertyArray.count > 0)
    {
        NSMutableData* postBody = [NSMutableData data];
        //NSArray* allKeys = [mPropertyDict allKeys];

        for(int i = 0 ; i < mPropertyArray.count - 1 ; i++)
        {
            SENameValue* nv = [ mPropertyArray objectAtIndex:i];
            NSString* key = [nv.name URLEncodedString];
            NSString* value = [nv.value URLEncodedString];
            NSString* data = [NSString stringWithFormat:@"%@=%@&", key, value];
            //NSString* newdata = [data URLEncodedString];
            [postBody appendData:[data dataUsingEncoding:NSUTF8StringEncoding]];
        }
        SENameValue* nv = [mPropertyArray lastObject];
        NSString* key = [nv.name URLEncodedString];
        NSString* value = [nv.value URLEncodedString];
        NSString* data = [NSString stringWithFormat:@"%@=%@", key, value];

        [postBody appendData:[data dataUsingEncoding:NSUTF8StringEncoding]];
        const char* bytes = (const char*)[postBody bytes];
        NSUInteger len = [postBody length];
        char* outStr = new char[len + 1];
        outStr[len] = 0;
        memcpy(outStr, bytes, len);
        NSLog(@"out str = %s", outStr);
        [req setHTTPBody:postBody];
        //[req setValue:[NSString stringWithFormat:@"%d", len] forHTTPHeaderField:@"Content-Length"];
    }
    if(mHeaderArray.count > 0)
    {
        for(int i = 0 ; i < mHeaderArray.count ; i++)
        {
            SENameValue* nv = [mHeaderArray objectAtIndex:i];
            NSString* header = nv.name;
            NSString* value = nv.value;
            [req setValue:value forHTTPHeaderField:header];
        }
    }
    
    NSURLConnection* conn = [NSURLConnection connectionWithRequest:req delegate:self];
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
    [mRecvData release];
    mRecvData = nil;
    NSLog(@"error : %@", [error userInfo]);
    [mDelegate sendError:error];
    [connection release];
}
- (void)connectionDidFinishLoading:(NSURLConnection *)connection
{
    NSLog(@"finished connection");
    [mDelegate sendOK:mRecvData];
    [connection release];
    
}

- (void) dealloc
{
    [mPropertyArray release];
    [mRecvData release];
    [mHeaderArray release];
    [mUrl release];
    [mApi release];
    [super dealloc];
}
@end
@implementation SEGooglePlusAccessor
- (id) init
{
    self = [super init];
    if(self)
    {
        googlePlusClientID = @"401508939250.apps.googleusercontent.com";
        googlePlusClientSecret = @"u8qSertmrpzmfscr72ODQxzO";
        mViewNav = [PhotoFrameAppDelegate getViewNavigator];
        mCurrentState = NO_GOOGLEPLUS_STATE;
    }
    return self;
}
- (void) dealloc
{
    [super dealloc];
}
- (void) webViewDidStartLoad:(UIWebView *)webView
{
    NSLog(@"web view start load");
    //NSJSONSerialization
}
- (BOOL)webView:(UIWebView *)webView shouldStartLoadWithRequest:(NSURLRequest *)request navigationType:(UIWebViewNavigationType)navigationType
{
    return YES;
}
- (NSString*) getAuthCode: (NSString*)bodyStr
{
    NSString* prefix = @"<input id=\"code\" type=\"text\" readonly=\"readonly\" value=\"";
    NSRange range = [bodyStr rangeOfString:prefix];
    if(range.location != NSNotFound)
    {
        NSUInteger location = range.location + prefix.length;
        NSString* authStr = [bodyStr substringFromIndex:location];
        NSRange endRange = [authStr rangeOfString:@"\""];
        if(endRange.location != NSNotFound)
        {
            NSString* authCode = [authStr substringToIndex:endRange.location];
            return authCode;
        }
        else {
            return nil;
        }
    }
    else {
        return nil;
    }
}

- (void)webViewDidFinishLoad:(UIWebView *)webView
{
    NSLog(@"web view finished load");
    NSString* title = [webView stringByEvaluatingJavaScriptFromString:@"document.title"];
    NSLog(@"title = %@", title);
    NSString* head = [webView stringByEvaluatingJavaScriptFromString:@"document.head.innerHTML"];
    NSLog(@"head = %@", head);
    NSString* body = [webView stringByEvaluatingJavaScriptFromString:@"document.body.innerHTML"];
    NSLog(@"body = %@", body);
    
    NSRange range = [title rangeOfString:@"="];
    if(range.location == NSNotFound)
        return;
    
    //[self dismissWebView];
    //NSString* authCode = [title substringFromIndex:range.location + 1];
    NSString* authCode = [self getAuthCode:body];
    NSString* newAuthCode = [webView stringByEvaluatingJavaScriptFromString:@"document.getElementById('code').value"];
    NSLog(@"newAuthcode = %@", newAuthCode);
    if(authCode == nil)
    {
        NSLog(@"can not find authCode\n");
        return;
    }
    NSLog(@"authcode = |%@|", authCode);
    SEWebAPISender* webSender = [[SEWebAPISender alloc] init];
    webSender.mDelegate = self;
    [webSender createCommand:@"https://accounts.google.com/o/oauth2/token" api:nil];
    [webSender setHeader:@"Content-Type" value:@"application/x-www-form-urlencoded"];
    [webSender setProperty:@"code" value:authCode];
    [webSender setProperty:@"redirect_uri" value:@"urn:ietf:wg:oauth:2.0:oob"];
    //[webSender setProperty:@"redirect_uri" value:@"https://oauth2-login-demo.appspot.com/code"];
    [webSender setProperty:@"grant_type" value:@"authorization_code"];
    [webSender setProperty:@"client_id" value:googlePlusClientID];
    [webSender setProperty:@"client_secret" value:googlePlusClientSecret];
    [webSender sendCommandByPost];
    mCurrentState = GET_ACCESS_TOKEN;
    mWebSender = webSender;
    
}
- (void)webView:(UIWebView *)webView didFailLoadWithError:(NSError *)error
{
    NSLog(@"web view load failed");
    NSString* title = [webView stringByEvaluatingJavaScriptFromString:@"document.head.innerHTML"];
    NSString* title2 = [webView stringByEvaluatingJavaScriptFromString:@"document.title"];
    NSLog(@"failed title = %@", title);
    NSLog(@"failed title2 = %@", title2);
    NSHTTPCookieStorage* cookieStorage = [NSHTTPCookieStorage sharedHTTPCookieStorage];
    for (NSHTTPCookie* cookie in [cookieStorage cookies])
    {
        NSLog(@"%@", cookie);
        [cookieStorage deleteCookie:cookie];
    }
    [mViewNav dismissWebView];
}

- (void) access
{
    [mViewNav showWebView];
    UIWebView* webView = [mViewNav getWebView];
    webView.delegate = self;
    NSString* clientID = [NSString stringWithFormat:@"client_id=%@",googlePlusClientID];
    NSString* redirect_uri = @"redirect_uri=urn:ietf:wg:oauth:2.0:oob&";
    NSString* response_type = @"response_type=code&";
    NSString* scope = @"scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.email+https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.profile&";
    NSString* urlString = @"https://accounts.google.com/o/oauth2/auth?";
    urlString = [urlString stringByAppendingFormat:@"%@%@%@%@", scope, redirect_uri, response_type, clientID];
    NSURL* url = [NSURL URLWithString:urlString];
    NSURLRequest* req = [NSURLRequest requestWithURL:url];
    [webView loadRequest:req];
}
- (void) parseJSONData: (const unsigned char*)buffer len: (int)len
{
    /*
    yajl_handle hand;
    //static unsigned char fileData[65536];
    // generator config
    yajl_gen g;
    yajl_status stat;
    size_t rd = len;
    int retval = 0;
    
    g = yajl_gen_alloc(NULL);
    yajl_gen_config(g, yajl_gen_beautify, 1);
    yajl_gen_config(g, yajl_gen_validate_utf8, 1);
    
    // ok.  open file.  let's read and parse
    hand = yajl_alloc(&callbacks, NULL, (void *) g);
    //and let's allow comments by default
    yajl_config(hand, yajl_allow_comments, 1);
    
    stat = yajl_parse(hand, buffer, len);
    
    if (stat != yajl_status_ok) 
        return;
    
    {
        const unsigned char * buf;
        size_t len;
        yajl_gen_get_buf(g, &buf, &len);
        fwrite(buf, 1, len, stdout);
        yajl_gen_clear(g);
    }
    
    stat = yajl_complete_parse(hand);
    
    if (stat != yajl_status_ok) {
        unsigned char * str = yajl_get_error(hand, 1, buffer, rd);
        fprintf(stderr, "%s", (const char *) str);
        yajl_free_error(hand, str);
        retval = 1;
    }
    
    yajl_gen_free(g);
    yajl_free(hand);
    */

}
- (void) handleGetAccessToken: (NSMutableArray*)root
{
    assert(root.count == 1);
    NSMutableDictionary* accessTokenData = [root objectAtIndex:0];
    NSString* accessToken = [accessTokenData valueForKey:@"access_token"];
    NSString* expire = [accessTokenData valueForKey:@"expires_in"];
    NSString* tokenType = [accessTokenData valueForKey:@"token_type"];
    NSString* refreshToken = [accessTokenData valueForKey:@"refresh_token"];
    [[NSUserDefaults standardUserDefaults] setObject:accessToken forKey:@"access_token"];
    [[NSUserDefaults standardUserDefaults] setObject:refreshToken forKey:@"refresh_token"];
    NSDate* date = [NSDate date];
    float timeExpire = [expire floatValue];
    NSDate* newDate = [NSDate dateWithTimeInterval:timeExpire sinceDate:date];
    [[NSUserDefaults standardUserDefaults] setObject:newDate forKey:@"access_token_expire_time"];
    [[NSUserDefaults standardUserDefaults] setObject:tokenType forKey:@"token_type"];
}
- (void) handleRefreshToken: (NSMutableArray*)root
{}
- (void) parseJSONDataString: (NSString*)jsonStr
{
    NSMutableArray* root = [MTJSON MTJSONStringToObject:jsonStr];
    switch (mCurrentState) {
        case GET_ACCESS_TOKEN:
            [self handleGetAccessToken:root];
            break;
        case REFRESH_TOKEN:
            [self handleRefreshToken:root];
            break;
        default:
            break;
    }
}
- (void) sendOK:(NSData *)recvData
{
    NSString* str = [[NSString alloc] initWithData:recvData encoding:NSUTF8StringEncoding];
    /*
    int dataLen = [recvData length];
    unsigned char* data = new unsigned char[dataLen + 1];
    const unsigned char* srcData = (const unsigned char*)[recvData bytes];
    memcpy(data, srcData, dataLen);
    data[dataLen] = '\0';
     */
    [self parseJSONDataString:str];
    [str release];
}
- (void) sendError:(NSError *)error
{}
@end