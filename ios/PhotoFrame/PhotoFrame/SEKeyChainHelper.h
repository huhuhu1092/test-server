//
//  SEKeyChainHelper.h
//  PhotoFrame
//
//  Created by 陈勇 on 13-2-20.
//  Copyright (c) 2013年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface SEKeyChainHelper : NSObject
+ (void) saveUserName:(NSString*)userName 
      userNameService:(NSString*)userNameService 
             psaaword:(NSString*)pwd 
      psaawordService:(NSString*)pwdService;
+ (void) savePassword: (NSString*)pwd pwdService: (NSString*)pwdService;
+ (void) deletePassword: (NSString*) pwdService;

+ (void) deleteWithUserNameService:(NSString*)userNameService 
                   psaawordService:(NSString*)pwdService;

+ (NSString*) getUserNameWithService:(NSString*)userNameService;

+ (NSString*) getPasswordWithService:(NSString*)pwdService;

//+ (NSString*) createMyKey;
+ (NSData*) createMyStringFromStringValue: (NSString*)s;
+ (BOOL) getStringValueFromString: (NSData*) data : (NSString**)s;
+ (NSData*) createMyStringFromIntValue: (int) v;
+ (BOOL) getIntValueFromString: (NSData*) str : (int*)v;
+ (NSData*) createMyStringFromFloatValue: (float)v;
+ (BOOL) getFloatValueFromString: (NSData*)str : (float*)v;
+ (NSData*) createMyStringFromBoolValue: (BOOL)v;
+ (BOOL) getBoolValueFromString:(NSData*)str : (BOOL*)v;
@end
