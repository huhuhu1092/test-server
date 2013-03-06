//
//  SEKeyChainHelper.m
//  PhotoFrame
//
//  Created by 陈勇 on 13-2-20.
//  Copyright (c) 2013年 __MyCompanyName__. All rights reserved.
//

#import "SEKeyChainHelper.h"
#import "SEMainDisplay.h"
#import "SEOptionsView.h"
#import "SEUITableView.h"
#import "SEUtil.h"
#import "SEResDefine.h"
#import "SESignatureView.h"
#import "SEUserUpgrate.h"
#import "SESystemConfig.h"
#import <CommonCrypto/CommonDigest.h>
#import <CommonCrypto/CommonCryptor.h>
NSString* gUserPrefix = @"com.thespeedsun.PhotoFrame.user.";
NSString* gPwdPrefix = @"com.thespeedsun.PhotoFrame.password.";
NSString* gUUIDStr = @"uuid";
@implementation SEKeyChainHelper
+ (NSMutableDictionary *)getKeyChainQuery:(NSString *)service {  
    return [NSMutableDictionary dictionaryWithObjectsAndKeys:  
            (id)kSecClassGenericPassword,(id)kSecClass,  
            service, (id)kSecAttrService,  
            service, (id)kSecAttrAccount,  
            (id)kSecAttrAccessibleAfterFirstUnlock,(id)kSecAttrAccessible,  
            nil];  
}  

+ (void) saveUserName:(NSString*)userName 
      userNameService:(NSString*)userNameService 
             psaaword:(NSString*)pwd 
      psaawordService:(NSString*)pwdService
{
    NSString* tmpService = [gUserPrefix stringByAppendingString:userNameService];
    NSMutableDictionary *keychainQuery = [self getKeyChainQuery:tmpService];  
    SecItemDelete((CFDictionaryRef)keychainQuery);  
    [keychainQuery setObject:[NSKeyedArchiver archivedDataWithRootObject:userName] forKey:(id)kSecValueData];  
    SecItemAdd((CFDictionaryRef)keychainQuery, NULL); 
    
    tmpService = [gPwdPrefix stringByAppendingString:pwdService];
    keychainQuery = [self getKeyChainQuery:tmpService];  
    SecItemDelete((CFDictionaryRef)keychainQuery);  
    [keychainQuery setObject:[NSKeyedArchiver archivedDataWithRootObject:pwd] forKey:(id)kSecValueData];  
    SecItemAdd((CFDictionaryRef)keychainQuery, NULL); 
}
+ (void) savePassword: (NSString*)pwd pwdService: (NSString*)pwdService
{
    NSString* tmpService = [gPwdPrefix stringByAppendingString:pwdService];
    NSMutableDictionary *keychainQuery = [self getKeyChainQuery:tmpService];  
    SecItemDelete((CFDictionaryRef)keychainQuery);  
    [keychainQuery setObject:[NSKeyedArchiver archivedDataWithRootObject:pwd] forKey:(id)kSecValueData];  
    SecItemAdd((CFDictionaryRef)keychainQuery, NULL); 
}
+ (void) deletePassword: (NSString*) pwdService
{
    NSString* tmpService = [gPwdPrefix stringByAppendingString:pwdService];
    NSMutableDictionary *keychainQuery = [self getKeyChainQuery:tmpService];  
    SecItemDelete((CFDictionaryRef)keychainQuery);
}
+ (void) deleteWithUserNameService:(NSString*)userNameService 
                   psaawordService:(NSString*)pwdService
{
    NSString* tmpService = [gPwdPrefix stringByAppendingString:userNameService];
    NSMutableDictionary *keychainQuery = [self getKeyChainQuery:tmpService];  
    SecItemDelete((CFDictionaryRef)keychainQuery); 
    
    tmpService = [gPwdPrefix stringByAppendingString:pwdService];
    keychainQuery = [self getKeyChainQuery:tmpService];  
    SecItemDelete((CFDictionaryRef)keychainQuery); 
}

+ (NSString*) getUserNameWithService:(NSString*)userNameService
{
    NSString* ret = nil;  
    NSString* tmpService = [gPwdPrefix stringByAppendingString:userNameService];
    NSMutableDictionary *keychainQuery = [self getKeyChainQuery:tmpService];  
    [keychainQuery setObject:(id)kCFBooleanTrue forKey:(id)kSecReturnData];  
    [keychainQuery setObject:(id)kSecMatchLimitOne forKey:(id)kSecMatchLimit];  
    CFDataRef keyData = NULL;  
    if (SecItemCopyMatching((CFDictionaryRef)keychainQuery, (CFTypeRef *)&keyData) == noErr) 
    {  
        @try 
        {  
            ret = [NSKeyedUnarchiver unarchiveObjectWithData:(NSData *)keyData];  
        } 
        @catch (NSException *e) 
        {  
            NSLog(@"Unarchive of %@ failed: %@", userNameService, e);  
        }
        @finally 
        {  
        }  
    }  
    if (keyData)   
        CFRelease(keyData);  
    return ret; 
}

+ (NSString*) getPasswordWithService:(NSString*)pwdService
{
    NSString* ret = nil;  
    NSString* tmpService = [gPwdPrefix stringByAppendingString:pwdService];
    NSMutableDictionary *keychainQuery = [self getKeyChainQuery:tmpService];  
    [keychainQuery setObject:(id)kCFBooleanTrue forKey:(id)kSecReturnData];  
    [keychainQuery setObject:(id)kSecMatchLimitOne forKey:(id)kSecMatchLimit];  
    CFDataRef keyData = NULL;  
    if (SecItemCopyMatching((CFDictionaryRef)keychainQuery, (CFTypeRef *)&keyData) == noErr) 
    {  
        @try 
        {  
            ret = [NSKeyedUnarchiver unarchiveObjectWithData:(NSData *)keyData];  
        } 
        @catch (NSException *e) 
        {  
            NSLog(@"Unarchive of %@ failed: %@", pwdService, e);  
        }
        @finally 
        {  
        }  
    }  
    if (keyData)   
        CFRelease(keyData);  
    return ret;
}
+ (NSString*) stringWithCharArray: (NSArray*) array
{
    NSString* s = @"";
    for(int i = 0 ; i < array.count ; i++)
    {
        NSNumber* num = [array objectAtIndex:i];
        s = [s stringByAppendingFormat:@"%c", [num unsignedCharValue ]];
    }
    return s;
}
+ (NSString*) createMD5: (NSString*)str
{
    const char* cStrValue = [str UTF8String];
    unsigned char theResult[CC_MD5_DIGEST_LENGTH];
    CC_MD5(cStrValue, strlen(cStrValue), theResult);
    
    return [NSString stringWithFormat:@"%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X",
            theResult[0], theResult[1], theResult[2], theResult[3],
            theResult[4], theResult[5], theResult[6], theResult[7],
            theResult[8], theResult[9], theResult[10], theResult[11],
            theResult[12], theResult[13], theResult[14], theResult[15]];
     
}

+ (NSString*) createMyKey
{
    //don't disturb the sequence from r1 -- r8, it will create different key
    NSArray* r1 = [SEOptionsAboutSetting getFeedChar];
    NSArray* r2 = [SEMainDisplay getFeedChar];
    NSArray* r3 = [SEFontLoader  getFeedChar];
    NSArray* r4 = [SESignatureView getFeedChar];
    NSArray* r5 = [SESystemConfig getFeedChar];
    NSArray* r6 = [SEUITableView getFeedChar];
    NSArray* r7 = [SEUtil getFeedChar];
    NSArray* r8 = [SEUserUpgrade getFeedChar];
    NSString* s1 = [SEKeyChainHelper stringWithCharArray:r1];
    NSString* s2 = [SEKeyChainHelper stringWithCharArray:r2];
    NSString* s3 = [SEKeyChainHelper stringWithCharArray:r3];
    NSString* s4 = [SEKeyChainHelper stringWithCharArray:r4];
    NSString* s5 = [SEKeyChainHelper stringWithCharArray:r5];
    NSString* s6 = [SEKeyChainHelper stringWithCharArray:r6];
    NSString* s7 = [SEKeyChainHelper stringWithCharArray:r7];
    NSString* s8 = [SEKeyChainHelper stringWithCharArray:r8];
    /*
     NSString* a[] = {s1, s2, s3, s4, s5, s6, s7, s8};
    for(int i = 0 ; i < 8 ; i++)
    {
        NSLog(@"%@ ", a[i]);
    }
     */
    NSString* ret = [NSString stringWithFormat:@"%@%@%@%@%@%@%@%@", s1, s2, s3, s4, s5, s6, s7, s8];
    //NSLog(@"ret key = %@", ret);
    return ret;
}
+ (NSString*) createMyMD5Key
{
    NSString* key = [SEKeyChainHelper createMyKey];
    return [SEKeyChainHelper createMD5:key];
}
+ (NSData*) AES256EncryptWithKey: (NSString*)key : (NSData*)srcData
{
    char keyPtr[kCCKeySizeAES256 + 1];
    bzero(keyPtr, sizeof(keyPtr));
    [key getCString:keyPtr maxLength:sizeof(keyPtr) encoding:NSUTF8StringEncoding];
    NSUInteger dataLength = [srcData length];
    size_t bufferSize  = dataLength + kCCBlockSizeAES128;
    void* buffer = malloc(bufferSize);
    size_t numBytesEncrypted    = 0;
    CCCryptorStatus cryptStatus = CCCrypt(kCCEncrypt, kCCAlgorithmAES128, kCCOptionPKCS7Padding,
                                          keyPtr, kCCKeySizeAES256,
                                          NULL /* initialization vector (optional) */,
                                          [srcData bytes], dataLength, /* input */
                                          buffer, bufferSize, /* output */
                                          &numBytesEncrypted);
    NSData* retData = nil;
    if (cryptStatus == kCCSuccess)
    {
        retData = [NSData dataWithBytesNoCopy:buffer length:numBytesEncrypted];
    }
    else
    {
        free(buffer);
    }
    return retData;
}
+ (NSData*)AES256DecryptWithKey:(NSString*)key : (NSData*)srcData
{
    
    char keyPtr[kCCKeySizeAES256 + 1]; // room for terminator (unused)
    bzero(keyPtr, sizeof(keyPtr)); // fill with zeroes (for padding)
    
    // fetch key data
    [key getCString:keyPtr maxLength:sizeof(keyPtr) encoding:NSUTF8StringEncoding];
    
    NSUInteger dataLength = [srcData length];
    
    size_t bufferSize           = dataLength + kCCBlockSizeAES128;
    void* buffer                = malloc(bufferSize);
    
    size_t numBytesDecrypted    = 0;
    CCCryptorStatus cryptStatus = CCCrypt(kCCDecrypt, kCCAlgorithmAES128, kCCOptionPKCS7Padding,
                                          keyPtr, kCCKeySizeAES256,
                                          NULL /* initialization vector (optional) */,
                                          [srcData bytes], dataLength, /* input */
                                          buffer, bufferSize, /* output */
                                          &numBytesDecrypted);
    NSData* retData = nil;
    if (cryptStatus == kCCSuccess)
    {
        retData = [NSData dataWithBytesNoCopy: buffer length:numBytesDecrypted];
    }
    else
    {
        free(buffer); //free the buffer;
    }
    return retData;
}
+ (NSString*) myUUID
{
    NSString* tmpService = [NSString stringWithFormat:@"%@%@", gPwdPrefix, gUUIDStr];
    NSString* uuid = [SEKeyChainHelper getPasswordWithService:tmpService];
    if(uuid)
    {
        return uuid;
    }
    CFUUIDRef puuid = CFUUIDCreate( nil );
    CFStringRef uuidString = CFUUIDCreateString( nil, puuid );
    NSString * result = (NSString *)CFStringCreateCopy( NULL, uuidString);
    CFRelease(puuid);
    CFRelease(uuidString);
    [SEKeyChainHelper savePassword:result pwdService:tmpService];
    [result release];
    uuid = [SEKeyChainHelper getPasswordWithService:tmpService];
    assert(uuid != nil);
    return uuid;
}
#define MY_INT 0
#define MY_FLOAT 1
+ (NSData*) createMyStringFromStringValue: (NSString*)s
{
    NSString* key = [SEKeyChainHelper createMyMD5Key];
    NSString* uuid = [SEKeyChainHelper myUUID];
    NSString* dataStr = [NSString stringWithFormat:@"%@%@", uuid, s];
    assert(dataStr != nil);
    NSData* data = [dataStr dataUsingEncoding:NSUTF8StringEncoding];
    NSData* encryptData = [SEKeyChainHelper AES256EncryptWithKey:key :data];
    return encryptData;
}
+ (BOOL) getStringValueFromString: (NSData*) srcData : (NSString**)s
{
    NSString* key = [SEKeyChainHelper createMyMD5Key];
    NSData* data = [SEKeyChainHelper AES256DecryptWithKey:key :srcData];
    NSString* uuid = [SEKeyChainHelper myUUID];
    NSString* valueStr = [[[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding] autorelease];
    NSRange range = [valueStr rangeOfString:uuid];
    if(range.location == NSNotFound)
    {
        return NO;
    }
    if(range.location != 0)
    {
        return NO;
    }
    NSString* realValueStr = [valueStr substringFromIndex:range.length];
    *s = realValueStr;
    return YES;
}
+ (NSData*) createMyStringFromContent: (NSNumber*)v : (int) type
{
    NSString* key = [SEKeyChainHelper createMyMD5Key];
    NSString* uuid = [SEKeyChainHelper myUUID];
    NSString* dataStr = nil;
    switch (type)
    {
        case MY_INT:
        {
            int n = [v intValue];
            dataStr = [NSString stringWithFormat:@"%@%d", uuid, n];
        }
            break;
        case MY_FLOAT:
        {
            float n = [v floatValue];
            dataStr = [NSString stringWithFormat:@"%@%f", uuid, n];
        }
            break;
        default:
            break;
    }
    assert(dataStr != nil);
    //NSUInteger size = [uuid length];
    NSData* data = [dataStr dataUsingEncoding:NSUTF8StringEncoding];
    NSData* encryptData = [SEKeyChainHelper AES256EncryptWithKey:key :data];
    return encryptData;
    /*
    NSUInteger allDataSize = sizeof(int) + encryptData.length;
    UInt8* outData = (UInt8*)malloc(allDataSize);
    NSOutputStream* output = [NSOutputStream outputStreamToBuffer:outData capacity:allDataSize];
    [output open];
    [output write:(const uint8_t *)(&size) maxLength:sizeof(NSUInteger)];
    int encryptDataLen = [encryptData length];
    [output write:(const uint8_t *)[encryptData bytes] maxLength: encryptDataLen];
    [output close];
    NSData* newData = [NSData dataWithBytes:outData length:allDataSize];
    free(outData);
    NSLog(@"new Data len = %u", newData.length);
    return newData;
     */
}
+ (NSNumber*) getContentFromString: (NSData*)str type: (int) type
{
    NSData* srcData = str;
    /*
    NSInputStream* inputStream = [NSInputStream inputStreamWithData:srcData];
    [inputStream open];
    int inputUUIDLen = 0;
    int ret = [inputStream read:(uint8_t*)&inputUUIDLen maxLength:sizeof(int)];
    assert(ret != -1);
    uint8_t* strData = (uint8_t*)malloc(srcData.length - sizeof(int));
    int strDataLen = srcData.length - sizeof(int);
    ret = [inputStream read:strData maxLength:strDataLen];
    assert(ret != -1);

    NSData* encryptData = [NSData dataWithBytes:strData length:strDataLen];
    free(strData);
    [inputStream close];
     */
    NSString* key = [SEKeyChainHelper createMyMD5Key];
    NSData* data = [SEKeyChainHelper AES256DecryptWithKey:key :srcData];
    NSString* uuid = [SEKeyChainHelper myUUID];
    NSString* valueStr = [[[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding] autorelease];
    NSRange range = [valueStr rangeOfString:uuid];
    if(range.location == NSNotFound)
    {
        return nil;
    }
    if(range.location != 0)
    {
        return nil;
    }
    NSString* realValueStr = [valueStr substringFromIndex:range.length];
    NSNumber* num = nil;
    switch (type) {
        case MY_INT:
        {
            int v = [realValueStr intValue];
            num = [NSNumber numberWithInt:v];
        }
            break;
        case MY_FLOAT:
        {
            float v = [realValueStr floatValue];
            num = [NSNumber numberWithFloat:v];
        }
            break;
        default:
            break;
    }
    return num;
}
+ (NSData*) createMyStringFromIntValue: (int) v
{
    return [SEKeyChainHelper createMyStringFromContent:[NSNumber numberWithInt:v] :MY_INT];

}
     
+ (BOOL) getIntValueFromString: (NSData*) str : (int*)v
{
    NSNumber* num = [SEKeyChainHelper getContentFromString:str type:MY_INT];
    if(num)
    {
        *v = [num intValue];
        return YES;
    }
    else
    {
        return NO;
    }

}
+ (NSData*) createMyStringFromBoolValue: (BOOL)v
{
    int n = v ? 1 : 0;
    return [SEKeyChainHelper createMyStringFromIntValue:n];
}
+ (BOOL) getBoolValueFromString:(NSData*)str : (BOOL*)v
{
    NSNumber* num = [SEKeyChainHelper getContentFromString:str type:MY_INT];
    if(num)
    {
        int n = [num intValue];
        *v = (n == 1);
        return YES;
    }
    else
    {
        return NO;
    }

}
+ (NSData*) createMyStringFromFloatValue: (float)v
{
    return [SEKeyChainHelper createMyStringFromContent:[NSNumber numberWithFloat:v] :MY_FLOAT];
}
+ (BOOL) getFloatValueFromString: (NSData*)str : (float*)v
{
    NSNumber* num = [SEKeyChainHelper getContentFromString:str type:MY_FLOAT];
    if(num)
    {
        *v = [num floatValue];
        return YES;
    }
    else
    {
        return NO;
    }
}
@end
