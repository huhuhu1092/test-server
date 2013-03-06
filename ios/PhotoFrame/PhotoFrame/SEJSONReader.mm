//
//  SEJSONReader.m
//  TestJSON
//
//  Created by 陈勇 on 12-9-27.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEJSONReader.h"
#import "yajl_parse.h"
#import "yajl_gen.h"
#import <CommonCrypto/CommonDigest.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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

static int getFileLen(FILE* fp)
{
    int        pos;
    int        end;
    
    pos = ftell (fp);
    fseek (fp, 0, SEEK_END);
    end = ftell (fp);
    fseek (fp, pos, SEEK_SET);
    
    return end;
    
}
@implementation SEJSONReader
- (void) parseJSONData: (const unsigned char*)buffer len: (int)len
{
    yajl_handle hand;
    //static unsigned char fileData[65536];
    /* generator config */
    yajl_gen g;
    yajl_status stat;
    size_t rd = len;
    int retval = 0;
    
    g = yajl_gen_alloc(NULL);
    yajl_gen_config(g, yajl_gen_beautify, 1);
    yajl_gen_config(g, yajl_gen_validate_utf8, 1);
    
    /* ok.  open file.  let's read and parse */
    hand = yajl_alloc(&callbacks, NULL, (void *) g);
    /* and let's allow comments by default */
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
}
- (void) parseJSON:(NSString*)fileName
{
    const char* cFileName = [fileName cStringUsingEncoding:NSASCIIStringEncoding];
    
    FILE* inFile = fopen(cFileName, "r");
    if(inFile == NULL)
        return;
    int len = getFileLen(inFile);
    unsigned char* buffer = new unsigned char[len + 1];
    if(fread(buffer, len, 1, inFile) != 1)
    {
        NSLog(@"read file error\n");
        return;
    }
    buffer[len] = '\0';
    yajl_handle hand;
    //static unsigned char fileData[65536];
    /* generator config */
    yajl_gen g;
    yajl_status stat;
    size_t rd = len;
    int retval = 0;

    g = yajl_gen_alloc(NULL);
    yajl_gen_config(g, yajl_gen_beautify, 1);
    yajl_gen_config(g, yajl_gen_validate_utf8, 1);
    
    /* ok.  open file.  let's read and parse */
    hand = yajl_alloc(&callbacks, NULL, (void *) g);
    /* and let's allow comments by default */
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

}
-(NSString *)md5:(NSString *)str { 
    const char *cStr = [str UTF8String]; 
    unsigned char result[32]; 
    CC_MD5( cStr, strlen(cStr), result ); 
    return [NSString stringWithFormat: 
            @"%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X",
            result[0], result[1], result[2], result[3], 
            result[4], result[5], result[6], result[7], 
            result[8], result[9], result[10], result[11], 
            result[12], result[13], result[14], result[15] 
            ]; 
}
- (void) parseURL:(NSURL*) url
{
    NSMutableURLRequest* req = [NSMutableURLRequest requestWithURL:url];
    [req setHTTPMethod:@"POST"];
    NSMutableData* postBody = [NSMutableData data];
    [postBody appendData:[[NSString stringWithFormat:@"login_name=%@&", @"13718215879"] dataUsingEncoding:NSUTF8StringEncoding]];
    NSString* password = [self md5:@"123456"];
    [postBody appendData:[[NSString stringWithFormat:@"password=%@&", password] dataUsingEncoding:NSUTF8StringEncoding]];
    [req setHTTPBody:postBody];
    NSURLConnection* conn = [NSURLConnection connectionWithRequest:req delegate:self];
    [conn retain];
}
- (id) init
{
    self = [super init];
    if(self)
    {
        mRecvData = [[NSMutableData data] retain];
    }
    return self;
}
- (void) dealloc
{
    [mRecvData release];
    [super dealloc];
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
    [connection release];
}
- (void)connectionDidFinishLoading:(NSURLConnection *)connection
{
    NSLog(@"finished connection");
    int dataLen = [mRecvData length];
    unsigned char* data = new unsigned char[dataLen + 1];
    const unsigned char* srcData = (const unsigned char*)[mRecvData bytes];
    memcpy(data, srcData, dataLen);
    data[dataLen] = '\0';
    [self parseJSONData:data len:dataLen];
    [connection release];
    
}

@end
