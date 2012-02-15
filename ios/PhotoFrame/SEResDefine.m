//
//  SEResDefine.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-1-29.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEResDefine.h"

@implementation SEResLoader
- (id) init
{
    self = [super init];
    if(self)
    {
        NSString* path = [[NSBundle mainBundle] pathForResource:@"ViewProperty" ofType:@"plist"];
        mResDict = [[NSDictionary alloc] initWithContentsOfFile:path];
    }
    return self;
}
- (void) dealloc
{
    [mResDict release];
    [super dealloc];
}

- (UIImage*) getImage: (NSString*) key
{
    NSString* name = [mResDict objectForKey:key];
    return [UIImage imageNamed:name];    
}
- (int) getInt : (NSString*)key
{
    NSNumber* num = [mResDict objectForKey:key];
    return [num intValue];
}
- (int) getFloat : (NSString*) key
{
    NSNumber* num = [mResDict objectForKey:key];
    return [num floatValue];
}
@end