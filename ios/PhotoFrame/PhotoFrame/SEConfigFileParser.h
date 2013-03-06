//
//  SEConfigFileParser.h
//  PhotoFrame
//
//  Created by 陈勇 on 13-1-16.
//  Copyright (c) 2013年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface SEConfigFileParser : NSObject
{
    NSMutableArray* mBlocks;
    id mTarget;
    SEL mAction;
}
//blockTypes are NSString which is used to define block type
// the string has the same meaning and content as in config file
- (void) setBlockDefine: (NSArray*)blockTypes;
//action must has an NSArray parameter
//the parameter is an array : the first object in array is block type string
//the second is the line of tokens belong to this block type
- (void) setTarget: (id)target action: (SEL)action;
- (void) parse: (NSString*)configName;
@end