//
//  SEConfigFileParser.m
//  PhotoFrame
//
//  Created by 陈勇 on 13-1-16.
//  Copyright (c) 2013年 __MyCompanyName__. All rights reserved.
//

#import "SEConfigFileParser.h"
#import "SEUtil.h"
@implementation SEConfigFileParser

- (id) init
{
    self = [super init];
    if(self)
    {
        mBlocks = [[NSMutableArray array] retain];
    }
    return self;
}
- (void) dealloc
{
    [mBlocks release];
    [super dealloc];
}
- (void) setBlockDefine: (NSArray*)blockTypes
{
    for(int i = 0 ; i < blockTypes.count ; i++)
    {
        [mBlocks addObject:[blockTypes objectAtIndex:i]];
    }
}
- (void) setTarget: (id)target action: (SEL)action
{
    mTarget = target;
    mAction = action;
}
- (void) parse: (NSString*)configName
{
    NSString* data = [SEUtil readDataFromDocumentDir:configName];
    if(data == nil)
    {
        data = [SEUtil readDataFromBundle:configName];
    }
    if(data == nil)
    {
        NSLog(@"## can not find user default manager package : %@ ##\n", configName);
        return;
    }
    NSArray* dataLines = [data componentsSeparatedByString:@"\n"];
    NSString* blockType = nil;
    for(int i = 0 ; i < dataLines.count ; i++)
    {
        NSString* line = [dataLines objectAtIndex:i];
        if([SEUtil isWhitespaceLine:line])
            continue;
        line = [SEUtil stringTrim:line];
        NSUInteger len = line.length;
        unichar firstC = [line characterAtIndex:0];
        unichar lastC = [line characterAtIndex:len - 1];
        if(firstC == '#')
            continue;
        if(firstC == '[' && lastC == ']')
        {
            NSRange range;
            range.location = 1;
            range.length = line.length - 2;
            blockType = [line substringWithRange:range];
            continue;
        }
        assert(blockType != nil);
        NSCharacterSet* cs = [NSCharacterSet whitespaceAndNewlineCharacterSet];
        NSArray* tokens = [line componentsSeparatedByCharactersInSet:cs];
        NSArray* newTokens = [NSArray array];
        for(int i = 0 ; i < tokens.count ; i++)
        {
            NSString* s = [tokens objectAtIndex:i];
            if([SEUtil isWhitespaceLine:s] == NO)
                newTokens = [newTokens arrayByAddingObject:s];
        }
        tokens = newTokens;
        NSArray* param = [NSArray arrayWithObjects:blockType, tokens, nil];
        [mTarget performSelector:mAction withObject:param];        
    }
}
@end
