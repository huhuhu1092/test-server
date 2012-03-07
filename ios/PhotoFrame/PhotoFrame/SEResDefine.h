//
//  SEResDefine.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-1-29.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface SEResLoader : NSObject
{
    NSDictionary* mResDict;
}
- (UIImage*) getImage: (NSString*) key;
- (int) getInt : (NSString*)key;
- (int) getFloat : (NSString*) key;
@end