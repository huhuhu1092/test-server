//
//  SEResDefine.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-1-29.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
enum FONT_STYLE {FONT_NORMALSTYLE, FONT_HANDSTYLE};
enum FONT_SIZE {FONT_NORMAL_SIZE, FONT_BIG_SIZE};
@interface SEResLoader : NSObject
{
    NSDictionary* mResDict;
}
- (UIImage*) getImage: (NSString*) key;
- (int) getInt : (NSString*)key;
- (int) getFloat : (NSString*) key;
@end

@interface SEFontLoader : NSObject 
{
    NSMutableArray* mFontArray;
}
- (UIImage*) getImage: (NSString*) key style: (enum FONT_STYLE) style size : (enum FONT_SIZE)size;
@end