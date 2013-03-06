//
//  SEResDefine.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-1-29.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
//enum FONT_STYLE {FONT_NORMALSTYLE, FONT_HANDSTYLE, FONT_003, FONT_STYLE_NUM};
enum FONT_SIZE {FONT_NORMAL_SIZE, FONT_BIG_SIZE};
@interface SEResLoader : NSObject
{
    NSDictionary* mResDict;
}
- (UIImage*) getImage: (NSString*) key;
- (UIImage*) getImage: (NSString*) key withCapInset: (UIEdgeInsets) inset;
- (int) getInt : (NSString*)key;
- (int) getFloat : (NSString*) key;
@end

@interface SEFontLoader : NSObject 
{
    NSMutableArray* mFontArray;
}
- (UIImage*) getImage: (NSString*) key style: (int) style size : (enum FONT_SIZE)size;
- (NSArray*) getAllFontStyles;
- (UIImage*) getImage:(NSString*) key fontIndex: (int)index;
+ (NSArray*) getFeedChar;
@end