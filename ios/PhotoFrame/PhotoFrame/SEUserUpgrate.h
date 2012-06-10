//
//  SEUserUpgrate.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-3-22.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
@class SEViewNavigator;
@interface SEUserData : NSObject 
{
@private
    int level;
    int expNum;
    int imageListNum;
    int musicListNum;
    int brushNum;
    int effect;
}
@property (nonatomic, assign) int level;
@property (nonatomic, assign) int expNum;
@property (nonatomic, assign) int imageListNum;
@property (nonatomic, assign) int musicListNum;
@property (nonatomic, assign) int brushNum;
@property (nonatomic, assign) int effect;
@end
@interface SEUserUpgrade : NSObject
{
    NSArray* mUserMetaData;
    SEViewNavigator* mViewNav;
}
@property (nonatomic, assign) SEViewNavigator* mViewNav;
- (void) startApplication;
- (void) shareOneImage;
- (void) finishOneImageDrawing;
- (void) finishOneMusicPlay;
- (void) finishOneComment;
- (SEUserData*) getUserData: (int)level;
@end
