//
//  SEImageAsyncLoader.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-5-12.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "SEProtocalDefine.h"
@class ALAssetsLibrary;
@class SEViewNavigator;
@interface SEImageAsyncLoader : NSObject
{
    ALAssetsLibrary* mAssetLib;
    BOOL mOwn;
    SEViewNavigator* mViewNav;
    
}
@property (nonatomic, assign) SEViewNavigator* mViewNav;
- (void) setAssetLibOwn: (ALAssetsLibrary*) lib;
- (void) setAssetLibNotOwn: (ALAssetsLibrary*)lib;
- (void) loadImageFromPhotoLib: (NSURL*)url size:(CGSize)size withHandler: (NSObject<SELoadedImageHandler>*) handler;
- (void) loadFullRepresentation: (NSURL*)url withHandler: (NSObject<SELoadedImageHandler>*)handler;
- (void) loadCoreDataFullRepresentation: (NSURL*)url date: (NSString*) date withHandler: (NSObject<SELoadedImageHandler>*)handler;
@end
