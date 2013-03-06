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
@class SEPageImageURL;

@interface SEImageAsyncLoader : NSObject
{
    ALAssetsLibrary* mAssetLib;
    BOOL mOwn;
    SEViewNavigator* mViewNav;
    
}
@property (nonatomic, assign) SEViewNavigator* mViewNav;
- (void) setAssetLibOwn: (ALAssetsLibrary*) lib;
- (void) setAssetLibNotOwn: (ALAssetsLibrary*)lib;
//if size.width == 0 && size.height == 0 we will load full size of image
//else will scale original image to indicated size
- (void) loadImageFromPhotoLib: (NSURL*)url size:(CGSize)size withHandler: (NSObject<SELoadedImageHandler>*) handler;
- (void) loadImageFromPhotoLibWithRatio: (NSURL*)url size: (CGSize)destSize withHandler: (NSObject<SELoadedImageHandler>*) handler;
- (void) loadImageThumbnailFromPhotoLib: (NSURL*)url size: (CGSize) size withHandler: (NSObject<SELoadedImageHandler>*) handler;
- (void) loadImageThumbnailFromCoreData: (NSURL*) url date: (NSString*) date withHandler: (NSObject<SELoadedImageHandler>*)handler;
- (void) loadFullRepresentation: (NSURL*)url withHandler: (NSObject<SELoadedImageHandler>*)handler;
- (void) loadCoreDataFullRepresentation: (NSURL*)url date: (NSString*) date withHandler: (NSObject<SELoadedImageHandler>*)handler;
//if size.width == 0 && size.height == 0 we will load full size of image
//else will scale original image to indicated size
- (void) loadCoreDataImage: (NSURL*)url date: (NSString*) date size: (CGSize) size withHandler: (NSObject<SELoadedImageHandler>*)handler;
- (void) loadCoreDataImageWithRatio: (NSURL*)url date: (NSString*) date size: (CGSize) size withHandler: (NSObject<SELoadedImageHandler>*)handler;
@end

@interface SEImageAsyncLoadHandler : NSObject<SELoadedImageHandler>
{
    SEImageAsyncLoader* imageAsyncLoader;
}
- (void) setAssetLibOwn: (ALAssetsLibrary*) lib;
- (void) setAssetLibNotOwn: (ALAssetsLibrary*)lib;
//if size.width == 0 && size.height == 0 we will load full size of image
//else will scale original image to indicated size
- (void) loadImageFromPhotoLib: (NSURL*)url size:(CGSize)size;
- (void) loadImageFromPhotoLibWithRatio: (NSURL*)url size: (CGSize)destSize;
- (void) loadImageThumbnailFromPhotoLib: (NSURL*)url size: (CGSize) size;
- (void) loadImageThumbnailFromCoreData: (NSURL*)url date: (NSString*) date;
- (void) loadFullRepresentation: (NSURL*)url;
- (void) loadCoreDataFullRepresentation: (NSURL*)url date: (NSString*) date;
//if size.width == 0 && size.height == 0 we will load full size of image
//else will scale original image to indicated size
- (void) loadCoreDataImage: (NSURL*)url date: (NSString*) date size: (CGSize) size;
- (void) loadCoreDataImageWithRatio: (NSURL*)url date: (NSString*) date size: (CGSize) size;
@end
