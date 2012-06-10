//
//  SEImageAsyncLoader.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-5-12.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEImageAsyncLoader.h"
#import <AssetsLibrary/AssetsLibrary.h>
#import "SEUtil.h"
#import "SEViewNavigator.h"
#import "PhotoFrameAppDelegate.h"
@implementation SEImageAsyncLoader
@synthesize mViewNav;
- (void) dealloc
{
    if(mOwn)
        [mAssetLib release];
    [super dealloc];
}
- (void) setAssetLibOwn:(ALAssetsLibrary *)lib
{
    mOwn = YES;
    mAssetLib = [lib retain];
}
- (void) setAssetLibNotOwn:(ALAssetsLibrary *)lib
{
    mOwn = NO;
    mAssetLib = lib;
}
- (void) loadImageHandle: (id)data
{
    NSObject<SELoadedImageHandler>* handler = (NSObject<SELoadedImageHandler>*)data;
    [handler handleImage];
    [handler release];
}
- (void) loadImageFromPhotoLibWithThread:(NSMutableArray*)data
{
    NSAutoreleasePool* newPool = [[NSAutoreleasePool alloc] init];
    NSURL* url = [data objectAtIndex:0];
    CGSize size = [[data objectAtIndex:1] CGSizeValue];
    NSObject<SELoadedImageHandler>* handler = [data objectAtIndex:2];
    NSURL* passedURL = [url retain];
    [data release];
    ALAssetsLibraryAssetForURLResultBlock getAsset = ^(ALAsset *asset)
    {
        NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
        NSThread* currentThread = [NSThread currentThread];
        NSLog(@"current thread = %@\n", currentThread);
        ALAssetRepresentation* rep = [asset defaultRepresentation];
        CGImageRef image = [rep fullResolutionImage];
        ALAssetOrientation orient = [rep orientation];
        CGFloat scale = [rep scale];
        CGSize srcS = CGSizeMake(CGImageGetWidth(image), CGImageGetHeight(image));
        CGSize s = [SEUtil computeFitSize:srcS toDst:size];
        CGImageRef retImage = [SEUtil fastScale:image withRect:s];
        float width = CGImageGetWidth(retImage);
        float height = CGImageGetHeight(retImage);
        NSLog(@"## retImage with = %f, height = %f ##\n", width, height);
        UIImageOrientation o = (UIImageOrientation)orient;
        UIImage* uiImage = [UIImage imageWithCGImage:retImage scale:scale orientation:o];
        CGImageRelease(retImage);
        [handler setImage:uiImage];
        [passedURL release];
        [self performSelectorOnMainThread:@selector(loadImageHandle:) withObject:handler waitUntilDone:NO];
        [pool release];
        
    };
    ALAssetsLibraryAccessFailureBlock failHandler = ^(NSError *error)
    {
        if(error)
        {
            NSLog(@"read photo lib error : %@", [error localizedDescription]);
        }
    };
    [mAssetLib assetForURL:passedURL resultBlock:getAsset failureBlock:failHandler];
    [newPool release];
}
- (void) loadImageFromPhotoLib: (NSURL*)url size:(CGSize)size withHandler: (NSObject<SELoadedImageHandler>*) handler
{
    NSMutableArray* data = [NSMutableArray array];
    [data addObject:url];
    [data addObject:[NSValue valueWithCGSize:size]];
    [data addObject:handler];
    data = [data retain];
    [self performSelectorInBackground:@selector(loadImageFromPhotoLibWithThread:) withObject:data];
}
- (void) getFullRepresentationWithThread:(NSMutableArray*) data
{
    NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
    NSURL* passedURL = [[data objectAtIndex:0] retain];
    NSObject<SELoadedImageHandler>* handler = [data objectAtIndex:1];
    [data release];
    ALAssetsLibraryAssetForURLResultBlock getAsset = ^(ALAsset *asset)
    {
        NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
        ALAssetRepresentation* rep = [asset defaultRepresentation];
        CGImageRef image = [rep fullResolutionImage];
        UIImageOrientation orient = (UIImageOrientation)[rep orientation];
        CGFloat scale = [rep scale];
        UIImage* uiImage = [UIImage imageWithCGImage:image scale:scale orientation:orient];
        [handler setImage:uiImage];
        [handler preHandleImage];
        [self performSelectorOnMainThread:@selector(loadImageHandle:) withObject:handler waitUntilDone:NO];
        [passedURL release];
        [pool release];
    };
    ALAssetsLibraryAccessFailureBlock failHandler = ^(NSError *error)
    {
        if(error)
        {
            NSLog(@"read photo lib error : %@", [error localizedDescription]);
        }
    };
    [mAssetLib assetForURL:passedURL resultBlock:getAsset failureBlock:failHandler];    
    [pool release];
}

- (void) getCoreDataFullRepresentationWithThread:(NSMutableArray*) data
{
    NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
    NSURL* passedURL = [[data objectAtIndex:0] retain];
    NSObject<SELoadedImageHandler>* handler = [data objectAtIndex:1];
    NSString* date = [data objectAtIndex:2];
    [date retain];
    [data release];
    NSManagedObjectContext* moc = [[NSManagedObjectContext alloc] init];
    [moc setPersistentStoreCoordinator:mViewNav.persistentStoreCoordinator];
    UIImage* image = [mViewNav getImageFromCoreData: [passedURL absoluteString] urlDate: date managedObjectContext:moc];
    [moc release];
    if(image != nil)
    {
        [handler setImage:image];
        [handler preHandleImage];
        [self performSelectorOnMainThread:@selector(loadImageHandle:) withObject:handler waitUntilDone:NO];
    }
    else 
    {
        NSMutableArray* data = [NSMutableArray array];
        [data retain];
        [data addObject:passedURL];
        [data addObject:handler];
        [self performSelectorInBackground:@selector(getFullRepresentationWithThread:) withObject:data];
    }
    [passedURL release];
    [date release];
    [pool release];
}
- (void) loadFullRepresentation: (NSURL*)url withHandler: (NSObject<SELoadedImageHandler>*)handler
{
    NSMutableArray* data = [NSMutableArray array];
    [data addObject:url];
    [data addObject:handler];
    data = [data retain];
    [self performSelectorInBackground:@selector(getFullRepresentationWithThread:) withObject:data];
}
- (void) loadCoreDataFullRepresentation: (NSURL*)url date: (NSString*)date withHandler: (NSObject<SELoadedImageHandler>*)handler
{
    NSMutableArray* data = [NSMutableArray array];
    [data addObject:url];
    [data addObject:handler];
    [data addObject:date];
    data = [data retain];
    [self performSelectorInBackground:@selector(getCoreDataFullRepresentationWithThread:) withObject:data];
}

@end
