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
#import "SESystemConfig.h"
@implementation SEImageAsyncLoader
@synthesize mViewNav;
- (void) dealloc
{
    NSLog(@"SEImageAsyncLoader dealloc");
    if(mOwn)
        [mAssetLib release];
    [super dealloc];
}
- (void) setAssetLibOwn:(ALAssetsLibrary *)lib
{
    mOwn = YES;
    [mAssetLib release];
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
    BOOL bFullRep = [[data objectAtIndex:3] boolValue];
    int op = [[data objectAtIndex:4] intValue];
    NSURL* passedURL = [url retain];
    [data release];
    ALAssetsLibraryAssetForURLResultBlock getAsset = ^(ALAsset *asset)
    {
        NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
        NSThread* currentThread = [NSThread currentThread];
        NSLog(@"current thread = %@\n", currentThread);
        ALAssetRepresentation* rep = [asset defaultRepresentation];
        /*
        NSDictionary* metaData = [rep metadata];
        NSArray* keys = [metaData allKeys];
        NSArray* objects = [metaData allValues];
        for(int i = 0 ;i < keys.count ; i++)
        {
            NSLog(@"## key = %@ , value = %@ ##", [keys objectAtIndex:i], [objects objectAtIndex:i]);
        }
         */
        if(bFullRep)
        {
            CGImageRef image = NULL;
            if(rep)
            {
                image = [rep fullResolutionImage];
            }
            ALAssetOrientation orient = [rep orientation];
            CGFloat scale = [rep scale];
            CGSize srcS = CGSizeMake(CGImageGetWidth(image), CGImageGetHeight(image));
            CGSize s;
            if(op == 0)
            {
                s = [SEUtil computeFitSize:srcS toDst:size];
            }
            else
            {
                s = [SEUtil calculateImageSizeByRatio:srcS dstSize:size];    
            }
            CGImageRef retImage = NULL;
            if(image)
            {
                retImage = [SEUtil fastScale:image withRect:s];
            }
            float width = CGImageGetWidth(retImage);
            float height = CGImageGetHeight(retImage);
            NSLog(@"## retImage with = %f, height = %f ##\n", width, height);
            UIImageOrientation o = (UIImageOrientation)orient;
            UIImage* uiImage = nil;
            if(retImage)
            {
                uiImage =  [UIImage imageWithCGImage:retImage scale:scale orientation:o];
                CGImageRelease(retImage);
            }
            
            [handler setImage:uiImage];
            [handler preHandleImage];
        }
        else
        {
            CGImageRef image = NULL;
            if(asset)
            {
                image = [asset thumbnail];
            }
            UIImage* uiImage = nil;
            if(image)
            {
                uiImage = [UIImage imageWithCGImage:image];
            }
            
            [handler setImage:uiImage];
            [handler preHandleImage];
        }
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
    [data addObject:[NSNumber numberWithBool:YES]];
    [data addObject:[NSNumber numberWithInt:0]];// use scale to dest size
    data = [data retain];
    [self performSelectorInBackground:@selector(loadImageFromPhotoLibWithThread:) withObject:data];
}
- (void) loadImageFromPhotoLibWithRatio: (NSURL*)url size: (CGSize)destSize withHandler: (NSObject<SELoadedImageHandler>*) handler
{
    NSMutableArray* data = [NSMutableArray array];
    [data addObject:url];
    [data addObject:[NSValue valueWithCGSize:destSize]];
    [data addObject:handler];
    [data addObject:[NSNumber numberWithBool:YES]];
    [data addObject:[NSNumber numberWithInt:0]]; //use ratio to scale to dest size;
    data = [data retain];
    [self performSelectorInBackground:@selector(loadImageFromPhotoLibWithThread:) withObject:data];
}
- (void) loadImageThumbnailFromPhotoLib: (NSURL*)url size: (CGSize) size withHandler: (NSObject<SELoadedImageHandler>*) handler
{
    NSMutableArray* data = [NSMutableArray array];
    [data addObject:url];
    [data addObject:[NSValue valueWithCGSize:size]];
    [data addObject:handler];
    [data addObject:[NSNumber numberWithBool:NO]];
    [data addObject:[NSNumber numberWithInt:0]];
    data = [data retain];
    [self performSelectorInBackground:@selector(loadImageFromPhotoLibWithThread:) withObject:data];
}
- (void) getFullRepresentationWithThread:(NSMutableArray*) data
{
    NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
    NSURL* passedURL = [[data objectAtIndex:0] retain];
    NSObject<SELoadedImageHandler>* handler = [data objectAtIndex:1];
    NSValue* v = [data objectAtIndex:2];
    int op = [[data objectAtIndex:3] intValue];
    CGSize s = [v CGSizeValue];
    NSLog(@"load full representation s.width = %f, s.height = %f", s.width, s.height);
    [data release];
    NSLog(@"url = %@", [passedURL absoluteString]);
    if([SESystemConfig isDefaultSelectedImageURL: [passedURL absoluteString] ])
    {
        UIImage* image = [SESystemConfig getDefaultSelectedImage];
        [handler setImage:image];
        [handler preHandleImage];
        [self performSelectorOnMainThread:@selector(loadImageHandle:) withObject:handler waitUntilDone:NO];
        [passedURL release];
    }
    else
    {
        ALAssetsLibraryAssetForURLResultBlock getAsset = ^(ALAsset *asset)
        {
            NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
            ALAssetRepresentation* rep = nil;
            if(asset)
            {
                rep = [asset defaultRepresentation];
            }
            CGImageRef image = NULL;
            if(rep)
            {
                image = [rep fullResolutionImage];
            }
            UIImageOrientation orient = (UIImageOrientation)[rep orientation];
            CGFloat scale = [rep scale];
            UIImage* uiImage = nil;
            if(image)
            {
                uiImage = [UIImage imageWithCGImage:image scale:scale orientation:orient];
            }
            if(s.width != 0 && s.height != 0 && uiImage != nil)
            {
                CGSize srcS = CGSizeMake(CGImageGetWidth(image), CGImageGetHeight(image));
                CGSize dstS;
                if(op == 0)
                {
                    dstS = [SEUtil computeFitSize:srcS toDst:s];
                }
                else {
                    dstS = [SEUtil calculateImageSizeByRatio:srcS dstSize:s];
                }
                CGImageRef retImage = [SEUtil fastScale:image withRect:dstS];
                uiImage = [UIImage imageWithCGImage:retImage scale:scale orientation:orient];
                CGImageRelease(retImage);
            }
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
    }
    [pool release];
}
- (void) loadThumbnailWithObject: (NSMutableArray*)data
{
    NSURL* url = [data objectAtIndex:0];
    NSObject<SELoadedImageHandler>* handler = [data objectAtIndex:1];
    [self loadImageThumbnailFromPhotoLib:url size:[[PhotoFrameAppDelegate getViewNavigator] getThumbnailSize] withHandler:handler];
    [data release];
}
- (void) getCoreDataThumbnailWithThread: (NSMutableArray*) data
{
    NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
    NSURL* passedURL = [[data objectAtIndex:0] retain];
    NSObject<SELoadedImageHandler>* handler = [data objectAtIndex:1];
    NSString* date = [[data objectAtIndex:2] retain];
    NSManagedObjectContext* moc = [[NSManagedObjectContext alloc] init];
    [moc setPersistentStoreCoordinator:mViewNav.persistentStoreCoordinator];
    UIImage* image = [mViewNav getThumbnailFromCoreData:[passedURL absoluteString] urlDate:date managedObjectContext:moc];
    [moc release];
    if(image != nil)
    {
        [handler setImage:image];
        [handler preHandleImage];
        [self performSelectorOnMainThread:@selector(loadImageHandle:) withObject:handler waitUntilDone:NO];
    }
    else
    {
        NSMutableArray* data = [[NSMutableArray array] retain];
        [data addObject:passedURL];
        [data addObject:handler];
        [self performSelectorOnMainThread:@selector(loadThumbnailWithObject:) withObject:data waitUntilDone:NO];
        
    }
    [passedURL release];
    [date release];
    [pool drain];
}
- (void) getCoreDataFullRepresentationWithThread:(NSMutableArray*) data
{
    NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
    NSURL* passedURL = [[data objectAtIndex:0] retain];
    NSObject<SELoadedImageHandler>* handler = [data objectAtIndex:1];
    NSString* date = [[data objectAtIndex:2] retain];
    NSValue* v = [data objectAtIndex:3];
    int op = [[data objectAtIndex:4] intValue];
    CGSize s = [v CGSizeValue];
    [data release];
    NSManagedObjectContext* moc = [[NSManagedObjectContext alloc] init];
    [moc setPersistentStoreCoordinator:mViewNav.persistentStoreCoordinator];
    UIImage* image = [mViewNav getImageFromCoreData: [passedURL absoluteString] urlDate: date managedObjectContext:moc];
    [moc release];
    if(image != nil)
    {
        if(s.width != 0 && s.height != 0)
        {
            CGImageRef imageRef = [image CGImage];
            //CGSize srcSize = CGSizeMake(image.size.width, image.size.height);
            CGSize srcSize = CGSizeMake(CGImageGetWidth(imageRef), CGImageGetHeight(imageRef));
            CGSize dstSize = s;
            if(op == 0)
            {
                dstSize = [SEUtil computeFitSize:srcSize toDst:dstSize];
            }
            else {
                dstSize = [SEUtil calculateImageSizeByRatio:srcSize dstSize:dstSize];
            }
            CGImageRef retImage = [SEUtil fastScale:imageRef withRect:dstSize];
            NSLog(@"raw image width = %ld, height = %ld", CGImageGetWidth(retImage), CGImageGetHeight(retImage));
            UIImage* retUIImage = [UIImage imageWithCGImage:retImage scale:image.scale orientation:image.imageOrientation];
            CGImageRelease(retImage);
            image = retUIImage;
        }
        [handler setImage:image];
        [handler preHandleImage];
        [self performSelectorOnMainThread:@selector(loadImageHandle:) withObject:handler waitUntilDone:NO];
    }
    else 
    {
        NSMutableArray* data = [[NSMutableArray array] retain];
        [data addObject:passedURL];
        [data addObject:handler];
        NSValue* newV = [NSValue valueWithCGSize:s];
        [data addObject:newV];
        [data addObject:[NSNumber numberWithInt:op]];
        [self performSelectorInBackground:@selector(getFullRepresentationWithThread:) withObject:data];
        //[self getFullRepresentationWithThread:data];
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
    NSValue* v = [NSValue valueWithCGSize:CGSizeMake(0, 0)];
    [data addObject:v];
    [data addObject:[NSNumber numberWithInt:0]];
    data = [data retain];
    [self performSelectorInBackground:@selector(getFullRepresentationWithThread:) withObject:data];
    //[self getFullRepresentationWithThread:data];
}
- (void) loadCoreDataFullRepresentation: (NSURL*)url date: (NSString*)date withHandler: (NSObject<SELoadedImageHandler>*)handler
{
    NSMutableArray* data = [NSMutableArray array];
    [data addObject:url];
    [data addObject:handler];
    [data addObject:date];
    NSValue* v = [NSValue valueWithCGSize:CGSizeMake(0, 0)];
    [data addObject:v];
    [data addObject:[NSNumber numberWithInt:0]];
    data = [data retain];
    [self performSelectorInBackground:@selector(getCoreDataFullRepresentationWithThread:) withObject:data];
}
- (void) loadCoreDataImage: (NSURL*)url date: (NSString*) date  size: (CGSize) size withHandler: (NSObject<SELoadedImageHandler>*)handler
{
    NSMutableArray* data = [[NSMutableArray array] retain];
    [data addObject:url];
    [data addObject:handler];
    [data addObject:date];
    NSValue* v = [NSValue valueWithCGSize:size];
    [data addObject:v];
    [data addObject:[NSNumber numberWithInt:0]];// use dest size to scale dest size
    [self performSelectorInBackground:@selector(getCoreDataFullRepresentationWithThread:) withObject:data];
}
- (void) loadImageThumbnailFromCoreData: (NSURL*) url date: (NSString*) date withHandler: (NSObject<SELoadedImageHandler>*)handler
{
    NSMutableArray* data = [[NSMutableArray array] retain];
    [data addObject:url];
    [data addObject:handler];
    [data addObject:date];
    [self performSelectorInBackground:@selector(getCoreDataThumbnailWithThread:) withObject:data];
}
- (void) loadCoreDataImageWithRatio: (NSURL*)url date: (NSString*) date size: (CGSize) size withHandler: (NSObject<SELoadedImageHandler>*)handler
{
    NSMutableArray* data = [[NSMutableArray array] retain];
    [data addObject:url];
    [data addObject:handler];
    [data addObject:date];
    NSValue* v = [NSValue valueWithCGSize:size];
    [data addObject:v];
    [data addObject:[NSNumber numberWithInt:0]];// use ratio to scale dest size
    [self performSelectorInBackground:@selector(getCoreDataFullRepresentationWithThread:) withObject:data];
}
@end

@implementation SEImageAsyncLoadHandler
- (id) init
{
    self = [super init];
    if(self)
    {
        imageAsyncLoader = [[SEImageAsyncLoader alloc] init];
        imageAsyncLoader.mViewNav = [PhotoFrameAppDelegate getViewNavigator];
    }
    return self;
}
- (void) dealloc
{
    [imageAsyncLoader release];
    [super dealloc];
}
- (void) setAssetLibOwn: (ALAssetsLibrary*) lib
{
    [imageAsyncLoader setAssetLibOwn:lib];
}
- (void) setAssetLibNotOwn: (ALAssetsLibrary*)lib
{
    [imageAsyncLoader setAssetLibNotOwn:lib];
}
- (void) loadImageFromPhotoLib: (NSURL*)url size:(CGSize)size
{
    [imageAsyncLoader loadImageFromPhotoLib:url size:size withHandler:self];
}
- (void) loadImageFromPhotoLibWithRatio: (NSURL*)url size: (CGSize)destSize
{
    [imageAsyncLoader loadImageFromPhotoLibWithRatio:url size:destSize withHandler:self];
}
- (void) loadImageThumbnailFromCoreData: (NSURL*)url date: (NSString*) date
{
    [imageAsyncLoader loadImageThumbnailFromCoreData:url date:date withHandler:self];
}
- (void) loadImageThumbnailFromPhotoLib: (NSURL*)url size: (CGSize) size
{
    [imageAsyncLoader loadImageThumbnailFromPhotoLib:url size:size withHandler:self];
}
- (void) loadFullRepresentation: (NSURL*)url
{
    [imageAsyncLoader loadFullRepresentation:url withHandler:self];
}
- (void) loadCoreDataFullRepresentation: (NSURL*)url date: (NSString*) date
{
    [imageAsyncLoader loadCoreDataFullRepresentation:url date:date withHandler:self];
}
- (void) loadCoreDataImage: (NSURL*)url date: (NSString*) date size: (CGSize) size
{
    [imageAsyncLoader loadCoreDataImage:url date:date size:size withHandler:self];
}
- (void) loadCoreDataImageWithRatio: (NSURL*)url date: (NSString*) date size: (CGSize) size
{
    [imageAsyncLoader loadCoreDataImageWithRatio:url date:date size:size withHandler:self];
}
- (void) preHandleImage
{}
- (void) setImage:(UIImage *)image
{}
- (void) handleImage
{}
@end
