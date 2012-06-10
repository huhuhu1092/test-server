//
//  SEUtil.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-1-21.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <AssetsLibrary/AssetsLibrary.h>
enum {COLOR_MULTIPLY, COLOR_ALPHA_SRC, COLOR_ALPHA_ONE_MINUS_SRC};

struct UIImageTransformProperty
{
    UInt8 r;
    UInt8 g;
    UInt8 b;
    float alpha;
    int op;
};
//return the new CGContextRef, user need to release it
extern CGContextRef MyCreateBitmapContext (int pixelsWide,
                                           int pixelsHigh);
@interface SEUtil : NSObject
// the last object must be nil
+ (NSArray*) getFilePathWithType: (id)firstType, ...;
+ (CGImageRef) fastScale: (CGImageRef)image withRect: (CGSize) s;
//create a new image with draw image in s
+ (UIImage*) cropUIImage: (UIImage*) image withRect:(CGSize)s;
+ (CGImageRef) copyImageRef: (CGImageRef) srcImage;
+ (CGImageRef) CGImageDrawInRect: (CGImageRef) image rect: (CGSize) size;
//current just handle bitmap with bpp == 32
+ (UIImage*) transformImage: (UIImage*)srcImage transProperty: (struct UIImageTransformProperty)p;
+ (CGImageRef) getImageFromPhotoLib: (NSURL*) url withAssetLib: (ALAssetsLibrary*)assetLib fitSize:(CGSize)size;
+ (CGImageRef) getFullRepresentationImageFromPhotoLib: (NSURL*) url withAssetLib: (ALAssetsLibrary*)assetLib;
+ (const UInt8*) getCGImageData: (CGImageRef) image;
+ (void)removeAllSubviews: (UIView*)view;
+ (CGSize) computeFitSize: (CGSize)src toDst: (CGSize) dst;
+ (UIImage*) createUIImageFrom: (UIImage*)background  withLineWidth: (CGFloat) lineWidth drawPoints:(NSMutableArray*)points;
+ (UIImage*) createSignatureInMainThread: (UIImage*)background size: (CGSize)size withLineWidth: (CGFloat) lineWidth drawPoints:(NSMutableArray*)points;

+ (BOOL) isRectContain: (CGRect) rect point: (CGPoint)p;
+ (UIImage*) drawImage : (UIImage*)image inRect:(CGRect)rect;
+ (NSString*) urlToString: (NSURL*)url;
+ (UIImage*) createThumbnail: (UIImage*)srcImage thumbnailSize: (CGSize) size;
+ (BOOL) reachabilityWithLocalWifi;
+ (BOOL) reachabilityForHostName: (NSString*) name;
+ (NSString*) dateToString: (NSDate*)date;
@end
