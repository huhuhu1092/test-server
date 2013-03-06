//
//  SEUtil.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-1-21.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <AssetsLibrary/AssetsLibrary.h>
#import "ppmtool.h"
enum {COLOR_MULTIPLY, COLOR_ALPHA_SRC, COLOR_ALPHA_ONE_MINUS_SRC};

struct UIImageTransformProperty
{
    UInt8 r;
    UInt8 g;
    UInt8 b;
    float alpha;
    int op;
};
@interface SEPhotoLibData : NSObject
{
    NSURL* url;
    NSString* urlDate;
    float width;
    float height;
    int orient;
}
@property (nonatomic, retain) NSURL* url;
@property (nonatomic, retain) NSString* urlDate;
@property (nonatomic, assign) float width;
@property (nonatomic, assign) float height;
@property (nonatomic, assign) int orient;
@end
////
@interface SEMusicLibData : NSObject
{
    NSString* title;
    NSString* artist;
    NSString* album;
}
@property (nonatomic, retain) NSString* title;
@property (nonatomic, retain) NSString* artist;
@property (nonatomic, retain) NSString* album;
@end
//return the new CGContextRef, user need to release it
extern CGContextRef MyCreateBitmapContext (int pixelsWide,
                                           int pixelsHigh);
@interface SEUtil : NSObject
// the last object must be nil
+ (NSArray*) getFilePathWithType: (id)firstType, ...;
+ (CGImageRef) createEmptyCGImage: (CGSize) size;
+ (CGImageRef) fastScale: (CGImageRef)image withRect: (CGSize) s;
//create a new image with draw image in s
+ (UIImage*) cropUIImage: (UIImage*) image withRect:(CGSize)s;
+ (CGImageRef) copyImageRef: (CGImageRef) srcImage;
+ (CGImageRef) copyImageRef:(CGImageRef)srcImage rect: (CGRect) frame;
+ (CGImageRef) CGImageDrawInRect: (CGImageRef) image rect: (CGSize) size;
//current just handle bitmap with bpp == 32
+ (UIImage*) transformImage: (UIImage*)srcImage transProperty: (struct UIImageTransformProperty)p;
+ (CGImageRef) getImageFromPhotoLib: (NSURL*) url withAssetLib: (ALAssetsLibrary*)assetLib fitSize:(CGSize)size;
+ (CGImageRef) getFullRepresentationImageFromPhotoLib: (NSURL*) url withAssetLib: (ALAssetsLibrary*)assetLib;
+ (const UInt8*) getCGImageData: (CGImageRef) image;
+ (void)removeAllSubviews: (UIView*)view;
+ (CGSize) computeFitSize: (CGSize)src toDst: (CGSize) dst;
+ (CGSize) computeFitSize2: (CGSize) src toDst: (CGSize) dst;
+ (UIImage*) createUIImageFrom: (UIImage*)background  withLineWidth: (CGFloat) lineWidth drawPoints:(NSMutableArray*)points;
+ (UIImage*) createSignatureInMainThread: (UIImage*)background size: (CGSize)size withLineWidth: (CGFloat) lineWidth drawPoints:(NSMutableArray*)points;

+ (BOOL) isRectContain: (CGRect) rect point: (CGPoint)p;
+ (UIImage*) drawImage : (UIImage*)image inRect:(CGRect)rect;
+ (NSString*) urlToString: (NSURL*)url;
+ (UIImage*) createThumbnail: (UIImage*)srcImage thumbnailSize: (CGSize) size;
+ (BOOL) reachabilityWithLocalWifi;
+ (BOOL) reachabilityForHostName: (NSString*) name;
+ (NSString*) dateToString: (NSDate*)date;
+ (NSString*) readDataFromDocumentDir:(NSString*) fileName;
+ (NSString*)readDataFromBundle:(NSString*)fileName;
+ (BOOL)isWhitespaceLine: (NSString*)line;
+ (NSString*) stringTrim: (NSString*) str;
+ (CGImageRef)createCGImage: (ppm_t) p;
+ (CGImageRef) createCGImageWithCopy:(ppm_t)p;
// user need to release the CGImageRef by this function
+ (CGImageRef) create32BitsCGimageWithCopy: (ppm_t)p;
+ (UIImage*) imageWithCap:(UIImage*)image top: (float) top bottom: (float) bottom left: (float) left right: (float)right;
+ (UIImage*) imageStretchForSlider: (UIImage*)image;
+ (UIImage*) imageWithInsets: (UIImage*)image top: (float) top bottom: (float) bottom left: (float) left right: (float)right;

+ (int) getSystemVersion;
+ (void) savePNGImageToDocument: (UIImage*)image withName:(NSString*)fileName;
+ (NSString*) getFontName;
+ (UIImage*) drawImage: (UIImage*)image toSize: (CGSize) size;
+ (UIImage*) imageWithCapInset: (UIImage*)image top:(float)top bottom: (float)bottom left: (float)left right: (float)right;
//This function will use image's width and height and its orientation to caluclate the 
//actual width and height
+ (void) calculateImageActualSizeWithWidth: (float)srcWidth height: (float)srcHeight orientation: (UIImageOrientation) orient : (float*)outWidth : (float*) outHeight;
+ (NSArray*) removeWhiteLineStringFrom: (NSArray*)tokens;
//user create ALAssetsLibrary* lib and photoArray and pass them to this function
//after this function end it will invoke target's action with parameter : lib and photoArray
// user must release lib and photoArray by themselves
+ (void) getImagesFromPhotoLib: (ALAssetsLibrary*)lib assetType: (NSArray*)assetTypes photoContainer:(NSMutableArray*)photoArray finishedTarget:(id)target finishedAction: (SEL)action;
+ (NSArray*) getMusicFromMusicLib;
+ (UIImage*) createImageWithColor: (float)r g: (float) g b: (float) b alphaImage:(UIImage*)image;
+ (UIImage*) createImageWithColorImage:(UIImage*)colorImage alphaImage: (UIImage*)alphaImage;
+ (CGSize) calculateImageSizeByRatio: (CGSize) size dstSize: (CGSize) dstSize;
+ (NSArray*) findMediaItemByTitle: (NSString*)title aritst: (NSString*)artist album: (NSString*)album;
+ (void) pauseCurrentMusic;
+ (NSString*) createNextName: (NSString*) prefix: (NSArray*) currentNameArray: (int) count;
+ (UIImage*) drawTextOnImage: (UIImage*)currImage text: (NSString*)str color: (UIColor*)color textSize: (float)textSize x: (float)x y: (float)y;
+ (ppm_t) blurBrush: (ppm_t) srcBrushc radius: (int) radius sig: (float)sig;
+ (ppm_t) blurBrush: (ppm_t) srcBrush horiz: (int) horz vert: (int)vert;
+ (NSArray*) getFeedChar;
+ (BOOL) isWifiConnectionOK;
@end
