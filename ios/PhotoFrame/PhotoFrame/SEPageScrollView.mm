//
//  SEPageScrollView.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-3-14.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEPageScrollView.h"
#import <AssetsLibrary/AssetsLibrary.h>
#import "SEViewNavigator.h"
#import "SEUtil.h"
#import "SelectedImage.h"
#import "UserInfo.h"
#import "SEResDefine.h"
#import "SEMultiTouchDetect.h"
#import "PhotoFrameAppDelegate.h"
////////////////
enum {SE_NO_PRESS, SE_PRESS_READY, SE_LONGPRESS_OK};
/////
/*
#define SEEXIT_EVENT 0
#define SEVISIBLERANGE_EVENT 1
#define SELOADPHOTO_EVENT 2
#define SERESTART_EVENT 3
#define SEUPDATEIMAGE_EVENT 4
#define SEUPDATEALL_EVENT 5
#define SEENDSCROLL_EVENT 6
#define SEREMOVEIMAGEFROMPHOTODICT_EVENT 7
#define SEINSERTURL_EVENT 8
#define SEREMOVEURL_EVENT 9
#define SEGETIMAGEURL_EVENT 10
#define SECHANGEIMAGEVIEWPOS_EVENT 11
*/
#define  SE_INVALID_PAGE -1
#define  SE_INVALID_IMAGE_INDEX -1
#define  NOTIFICATION_LOADIMAGE_FROM_PHOTOLIB @"LoadImageFromPhotoLib"
#define NOTIFICATION_LOADIMAGE_FROM_COREDATA @"LoadImageFromCoreData"
////////////
static CGImageRef createCGImageCopy(CGImageRef srcImage)
{
    CGColorSpaceRef srcColorRef = CGImageGetColorSpace(srcImage);
    return CGImageCreateCopyWithColorSpace(srcImage, srcColorRef);
}
/////////
@interface PageImageLoadInfo : NSObject {
@private
    int page;
    int row;
    int col;
    int startPage;
    int endPage;
    int index;
}
@property (nonatomic, assign) int startPage;
@property (nonatomic, assign) int endPage;
@property (nonatomic, assign) int page;
@property (nonatomic, assign) int row;
@property (nonatomic, assign) int col;
@property (nonatomic, assign) int index;
- (PageImageLoadInfo*) clone;
@end
@implementation PageImageLoadInfo
@synthesize page;
@synthesize row;
@synthesize col;
@synthesize startPage;
@synthesize endPage;
@synthesize index;
- (PageImageLoadInfo*) clone
{
    PageImageLoadInfo* info = [[[PageImageLoadInfo alloc] init] autorelease];
    info.page = page;
    info.row = row;
    info.col = col;
    info.startPage = startPage;
    info.endPage = endPage;
    info.index = index;
    return info;
}
@end
//////////
@interface SELoadImageFromCoreDataOperation : NSOperation
{
    PageImageLoadInfo* mPageLoadInfo;
    CGSize mFitSize;
    UIImage* mResultImage;
    SEPageImageURL* mURL;
    BOOL mNeedLoadFromPhotoLib;
    SEPageUIScrollView* mPageScrollView;
}
@property (nonatomic, retain) PageImageLoadInfo* mPageLoadInfo;
@property (nonatomic, assign) CGSize mFitSize;
@property (nonatomic, retain) UIImage* mResultImage;
@property (nonatomic, retain) SEPageImageURL* mURL;
@property (nonatomic, assign) BOOL mNeedLoadFromPhotoLib;
@property (nonatomic, retain) SEPageUIScrollView* mPageScrollView;
@end
@implementation SELoadImageFromCoreDataOperation
@synthesize mPageLoadInfo;
@synthesize mResultImage;
@synthesize mURL;
@synthesize mFitSize;
@synthesize mNeedLoadFromPhotoLib;
@synthesize mPageScrollView;
- (id) initWithURL: (SEPageImageURL*)url pageInfo: (PageImageLoadInfo*)pageInfo
{
    self = [super init];
    if(self)
    {
        self.mURL = url;
        self.mPageLoadInfo = pageInfo;
    }
    return self;
}
- (void) dealloc
{
    //NSLog(@"SELoadImageFromCoreDataOperation dealloc");
    [mURL release];
    [mPageLoadInfo release];
    [mResultImage release];
    [mPageScrollView release];
    [super dealloc];
}

- (void) main
{
    if([self isCancelled] == YES)
        return;
    //NSLog(@"SELoadImageFromCoreDataOperation main");
    NSAutoreleasePool* newPool = [[NSAutoreleasePool alloc] init];
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    NSManagedObjectContext* moc = [[NSManagedObjectContext alloc] init];
    [moc setPersistentStoreCoordinator:viewNav.persistentStoreCoordinator];
    UIImage* image = [viewNav getImageFromCoreData:[SEUtil urlToString:mURL.url] urlDate:mURL.urlDate managedObjectContext:moc];
    if(image)
    {
        CGImageRef imageRef = [image CGImage];
        //CGSize srcSize = CGSizeMake(image.size.width, image.size.height);
        CGSize srcSize = CGSizeMake(CGImageGetWidth(imageRef), CGImageGetHeight(imageRef));
        CGSize dstSize = mFitSize;
        dstSize = [SEUtil computeFitSize:srcSize toDst:dstSize];
        CGImageRef retImage = [SEUtil fastScale:imageRef withRect:dstSize];
        NSLog(@"raw image width = %ld, height = %ld", CGImageGetWidth(retImage), CGImageGetHeight(retImage));
        UIImage* retUIImage = [UIImage imageWithCGImage:retImage scale:1.0 orientation: (UIImageOrientation)mURL.orientation];
        NSLog(@"image width = %f, height = %f, orientation = %d", retUIImage.size.width, retUIImage.size.height, retUIImage.imageOrientation);
        CGImageRelease(retImage);
        self.mResultImage = retUIImage;
        mNeedLoadFromPhotoLib = NO;
    }
    else
    {
        self.mResultImage = nil;
        mNeedLoadFromPhotoLib = YES;
    }
    [moc release];
    if([self isCancelled] == NO)
    {
        [[NSNotificationCenter defaultCenter] postNotificationName:NOTIFICATION_LOADIMAGE_FROM_COREDATA object:self];
    }
    [newPool drain];
}

@end
/////////
@interface SELoadImageOperation : NSOperation   
{
    SEPageImageURL* mURL;
    PageImageLoadInfo* mPageLoadInfo;
    CGSize mFitSize;
    BOOL mIsFromCoreData;
    ALAsset* mAsset;
    UIImage* mResultImage;
    SEPageUIScrollView* mPageScrollView;
    
    int mOrigWidth;
    int mOrigHeight;
    int mOrientation;
}
@property (nonatomic, assign) int mOrigWidth;
@property (nonatomic, assign) int mOrigHeight;
@property (nonatomic, assign) int mOrientation;
@property (nonatomic, retain) SEPageImageURL* mURL;
@property (nonatomic, assign) int mIndex;
@property (nonatomic, assign) CGSize mFitSize;
@property (nonatomic, assign) BOOL mIsFromCoreData;
@property (nonatomic, retain) ALAsset* mAsset;
@property (nonatomic, retain) UIImage* mResultImage;
@property (nonatomic, retain) PageImageLoadInfo* mPageLoadInfo;
@property (nonatomic, retain) SEPageUIScrollView* mPageScrollView;
@end
@implementation SELoadImageOperation
@synthesize mURL;
@synthesize mIndex;
@synthesize mFitSize;
@synthesize mIsFromCoreData;
@synthesize mAsset;
@synthesize mResultImage;
@synthesize mPageLoadInfo;
@synthesize mPageScrollView;
@synthesize mOrientation;
@synthesize mOrigHeight;
@synthesize mOrigWidth;
- (id) initWithURL: (SEPageImageURL*)url pageInfo: (PageImageLoadInfo*)pageInfo
{
    self = [super init];
    if(self)
    {
        mURL = [url retain];
        mPageLoadInfo = [pageInfo retain];
    }
    return self;
}
- (void) dealloc
{
    //NSLog(@"dealloc of SELoadImageOperation");
    [mURL release];
    [mAsset release];
    [mResultImage release];
    [mPageLoadInfo release];
    [mPageScrollView release];
    [super dealloc];
}
- (void) main
{
    if([self isCancelled])
        return;
    //NSLog(@"SELoadImageOperation main");
    if(mAsset == nil)
    {
        self.mOrigWidth = 0;
        self.mOrigHeight = 0;
        self.mOrientation = 0;
        self.mResultImage = nil;
    }
    else
    {
        ALAssetRepresentation* rep = [mAsset defaultRepresentation];        
        ALAssetOrientation orient = [rep orientation];
        NSDictionary* metaDataDict = [rep metadata];
        NSNumber* metaWidth = [metaDataDict objectForKey:@"PixelWidth"];
        NSNumber* metaHeight = [metaDataDict objectForKey:@"PixelHeight"];
        NSNumber* metaOrient = [metaDataDict objectForKey:@"Orientation"];
        
        CGFloat scale = [rep scale];
        CGImageRef image = [rep fullResolutionImage];
        //CGImageRef image = [rep fullScreenImage];
        
        //CGImageRef thumbnail = [asset thumbnail];
        CGSize srcS = CGSizeMake(CGImageGetWidth(image), CGImageGetHeight(image));
        //NSLog(@"meta orient = %d, orient = %d", [metaOrient intValue], orient);
        //NSLog(@"meta width = %d, width = %d", [metaWidth intValue], (int)srcS.width);
        //NSLog(@"meta height = %d, height = %d", [metaHeight intValue], (int) srcS.height);
        CGSize s = [SEUtil computeFitSize:srcS toDst:mFitSize];
        CGImageRef retImage = [SEUtil CGImageDrawInRect:image rect:s];

        UIImageOrientation o = (UIImageOrientation)orient;
        UIImage* uiImage = [UIImage imageWithCGImage:retImage scale:scale orientation:o];    
        self.mOrigWidth = srcS.width;
        self.mOrigHeight = srcS.height;
        self.mOrientation = o;
        CGImageRelease(retImage);
        self.mResultImage = uiImage;
    }
    if([self isCancelled] == NO)
    {
        [[NSNotificationCenter defaultCenter] postNotificationName:NOTIFICATION_LOADIMAGE_FROM_PHOTOLIB object:self];
    }
}

@end
////////////


@class SEPageUIScrollViewEvent;
@interface SEPageUIScrollView (Private)
- (int) lastImageViewIndex;
- (void) initAssetLib;
- (void) getImageFromURL:(SEPageImageURL*)url continueLoad:(BOOL) continueLoad pageInfo:(PageImageLoadInfo*)pageInfo; 
- (SEPageUIImageView*) getImageViewByIndex: (int)index;
- (void) loadImageHandle: (id)data;//after load image will invoke handler for this image
- (void) garbageClean;
- (size_t) getImageSizeSumInDict;
- (int) getImageSize: (UIImage*)image;
- (UIImage*) getImageFromPhotoDict: (SEPagePhotoDictKey*)key;
- (void) addImageToPhotoDict: (SEPagePhotoDictKey*) key image: (UIImage*)image;
- (void) removeImageFromPhotoDict: (SEPagePhotoDictKey*)key;

- (void) clearState;
- (void) initPhotoDict;
- (void) calculateVisiblePage: (CGFloat)contentOffset;
- (BOOL) isRunOnMainThread;
- (BOOL) isRunOnWorkderThread;
- (void) createSingleColumnContent: (SEPAGE_PART_TYPE) reservePart;
- (void) createMultiColumnContent : (SEPAGE_PART_TYPE)movePart;
- (void) doScaleAnimationForInsert;
- (void) insertURLToPhotoAssetArray: (NSMutableArray*)urlArray : (int) index : (int) lastURLIndex;
- (int) lastURLIndex;
- (BOOL) isIntervalContain: (int) srcStart : (int)srcEnd : (int) dstStart : (int) dstEnd;
/////// new add
- (void) assertPhotoAssert;
- (int) getCurrentImageViewCount;
- (void) printPhotoAssetURL;
//////// function new
//this function will compare url's content with array's url content
- (BOOL) imageURLContentInArray: (NSArray*) array : (SEPageImageURL*)url;
- (void) calculatePageRowCol: (int) index: (int*)outPage : (int*)outRow : (int*)outCol;
- (UIImage*) getImageFromPhotoDictByIndex: (int)index;
- (BOOL) isValidPhotoIndex: (int)index;
- (void) getImageFromPhotoLib: (SEPageImageURL*)url pageInfo: (PageImageLoadInfo*)pageLoadInfo;
- (void) getImageFromCoreDataNew: (SEPageImageURL*)url pageInfo: (PageImageLoadInfo*)pageLoadInfo;
- (void) finishLoadImageWrapper: (NSNotification*)notify;
- (void) finishLoadImageFromCoreData: (SELoadImageFromCoreDataOperation*) loadImageOp;
- (void) finishLoadImage: (SELoadImageOperation*) loadImageOp;
- (void) getImageFromURLNew:(SEPageImageURL*)url pageInfo: (PageImageLoadInfo*)pageLoadInfo;
- (void) loadNextImageNew;
- (void) loadImageFromPhotoLib: (SEPageImageURL*)url size:(CGSize)size withHandler: (NSObject<SELoadedImageHandler>*) handler;
- (void) loadFullRepresentation: (SEPageImageURL*)url withHandler: (NSObject<SELoadedImageHandler>*)handler;
- (int) getCurrentVisibleImageViewCount;
- (int) getVisibleImageViewCountNoneDefaultImage;
@end
///////
@implementation SEPageURLID
@synthesize origHeight;
@synthesize origWidth;
@synthesize url;
@synthesize urlDate;
@synthesize orientation;
- (void)dealloc
{
    [url release];
    [urlDate release];
    [super dealloc];
}
+ (SEPageURLID*)create: (NSURL*)tu : (NSString*)date : (float) width : (float) height : (int)orientation
{
    SEPageURLID* url = [[SEPageURLID alloc] init];
    url.url = tu;
    url.urlDate = date;
    url.origWidth = width;
    url.origHeight = height;
    url.orientation = orientation;
    [url autorelease];
    return url;
}
@end
////////////////////////////////////
@implementation SEPageImageURL
@synthesize orientation;
@synthesize url;
@synthesize urlDate;
@synthesize type;
@synthesize filepath;
@synthesize origWidth;
@synthesize origHeight;
- (SEPageImageURL*) clone
{
    SEPageImageURL* newUrl = [[SEPageImageURL alloc] init];
    newUrl.orientation = orientation;
    newUrl.url = [url copy];
    newUrl.urlDate = [urlDate copy];
    newUrl.type = type;
    newUrl.filepath = [filepath copy];
    newUrl.origWidth = origWidth;
    newUrl.origHeight = origHeight;
    return [newUrl autorelease];
}
- (void) dealloc
{
    [url release];
    [urlDate release];
    [filepath release];
    [super dealloc];
}
- (id) init
{
    self = [super init];
    if(self)
    {
        type = SEPAGE_PHOTO_LIB_URL;
    }
    return self;
}

@end
////////
@implementation SEPagePhotoDictKey
@synthesize url;
@synthesize date;
@synthesize type;
- (void) dealloc
{
    [url release];
    [date release];
    [super dealloc];
}
- (BOOL)isEqualToKey:(SEPagePhotoDictKey *)key
{
    assert(key.type != SEPAGE_FILE_PATH && type != SEPAGE_FILE_PATH);
    return [url isEqualToString:key.url] && type == key.type;
    //return [url isEqualToString:key.url] && [date isEqualToString: key.date] && type == key.type;
}
+ (SEPagePhotoDictKey*) createFromImageURL: (SEPageImageURL*)url
{
    SEPagePhotoDictKey* key = [[SEPagePhotoDictKey alloc] init];
    key.url = [url.url absoluteString];
    key.date = url.urlDate;
    key.type = url.type;
    [key autorelease];
    return key;
}
@end
/////////////////////
@implementation SEPagePhotoDictItem
@synthesize key;
@synthesize image;
- (void) dealloc
{
    [key release];
    [image release];
    [super dealloc];
}
@end
///////////
@interface SELoadFullRepresentation : NSObject<SELoadedImageHandler >
{
    UIImage* currentImage;
    SEPageImageURL* url;
    SEViewNavigator* viewNav;
    int index;
}
@property (nonatomic, assign) int index;
@property (nonatomic, retain) SEPageImageURL* url;
@property (nonatomic, assign) SEViewNavigator* viewNav;
@end
@implementation SELoadFullRepresentation
@synthesize url;
@synthesize viewNav;
@synthesize index;
-(void) setImage:(UIImage *)image
{
    [currentImage release];
    currentImage = [image retain];
}
- (void) preHandleImage
{}
- (void)dealloc
{
    [currentImage release];
    [url release];
    [super dealloc];
}
- (void)handleImage
{
    [viewNav saveImageThumbnailToCoreData:currentImage urlName:[url.url absoluteString] urlDate:url.urlDate index: index];
}

@end
//////
@implementation SEPageUIImageView
@synthesize alpha;
@synthesize highlightedFrameImage = mHighlightedFrameImage;
@synthesize frameImage = mFrameImage;
@synthesize mScrollView;
@synthesize mIsAnimation;
@synthesize mOriginRect;
- (void) dealloc
{
    [mContentImage release];
    [mFrameImage release];
    [mHighlightedFrameImage release];
    [super dealloc];
}
- (BOOL) isDefaultImage
{
    return mContentImage == mScrollView.mDefaultImage;
}
- (BOOL) highlighted
{
    return highlighted;
}
- (SEPageUIImageView*) copy
{
    SEPageUIImageView* ret = [[SEPageUIImageView alloc] init];
    ret.frame = self.frame;
    ret.image = self.image;
    ret.frameImage = self.frameImage;
    ret.highlightedFrameImage = self.highlightedFrameImage;
    ret.contentSize = self.contentSize; // this is content size which will contain mContentImage
    ret.alpha = self.alpha;
    ret.highlighted = self.highlighted;
    ret.mScrollView = self.mScrollView;
    return ret;
}
- (void) setHighlighted:(BOOL)h
{
    if(highlighted != h && mContentImage != mScrollView.mDefaultImage)
    {
        if(mContentImage != mScrollView.mNotFoundImage || [mScrollView.mName isEqualToString:@"image_selected_view"])
        {
            highlighted = h;
            [self setNeedsDisplay];
        }
    }
}
- (UIImage*) image
{
    return mContentImage;
}
- (void) setImage:(UIImage *)image
{
    [mContentImage release];
    mContentImage = [image retain];
    [self setNeedsDisplay];
}
/*
- (UIImage*) frameImage
{
    return mFrameImage;
}
- (void) setFrameImage:(UIImage *)frameImage
{
    [mFrameImage release];
    mFrameImage = [frameImage retain];
    [self setNeedsDisplay];
}
 */
- (id) initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if(self)
    {
        mFrameImage = nil;
        mContentImage = nil;
        alpha = 1.0;
    }
    return self;
}
- (CGSize) contentSize
{
    return mContentSize;//CGSizeMake(self.frame.size.width - 20, self.frame.size.height - 20);
}
- (void) setContentSize:(CGSize)contentSize
{
    mContentSize = contentSize;
}
- (void) drawRect:(CGRect)rect
{
    if(mContentImage.size.width == rect.size.width && mContentImage.size.height == rect.size.height)
    {
        if(alpha == 1.0)
        {
            [mContentImage drawInRect:rect];
        }
        else
        {
            [mContentImage drawInRect:rect blendMode:kCGBlendModeSourceAtop alpha:alpha];
        }
        return;
    }
    CGFloat startx = (rect.size.width - mContentImage.size.width) / 2;
    CGFloat starty = (rect.size.height - mContentImage.size.height) / 2;
    CGBlendMode blendMode = kCGBlendModeSourceAtop;
    [mContentImage drawAtPoint:CGPointMake(startx, starty)];
    UIImage* frameImage = nil;
    if(mFrameImage != nil && self.highlighted == NO)
    {
        frameImage = mFrameImage;
    }
    else if(mHighlightedFrameImage != nil && self.highlighted == YES)
    {
        frameImage = mHighlightedFrameImage;
    }
    if(frameImage == nil)
        return;
    //CGFloat topPadding = 3, leftPadding = 3;
    UIGraphicsBeginImageContext(CGSizeMake(9, 9));
    [frameImage drawAtPoint:CGPointMake(0, 0)];
    UIImage* leftTopCornerImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    if(alpha == 1.0)
    {
        [leftTopCornerImage drawAtPoint:CGPointMake(startx - 9, starty - 9)];
    }
    else
    {
        [leftTopCornerImage drawAtPoint:CGPointMake(startx - 9, starty - 9) blendMode:blendMode alpha:alpha];
    }
    
    UIGraphicsBeginImageContext(CGSizeMake(mContentImage.size.width, 9));
    [frameImage drawAtPoint:CGPointMake(-9, 0)];
    UIImage* topImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    if(alpha == 1.0)
    {
        [topImage drawAtPoint:CGPointMake(startx, starty - 9)];
    }
    else
    {
        [topImage drawAtPoint:CGPointMake(startx, starty - 9) blendMode:blendMode alpha:alpha];
    }
    
    UIGraphicsBeginImageContext(CGSizeMake(11, 11));
    [frameImage drawAtPoint:CGPointMake(-frameImage.size.width + 11, 0)];
    UIImage* rightTopImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    if(alpha == 1.0)
    {
        [rightTopImage drawAtPoint:CGPointMake(startx + mContentImage.size.width, starty - 9)];
    }
    else
    {
        [rightTopImage drawAtPoint:CGPointMake(startx + mContentImage.size.width, starty - 9) blendMode:blendMode alpha:alpha];
    }
    
    UIGraphicsBeginImageContext(CGSizeMake(9, mContentImage.size.height));
    [frameImage drawAtPoint:CGPointMake(0, -9)];
    UIImage* leftImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    if(alpha == 1.0)
    {
        [leftImage drawAtPoint:CGPointMake(startx - 9, starty)];
    }
    else
    {
        [leftImage drawAtPoint:CGPointMake(startx - 9, starty) blendMode:blendMode alpha:alpha];
    }
    
    UIGraphicsBeginImageContext(CGSizeMake(11, mContentImage.size.height));
    [frameImage drawAtPoint:CGPointMake(-frameImage.size.width + 11, -9)];
    UIImage* rightImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    if(alpha == 1.0)
    {
        [rightImage drawAtPoint:CGPointMake(startx + mContentImage.size.width, starty)];
    }
    else
    {
        [rightImage drawAtPoint:CGPointMake(startx + mContentImage.size.width, starty) blendMode:blendMode alpha:alpha];
    }
    
    UIGraphicsBeginImageContext(CGSizeMake(9, 11));
    [frameImage drawAtPoint:CGPointMake(0, -frameImage.size.height + 11)];
    UIImage* leftBottomImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    if(alpha == 1.0)
    {
        [leftBottomImage drawAtPoint:CGPointMake(startx - 9, starty + mContentImage.size.height)];
    }
    else
    {
        [leftBottomImage drawAtPoint:CGPointMake(startx - 9, starty + mContentImage.size.height) blendMode:blendMode alpha:alpha];
    }
    
    UIGraphicsBeginImageContext(CGSizeMake(mContentImage.size.width, 11));
    [frameImage drawAtPoint:CGPointMake(-9, -frameImage.size.height + 11)];
    UIImage* bottomImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    if(alpha == 1.0)
    {
        [bottomImage drawAtPoint:CGPointMake(startx, starty + mContentImage.size.height)];
    }
    else
    {
        [bottomImage drawAtPoint:CGPointMake(startx, starty + mContentImage.size.height) blendMode:blendMode alpha:alpha];
    }
    
    UIGraphicsBeginImageContext(CGSizeMake(11, 11));
    [frameImage drawAtPoint:CGPointMake(-frameImage.size.width + 11, -mFrameImage.size.height + 11)];
    UIImage* rightBottomImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    if(alpha == 1.0)
    {
        [rightBottomImage drawAtPoint:CGPointMake(startx + mContentImage.size.width, starty + mContentImage.size.height)];
    }
    else
    {
        [rightBottomImage drawAtPoint:CGPointMake(startx + mContentImage.size.width, starty + mContentImage.size.height) blendMode:blendMode alpha:alpha];
    }
}
@end
/////
@interface PageImageData : NSObject
{
    CGImageRef imageRef;
    int index;
}
@property (nonatomic, assign) CGImageRef imageRef;
@property (nonatomic, assign) int index;
@end;
@implementation PageImageData
@synthesize imageRef;
@synthesize index;

@end
/////

@interface PageGarbageInfo : NSObject
{
    int index;
    SEPAGE_REMOVE_OP op;
}
@property (nonatomic, assign) int index;
@property (nonatomic, assign) SEPAGE_REMOVE_OP op;
@end
@implementation PageGarbageInfo
@synthesize index;
@synthesize op;

@end
/////////
/////////////////////
@implementation SEPageUIScrollViewDelegate
-(void) scrollViewDidScroll:(UIScrollView *)scrollView
{
    //NSLog(@"content offset = %f", scrollView.contentOffset.y);
    
    SEPageUIScrollView* currentView = (SEPageUIScrollView*)scrollView;
    //static int i = 1;
    //if(currentView.mIsCalculateVisibleRange && i == 1)
    {
        [currentView calculateVisiblePage:scrollView.contentOffset.y];
        //i--;
    }
     
}
- (void)scrollViewWillBeginDecelerating:(UIScrollView *)scrollView
{
    NSLog(@"begin decelerate");
    SEPageUIScrollView* currentView = (SEPageUIScrollView*)scrollView;
    currentView.mIsScrolling = YES;
}
- (void) scrollViewDidEndDecelerating:(UIScrollView *)scrollView
{
    NSLog(@"end decelerate\n");
    SEPageUIScrollView* currentView = (SEPageUIScrollView*)scrollView;
    [currentView garbageClean];
    currentView.mIsScrolling = NO;
    //[currentView calculateVisiblePage:scrollView.contentOffset.y];
    /*
    SEPageUIScrollView* currentView = (SEPageUIScrollView*)scrollView;
    static int i = 1;
    if(currentView.mIsCalculateVisibleRange && i == 1)
    {
        [currentView calculateVisiblePage:scrollView.contentOffset.y];
        //i--;
    }
    */
}
- (void) scrollViewDidEndScrollingAnimation:(UIScrollView*)scrollView
{
    SEPageUIScrollView* currentView = (SEPageUIScrollView*)scrollView;
    NSLog(@"end scroll animation");
    if(currentView.mCurrentImageArray)
    {
        [currentView doScaleAnimationForInsert];
    }
}
@end
//////////////////////
////
/////////////////////////

////////
@implementation SEPageUIScrollView (Private)
- (int) getCurrentImageViewCount
{
    //return [mPhotoAssetURL count] + [mViewNavigator getRemainingImageCount];
    return mImageViewCount;
}
- (int) getCurrentVisibleImageViewCount
{
    int count = 0;
    int pageViewCount = mPageRow * mPageCol;
    for(int i = mStartPage ; i <= mEndPage ; i++)
    {
        for(int j = i * pageViewCount ; j < (i * pageViewCount + pageViewCount) ; j++)
        {
            if(j < mImageViewCount)
            {
                count++;
            }
            else
            {
                break;
            }
        }
    }
    return count;
}

// return the number of imageview whose image count is not default image
// 
- (int) getVisibleImageViewCountNoneDefaultImage
{
    
    int count = 0;
    int pageViewCount = mPageRow * mPageCol;
    for(int i = mStartPage ; i <= mEndPage ; i++)
    {
        for(int j = i * pageViewCount ; j < (i * pageViewCount + pageViewCount) ; j++)
        {
            if(j < mImageViewCount)
            {
                SEPageUIImageView* v = [self imageView:j];
                if(v.image != mDefaultImage)
                {
                    count++;
                }
            }
            else 
            {
                break;
            }
        }
    }
    return count;
}
- (BOOL) imageURLContentInArray: (NSArray*) array : (SEPageImageURL*)url
{
    for(SEPageImageURL* pageURL in array)
    {
        BOOL urlEqual = [[url.url absoluteString] isEqualToString:[pageURL.url absoluteString]];
        BOOL fileURLEqual = url.filepath != nil && pageURL.filepath != nil && [[url.filepath absoluteString] isEqualToString:[pageURL.filepath absoluteString]];
        if(urlEqual || fileURLEqual)
            return YES;
    }
    return NO;
}
- (void) printPhotoAssetURL
{
    NSLog(@"photo asset url count = %d", mPhotoAssetURL.count);
    for(int i = 0 ; i < mPhotoAssetURL.count ; i++)
    {
        SEPageImageURL* url = [mPhotoAssetURL objectAtIndex:i];
        NSLog(@"photo image url = %@, %@", [url.url absoluteString], [url.filepath absoluteString]);
    }
}
- (void) assertPhotoAssert
{
    for(int i = 0 ; i < mPhotoAssetURL.count ; i++)
    {
        SEPageImageURL* url = [mPhotoAssetURL objectAtIndex:i];
        assert(url.url != nil || url.filepath != nil);
    }
}

- (void) initAssetLib
{
    //mAssetIdentity[0] = ALAssetsGroupAlbum;
    //mAssetIdentity[1] = ALAssetsGroupSavedPhotos;
    mAssetIdentity[0] =  ALAssetsGroupAll;
    mAssetIdentity[1] = 0;
    [mAssetLibrary release];
    //NSDate* date1 = [NSDate date];
    mAssetLibrary = [[ALAssetsLibrary alloc] init];
    //NSDate* date2 = [NSDate date];
    //NSTimeInterval timeInterv = [date2 timeIntervalSinceDate:date1];
    //NSLog(@"## time interv = %f ##\n", timeInterv);
}
- (BOOL) isInPhotoAssetArray: (SEPageImageURL*)imageURL
{
    for(SEPageImageURL* url in mPhotoAssetURL)
    {
        if([url.url isEqual:imageURL.url] && [url.urlDate isEqualToString:imageURL.urlDate])
            return YES;
    }
    return NO;
}
- (void) addPhotoByDateSequent: (SEPageImageURL*)url
{
    int index = 0;
    int count = mPhotoAssetURL.count;
    for(int j = 0 ; j < mPhotoAssetURL.count ; j++, index++)
    {
        SEPageImageURL* urlInPhotoAsset = [mPhotoAssetURL objectAtIndex:j];
        NSString* tmpUrlDate = urlInPhotoAsset.urlDate;
        NSComparisonResult ret = [tmpUrlDate compare:url.urlDate];
        if(ret == NSOrderedAscending)
        {
            break;
        }
    }
    if(index < mPhotoAssetURL.count)
    {
        [mPhotoAssetURL insertObject:url atIndex:index];
    }
    else
    {
        [mPhotoAssetURL addObject:url];
    }
    assert(mPhotoAssetURL.count == (count + 1));
}
- (void) createPhotoUrlArrayFromPhotoLib
{
    ALAssetsGroupEnumerationResultsBlock getPix =
    ^(ALAsset *result, NSUInteger index, BOOL* stop)
    {
        //NSLog(@"## reslult = %@ ###", result);
        if(!result)
            return;
        ALAssetRepresentation* rep = [result defaultRepresentation];
        NSURL* assetURL = [rep url];
        //NSThread* thread = [NSThread currentThread];
        //NSLog(@"enumerate assert thread = %@", thread);
        SEPageImageURL* imageURL = [[SEPageImageURL alloc] init];
        imageURL.url = assetURL;
        NSDate* date = [result valueForProperty: ALAssetPropertyDate];
        NSString* dateStr = [SEUtil dateToString: date];
        imageURL.urlDate = dateStr;
        
        //NSLog(@"url date = %@", dateStr);
        imageURL.type = SEPAGE_PHOTO_LIB_URL;
        //assert([self isInPhotoAssetArray:imageURL] == NO);
        if([self isInPhotoAssetArray:imageURL] == NO)
        {
            [self addPhotoByDateSequent:imageURL];
        }
        [imageURL release];
        //CGImageRef im = [rep fullResolutionImage];
        //NSLog(@"asset UTI = %@", [rep UTI]);
        *stop = NO;
    }   ;
    ALAssetsLibraryGroupsEnumerationResultsBlock getGroups =
    ^(ALAssetsGroup* group, BOOL* stop)
    {
        NSLog(@"## group = %@ ###", group);
        if(!group)
        {
            mCurrentAssetIndex++;
            if(mCurrentAssetIndex == 1)//ASSET_NUM)
            {
                [self performSelectorOnMainThread:@selector(createContent) withObject:nil waitUntilDone:NO];
            }
            return;
        }
        [group setAssetsFilter:[ALAssetsFilter allPhotos]];
        NSString* title = [group valueForProperty: ALAssetsGroupPropertyName];
        NSLog(@"title = %@", title);
        [group enumerateAssetsUsingBlock: getPix];
        *stop = NO;
    };
    ALAssetsLibraryAccessFailureBlock oops =
    ^(NSError* error)
    {
        NSLog(@"oops ! %@", [error localizedDescription]);
    };
    for(int i = 0 ; i < ASSET_NUM ; i++)
    {
        if(mAssetIdentity[i] != 0)
            [mAssetLibrary enumerateGroupsWithTypes:mAssetIdentity[i] usingBlock:getGroups failureBlock:oops];
    }
    NSLog(@"photo enumerate end");
}
- (void)createPhotoUrlArrayFromCoreData
{
    NSArray* siSet = [mViewNavigator getSelectedImageArrayByName:mImageListName];//[mViewNavigator getUserImageProperty];
    for(NSUInteger i = 0 ; i < [siSet count] ; i++)
    {
        SelectedImage*  si = [siSet objectAtIndex:i];
        NSNumber* seq = si.seq;
        NSString* str = si.filepath;
        NSString* url = [si.url copy];
        NSURL* fileURL = [NSURL URLWithString:str];
        SEPageImageURL* imageURL = [[SEPageImageURL alloc] init];
        NSURL* newURL = [NSURL URLWithString: url];
        imageURL.url = newURL;
        [url release];
        imageURL.urlDate = si.urldate;
        imageURL.filepath = fileURL;
        imageURL.type = (SEPAGE_URL_TYPE)[si.urltype intValue];
        imageURL.orientation = [si.orientation intValue];
        imageURL.origWidth = [si.width intValue];
        imageURL.origHeight = [si.height intValue];
        [mPhotoAssetURL addObject:imageURL];
        [imageURL release];
        assert(imageURL.url != nil || imageURL.filepath != nil);
        //debug
        if(str != nil || url != nil)
        {
            NSLog(@"seq = %@, str = %@, url = %@", seq, str, url);
        }
        //end
    }
    NSLog(@"### photo asset url num = %d ###", mPhotoAssetURL.count);
    
    //for test
    if([siSet count] > 0)
    {
        for(NSUInteger i = 0 ; i < [siSet count] - 1; i++)
        {
            SelectedImage* firstSi = [ siSet objectAtIndex:i];
            SelectedImage* secondSi = [siSet objectAtIndex:i + 1];
            NSNumber* firstNum = firstSi.seq;
            NSNumber* secondNum = secondSi.seq;
            assert(([firstNum intValue] + 1) == [secondNum intValue]);
        }
    }
    //end
    
    //[self checkAllSelectedImage:YES];
}
- (void) createPhotoUrlArray
{
    switch (mScrollViewType) {
        case PHOTOLIB_SCROLLVIEW:
            [self createPhotoUrlArrayFromPhotoLib];
            break;
        case COREDATA_SCROLLVIEW:
            [self createPhotoUrlArrayFromCoreData];
            break;
        default:
            break;
    }
}
- (void) updateImageView
{
    NSArray* pages = mParentView.subviews;
    int imageViewCount = [self getCurrentImageViewCount];
    assert(mPhotoAssetURL.count <= imageViewCount);
    for(int i = mStartPage ; i <= mEndPage ; i++)
    {
        UIView* page = [pages objectAtIndex:i];
        int count = page.subviews.count;
        for(int j = 0 ; j < count ; j++)
        {
            SEPageUIImageView* imageView = [page.subviews objectAtIndex:j];
            int index = i * mPageRow * mPageCol + j;
            assert(index < imageViewCount);
            if(index < imageViewCount)
            {
                SEPageImageURL* url = [self getImageURL:index];
                if(url != nil)
                {
                    SEPagePhotoDictKey* key = [SEPagePhotoDictKey createFromImageURL:url];
                    UIImage* image = [self getImageFromPhotoDict:key];
                    if(image)
                    {
                        if(image != imageView.image)
                            imageView.image = image;
                    }
                    else 
                    {
                        imageView.image = mDefaultImage;
                    }
                }
                else
                {
                    imageView.image = mDefaultImage;
                }
            }
        }
    }
}
//////////////////////////
- (void) finishLoadImage: (SELoadImageOperation*) loadImageOp
{
    //NSLog(@"finishLoadImage");
    assert([self isRunOnMainThread]);
    if([self isStopLoadImage])
    {
        //NSLog(@"loadImageOp retain count = %d", [loadImageOp retainCount]);
        [[NSNotificationCenter defaultCenter] removeObserver:self name: NOTIFICATION_LOADIMAGE_FROM_PHOTOLIB object:loadImageOp];
        [loadImageOp release];
        mIsLoadingImage = NO;
        return;
    }
    UIImage* image = loadImageOp.mResultImage;
    int index = loadImageOp.mPageLoadInfo.index;
    SEPageImageURL* url = loadImageOp.mURL;
    url.origWidth = loadImageOp.mOrigWidth;
    url.origHeight = loadImageOp.mOrigHeight;
    url.orientation = loadImageOp.mOrientation;
    //NSLog(@"load image width = %f, height = %f, orient = %d", url.origWidth, url.origHeight, url.orientation);
    /*
    if(url.origHeight == 0 && url.origWidth == 0 && mScrollViewType == COREDATA_SCROLLVIEW)
    {
        SEPageImageURL* photoURL = [mPhotoAssetURL objectAtIndex:index];
        photoURL.origWidth = 0;
        photoURL.origHeight = 0;
        [mViewNavigator setPhotoURLToCoreData:mPhotoAssetURL];
    }
     */
    if(image != nil)
    {
        SEPagePhotoDictKey* key = [SEPagePhotoDictKey createFromImageURL:url];
        UIImage* imageInDict = [self getImageFromPhotoDictByIndex:index];
        assert(imageInDict == nil);
        [self addImageToPhotoDict:key image:image];
        SEPageUIImageView* view = [self imageView:index];
        view.image = image;
    }
    else
    {
        SEPageImageURL* newURL = [[SEPageImageURL alloc] init];
        newURL.url = [NSURL URLWithString:@"*EmptyImageURL*"];
        newURL.type = SEPAGE_PHOTO_LIB_URL;
        SEPagePhotoDictKey* key = [SEPagePhotoDictKey createFromImageURL:newURL];
        [newURL release];
        UIImage* newImage = [self getImageFromPhotoDict:key];
        if(newImage == nil)
        {
            newImage = [mViewNavigator.mResLoader getImage:@"NoImageFound"];
            UIGraphicsBeginImageContext(CGSizeMake(80, 80));
            [newImage drawInRect:CGRectMake(0, 0, 80, 80)];
            UIImage* ret = UIGraphicsGetImageFromCurrentImageContext();
            UIGraphicsEndImageContext();
            [self addImageToPhotoDict:key image:ret];
        }
        SEPageUIImageView* view = [self imageView:index];
        if(view.image != newImage)
        {
            view.image = newImage;
        }
        
    }
    //NSLog(@"finishImageLoad index = %d, mNextLoadImageURLIndex = %d", index, mNextLoadImageURLIndex);
    assert(mNextLoadImageURLIndex == index);
    mIsLoadingImage = NO;
    
    if(loadImageOp.mPageLoadInfo.startPage != mStartPage || loadImageOp.mPageLoadInfo.endPage != mEndPage)
    {
        mNextLoadImageURLIndex = mStartPage * mPageRow * mPageCol;
        [self loadNextImageNew];
    }
    else
    {
        mNextLoadImageURLIndex++;
        int endIndex = mEndPage * mPageRow * mPageCol + mPageRow * mPageCol;
        if(mNextLoadImageURLIndex < endIndex)
            [self loadNextImageNew];
    }
    //NSLog(@"load image operation retain count = %d", [loadImageOp retainCount]);
    [[NSNotificationCenter defaultCenter] removeObserver:self name: NOTIFICATION_LOADIMAGE_FROM_PHOTOLIB object:loadImageOp];
    [loadImageOp release];
}
- (void) finishLoadImageFromCoreData: (SELoadImageFromCoreDataOperation*) loadImageOp
{
    //NSLog(@"finishLoadImageFromCoreData");
    if([self isStopLoadImage])
    {
        //NSLog(@"loadImageOp retain count = %d", [loadImageOp retainCount]);
        [[NSNotificationCenter defaultCenter] removeObserver:self name: NOTIFICATION_LOADIMAGE_FROM_COREDATA object:loadImageOp];
        [loadImageOp release];
        mIsLoadingImage = NO;
        return;
    }
    UIImage* image = loadImageOp.mResultImage;
    BOOL needLoadFromPhotoLib = loadImageOp.mNeedLoadFromPhotoLib;
    int index = loadImageOp.mPageLoadInfo.index;
    SEPageImageURL* url = loadImageOp.mURL;
    if(needLoadFromPhotoLib == NO)
    {
        NSLog(@"finishImageLoad index = %d, mNextLoadImageURLIndex = %d", index, mNextLoadImageURLIndex);
        SEPagePhotoDictKey* key = [SEPagePhotoDictKey createFromImageURL:url];
        UIImage* imageInDict = [self getImageFromPhotoDictByIndex:index];
        assert(imageInDict == nil);
        [self addImageToPhotoDict:key image:image];
        SEPageUIImageView* view = [self imageView:index];
        view.image = image;

        assert(mNextLoadImageURLIndex == index);
        mIsLoadingImage = NO;
        if(loadImageOp.mPageLoadInfo.startPage != mStartPage || loadImageOp.mPageLoadInfo.endPage != mEndPage)
        {
            mNextLoadImageURLIndex = mStartPage * mPageRow * mPageCol;
            [self loadNextImageNew];
        }
        else
        {
            mNextLoadImageURLIndex++;
            int endIndex = mEndPage * mPageRow * mPageCol + mPageRow * mPageCol;
            if(mNextLoadImageURLIndex < endIndex)
                [self loadNextImageNew];
        }
    }
    else
    {
        SEPageImageURL* newURL = [url clone];
        PageImageLoadInfo* loadInfo = [loadImageOp.mPageLoadInfo clone];
        [self getImageFromPhotoLib:newURL pageInfo:loadInfo];
    }
    //NSLog(@"load image operation from core data retain count = %d", [loadImageOp retainCount]);
    [[NSNotificationCenter defaultCenter] removeObserver:self name: NOTIFICATION_LOADIMAGE_FROM_COREDATA object:loadImageOp];
    [loadImageOp release];
}
- (void) finishLoadImageWrapper: (NSNotification*)notify
{
    NSObject* obj = [notify object];
    if([obj isMemberOfClass:[SELoadImageOperation class]])
    {
        [self performSelectorOnMainThread:@selector(finishLoadImage:) withObject:[notify object] waitUntilDone:NO];    
    }
    else if([obj isMemberOfClass:[SELoadImageFromCoreDataOperation class]])
    {
        [self performSelectorOnMainThread:@selector(finishLoadImageFromCoreData:) withObject:[notify object] waitUntilDone:NO];
    }
}
-(void) getImageFromCoreDataNew: (SEPageImageURL*)url pageInfo: (PageImageLoadInfo*)pageLoadInfo
{
    CGSize fitSize = CGSizeMake(mPhotoWidth - 20, mPhotoHeight - 20);
    SELoadImageFromCoreDataOperation* loadOperation = [[SELoadImageFromCoreDataOperation alloc] initWithURL:url pageInfo:pageLoadInfo];
    loadOperation.mFitSize = fitSize;
    loadOperation.mPageScrollView = self;
    mIsLoadingImage = YES;
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(finishLoadImageWrapper:) name:NOTIFICATION_LOADIMAGE_FROM_COREDATA object:loadOperation];
    [mOperationQueue addOperation:loadOperation];
}
- (void) getImageFromPhotoLib: (SEPageImageURL*)url pageInfo: (PageImageLoadInfo*)pageLoadInfo
{
    CGSize fitSize = CGSizeMake(mPhotoWidth - 20, mPhotoHeight - 20);
    PageImageLoadInfo* tmpPageLoadInfo = [pageLoadInfo retain];
    SEPageUIScrollView* retainSelf = [self retain];
    ALAssetsLibraryAssetForURLResultBlock getAsset = ^(ALAsset *asset)
    {
        assert([self isRunOnMainThread]);
        if([self isStopLoadImage])
        {
            [tmpPageLoadInfo release];
            [retainSelf release];
            return;
        }
        if(asset == nil)
        {
            NSLog(@"## image asset = nil ###");
        }
        //NSLog(@"load image operation index = %d, mNextLoadImageIndex = %d ", pageLoadInfo.index, mNextLoadImageURLIndex);
        SELoadImageOperation* loadImageOperation = [[SELoadImageOperation alloc] initWithURL:url pageInfo:tmpPageLoadInfo];
        [tmpPageLoadInfo release];
        loadImageOperation.mAsset = asset;
        loadImageOperation.mFitSize = fitSize;
        loadImageOperation.mIsFromCoreData = NO;
        loadImageOperation.mPageScrollView = retainSelf;
        [retainSelf release];
        //NSLog(@"before self retain count = %d", [self retainCount]);
        //NSLog(@"before operation count = %d", [loadImageOperation retainCount]);
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(finishLoadImageWrapper:) name:NOTIFICATION_LOADIMAGE_FROM_PHOTOLIB object:loadImageOperation];
        //NSLog(@"after self retain count = %d", [self retainCount]);
        //NSLog(@"after operation retain count = %d", [loadImageOperation retainCount]);
        [mOperationQueue addOperation:loadImageOperation];
    };
    ALAssetsLibraryAccessFailureBlock failHandler = ^(NSError *error)
    {
        mAssetAcessError = 1;
        //mIsLoadingImage = NO;
        NSLog(@"read photo lib error : %@", [error localizedDescription]);
        assert([self isRunOnMainThread]);
        if([self isStopLoadImage])
        {
            [tmpPageLoadInfo release];
            [retainSelf release];
            return;
        }
        SELoadImageOperation* loadImageOperation = [[SELoadImageOperation alloc] initWithURL:url pageInfo:tmpPageLoadInfo];
        [tmpPageLoadInfo release];
        loadImageOperation.mAsset = nil;
        loadImageOperation.mFitSize = fitSize;
        loadImageOperation.mIsFromCoreData = NO;
        loadImageOperation.mPageScrollView = retainSelf;
        [retainSelf release];
        //NSLog(@"before self retain count = %d", [self retainCount]);
        //NSLog(@"before operation count = %d", [loadImageOperation retainCount]);
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(finishLoadImageWrapper:) name:NOTIFICATION_LOADIMAGE_FROM_PHOTOLIB object:loadImageOperation];
        //NSLog(@"after self retain count = %d", [self retainCount]);
        //NSLog(@"after operation retain count = %d", [loadImageOperation retainCount]);
        [mOperationQueue addOperation:loadImageOperation];
    };
    mIsLoadingImage = YES;
    [mAssetLibrary assetForURL:url.url resultBlock:getAsset failureBlock:failHandler];
}
- (void) getImageFromURLNew:(SEPageImageURL*)url pageInfo: (PageImageLoadInfo*)pageLoadInfo
{
    switch (mScrollViewType) 
    {
        case PHOTOLIB_SCROLLVIEW:
        {
            [self getImageFromPhotoLib:url pageInfo:pageLoadInfo];
        }
            break;
        case COREDATA_SCROLLVIEW:
        {
            [self getImageFromCoreDataNew: url pageInfo: pageLoadInfo];
        }
            break;
        default:
            break;
    }

}
- (UIImage*) getImageFromPhotoDictByIndex: (int)index
{
    if(index < 0 || index >= mPhotoAssetURL.count)
        return nil;
    SEPageImageURL* url = [mPhotoAssetURL objectAtIndex:index];
    SEPagePhotoDictKey* key = [SEPagePhotoDictKey createFromImageURL:url];
    UIImage* image = [self getImageFromPhotoDict:key];
    return image;
}
- (BOOL) canLoadImageByIndex: (int)index
{
    if(index < 0 || index >= mPhotoAssetURL.count)
    {
        return NO;
    }
    if(mStopLoadImage == YES)
    {
        return NO;
    }
    UIImage* image = [self getImageFromPhotoDictByIndex:index];
    if(image == nil)
    {
        return YES;
    }
    else 
    {
        return NO;
    }
}
- (BOOL) isValidPhotoIndex: (int)index
{
    return index >= 0 && index < mPhotoAssetURL.count;
}
- (void) loadNextImageNew
{
    if(mNextLoadImageURLIndex < 0 || mNextLoadImageURLIndex >= mPhotoAssetURL.count)
    {
        mIsLoadingImage = NO;
        return;
    }
    if(mStopLoadImage == YES)
    {
        mIsLoadingImage = NO;
        return;
    }
    SEPageImageURL* url = [mPhotoAssetURL objectAtIndex:mNextLoadImageURLIndex];
    SEPagePhotoDictKey* key = [SEPagePhotoDictKey createFromImageURL:url];
    UIImage* image = [self getImageFromPhotoDict:key];
    if(image == nil)
    {
        //NSLog(@"loadnextimagenew mNextLoadImageURLIndex = %d", mNextLoadImageURLIndex);
        PageImageLoadInfo* pageInfo = [[[PageImageLoadInfo alloc] init] autorelease];
        pageInfo.startPage = mStartPage;
        pageInfo.endPage = mEndPage;
        pageInfo.index = mNextLoadImageURLIndex;
        [self getImageFromURLNew:url pageInfo:pageInfo];
    }
    else
    {
        SEPageUIImageView* view = [self imageView:mNextLoadImageURLIndex];
        if(view.image == mDefaultImage)
            view.image = image;
        while(image != nil)
        {
            mNextLoadImageURLIndex++;
            image = [self getImageFromPhotoDictByIndex:mNextLoadImageURLIndex];
            if(mNextLoadImageURLIndex >= 0 && mNextLoadImageURLIndex < mPhotoAssetURL.count)
            {
                view = [self imageView:mNextLoadImageURLIndex];
                if(view.image == mDefaultImage && image != nil)
                    view.image = image;
            }
        }
        int endIndex = mEndPage * mPageRow * mPageCol + mPageRow * mPageCol;
        if(mNextLoadImageURLIndex >= endIndex)
        {
            int startIndex = mStartPage * mPageRow * mPageCol;
            for(int i = startIndex ; i < endIndex ; i++)
            {
                if(i < mPhotoAssetURL.count)
                {
                    image = [self getImageFromPhotoDictByIndex:i];
                    if(image == nil)
                    {
                        mNextLoadImageURLIndex = i;
                        break;
                    }
                    else
                    {
                        view = [self imageView:i];
                        if(view.image == mDefaultImage)
                        {
                            view.image = image;
                        }
                    }
                }
            }
        }
        if(image == nil && [self isValidPhotoIndex: mNextLoadImageURLIndex] && mNextLoadImageURLIndex < endIndex)
        {
            //NSLog(@"2 loadnextimagenew mNextLoadImageURLIndex = %d", mNextLoadImageURLIndex);
            url = [mPhotoAssetURL objectAtIndex:mNextLoadImageURLIndex];
            PageImageLoadInfo* pageInfo = [[[PageImageLoadInfo alloc] init] autorelease];
            pageInfo.startPage = mStartPage;
            pageInfo.endPage = mEndPage;
            pageInfo.index = mNextLoadImageURLIndex;
            [self getImageFromURLNew: url pageInfo:pageInfo];
        }
    }
}

- (void) loadNextImage: (BOOL) continueLoad
{
    assert([self isRunOnMainThread]);
    NSArray* pages = mParentView.subviews;
    for(int i = mStartPage ; i <= mEndPage ; i++)
    {
        UIView* page = [pages objectAtIndex:i];
        int count = page.subviews.count;
        for(int j = 0 ; j < count ; j++)
        {
            int index = i * mPageRow * mPageCol + j;
            if(index < mPhotoAssetURL.count)
            {
                SEPageImageURL* url = [mPhotoAssetURL objectAtIndex:index];
                SEPagePhotoDictKey* key = [SEPagePhotoDictKey createFromImageURL:url];
                if(mScrollViewType == COREDATA_SCROLLVIEW)
                    NSLog(@"## image url = %@", key.url);
                UIImage* image = [self getImageFromPhotoDict:key];
                if(image == nil)
                {
                    //NSLog(@" load image start page = %d, end page = %d", mStartPage, mEndPage);
                    PageImageLoadInfo* pageInfo = [[PageImageLoadInfo alloc] init];
                    pageInfo.startPage = mStartPage;
                    pageInfo.endPage = mEndPage;
                    pageInfo.row = j / mPageCol;
                    pageInfo.col = j % mPageCol;
                    pageInfo.page = i;
                    [self getImageFromURL:url continueLoad:continueLoad pageInfo:pageInfo];
                    return;
                }
                else
                {
                    SEPageUIImageView* view = [self imageView:index];
                    //if(view.image != mDefaultImage)
                    view.image = image;
                }
            }
        }
    }
}
- (void) loadNextImageWrapper : (NSMutableArray*)data
{
    BOOL c = [[data objectAtIndex:0] boolValue];
    [self loadNextImage:c];
    [data release];
}
- (void) finishedLoadImage: (id)data
{
    assert([self isRunOnMainThread]);

    NSMutableArray* dataArray = (NSMutableArray*)data;
    SEPageImageURL* url = [dataArray objectAtIndex:0];
    UIImage* image = [dataArray objectAtIndex:1];
    BOOL continueLoad = [[dataArray objectAtIndex:2] boolValue];
    PageImageLoadInfo* pageInfo = [dataArray objectAtIndex:3];
    id urlData = [dataArray objectAtIndex:4];
    if(mScrollViewType == COREDATA_SCROLLVIEW)
    {
        int index = pageInfo.page * mPageRow * mPageCol + pageInfo.row * mPageCol + pageInfo.col;
        SEPageUIImageView* view = [self imageView:index];
        NSLog(@"loaed page = %d, row = %d, col = %d, imageview = %@", pageInfo.page, pageInfo.row, pageInfo.col, view);
    }
    if(urlData != [NSNull null])
    {
        int width = [[urlData objectAtIndex:0] intValue];
        int height = [[urlData objectAtIndex:1] intValue];
        int orient = [[urlData objectAtIndex:2] intValue];
        url.orientation = orient;
        url.origWidth = width;
        url.origHeight = height;
    }
    SEPagePhotoDictKey* key = [SEPagePhotoDictKey createFromImageURL:url];
    [self addImageToPhotoDict:key image:image];
    [dataArray release];
    //[self updateImageView];
    int index = pageInfo.page * mPageRow * mPageCol + pageInfo.row * mPageCol + pageInfo.col;
    SEPageUIImageView* view = [self imageView:index];
    assert(index < mPhotoAssetURL.count);
    view.image = image;
    //
    int startPage = pageInfo.startPage;
    int endPage = pageInfo.endPage;
    //BOOL bInterval = [self isIntervalContain:startPage :endPage :mStartPage :mEndPage] && [self isIntervalContain:mStartPage :mEndPage :startPage :endPage];
    BOOL bChanged = startPage != mStartPage || endPage != mEndPage;
    if(mScrollViewType == COREDATA_SCROLLVIEW)
    {
        NSLog(@"## loaded image = %@", url.url);
    }
    if(continueLoad && !bChanged && mStopLoadImage == NO)
    {
        //NSMutableArray* array = [NSMutableArray array];
        //[array addObject:[NSNumber numberWithBool:continueLoad]];
        //[array retain];
        //[self performSelector:@selector(loadNextImageWrapper:) withObject:array afterDelay:0.05];
        [self loadNextImage:continueLoad];
    }
}
- (void) loadImageHandle: (id)data
{
    NSObject<SELoadedImageHandler>* handler = (NSObject<SELoadedImageHandler>*)data;
    [handler handleImage];
    [handler release];
}
- (void) getImageFromPhotoLibWithThread: (id)data
{
    NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
    NSMutableArray* dataArray = (NSMutableArray*)data;
    SEPageImageURL* url = [dataArray objectAtIndex:0];
    CGSize fitSize = [[dataArray objectAtIndex:1] CGSizeValue];
    BOOL continueLoad = [[dataArray objectAtIndex:2] boolValue];
    PageImageLoadInfo* pageInfo = [data objectAtIndex:3];
    SEPageImageURL* passedURL = [url retain];
    [dataArray release];
    ALAssetsLibraryAssetForURLResultBlock getAsset = ^(ALAsset *asset)
    {
        NSAutoreleasePool* newPool = [[NSAutoreleasePool alloc] init];
        [self isRunOnWorkderThread];
        ALAssetRepresentation* rep = [asset defaultRepresentation];
        //CGImageRef image = [asset thumbnail];
        //NSLog(@"## thumb width = %lu, height = %lu\n", CGImageGetWidth(image),CGImageGetHeight(image));
        
        ALAssetOrientation orient = [rep orientation];
        CGFloat scale = [rep scale];
        //NSLog(@"image orientation = %d \n", orient);
        CGImageRef image = [rep fullResolutionImage];
        //CGImageRef thumbnail = [asset thumbnail];
        CGSize srcS = CGSizeMake(CGImageGetWidth(image), CGImageGetHeight(image));
        CGSize s = [SEUtil computeFitSize:srcS toDst:fitSize];
        //CGImageRef retImage = [SEUtil fastScale:thumbnail withRect:s];
        CGImageRef retImage = [SEUtil CGImageDrawInRect:image rect:s];
        if(mScrollViewType == COREDATA_SCROLLVIEW)
        {
            float width = CGImageGetWidth(retImage);
            float height = CGImageGetHeight(retImage);

            NSLog(@"## image width = %lu, height = %lu # \n", CGImageGetWidth(image), CGImageGetHeight(image));
            NSLog(@"## image orient = %d ##\n", orient);
            NSLog(@"## retImage with = %f, height = %f ##\n", width, height);
        }
        UIImageOrientation o = (UIImageOrientation)orient;
        UIImage* uiImage = [UIImage imageWithCGImage:retImage scale:scale orientation:o];
        CGImageRelease(retImage);
        /*
        if(mScrollViewType == PHOTOLIB_SCROLLVIEW)
        {
            passedURL.origWidth = srcS.width;
            passedURL.origHeight = srcS.height;
            passedURL.orientation = o;
        }
         */
        NSMutableArray* urlData = [NSMutableArray array];
        [urlData addObject:[NSNumber numberWithInt:srcS.width]];
        [urlData addObject:[NSNumber numberWithInt:srcS.height]];
        [urlData addObject:[NSNumber numberWithInt:o]];
        NSMutableArray* retData = [NSMutableArray array];
        retData = [retData retain];
        [retData addObject:passedURL];
        [retData addObject:uiImage];
        [retData addObject:[NSNumber numberWithBool:continueLoad]];
        [retData addObject:pageInfo];
        [retData addObject:urlData];
        [passedURL release];
        [self performSelectorOnMainThread:@selector(finishedLoadImage:) withObject:retData waitUntilDone:NO];
        [newPool release];
        
    };
    ALAssetsLibraryAccessFailureBlock failHandler = ^(NSError *error)
    {
        if(error)
        {
            mAssetAcessError = 1;
            NSLog(@"read photo lib error : %@", [error localizedDescription]);
        }
    };
    [mAssetLibrary assetForURL:url.url resultBlock:getAsset failureBlock:failHandler];
    [pool release];
}
- (void) getImageFromPhotoLib:(SEPageImageURL *)url continueLoad:(BOOL) continueLoad pageInfo:(PageImageLoadInfo*)pageInfo
{
    //[self garbageClean];
    CGSize fitSize = CGSizeMake(mPhotoWidth - 20, mPhotoHeight - 20);
    NSMutableArray* data = [NSMutableArray array];
    data = [data retain];
    [data addObject:url];
    [data addObject:[NSValue valueWithCGSize:fitSize]];
    [data addObject:[NSNumber numberWithBool:continueLoad]];
    [data addObject:pageInfo];
    [self performSelectorInBackground:@selector(getImageFromPhotoLibWithThread:) withObject:data];
}
- (void) getImageFromCoreDataWithThread: (NSMutableArray*)array
{
    NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
    SEPageImageURL* url = [array objectAtIndex:1];
    BOOL continueLoad = [[array objectAtIndex:0] boolValue];
    PageImageLoadInfo* pageInfo = [array objectAtIndex:2];
    NSManagedObjectContext* moc = [[NSManagedObjectContext alloc] init];
    [moc setPersistentStoreCoordinator:mViewNavigator.persistentStoreCoordinator];
    UIImage* image = [mViewNavigator getImageFromCoreData:[SEUtil urlToString:url.url] urlDate:url.urlDate managedObjectContext:moc];
    [moc release];
    if(image)
    {
        CGImageRef imageRef = [image CGImage];
        //CGSize srcSize = CGSizeMake(image.size.width, image.size.height);
        CGSize srcSize = CGSizeMake(CGImageGetWidth(imageRef), CGImageGetHeight(imageRef));
        CGSize dstSize = CGSizeMake(mPhotoWidth - 20, mPhotoHeight - 20);
        dstSize = [SEUtil computeFitSize:srcSize toDst:dstSize];
        CGImageRef retImage = [SEUtil fastScale:imageRef withRect:dstSize];
        NSLog(@"raw image width = %ld, height = %ld", CGImageGetWidth(retImage), CGImageGetHeight(retImage));
        UIImage* retUIImage = [UIImage imageWithCGImage:retImage scale:1.0 orientation: (UIImageOrientation)url.orientation];
        NSLog(@"image width = %f, height = %f, orientation = %d", retUIImage.size.width, retUIImage.size.height, retUIImage.imageOrientation);
        CGImageRelease(retImage);
        NSMutableArray* retData = [NSMutableArray array];
        retData = [retData retain];
        [retData addObject:url];
        [retData addObject:retUIImage];
        [retData addObject:[NSNumber numberWithBool:continueLoad]];
        [retData addObject:pageInfo];
        [retData addObject:[NSNull null]];
        [self performSelectorOnMainThread:@selector(finishedLoadImage:) withObject:retData waitUntilDone:NO];
    }
    else 
    {
    
        NSURL* photoURL = url.url;
        if(photoURL)
        {
            [self getImageFromPhotoLib:url continueLoad: continueLoad pageInfo:pageInfo];
        }
    }
    //[url release];
    [array release];
    [pool release];
}
- (void) getImageFromCoreData: (SEPageImageURL*)url continueLoad:(BOOL)continueLoad pageInfo:(PageImageLoadInfo*)pageInfo
{
    SEPageImageURL* passedURL = url;//[url retain];
    NSMutableArray* array = [NSMutableArray array];
    array = [array retain];
    NSNumber* n = [NSNumber numberWithBool:continueLoad];
    [array addObject:n];
    [array addObject:passedURL];
    [array addObject:pageInfo];
    [self performSelectorInBackground:@selector(getImageFromCoreDataWithThread:) withObject:array];
}
- (void) getImageFromURL:(SEPageImageURL*)url continueLoad:(BOOL)continueLoad pageInfo:(PageImageLoadInfo*)pageInfo
{
    switch (mScrollViewType) 
    {
        case PHOTOLIB_SCROLLVIEW:
            [self getImageFromPhotoLib:url continueLoad:continueLoad pageInfo:pageInfo] ;
            break;
        case COREDATA_SCROLLVIEW:
            [self getImageFromCoreData:url continueLoad:continueLoad pageInfo:pageInfo];
            break;
        default:
            break;
    }
}
- (void) doScaleAnimationForInsert
{
    int count = mCurrentImageArray.count;
    mAnimEndCount = count;
    for(int i = 0 ;i < count ; i++)
    {
        SEPageUIImageView* v = [self imageView:mLastImageViewIndex + i];
        SEPageUIImageView* scaleView = [[SEPageUIImageView alloc] initWithFrame:v.frame];
        scaleView.image = [mCurrentImageArray objectAtIndex:i];
        scaleView.backgroundColor = [UIColor clearColor];
        [v.superview addSubview:scaleView];
        [scaleView release];
        CGAffineTransform m = CGAffineTransformMakeScale(2, 2);
        scaleView.transform = m;
        void (^animBlock)(void) = ^{
            scaleView.transform = CGAffineTransformIdentity;
        };
        void (^animEnd) (BOOL) = ^(BOOL){
            v.image = [mCurrentImageArray objectAtIndex:i];
            mAnimEndCount--;
            if(mAnimEndCount == 0)
            {
                [mCurrentImageArray release];
                mCurrentImageArray = nil;
            }
            mLastImageViewIndex = -1;
            [scaleView removeFromSuperview];
        };
        int opts = UIViewAnimationOptionCurveLinear;
        [UIView animateWithDuration:0.5 delay: 0 options: opts animations:animBlock completion:animEnd];
    }
}
/*
 this function access mEndPage which is shared by worker thread and main thread.
 But we will not add lock about it because when access mEndPage worker thread will not access it
*/
- (void) createSingleColumnContent: (SEPAGE_PART_TYPE)reservePart
{
    assert(mStartPage != mEndPage);
    int startIndex = 0;
    int endIndex = mPageCount - 1;
    if(reservePart == RIGHT_PAGE)
    {
        for(int i = startIndex ; i <= endIndex ; i++)
        {
            UIView* v = [mParentView.subviews objectAtIndex:i];
            v.frame = CGRectMake(mPageWidth, i * mPageHeight, mPageWidth, mPageHeight);
        }
    }
    else if(reservePart == LEFT_PAGE)
    {
        for(int i = startIndex ; i <= endIndex ; i++)
        {
            UIView* v = [mParentView.subviews objectAtIndex:i];
            v.frame = CGRectMake(0, i * mPageHeight, mPageWidth, mPageHeight);
        }
    }

}
- (void) setPageFrame: (int)pageIndex
{
    UIView* v = [mParentView.subviews objectAtIndex:pageIndex];
    if(pageIndex % 2 == 0)
    {
        v.frame = CGRectMake(0, (pageIndex / 2) * mPageHeight, mPageWidth, mPageHeight);
    }
    else
    {
        v.frame = CGRectMake(mPageWidth, (pageIndex / 2) * mPageHeight, mPageWidth, mPageHeight);    
    }
}
/*
 */
- (void) createMultiColumnContent : (SEPAGE_PART_TYPE)movePart
{
    assert(mStartPage == mEndPage);
    if(movePart == LEFT_PAGE)
    {          
        UIView* leftPage = nil;
        UIView* rightPage = nil;
        int prevPageEnd = 0;
        int nextPageStart = 0;
        if(mStartPage % 2 == 0)
        {
            leftPage = [mParentView.subviews objectAtIndex:mStartPage + 1];
            rightPage = [mParentView.subviews objectAtIndex:mStartPage];
            prevPageEnd = mStartPage - 1;
            nextPageStart = mStartPage + 2;
        }
        else
        {
            leftPage = [mParentView.subviews objectAtIndex:mStartPage - 1];
            rightPage = [mParentView.subviews objectAtIndex:mStartPage];
            prevPageEnd = mStartPage - 2;
            nextPageStart = mStartPage + 1;

        }
        leftPage.frame = CGRectMake(0, (mStartPage / 2) * mPageHeight, mPageWidth, mPageHeight);
        rightPage.frame = CGRectMake(mPageWidth, (mStartPage / 2) * mPageHeight, mPageWidth, mPageHeight);
        for(int i = 0 ; i <= prevPageEnd ; i++)
        {
            [self setPageFrame:i];
        }
        for(int i = nextPageStart ; i < mPageCount ; i++)
        {
            [self setPageFrame:i];
        }
    }
    else if(movePart == RIGHT_PAGE)
    {
        UIView* leftPage = nil;
        UIView* rightPage = nil;
        int prevPageEnd = 0;
        int nextPageStart = 0;
        if(mStartPage % 2 == 0)
        {
            leftPage = [mParentView.subviews objectAtIndex:mStartPage];
            rightPage = [mParentView.subviews objectAtIndex:mStartPage + 1];
            prevPageEnd = mStartPage - 1;
            nextPageStart = mStartPage + 2;
        }
        else
        {
            leftPage = [mParentView.subviews objectAtIndex:mStartPage];
            rightPage = [mParentView.subviews objectAtIndex:mStartPage - 1];
            prevPageEnd = mStartPage - 2;
            nextPageStart = mStartPage + 1;
        }
        leftPage.frame = CGRectMake(0, (mStartPage / 2) * mPageHeight, mPageWidth, mPageHeight);
        rightPage.frame = CGRectMake(mPageWidth, (mStartPage / 2) * mPageHeight, mPageWidth, mPageHeight);
        for(int i = 0 ; i <= prevPageEnd ; i++)
        {
            [self setPageFrame:i];
        }
        for(int i = nextPageStart ; i < mPageCount ; i++)
        {
            [self setPageFrame:i];
        }
     }
}
- (BOOL) isRunOnMainThread
{
    NSThread* t = [NSThread currentThread];
    NSThread* m = [NSThread mainThread];
    return (t == m);
}
- (BOOL) isRunOnWorkderThread
{
    NSThread* t = [NSThread currentThread];
    NSThread* m = [NSThread mainThread];
    return (t != m);
}

- (void) removeAllImageViews
{
    NSArray* subviews = [mParentView subviews];
    for(NSUInteger i = 0  ; i < [subviews count] ; i++)
    {
        UIView* page = [subviews objectAtIndex:i];
        [page removeFromSuperview];
    }
}
- (UIImage*) getImageFromPhotoDict: (SEPagePhotoDictKey*)key
{
    for(SEPagePhotoDictItem* item in mPhotoDict)
    {
        if([item.key isEqualToKey:key])
        {
            return item.image;
        }
    }
    return nil;
}
- (void) addImageToPhotoDict: (SEPagePhotoDictKey*) key image: (UIImage*)image
{
    for(SEPagePhotoDictItem* item in mPhotoDict)
    {
        if([item.key isEqualToKey:key])
        {
            item.image = image;
            return;
        }
    }
    SEPagePhotoDictItem* item = [[SEPagePhotoDictItem alloc] init];
    item.key = key;
    item.image = image;
    [mPhotoDict addObject:item];
    [item release];
}
- (SEPagePhotoDictItem*) findItemInPhotoDict: (SEPagePhotoDictKey*)key
{
    for(SEPagePhotoDictItem* item in mPhotoDict)
    {
        if([item.key isEqualToKey:key])
            return item;
    }
    return nil;
}
- (void) removeImageFromPhotoDict: (SEPagePhotoDictKey*)key
{
    SEPagePhotoDictItem* item = [self findItemInPhotoDict:key];
    if(item)
    {
        [mPhotoDict removeObject:item];
    }
}
- (size_t) getImageSizeSumInDict
{
    size_t sum = 0;
    for(SEPagePhotoDictItem* item in mPhotoDict)
    {
        CGImageRef imageRef = [item.image CGImage];
        size_t h = CGImageGetHeight(imageRef);
        size_t bytesPerRow = CGImageGetBytesPerRow(imageRef);
        sum += h * bytesPerRow;
    }
    return sum;
}

//this function run in main thread
- (void) removeImageFromView : (PageGarbageInfo*) gbInfo
{
    assert([self isRunOnMainThread]);
    int index = gbInfo.index;
    switch (gbInfo.op)
    {
        case INDEX_LESS:
        {
            for(int i = 0 ; i < index && i < mPhotoAssetURL.count; i++)
            {
                SEPageUIImageView* iv = [self getImageViewByIndex:i];
                if(iv.image != mDefaultImage)
                {
                    NSLog(@" remove image ref count = %d", [iv.image retainCount]);
                }
                iv.image = mDefaultImage;
            }
        }
            break;
        case INDEX_GREAT:
        {
            for(int i = index + 1 ; i < mPhotoAssetURL.count ; i++)
            {
                SEPageUIImageView* iv = [self getImageViewByIndex:i];
                if(iv.image != mDefaultImage)
                {
                    NSLog(@" remove image ref count = %d", [iv.image retainCount]);
                }
                iv.image = mDefaultImage;
            }
        }
            break;
        case INDEX_EQUAL:
        {
            SEPageUIImageView* iv = [self getImageViewByIndex:index];
            if(iv.image != mDefaultImage)
            {
                NSLog(@" remove image ref count = %d", [iv.image retainCount]);
            }
            iv.image = mDefaultImage;
        }
            break;
    }
    [gbInfo release];
}
- (void) removeImageFromPhotoDictByURL: (SEPageImageURL*)url
{
    SEPagePhotoDictKey* key = [[SEPagePhotoDictKey alloc] init];
    key.url = [url.url absoluteString];
    key.date = url.urlDate;
    key.type = url.type;
    [self removeImageFromPhotoDict:key];
    [key release];
}
// this function run in main thread
- (void) garbageClean
{
    assert([self isRunOnMainThread]);
    size_t sum = [self getImageSizeSumInDict];
    NSLog(@"## image sum = %lu ###\n", sum);
    if(sum < 30 * 1024 * 1024)
        return;
    NSLog(@"### garbage clean ###");
    int prevEndPageIndex = mStartPage - 1;
    int nextStartPageIndex = mEndPage + 1;
    int assetURLCount = mPhotoAssetURL.count;
    if(prevEndPageIndex > 0)
    {
        int lastIndex = prevEndPageIndex * mPageCol * mPageRow + mPageRow * mPageCol;
        for(int i = 0 ; i < lastIndex ; i++)
        {
            if(i < assetURLCount)
            {
                SEPageImageURL* url = [self getImageURL:i];//[mPhotoAssetURL objectAtIndex:i];
                if(url != nil)
                    [self removeImageFromPhotoDictByURL:url];
            }
        }
        PageGarbageInfo* gbInfo = [[PageGarbageInfo alloc] init];
        gbInfo.index = lastIndex;
        gbInfo.op = INDEX_LESS;
        [self removeImageFromView:gbInfo];
        /*
        [self performSelectorOnMainThread:@selector(removeImageFromView:) withObject:gbInfo waitUntilDone:NO];
         */
    }
    if(nextStartPageIndex < mPageCount)
    {
        int firstIndex = nextStartPageIndex * mPageCol * mPageRow;
        int imageViewCount = [self getCurrentImageViewCount];
        for(int i = firstIndex ; i < imageViewCount; i++)
        {
            if(i < assetURLCount)
            {
                SEPageImageURL* url = [self getImageURL:i];//[mPhotoAssetURL objectAtIndex:i];
                [self removeImageFromPhotoDictByURL:url];
            }
        }
        PageGarbageInfo* gbInfo = [[PageGarbageInfo alloc] init];
        gbInfo.index = firstIndex;
        gbInfo.op = INDEX_GREAT;
        [self removeImageFromView:gbInfo];
    }
}
//this function run in main thread. At this time worker thread has exited. So main thread can
//access the data owned by worker thread
- (void) clearState
{
    //assert([self isRunOnMainThread]);
    [mPhotoDict release];
    [mPhotoAssetURL release];
    [mAssetLibrary release];
}

- (BOOL) isIntervalContain: (int) srcStart : (int)srcEnd : (int) dstStart : (int) dstEnd
{
    
    if(dstStart >= srcStart && dstStart <= srcEnd && dstEnd >= srcStart && dstEnd <= srcEnd)
        return YES;
    else
        return NO;
}

- (SEPageUIImageView*) getImageViewByIndex : (int)index
{
    assert([self isRunOnMainThread]);
    int page = index / (mPageRow * mPageCol);
    int imageIndex = index % (mPageRow * mPageCol);
    NSArray* subviews = [mParentView subviews];
    UIView* pageView = [subviews objectAtIndex:page];
    subviews = pageView.subviews;
    SEPageUIImageView* view = (SEPageUIImageView*)[subviews objectAtIndex: imageIndex];
    return view;
}
- (void) setImageToView : (PageImageData*) imageData
{
    assert([self isRunOnMainThread]);
    //NSLog(@"image view index = %d\n", imageData.index);
    SEPageUIImageView* view = [self getImageViewByIndex:imageData.index];
    UIImage* uiImage = mDefaultImage;
    if(imageData.imageRef)
        uiImage = [UIImage imageWithCGImage:imageData.imageRef];
    if(uiImage)
    {
        view.image = uiImage;
    }
    CGImageRelease(imageData.imageRef);
    [imageData release];
}

- (void) calculatePageRowCol: (int) index: (int*)outPage : (int*)outRow : (int*)outCol
{
    int page = index / (mPageRow * mPageCol);
    int nReminder = index % (mPageRow * mPageCol);
    int row = 0, col = 0;
    if(nReminder > 0)
    {
        row = nReminder / mPageCol;
        col = nReminder % mPageCol;
    }
    *outPage = page;
    *outRow = row;
    *outCol = col;
}
- (BOOL) imageURLInArray: (NSMutableArray*) array : (SEPageImageURL*)url
{
    for(SEPageImageURL* u in array)
    {
        if(url == u)
            return YES;
    }
    return NO;
}
- (void) moveURLs: (NSMutableArray*)indexArray toIndex: (int)dstIndex
{
    assert([self isRunOnMainThread]);
    assert(dstIndex <= mPhotoAssetURL.count);
    for(int i = 0 ;i  < indexArray.count ; i++)
    {
        NSLog(@"## moved index  = %@", [indexArray objectAtIndex:i]);
    }
    NSLog(@"## dest index = %d ##", dstIndex);
    NSMutableArray* newPhotoURL = [NSMutableArray array];
    NSMutableArray* needChangeURLs = [NSMutableArray array];
    for(NSNumber* n in indexArray)
    {
        int index = [n intValue];
        SEPageImageURL* imageURL = [mPhotoAssetURL objectAtIndex:index];
        [needChangeURLs addObject:imageURL];
    }
    for(int i = 0 ; i < dstIndex ; i++)
    {
        SEPageImageURL* imageURL = [mPhotoAssetURL objectAtIndex:i];
        if([self imageURLInArray:needChangeURLs :imageURL] == NO)
        {
            [newPhotoURL addObject:imageURL];
        }
    }
    for(int i = 0 ; i < needChangeURLs.count ; i++)
    {
        [newPhotoURL addObject:[needChangeURLs objectAtIndex:i]];
    }
    /// test
    for(int i = 0 ; i < mPhotoAssetURL.count ; i++)
    {
        SEPageImageURL* url = [mPhotoAssetURL objectAtIndex:i];
        NSLog(@"before url %d = %@ width = %f, height = %f, orient = %d", i, url.url, url.origWidth, url.origHeight, url.orientation);
    }
    ///
    int lastURLIndex = [self lastURLIndex];
    int lastDefaultImageViewIndex = (int)[self lastImageViewIndex];
    NSLog(@"last url index = %d, last default image view = %d ", lastURLIndex,lastDefaultImageViewIndex);
    assert(lastURLIndex == lastDefaultImageViewIndex);
    if(dstIndex < lastURLIndex)
    {
        int startIndex = dstIndex;
        SEPageImageURL* url = [mPhotoAssetURL objectAtIndex:dstIndex];
        if([self imageURLInArray:needChangeURLs:url] == YES)
        {
            startIndex = dstIndex + 1;
        }
        for(int i = startIndex ; i < lastURLIndex ; i++)
        {
            SEPageImageURL* url = [mPhotoAssetURL objectAtIndex:i];
            if([self imageURLInArray:needChangeURLs :url] == NO)
            {
                [newPhotoURL addObject:url];
            }
        }
    }
    NSLog(@"newPhotoURL count = %d , lastURLIndex = %d", newPhotoURL.count ,lastURLIndex);
    assert(newPhotoURL.count == lastURLIndex);
    [mPhotoAssetURL release];
    mPhotoAssetURL = [newPhotoURL retain];
    //for test
    for(int i = 0 ; i < mPhotoAssetURL.count ; i++)
    {
        SEPageImageURL* url = [mPhotoAssetURL objectAtIndex:i];
        NSLog(@"new url width = %f, height = %f, orient = %d", url.origWidth, url.origHeight, url.orientation);
    }
    //end
}

//we can prove that this function spec:
- (void) insertURLToPhotoAssetArray: (NSMutableArray*)urlArray : (int) index : (int) lastURLIndex
{
    assert([self isRunOnMainThread]);
    assert(index <= lastURLIndex);
    assert(lastURLIndex == mPhotoAssetURL.count);
    int oldCount = mPhotoAssetURL.count;
    NSArray* oldPhotoAssertURL = [NSArray arrayWithArray:mPhotoAssetURL];
    [self printPhotoAssetURL];
    NSMutableArray* newPhotoURL = [NSMutableArray array];
    for(int i = 0 ; i < index ; i++)
    {
        [newPhotoURL addObject: [mPhotoAssetURL objectAtIndex:i]];
    }
    for(int i = 0 ; i < urlArray.count ; i++)
    {
        SEPageURLID* urlID = [urlArray objectAtIndex:i];
        SEPageImageURL* imageURL = [[SEPageImageURL alloc] init];//
        imageURL.url = urlID.url;
        imageURL.urlDate = urlID.urlDate;
        imageURL.origWidth = urlID.origWidth;
        imageURL.origHeight = urlID.origHeight;
        imageURL.orientation = urlID.orientation;
        imageURL.filepath = nil;
        imageURL.type = SEPAGE_PHOTO_LIB_URL;
        [newPhotoURL addObject:imageURL];
        [imageURL release];
    }
    for(int i = index ; i < lastURLIndex ; i++)
    {
        [newPhotoURL addObject:[mPhotoAssetURL objectAtIndex:i]];
    }
    [mPhotoAssetURL release];
    mPhotoAssetURL = [newPhotoURL retain];
    //for post condition
    assert(mPhotoAssetURL.count == (oldCount + urlArray.count));
    for(SEPageImageURL* url in oldPhotoAssertURL)
    {
        assert([self imageURLContentInArray:mPhotoAssetURL :url]);
    }
    for(SEPageURLID* urlID in urlArray)
    {
        SEPageImageURL* imageURL = [[SEPageImageURL alloc] init];//
        imageURL.url = urlID.url;
        imageURL.urlDate = urlID.urlDate;
        imageURL.origWidth = urlID.origWidth;
        imageURL.origHeight = urlID.origHeight;
        imageURL.orientation = urlID.orientation;
        imageURL.filepath = nil;
        imageURL.type = SEPAGE_PHOTO_LIB_URL;
        assert([self imageURLContentInArray:mPhotoAssetURL :imageURL]);
        [imageURL release];
    }
    //post condition end
}
//return the nil url index
- (int) lastURLIndex
{
    assert([self isRunOnMainThread]);
    int index = -1;
    for(int i = 0 ; i < mPhotoAssetURL.count ; i++)
    {
        SEPageImageURL* url = [mPhotoAssetURL objectAtIndex:i];
        if(url.url != nil || url.filepath != nil)
            index = i;
    }
    assert(index == -1 || (index + 1)== mPhotoAssetURL.count);
    return index + 1;
}
- (void) setCurrentPhotoURLToCoreData
{
    assert([self isRunOnMainThread]);
    NSMutableArray* photoURLArray = mPhotoAssetURL;
    [mViewNavigator setPhotoURLToCoreData: photoURLArray];
}
- (void) saveCurrentPhotoURL
{
    assert([self isRunOnMainThread]);
    [self setCurrentPhotoURLToCoreData];
}
- (void) savePhotoThumbnailToCoreData : (NSArray*)urlArray
{
    assert([self isRunOnMainThread]);
    NSArray* photoURLArray = urlArray;//(NSMutableArray*)data;
    int index = 0;
    for(SEPageImageURL* url in photoURLArray)
    {
        if(url.url != nil)
        {
            SELoadFullRepresentation* fullLoad = [[SELoadFullRepresentation alloc] init];
            fullLoad.url = url;
            fullLoad.viewNav = mViewNavigator;
            fullLoad.index = index;
            [self loadFullRepresentation:url withHandler:fullLoad];
            index++;
        }
    }
}
- (void)saveCurrentPhotoThumbnail : (NSArray*)urlArray
{
    [self savePhotoThumbnailToCoreData : urlArray];
}

- (BOOL) containIndex: (NSMutableArray*)array index: (int)index
{
    for(NSNumber* num in array)
    {
        int i = [num intValue];
        if(i == index)
            return YES;
    }
    return NO;
}
- (void)removeImageFromCoreData: (NSArray*)urlArray
{
    assert([self isRunOnMainThread]);
    //you must remove urlArray from mPhotoAssetURL first
    for(SEPageImageURL* url in urlArray)
    {
        if([self imageURLContentInArray:mPhotoAssetURL :url] == NO)
            [mViewNavigator removeImageFromCoreDate:[url.url absoluteString] urlDate:url.urlDate];
    }
}
- (void) removeURLFromPhotoURLAsset: (NSMutableArray*)indexArray
{
    assert([self isRunOnMainThread]);
    NSMutableArray* newArray = [NSMutableArray array];
    int oldCount = mPhotoAssetURL.count;
    for(int i = 0 ; i < mPhotoAssetURL.count; i++)
    {
        if([self containIndex:indexArray index:i] == NO)
        {
            [newArray addObject:[mPhotoAssetURL objectAtIndex:i]];
        }
    }
    [mPhotoAssetURL release];
    mPhotoAssetURL = [newArray retain];
    assert(oldCount == (mPhotoAssetURL.count + indexArray.count));
}


- (int) getImageSize:(UIImage*)image
{
    CGImageRef imageRef = [image CGImage];
    size_t bytesPerRow = CGImageGetBytesPerRow(imageRef);
    size_t h = CGImageGetHeight(imageRef);
    return h * bytesPerRow;
}

- (void) calculateVisiblePage: (CGFloat)contentOffset;
{
    CGFloat starty = -contentOffset;
    int startPageIndex, endPageIndex;
    int pageRowCount = 0;
    if(mIsSingleColumn)
    {
        pageRowCount = mPageCount;
    }
    else
    {
        pageRowCount = mPageCount / 2;
    }
    int pageHeight = mPageHeight;
    int n = 0;
    int startRow = 0;
    int endRow = 0;
    CGFloat pageTopPadding = 0;
    CGFloat pageVMargin = 0;
    starty += pageTopPadding;
    starty += pageHeight;
    while(starty <= 0)
    {
        starty += pageVMargin + pageHeight;
        n++;
    }
    startRow = n;
    
    n = 0;
    starty = -contentOffset;
    starty += pageTopPadding;
    while(starty < self.frame.size.height)
    {
        starty += pageHeight + pageVMargin;
        n++;
    }
    endRow = n - 1;
    //NSLog(@"## row = %d, %d ##\n", startRow, endRow);
    if(endRow == -1)
        endRow = startRow;
    else if(endRow >= pageRowCount)
        endRow = pageRowCount - 1;
    if(startRow != -1 && endRow != -1)
    {
        int prevStartPage = mStartPage, prevEndPage = mEndPage;
        //NSLog(@"mIsSingleColumn = %d", mIsSingleColumn);
        if(mIsSingleColumn)
        {
            startPageIndex = startRow;
            endPageIndex = endRow;
        }
        else
        {
            startPageIndex = startRow * 2;
            endPageIndex = endRow * 2 + 1;
        }
        mStartPage = startPageIndex;
        mEndPage = endPageIndex;
        if(prevStartPage != mStartPage || prevEndPage != mEndPage)
        {
            NSLog(@"## current start, end page = %d, %d", mStartPage, mEndPage);
            if(mIsLoadingImage == NO)
            {
                mNextLoadImageURLIndex = mStartPage * mPageRow * mPageCol;
                [self loadNextImageNew];
            }
        }

    }
}
- (void) initPhotoDict
{
    if(mPhotoDict)
        [mPhotoDict release];
    mPhotoDict = [[NSMutableArray alloc] init];
}
//the last image view which image is default image
- (int) lastImageViewIndex
{
    /*
    int pageIndex = 0;
    for(UIView* page in mParentView.subviews)
    {
        int viewIndex = 0;
        for(SEPageUIImageView* v in page.subviews)
        {
            UIImage* image = v.image;
            if(image == mDefaultImage)
                return pageIndex * mPageRow * mPageCol + viewIndex;
            else
                viewIndex++;
        }
        pageIndex++;
    }
     */
    return mPhotoAssetURL.count;
}
/*
- (void) getImageFromPhotoLib:(SEPageImageURL *)url
{}
 */
- (void) loadImageFromPhotoLibWithThread:(NSMutableArray*)data
{
    SEPageImageURL* url = [data objectAtIndex:0];
    CGSize size = [[data objectAtIndex:1] CGSizeValue];
    NSObject<SELoadedImageHandler>* handler = [data objectAtIndex:2];
    SEPageImageURL* passedURL = [url retain];
    [data release];
    ALAssetsLibraryAssetForURLResultBlock getAsset = ^(ALAsset *asset)
    {
        NSThread* currentThread = [NSThread currentThread];
        //NSLog(@"current thread = %@\n", currentThread);
        ALAssetRepresentation* rep = [asset defaultRepresentation];
        CGImageRef image = NULL;
        if(rep)
        {
            image = [rep fullResolutionImage];
        }
        CGSize srcS = CGSizeMake(CGImageGetWidth(image), CGImageGetHeight(image));
        CGSize s = [SEUtil computeFitSize:srcS toDst:size];
        CGImageRef retImage = NULL;
        if(image)
        {
            retImage = [SEUtil fastScale:image withRect:s];
        }
        float width = CGImageGetWidth(retImage);
        float height = CGImageGetHeight(retImage);
        //NSLog(@"## retImage with = %f, height = %f ##\n", width, height);
        UIImageOrientation orient = (UIImageOrientation)[rep orientation];
        CGFloat scale = [rep scale];
        UIImage* uiImage = nil;
        if(retImage)
        {
            uiImage = [UIImage imageWithCGImage:retImage scale:scale orientation:orient] ;
        }
        if(retImage)
        {
            CGImageRelease(retImage);
        }
        [handler setImage:uiImage];
        [passedURL release];
        [self performSelectorOnMainThread:@selector(loadImageHandle:) withObject:handler waitUntilDone:NO];
        
    };
    ALAssetsLibraryAccessFailureBlock failHandler = ^(NSError *error)
    {
        if(error)
        {
            mAssetAcessError = 1;
            NSLog(@"read photo lib error : %@", [error localizedDescription]);
        }
    };
    [mAssetLibrary assetForURL:url.url resultBlock:getAsset failureBlock:failHandler];
}
- (void) loadImageFromPhotoLib: (SEPageImageURL*)url size:(CGSize)size withHandler: (NSObject<SELoadedImageHandler>*) handler
{
    NSMutableArray* data = [NSMutableArray array];
    [data addObject:url];
    [data addObject:[NSValue valueWithCGSize:size]];
    [data addObject:handler];
    data = [data retain];
    [self performSelectorInBackground:@selector(loadImageFromPhotoLibWithThread:) withObject:data];
}
- (void) loadFullRepresentationWithThread:(NSMutableArray*) data
{
    NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
    SEPageImageURL* passedURL = [[data objectAtIndex:0] retain];
    NSObject<SELoadedImageHandler>* handler = [data objectAtIndex:1];
    [data release];
    ALAssetsLibraryAssetForURLResultBlock getAsset = ^(ALAsset *asset)
    {
        NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
        [self isRunOnWorkderThread];
        ALAssetRepresentation* rep = [asset defaultRepresentation];
        CGImageRef image = [rep fullResolutionImage];
        UIImageOrientation orient = (UIImageOrientation)[rep orientation];
        CGFloat scale = [rep scale];
        UIImage* uiImage = [UIImage imageWithCGImage:image scale:scale orientation:orient];
        [handler setImage:uiImage];
        [self performSelectorOnMainThread:@selector(loadImageHandle:) withObject:handler waitUntilDone:NO];
        [passedURL release];
        [pool release];
    };
    ALAssetsLibraryAccessFailureBlock failHandler = ^(NSError *error)
    {
        if(error)
        {
            mAssetAcessError = 1;
            NSLog(@"read photo lib error : %@", [error localizedDescription]);
        }
    };
    [mAssetLibrary assetForURL:passedURL.url resultBlock:getAsset failureBlock:failHandler];    
    [pool release];
}
- (void) loadFullRepresentation: (SEPageImageURL*)url withHandler: (NSObject<SELoadedImageHandler>*)handler
{
    NSMutableArray* data = [NSMutableArray array];
    [data addObject:url];
    [data addObject:handler];
    data = [data retain];
    [self performSelectorInBackground:@selector(loadFullRepresentationWithThread:) withObject:data];
}
@end
////
@implementation SEPageUIScrollView
@synthesize mNotFoundImage;
@synthesize mIsLoadingImage;
@synthesize mScrollViewType;
@synthesize mResLoader;
@synthesize mNotStartThread;
@synthesize mDefaultImage;
@synthesize mName;
@synthesize mPhotoWidth;
@synthesize mPhotoHeight;
@synthesize mHMargin;
@synthesize mVMargin;
@synthesize mLeftPadding;
@synthesize mRightPadding;
@synthesize mTopPadding;
@synthesize mBottomPadding;
@synthesize mViewNavigator;
//@synthesize mPhotoLoaderDelegate;
@synthesize mCanTouchResponse;
@synthesize mHandleMultiTouchDelegate;
@synthesize mUseMultiTouch;
@synthesize mFrameImage;
@synthesize mHighlightedFrameImage;
@synthesize mSelectedIndex;
@synthesize mPageCol;
@synthesize mPageRow;
@synthesize mPageBackground;
@synthesize mStartPage;
@synthesize mEndPage;
@synthesize mCurrentStartPage;
@synthesize mBackgroundImage;
@synthesize mIsSingleColumn;
@synthesize mPageImageCount;
@synthesize mCurrentImageArray;
@synthesize mLastImageViewIndex;
@synthesize mIsCalculateVisibleRange;
@synthesize mIsAnimEnd;
@synthesize mLongPressHandler;
@synthesize mGetImageURLInMainThread;
@synthesize mImageListName;
@synthesize mIsScrolling;
- (void) dealloc
{
    NSLog(@"SEPageScrollView dealloc");
    [iScrollViewDelegate release];
    [self clearState];
    [mName release];
    [mOperationQueue release];
    [mImageListName release];
    [mBackgroundImage release];
    [mNotFoundImage release];
    [super dealloc];
}

- (BOOL) isPoint:(CGPoint)p In : (CGRect) r
{
    return p.x >= r.origin.x && p.y >= r.origin.y && 
    p.x <= (r.origin.x + r.size.width) && p.y <= (r.origin.y + r.size.height);
}
- (SEPageHitProperty) hitRect: (CGPoint)p
{

    if(mStartPage == -1 || mEndPage == -1 || [self getCurrentImageViewCount] == 0)
    {
        SEPageHitProperty hp;
        hp.rect = CGRectMake(0, 0, 0, 0);
        hp.imageView = nil;
        hp.index = SE_INVALID_IMAGE_INDEX;
        return hp;
    }
    for(int pageIndex = mStartPage ; pageIndex <= mEndPage ; pageIndex++)
    {
        UIView* pageView = [mParentView.subviews objectAtIndex:pageIndex];
        for(int i = 0 ; i < pageView.subviews.count ; i++)
        {
            UIView* v = [pageView.subviews objectAtIndex:i];
            CGPoint subp = [self convertPoint:p toView:v];
            if(subp.x >= 0 && subp.y >= 0 && subp.x < v.frame.size.width && subp.y < v.frame.size.height)
            {
                SEPageHitProperty hp;
                hp.rect = v.bounds;
                hp.imageView = (SEPageUIImageView*)v;
                hp.index = pageIndex * mPageRow * mPageCol + i;
                return hp;
            }
        }
    }
    SEPageHitProperty hp;
    hp.rect = CGRectMake(0, 0, 0, 0);
    hp.imageView = nil;
    hp.index = SE_INVALID_IMAGE_INDEX;
    return hp;
}

- (void) initState
{
    mStartPage = SE_INVALID_PAGE;
    mEndPage = SE_INVALID_PAGE;
    mSelectedIndex = SE_INVALID_IMAGE_INDEX;
    mIsCalculateVisibleRange = YES;
    mIsAnimEnd = YES;
    //mMultiTouchDetect = [[SEMultiTouchDetect alloc] init];
    //mMultiTouchDetect.mViewForPoint = self;
    //[mMultiTouchDetect initData];
    iScrollViewDelegate = [[SEPageUIScrollViewDelegate alloc] init];
    self.delegate = iScrollViewDelegate;
    //
    //mEventListCondition = [[NSCondition alloc] init];
    //mEventList = [NSMutableArray array];
    //[mEventList retain];
    [self initPhotoDict];
    [self initAssetLib];
    UIView* v = [[UIView alloc] init];
    //v.backgroundColor = [UIColor redColor];
    [self addSubview:v];
    [v release];
    mParentView = v;
    mParentView.userInteractionEnabled = YES;
    mUseMultiTouch = NO;
    mPressMoveSpacing = 5;
    [mNotFoundImage release];
    mNotFoundImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"NoImageFound"];
    [mNotFoundImage retain];
    if(mOperationQueue == nil)
    {
        mOperationQueue = [[NSOperationQueue alloc] init];
        [mOperationQueue setMaxConcurrentOperationCount:NSOperationQueueDefaultMaxConcurrentOperationCount];
    }
}

- (void) syncImageInCoreData
{
    [mViewNavigator removeSelectedImageNotInImageURLs: mPhotoAssetURL];
}
- (void) createContent
{
    assert(mPageCol != 0);
    assert(mPageRow != 0);
    assert(mPhotoWidth != 0);
    assert(mPhotoHeight != 0);
    int pageImageCount = mPageCol * mPageRow;
    int totalPhotoCount = 0;
    if(mScrollViewType == PHOTOLIB_SCROLLVIEW)
    {
        totalPhotoCount = mPhotoAssetURL.count;
    }
    else
    {
        totalPhotoCount = [mViewNavigator getRemainingImageCount] + [mPhotoAssetURL count];
    }
    mImageViewCount = totalPhotoCount;
    NSLog(@"## totalPhotoCount = %d ##", totalPhotoCount);
    if(totalPhotoCount == 0)
        return;
    /*
    for(int i = 0 ; i < mPhotoAssetURL.count ; i++)
    {
        SEPageImageURL* url = [mPhotoAssetURL objectAtIndex:i];
        NSString* str = url.urlDate;
        NSLog(@"sequence url = %@", str);
    }
     */
    int n = totalPhotoCount / pageImageCount;
    int nReminder = totalPhotoCount % pageImageCount;
    if(mScrollViewType == PHOTOLIB_SCROLLVIEW)
    {
        //[self syncImageInCoreData];
        [mPhotoAssetURL sortUsingComparator:^NSComparisonResult(id obj1, id obj2) {
            SEPageImageURL* url1 = (SEPageImageURL*)obj1;
            SEPageImageURL* url2 = (SEPageImageURL*)obj2;
            return [url2.urlDate compare:url1.urlDate];
        }];
    }
    if(nReminder > 0)
    {
        n++;
    }
    if(n == 0)
        return;
    if(n == 1)
        n = 2;
    if(n % 2 != 0)
        n++;
    mPageCount = n;
    CGFloat contentWidth = self.frame.size.width;
    CGFloat contentHeight = (mPageCount / 2) * mPageBackground.size.height;
    float startx = 0;
    float starty = 0;
    NSUInteger j = 0;
    BOOL overflow = NO;
    float pagestartx;
    float pagestarty;
    CGFloat pageWidth = mPageBackground.size.width;
    CGFloat pageHeight = mPageBackground.size.height;
    assert(pageHeight == 705);
    mPageWidth = pageWidth;
    mPageHeight = pageHeight;
    //test
    //mPageCount = 6;
    //
    for(int pageIndex = 0 ; pageIndex < mPageCount; pageIndex++)
    {
        if(pageIndex % 2 == 0)
        {
            pagestartx = 0;
            pagestarty = ( pageIndex / 2) * pageHeight; 
        }
        else
        {
            pagestartx = pageWidth;
            pagestarty = ( pageIndex / 2 ) * pageHeight; 
        }
        UIImageView* pageView = [[UIImageView alloc] initWithFrame:CGRectMake(pagestartx, pagestarty, pageWidth, pageHeight)];
        pageView.image = mPageBackground;
        pageView.tag = pageIndex;
        [mParentView addSubview:pageView];
        [pageView release];
        startx = mLeftPadding;
        starty = mTopPadding;
        for(int i = 0 ; i < mPageRow && overflow == NO; i++)
        {
            startx = mLeftPadding;
            for(int k = 0 ; k < mPageCol && overflow == NO; k++)
            {
                if(j < totalPhotoCount)
                {
                    CGRect frame = CGRectMake(startx, starty, mPhotoWidth, mPhotoHeight);
                    SEPageUIImageView* imageView = [[SEPageUIImageView alloc] initWithFrame:frame];
                    imageView.mOriginRect = frame;
                    imageView.image = mDefaultImage;
                    imageView.mScrollView = self;
                    imageView.frameImage = mFrameImage;
                    imageView.highlightedFrameImage = mHighlightedFrameImage;
                    imageView.backgroundColor = [UIColor clearColor];
                    [pageView addSubview:imageView];
                    [imageView release];
                    j++;
                    startx += mPhotoWidth + mHMargin;
                }
                else
                {
                    overflow = YES;
                }
            }
            starty += mPhotoHeight + mVMargin;
        }
    }

    self.contentSize = CGSizeMake(contentWidth, contentHeight);
    float currOffset = mCurrentStartPage * pageHeight;
    self.contentOffset = CGPointMake(0, currOffset);
    [self calculateVisiblePage:currOffset];
}
- (void) initPhotoLibUrl
{
    if(mPhotoAssetURL)
        [mPhotoAssetURL release];
    mPhotoAssetURL = [[NSMutableArray array] retain];
    [self createPhotoUrlArray];
    //[mPhotoLoaderDelegate createPhotoUrlArray:mPhotoAssetURL];
}
- (NSArray*) touchToPoints: (NSSet*)touchSet
{
    NSArray* touches = [touchSet allObjects];
    NSArray* pointInScrollView = [NSArray array];
    for(UITouch* touch in touches)
    {
        CGPoint p = [touch locationInView:self];
        NSValue* v = [NSValue valueWithCGPoint:p];
        pointInScrollView = [pointInScrollView arrayByAddingObject:v];
    }
    return pointInScrollView;
}
- (void)longPressHandler: (id)param
{
    NSValue* v = (NSValue*)param;
    if(mPressState == SE_PRESS_READY)
    {
        NSLog(@"target long press");
        mPressState = SE_LONGPRESS_OK;
        CGPoint p = [v CGPointValue];
        [mLongPressHandler longPressBegin:p scrollView:self];
    }
    [v release];
}
- (NSValue*) touchesToValue:(NSSet*)touches
{
    CGPoint p = [[touches anyObject] locationInView:self];
    NSValue* v = [NSValue valueWithCGPoint:p];
    [v retain];
    return v;
}
- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{
    //NSLog(@"scroll view touch begin: %@", mName);
    //NSArray* pointsInScrollView = [self touchToPoints:touches];
    //mHasLongPressed = YES;
    if([self getCurrentImageViewCount] == 0)
        return;
    NSValue* v = [self touchesToValue:touches];
    mPressStartPoint = [[touches anyObject] locationInView:self];
    mPressState = SE_PRESS_READY;
    [self performSelector:@selector(longPressHandler:) withObject:v afterDelay:0.5];
    CGPoint p = [[touches anyObject] locationInView:self];
    [mLongPressHandler pressed:p scrollView:self];
    /*
    if(mUseMultiTouch)
    {
        [mMultiTouchDetect touchStateChange:touches];
    }
    if(mMultiTouchDetect.mTouchState == TOUCH2)
    {
        NSLog(@"### go into multitouch ###\n");
        CGPoint p1 = [[mMultiTouchDetect.points1 objectAtIndex:0] CGPointValue];
        CGPoint p2 = [[mMultiTouchDetect.points2 objectAtIndex:0] CGPointValue];
        SEPageHitProperty hp = [self hitRect:p1];
        SEPageUIImageView* imageView1 = hp.imageView;
        hp = [self hitRect:p2];
        SEPageUIImageView* imageView2 = hp.imageView;
        if(imageView1 == imageView2)
            mMultiTouchBegan = YES;
    }
    if(mMultiTouchBegan)
    {
        [mHandleMultiTouchDelegate touchBegin: mMultiTouchDetect.points1 : mMultiTouchDetect.points2 withPointDelta:CGPointMake(0, 0) withScrollView:self];
    }
    else
        [super touchesBegan:touches withEvent:event];
     */
}

- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event
{
    if([self getCurrentImageViewCount] == 0)
        return;
    CGPoint p = [[touches anyObject] locationInView:self];
    if(mPressState == SE_PRESS_READY)
    {
        CGFloat deltax = p.x - mPressStartPoint.x;
        CGFloat deltay = p.y - mPressStartPoint.y;
        if(fabsf(deltax) > mPressMoveSpacing || fabsf(deltay) > mPressMoveSpacing)
        {
            mPressState = SE_NO_PRESS;
        }
    }
    if(mPressState == SE_LONGPRESS_OK)
    {
        [mLongPressHandler longPressMove:p scrollView:self];
    }
    //NSLog(@"scroll view touch move: %@", mName);
    //NSArray* pointsInScrollView = [self touchToPoints:touches];
    /*
    if(mUseMultiTouch)
    {
        [mMultiTouchDetect touchStateChange:touches];
    }
    if(mMultiTouchBegan)
    {
        CGPoint p = [mMultiTouchDetect getCurrentDeltaPoint];
        [mHandleMultiTouchDelegate touchMove:mMultiTouchDetect.points1 : mMultiTouchDetect.points2 withPointDelta:p withScrollView:self];
    }
    else
        [super touchesMoved:touches withEvent:event];
     */
}
- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event
{
    ///NSLog(@"scroll view touch end: %@", mName);
    if([self getCurrentImageViewCount] == 0)
        return;
    CGPoint p = [[touches anyObject] locationInView:self];
    if(mPressState == SE_LONGPRESS_OK)
    {
        [mLongPressHandler longPressEnd:p scrollView:self];
    }
    else if(mPressState == SE_PRESS_READY)
    {
        [mLongPressHandler clicked:p scrollView:self];
    }
    mPressState = SE_NO_PRESS;
    /*
    if(mUseMultiTouch)
    {
        [mMultiTouchDetect touchStateChange:touches];
    }
    if(mMultiTouchDetect.mTouchState == NO_TOUCH)
    {
        CGPoint p = [mMultiTouchDetect getCurrentDeltaPoint];
        [mHandleMultiTouchDelegate touchEnd:mMultiTouchDetect.points1 : mMultiTouchDetect.points2  withPointDelta:p withScrollView:self];
        mMultiTouchBegan = NO;
    }
    else
        [super touchesEnded:touches withEvent:event];
     */
}
- (void)touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event
{
    if([self getCurrentImageViewCount] == 0)
        return;
    CGPoint p = [[touches anyObject] locationInView:self];
    if(mPressState == SE_LONGPRESS_OK)
    {
        [mLongPressHandler longPressEnd:p scrollView:self];
    }
    mPressState = SE_NO_PRESS;
    /*
    if(mUseMultiTouch)
    {
        [mMultiTouchDetect touchStateChange:touches];
    }
    if(mMultiTouchDetect.mTouchState == NO_TOUCH)
    {
        CGPoint p = [mMultiTouchDetect getCurrentDeltaPoint];
        [mHandleMultiTouchDelegate touchEnd:mMultiTouchDetect.points1 : mMultiTouchDetect.points2 withPointDelta:p withScrollView:self];
        mMultiTouchBegan = NO;
    }
    else
        [super touchesCancelled:touches withEvent:event];
     */
}
- (void) updateImageViewAfterRemove
{
    int imageViewCount = [self getCurrentImageViewCount];
    for(int i = 0 ; i < imageViewCount ; i++)
    {
        SEPageUIImageView* view = [self imageView:i];
        view.image = mDefaultImage;
    }
    NSArray* pages = mParentView.subviews;
    for(int i = mStartPage ; i <= mEndPage ; i++)
    {
        UIView* page = [pages objectAtIndex:i];
        int count = page.subviews.count;
        for(int j = 0 ; j < count ; j++)
        {
            SEPageUIImageView* imageView = [page.subviews objectAtIndex:j];
            int index = i * mPageRow * mPageCol + j;
            assert(index < imageViewCount);
            if(index < imageViewCount)
            {
                SEPageImageURL* url = [self getImageURL:index];
                if(url != nil)
                {
                    SEPagePhotoDictKey* key = [SEPagePhotoDictKey createFromImageURL:url];
                    UIImage* image = [self getImageFromPhotoDict:key];
                    if(image)
                    {
                        imageView.image = image;
                    }
                    else 
                    {
                        imageView.image = mDefaultImage;
                    }
                }
                else
                {
                    imageView.image = mDefaultImage;
                }
            }
        }
    }

}
- (void) removeFromPhotoURLAsset: (NSMutableArray*)indexArray
{
    if(mIsLoadingImage)
        return;
    
    for(NSNumber* num in indexArray)
    {
        int i = [num intValue];
        SEPageUIImageView* iv = [self getImageViewByIndex:i];
        iv.image = mDefaultImage;
    }
     
    NSMutableArray* urlArray = [NSMutableArray array];
    for(int i = 0 ;i < indexArray.count ; i++)
    {
        int index = [[indexArray objectAtIndex:i] intValue];
        SEPageImageURL* purl = [mPhotoAssetURL objectAtIndex:index];
        [urlArray addObject:purl];
    }
    [self removeURLFromPhotoURLAsset: indexArray];
    [self removeImageFromCoreData:urlArray];
    for(SEPageImageURL* url in urlArray)
    {
        SEPagePhotoDictKey* key = [SEPagePhotoDictKey createFromImageURL:url];
        if([self imageURLContentInArray:mPhotoAssetURL :url] == NO)
        {
            [self removeImageFromPhotoDict:key];
        }
    }
    [self updateImageViewAfterRemove];
    //[self loadNextImage : YES];
    [self startLoadImage];
    [self saveCurrentPhotoURL];
    [mViewNavigator printSelectedImage];
}
/*
- (void) removeFromPhotoDict: (int)index removeCondition:(SEPAGE_REMOVE_OP)op
{
    SEPageRemoveFromPhotoDict* event = [[SEPageRemoveFromPhotoDict alloc] initWithIndex:index op:op];
    event.prevStartPage = mStartPage;
    event.prevEndPage = mEndPage;
    event.startPage = mStartPage;
    event.endPage = mEndPage;
    [self addEvent:event];
}
*/
- (SEPageImageURL*) getImageURL: (int)index
{
    if(index < 0 || index >= mPhotoAssetURL.count)
    {
        return nil;
    }
    else
    {
        return [mPhotoAssetURL objectAtIndex:index];
    }
}
- (NSUInteger) getImageURLNum
{
    return [mPhotoAssetURL count];
}

- (BOOL) canAdjust
{
    return YES;
}
- (void) disableAllGestures
{

    NSArray* gesArray = self.gestureRecognizers;
    for(UIGestureRecognizer* ges in gesArray)
    {
        ges.enabled = NO;
    }
}

- (void) enableAllGestures
{
    NSArray* gesArray = self.gestureRecognizers;
    for(UIGestureRecognizer* ges in gesArray)
    {
        ges.enabled = YES;
    }    
}
- (SEPageUIImageView*) imageView:(int)index
{
    int pageIndex = index / (mPageRow * mPageCol);
    int nReminder = index % (mPageRow * mPageCol);
    UIView* pageView = [mParentView.subviews objectAtIndex:pageIndex];
    if(pageView.subviews.count > 0)
        return [pageView.subviews objectAtIndex:nReminder];
    else
        return nil;
}
/*
- (void) updateImage: (int) index
{
    SEPageUpdateImageEvent* event = [[SEPageUpdateImageEvent alloc] init];
    event.index = index;
    event.startPage = mStartPage;
    event.endPage = mEndPage;
    [self addEvent:event];
}
 */
- (void) update
{
    //SEPageUpdateAllEvent* event = [[SEPageUpdateAllEvent alloc] init];
    //[self addEvent:event];
    mIsAnimEnd = YES;
}
- (void) changeContentToMultipleColumn: (CGFloat)currentViewportWidth movePart: (SEPAGE_PART_TYPE) partType
{
    if(mParentView.subviews.count == 0)
        return;
    if(mIsAnimEnd == NO)
        return;
    
    //assert(mStartPage == mEndPage);
    if(mStartPage != mEndPage)
    {
        NSLog(@"scroll view is scrolling");
        //mIsScrolling = YES;
        return;
    }
    if(partType == LEFT_PAGE)
    {
        int currPage = mStartPage;
        UIView* currentPageView = [mParentView.subviews objectAtIndex:currPage];
        UIView* leftPage = nil;
        if(currPage % 2 == 0)
        {
            assert(mStartPage + 1 < mPageCount);
            leftPage = [mParentView.subviews objectAtIndex:mStartPage + 1];
        }
        else
        {
            assert(mStartPage - 1 >= 0);
            leftPage = [mParentView.subviews objectAtIndex:mStartPage - 1];
        }
        float dist = currentViewportWidth - currentPageView.frame.size.width;
        NSLog(@"dist = %f\n", dist);
        if(dist < 0)
            dist = -dist;
        CGPoint p = currentPageView.center;
        leftPage.frame = CGRectMake(currentPageView.frame
                                    .origin.x - dist - mPageWidth, currentPageView.frame.origin.y, mPageWidth, mPageHeight);
        void (^animBlock) (void) = ^{
            leftPage.center = CGPointMake(p.x - mPageWidth, p.y);
        };
        void (^animEnd) (BOOL) = ^(BOOL f)
        {
            NSLog(@"1: single column = NO\n");
            
            //NSLog(@"animEnd thread = %@", [NSThread currentThread] );
            //NSLog(@"main thread = %@", [NSThread mainThread] );
            
            [self createMultiColumnContent: LEFT_PAGE];
            mIsCalculateVisibleRange = NO;
            self.contentSize = CGSizeMake(self.contentSize.width, (mPageCount / 2) * mPageHeight);
            mIsCalculateVisibleRange = YES;
            self.contentOffset = CGPointMake(0, (currPage / 2) * mPageHeight);
            mIsSingleColumn = NO;
            [self calculateVisiblePage:self.contentOffset.y];
            NSLog(@"### %@ : anim end = %d ####", mName,mIsAnimEnd);
            mIsAnimEnd = YES;
        };
        mIsAnimEnd = NO;
        int opts = UIViewAnimationOptionCurveLinear | UIViewAnimationOptionAllowUserInteraction;
        [UIView animateWithDuration:0.2 delay: 0 options: opts animations:animBlock completion:animEnd];
    }
    else if(partType == RIGHT_PAGE)
    {
        int currPage = mStartPage;
        UIView* currentPageView = [mParentView.subviews objectAtIndex:currPage];
        UIView* rightPage = nil;
        if((currPage % 2) != 0)
        {
            rightPage = [mParentView.subviews objectAtIndex:currPage - 1];
        }
        else
        {
            rightPage = [mParentView.subviews objectAtIndex:currPage + 1];
        }
        float dist = currentViewportWidth - currentPageView.frame.size.width;
        NSLog(@"dist = %f\n", dist);
        if(dist < 0)
            dist = -dist;
        CGPoint p = currentPageView.center;
        rightPage.frame = CGRectMake(currentPageView.frame
                                    .origin.x + dist + mPageWidth, currentPageView.frame.origin.y, mPageWidth, mPageHeight);
        void (^animBlock) (void) = ^{
            rightPage.center = CGPointMake(p.x + mPageWidth, p.y);
        };
        void (^animEnd) (BOOL) = ^(BOOL f)
        {
            NSLog(@"2: single column = NO");
            
            [self createMultiColumnContent: RIGHT_PAGE];
            mIsCalculateVisibleRange = NO;
            self.contentSize = CGSizeMake(self.contentSize.width, (mPageCount / 2) * mPageHeight);
            mIsCalculateVisibleRange = YES;
            self.contentOffset = CGPointMake(0, (currPage / 2) * mPageHeight);
            mIsSingleColumn = NO;
            [self calculateVisiblePage:self.contentOffset.y];
            NSLog(@"### %@ : anim end = %d ####", mName , mIsAnimEnd);
            mIsAnimEnd = YES;
            
        };
        mIsAnimEnd = NO;
        int opts = UIViewAnimationOptionCurveLinear | UIViewAnimationOptionAllowUserInteraction;
        [UIView animateWithDuration:0.2 delay: 0 options: opts animations:animBlock completion:animEnd];
    }
}
- (void) changeContentToSingleColumn: (CGFloat) currentViewportWidth movePart: (SEPAGE_PART_TYPE)partType
{
    if(mParentView.subviews.count == 0)
        return;
    if(mIsAnimEnd == NO)
        return;
    //mIsAnimEnd = NO;
    //assert(mStartPage != mEndPage);
    if(mStartPage == mEndPage)
        return;
    float dist = currentViewportWidth - mPageWidth;
    if(dist < 0)
        dist = -dist;
    if(partType == LEFT_PAGE)
    {
        UIView* leftPage = nil;
        UIView* startPage = [mParentView.subviews objectAtIndex:mStartPage];
        UIView* endPage = [mParentView.subviews objectAtIndex:mEndPage];
        BOOL reserverEndPage = YES;
        if(startPage.frame.origin.x > endPage.frame.origin.x)
        {
            leftPage = [mParentView.subviews objectAtIndex:mEndPage];
            reserverEndPage = NO;
        }
        else
            leftPage = [mParentView.subviews objectAtIndex:mStartPage];

        CGPoint p = leftPage.center;
        void (^animBlock) (void) = ^{
            leftPage.center = CGPointMake(p.x - dist, p.y);
        };
        void (^animEnd) (BOOL) = ^(BOOL f)
        {
            NSLog(@"3: single column YES");
            mIsCalculateVisibleRange = NO;
            self.contentSize = CGSizeMake(self.contentSize.width, mPageCount * mPageHeight);
            mIsCalculateVisibleRange = YES;
            [self createSingleColumnContent: RIGHT_PAGE];
            if(reserverEndPage)
                self.contentOffset = CGPointMake(0, mEndPage * mPageHeight);
            else
                self.contentOffset = CGPointMake(0, mStartPage * mPageHeight);
            mIsSingleColumn = YES;
            [self calculateVisiblePage:self.contentOffset.y];
            NSLog(@"### %@ : anim end = %d ####", mName, mIsAnimEnd);
            mIsAnimEnd = YES;
        };
        mIsAnimEnd = NO;
        [UIView animateWithDuration:0.2 delay: 0 options: UIViewAnimationOptionCurveLinear animations:animBlock completion:animEnd];
    }    
    else if(partType == RIGHT_PAGE)
    {
        UIView* rightPage = nil;
        UIView* startPage = [mParentView.subviews objectAtIndex:mStartPage];
        UIView* endPage = [mParentView.subviews objectAtIndex:mEndPage];
        BOOL reserveStartPage = YES;
        if(startPage.frame.origin.x > endPage.frame.origin.x)
        {
            rightPage = startPage;
            reserveStartPage = NO;
        }
        else
            rightPage = [mParentView.subviews objectAtIndex:mEndPage];
        CGPoint p = rightPage.center;
        void (^animBlock) (void) = ^{
            rightPage.center = CGPointMake(p.x + dist, p.y);
        };
        void (^animEnd) (BOOL) = ^(BOOL f)
        {
            NSLog(@"4: single column YES");
            mIsCalculateVisibleRange = NO;
            self.contentSize = CGSizeMake(self.contentSize.width, mPageCount * mPageHeight);
            mIsCalculateVisibleRange = YES;
            [self createSingleColumnContent: LEFT_PAGE];
            if(reserveStartPage)
                self.contentOffset = CGPointMake(0, mStartPage * mPageHeight);
            else
                self.contentOffset = CGPointMake(0, mEndPage * mPageHeight);
            mIsSingleColumn = YES;
            [self calculateVisiblePage:self.contentOffset.y];
            NSLog(@"### %@ : anim end = %d ####", mName,  mIsAnimEnd);
            mIsAnimEnd = YES;
        };
        mIsAnimEnd = NO;
        [UIView animateWithDuration:0.2 delay: 0 options: UIViewAnimationOptionCurveLinear animations:animBlock completion:animEnd];
    }
}
- (void) insertURLToImageView: (int)index : (int) lastURLIndex : (int) newLastURLIndex
{
    assert(index <= lastURLIndex);
    if(index == lastURLIndex)
    {
        int page = lastURLIndex / (mPageRow * mPageCol);
        if(page != mStartPage && page != mEndPage)
        {
            CGRect rectVisible;
            if(mIsSingleColumn)
            {
                rectVisible = CGRectMake(0, page * mPageHeight, self.frame.size.width, mPageHeight);
            }
            else
            {
                rectVisible = CGRectMake(0, (page / 2) * mPageHeight, self.frame.size.width, mPageHeight);
            } 
            mLastImageViewIndex = lastURLIndex;
            [self scrollRectToVisible:rectVisible animated:YES];
        }
        else
        {
            mLastImageViewIndex = lastURLIndex;
            [self doScaleAnimationForInsert];
        }
        
    }
    else 
    {
        int imageCount = mCurrentImageArray.count;
        int endIndex = index + imageCount;
        int changeIndex = newLastURLIndex - 1;
        for(int i = lastURLIndex - 1 ; i > index ; i--)
        {
            SEPageUIImageView* firstView = [self imageView:i];
            SEPageUIImageView* secondView = [self imageView:changeIndex];
            secondView.image = firstView.image;
            changeIndex--;
        }
        SEPageUIImageView* endView = [self imageView:endIndex];
        assert(endIndex == changeIndex);
        SEPageUIImageView* currentView = [self imageView:index];
        endView.image = currentView.image;
        for(int i = 0 ; i < imageCount ; i++)
        {
            SEPageUIImageView* v = [self imageView:index + i];
            v.image = [mCurrentImageArray objectAtIndex:i];
        }
        
        mLastImageViewIndex = index;
        [self doScaleAnimationForInsert];
    }
}
- (void) insertImageToPhotoDict: (NSMutableArray*)imageArray urlArray:(NSMutableArray *)urlArray
{
    assert(imageArray.count == urlArray.count);
    for(int i = 0 ; i < imageArray.count ;i++)
    {
        SEPageImageURL* url = [urlArray objectAtIndex:i];
        UIImage* image = [imageArray objectAtIndex:i];
        SEPagePhotoDictKey* key = [SEPagePhotoDictKey createFromImageURL:url];
        [self addImageToPhotoDict:key image:image];
    }
}
//index is the imageview index
- (void) insertURLToPhotoAsset:(int)index url:(NSMutableArray *)urlIDArray image:(NSMutableArray *)imageArray
{
    if(mIsLoadingImage)
        return;
    int imageCount = [mViewNavigator getCurrentAllSelectedImageCount];
    int insertCount = urlIDArray.count;
    BOOL canInsert = YES;
    int maxSelectedCount = [mViewNavigator getCurrentLevelImageNum];//[mViewNavigator getMaxSelectedImageCount];
    if((imageCount + insertCount) <= maxSelectedCount)
        canInsert = YES;
    else
    {
        if(imageCount >= maxSelectedCount)
        {
            canInsert = NO;
        }
        else
        {
            canInsert = YES;
            int count = maxSelectedCount - imageCount;
            assert(count < insertCount);
            NSMutableArray* newURLIDArray = [NSMutableArray array];
            NSMutableArray* newImageArray = [NSMutableArray array];
            for(int i = 0 ; i < count ; i++)
            {
                [newURLIDArray addObject:[urlIDArray objectAtIndex:i]];
                [newImageArray addObject:[imageArray objectAtIndex:i]];
            }
            urlIDArray = newURLIDArray;
            imageArray = newImageArray;
        }
    }
    if(canInsert == NO)
        return;
    int lastURLIndex = [self lastURLIndex];
    assert(lastURLIndex <= mPhotoAssetURL.count);
    //if(lastURLIndex >= mPhotoAssetURL.count)
    //    return;
    if(index > lastURLIndex)
        index = lastURLIndex;
    mCurrentImageArray = [imageArray copy];
    assert(index <= mPhotoAssetURL.count && canInsert == YES);
    [self insertURLToPhotoAssetArray:urlIDArray :index :lastURLIndex];
    NSMutableArray* urlArray = [NSMutableArray array];
    for(SEPageURLID* urlID in urlIDArray)
    {
        SEPageImageURL* url = [[SEPageImageURL alloc] init];
        url.url = urlID.url;
        url.urlDate = urlID.urlDate;
        url.type = SEPAGE_PHOTO_LIB_URL;
        url.origWidth = urlID.origWidth;
        url.origHeight = urlID.origHeight;
        url.orientation = urlID.orientation;
        NSLog(@"insertURL url width = %f, height = %f, orient = %d", url.origWidth, url.origHeight, url.orientation);
        NSLog(@"insertURL url date = %@", url.urlDate);
        [urlArray addObject:url];
        [url release];
    }
    [self insertImageToPhotoDict: imageArray urlArray:urlArray];
    [self insertURLToImageView:index :lastURLIndex :(lastURLIndex + urlArray.count)];
    [self saveCurrentPhotoURL];
    //[self saveCurrentPhotoThumbnail: urlArray];
    [mViewNavigator printSelectedImage];
    
}

- (BOOL) imageViewInArray: (UIView*) image :(NSMutableArray*)array
{
    for(int i = 0 ;i < array.count ; i++)
    {
        if([array objectAtIndex:i] == image)
            return YES;
    }
    return NO;
}

- (void) changeImageView: (NSMutableArray*) imageViewIndexArray toPos : (int)dstImageViewIndex
{
    if(mIsLoadingImage)
        return;
    
    int lastImageViewIndex = [self lastImageViewIndex];
    if(lastImageViewIndex == -1)
    {
        lastImageViewIndex = mPhotoAssetURL.count;
    }
    if(dstImageViewIndex > lastImageViewIndex)
        dstImageViewIndex = lastImageViewIndex;

    NSMutableArray* indexArray = imageViewIndexArray;
    int dstIndex = dstImageViewIndex;
    [self moveURLs:indexArray toIndex:dstIndex];
    [self saveCurrentPhotoURL];
    
    NSMutableArray* allImage = [NSMutableArray array];
    int pageIndex = 0;
    for(UIView* page in mParentView.subviews)
    {
        for(SEPageUIImageView* v in page.subviews)
        {
            if(pageIndex < mPhotoAssetURL.count)
            {
                [allImage addObject:v];
                pageIndex++;
            }
        }
    }
    assert(allImage.count == mPhotoAssetURL.count);
    NSMutableArray* needChangeImageView = [NSMutableArray array];
    for(int i = 0 ;i < imageViewIndexArray.count ; i++)
    {
        int index = [[imageViewIndexArray objectAtIndex:i] intValue];
        SEPageUIImageView* imageView = [self imageView:index];
        [needChangeImageView addObject:imageView];
    }
    NSMutableArray* newImageArray = [NSMutableArray array];
    for(int i = 0 ; i < dstImageViewIndex ; i++)
    {
        UIView* imageView = [allImage objectAtIndex:i];
        if([self imageViewInArray:imageView :needChangeImageView] == NO)
        {
            [newImageArray addObject:imageView];
        }
    }
    for(int i = 0 ; i < needChangeImageView.count ; i++)
    {
        [newImageArray addObject:[needChangeImageView objectAtIndex:i]];
    }
    if(dstImageViewIndex < lastImageViewIndex)
    {
        int startIndex = dstImageViewIndex;
        UIView* v = [self imageView:dstImageViewIndex];
        if([self imageViewInArray:v :needChangeImageView])
        {
            startIndex = dstImageViewIndex + 1;
        }
        for(int i = startIndex ; i < lastImageViewIndex ; i++)
        {
            UIView* imageView = [allImage objectAtIndex:i];
            if([self imageViewInArray:imageView :needChangeImageView] == NO)
            {
                [newImageArray addObject:imageView];
            }
        }
    }
    NSLog(@"newimagearraycount = %d, allImage count = %d", newImageArray.count, allImage.count);
    assert(newImageArray.count == allImage.count);
    NSMutableArray* imageArray  = [ NSMutableArray array];
    for(int i = 0 ; i < newImageArray.count ; i++)
    {
        SEPageUIImageView* view = [newImageArray objectAtIndex:i];
        [imageArray addObject:view.image];
    }
    assert(lastImageViewIndex == mPhotoAssetURL.count);
    for(int i = 0 ; i < lastImageViewIndex ; i++)
    {
        SEPageUIImageView* imageView = [self imageView:i];
        imageView.image = [imageArray objectAtIndex:i];
    }
    [mViewNavigator printSelectedImage];
}

- (void) drawRect:(CGRect)rect
{
    if(mBackgroundImage)
        [mBackgroundImage drawInRect:rect];
}
- (NSArray*) getHighlightedImageView
{
    NSArray* ret = [NSArray array];
    NSArray* pageViews = mParentView.subviews;
    for(UIView* page in pageViews)
    {
        for(UIView* v in page.subviews)
        {
            SEPageUIImageView* imageView = (SEPageUIImageView*)v;
            if(imageView.highlighted)
            {
                ret = [ret arrayByAddingObject:imageView];
            }
        }
    }
    return ret;
}
- (int) getIndexForImageView:(SEPageUIImageView*)imageView
{
    for(int page = 0 ; page < mParentView.subviews.count ; page++)
    {
        UIView* pageView = [mParentView.subviews objectAtIndex:page];
        for(int i = 0 ; i < pageView.subviews.count ; i++)
        {
            UIView* v = [pageView.subviews objectAtIndex:i];
            if(v == imageView)
            {
                return page * mPageRow * mPageCol + i;
            }
        }
    }
    return -1;
}


- (void)relayout
{}

- (void) startLoadImage
{
    NSLog(@"start load image");
    mStopLoadImage = NO;
    if(mIsLoadingImage == NO)
        [self loadNextImageNew];
}
- (void) stopLoadImage
{
    NSLog(@"stop load image");
    //[mOperationQueue cancelAllOperations];
    mStopLoadImage = YES;
}
- (BOOL) isStopLoadImage
{
    return mStopLoadImage;
}
- (void) updateImageViewByScroll
{
    for(int i = 0 ; i < mPhotoAssetURL.count ; i++)
    {
        SEPageImageURL* url = [mPhotoAssetURL objectAtIndex:i];
        [self removeImageFromPhotoDictByURL: url];
        SEPageUIImageView* view = [self imageView:i];
        view.image = mDefaultImage;
    }
    CGPoint currentOffset = self.contentOffset;
    CGPoint newOffset = CGPointMake(currentOffset.x, currentOffset.y + 1);
    self.contentOffset = newOffset;
    self.contentOffset = currentOffset;
    [self startLoadImage];
}
- (void) printPhotoDict
{
    int i = 0;
    for(SEPagePhotoDictItem* item in mPhotoDict)
    {
        UIImage* image = item.image;
        NSLog(@"## photo dict image %d : %@##", i, image);
        i++;
    }
}
@end
//////
