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
}
@property (nonatomic, assign) int startPage;
@property (nonatomic, assign) int endPage;
@property (nonatomic, assign) int page;
@property (nonatomic, assign) int row;
@property (nonatomic, assign) int col;
@end
@implementation PageImageLoadInfo
@synthesize page;
@synthesize row;
@synthesize col;
@synthesize startPage;
@synthesize endPage;
@end
//////////

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
- (void) setHighlighted:(BOOL)h
{
    if(highlighted != h && mContentImage != mScrollView.mDefaultImage)
    {
        highlighted = h;
        [self setNeedsDisplay];
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
- (void) scrollViewDidEndDecelerating:(UIScrollView *)scrollView
{
    NSLog(@"end decelerate\n");
    SEPageUIScrollView* currentView = (SEPageUIScrollView*)scrollView;
    [currentView garbageClean];
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
- (void) initAssetLib
{
    mAssetIdentity[0] = ALAssetsGroupAlbum;
    mAssetIdentity[1] = ALAssetsGroupSavedPhotos;
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
        imageURL.urlDate = [SEUtil dateToString: date];
        //NSLog(@"url date = %@", imageURL.urlDate);
        imageURL.type = SEPAGE_PHOTO_LIB_URL;
        //assert([self isInPhotoAssetArray:imageURL] == NO);
        if([self isInPhotoAssetArray:imageURL] == NO)
            [mPhotoAssetURL addObject:imageURL];
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
            if(mCurrentAssetIndex == ASSET_NUM)
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
        [mAssetLibrary enumerateGroupsWithTypes:mAssetIdentity[i] usingBlock:getGroups failureBlock:oops];
    }
    NSLog(@"photo enumerate end");
}
- (void)createPhotoUrlArrayFromCoreData
{
    NSArray* siSet = [mViewNavigator getUserImageProperty];
    for(NSUInteger i = 0 ; i < [siSet count] ; i++)
    {
        SelectedImage*  si = [siSet objectAtIndex:i];
        NSNumber* seq = si.seq;
        NSString* str = si.filepath;
        NSString* url = si.url;
        NSURL* fileURL = [NSURL URLWithString:str];
        SEPageImageURL* imageURL = [[SEPageImageURL alloc] init];
        imageURL.url = [NSURL URLWithString: url];
        imageURL.urlDate = si.urldate;
        imageURL.filepath = fileURL;
        imageURL.type = (SEPAGE_URL_TYPE)[si.urltype intValue];
        imageURL.orientation = [si.orientation intValue];
        [mPhotoAssetURL addObject:imageURL];
        [imageURL release];
        //debug
        if(str != nil || url != nil)
        {
            NSLog(@"seq = %@, str = %@, url = %@", seq, str, url);
        }
        //end
    }
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
    for(int i = mStartPage ; i <= mEndPage ; i++)
    {
        UIView* page = [pages objectAtIndex:i];
        int count = page.subviews.count;
        for(int j = 0 ; j < count ; j++)
        {
            SEPageUIImageView* imageView = [page.subviews objectAtIndex:j];
            int index = i * mPageRow * mPageCol + j;
            if(index < mPhotoAssetURL.count)
            {
                SEPageImageURL* url = [mPhotoAssetURL objectAtIndex:index];
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
        CGSize srcSize = CGSizeMake(image.size.width, image.size.height);
        CGSize dstSize = CGSizeMake(mPhotoWidth - 20, mPhotoHeight - 20);
        dstSize = [SEUtil computeFitSize:srcSize toDst:dstSize];
        CGImageRef retImage = [SEUtil fastScale:imageRef withRect:dstSize];
        UIImage* retUIImage = [UIImage imageWithCGImage:retImage scale:1.0 orientation: (UIImageOrientation)url.orientation];
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
/*
- (void) restartAll: (id)param
{
    [self removeAllImageViews];
    [self initPhotoDict];
    [self initPhotoLibUrl];
    [self createContent];
}
- (void) restartReal: (id) param
{
    [self removeAllImageViews];

    [self createContent];
}
*/
/*
- (int) getPhotoDictKey: (id)key
{
    assert([self isRunOnWorkderThread]);
    NSNumber* indexNum = (NSNumber*)key;
    return [indexNum intValue];
}
- (CGImageRef) getPhotoDictContent: (id)obj
{
    assert([self isRunOnWorkderThread]);
    NSValue* v = (NSValue*)obj;
    CGImageRef image = (CGImageRef)[v pointerValue];
    return image;
}
 */
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
/*
- (void) removeFromPhotoDictUsingBlock: (BOOL (^)(int photoIndex)) block
{
    assert([self isRunOnWorkderThread]);
    NSMutableArray* indexArray = [NSMutableArray array];
    //get the key need be deleted
    [mPhotoDict enumerateKeysAndObjectsUsingBlock:^(id key, id obj, BOOL *stop) {
        int photoIndex = [self getPhotoDictKey:key];
        if(block(photoIndex))
            [indexArray addObject:key];
    }];
    for(NSUInteger i = 0 ; i < [indexArray count] ; i++)
    {
        id key = [indexArray objectAtIndex:i];
        CGImageRef image = [self getPhotoDictContent:[mPhotoDict objectForKey:key]];
        CGImageRelease(image);
    }
    [mPhotoDict removeObjectsForKeys:indexArray];
}
 */
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
                SEPageImageURL* url = [mPhotoAssetURL objectAtIndex:i];
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
        for(int i = firstIndex ; i < mPhotoAssetURL.count; i++)
        {
            if(i < assetURLCount)
            {
                SEPageImageURL* url = [mPhotoAssetURL objectAtIndex:i];
                [self removeImageFromPhotoDictByURL:url];
            }
        }
        PageGarbageInfo* gbInfo = [[PageGarbageInfo alloc] init];
        gbInfo.index = firstIndex;
        gbInfo.op = INDEX_GREAT;
        [self removeImageFromView:gbInfo];
        /*
        [self performSelectorOnMainThread:@selector(removeImageFromView:) withObject:gbInfo waitUntilDone:NO];
         */
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
        if([self imageURLInArray:needChangeURLs :imageURL] == NO )
        {
            [newPhotoURL addObject:imageURL];
        }
    }
    for(int i = 0 ; i < needChangeURLs.count ; i++)
    {
        [newPhotoURL addObject:[needChangeURLs objectAtIndex:i]];
    }
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
    for(int i = lastURLIndex ; i < mPhotoAssetURL.count ; i++)
    {
        SEPageImageURL* imageURL = [[SEPageImageURL alloc] init];
        [newPhotoURL addObject:imageURL];
        [imageURL release];
    }
    assert(newPhotoURL.count == mPhotoAssetURL.count);
    [mPhotoAssetURL release];
    mPhotoAssetURL = [newPhotoURL retain];
}
- (void) insertURLToPhotoAssetArray: (NSMutableArray*)urlArray : (int) index : (int) lastURLIndex
{
    assert([self isRunOnMainThread]);
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
    int num = mPhotoAssetURL.count - newPhotoURL.count;
    for(int i = 0 ; i < num;  i++)
    {
        SEPageImageURL* imageURL = [[SEPageImageURL alloc] init];
        [newPhotoURL addObject:imageURL];
        [imageURL release];
    }
    assert(newPhotoURL.count == mPhotoAssetURL.count);
    [mPhotoAssetURL release];
    mPhotoAssetURL = [newPhotoURL retain];
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
    for(SEPageImageURL* url in urlArray)
    {
        [mViewNavigator removeImageFromCoreDate:[url.url absoluteString] urlDate:url.urlDate];
    }
}
- (void) removeURLFromPhotoURLAsset: (NSMutableArray*)indexArray
{
    assert([self isRunOnMainThread]);
    NSMutableArray* newArray = [NSMutableArray array];
    for(int i = 0 ; i < mPhotoAssetURL.count; i++)
    {
        if([self containIndex:indexArray index:i] == NO)
        {
            [newArray addObject:[mPhotoAssetURL objectAtIndex:i]];
        }
    }
    int diffCount = mPhotoAssetURL.count - newArray.count;
    for(int i = 0  ; i < diffCount ; i++)
    {
        SEPageImageURL* imageURL = [[SEPageImageURL alloc] init];
        [newArray addObject:imageURL];
        [imageURL release];
    }
    [mPhotoAssetURL release];
    mPhotoAssetURL = [newArray retain];
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
            [self loadNextImage : YES];
        }
        /*
        if([self isIntervalContain:prevStartPage :prevEndPage :mStartPage :mEndPage] || 
           [self isIntervalContain:mStartPage :mEndPage :prevStartPage :prevEndPage])
        {
            NSLog(@"## page intersect : not load ##");
        }
        else 
        {
            [self loadNextImage : YES];
        }
         */
        //NSLog(@"page index = %d, %d\n", startPageIndex, endPageIndex);
        //SEPageVisibaleRangeEvent* event = [[SEPageVisibaleRangeEvent alloc] initWithRange:prevStartPage : prevEndPage : startPageIndex :endPageIndex];
        //NSDate* date1 = [NSDate date];
        //[self addEvent:event];
        //NSDate* date2 = [NSDate date];
        //NSTimeInterval timeV = [date2 timeIntervalSinceDate:date1];
        //NSLog(@"### add event time = %f ###\n", timeV);
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
    return -1;
}
/*
- (void) getImageFromPhotoLib:(SEPageImageURL *)url
{}
 */
@end
////
@implementation SEPageUIScrollView
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
- (void) dealloc
{
    [iScrollViewDelegate release];
    [self clearState];
    [mName release];
    [mOperationQueue release];
    [super dealloc];
}

- (BOOL) isPoint:(CGPoint)p In : (CGRect) r
{
    return p.x >= r.origin.x && p.y >= r.origin.y && 
    p.x <= (r.origin.x + r.size.width) && p.y <= (r.origin.y + r.size.height);
}
- (SEPageHitProperty) hitRect: (CGPoint)p
{
    /*
    //test operation
    NSOperation* operation = [[SEPageLoadImageOperation alloc] init];
    [mOperationQueue addOperation:operation];
    [operation release];
    //end
     */
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
    mMultiTouchDetect = [[SEMultiTouchDetect alloc] init];
    mMultiTouchDetect.mViewForPoint = self;
    [mMultiTouchDetect initData];
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
    mUseMultiTouch = YES;
    mPressMoveSpacing = 5;
    if(mOperationQueue == nil)
    {
        mOperationQueue = [[NSOperationQueue alloc] init];
        [mOperationQueue setMaxConcurrentOperationCount:1];
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
    int totalPhotoCount = [mPhotoAssetURL count];
    NSLog(@"## totalPhotoCount = %d ##", totalPhotoCount);
    int n = totalPhotoCount / pageImageCount;
    int nReminder = totalPhotoCount % pageImageCount;
    if(mScrollViewType == PHOTOLIB_SCROLLVIEW)
    {
        [self syncImageInCoreData];
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
    mPageWidth = pageWidth;
    mPageHeight = pageHeight;
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
    mPhotoAssetURL = [NSMutableArray array];
    [mPhotoAssetURL retain];
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
    if(mPressState == SE_PRESS_READY)
    {
        NSLog(@"target long press");
        mPressState = SE_LONGPRESS_OK;
        NSValue* v = (NSValue*)param;
        CGPoint p = [v CGPointValue];
        [mLongPressHandler longPressBegin:p scrollView:self];
        [v release];
    }
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

- (void) removeFromPhotoURLAsset: (NSMutableArray*)indexArray
{
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
    [self removeImageFromCoreData:urlArray];
    [self removeURLFromPhotoURLAsset: indexArray];
    for(SEPageImageURL* url in urlArray)
    {
        SEPagePhotoDictKey* key = [SEPagePhotoDictKey createFromImageURL:url];
        [self removeImageFromPhotoDict:key];
    }
    [self updateImageView];
    [self loadNextImage : YES];
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
    return [mPhotoAssetURL objectAtIndex:index];
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
        return;
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
            mIsCalculateVisibleRange = NO;
            self.contentSize = CGSizeMake(self.contentSize.width, (mPageCount / 2) * mPageHeight);
            mIsCalculateVisibleRange = YES;
            [self createMultiColumnContent: LEFT_PAGE];
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
        if(currPage == mPageCount - 1)
            rightPage = [mParentView.subviews objectAtIndex:currPage - 1];
        else
            rightPage = [mParentView.subviews objectAtIndex:currPage + 1];
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
            mIsCalculateVisibleRange = NO;
            self.contentSize = CGSizeMake(self.contentSize.width, (mPageCount / 2) * mPageHeight);
            mIsCalculateVisibleRange = YES;
            [self createMultiColumnContent: RIGHT_PAGE];
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
    //NSMutableArray* dataArray = (NSMutableArray*)data;
    //int index = [[dataArray objectAtIndex:0] intValue];
    //int lastURLIndex = [[dataArray objectAtIndex:1] intValue];
    //int newLastURLIndex = [[dataArray objectAtIndex:2] intValue];
    //[dataArray release];
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
        /*
        for(int i = index ; i < newLastURLIndex ; i++)
        {
            [self updateImage:i];
        }
        */
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
    int imageCount = [mViewNavigator getCurrentAllSelectedImageCount];
    int insertCount = urlIDArray.count;
    BOOL canInsert = YES;
    if((imageCount + insertCount) <= [mViewNavigator getMaxSelectedImageCount])
        canInsert = YES;
    else
        canInsert = NO;
    if(canInsert == NO)
        return;
    int lastURLIndex = [self lastURLIndex];
    if(lastURLIndex >= mPhotoAssetURL.count)
        return;
    if(index > lastURLIndex)
        index = lastURLIndex;
    mCurrentImageArray = [imageArray copy];
    assert(index < mPhotoAssetURL.count && canInsert == YES);
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
        [urlArray addObject:url];
        [url release];
    }
    [self insertImageToPhotoDict: imageArray urlArray:urlArray];
    [self insertURLToImageView:index :lastURLIndex :(lastURLIndex + urlArray.count)];
    [self saveCurrentPhotoURL];
    [self saveCurrentPhotoThumbnail: urlArray];
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
/*
- (UIImage*) getImageFromAfterDstImageView: (NSMutableArray*)needChangeImageView : (NSMutableArray*) allImage : (int) startIndex: (int) index
{
    int n = 0;
    for(int i = startIndex ; i < allImage.count ; i++)
    {
        if([self imageInArray:[allImage objectAtIndex:i]:needChangeImageView] == NO)
        {
            if(n == index)
                return [allImage objectAtIndex:i];
            else if(n < index)
                n++;
        }
    }
    return nil;
}
 */
- (void) changeImageView: (NSMutableArray*) imageViewIndexArray toPos : (int)dstImageViewIndex
{
    int lastImageViewIndex = [self lastImageViewIndex];
    if(lastImageViewIndex == -1)
        return;
    if(dstImageViewIndex > lastImageViewIndex)
        dstImageViewIndex = lastImageViewIndex;

    NSMutableArray* indexArray = imageViewIndexArray;
    int dstIndex = dstImageViewIndex;
    [self moveURLs:indexArray toIndex:dstIndex];
    [self saveCurrentPhotoURL];
    
    NSMutableArray* allImage = [NSMutableArray array];
    for(UIView* page in mParentView.subviews)
    {
        for(SEPageUIImageView* v in page.subviews)
        {
            if(v.image != mDefaultImage)
            {
                [allImage addObject:v];
            }
        }
    }
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
        CGImageRef image = [rep fullResolutionImage];
        CGSize srcS = CGSizeMake(CGImageGetWidth(image), CGImageGetHeight(image));
        CGSize s = [SEUtil computeFitSize:srcS toDst:size];
        CGImageRef retImage = [SEUtil fastScale:image withRect:s];
        float width = CGImageGetWidth(retImage);
        float height = CGImageGetHeight(retImage);
        //NSLog(@"## retImage with = %f, height = %f ##\n", width, height);
        UIImageOrientation orient = (UIImageOrientation)[rep orientation];
        CGFloat scale = [rep scale];
        UIImage* uiImage = [UIImage imageWithCGImage:retImage scale:scale orientation:orient] ;
        CGImageRelease(retImage);
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

- (void)relayout
{}
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
- (void) stopLoadImage
{
    mStopLoadImage = YES;
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
