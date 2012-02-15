//
//  SEUIScrollView.mm
//  PhotoFrame
//
//  Created by 陈勇 on 12-1-20.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEUIScrollView.h"
#import <AssetsLibrary/AssetsLibrary.h>
#import "SEViewNavigator.h"
#import "SEUtil.h"
#import "SelectedImage.h"
#import "UserInfo.h"
////////////////
/////
#define SEEXIT_EVENT 0
#define SEVISIBLERANGE_EVENT 1
#define SELOADPHOTO_EVENT 2
#define SERESTART_EVENT 3
////////////

/////
static CGImageRef createCGImageCopy(CGImageRef srcImage)
{
    CGColorSpaceRef srcColorRef = CGImageGetColorSpace(srcImage);
    return CGImageCreateCopyWithColorSpace(srcImage, srcColorRef);
}
/////////

@implementation SEImageURL
@synthesize url;
@synthesize type;
@synthesize filepath;
- (void) dealloc
{
    [url release];
    [super dealloc];
}

@end
//////
@implementation SEUIImageView

//@synthesize mHighlightedImage;
@synthesize mImageView;
- (void) dealloc
{
    //[mHighlightedImage release];
    [super dealloc];
}
- (UIImage*) image
{
    return mImageView.image;
}
- (void) setImage:(UIImage *)image
{
    mImageView.image = image;
}
- (id) initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if(self)
    {
        CGRect bounds = self.bounds;
        mImageView = [[UIImageView alloc] initWithFrame:bounds];
        mImageView.contentMode = UIViewContentModeCenter;
        mImageView.clipsToBounds = YES;
        mImageView.backgroundColor = [UIColor blueColor];
        [self addSubview:mImageView];
        [mImageView release];
    }
    return self;
}
@end
/////
@interface ImageData : NSObject
{
    CGImageRef imageRef;
    int index;
}
@property (nonatomic, assign) CGImageRef imageRef;
@property (nonatomic, assign) int index;
@end;
@implementation ImageData
@synthesize imageRef;
@synthesize index;

@end
/////
enum REMOVE_OP {LESS, GREAT};
@interface GarbageInfo : NSObject
{
    int index;
    REMOVE_OP op;
}
@property (nonatomic, assign) int index;
@property (nonatomic, assign) REMOVE_OP op;
@end
@implementation GarbageInfo
@synthesize index;
@synthesize op;

@end
/////////
@implementation SEUIPhotoLibLoaderDelegete 
@synthesize mScrollView;
- (void) initState
{
    mAssetIdentity[0] = ALAssetsGroupAlbum;
    mAssetIdentity[1] = ALAssetsGroupSavedPhotos;
    mAssetLibrary = [[ALAssetsLibrary alloc] init];
}
- (void) createPhotoUrlArray: (NSMutableArray*)photoURLArray
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
        SEImageURL* imageURL = [[SEImageURL alloc] init];
        imageURL.url = assetURL;
        imageURL.type = PHOTO_LIB_URL;
        [photoURLArray addObject:imageURL];
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
                [mScrollView performSelectorOnMainThread:@selector(createContent) withObject:nil waitUntilDone:NO];
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
- (CGImageRef) getPhotoFromURL:(SEImageURL *)url
{
    return [SEUtil getImageFromPhotoLib:url.url withAssetLib:mAssetLibrary];
    /*
    __block CGImageRef retImage  = NULL;
    __block int accessError = 0;
    ALAssetsLibraryAssetForURLResultBlock getAsset = ^(ALAsset *asset)
    {
        CGImageRef image = [asset thumbnail];
        retImage = createCGImageCopy(image);
    };
    ALAssetsLibraryAccessFailureBlock failHandler = ^(NSError *error)
    {
        if(error)
        {
            accessError = 1;
            NSLog(@"read photo lib error : %@", [error localizedDescription]);
        }
    };
    [mAssetLibrary assetForURL:url.url resultBlock:getAsset failureBlock:failHandler];
    if(!accessError)
        return retImage;
    else
        return NULL;
     */
}
- (void) dealloc
{
    [mAssetLibrary release];
    [super dealloc];
}
@end
/////////////////////
@implementation SEUIPhotoFileLoaderDelegate
@synthesize mScrollView;
@synthesize mNavView;
- (void) initState
{
    mAssetLibrary = [[ALAssetsLibrary alloc] init];
}
- (void)createPhotoUrlArray: (NSMutableArray*)photoURLArray
{
    /*
    NSArray* array = [SEUtil getFilePathWithType:@"jpg", @"JPG", @"png", @"PNG", nil];
    for(NSUInteger i = 0 ; i < [array count] ; i++)
    {
        NSString* str = [array objectAtIndex:i];
        NSURL* url = [NSURL fileURLWithPath:str];
        [photoURLArray addObject:url];
    }
     */
    NSMutableArray* si = [mNavView getUserImageProperty];
    for(NSUInteger i = 0 ; i < [si count] ; i++)
    {
        NSManagedObject* mo = [si objectAtIndex:i];
        NSNumber* seq = [mo valueForKey:@"seq"];
        NSLog(@"seq = %@", seq);
        NSString* str = [mo valueForKey:@"filepath"];
        NSString* url = [mo valueForKey:@"url"];
        NSURL* fileURL = [NSURL URLWithString:str];
        SEImageURL* imageURL = [[SEImageURL alloc] init];
        imageURL.url = [NSURL URLWithString: url];
        imageURL.filepath = fileURL;
        imageURL.type = FILE_PATH;
        [photoURLArray addObject:imageURL];
        [imageURL release];
        //debug
        if(str != nil || url != nil)
        {
            NSLog(@"str = %@, url = %@", str, url);
        }
        //end
    }
}
- (CGImageRef) getPhotoFromURL:(SEImageURL *)url
{
    NSURL* filePath = url.filepath;
    if(filePath != nil)
    {
        NSString* str = [filePath lastPathComponent];
        UIImage* uiImage = [UIImage imageNamed:str];
        uiImage = [SEUtil cropUIImage:uiImage withRect:CGSizeMake(mScrollView.mPhotoWidth, mScrollView.mPhotoHeight)];
        CGImageRef image = [uiImage CGImage];
        CGImageRetain(image);
        return image;
    }
    else
    {
        NSURL* photoURL = url.url;
        if(photoURL)
        {
            return [SEUtil getImageFromPhotoLib:photoURL withAssetLib:mAssetLibrary];
        }
        else
        {
            return NULL;
        }
    }
}
- (void)dealloc
{
    [mAssetLibrary release];
    [super dealloc];
}
@end
/////////////////////
@implementation SEUIScrollViewDelegate
-(void) scrollViewDidScroll:(UIScrollView *)scrollView
{
    //NSLog(@"content offset = %f", scrollView.contentOffset.y);
    SEUIScrollView* currentView = (SEUIScrollView*)scrollView;
    [currentView calculateVisibleView:scrollView.contentOffset.y];
}

@end
//////////////////////
////
@interface SEUIScrollViewEvent : NSObject 
{
    int type;
}
@property (nonatomic, assign) int type;
@end
@implementation SEUIScrollViewEvent
@synthesize type;
@end
@interface SEVisibaleRangeEvent : SEUIScrollViewEvent 
{
    int startRow;
    int endRow;
}
@property (nonatomic, assign) int startRow;
@property (nonatomic, assign) int endRow;
- (id) initWithRange: (int)start :(int) end;
@end
@implementation SEVisibaleRangeEvent
@synthesize startRow;
@synthesize endRow;
- (id) initWithRange: (int)start :(int) end
{
    self = [super init];
    if(self)
    {
        startRow = start;
        endRow = end;
        type = SEVISIBLERANGE_EVENT;
    }
    return self;
}

@end
@interface SELoadPhotoEvent : SEUIScrollViewEvent {
    int row;
    int col;
}
@property (nonatomic, assign) int row;
@property (nonatomic, assign) int col;
- (id) initWithRow : (int) trow Column:(int) tcol;
@end
@implementation SELoadPhotoEvent
@synthesize row;
@synthesize col;
- (id) initWithRow : (int) trow Column:(int) tcol
{
    self = [super init];
    if(self)
    {
        self.row = trow;
        self.col = tcol;
        type = SELOADPHOTO_EVENT;
    }
    return self;
}
@end
@interface SEExitEvent : SEUIScrollViewEvent {
}
@end
@implementation SEExitEvent
- (id)init
{
    self = [super init];
    if(self)
    {
        type = SEEXIT_EVENT;
    }
    return self;
}

@end
@interface SERestartEvent : SEUIScrollViewEvent {

}
@end
@implementation SERestartEvent
- (id) init
{
    self = [super init];
    if(self)
    {
        type = SERESTART_EVENT;
    }
    return self;
}

@end
////////
@interface SEUIScrollView (Private)
- (void) restartReal : (id) param;
- (void) removeAllImageViews;

- (SEUIImageView*) getImageViewByIndex: (int)index;
- (void) garbageClean;
- (size_t) getImageSizeSumInDict;
- (CGImageRef) getPhotoDictContent: (id)obj;
- (int) getPhotoDictKey: (id)key;
- (void) clearState;
- (CGImageRef) getPhotoFromPhotoLib : (SEImageURL*)photoURL;
//return the bytes size of an image
- (int) getImageSize: (CGImageRef)imageRef;
// return the row count in UIScrollView viewport
- (int) getVisibleRowCount; 
- (void) startupPhotoLoadThread;
//
- (void) photoThreadLoadFunc;
- (void) addEvent: (SEUIScrollViewEvent*) event;
- (void) getPrevPageRows: (int)start : (int) end :(int*)outStart : (int*)outEnd;
- (void) getNextPageRows: (int)start : (int)end :(int*)outStart : (int*) outEnd;
@end
@implementation SEUIScrollView (Private)
- (void) removeAllImageViews
{
    NSArray* subviews = [mParentView subviews];
    for(NSUInteger i = 0  ; i < [subviews count] ; i++)
    {
        UIView* v = [subviews objectAtIndex:i];
        [v removeFromSuperview];
    }
}

- (void) restartReal: (id) param
{
    [self removeAllImageViews];
    int startIndex = mStartRow * mColumn;
    self.mStartIndex = startIndex;
    [self createContent];
}

- (int) getPhotoDictKey: (id)key
{
    NSNumber* indexNum = (NSNumber*)key;
    return [indexNum intValue];
}
- (CGImageRef) getPhotoDictContent: (id)obj
{
    NSValue* v = (NSValue*)obj;
    CGImageRef image = (CGImageRef)[v pointerValue];
    return image;
}
- (size_t) getImageSizeSumInDict
{
    __block size_t sum = 0;
    [mPhotoDict enumerateKeysAndObjectsUsingBlock:^(id key, id obj, BOOL *stop) {
        CGImageRef imageRef = [self getPhotoDictContent:obj];
        size_t h = CGImageGetHeight(imageRef);
        size_t bytesPerRow = CGImageGetBytesPerRow(imageRef);
        sum += h * bytesPerRow;
    }];
    return sum;
}
- (void) removeFromPhotoDictUsingBlock: (BOOL (^)(int photoIndex)) block
{
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
- (void) removeImageFromView : (GarbageInfo*) gbInfo
{
    int index = gbInfo.index;
    switch (gbInfo.op)
    {
        case LESS:
        {
            for(int i = 0 ; i < index ; i++)
            {
                SEUIImageView* iv = [self getImageViewByIndex:i];
                iv.image = mDefaultImage;
                //iv.mHighlightedImage = nil;
            }
        }
            break;
        case GREAT:
        {
            for(int i = index + 1 ; i < [mPhotoAssetURL count] ; i++)
            {
                SEUIImageView* iv = [self getImageViewByIndex:i];
                iv.image = mDefaultImage;
                //iv.mHighlightedImage = nil;
            }
        }
            break;
    }
    [gbInfo release];
}
- (void) garbageClean
{
    size_t sum = [self getImageSizeSumInDict];
    int startRow[2] = {mPrevStartRow, mStartRow};
    int endRow[2] = {mNextEndRow, mEndRow};
    int i = 0;
    //NSLog(@"all image size = %lu", sum);
    while((sum > PHOTODICT_SIZE) && i < 2)
    {
        if(startRow[i] != -1)
        {
            int startIndex = startRow[i] * mColumn;
            [self removeFromPhotoDictUsingBlock:^BOOL(int photoIndex) {
                return photoIndex < startIndex;
            }];
            GarbageInfo* gbInfo = [[GarbageInfo alloc] init];
            gbInfo.index = startIndex;
            gbInfo.op = LESS;
            [self performSelectorOnMainThread:@selector(removeImageFromView:) withObject:gbInfo waitUntilDone:NO];
        }
        if(endRow[i] != -1)
        {
            int endIndex = endRow[i] * mColumn + mColumn;
            [self removeFromPhotoDictUsingBlock:^BOOL(int photoIndex) {
                return photoIndex >= endIndex;
            }];
            GarbageInfo* gbInfo = [[GarbageInfo alloc] init];
            gbInfo.index = endIndex;
            gbInfo.op = GREAT;
            [self performSelectorOnMainThread:@selector(removeImageFromView:) withObject:gbInfo waitUntilDone:NO];
        }
        sum = [self getImageSizeSumInDict];
        i++;
    }
}
- (void) clearState
{
    [mPhotoDict enumerateKeysAndObjectsUsingBlock:^(id key, id obj, BOOL *stop) {
        CGImageRef image = [self getPhotoDictContent:obj];
        CGImageRelease(image);
    }];
}
- (void) getPrevPageRows: (int)start : (int) end :(int*)outStart : (int*)outEnd
{
    int rowCount = [self getVisibleRowCount];
    int prevEnd = start - 1;
    int prevStart = start - rowCount;
    if(prevStart < 0)
        prevStart = -1;
    *outStart = prevStart;
    *outEnd = prevEnd;
}
- (void) getNextPageRows: (int)start : (int)end :(int*)outStart : (int*) outEnd
{
    int rowCount = [self getVisibleRowCount];
    int nextStart = end + 1;
    int nextEnd = end + rowCount;
    if(nextEnd > mRow)
        nextEnd = mRow;
    *outStart = nextStart;
    *outEnd = nextEnd;
}
-(void) adjustPotentialShowRow : (int)start : (int)end
{
    /*
    if(mStartRow > start)
        mDirection = UP;
    else if(mStartRow < start)
        mDirection = DOWN;
    else
        mDirection = STATIC;
     */
    mStartRow = start;
    mEndRow = end;
    [self getPrevPageRows:start :end :&mPrevStartRow :&mPrevEndRow];
    [self getNextPageRows:start :end :&mNextStartRow :&mNextEndRow];
}
- (BOOL) isIntervalContain: (int) srcStart : (int)srcEnd : (int) dstStart : (int) dstEnd
{
    if(dstStart >= srcStart && dstStart <= srcEnd && dstEnd >= srcStart && dstEnd <= srcEnd)
        return YES;
    else
        return NO;
}
- (NSArray*) adjustPhotoLoad : (NSArray*)photoLoadArray
{
    NSArray* newPhotoLoadArray = [NSArray array];
    for(NSUInteger i = 0 ; i < [photoLoadArray count] ; i += 2)
    {
        NSNumber* row = [photoLoadArray objectAtIndex:i];
        NSNumber* col = [photoLoadArray objectAtIndex:(i + 1)];
        int rowV = [row intValue];
        if(rowV <= mEndRow && rowV >= mStartRow)
        {
            newPhotoLoadArray = [newPhotoLoadArray arrayByAddingObject:row];
            newPhotoLoadArray = [newPhotoLoadArray arrayByAddingObject:col];
        }
    }
    if([newPhotoLoadArray count] == 0)
    {
        NSNumber* row = [NSNumber numberWithInt:mStartRow];
        NSNumber* col = [NSNumber numberWithInt:0];
        newPhotoLoadArray = [newPhotoLoadArray arrayByAddingObject:row];
        newPhotoLoadArray = [newPhotoLoadArray arrayByAddingObject:col];
    }
    return newPhotoLoadArray;
}
- (CGImageRef) getPhoto : (int)index
{
    NSNumber* numIndex = [NSNumber numberWithInt:index];
    NSValue* v = [mPhotoDict objectForKey:numIndex];
    return (CGImageRef)[v pointerValue];
}

- (void) addPhoto : (int) index : (CGImageRef)imageRef
{
    NSNumber* numIndex = [NSNumber numberWithInt:index];
    NSValue* v = [NSValue valueWithPointer:imageRef];
    [mPhotoDict setObject:v forKey:numIndex];
}
/*
- (void) setImageView : (NSArray*)rowcol
{
    NSNumber* rowNum = [rowcol objectAtIndex:0];
    NSNumber* colNum = [rowcol objectAtIndex:1];
    int row = [rowNum intValue];
    int col = [colNum intValue];
    NSArray* subviews = [mParentView subviews];
    UIImageView* view = (UIImageView*)[subviews objectAtIndex:(row * mColumn + col)];
    int index = row * mColumn + col;
    NSNumber* indexKey = [NSNumber numberWithInt:index];
    NSValue* v = [mPhotoDict objectForKey:indexKey];
    CGImageRef image = (CGImageRef)[v pointerValue];
    UIImage* uiImage = [UIImage imageWithCGImage:image];
    if(uiImage)
    {
        view.image = uiImage;
    }
    [rowcol release];
}
*/
- (SEUIImageView*) getImageViewByIndex : (int)index
{
    NSArray* subviews = [mParentView subviews];
    SEUIImageView* view = (SEUIImageView*)[subviews objectAtIndex: index];
    return view;
}
- (void) setImageToView : (ImageData*) imageData
{
    SEUIImageView* view = [self getImageViewByIndex:imageData.index];
    UIImage* uiImage = [UIImage imageWithCGImage:imageData.imageRef];
    if(uiImage)
    {
        view.image = uiImage;
    }
    [imageData release];
}
- (void) loadPhoto: (NSArray*) photoLoadArray
{
    BOOL stopLoad = NO;
    NSLog(@"loadPhoto mStartRow = %d, mEndRow = %d", mStartRow, mEndRow);
    for(int i = mStartRow ; i <= mEndRow && (!stopLoad); i++)
    {
        for(int j = 0 ; j < mColumn && (!stopLoad); j++)
        {
            int index = i * mColumn + j;
            if(index >= [mPhotoAssetURL count])
            {
                stopLoad = YES;
                break;
            }
            CGImageRef image = [self getPhoto: index];
            if(image == NULL)
            {
                //NSURL* url = [mPhotoAssetURL objectAtIndex:index];
                SEImageURL* url = [mPhotoAssetURL objectAtIndex:index];
                image = [self getPhotoFromPhotoLib:url];
                //size_t w = CGImageGetWidth(image);
                //size_t h = CGImageGetHeight(image);
                //NSLog(@"photo w = %lu, h = %lu ", w, h);
                if(image != NULL)
                {
                    [self addPhoto:index :image];
                    ImageData* imgData = [[ImageData alloc] init];
                    imgData.imageRef = image;
                    imgData.index = index;
                    SELoadPhotoEvent* event = [[SELoadPhotoEvent alloc] init];
                    event.type = SELOADPHOTO_EVENT;
                    [self addEvent:event];
                    stopLoad = YES;
                    [self performSelectorOnMainThread:@selector(setImageToView:) withObject:imgData waitUntilDone:NO];
                }
            }
            else
            {
                ImageData* imgData = [[ImageData alloc] init];
                imgData.imageRef = image;
                imgData.index = index;
                [self performSelectorOnMainThread:@selector(setImageToView:) withObject:imgData waitUntilDone:NO];
            }
        }
    }
    /*
    int nextRow = -1, nextCol = -1;
    BOOL toEnd = NO;
    for(NSUInteger i = 0 ; i < [photoLoadArray count] ; i += 2)
    {
        NSNumber* row = [photoLoadArray objectAtIndex:i];
        NSNumber* col = [photoLoadArray objectAtIndex:(i + 1)];
        
        int rowV = [row intValue];
        int colV = [col intValue];
        if(colV != -1)
        {
            [self setPhoto:rowV :colV];
            nextCol = colV + 1;
            nextRow = rowV;
            if(nextCol >= mColumn)
            {
                nextCol = 0;
                nextRow = rowV + 1;
                
            }
        }
        else
        {
            for(int j = 0 ; j < mColumn ; j++)
            {
                [self setPhoto:rowV :j];
            }
        }

    }
    if(row >= 0 && row < mRow)
    {
        SELoadPhotoEvent* event = [[SELoadPhotoEvent alloc] initWithRow:row Column:-1];
        [self addEvent:event];
    }
     */
}
- (void) photoThreadLoadFunc
{
    NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
    BOOL exit = NO;
    BOOL restartOccor = NO;
    while(!exit)
    {
        [mEventListCondition lock];
        NSUInteger count = [mEventList count];
        //NSLog(@"count = %d", count);
        while(count == 0)
        {
            //NSLog(@"before wait");
            [mEventListCondition wait];
            count = [mEventList count];
            //NSLog(@"count = %d", count);
            //NSLog(@"end wait");
        }
        //SEUIScrollViewEvent* lastRangeEvent = nil;
        int startRow = -1, endRow = -1;
        BOOL exitEventOccur = NO;
        
        NSArray* loadPhotoRows = [NSArray array];
        for(NSUInteger i = 0 ; i < count ; i++)
        {
            SEUIScrollViewEvent* event = [mEventList objectAtIndex:i];
            switch(event.type)
            {
                case SEVISIBLERANGE_EVENT:
                {
                    startRow = ((SEVisibaleRangeEvent*)event).startRow;
                    endRow = ((SEVisibaleRangeEvent*)event).endRow;
                }
                    break;
                case SEEXIT_EVENT:
                {
                    exitEventOccur = YES;    
                }
                    break;
                case SERESTART_EVENT:
                {
                    exitEventOccur = YES;
                    restartOccor = YES;
                    
                }
                    break;
                case SELOADPHOTO_EVENT:
                {
                    NSNumber* num = [NSNumber numberWithInt:((SELoadPhotoEvent*)event).row];
                    loadPhotoRows = [loadPhotoRows arrayByAddingObject:num];
                    num = [NSNumber numberWithInt:((SELoadPhotoEvent*)event).col];
                    loadPhotoRows = [loadPhotoRows arrayByAddingObject:num];
                }
                    break;
                default:
                    NSLog(@"error event type!");
                    break;
            }
            [event release];
        }
        if(exitEventOccur)
        {
            exit = YES;
            [mEventList removeAllObjects];
            [mEventListCondition unlock];
            continue;
        }
        [mEventList removeAllObjects];
        [mEventListCondition unlock];
        if(startRow != -1 && endRow != -1)
        {
            if(mStartRow == -1 && mEndRow == -1)
            {
                assert([loadPhotoRows count] == 0);
                mStartRow = startRow;
                mEndRow = endRow;
                SELoadPhotoEvent* loadEvent = [[SELoadPhotoEvent alloc] initWithRow:mStartRow Column:0];
                [self addEvent:loadEvent];
                [self getPrevPageRows:startRow :endRow :&mPrevStartRow :&mPrevEndRow];
                [self getNextPageRows:startRow :endRow :&mNextStartRow :&mNextEndRow];
            }
            else
            {
                [self adjustPotentialShowRow:startRow :endRow];
                loadPhotoRows = [self adjustPhotoLoad:loadPhotoRows];
            }
        }
        [self loadPhoto: loadPhotoRows];
        [self garbageClean];
    }
    if(restartOccor)
    {
        [self performSelectorOnMainThread:@selector(restartReal:) withObject:nil waitUntilDone:NO];
    }
    NSLog(@"photo load thread exit!");
    [pool release];
}
- (void) addEvent: (SEUIScrollViewEvent*) event
{
    [mEventListCondition lock];
    [mEventList addObject:event];
    [mEventListCondition signal];
    [mEventListCondition unlock];
}
- (void) startupPhotoLoadThread
{
    [NSThread detachNewThreadSelector:@selector(photoThreadLoadFunc) toTarget:self withObject:nil];
}
- (int) getVisibleRowCount
{
    int cellHeight = mPhotoHeight + mVMargin;
    int n = mViewHeight / cellHeight;
    int nReminder = mViewHeight % cellHeight;
    if(nReminder > 0)
        n++;
    return n;
}
- (int) getImageSize:(CGImageRef)imageRef
{
    size_t bytesPerRow = CGImageGetBytesPerRow(imageRef);
    size_t h = CGImageGetHeight(imageRef);
    return h * bytesPerRow;
}

- (CGImageRef) getPhotoFromPhotoLib : (SEImageURL*)photoURL
{
    if(mPhotoLoaderDelegate)
        return [mPhotoLoaderDelegate getPhotoFromURL:photoURL];
    else
        return NULL;
}

@end
////
@implementation SEUIScrollView
@synthesize mNotStartThread;
@synthesize mDefaultImage;
@synthesize mName;
@synthesize mPhotoWidth;
@synthesize mPhotoHeight;
@synthesize mContentWidth;
@synthesize mContentHeight;
@synthesize mHMargin;
@synthesize mVMargin;
@synthesize mLeftPadding;
@synthesize mRightPadding;
@synthesize mTopPadding;
@synthesize mBottomPadding;
@synthesize mColumn;
@synthesize mRow;
@synthesize mViewWidth;
@synthesize mViewHeight;
@synthesize mViewNavigator;
@synthesize mPhotoLoaderDelegate;
@synthesize mStartIndex;
@synthesize mCanTouchResponse;
@synthesize mStartRow;
@synthesize mEndRow;
- (void) dealloc
{
    [iScrollViewDelegate release];
    [mPhotoLoaderDelegate release];
    [self clearState];
    [mName release];
    [super dealloc];
}

- (BOOL) isPoint:(CGPoint)p In : (CGRect) r
{
    return p.x >= r.origin.x && p.y >= r.origin.y && 
    p.x <= (r.origin.x + r.size.width) && p.y <= (r.origin.y + r.size.height);
}
- (HitProperty) hitRect: (CGPoint)p
{
    int startIndex = mStartRow * mColumn;
    int endIndex = mEndRow * mColumn + mColumn - 1;
    NSArray* viewArray = [mParentView subviews];
    CGPoint contentPoint = p;
    //contentPoint.x = p.x + self.contentOffset.x;
    //contentPoint.y = p.y + self.contentOffset.y;
    for(NSInteger i = 0 ; i < [viewArray count] ; i++)
    {
        if(i <= endIndex && i >= startIndex)
        {
            UIView* view = [viewArray objectAtIndex:i];
            CGRect r = view.frame;
            if([self isPoint:contentPoint In:r])
            {
                HitProperty hp;
                hp.rect = CGRectMake(r.origin.x - self.contentOffset.x, r.origin.y - self.contentOffset.y, r.size.width, r.size.height);
                hp.imageView = (SEUIImageView*)view;
                hp.index = i;
                return hp;
            }
        }
    }
    HitProperty hp;
    hp.rect = CGRectMake(0, 0, 0, 0);
    hp.imageView = nil;
    hp.index = -1;
    return hp;
}
- (void) initState
{
    mPrevStartRow = -1;
    mPrevEndRow = -1;
    mStartRow = -1;
    mEndRow = -1;
    mNextStartRow = -1;
    mNextEndRow = -1;
    //mDirection = STATIC;
    iScrollViewDelegate = [[SEUIScrollViewDelegate alloc] init];
    self.delegate = iScrollViewDelegate;
    //
    mEventListCondition = [[NSCondition alloc] init];
    mEventList = [NSMutableArray array];
    [mEventList retain];
    mPhotoDict = [[NSMutableDictionary alloc] init];
    [mPhotoLoaderDelegate initState];
    UIView* v = [[UIView alloc] init];
    [self addSubview:v];
    [v release];
    mParentView = v;

}

- (void) calculateVisibleView: (CGFloat)contentOffset;
{
    CGFloat starty = -contentOffset;
    int n = 0 ;
    int startRow = 0;
    int endRow = 0;
    starty += mTopPadding;
    starty += mPhotoHeight;
    while(starty <= 0)
    {
        starty += mVMargin + mPhotoHeight;
        n++;
    }
    startRow = n;// n is the startRow in UIScrollView
    n = 0; //reset n to 0
    starty = -contentOffset;
    starty += mTopPadding;
    while(starty < mViewHeight)
    {
        starty += mPhotoHeight + mVMargin;
        n++;
    }
    // n is the row which is below the bottom of view;
    endRow = n - 1; // endRow maybe -1 , so there has no view can be seen
    if(endRow == -1)
        startRow = -1;
    
    SEVisibaleRangeEvent* event = [[SEVisibaleRangeEvent alloc] initWithRange:startRow :endRow];
    [self addEvent:event];
}

- (void) relayout
{
    SERestartEvent* e = [[SERestartEvent alloc] init];
    [self addEvent:e];
}
- (void) createContent
{
    assert(mViewWidth != 0 );
    assert(mViewHeight != 0);
    assert(mPhotoWidth != 0);
    assert(mPhotoHeight != 0);
    int realWidth = mViewWidth - mLeftPadding - mRightPadding;
    int n = (realWidth + mHMargin) / (mPhotoWidth + mHMargin);
    int nReminder = (realWidth + mHMargin) % (mPhotoWidth + mHMargin);
    
    if(nReminder > 0)
    {
        n++;
    }
    mContentWidth = mLeftPadding + mRightPadding + n * mPhotoWidth + (n - 1) * mHMargin;
    mColumn = n;
    int totalPhotoCount = [mPhotoAssetURL count];
    n = totalPhotoCount / mColumn;
    nReminder = totalPhotoCount % mColumn;
    if(nReminder > 0)
    {
        n++;
    }
    mRow = n;
    mContentHeight = mTopPadding + mBottomPadding + n * mPhotoHeight + (n - 1) * mVMargin;
    int startx = mLeftPadding;
    int starty = mTopPadding;
    NSUInteger j = 0;
    BOOL overflow = NO;
    for(int i = 0 ; i < mRow && (!overflow) ; i++)
    {
        startx = mLeftPadding;
        for(int k = 0 ; k < mColumn && (!overflow); k++)
        {
            if(j < totalPhotoCount)
            {
                CGRect frame = CGRectMake(startx, starty, mPhotoWidth, mPhotoHeight);
                SEUIImageView* imageView = [[SEUIImageView alloc] initWithFrame:frame];
                /*
                imageView.contentMode = UIViewContentModeCenter;
                imageView.clipsToBounds = YES;
                imageView.backgroundColor = [UIColor blueColor];
                 */
                imageView.image = mDefaultImage;
                [mParentView addSubview:imageView];
                [imageView release];
                startx += mPhotoWidth + mHMargin;
                j++;
            }
            else
            {
                overflow = YES;
            }
        }
        starty += mPhotoHeight + mVMargin;
    }
    self.contentSize = CGSizeMake(mContentWidth, mContentHeight);
    int currentRow = mStartIndex / mColumn;
    float currOffset = currentRow * (mPhotoHeight + mVMargin);
    self.contentOffset = CGPointMake(0, currOffset);
    if(mNotStartThread == NO)
    {
        [self startupPhotoLoadThread];
    }
    [self calculateVisibleView:currOffset];
}
- (void) initPhotoLibUrl
{
    if(mPhotoAssetURL)
        [mPhotoAssetURL release];
    mPhotoAssetURL = [NSMutableArray array];
    [mPhotoAssetURL retain];
    [mPhotoLoaderDelegate createPhotoUrlArray:mPhotoAssetURL];
}
- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"scroll view touch begin: %@", mName);
    [super touchesBegan:touches withEvent:event];
}
- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"scroll view touch move: %@", mName);
    if([mViewNavigator isFloatViewShow])
    {
        CGPoint p = [[touches anyObject] locationInView:self];
        NSLog(@"touch in scrollview : p = %f, %f", p.x, p.y);
        [mViewNavigator moveFloatViewToPoint:p];
    }
    [super touchesMoved:touches withEvent:event];
}
- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"scroll view touch end: %@", mName);
    [super touchesEnded:touches withEvent:event];
}
- (void)touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"scroll view touch cancel: %@", mName);
    [super touchesCancelled:touches withEvent:event];
}
- (float) calculateContentOffset : (int) startRow
{
    return startRow * (mPhotoHeight + mVMargin);
}
- (SEImageURL*) getImageURL: (int)index
{
    return [mPhotoAssetURL objectAtIndex:index];
}
- (void) removeFromPhotoDict: (int)index
{
    NSNumber* num = [NSNumber numberWithInt:index];
    CGImageRef image = [self getPhotoDictContent:[mPhotoDict objectForKey:num]];
    CGImageRelease(image);
    [mPhotoDict removeObjectForKey:num];
}
- (NSUInteger) getImageURLNum
{
    return [mPhotoAssetURL count];
}
- (BOOL) canAdjust
{
    return YES;
}
@end
//////
