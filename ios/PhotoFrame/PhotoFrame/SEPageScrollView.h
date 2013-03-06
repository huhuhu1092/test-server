//
//  SEPageScrollView.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-3-14.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>

#import "SEProtocalDefine.h"
enum SEPAGE_SCROLL_DIR {PAGEUP, PAGEDOWN, PAGESTATIC};
enum {SEPAGE_PHOTODICT_SIZE = 2 * 1024 * 1024};
enum {SEPAGE_ASSET_NUM = 2};
enum SEPAGE_URL_TYPE {SEPAGE_FILE_PATH, SEPAGE_PHOTO_LIB_URL};
enum SEPAGE_PART_TYPE {LEFT_PAGE, RIGHT_PAGE};
enum SEPAGE_REMOVE_OP {INDEX_LESS, INDEX_GREAT, INDEX_EQUAL};
enum SEPAGE_SCROLLVIEW_TYPE {PHOTOLIB_SCROLLVIEW, COREDATA_SCROLLVIEW, INVALID_SCROLLVIEW};
#define SE_INVALID_IMAGE_INDEX -1
@class SEPageUIImageView;
@class SEViewNavigator;
@class SEResLoader;
@class SEMultiTouchDetect;
@class SEPageUIScrollView;
struct SEPageHitProperty
{
    CGRect rect; // the frame in SEPageUIImageView coordinate
    SEPageUIImageView* imageView;
    int index;
};
@interface SEPageURLID : NSObject {
@private
    NSURL* url;
    NSString* urlDate;
    float origWidth;
    float origHeight;
    int orientation;
}
@property (nonatomic, assign) int orientation;
@property (nonatomic, assign) float origWidth;
@property (nonatomic, assign) float origHeight;
@property (nonatomic, retain) NSURL* url;
@property (nonatomic, retain) NSString* urlDate;
+ (SEPageURLID*)create: (NSURL*)tu : (NSString*)date : (float)width :(float) height : (int)orientation;
@end
///
@class SEPageImageURL;
@interface SEPagePhotoDictKey : NSObject
{
    NSString* url;
    NSString* date;
    SEPAGE_URL_TYPE type;
}
@property (nonatomic, retain) NSString* url;
@property (nonatomic, retain) NSString* date;
@property (nonatomic, assign) SEPAGE_URL_TYPE type;
- (BOOL) isEqualToKey: (SEPagePhotoDictKey*)key;
+ (SEPagePhotoDictKey*) createFromImageURL : (SEPageImageURL*)url;
@end
//////
@interface SEPagePhotoDictItem : NSObject
{
    SEPagePhotoDictKey* key;
    UIImage* image;
}
@property (nonatomic, retain) SEPagePhotoDictKey* key;
@property (nonatomic, retain) UIImage* image;
@end
/////
@interface SEPageImageURL : NSObject 
{
    NSURL* url;
    NSString* urlDate;
    SEPAGE_URL_TYPE type;
    NSURL* filepath;
    float origWidth;
    float origHeight;
    int orientation;
}
@property (nonatomic, assign) int orientation;
@property (nonatomic, assign) float origWidth;
@property (nonatomic, assign) float origHeight;
@property (nonatomic, retain) NSString* urlDate;
@property (nonatomic, retain) NSURL* url;
@property (nonatomic, assign) SEPAGE_URL_TYPE type;
@property (nonatomic, retain) NSURL* filepath;
- (SEPageImageURL*) clone;
@end
///////////////////////
@interface SEPageUIImageView : UIView 
{
@private
    UIImage* mContentImage;
    UIImage* mFrameImage;
    UIImage* mHighlightedFrameImage;
    CGSize mContentSize; // this is content size which will contain mContentImage
    CGFloat alpha;
    BOOL highlighted;
    SEPageUIScrollView* mScrollView;
    BOOL mIsAnimation;
    CGRect mOriginRect;
}
@property (nonatomic, assign) CGRect mOriginRect;
@property (nonatomic, assign) BOOL mIsAnimation;
@property (nonatomic, assign) SEPageUIScrollView* mScrollView;
@property (nonatomic, retain) UIImage* image;
@property (nonatomic, retain) UIImage* frameImage;
@property (nonatomic, retain) UIImage* highlightedFrameImage;
@property (nonatomic, assign) CGSize contentSize;
@property (nonatomic, assign) CGFloat alpha;
@property (nonatomic, assign) BOOL highlighted;
- (BOOL) isDefaultImage;
- (SEPageUIImageView*) copy;
@end
///////////////////////
@class ALAssetsLibrary;
@interface SEPageUIScrollViewDelegate  : NSObject <UIScrollViewDelegate>
- (void) scrollViewDidScroll:(UIScrollView *)scrollView;
@end
/////////
@protocol SEPageHandleMultiTouchDelegate <NSObject>

- (void) touchBegin: (NSArray*)points1 : (NSArray*)points2 withPointDelta: (CGPoint)deltap withScrollView: (SEPageUIScrollView*)scrollView;
- (void) touchMove: (NSArray*)points : (NSArray*)points2 withPointDelta: (CGPoint)deltap withScrollView: (SEPageUIScrollView*)scrollView;
- (void) touchEnd: (NSArray*)points : (NSArray*)points2 withPointDelta: (CGPoint)deltap withScrollView: (SEPageUIScrollView*)scrollView;

@end

@protocol SEPageUIScrollViewPressHandler <NSObject>

- (void) longPressBegin: (CGPoint)p scrollView:(SEPageUIScrollView*)scrollView;
- (void) longPressMove: (CGPoint)p scrollView:(SEPageUIScrollView*)scrollView;
- (void) longPressEnd: (CGPoint)p scrollView:(SEPageUIScrollView*)scrollView;
- (void) clicked: (CGPoint) p scrollView: (SEPageUIScrollView*)scrollView;
- (void) pressed: (CGPoint) p scrollView: (SEPageUIScrollView*)scrollView;
@end

//typedef void (^ImageURLHandlerBlock) (SEPageImageURL* imageURL);
/*
 when you use finger to scroll , SEUIScrollView will calculate the start row and end row according to 
 current content offset, and send start row and end row to photo load thread.
 
 */
@interface SEPageUIScrollView : UIScrollView <SEAdjustContentView>
{
    NSString* mName;
    NSMutableArray* mPhotoAssetURL;
    //NSUInteger mVisibleStartIndex;
    //NSUInteger mVisibleEndIndex;
    int mPhotoWidth; // every photo frame width
    int mPhotoHeight; // every photo frame height
    int mHMargin; // horizontal margin between photo frame
    int mVMargin; // vertical margin between photo frame
    int mLeftPadding;
    int mRightPadding;
    int mTopPadding;
    int mBottomPadding;
    SEPAGE_SCROLLVIEW_TYPE mScrollViewType;
    //this is the page count, each page has 24 images
    int mPageCount;
    UIImage* mPageBackground;
    int mPageRow;
    int mPageCol;
    CGFloat mPageWidth;
    CGFloat mPageHeight;
    SEViewNavigator* mViewNavigator;

    UIView* mParentView;// this view is the child of UIScrollView , and parent of all added view
    NSOperationQueue* mOperationQueue;
    //////// the following is the internal state ////
    size_t mPhotoSize; // the image byte size in image dictionary, unit is K
    //NSMutableDictionary* mPhotoDict;
    NSMutableArray* mPhotoDict; // array of SEPagePhotoDictItem
    
    int mAssetIdentity[SEPAGE_ASSET_NUM];
    int mCurrentAssetIndex;
    ALAssetsLibrary* mAssetLibrary;
    int mAssetAcessError;
    BOOL mStopLoadImage;
    UIImage* mNotFoundImage;
    ////////////////////////////////////
    NSMutableArray* mEventList;
    NSCondition* mEventListCondition;
    int mStartPage; //the first page you can see currently
    int mEndPage; // the last page you can see currently
    
    SEPageUIScrollViewDelegate* iScrollViewDelegate;
    id <SEPageHandleMultiTouchDelegate> mHandleMultiTouchDelegate;
    id <SEPageUIScrollViewPressHandler> mLongPressHandler;
    BOOL mMultiTouchBegan;
    int mCurrentStartPage; //This is the start page in current view port
    BOOL mCanTouchResponse;
    UIImage* mDefaultImage;
    SEResLoader* mResLoader;
    UIImage* mBackgroundImage;
    UIImage* mFrameImage;
    UIImage* mHighlightedFrameImage;
    //NSArray* mGesture;
    SEMultiTouchDetect* mMultiTouchDetect;
    int mSelectedIndex;
    BOOL mUseMultiTouch;
    BOOL mIsSingleColumn;
    int mPageImageCount;
    NSMutableArray* mCurrentImageArray;
    int mAnimEndCount;
    int mLastImageViewIndex;
    //NSURL* mCurrentURL;
    BOOL mIsCalculateVisibleRange;
    BOOL mIsAnimEnd;
    NSString* mImageListName;
    BOOL mIsScrolling;
    
    int mPressState;
    CGFloat mPressMoveSpacing;
    CGPoint mPressStartPoint;
    
    BOOL mGetImageURLInMainThread;
    int mImageViewCount;//this count is used to maintain image view count, it is initialized in createContent. After initialized , it will not be changed.
    BOOL mIsLoadingImage;
    int mNextLoadImageURLIndex;
    //NSOperationQueue* mImageLoadingOperationQueue;
    //property for debug
    BOOL mNotStartThread;
    //end
}
@property (nonatomic, retain) UIImage* mNotFoundImage;
@property (nonatomic, assign) BOOL mIsLoadingImage;
@property (nonatomic, assign) BOOL mIsScrolling;
@property (nonatomic, retain) NSString* mImageListName;
@property (nonatomic, assign) SEPAGE_SCROLLVIEW_TYPE mScrollViewType;
@property (nonatomic, assign) BOOL mGetImageURLInMainThread;
@property (nonatomic, readonly) BOOL mIsAnimEnd;
@property (nonatomic, readonly) BOOL mIsCalculateVisibleRange;
@property (nonatomic, readonly) NSMutableArray* mCurrentImageArray;
@property (nonatomic, readonly) int mLastImageViewIndex;
@property (nonatomic, readonly) int mPageImageCount;
@property (nonatomic, assign) BOOL mIsSingleColumn;
@property (nonatomic, assign) int mSelectedIndex;
@property (nonatomic, assign) BOOL mUseMultiTouch;
@property (nonatomic, assign) id <SEPageHandleMultiTouchDelegate> mHandleMultiTouchDelegate;
@property (nonatomic, assign) id <SEPageUIScrollViewPressHandler> mLongPressHandler;
@property (nonatomic, assign) SEResLoader* mResLoader;
@property (nonatomic, assign) BOOL mNotStartThread;
@property (nonatomic, retain) UIImage* mDefaultImage;
@property (nonatomic, retain) NSString* mName;
@property (nonatomic, assign) SEViewNavigator* mViewNavigator;
@property (nonatomic, assign) int mPhotoWidth;
@property (nonatomic, assign) int mPhotoHeight;
@property (nonatomic, assign) int mHMargin;
@property (nonatomic, assign) int mVMargin;
@property (nonatomic, assign) int mLeftPadding;
@property (nonatomic, assign) int mRightPadding;
@property (nonatomic, assign) int mTopPadding;
@property (nonatomic, assign) int mBottomPadding;
@property (nonatomic, retain) UIImage* mPageBackground;
@property (nonatomic, retain) UIImage* mBackgroundImage;
@property (nonatomic, assign) int mPageRow;
@property (nonatomic, assign) int mPageCol;
@property (nonatomic, readonly) int mStartPage;
@property (nonatomic, readonly) int mEndPage;
@property (nonatomic, assign) int mCurrentStartPage;
@property (nonatomic, assign) BOOL mCanTouchResponse;
@property (nonatomic, retain) UIImage* mFrameImage;
@property (nonatomic, retain) UIImage* mHighlightedFrameImage;

/* use case:
 set mViewWidth and mViewHeight, mPhotoWidth, mPhotoHeight  -- > initState --> initPhotoLibUrl --> createContent
 */
- (void) createContent;

- (void) initPhotoLibUrl;
- (void) initState;
//- (void) relayout;
- (BOOL) canAdjust;

// p is the cooridnate in scroll view
// it is the point to content view's origin , but not scroll view's origin
- (SEPageHitProperty) hitRect: (CGPoint)p;
- (SEPageImageURL*) getImageURL: (int)index;
- (NSUInteger) getImageURLNum;
//this operation just remove image from photo cache, but not from photo url array
//- (void) removeFromPhotoDict: (int)index removeCondition:(SEPAGE_REMOVE_OP)op;
- (void) removeFromPhotoURLAsset: (NSMutableArray*)indexArray;
- (void) enableAllGestures;
- (void) disableAllGestures;
- (SEPageUIImageView*) imageView:(int)index;
//- (void) updateImage: (int) index;
- (void) changeContentToMultipleColumn: (CGFloat)currentViewportWidth movePart: (SEPAGE_PART_TYPE) partType;
- (void) changeContentToSingleColumn: (CGFloat) currentViewportWidth movePart: (SEPAGE_PART_TYPE)partType;
- (void) insertURLToPhotoAsset: (int) index url: (NSMutableArray*)urlArray image: (NSMutableArray*)imageArray;
- (NSArray*) getHighlightedImageView;

- (int) getIndexForImageView:(SEPageUIImageView*)imageView;
//- (void) getImageURL: (int) index withBlock: (ImageURLHandlerBlock) block;
- (void) changeImageView: (NSMutableArray*) imageViewIndexArray toPos : (int)dstImageViewIndex;
//stop will remove the read photo thread, and then send event to delete SEPageScrollView
//- (void) stopAsync: (id) target withAction: (SEL)action withObject: (id) obj;
- (void) stopLoadImage;
- (void) startLoadImage;
- (void) printPhotoDict;
- (BOOL) isStopLoadImage;
- (void) updateImageViewByScroll;
@end
