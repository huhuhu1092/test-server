//
//  SEUIScrollView.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-1-20.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "SEProtocalDefine.h"
enum SCROLL_DIR {UP, DOWN, STATIC};
enum {PHOTODICT_SIZE = 2 * 1024 * 1024};
enum {ASSET_NUM = 2};
enum URL_TYPE {FILE_PATH, PHOTO_LIB_URL};
@class SEUIImageView;
@class SEViewNavigator;
@class SEResLoader;
@class SEMultiTouchDetect;
struct HitProperty
{
    CGRect rect;
    SEUIImageView* imageView;
    int index;
};

@interface SEImageURL : NSObject 
{
    NSURL* url;
    URL_TYPE type;
    NSURL* filepath;
}
@property (nonatomic, retain) NSURL* url;
@property (nonatomic, assign) URL_TYPE type;
@property (nonatomic, retain) NSURL* filepath;
@end
///////////////////////
@interface SEUIImageView : UIView 
{
    //UIImage* mHighlightedImage;
    //UIView* mBackground;
    //UIImage* mNormalImageView;
    UIImageView* mImageView;
    UIImageView* mFrameImageView;
}
//@property (nonatomic, retain) UIImage* mHighlightedImage;
@property (nonatomic, readonly) UIImageView* mImageView;
@property (nonatomic, assign) UIImage* image;
@property (nonatomic, assign) UIImage* frameImage;
@end
///////////////////////
@class ALAssetsLibrary;
@interface SEUIScrollViewDelegate  : NSObject <UIScrollViewDelegate>
- (void) scrollViewDidScroll:(UIScrollView *)scrollView;
@end
@class SEImageURL;
@protocol SEUIPhotoLoaderDelegate <NSObject>

- (void) initState;
- (void) createPhotoUrlArray : (NSMutableArray*)photoURLArray;
- (CGImageRef) getPhotoFromURL: (SEImageURL*) url;
@end
/////////
@class SEUIScrollView;
@interface SEUIPhotoLibLoaderDelegete : NSObject <SEUIPhotoLoaderDelegate> 
{
    int mAssetIdentity[ASSET_NUM];
    int mCurrentAssetIndex;
    ALAssetsLibrary* mAssetLibrary;
    SEUIScrollView* mScrollView;
}
@property (nonatomic, assign) SEUIScrollView* mScrollView;
- (void) initState;
- (void) createPhotoUrlArray: (NSMutableArray*)photoURLArray;
- (CGImageRef) getPhotoFromURL:(SEImageURL *)url;
@end
//////////
@interface SEUIPhotoFileLoaderDelegate : NSObject <SEUIPhotoLoaderDelegate>
{
    SEUIScrollView* mScrollView;
    SEViewNavigator* mNavView;
    ALAssetsLibrary* mAssetLibrary;
}
@property (nonatomic, assign) SEUIScrollView* mScrollView;
@property (nonatomic, assign) SEViewNavigator* mNavView;
- (void) initState;
- (void) createPhotoUrlArray: (NSMutableArray*)photoURLArray;
- (CGImageRef) getPhotoFromURL:(SEImageURL *)url;
@end
/////////
@protocol SEHandleMultiTouchDelegate <NSObject>

- (void) touchBegin: (NSArray*)points1 : (NSArray*)points2 withPointDelta: (CGPoint)deltap withScrollView: (SEUIScrollView*)scrollView;
- (void) touchMove: (NSArray*)points : (NSArray*)points2 withPointDelta: (CGPoint)deltap withScrollView: (SEUIScrollView*)scrollView;
- (void) touchEnd: (NSArray*)points : (NSArray*)points2 withPointDelta: (CGPoint)deltap withScrollView: (SEUIScrollView*)scrollView;

@end
@class SEUIScrollViewDelegate;

/*
 when you use finger to scroll , SEUIScrollView will calculate the start row and end row according to 
 current content offset, and send start row and end row to photo load thread.
 
 */
@interface SEUIScrollView : UIScrollView <SEAdjustContentView>
{
    NSString* mName;
    NSMutableArray* mPhotoAssetURL;
    NSUInteger mVisibleStartIndex;
    NSUInteger mVisibleEndIndex;
    int mPhotoWidth; // every photo frame width
    int mPhotoHeight; // every photo frame height
    int mHMargin; // horizontal margin between photo frame
    int mVMargin; // vertical margin between photo frame
    int mLeftPadding;
    int mRightPadding;
    int mTopPadding;
    int mBottomPadding;
    // the following two property is used to create scroll view's content size
    int mContentWidth; // content width in scroll view
    int mContentHeight; //content height in scroll view, 
    // the following two property is the number of row and column in scroll view
    int mColumn;
    int mRow;
    SEViewNavigator* mViewNavigator;
    /// after set mViewWidth and mViewHeight, createContent can work 
    int mViewWidth; // this is scroll view's width
    int mViewHeight; // this is scroll view's height
    UIView* mParentView;// this view is the child of UIScrollView , and parent of all added view
    //////// the following is the internal state ////
    size_t mPhotoSize; // the image byte size in image dictionary, unit is K
    NSMutableDictionary* mPhotoDict;
    NSMutableArray* mEventList;
    NSCondition* mEventListCondition;
    int mPrevStartRow;
    int mPrevEndRow;
    int mStartRow;
    int mEndRow;
    int mNextStartRow;
    int mNextEndRow;
    //int mDirection;//0 : UP, 1 : DOWN 2: STATIC
    SEUIScrollViewDelegate* iScrollViewDelegate;
    id <SEUIPhotoLoaderDelegate> mPhotoLoaderDelegate;
    id <SEHandleMultiTouchDelegate> mHandleMultiTouchDelegate;
    BOOL mMultiTouchBegan;
    int mStartIndex;
    BOOL mCanTouchResponse;
    UIImage* mDefaultImage;
    SEResLoader* mResLoader;
    UIImage* mBackgroundImage;
    UIImage* mSelectedFrameImage;
    NSArray* mGesture;
    SEMultiTouchDetect* mMultiTouchDetect;
    int mSelectedIndex;
    BOOL mUseMultiTouch;
    //property for debug
    BOOL mNotStartThread;
    //end
}
@property (nonatomic, assign) int mSelectedIndex;
@property (nonatomic, assign) BOOL mUseMultiTouch;
@property (nonatomic, assign) id <SEHandleMultiTouchDelegate> mHandleMultiTouchDelegate;
@property (nonatomic, assign) SEResLoader* mResLoader;
@property (nonatomic, assign) BOOL mNotStartThread;
@property (nonatomic, retain) UIImage* mDefaultImage;
@property (nonatomic, retain) NSString* mName;
@property (nonatomic, assign) SEViewNavigator* mViewNavigator;
@property (nonatomic, assign) int mViewWidth;
@property (nonatomic, assign) int mViewHeight;
@property (nonatomic, readonly) int mContentWidth;
@property (nonatomic, readonly) int mContentHeight;
@property (nonatomic, assign) int mPhotoWidth;
@property (nonatomic, assign) int mPhotoHeight;
@property (nonatomic, assign) int mHMargin;
@property (nonatomic, assign) int mVMargin;
@property (nonatomic, assign) int mLeftPadding;
@property (nonatomic, assign) int mRightPadding;
@property (nonatomic, assign) int mTopPadding;
@property (nonatomic, assign) int mBottomPadding;
@property (nonatomic, readonly) int mRow;
@property (nonatomic, readonly) int mColumn;
@property (nonatomic, readonly) int mStartRow;
@property (nonatomic, readonly) int mEndRow;
@property (nonatomic, assign) id <SEUIPhotoLoaderDelegate> mPhotoLoaderDelegate;
@property (nonatomic, assign) int mStartIndex;
@property (nonatomic, assign) BOOL mCanTouchResponse;
@property (nonatomic, retain) UIImage* mSelectedFrameImage;

/* use case:
 set mViewWidth and mViewHeight, mPhotoWidth, mPhotoHeight  -- > initState --> initPhotoLibUrl --> createContent
 */
- (void) createContent;
- (void) calculateVisibleView: (CGFloat)contentOffset;
- (void) initPhotoLibUrl;
- (void) initState;
- (void) relayout;
- (BOOL) canAdjust;
- (float) calculateContentOffset : (int) startRow;
// p is the cooridnate in scroll view
// it is the point to content view's origin , but not scroll view's origin
- (HitProperty) hitRect: (CGPoint)p;
- (SEImageURL*) getImageURL: (int)index;
- (NSUInteger) getImageURLNum;
- (void) removeFromPhotoDict: (int)index;
- (void) saveGesture;
- (void) restoreGesture;
- (void) removeAllGestures;
- (SEUIImageView*) imageView:(int)index;
- (void) updateImage: (int) index;
@end
