//
//  SEViewNavigator.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-1-6.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
#import <CoreData/CoreData.h>
#import "SEUIScrollView.h"
enum VIEW_TYPE {IMAGE_PICKER, SELECTED_IMAGE_VIEW, MUSIC_PICKER, INVALID_VIEW, VIEW_NUM};

enum MOVE_DIRECT {NO_MOVE, MOVE_LEFT, MOVE_RIGHT};
enum BAR_LOC_TYPE {LEFT_BAR, RIGHT_BAR, INVALID_BAR};
struct BarViewType
{
    VIEW_TYPE leftView;
    VIEW_TYPE rightView;
};
@interface SEUIRootView : UIView
@end

@interface SEMusicPickerDelegate : NSObject <UITableViewDelegate, UITableViewDataSource> {

}
@end
@interface SEMusicSelectedViewDelegate : NSObject <UITableViewDelegate, UITableViewDataSource> {

}
@end
@class SEContentViewContainer;
@class SEViewNavigator;
@interface SEBarView : UIView 
{
@private
    SEViewNavigator* mViewNav;
    SEContentViewContainer* mLeftContentContainer;
    SEContentViewContainer* mRightContentContainer;
    BarViewType mBarViewType;
@private
    CGPoint mOrig;
    MOVE_DIRECT mDirect;
    UIImageView* mLeftImageView;
    UIImageView* mRihtImageView;
    BOOL mCanStopInMid;
}
@property (nonatomic, assign) SEViewNavigator* mViewNav;
@property (nonatomic, assign) SEContentViewContainer* mLeftContentContainer;
@property (nonatomic, assign) SEContentViewContainer* mRightContentContainer;
@property (nonatomic, assign) BarViewType mBarViewType;
@property (nonatomic, assign) BOOL mCanStopInMid;
- (BAR_LOC_TYPE) barType;
- (CGPoint) pointInScreen: (CGPoint) point;
@end
////////
/*
 * content view's child is UIView<SEAdjustContent>
 */
@interface SEContentViewContainer : UIView 
{
    VIEW_TYPE mType;
    SEViewNavigator* mViewNav;
}
@property (nonatomic, assign) VIEW_TYPE mType;
@property (nonatomic, assign) SEViewNavigator* mViewNav;
- (BOOL) hasContent;
- (UIView<SEAdjustContentView>*) contentView;
- (void) initContainer;
@end
///////
@class SEUIScrollView;
@class SEUIFloatView;
@class SEResLoader;
@class SEUIImageView;
@class SEImageURL;
@class SelectedImage;
@interface SEViewNavigator : UIViewController <NSFetchedResultsControllerDelegate>
{
    SEContentViewContainer* mViewArray[VIEW_NUM];
    NSMutableArray* mBarViewArray;
    SEUIRootView* mRootView;
    UIView* mContentView;
    enum VIEW_TYPE mCurrView;
    enum VIEW_TYPE mPrevView;
    enum VIEW_TYPE mNextView;
    SEUIFloatView* mFloatView;
    SEResLoader* mResLoader;
    
    NSManagedObjectContext* managedObjectContent;
    NSArray* mUserInfoProperty;
@private
    SEImageURL* mSelectedPhotoURL;
    SEUIImageView* mPlacedView;
    int mPlacedViewIndex;
    SEMusicPickerDelegate* mMusicPickerDelegate;
    SEMusicSelectedViewDelegate* mMusicSelectedViewDelegate;
    int mViewPortWidth;
    int mViewPortHeight;
    float mBarWidth;
    SEBarView* mCurrentLeftBarView;
    SEBarView* mCurrentRightBarView;
}
@property (nonatomic, readonly) UIView* mContentView;
@property (nonatomic, readonly) UIView* mRootView;
@property (nonatomic, assign) SEResLoader* mResLoader;
@property (nonatomic, retain) NSManagedObjectContext* managedObjectContext;
@property (nonatomic, retain) NSArray* mUserInfoProperty;
@property (nonatomic, readonly) SEMusicPickerDelegate* mMusicPickerDelegate;
@property (nonatomic, readonly) SEMusicSelectedViewDelegate* mMusicSelectedViewDelegate;
@property (nonatomic, assign) int mViewPortWidth;
@property (nonatomic, assign) int mViewPortHeight;
@property (nonatomic, readonly) VIEW_TYPE mPrevView;
@property (nonatomic, readonly) VIEW_TYPE mNextView;
@property (nonatomic, readonly) VIEW_TYPE mCurrView;
@property (nonatomic, readonly) float mBarWidth;
- (id) initWithResLoader: (SEResLoader*) resLoader;
- (void) removeAllViewFromRoot;
- (void) setCurrentView: (enum VIEW_TYPE) view_type;
- (void) initData;
- (NSMutableArray*) getUserImageProperty;
- (void) saveContext;
- (SelectedImage*) getSelectedImageProperty: (int)index; 
- (UIView<SEAdjustContentView>*) createSettingView: (VIEW_TYPE) viewType withFrame: (CGRect) r;
- (void) addSettingView: (UIView*)view;
- (void) addContentToContentContainer: (VIEW_TYPE) vp;
@end
