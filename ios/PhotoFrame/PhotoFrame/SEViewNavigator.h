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
enum VIEW_TYPE {MAIN_DISPLAY, OPTIONS, PREVIEW_3D, IMAGE_PICKER, SELECTED_IMAGE_VIEW, MUSIC_PICKER, MUSIC_IMAGE_LIST_ATTACH, SIGNATURE_PREVIEW, SIGNATURE_VIEW,INVALID_VIEW, VIEW_NUM};
enum VIEW_SEQ_TYPE{MAIN_DISPLAY_ONLY, MAIN_DISPLAY_IMAGE_PICKER, MAIN_DISPLAY_MUSIC_PICKER, MAIN_DISPLAY_OPTIONS, MAIN_DISPLAY_MUSIC_IMAGE_LIST_ATTACH,
    OPTIONS_IMAGE_PICKER, OPTIONS_MUSIC_PICKER, OPTIONS_SIGNATURE, 
    SIGNATURE_SIGNATURE_PREVIEW,
    INVALID_SEQ_TYPE
};

enum MOVE_DIRECT {NO_MOVE, MOVE_LEFT, MOVE_RIGHT};
enum BAR_LOC_TYPE {LEFT_BAR, RIGHT_BAR, INVALID_BAR};
struct BarViewType
{
    VIEW_TYPE leftView;
    VIEW_TYPE rightView;
    BOOL canSeenAsLeftBar;
    BOOL canSeenAsRightBar;
};
//////////
@interface SEUIRootView : UIView
@end
//////////
@interface SEImageListProperty : NSObject {
    NSString* name;
    int imageCount;
    NSString* firstURLString;
}
@property (nonatomic, assign) int imageCount;
@property (nonatomic, retain) NSString* name;
@property (nonatomic, retain) NSString* firstURLString;
@end
////
@interface SEMusicListProperty : NSObject {
@private
    NSString* name;
    int musicCount;
    NSString* firstURLString;
}
@property (nonatomic, retain) NSString* name;
@property (nonatomic, assign) int musicCount;
@property (nonatomic, retain) NSString* firstURLString;
@end
//////////

@class SEContentViewContainer;
@class SEViewNavigator;
@interface SEBarView : UIView 
{
@private
    SEViewNavigator* mViewNav;
    SEContentViewContainer* mLeftContentContainer;
    SEContentViewContainer* mRightContentContainer;
    BarViewType mBarViewType;
    CGRect mHintRect;
    SEResLoader* mResLoader;
@private
    CGPoint mOrig;
    MOVE_DIRECT mDirect;
    UIImageView* mLeftImageView;
    UIImageView* mRightImageView;
    BOOL mCanStopInMid;
    NSObject <SEContainerAnimationHandler>* mContainerAnimHandler;
}
@property (nonatomic, assign) SEResLoader* mResLoader;
@property (nonatomic, assign) CGRect mHintRect;
@property (nonatomic, assign) SEViewNavigator* mViewNav;
@property (nonatomic, assign) SEContentViewContainer* mLeftContentContainer;
@property (nonatomic, assign) SEContentViewContainer* mRightContentContainer;
@property (nonatomic, assign) BarViewType mBarViewType;
@property (nonatomic, assign) BOOL mCanStopInMid;
@property (nonatomic, assign) NSObject<SEContainerAnimationHandler>* mContainerAnimHandler;
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
    NSString* mBarBackgroundKey;
@private
    CGRect mHintRect;
}
@property (nonatomic, retain) NSString* mBarBackgroundKey;
@property (nonatomic, assign) CGRect mHintRect;
@property (nonatomic, assign) VIEW_TYPE mType;
@property (nonatomic, assign) SEViewNavigator* mViewNav;
- (BOOL) hasContent;
- (UIView<SEAdjustContentView>*) contentView;
- (void) initContainer;
- (void) saveContext : (NSArray*)userInfoArray;
@end
///////
@class SEUIScrollView;
@class SEUIFloatView;
@class SEResLoader;
@class SEUIImageView;
@class SEImageURL;
@class SelectedImage;
@class UserInfo;
@class PainterManager;
@class Signature;
@class ImageList;
@class MusicList;
@interface SEViewNavigator : UIViewController <NSFetchedResultsControllerDelegate, SEHandleMultiTouchDelegate, SENextImageDisplay, SEContainerAnimationHandler>
{
    SEContentViewContainer* mViewArray[VIEW_NUM];
    NSMutableArray* mBarViewArray;
    SEUIRootView* mRootView;
    UIView* mContentContainerParent;
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
    int mViewPortWidth;
    int mViewPortHeight;
    float mBarWidth;
    SEBarView* mCurrentLeftBarView;
    SEBarView* mCurrentRightBarView;
    VIEW_SEQ_TYPE mCurrentViewSeqType;
    NSString* mCurrentImageList;
    NSString* mCurrentMusicList;
    NSNumber* mCurrentSignature;
    PainterManager* mPainterManager;
    NSMutableArray* mViewSeqTypeStack;
}
@property (nonatomic, readonly) UIView* mContentContainerParent;
@property (nonatomic, readonly) UIView* mRootView;
@property (nonatomic, assign) SEResLoader* mResLoader;
@property (nonatomic, retain) NSManagedObjectContext* managedObjectContext;
@property (nonatomic, retain) NSArray* mUserInfoProperty;
@property (nonatomic, assign) int mViewPortWidth;
@property (nonatomic, assign) int mViewPortHeight;
@property (nonatomic, readonly) VIEW_TYPE mPrevView;
@property (nonatomic, readonly) VIEW_TYPE mNextView;
@property (nonatomic, readonly) VIEW_TYPE mCurrView;
@property (nonatomic, readonly) float mBarWidth;
- (id) initWithResLoader: (SEResLoader*) resLoader;
- (void) removeAllViewFromRoot;
- (void) setCurrentView: (enum VIEW_TYPE) view_type isAnimation: (BOOL)bAnim;
- (void) initData;
- (NSMutableArray*) getUserImageProperty;
- (void) saveContext;
- (SelectedImage*) getSelectedImageProperty: (int)index;
- (UserInfo*) getUserInfo;
- (int) getImageQuality;
- (int) getImageTimes;
- (UIView<SEAdjustContentView>*) createContentView: (VIEW_TYPE) viewType withFrame: (CGRect) r;
- (void) addSettingView: (UIView*)view;
- (void) addContentToContentContainer: (VIEW_TYPE) vp;
- (BOOL) isFloatViewShow;
- (void) moveFloatViewToPoint : (CGPoint)p;
- (void) moveToView:(VIEW_SEQ_TYPE) vst :(VIEW_TYPE)vp hasAnimation:(BOOL)isAnim isPush:(BOOL) bPush;
- (int) getImageListCount;
- (int) getMusicListCount;
- (Signature*) getCurrentSignature;
- (NSArray*) getAllSignatures;// string array
- (NSArray*) getAllImageList;
- (NSArray*) getAllMusicList;
- (NSArray*) getCurrentSelectedMusicArray;
- (ImageList*) getImageListByName: (NSString*)imageListName;
//the return value is array of ImageList
- (NSArray*) getMusicAttachedImage: (NSString*)name;
- (void) attachImageToMusic: (NSString*)musicListName imageListName:(NSString*) imageListName;
- (MusicList*) getMusicListByName: (NSString*)musicListName;
- (MusicList*) addMusicList:(NSString*) musicListName;
- (ImageList*) addImageList: (NSString*) imageListName;
- (void) removeImageListByName : (NSString*)imageListName;
- (void) removeMusicListByName : (NSString*)musicListName;
- (NSArray*) getAttachedMusicListByImageListName: (NSString*)imageListName;
- (ImageList*) getImageListBySeq:(NSNumber*)seq;
- (MusicList*) getMusicListBySeq: (NSNumber*)seq;
- (BOOL) musicListContainImageList: (MusicList*)musicList :(NSString*) imageListName;
- (VIEW_SEQ_TYPE) popViewSeqType;
- (void) pushViewSeqType:(VIEW_SEQ_TYPE)vst;
- (NSManagedObject*)newObjectByEntityName: (NSString*)entityName;
- (void) clearPlacedView;
@end
