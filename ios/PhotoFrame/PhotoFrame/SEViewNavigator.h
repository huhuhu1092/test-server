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
#import "SEPageScrollView.h"
#import "SEProtocalDefine.h"
//#import "OAuthController.h"
//#import "WeiboClient.h"
enum VIEW_TYPE {MAIN_DISPLAY, OPTIONS, PREVIEW_3D, IMAGE_PICKER, SELECTED_IMAGE_VIEW, MUSIC_PICKER, SELECTED_MUSIC_VIEW, MUSIC_IMAGE_LIST_ATTACH, SIGNATURE_PREVIEW, SIGNATURE_VIEW, MAIN_DISPLAY_MIRROR,OPTIONS_MIRROR, MUSIC_IMAGE_LIST_ATTACH_MIRROR ,INVALID_VIEW, VIEW_NUM};
enum VIEW_SEQ_TYPE{MAIN_DISPLAY_ONLY, MAIN_DISPLAY_IMAGE_PICKER, MAIN_DISPLAY_MUSIC_PICKER, MAIN_DISPLAY_OPTIONS, MAIN_DISPLAY_MUSIC_IMAGE_LIST_ATTACH,
    OPTIONS_IMAGE_PICKER, OPTIONS_MUSIC_PICKER, OPTIONS_SIGNATURE, 
    SIGNATURE_SIGNATURE_PREVIEW,
    MUSIC_IMAGE_LIST_ATTACH_IMAGE_PICKER,
    MUSIC_IMAGE_LIST_ATTACH_MUSIC_PICKER,
    PREVIEW_3D_ONLY,
    INVALID_SEQ_TYPE
};
enum VIEW_RELATION_TYPE {TYPE1, TYPE2};
enum MOVE_DIRECT {NO_MOVE, MOVE_LEFT, MOVE_RIGHT};
enum BAR_LOC_TYPE {LEFT_BAR, RIGHT_BAR, MID_BAR,INVALID_BAR};
enum {PRESENTONDUTY_MEDAL, NEWPERSON_MEDAL, DRAWING_MEDAL, SHARE_MEDAL, FANS_MEDAL, USER_MEDAL_COUNT};
enum {COPPER_MEDAL, SILVER_MEDAL, GOLD_MEDAL, INVALID_MEDAL_LEVEL, MEDAL_LEVEL_COUNT = INVALID_MEDAL_LEVEL};
/////////////////
struct SignaturePointData
{
    CGFloat lineWidth;
    NSMutableArray* points;
};
//////////////////

struct BarViewType
{
    VIEW_TYPE leftView;
    VIEW_TYPE rightView;
    BOOL canSeenAsLeftBar;
    BOOL canSeenAsRightBar;
    BOOL canStopInMid;
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
//////////
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
    BOOL mIsBarAnimationEnd;
    BOOL mTouchStart;
@private
    BOOL mStayOnMid;
    CGPoint mOrig;
    MOVE_DIRECT mDirect;
    UIImageView* mLeftImageView;
    UIImageView* mRightImageView;
    BOOL mCanStopInMid;
    NSObject <SEContainerAnimationHandler>* mContainerAnimHandler;
    BOOL mFirstMove;
}
@property (nonatomic, assign) BOOL mIsBarAnimationEnd;
@property (nonatomic, assign) SEResLoader* mResLoader;
@property (nonatomic, assign) CGRect mHintRect;
@property (nonatomic, assign) SEViewNavigator* mViewNav;
@property (nonatomic, assign) SEContentViewContainer* mLeftContentContainer;
@property (nonatomic, assign) SEContentViewContainer* mRightContentContainer;
@property (nonatomic, assign) BarViewType mBarViewType;
@property (nonatomic, assign) BOOL mCanStopInMid;
@property (nonatomic, assign) NSObject<SEContainerAnimationHandler>* mContainerAnimHandler;
@property (nonatomic, assign) BOOL mStayOnMid;
- (BAR_LOC_TYPE) barLocationType;
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
    UIImage* mBackgroundImage;
@private
    CGRect mHintRect;
}
@property (nonatomic, retain) NSString* mBarBackgroundKey;
@property (nonatomic, assign) CGRect mHintRect;
@property (nonatomic, assign) VIEW_TYPE mType;
@property (nonatomic, assign) SEViewNavigator* mViewNav;
@property (nonatomic, retain) UIImage* mBackgroundImage;
- (BOOL) hasContent;
- (UIView<SEAdjustContentView>*) contentView;
- (void) initContainer;
- (void) saveContext : (NSArray*)userInfoArray;
- (void) updateContent;
- (void) decreaseContent: (float) contentWidth part: (int)partType;
- (void) expandContent:(float) contentWidth part: (int)partType;
- (BOOL) isContentAnimEnd;
- (void) removeContentView;
- (void) stop;
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
@class SEFontLoader;
@class SEUserUpgrade;
@class SEMusicPickerController;
@class SEOperationViewGroup;
@class SEOperationHandlerConcrete;
@class SEPopupView;
@class SEDataUploadManager;
@class SEMusicItemProperty;
//@class OAuthEngine;
@interface SEViewNavigator : UIViewController <NSFetchedResultsControllerDelegate, SEPageUIScrollViewPressHandler, SEPageHandleMultiTouchDelegate, SEContainerAnimationHandler>
{
    SEContentViewContainer* mViewArray[VIEW_NUM];
    NSMutableArray* mBarViewArray;
    SEUIRootView* mRootView;
    UIView* mContentContainerParent;
    enum VIEW_TYPE mCurrView;
    //enum VIEW_TYPE mPrevView;
    //enum VIEW_TYPE mNextView;
    SEUIFloatView* mFloatView;
    SEResLoader* mResLoader;
    SEFontLoader* mFontLoader;
    NSManagedObjectContext* managedObjectContent;
    NSPersistentStoreCoordinator* persistentStoreCoordinator;
    NSArray* mUserInfoProperty;
    SEUserUpgrade* mUserUpgradeInfo;
    SEDataUploadManager* mDataUploadManager;
@private
    //SEPageImageURL* mSelectedPhotoURL;
    NSMutableArray* mSelectedPhotoURL; // array of SEPageImageURL
    NSMutableArray* mSelectedImage; //array of selected image
    NSMutableArray* mSelectedImageViewIndex;
    BOOL mOperationEnable[64];
    SEContentViewContainer* mOperationContainter;
    int mPlacedViewIndex;
    int mViewPortWidth;
    int mViewPortHeight;
    float mBarWidth;
    SEBarView* mCurrentLeftBarView;
    SEBarView* mCurrentRightBarView;
    VIEW_SEQ_TYPE mCurrentViewSeqType;
    PainterManager* mPainterManager;
    NSMutableArray* mViewSeqTypeStack;
    SEOperationViewGroup* mOperationViewGroup;
    SEOperationHandlerConcrete* mOperationHandler;
    SEPopupView* mPopupView;
    NSString* mDrawingImageList;
    UIView* mMusicFloatView;
    NSMutableData* mRecvData;
    BOOL mNewConfig;
    id mLabel;
    /// for weibo
    NSObject<SEShareImage>* mShareImage;
    //OAuthEngine				*_engine;
	//WeiboClient *weiboClient;
	//NSMutableArray *statuses;
    //NSMutableArray* mSharedImageArray;
    //end
}
@property (nonatomic, assign) BOOL mNewConfig;
@property (nonatomic, assign) UIView* mMusicFloatView;
@property (nonatomic, readonly) NSMutableArray* mSelectedImageViewIndex;
@property (nonatomic, readonly) SEContentViewContainer* mOperationContainer;
@property (nonatomic, readonly) SEFontLoader* mFontLoader;
@property (nonatomic, readonly) UIView* mContentContainerParent;
@property (nonatomic, readonly) UIView* mRootView;
@property (nonatomic, assign) SEResLoader* mResLoader;
@property (nonatomic, retain) NSPersistentStoreCoordinator* persistentStoreCoordinator;
@property (nonatomic, retain) NSManagedObjectContext* managedObjectContext;
@property (nonatomic, retain) NSArray* mUserInfoProperty;
@property (nonatomic, assign) int mViewPortWidth;
@property (nonatomic, assign) int mViewPortHeight;
@property (nonatomic, readonly, getter = getPrevView) VIEW_TYPE prevView;
@property (nonatomic, readonly, getter = getNextView) VIEW_TYPE nextView;
@property (nonatomic, readonly) VIEW_TYPE mCurrView;
@property (nonatomic, readonly) float mBarWidth;
- (SEDataUploadManager*) getDataUploadManager;
- (BOOL) operationEnable: (int)index;
- (id) initWithResLoader: (SEResLoader*) resLoader;
- (void) removeAllViewFromRoot;
- (void) setCurrentView: (enum VIEW_TYPE) view_type isAnimation: (BOOL)bAnim;
- (void) initData;
- (NSArray*) getUserImageProperty;
- (void) saveContext;
- (void) saveCoreDataContext;
- (SelectedImage*) getSelectedImageProperty: (int)index;
- (SelectedImage*) getSelectedImageByUrl : (NSString*) urlString andDate: (NSString*)date;
- (UserInfo*) getUserInfo;
- (int) getImageQuality;
- (int) getImageTimes;
- (void) setImageQuality:(int) q;
- (void) setImageTimes: (int)t;
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
- (MusicList*) getMusicListByImageList: (NSString*)imageListName;

- (void) addSelectedMusicTitle: (NSString*)title aritist: (NSString*) artist album:(NSString*)album toMusicList: (NSString*) musicListName;
- (VIEW_SEQ_TYPE) popViewSeqType;
- (void) pushViewSeqType:(VIEW_SEQ_TYPE)vst;
- (NSManagedObject*)newObjectByEntityName: (NSString*)entityName;
- (void) clearPlacedView;

- (void) setViewRelationType:(VIEW_RELATION_TYPE) type;
//return the max count of image user can select
- (int) getMaxSelectedImageCount;
//return the max count of music user can select
- (int) getMaxSelectedMusicCount;
//return all image count in all user defined image list
- (int) getCurrentAllSelectedImageCount;
//return all music count in all user defined music list
- (int) getCurrentAllSelectedMusicCount;
- (void) setPhotoURLToCoreData: (NSMutableArray*)photoURLArray;
- (void) setSelectedMusicToCoreData: (NSArray*)musicArray;
- (void) popupView:(UIView*)v;
- (void) dismissPopup;
//uiImage is the original size image
- (void) saveImageThumbnailToCoreData:(UIImage*)uiImage urlName:(NSString*)url urlDate: (NSString*)date index:(int)index;
- (void) saveImageToCoreData: (UIImage*)uiImage urlName:(NSString*)url urlDate: (NSString*)date index:(int)index;
- (void) saveImageAndThumbnailToCoreData:(UIImage*)uiImage urlName:(NSString*)url urlDate: (NSString*)date index:(int)index;
- (void) removeImageFromCoreDate:(NSString*)url urlDate: (NSString*)date;
- (UIImage*) getImageFromCoreData: (NSString*)url urlDate: (NSString*)date;
- (UIImage*) getImageFromCoreData: (NSString*)url urlDate: (NSString*)date managedObjectContext: (NSManagedObjectContext*)managedObjectContext;
- (UIImage*) getThumbnailFromCoreData:(NSString*)url urlDate: (NSString*)date;
- (UIImage*) getImageFromPhotoLib : (NSString*)url urlDate: (NSString*)date;
- (CGSize) getThumbnailSize;
- (void) removeSelectedImageNotInImageURLs:(NSMutableArray*)photoURLs;
- (NSSet*) getUnsendIssueReport;
- (void) saveIssueReport: (NSString*)devName : (double)date : (NSString*)title : (NSString*)descript;
- (void) printSelectedImage;
- (void) printThumbnail;
- (void) determineSelectedMusicRow;
- (void) addMusicToSelectedView: (SEMusicItemProperty*)item;
- (SignaturePointData) getSignaturePoints:(NSNumber*)seq;
- (SignaturePointData) getSignaturePointsWithSeqName:(NSString*)name;
- (Signature*) getSignature: (NSNumber*)seq;
- (SignaturePointData) getCurrentSignaturePoints;
- (void)displayNextImage;
- (void) setSelectedImageProperty: (NSString*)url urlDate: (NSString*)urlDate orientation: (int) orient image:(UIImage*)image;
//for test
- (void) downloadParamConfig : (id) label;
//obsolete
- (void) shareImage;
- (void) addShareImage : (UIImage*) image;
@end
