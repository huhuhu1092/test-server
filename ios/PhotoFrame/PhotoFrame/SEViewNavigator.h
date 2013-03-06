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
#import "FontLabel.h"
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
enum {PRESENTONDUTY_MEDAL, NEWPERSON_MEDAL, DRAWING_MEDAL, SHARE_MEDAL, FANS_MEDAL, INVALID_ACHIEVE,USER_MEDAL_COUNT = INVALID_ACHIEVE};
enum {INVALID_LEVEL};
enum {INVALID_MEDAL_LEVEL, COPPER_MEDAL, SILVER_MEDAL, GOLD_MEDAL, MEDAL_LEVEL_COUNT};
enum DRAW_IMAGE_STATE {INIT_DRAW_IMAGE_STATE, START_DRAW_IMAGE, PAUSE_DRAW_IMAGE, STOPPING_DRAWING_IMAGE, STOP_DRAW_IMAGE, START_DRAW_IMAGE_PENDING, DRAW_FINISHED };
/////////////////
struct SignaturePointData
{
    CGFloat lineWidth;
    NSMutableArray* points;
    NSMutableArray* colors;
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
@interface SEDimView : UIView

@end
////////
@interface SEDialogView : UIView
{
    UIView* mContentView;
}
//@property (nonatomic, retain) UIView* mContentView;
- (void) setContentView: (UIView*) v;
- (UIView*) getContentView;
@end
///////
@interface SEUIRootView : UIView
{
    //UIImageView* imageView;
}
//@property (nonatomic, retain) UIImageView* imageView;
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
    //UIImageView* mArrowView;
    UIButton* mLeftArrowButton;
    UIButton* mRightArrowButton;
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

@interface SEContentContainerLabel : FontLabel <SEAdjustContentView>

@end
@interface SEContentViewContainer : UIView 
{
    VIEW_TYPE mType;
    SEViewNavigator* mViewNav;
    NSString* mBarBackgroundKey;
    UIImage* mBackgroundImage;
    SEContentContainerLabel* mTitleLabel;
    SEContentContainerLabel* mStatusLabel;
    CGFloat mTitleLabelLeftX;
    CGFloat mTitleLabelMidX;
    CGFloat mStatusLabelRightX;
    CGFloat mStatusLabelMidX;
    CGFloat mStatusLabelWidth;
    CGFloat mStatusLabelHeight;
    CGFloat mStatusLabelXRelativeToBar;
    CGFloat mTitleLabelXRelativeToBar;
@private
    CGRect mHintRect;
}
@property (nonatomic, assign) CGFloat mStatusLabelXRelativeToBar;
@property (nonatomic, assign) CGFloat mTitleLabelXRelativeToBar; 
@property (nonatomic, assign) CGFloat mStatusLabelWidth;
@property (nonatomic, assign) CGFloat mStatusLabelHeight;
@property (nonatomic, assign) CGFloat mTitleLabelLeftX;
@property (nonatomic, assign) CGFloat mTitleLabelMidX;
@property (nonatomic, assign) CGFloat mStatusLabelRightX;
@property (nonatomic, assign) CGFloat mStatusLabelMidX;
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
- (void) createTitleLabel :(CGRect)frame fontName: (NSString*)fontName fontSize: (CGFloat)fontSize textColor: (UIColor*)color;
- (void) createStatusLabel : (CGRect) frame fontName: (NSString*)fontName fontSize: (CGFloat) fontSize textColor:(UIColor*)color;

- (SEContentContainerLabel*) getTitleLabel;
- (SEContentContainerLabel*) getStatusLabel;
- (void) setTitleText: (NSString*) str;
- (void) setStatusText: (NSString*)str;
@end
///////
enum LOADING_STAGE_TYPE {LOADING_STAGE1, LOADING_STAGE2, LOADING_STAGE3, NO_LOADING_STAGE, LOADING_STAGE_NUM = NO_LOADING_STAGE};
@class SEUIProgressView;
@class SEOptionsLabel;
@interface SELoadingButton : UIView
{
    UIButton* button;
    FontLabel* buttonText;
}
@property (nonatomic, readonly) UIButton* button;
@property (nonatomic, readonly) FontLabel* buttonText;
@end
@interface SELoadingView : UIView
{
    //UIImageView* backgroundView;
    enum {SE_LOADINVIEW_TWO_BUTTON, SE_LOADINGVIEW_ONE_BUTTON};
    UIImageView* iconView;
    UIImageView* stageView;
    int mCurrentStage;
    SEUIProgressView* mProgressView;
    SEOptionsLabel* mTextLabel;
    SEOptionsLabel* mTextLabel2;
    SELoadingButton* mButtonExit;
    SELoadingButton* mButtonContinue;
}
@property (nonatomic, readonly) int mCurrentStage;
- (void) setIconViewTransform: (CGAffineTransform) transform;
- (void) setStage: (enum LOADING_STAGE_TYPE) stage;
- (void) showStageView;
/////////////////////////////////////////////////////
- (void) setPercent: (float) v;
- (void) setText: (NSString*) text;
- (void) setText2: (NSString*)text;
- (void) useTextView;
- (void) useProgressView;
- (BOOL) isUseTextView;
- (BOOL) isUseProgressView;
- (void) showButton: (int) type;
- (SELoadingButton*) getExitButton;
- (SELoadingButton*) getContinueButton;
@end
///////
@interface SEUIFloatView : UIView
{
    CGPoint p;
    CGPoint origC;
    NSTimeInterval time;
    NSMutableDictionary* d;
    
    UIImageView* mBackgroundView;
    UIImageView* mForegroundView;
    UILabel* mLabel;
}
@property (nonatomic, assign) CGPoint p;
@property (nonatomic, assign) CGPoint origC;
@property (nonatomic, assign) UIImage* image;
@property (nonatomic, assign) UIImage* backgroundImage;
- (void) setCount: (int)n;
@end
/////////
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
@class SEUserUpgradeViewController;
@class SEUserData;
@class SEMusicOperationHandlerConcrete;
@class SEMusicImageListPopupView;
@class SEConfirmDlg;
@class MPMediaItem;
@class UpgradeInfo;
@class SEWebServiceManager;
@class SEUserUpgradeTestController;
@class SEDrawingStateManager;
@class SECommentSender;
@class SESystemDataManager;
//@class OAuthEngine;
@interface SEViewNavigator : UIViewController <NSFetchedResultsControllerDelegate, SEPageUIScrollViewPressHandler, SEPageHandleMultiTouchDelegate, SEContainerAnimationHandler>
{
    enum FLOAT_VIEW_TYPE {NO_FLOATVIEW, FLOATVIEW_IMAGE, FLOATVIEW_MUSIC};
    SEContentViewContainer* mViewArray[VIEW_NUM];
    NSMutableArray* mBarViewArray;
    SEUIRootView* mRootView;
    UIView* mContentContainerParent;
    UIView* mNotificationView;
    UIView* mUserUpdateView;
    UIView* mLogView;
    enum VIEW_TYPE mCurrView;
    enum VIEW_TYPE mPrevCurrView;
    //enum VIEW_TYPE mPrevView;
    //enum VIEW_TYPE mNextView;
    SEUIFloatView* mFloatView;
    SEResLoader* mResLoader;
    SEFontLoader* mFontLoader;
    NSManagedObjectContext* managedObjectContent;
    NSPersistentStoreCoordinator* persistentStoreCoordinator;
    NSArray* mUserInfoProperty;
    SEUserUpgrade* mUserUpgradeInfo;
    SEUserUpgradeViewController* mUserUpgradeViewController;
    SEDataUploadManager* mDataUploadManager;
    SEWebServiceManager* mWebServiceManager;
    BOOL mMusicPlayListChange;
    BOOL mMoveViewFromMainDisplay;
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
    SESystemDataManager* mSystemDataManager;
    //SEOperationHandlerConcrete* mOperationHandler;
    
    //SEOperationViewGroup* mMusicOperationViewGroup;
    //SEMusicOperationHandlerConcrete* mMusicOperationHandler;
    
    SEPageUIImageView* mCurrentPageUIImageView;
    CGRect mCurrentPageUIImageViewRect;
    
    SEPopupView* mPopupView;
    NSString* mDrawingImageList;
    UIView* mMusicFloatView;
    UIInterfaceOrientation mCurrentOrientation;
    SEDimView* mDimView;
    NSString* mCurrentLoadedImageListName;
    NSString* mCurrentLoadedMusicListName;
    FLOAT_VIEW_TYPE mCurrentFloatViewType;
    float mCurrentSystemBright;
    NSMutableData* mRecvData;
    BOOL mNewConfig;
    id mLabel;
    UILabel* mFirstNumView;
    UILabel* mSecondNumView;
    SELoadingView* mLoadingView;
    NSTimer* mLoadingTimer;
    float mStartAngle;
    SEMusicImageListPopupView* mMusicImageAttachInputPopupView;
    SEConfirmDlg* mConfirmDlg;
    id mConfirmTarget;
    SEL mConfirmOK;
    SEL mConfirmCancel;
    UIColor* mSignatureAutoColor;
    NSTimer* mUpgradeTimer;
    UIWebView* mWebView;
    SEDialogView* mDialogView;
    BOOL mCurrentPlay;
    NSString* mCurrentDownloadingFileName;
    NSString* mCurrentPlayMusicListName;
    DRAW_IMAGE_STATE mDrawImageState;
    NSTimer* mNotificationViewTimer;
    BOOL mBrushRandom;
    BOOL mStartLaunch;
    //for test
    SEUserUpgradeTestController* mUserUpgradeTestControler;
    UIView* mUserUpgradePopupTestView;
    //BOOL mIsShouldRotate;
    BOOL mViewLoaded;
    SEDrawingStateManager* mDrawingStateManager;
    BOOL mUpgradeViewShow;
    BOOL mMoveFromOtherView;
    
    BOOL mMusicObservFirst;
    SECommentSender* mCommentSender;
    //
    /// for weibo
    NSObject<SEShareImage>* mShareImage;
    //OAuthEngine				*_engine;
	//WeiboClient *weiboClient;
	//NSMutableArray *statuses;
    //NSMutableArray* mSharedImageArray;
    //end
}
@property (nonatomic, assign) BOOL mStartLaunch;
@property (nonatomic, readonly) SESystemDataManager* mSystemDataManager;
@property (nonatomic, assign) BOOL mMusicPlayListChange;
@property (nonatomic, retain) NSString* mCurrentDownloadingFileName;
@property (nonatomic, readonly) SEWebServiceManager* mWebServiceManager;
@property (nonatomic, retain) UIColor* mSignatureAutoColor;
@property (nonatomic, assign) float mCurrentSystemBright;
@property (nonatomic, assign) FLOAT_VIEW_TYPE mCurrentFloatViewType;
@property (nonatomic, retain) NSString* mCurrentLoadedImageListName;
@property (nonatomic, retain) NSString* mCurrentLoadedMusicListName;
@property (nonatomic, readonly) UIInterfaceOrientation mCurrentOrientation;
@property (nonatomic, assign) BOOL mNewConfig;
@property (nonatomic, assign) UIView* mMusicFloatView;
@property (nonatomic, retain) NSMutableArray* mSelectedPhotoURL;
@property (nonatomic, retain) NSMutableArray* mSelectedImage;
@property (nonatomic, retain) NSMutableArray* mSelectedImageViewIndex;
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
- (void) makeImageMusicPickerMoveToMid;
- (SEDataUploadManager*) getDataUploadManager;
- (BOOL) operationEnable: (int)index;
- (id) initWithResLoader: (SEResLoader*) resLoader;
- (void) removeAllViewFromRoot;
- (void) setCurrentView: (enum VIEW_TYPE) view_type isAnimation: (BOOL)bAnim;
- (void) initData;
- (NSArray*) getUserImageProperty;
- (NSArray*) getSelectedImageArrayByName: (NSString*)name;
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
- (NSArray*) getSortedSelectedImageArrayBySeq: (ImageList*) il;
- (SelectedImage*) getNoneNullSelectedImage: (ImageList*)il :(int) n;
//the return value is array of ImageList
- (NSArray*) getMusicAttachedImage: (NSString*)name;
- (void) removeAllSelectedImageInImageList:(NSString*) ilName;
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
- (void) removeAllAttachedImageFromMusicList: (NSString*)musicListName;
- (void) removeAttachedImageListFromMusicList: (NSString*) imageListName;
- (int) getCurrentLevelImageNum;
- (int) getCurrentLevelMusicNum;
- (void) printAllImageList;
- (void) printAllMusicList;
- (void) addSelectedMusicTitle: (NSString*)title aritist: (NSString*) artist album:(NSString*)album toMusicList: (NSString*) musicListName;
- (VIEW_SEQ_TYPE) popViewSeqType;
- (void) pushViewSeqType:(VIEW_SEQ_TYPE)vst;
- (NSManagedObject*)newObjectByEntityName: (NSString*)entityName;
- (void) clearPlacedView;
- (void) fetchAllSelectedImage;
- (void) setViewRelationType:(VIEW_RELATION_TYPE) type;
//return the max count of image user can select
//- (int) getMaxSelectedImageCount;
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
- (SEPopupView*) getPopupView;
//uiImage is the original size image
- (void) saveImageThumbnailToCoreData:(UIImage*)uiImage urlName:(NSString*)url urlDate: (NSString*)date index:(int)index;
- (void) saveImageToCoreData: (UIImage*)uiImage urlName:(NSString*)url urlDate: (NSString*)date index:(int)index;
- (void) saveImageAndThumbnailToCoreData:(UIImage*)uiImage urlName:(NSString*)url urlDate: (NSString*)date orientation:(int)orientation index:(int)index;
- (void) removeImageFromCoreDate:(NSString*)url urlDate: (NSString*)date;
- (UIImage*) getImageFromCoreData: (NSString*)url urlDate: (NSString*)date;
- (UIImage*) getImageFromCoreData: (NSString*)url urlDate: (NSString*)date managedObjectContext: (NSManagedObjectContext*)managedObjectContext;
- (UIImage*) getThumbnailFromCoreData:(NSString*)url urlDate: (NSString*)date;
- (UIImage*) getThumbnailFromCoreData:(NSString*)url urlDate: (NSString*)date managedObjectContext: (NSManagedObjectContext*)moc;
- (UIImage*) getImageFromPhotoLib : (NSString*)url urlDate: (NSString*)date;
- (CGSize) getThumbnailSize;
- (void) removeSelectedImageNotInImageURLs:(NSMutableArray*)photoURLs;
- (NSSet*) getUnsendIssueReport;
- (void) saveIssueReport: (NSString*)devName : (double)date : (NSString*)title : (NSString*)descript;
- (void) printSelectedImage;
- (void) printThumbnail;
- (SEDimView*) getDimView;
- (void) removeDimView;
- (void) setDimViewBrightness: (float)alpha;
- (void) determineSelectedMusicRow;
- (BOOL) addMusicToSelectedView: (NSArray*)item;
- (SignaturePointData) getSignaturePoints:(NSNumber*)seq;
- (SignaturePointData) getSignaturePointsWithSeqName:(NSString*)name;
- (Signature*) getSignature: (NSNumber*)seq;
- (SignaturePointData) getCurrentSignaturePoints;
- (void)displayNextImage;
- (void) setSelectedImageProperty: (NSString*)url urlDate: (NSString*)urlDate orientation: (int) orient image:(UIImage*)image;
- (void) notificationShow;
- (void) notificationHide;
- (UIView*) getNotificationView;
- (void) finishShareOneImage;
- (void) finishOneImageDrawing;
- (void) finishOneMusicPlay;
- (void) finishOneComment;
- (void) getMedalPercent: (int) achieveType : (int)medal : (float*)outPercent : (float*)outPercentArray;
- (float) getCurrentLevelPercent;
- (SEUserData*)getUserData: (int) level;
- (int) getMaxUserLevel;
- (void) clearUserUpgrade;
- (UIView*) getUserUpgradeView;
- (UIView*) getLogView;
- (void) addLog: (NSString*)text;
- (void) showLogView;
- (void) hideLogView;
- (BOOL) isLogViewHide;
- (void) playMusicWithTitle: (NSString*)title artist: (NSString*) artist album: (NSString*)album;
- (void) pauseMusic;
//- (void) popupMusicOperationView;
- (void) removeCurrentSelectMusic;
- (void) drawSignatureAnim;
- (void) signatureAnimEnd;
- (void) showFirstNumView;
- (void) showSecondNumView;
- (void) setFirstViewNum: (int) n;
- (void) setSecondViewNum: (int)n;
- (void) showLoadingView;
- (void) showLoadingProgressView;
- (void) showLoadingTextView;
- (void) hideLoadingView;
- (SELoadingView*) getLoadingView;
- (void) setLoadingViewStage: (NSNumber*) stage;
- (int) getLoadingStage;
- (void) showMusicImageAttachInputView;
- (void) hideMusicImageAttachInputView;
- (SEMusicImageListPopupView*) getMusicImageListPopupView;
- (void) createMusicImageAttachInputView;
- (void) createConfirmDlg: (id) target ok: (SEL)okOp cancel : (SEL) cancelOp;
- (void) createConfirmDlg: (id) target ok: (SEL)okOp;
- (void) showConfirmDlg;
- (void) dismissConfirmDlg;
- (SEConfirmDlg*) getConfirmDlg;
- (NSArray*) fetchDefaultSelectedImage;
- (void) popupOperationViewGroup: (SEOperationViewGroup*) operatoinViewGroup;
- (BOOL) intersectWithOperationView;
- (void) disappearOperationViewGroup;
- (void) resetSelectedMusicViewCollisionedRow;
- (CGRect) getSignatureViewRect;
- (int) getRemainingImageCount;
- (int) getRemainingMusicCount;
- (void) startUpgradeTimer;
- (void) setSelectedMusicNumToMusicPicker : (NSString*) numStr;
- (void) setSelectedMusicNumToSelectedMusicView: (NSString*)numStr;
- (MPMediaItem*) findMediaItemInMusicPicker: (SEMusicItemProperty*)item;
- (int) getAllUpgradeInfoCount;
- (UpgradeInfo*) getUpgradeInfoBySeq:(int)seq;
//return is array of UpgradeInfo
- (NSArray*) getUpgradeInfoArray;
// invalid level value is -1
// invalid achievementType is INVALID_ACHIEVE
// invalid medal is INVALID_MEDAL_LEVEL
- (void) addUpgradeInfoToCoreData: (int)fromLevel toLevel : (int)toLevel achievementType: (int)achieveType medal: (int)medal;
- (NSString*) getAchievementDescription: (int)achieveType : (int)medal;
- (void) removeUpgradeInfo;
- (SEUserUpgrade*) getUserUpgrade;
- (void) showUserUpgradeView;
- (void) hideUserUpgradeView;
- (void) handleBrushBuied: (NSArray*)nameArray;
- (void) handleTimeBuied: (NSArray*)nameArray;
- (void) showWebView;
- (UIWebView*) getWebView;
- (void) dismissWebView;
- (SEDialogView*)createDialogView;
- (void) showDialogView;
- (SEDialogView*) getDialogView;
- (void) dismissDialogView;
- (void) showDrawingWaitingView;
- (void) hideDrawingWaitingView;
- (int) getDrawingLoadingStage;
- (void) setDrawingLoaingStage: (NSNumber*)i;
- (void) restoreFromPreview3D;
- (void) playDrawFrameShowAnim;
- (void) playDrawFrameHideAnim;
- (void) calculateUpgradeInfo;
- (DRAW_IMAGE_STATE) getDrawImageState;
- (void) setDrawImageState: (DRAW_IMAGE_STATE)state;
- (void) computeEnd;
- (void) drawFinished;
- (BOOL) isInPlayState;
- (void) playMusicByState;
- (void) resumeMusicByState;
- (void) stopMusicByState;
- (void) pauseMusicByState;
- (void) showIndication: (NSString*)str justInMainDisplay: (BOOL) bInMainDisplay time: (int) seconds;
- (void) hideIndication;
- (void) setBrushRandom: (NSNumber*)bOk;
- (BOOL) isBrushRandom;
- (void) pauseMusicInMusicPicker: (UIView*)currentView;
- (void) updateImageInSelectedImageView;
- (void) updateImageInImagePickerView;
- (void) updateMusicInSelectedImageView;
- (void) updateMusicInMusicPickerView;
- (BOOL) isInMainDisplay;
- (CGRect) getSignatureViewRect;
- (CGImageRef) createSignatureImageWithPoints: (NSArray*)points colors: (NSArray*)colors;
- (void) sendComment: (NSString*)comment label: (UILabel*) label;
- (void) restoreProduct;
//b : true for play icon , false for pause icon
- (void) changePlayPauseIcon: (BOOL)b;
//if the image has remove from image lib, it must be removed from selected image
- (void) syncSelectedImageWithImageLib;
//for test
- (void) downloadParamConfig : (id) label textName:(NSString*)textName;
- (void) popupUpgradeInfoTest;
- (void) dismissPopupTest;
- (BOOL) isShouldRotate;
- (void) setShouldRotate: (BOOL) b;
- (void) testSuite;
//obsolete
- (void) shareImage;
- (void) addShareImage : (UIImage*) image;
@end
