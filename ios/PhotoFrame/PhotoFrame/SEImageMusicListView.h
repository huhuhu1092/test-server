//
//  SEImageMusicListView.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-2-20.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "SEFixedView.h"
@class SEViewNavigator;
@class SEResLoader;
@class SEMusicImageListView;
@class SEMusicImageListPopup;
@class ImageList;
@class MusicList;
@class SEMusicImageListPopupView;
@protocol SEHandleIndicatorTouch <NSObject>
- (void) touchBegan: (CGPoint) p;
- (void) touchMove: (CGPoint) p;
- (void) touchEnd: (CGPoint)p;

@end
/////
@interface SELineView : UIImageView

@end
//////
@interface SEImageListURLProperty : NSObject 
{
    int currentIndex;
    NSMutableArray* selectedImageArray;
    NSString* name;
}
@property (nonatomic, retain) NSString* name;
@property (nonatomic, assign) int currentIndex;
@property (nonatomic, retain) NSMutableArray* selectedImageArray;
@end
///////
@interface SEPlayView : UIImageView
{
    int row;
    int currSeq;
    BOOL play;
}
@property (nonatomic, assign) BOOL play;
@property (nonatomic, assign) int currSeq;
@property (nonatomic, assign) int row;
@end
/////
@interface SEMusicItem : NSObject
{
    BOOL play;
    int currSeq;
    int row;
    NSString* musicListName;
}
@property (nonatomic, assign)BOOL play;
@property (nonatomic, assign) int currSeq;
@property (nonatomic, assign) int row;
@property (nonatomic, retain) NSString* musicListName;
@end
@interface SEIndicatorView : UIImageView 
{
@private
    NSArray* mSavedGesture;
    SEMusicImageListView* mTouchHandler;
    SEResLoader* mResLoader;
    int mType;
    int row;
}
@property (nonatomic, assign) int row;
@property (nonatomic, assign) int mType;
@property (nonatomic, assign) SEResLoader* mResLoader;
@property (nonatomic, assign) SEMusicImageListView* mTouchHandler;
@end
@interface SEImageListTableView : UITableView <UITableViewDelegate, UITableViewDataSource, UIScrollViewDelegate>
{
    SEViewNavigator* mViewNav;
    SEResLoader* mResLoader;
@private
    NSArray* mImageListPropertyArray;
    SEMusicImageListView* mMusicImageListView;
}

@property (nonatomic, assign) SEMusicImageListView* mMusicImageListView;
@property (nonatomic, assign) SEResLoader* mResLoader;
@property (nonatomic, retain) NSArray* mImageListPropertyArray;
@property (nonatomic, assign) SEViewNavigator* mViewNav;
- (void) deselectImageList: (ImageList*)imageList;
- (void) selectImageCell: (UITableViewCell*)cell;
- (void) deselectImageCell: (UITableViewCell*)cell;
- (void) updateIndicator;
@end

@interface SEMusicListTableView : UITableView <UITableViewDelegate, UITableViewDataSource, UIScrollViewDelegate>
{
    SEViewNavigator* mViewNav;
    SEResLoader* mResLoader;
    SEMusicImageListView* mMusicImageListView;
    NSMutableArray* mMusicItems;
@private
    NSArray* mMusicListPropertyArray;
}
@property (nonatomic, retain) NSMutableArray* mMusicItems;
@property (nonatomic, assign) SEMusicImageListView* mMusicImageListView;
@property (nonatomic, assign) SEResLoader* mResLoader;
@property (nonatomic, retain) NSArray* mMusicListPropertyArray;
@property (nonatomic, assign) SEViewNavigator* mViewNav;
- (void) deselectPrevRow : (UITableView*)tableView;
- (void) selectMusicCellForImageList: (ImageList*)imageList;
- (void) deselectHighlightedCell;
- (void) updateIndicator;
- (void) pauseMusic;
@end
@class SEPopupImageTextLabel;
@interface SEMusicImageListView : SEFixedView <UIAlertViewDelegate>
{
@private
    SEImageListTableView* mImageListTableView;
    SEMusicListTableView* mMusicListTableView;
    SEPopupImageTextLabel* mImageListLabel;
    SEPopupImageTextLabel* mMusicListLabel;
    SEViewNavigator* mViewNav;
    UIView* mClipLineView;
    UIImage* mBackground;
    //for test
    UIButton* mTestButton;
    UITextField* mTestTextField;
@private
    SEResLoader* mResLoader;
    CGPoint mOrig;
    NSMutableArray* mImageListURLPropertyArray;
    UIImageView* mLineView;
    UIImageView* mBackgroundView;
    //UIButton* mAddMusicListButton;
    //UIButton* mAddImageListButton;
    //UIButton* mRemoveButton;
    UIImage* mMusicListBackground;
    UIImage* mImageListBackground;
    CGPoint mImageIndicatorCenter;
    CGPoint mMusicIndicatorCenter;
    CGPoint mImageIndicatorPointInCell;
    CGPoint mMusicIndicatorPointInCell;
    CGFloat mImageCellHeight;
    CGFloat mMusicCellHeight;
    UIImage* mLineImage;
    UIImage* mLineImageNotSelected;
    //for running state begin
    int mAlertType;
    //UIPopoverController* mPopup;
    //SEMusicImageListPopup* mDlg;
    NSString* mCurrentName;
    int mTableType;
    int mNeedDeleteRow;
    
    int mStartIndicatorRow;
    int mStartIndicatorTable;
    
    int mEndIndicatorRow;
    int mEndIndicatorTable;
    NSMutableArray* mLineViewArray;// the displayed line for current image list and current music list and current draw line
    /// for running  state end
}
@property (nonatomic, assign) int mTableType;
@property (nonatomic, assign) int mNeedDeleteRow;
@property (nonatomic, retain) UIImage* mLineImage;
@property (nonatomic, retain) UIImage* mLineImageNotSelected;
@property (nonatomic, retain) NSMutableArray* mImageListURLPropertyArray;
@property (nonatomic, readonly) SEMusicListTableView* mMusicListTableView;
@property (nonatomic, readonly) SEImageListTableView* mImageListTableView;
@property (nonatomic, assign) SEResLoader* mResLoader;
@property (nonatomic, retain) UIImage* mBackground;
@property (nonatomic, assign) SEViewNavigator* mViewNav;
//- (id)initWithFrame:(CGRect)frame fromViewNav: (SEViewNavigator*)viewNav withBackground:(UIImage*)image withResLoader: (SEResLoader*)resLoader;
- (void) initData;
- (void) updateImageListURL;
- (int) getViewRow: (UIView*)v;
- (BOOL) touchBegan: (CGPoint) p;
- (BOOL) touchMove: (CGPoint) p;
- (BOOL) touchEnd: (CGPoint)p;
- (void) updateMusicListTableViewIndicator;
- (void) updateImageListTableViewIndicator;
- (void) pauseMusic;
@end
