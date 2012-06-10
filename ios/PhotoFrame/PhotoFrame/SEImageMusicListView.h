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
@protocol SEHandleIndicatorTouch <NSObject>
- (void) touchBegan: (CGPoint) p;
- (void) touchMove: (CGPoint) p;
- (void) touchEnd: (CGPoint)p;

@end
@interface SEIndicatorView : UIImageView 
{
@private
    NSArray* mSavedGesture;
    SEMusicImageListView* mTouchHandler;
    SEResLoader* mResLoader;
    int mType;
}
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
- (void) dehighlightImageCell: (NSIndexPath*)indexPath;
- (void) deselectImageList: (ImageList*)imageList;
- (void) selectImageCell: (NSIndexPath*)indexPath;
- (UIImageView*) tableViewCellBackground: (NSIndexPath*)indexPath;
- (void) deselectImageCell: (NSIndexPath*)indexPath;
- (void) highlightCell: (NSIndexPath*)indexPath;
@end
@interface SEMusicListTableView : UITableView <UITableViewDelegate, UITableViewDataSource, UIScrollViewDelegate>
{
    SEViewNavigator* mViewNav;
    SEResLoader* mResLoader;
    SEMusicImageListView* mMusicImageListView;
@private
    NSArray* mMusicListPropertyArray;
}
@property (nonatomic, assign) SEMusicImageListView* mMusicImageListView;
@property (nonatomic, assign) SEResLoader* mResLoader;
@property (nonatomic, retain) NSArray* mMusicListPropertyArray;
@property (nonatomic, assign) SEViewNavigator* mViewNav;
- (void) deselectPrevRow : (UITableView*)tableView;

- (void) selectMusicCellForImageList: (ImageList*)imageList;
- (void) deselectMusicCellForImageList: (ImageList*)imageList;
- (void) deselectHighlightedCell;
@end
@interface SEMusicImageListView : SEFixedView <UIAlertViewDelegate>
{
@private
    SEImageListTableView* mImageListTableView;
    SEMusicListTableView* mMusicListTableView;
    SEViewNavigator* mViewNav;
    UIImage* mBackground;
@private
    SEResLoader* mResLoader;
    CGPoint mOrig;
    UIImageView* mLineView;
    UIImageView* mBackgroundView;
    UIButton* mAddMusicListButton;
    UIButton* mAddImageListButton;
    UIButton* mRemoveButton;
    UIImage* mMusicListBackground;
    UIImage* mImageListBackground;
    CGPoint mImageIndicatorCenter;
    CGPoint mMusicIndicatorCenter;
    CGPoint mImageIndicatorPointInCell;
    CGPoint mMusicIndicatorPointInCell;
    CGFloat mImageCellHeight;
    CGFloat mMusicCellHeight;
    //for running state begin
    int mAlertType;
    UIPopoverController* mPopup;
    SEMusicImageListPopup* mDlg;
    NSString* mCurrentName;
    int mTableType;
    
    int mStartIndicatorRow;
    int mStartIndicatorTable;
    
    int mEndIndicatorRow;
    int mEndIndicatorTable;
    NSMutableArray* mLineViewArray;
    /// for running  state end
}
@property (nonatomic, readonly) SEMusicListTableView* mMusicListTableView;
@property (nonatomic, readonly) SEImageListTableView* mImageListTableView;
@property (nonatomic, assign) SEResLoader* mResLoader;
@property (nonatomic, retain) UIImage* mBackground;
@property (nonatomic, assign) SEViewNavigator* mViewNav;
//- (id)initWithFrame:(CGRect)frame fromViewNav: (SEViewNavigator*)viewNav withBackground:(UIImage*)image withResLoader: (SEResLoader*)resLoader;
- (void) initData;
- (void) touchBegan: (CGPoint) p;
- (void) touchMove: (CGPoint) p;
- (void) touchEnd: (CGPoint)p;
@end
