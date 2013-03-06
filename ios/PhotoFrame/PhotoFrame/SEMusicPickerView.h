//
//  SEMusicPickerView.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-2-27.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "SEProtocalDefine.h"
#import "SEFixedView.h"
#import "SEUITableView.h"
@class SEResLoader;
@class MPMediaItem;
enum {TABLE_CELL_BIG, TABLE_CELL_SMALL};
enum MUSIC_PLAY_STATE {MUSIC_STOP, MUSIC_PLAY, MUSIC_PAUSE};
@interface SEMusicItemProperty : NSObject
{
@private
    NSString* title;
    NSString* artist;
    NSString* album;
    MPMediaItem* song;
    NSNumber* seq;
    BOOL highlighted;
    MUSIC_PLAY_STATE musicPlayState;
}
@property (nonatomic, retain) MPMediaItem* song;
@property (nonatomic, retain) NSNumber* seq;
@property (nonatomic, retain) NSString* title;
@property (nonatomic, retain) NSString* artist;
@property (nonatomic, retain) NSString* album;
@property (nonatomic, assign) BOOL highlighted;
@property (nonatomic, assign) MUSIC_PLAY_STATE musicPlayState;
- (BOOL) isEqualToMusicItemProperty: (SEMusicItemProperty*) item;
- (SEMusicItemProperty*) clone;
@end


//////////////////////
@class SEUITableView;
@class SEViewNavigator;
@class MPMusicPlayerController;
@class SESelectedMusicView;
////////////////
@interface SESelectedMusicItem : NSObject
{
    SEMusicItemProperty* item;
    int row;
}
@property (nonatomic , retain) SEMusicItemProperty* item;
@property (nonatomic, assign) int row;
@end
//////////////
enum MUSIC_RESOURCE_TYPE {MUSIC_RESOURCE_LIB, MUSIC_RESOURCE_CORE_DATA};
@interface SEMusicSelectedImpl : NSObject <UITableViewDelegate, UITableViewDataSource>
{
@private
    MUSIC_RESOURCE_TYPE mMusicResourceType;
    UITableView* mMusicTableView;
    UIButton* mTitleButton;
    UIButton* mArtistButton;
    UIButton* mAlbumButton;
    SEViewNavigator* mViewNav;
    int mSortType;
    NSMutableArray* mMusicItemArray;//this is the data for every table view
    int mMusicItemCount;
    SESelectedMusicView* mSelectedMusicView;
    int mTableCellType;
    CGRect mOrigRect;
    NSMutableArray* mSelectedMusicItemArray;
    //SEMusicItemProperty* mCurrentLongPressItem;
    NSString* mMusicCellBgStr;
    NSString* mMusicCellHBgStr;
    int mCollisionedRow;
    
    UITableViewCell* mCurrentHighlightedCell;
    CGRect mCurrentHighlightedCellRect;
    UIImage* mNormalCellBg;
    UIImage* mHighlightedCellBg;
    UIImage* mNullCellBg;
    SEFixedView* mParent;
    
    int mTableViewItemCount;
    
    int mCurrentPlayItem;
    //SEMusicItemProperty* mCurrentPlayMusicItem;
}
@property (nonatomic, assign) SEFixedView* mParent;
@property (nonatomic, assign) UITableView* mMusicTableView;
@property (nonatomic, assign) UIButton* mTitleButton;
@property (nonatomic, assign) UIButton* mArtistButton;
@property (nonatomic, assign) UIButton* mAlbumButton;
@property (nonatomic, assign) SEViewNavigator* mViewNav;
@property (nonatomic, retain) NSString* mMusicCellBgStr;
@property (nonatomic, retain) NSString* mMusicCellHBgStr;
@property (nonatomic, assign) MUSIC_RESOURCE_TYPE mMusicResourceType;
@property (nonatomic, assign) int mTableCellType;
@property (nonatomic, retain) NSMutableArray* mSelectedMusicItemArray;
@property (nonatomic, assign) SESelectedMusicView* mSelectedMusicView;
@property (nonatomic, assign) CGRect mOrigRect;
- (void) initData;
- (BOOL) addNewMusic: (NSArray*)item;
- (void) determineSelectedMusicRow: (UIView*)floatView;
- (void) resetCollisoinedRowRect;
- (int) getHighlightedViewNum;
- (MPMediaItem*)findMediaItemByItemProperty: (SEMusicItemProperty*)item;
- (void) pauseAllMusic;
@end
//////////
@interface SEMusicPickerView : SEFixedView 
{
@private
    SEMusicSelectedImpl* mMusicSelectedImpl;
    SEViewNavigator* mViewNav;
}

@property (nonatomic, assign) SEViewNavigator* mViewNav;
- (void) initData;
- (void) handleWhenStop: (BOOL) bStopInMid;
- (MPMediaItem*)findMediaItemByItemProperty: (SEMusicItemProperty*)item;
- (void) pauseMusic;
- (void) updateMusicView;
//- (UIImageView*) getFrameImageView: (UITableViewCell*)cell;
@end
////////////
@interface SESelectedMusicView : SEFixedView
{
@private
    SEMusicSelectedImpl* mMusicSelectedImpl;
    SEViewNavigator* mViewNav;
}
@property (nonatomic, assign) SEViewNavigator* mViewNav;
- (void) initData;
- (BOOL) addNewMusic: (NSArray*)items;
- (void) handleWhenStop: (BOOL) bStopInMid;
- (void) determineSelectedMusicRow: (UIView*)floatView;
- (void)removeSelectedMusic;
- (void) resetCollisoinedRowRect;
- (void) pauseMusic;
- (void) updateMusicView;
//- (void) removeSelectedMusic: (NSArray*) items;
//- (void) removeCurrentSelectedMusic;
//- (void) resetHighlighted;
@end
