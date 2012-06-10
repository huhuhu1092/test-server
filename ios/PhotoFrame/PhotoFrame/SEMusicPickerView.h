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
@interface SEMusicItemProperty : NSObject {
@private
    NSString* title;
    NSString* artist;
    NSString* album;
    MPMediaItem* song;
    NSNumber* seq;
}
@property (nonatomic, retain) MPMediaItem* song;
@property (nonatomic, retain) NSNumber* seq;
@property (nonatomic, retain) NSString* title;
@property (nonatomic, retain) NSString* artist;
@property (nonatomic, retain) NSString* album;
@end


//////////////////////
@class SEUITableView;
@class SEViewNavigator;
@class MPMusicPlayerController;
@class SESelectedMusicView;
////////////////

@interface SEMusicPickerView : SEFixedView <UITableViewDelegate, UITableViewDataSource> 
{
@private
    
    UIImage* mMusicPickerBackgroundImage;
    UITableView* mMusicPicker;
    SEViewNavigator* mViewNav;
    int mSortType;
    NSMutableArray* mMusicItemArray;
    UITableViewCell* mSelectedTableViewCell;
    int mMusicItemCount;
    int mCurrentMusicItemIndex;
    SESelectedMusicView* mSelectedMusicView;
    int mTableCellType;
    CGRect mOrigRect;
    SEMusicItemProperty* mSelectedMusciItem;
    UIImage* mMusicCellBackgroundImage;
}
@property (nonatomic, assign) SESelectedMusicView* mSelectedMusicView;
@property (nonatomic, readonly) NSMutableArray* musicItemArray;
@property (nonatomic, assign) SEViewNavigator* mViewNav;
- (void) initData;
- (void) handleWhenStop: (BOOL) bStopInMid;
@end
////////////
@interface SESelectedMusicView : SEFixedView <UITableViewDelegate, UITableViewDataSource>
{
@private
    UIImage* mSelectedMusicViewBackgroundImage;
    UITableView* mSelectedTableView;
    SEViewNavigator* mViewNav;
    NSMutableArray* mSelectedMusicItemArray;
    int mSortType;
    int mHighlightedRow;
    int mTableCellType;
    CGRect mOrigRect;
    UIImage* mSelectedCellBackgroundImage;
}
@property (nonatomic, readonly) NSArray* musicItemArray;
@property (nonatomic, assign) SEViewNavigator* mViewNav;
- (void) initData;
- (void) addNewMusic: (SEMusicItemProperty*)item;
- (void) handleWhenStop: (BOOL) bStopInMid;
- (void) determineSelectedMusicRow: (UIView*)floatView;
@end
