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
@interface SEMusicItemProperty : NSObject {
@private
    NSString* title;
    NSString* artist;
    NSString* album;
    NSNumber* seq;
}
@property (nonatomic, retain) NSNumber* seq;
@property (nonatomic, retain) NSString* title;
@property (nonatomic, retain) NSString* artist;
@property (nonatomic, retain) NSString* album;
@end


//////////////////////
@class SEMusicPickerView;
@class SEUITableView;
@class SEViewNavigator;
@interface SEMusicPickerDelegate : NSObject <UITableViewDelegate, UITableViewDataSource> 
{
    SEMusicPickerView* mMusicPickerView;
}
@property (nonatomic, assign) SEMusicPickerView* mMusicPickerView;
@end
//////

@interface SEMusicSelectedViewDelegate : NSObject <UITableViewDelegate, UITableViewDataSource> 
{
    SEMusicPickerView* mMusicView;
}
@property (nonatomic, assign) SEMusicPickerView* mMusicPickerView;
@end
//////
////////////////
@interface SEMusicPickerView : SEFixedView
{
    SEUITableView* mMusicPicker;
    SEUITableView* mSelectedMusicView;
    UITableViewCell* mSelectedTableViewCell;
@private
    SEMusicPickerDelegate* mMusicPickerDelegate;
    SEMusicSelectedViewDelegate* mMusicSelectedViewDelegate;
    int mMusicItemCount;
    NSMutableArray* mMusicItemArray;
    NSArray* mSelectedMusicItemArray;
    SEViewNavigator* mViewNav;
}
@property (nonatomic, readonly) NSArray* mSelectedMusicItemArray;
@property (nonatomic, assign) SEViewNavigator* mViewNav;
@property (nonatomic, readonly) int mMusicItemCount;
@property (nonatomic, retain) SEUITableView* mMusicPicker;
@property (nonatomic, retain) SEUITableView* mSelectedMusicView;
- (void) createSelectedTableViewCell: (NSValue*) frame;
- (void) touchBeginHandler: (UITableViewCell*) pressedView;
- (void) touchMoveHandler: (NSValue*) vDeltap;
- (void) touchEndHandler;
- (void) initMusicPicker;
- (int) getMusicItemPropertyCount;
- (int) getSelectedMusicItemProperyCount;
- (SEMusicItemProperty*)getMusicItemProperty: (int)index;
- (SEMusicItemProperty*)getSelectedMusicItemProperty: (int)index;
- (void)reloadPickerTableData;
@end
