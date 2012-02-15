//
//  SEUITableView.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-2-9.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
@interface SEUITableViewCell: UITableViewCell 
{
}
@end
///////////////////////////////////////////////
@interface SEUITableView : UITableView
{
    CGPoint mOrig;
    CGPoint mSavePoint;
    BOOL mHasLongPress;
    UITableViewCell* mPressedView;
    NSArray* mGesture;
@private
    id mLongPressTarget;
    SEL mLongPressAction;
    
    id mTouchMoveTarget;
    SEL mTouchMoveAction;
    
    id mTouchEndTarget;
    SEL mTouchEndAction;
}
- (void) longPressHandler: (UILongPressGestureRecognizer*)longPress;

- (void) setLongPressTarget: (id) obj withAction:(SEL)action;
- (void) setTouchMoveTarget: (id) obj withAction:(SEL) action;
- (void) setTouchEndTarget: (id) obj withAction:(SEL)action;
- (void) saveGesture;
- (void) restoreGesture;
- (void) removeAllGestures;
@end
