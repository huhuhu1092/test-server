//
//  SEPopupView.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-4-12.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface SEPopupView : UIView
{
@private
    UIView* mContentView;
    BOOL isShow;
}
@property (nonatomic, retain) UIView* mContentView;
- (void)showAt: (CGPoint)p parent: (UIView*)parent;
- (void)dismiss;
@end
