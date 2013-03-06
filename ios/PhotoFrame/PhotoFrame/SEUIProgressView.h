//
//  SEUIProgressView.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-7-9.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>

@class SEViewNavigator;
@interface SEUIProgressView : UIView
{
@private
    UIImageView* mBackgroundView;
    UIImageView* mForegroundView;
    UIImageView* mDefaultForegroundView;
    CGFloat percent;//value is 0 -- 1
    NSString* mForegroundImageStr;
    NSString* mBackgroundImageStr;
    NSMutableArray* mForegroundImageGroupStr;
    NSMutableArray* mGroupPercentArray;
    NSMutableArray* mGroupForgroundViewArray;
}
//@property (nonatomic, retain) UIImage* mBackground;
//@property (nonatomic, retain) UIImage* mForeground;
@property (nonatomic, retain) NSString* mForegroundImageStr;
@property (nonatomic, retain) NSString* mBackgroundImageStr;
@property (nonatomic, assign) CGFloat percent;
- (void) initData: (SEViewNavigator*)viewNav;
- (void) setForegroundImage: (UIImage*)image;
- (void) setForegroundGroupImageStr: (NSArray*) imageStrArray;
- (void) setGroupPercent: (NSArray*)percentArray;
- (NSArray*) getGroupPercent;
@end