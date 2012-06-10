//
//  SEOperationView.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-4-8.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "SEProtocalDefine.h"
enum OPERATION_TYPE {SHARE_OP, DELETE_OP, OPERATION_NUM};
enum NOTIFICATION_TYPE {INFO_NOTIFY, OPERATE_NOTIFY, NOTIFICATION_NUM};
@class SEOperationView;
@class SEViewNavigator;
@protocol SEOperationHandler <NSObject>

- (void) handleOperation: (OPERATION_TYPE)op view:(SEOperationView*)opView;
- (void) handleNotify: (NOTIFICATION_TYPE) notify view: (SEOperationView*)notifyView;

@end
@interface SEOperationView : UIView
{
    UIImageView* mBackgroundView;
    UIImageView* mForegroundView;
    UIImage* mBgImage;
    UIImage* mForeImage;
    BOOL mHighlighted;
    OPERATION_TYPE mType;
    SEViewNavigator* mViewNav;
}
@property (nonatomic, assign) SEViewNavigator* mViewNav;
@property (nonatomic, retain) UIImage* mBgImage;
@property (nonatomic, retain) UIImage* mForeImage;
@property (nonatomic, assign) BOOL highlighted;
@property (nonatomic, assign) OPERATION_TYPE mType;
- (CGRect) calculateFrame;
- (void)initData;
@end

@interface SEOperationViewGroup : UIView {
@private
    SEOperationView* mOperationViews[OPERATION_NUM];
    id <SEOperationHandler> mOperationHandler;
    SEViewNavigator* mViewNav;
}
@property (nonatomic, assign) id <SEOperationHandler> mOperationHandler;
@property (nonatomic, assign) SEViewNavigator* mViewNav;
- (CGRect) calculateFrame;
- (void) initData;
//rect is the coordinate in SEOperationViewGroup reference frame
- (void) intersectOperationView: (CGRect) rect;
- (void) operate;
@end

@interface SENotificationViewGroup : UIView{
@private
    SEOperationView* mOperationView[NOTIFICATION_NUM];
    id <SEOperationHandler> mOperationHandler;
    SEViewNavigator* mViewNav;
}
@property (nonatomic, assign) id <SEOperationHandler> mOperationHandler;
@property (nonatomic, assign) SEViewNavigator* mViewNav;
- (CGRect) calculateFrame;
- (void) initData;
@end
