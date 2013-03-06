//
//  SEOperationView.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-4-8.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "SEProtocalDefine.h"
//enum OPERATION_TYPE {SHARE_OP, DELETE_OP, OPERATION_NUM};
//enum NOTIFICATION_TYPE {INFO_NOTIFY, OPERATE_NOTIFY, NOTIFICATION_NUM};
@class SEOperationView;
@class SEViewNavigator;
@protocol SEOperationHandler <NSObject>

- (void) handleOperation: (NSString*)op view:(SEOperationView*)opView;
//- (void) handleNotify: (NOTIFICATION_TYPE) notify view: (SEOperationView*)notifyView;

@end
@interface SEOperationView : UIView
{
    UIImageView* mBackgroundView;
    UIImageView* mForegroundView;
    UIImage* mBgImage;
    UIImage* mForeImage;
    BOOL mHighlighted;
    NSString* mType;
    SEViewNavigator* mViewNav;
    NSString* foregroundImageNormalStr;
    NSString* backgroundImageNormalStr;
    NSString* foregroundImageHighlightedStr;
    NSString* backgroundImageHighlightedStr;
}
@property (nonatomic, retain)  NSString* foregroundImageNormalStr;
@property (nonatomic, retain)  NSString* backgroundImageNormalStr;
@property (nonatomic, retain)  NSString* foregroundImageHighlightedStr;
@property (nonatomic, retain)  NSString* backgroundImageHighlightedStr;
@property (nonatomic, assign) SEViewNavigator* mViewNav;
@property (nonatomic, retain) UIImage* mBgImage;
@property (nonatomic, retain) UIImage* mForeImage;
@property (nonatomic, assign) BOOL highlighted;
@property (nonatomic, retain) NSString* mType;
- (CGRect) calculateFrame;
@end

@interface SEOperationViewGroup : UIView {
@private
    //SEOperationView* mOperationViews[OPERATION_NUM];
    id <SEOperationHandler> mOperationHandler;
    SEViewNavigator* mViewNav;
    NSMutableArray* mOperators;
    NSMutableArray* mOperationViews;
    NSMutableArray* mImageResources;
}
@property (nonatomic, retain) id <SEOperationHandler> mOperationHandler;
//@property (nonatomic, assign) SEViewNavigator* mViewNav;
//- (CGRect) calculateFrame;
//- (void) initData;
// the resources's element is also an four element array , first is normal background image and second is normal foreground image, the third is highlighted background image the forth is highlighted foreground image
- (id) initWithOperators:(NSArray*) operators withImageResource: (NSArray*) resources;
//rect is the coordinate in SEOperationViewGroup reference frame
- (BOOL) intersectOperationView: (CGRect) rect;
- (void) operate;
@end
/*
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
*/