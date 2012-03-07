//
//  SESignaturePreview.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-3-4.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "SEFixedView.h"
@class SEViewNavigator;
@interface SESignaturePreview : SEFixedView <UIPickerViewDataSource, UIPickerViewDelegate>
{
    SEViewNavigator* mViewNav;
@private
    NSArray* mSiteString;
    NSArray* mSizeString;
    int mCurrentSize;
    int mCurrentSite;
}
@property (nonatomic, assign) SEViewNavigator* mViewNav;
@property (nonatomic, assign) int mCurrentSize;
@property (nonatomic, assign) int mCurrentSite;
- (void) initData;
- (void) handleChange:(id)sender;
@end
