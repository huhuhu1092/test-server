//
//  SESignaturePopupViewController.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-3-2.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
@class SESignatureView;
@interface SESignaturePopupViewController : UIViewController
{
    SESignatureView* mSignatureView;
    UITextField* mTextField;
    UIButton* mOkButton;
    UIButton* mCancelButton;
}
@property (nonatomic, assign) SESignatureView* mSignatureView;
- (NSString*) textString;
@end
