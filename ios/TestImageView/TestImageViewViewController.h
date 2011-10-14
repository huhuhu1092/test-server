//
//  TestImageViewViewController.h
//  TestImageView
//
//  Created by 陈勇 on 11-9-27.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
@class SSImageLoader;
@class ParameterSetMainUI;
@class PainterManager;
@interface TestImageViewViewController : UIViewController
{
    UILabel* infoNotification;
    UIImageView* imageView;
    SSImageLoader* imageLoader;
    ParameterSetMainUI* secondViewOwner;
    PainterManager* painterManager;
@private
    UIScrollView* scrollView;
}
@property (nonatomic, retain) IBOutlet UILabel* infoNotification;
@property (nonatomic, retain) IBOutlet UIImageView* imageView;
- (void)displayImage:(UIImage*)image;
- (IBAction)setButtonPressed:(id)sender;
@end
