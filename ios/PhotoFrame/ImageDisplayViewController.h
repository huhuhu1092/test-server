//
//  ImageDisplayViewController.h
//  PhotoFrame
//
//  Created by 陈勇 on 11-10-26.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
@class SSImageLoader;
@class PainterManager;
@class PHImageView;
@interface ImageDisplayViewController : UIViewController
{
    PHImageView* imageView;
    UIButton* displayButton;
    UIButton* settingButton;
    UIButton* imageSelectButton;
    SSImageLoader* imageLoader;
    PainterManager* painterManager;
@private
    id appDelegate;
    NSString* imageName;
}
@property (nonatomic, retain) IBOutlet PHImageView* imageView;
@property (nonatomic, retain) IBOutlet UIButton* displayButton;
@property (nonatomic, retain) IBOutlet UIButton* settingButton;
@property (nonatomic, retain) IBOutlet UIButton* imageSelectButton;
@property (nonatomic, assign) id appDelegate;
@property (nonatomic, retain) NSString* imageName;
- (void) displayImage:(UIImage*)im;
- (IBAction) handleImagePress:(id)sender;
- (IBAction)imageSelectPress:(id)sender;
- (IBAction)displayPress:(id)sender;
- (IBAction)show3D:(id)sender;
- (void) setCurrentPPM: (NSString*)brushName;
- (void) setDisplayButtonEnable;
- (void) hideSettingUI;
- (void) showSettingUI;
@end
