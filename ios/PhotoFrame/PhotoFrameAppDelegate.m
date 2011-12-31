//
//  PhotoFrameAppDelegate.m
//  PhotoFrame
//
//  Created by 陈勇 on 11-10-26.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import "PhotoFrameAppDelegate.h"
#import "ImageDisplayViewController.h"
#import "PhotoFrameSettingController.h"
#import "PainterParameterController.h"
#import "BrushTableController.h"
#import "PainterParameterPickerController.h"
#import "PhotoFrame3DViewController.h"
#import "PainterManager.h"
@implementation PhotoFrameAppDelegate
//@synthesize imageViewController;
@synthesize window = _window;
- (void) setParamIDArray: (NSArray*) paramArray
{
    if(paramIDArray)
        [paramIDArray release];
    paramIDArray = paramArray;
    [paramIDArray retain];
}
- (NSArray*) paramIDArray
{
    return paramIDArray;
}

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions
{
    // Override point for customization after application launch.
    [[UIApplication sharedApplication] setStatusBarHidden:YES];
    imageViewController = [[ImageDisplayViewController alloc] initWithNibName:@"ImageViewDisplay" bundle:nil];
    imageViewController.appDelegate = self;
    self.window.rootViewController = imageViewController;
    [self.window makeKeyAndVisible];
    return YES;
}

- (void)applicationWillResignActive:(UIApplication *)application
{
    /*
     Sent when the application is about to move from active to inactive state. This can occur for certain types of temporary interruptions (such as an incoming phone call or SMS message) or when the user quits the application and it begins the transition to the background state.
     Use this method to pause ongoing tasks, disable timers, and throttle down OpenGL ES frame rates. Games should use this method to pause the game.
     */
}

- (void)applicationDidEnterBackground:(UIApplication *)application
{
    /*
     Use this method to release shared resources, save user data, invalidate timers, and store enough application state information to restore your application to its current state in case it is terminated later. 
     If your application supports background execution, this method is called instead of applicationWillTerminate: when the user quits.
     */
}

- (void)applicationWillEnterForeground:(UIApplication *)application
{
    /*
     Called as part of the transition from the background to the inactive state; here you can undo many of the changes made on entering the background.
     */
}

- (void)applicationDidBecomeActive:(UIApplication *)application
{
    /*
     Restart any tasks that were paused (or not yet started) while the application was inactive. If the application was previously in the background, optionally refresh the user interface.
     */
}

- (void)applicationWillTerminate:(UIApplication *)application
{
    /*
     Called when the application is about to terminate.
     Save data if appropriate.
     See also applicationDidEnterBackground:.
     */
}

- (void)dealloc
{
    [imageViewController release];
    [photoFrameSettingController release];
    [paramIDArray release];
    [_window release];
    [super dealloc];
}
- (void) showPhotoFrameSettingController
{
    if(!photoFrameSettingController)
    {
        photoFrameSettingController = [[PhotoFrameSettingController alloc] initWithNibName:@"PhotoFrameSetting" bundle:nil];
        photoFrameSettingController.appDelegate = self;
    }
}
- (void) showPainterParameterController
{
    if(!painterParameterController)
    {
        painterParameterController = [[PainterParameterController alloc] initWithNibName: @"PainterParameter" bundle: nil];
        painterParameterController.appDelegate  = self;
    }
}
- (void) showImageDisplayController
{

}
- (void) showBrushTableController
{
    if(!brushTableController)
    {
        brushTableController = [[BrushTableController alloc] initWithNibName: @"BrushTable" bundle: nil];
        brushTableController.appDelegate = self;
    }

}
- (void) showPhotoFrame3DController
{
    if(!photoFrame3DController)
    {
        photoFrame3DController = [[PhotoFrame3DViewController alloc] initWithNibName:@"PhotoFrame3D" bundle:nil];
        photoFrame3DController.appDelegate = self;
    }
}
- (void) showPainterParameterPicker
{
    if(!painterParameterPicker)
    {
        painterParameterPicker = [[PainterParameterPickerController alloc] initWithNibName:@"ParamType" bundle:nil];
        painterParameterPicker.appDelegate = self;
    }
}

- (void) showView : (NSString*) viewName
{
    CGRect frame = [[UIScreen mainScreen] applicationFrame];
    if([viewName isEqualToString:@"setting"])
    {
        photoFrameSettingController.view.frame = frame;
        [self.window addSubview:photoFrameSettingController.view];
    }
    else if([viewName isEqualToString:@"param"])
    {
        painterParameterController.view.frame = frame;
        [self.window addSubview:painterParameterController.view];
    }
    else if([viewName isEqualToString:@"ppm"])
    {
        brushTableController.view.frame = frame;
        [self.window addSubview:brushTableController.view];
    }
    else if([viewName isEqualToString:@"picker"])
    {
        painterParameterPicker.view.frame = frame;
        [self.window addSubview:painterParameterPicker.view];
    }
    else if([viewName isEqualToString:@"3D"])
    {
        photoFrame3DController.view.frame = frame;
        [self.window addSubview:photoFrame3DController.view];
    }
}
- (void) hideView : (NSString*) viewName
{
    
    if([viewName isEqualToString:@"setting"])
    {
        
        [photoFrameSettingController.view removeFromSuperview];
    }
    else if([viewName isEqualToString:@"param"])
    {
        
        [painterParameterController.view removeFromSuperview];
    }
    else if([viewName isEqualToString:@"ppm"])
    {
        [brushTableController.view removeFromSuperview];
    }
    else if([viewName isEqualToString:@"picker"])
    {
        [painterParameterPicker.view removeFromSuperview];
    }
    else if([viewName isEqualToString:@"3D"])
    {
        [photoFrame3DController.view removeFromSuperview];
    }
}
- (PainterParameterController*) painterParameterController
{
    return painterParameterController;
}
- (BrushTableController*) brushTableController
{
    return brushTableController;
}
- (PhotoFrameSettingController*) photoFrameSettingController
{
    return photoFrameSettingController;
}
- (PainterParameterPickerController*) painterParameterPicker
{
    return painterParameterPicker;
}
- (PhotoFrame3DViewController*) photoFrame3D
{
    return photoFrame3DController;
}
- (void)setImageView:(UIImageView*)imageView withPPMName:(NSString*)name
{
    ppm_t ppm = {0, 0, NULL};
    ppm_load([name cStringUsingEncoding:NSASCIIStringEncoding], &ppm);
    CGImageRef imageRef = [PainterManager createCGImageWithCopy:ppm];
    UIImage* image = [UIImage imageWithCGImage:imageRef];
    CGImageRelease(imageRef);
    imageView.image = image;
    ppm_kill(&ppm);
}
@end
