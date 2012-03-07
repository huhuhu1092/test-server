//
//  ImageDisplayViewController.m
//  PhotoFrame
//
//  Created by 陈勇 on 11-10-26.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import "ImageDisplayViewController.h"
#import "SSImageLoader.h"
#import "PainterManager.h"
#import "BrushTableController.h"
#import "PHImageView.h"
@implementation ImageDisplayViewController
@synthesize imageView;
@synthesize appDelegate;
@synthesize imageName;
@synthesize displayButton;
@synthesize settingButton;
@synthesize imageSelectButton;
- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)didReceiveMemoryWarning
{
    // Releases the view if it doesn't have a superview.
    [super didReceiveMemoryWarning];
    
    // Release any cached data, images, etc that aren't in use.
}

#pragma mark - View lifecycle

/*
// Implement loadView to create a view hierarchy programmatically, without using a nib.
- (void)loadView
{
}
*/

- (void) setSystemParam
{
    CGRect r = [[UIScreen mainScreen] applicationFrame];
    NSLog(@"## screen width = %f, height = %f", r.size.width, r.size.height);
    CGSize fitSize = [PainterManager computeFitSize:CGSizeMake(2560, 1920) toDst:CGSizeMake(1024, 768)];
    NSLog(@"## fitsize = %f, %f",fitSize.width, fitSize.height);
}
// Implement viewDidLoad to do additional setup after loading the view, typically from a nib.
- (void)viewDidLoad
{
    [super viewDidLoad];
    UIImage* image = [UIImage imageNamed:@"mainbackground_002.jpg"];
    //imageView.image = image;
    imageLoader = [[SSImageLoader alloc] init];
    painterManager = [PainterManager painterManager];
    painterManager.bgWidth = 1024;
    painterManager.bgHeight =768;
    imageLoader.setImageCallback = self;
    NSArray* paramArray = [painterManager painterParamsByQuality:7 withTimes:5];
    [painterManager setCurrentPainterParamID:paramArray];
    for(NSUInteger i = 0 ; i < [paramArray count]; i++)
    {
        NSString* s = [paramArray objectAtIndex:i];
        NSLog(@"## s = %@ ", s);
    }
    imageView.image = image;
    //painterManager.imageName = @"test3.png";
    //painterManager.imageName = @"paintbrush.jpg";
    //painterManager.imageName = @"test2.jpg";
    //painterManager.imageName = @"test.jpg";
    //[painterManager paintImage];
}


- (void)viewDidUnload
{
    [super viewDidUnload];
    // Release any retained subviews of the main view.
    // e.g. self.myOutlet = nil;
}
- (void)viewWillAppear:(BOOL)animated
{
    [self setSystemParam];
}
- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    // Return YES for supported orientations
    //return (interfaceOrientation == UIInterfaceOrientationPortrait);
    return (interfaceOrientation == UIInterfaceOrientationLandscapeLeft) || (interfaceOrientation == UIInterfaceOrientationLandscapeRight);
    //return YES;
}
- (void) displayImage:(UIImage*)im
{
    imageView.image = im;
}
- (void)dealloc
{
    [imageLoader release];
    [appDelegate release];
    [displayButton release];
    [super dealloc];
}
- (IBAction) handleImagePress:(id)sender
{
    /*
    PainterManager* painterManager = [PainterManager painterManager];
    painterManager.painterState.currentSeq++;
    [painterManager paintImage]; 
     */
    if([appDelegate respondsToSelector:@selector(showPhotoFrameSettingController)])
    {
        [appDelegate showPhotoFrameSettingController];
    }
    if([appDelegate respondsToSelector:@selector(showView:)])
    {
        [appDelegate showView:@"setting"];
    }
}
- (IBAction)imageSelectPress:(id)sender
{
    if([appDelegate respondsToSelector:@selector(showBrushTableController)])
    {
        [appDelegate showBrushTableController];
        NSArray* allImage = [[NSBundle mainBundle] pathsForResourcesOfType:@".jpg" inDirectory:nil];
        NSArray* ret = [[NSBundle mainBundle] pathsForResourcesOfType:@".png" inDirectory:nil];
        NSArray* ret1 = [[NSBundle mainBundle] pathsForResourcesOfType:@".JPG" inDirectory:nil];
        allImage = [allImage arrayByAddingObjectsFromArray:ret];
        allImage = [allImage arrayByAddingObjectsFromArray:ret1];
        int count = [allImage count];
        for(int i = 0 ; i < count ; i++)
        {
            NSLog(@"name = %@", [allImage objectAtIndex:i]);
        }
        BrushTableController* btc = [appDelegate brushTableController];
        btc.brushes = allImage;
        btc.parent = self;
        
        [btc.tableView reloadData];
    }
    if([appDelegate respondsToSelector:@selector(showView:)])
    {
        
        [appDelegate showView:@"ppm"];
    }         
}
- (IBAction)show3D:(id)sender
{
    [appDelegate showPhotoFrame3DController];
    [appDelegate showView:@"3D"];
}
- (void) setCurrentPPM: (NSString*)brushName
{
    self.imageName = brushName;
}
- (void) setDisplayButtonEnable
{
    displayButton.enabled = YES;
}
- (void) showSettingUI
{
    displayButton.hidden = NO;
    imageSelectButton.hidden = NO;
    settingButton.hidden = NO;
}
- (void) hideSettingUI
{
    displayButton.hidden = YES;
    imageSelectButton.hidden = YES;
    settingButton.hidden = YES;
}
- (IBAction)displayPress:(id)sender
{
    /*
    painterManager.imageName = imageName;
    if(imageName && ![imageName isEqualToString:@""])
    {
        painterManager.painterState.currentSeq = 0;
        [painterManager nextDisplayStage];
        //[painterManager setTimer];
        [painterManager paintImage];
        [self hideSettingUI];
        //displayButton.enabled = NO;
    }
     */
}
@end
