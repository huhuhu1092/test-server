//
//  SEMusicImageListPopup.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-3-8.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEMusicImageListPopup.h"

@implementation SEMusicImageListPopup
@synthesize title;
@synthesize content;
@synthesize errorMsg;
@synthesize okButton;
@synthesize cancelButton;
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


// Implement loadView to create a view hierarchy programmatically, without using a nib.
- (void)loadView
{
    UIView* view = [[[NSBundle mainBundle] loadNibNamed:@"MusicImageListPopup" owner:self options:nil] lastObject];
    self.view = view;
    title = (UILabel*)[view viewWithTag:104];
    content = (UITextField*)[view viewWithTag:101];
    okButton = (UIButton*)[view viewWithTag:102];
    cancelButton = (UIButton*)[view viewWithTag:103];
    errorMsg = (UILabel*)[view viewWithTag:105];
}


/*
// Implement viewDidLoad to do additional setup after loading the view, typically from a nib.
- (void)viewDidLoad
{
    [super viewDidLoad];
}
*/

- (void)viewDidUnload
{
    [super viewDidUnload];
    // Release any retained subviews of the main view.
    // e.g. self.myOutlet = nil;
}

- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    // Return YES for supported orientations
	return YES;
}

@end
