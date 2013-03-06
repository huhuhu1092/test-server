//
//  SESignaturePopupViewController.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-3-2.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SESignaturePopupViewController.h"

@implementation SESignaturePopupViewController
@synthesize mSignatureView;
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
    
    UIView* view  = [[[NSBundle mainBundle] loadNibNamed:@"SignatureSavePopup" owner:self options:nil] lastObject];
    mTextField = (UITextField*)[view viewWithTag:101];
    mOkButton = (UIButton*)[view viewWithTag:102];
    mCancelButton = (UIButton*)[view viewWithTag:103];
    [mOkButton addTarget:mSignatureView action:@selector(realAdd:) forControlEvents:UIControlEventTouchUpInside];
    [mCancelButton addTarget:mSignatureView action:@selector(cancel:) forControlEvents:UIControlEventTouchUpInside];
    
    self.view = view;
}

- (NSString*) textString
{
    return mTextField.text;
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
