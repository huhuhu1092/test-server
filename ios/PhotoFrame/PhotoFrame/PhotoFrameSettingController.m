//
//  PhotoFrameSetting.m
//  PhotoFrame
//
//  Created by 陈勇 on 11-11-1.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import "PhotoFrameSettingController.h"
#import "PainterManager.h"
#import "PainterParameterController.h"
#import "BrushTableController.h"
@implementation PhotoFrameSettingController
@synthesize percentSlider;
@synthesize timesSlider;
@synthesize appDelegate;
@synthesize percentLabel;
@synthesize timesLabel;
@synthesize paperImageView;
@synthesize paper;
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

- (void) viewWillAppear:(BOOL)animated
{
    PainterManager* pm = [PainterManager painterManager];
    timesLabel.text = [NSString stringWithFormat:@"%d", pm.painterProperty.times];
    percentLabel.text = [NSString stringWithFormat:@"%d", pm.painterProperty.percent];
    percentSlider.minimumValue = 5;
    percentSlider.maximumValue = 10;

    int maxTimes, minTimes;
    [pm getMinMaxTimesValue: pm.painterProperty.percent outMin:&minTimes outMax:&maxTimes];
    timesSlider.minimumValue = minTimes;
    timesSlider.maximumValue = maxTimes;
    percent = pm.painterProperty.percent;
    times = pm.painterProperty.times;
    paper = pm.painterProperty.paper;
    [percentSlider setValue:pm.painterProperty.percent];
    [timesSlider setValue:pm.painterProperty.times];
    if(paramIDArray)
        [paramIDArray release];
    NSArray* pa = [pm painterParamsByQuality:percent withTimes:times];
    paramIDArray = pa;
    [paramIDArray retain];
    if(paramArray)
        [paramArray release];
    paramArray = [[NSMutableArray alloc] initWithArray:[NSArray array]];
    NSUInteger i;
    for(i = 0 ; i < [paramIDArray count] ; i++)
    {
        NSString* sid = [paramIDArray objectAtIndex:i];
        PainterParam* p = [pm painterParam:sid];
        [paramArray addObject:p];
    }
    [appDelegate setImageView:paperImageView withPPMName:paper];
}
// Implement viewDidLoad to do additional setup after loading the view, typically from a nib.
- (void)viewDidLoad
{
    [super viewDidLoad];
}


- (void)viewDidUnload
{
    [super viewDidUnload];
    // Release any retained subviews of the main view.
    // e.g. self.myOutlet = nil;
}

- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    // Return YES for supported orientations
    //return (interfaceOrientation == UIInterfaceOrientationPortrait);
    return (interfaceOrientation == UIInterfaceOrientationLandscapeLeft) || (interfaceOrientation == UIInterfaceOrientationLandscapeRight);
}
- (void)dealloc
{
    [percentSlider release];
    [timesSlider release];
    [percentLabel release];
    [timesLabel release];
    [appDelegate release];
    [paramIDArray release];
    [paramArray release];
    [paperImageView release];
    [super dealloc];
}
- (void) showImageDisplayController
{
    if([appDelegate respondsToSelector:@selector(hideView:)])
    {
        [appDelegate hideView:@"setting"];
    }
}
- (void) setValueToLabel: (int) v label: (UILabel*)label
{
    label.text = [NSString stringWithFormat:@"%d", v];
}
- (IBAction)okHandler:(id)sender
{
    PainterManager* pm = [PainterManager painterManager];
    percent = [percentLabel.text intValue];
    times = [timesLabel.text intValue];
    pm.painterProperty.percent = percent;
    pm.painterProperty.times = times;
    pm.painterProperty.paper = paper;
    if(paramIDArray && paramArray)
    {
        NSUInteger i;
        for(i = 0 ; i < [paramIDArray count] ; i++)
        {
            NSString* sid = [paramIDArray objectAtIndex:i];
            [pm setParam:[paramArray objectAtIndex:i] withID:sid];
        }
    }
    NSArray* paramArray = [pm painterParamsByQuality:pm.painterProperty.percent withTimes:pm.painterProperty.times];
    [pm setCurrentPainterParamID:paramArray];
    [self showImageDisplayController];
}

- (IBAction)percentSliderHandler:(UISlider*)sender
{
    float v = sender.value;
    if(((int)v) != [percentLabel.text intValue])
    {
        int minTimes, maxTimes;
        PainterManager* pm = [PainterManager painterManager];
        [pm getMinMaxTimesValue:(int)v outMin:&minTimes outMax:&maxTimes];
        timesSlider.minimumValue = minTimes;
        timesSlider.maximumValue = maxTimes;
        [timesSlider setValue:maxTimes];
        [self setValueToLabel:maxTimes label:timesLabel];
    }
    [self setValueToLabel:(int)v label:percentLabel];
    NSLog(@"## percen = %f ##", v);
}
- (IBAction)timesSliderHandler:(UISlider*)sender
{
    float v = sender.value;
    [self setValueToLabel:(int)v label:timesLabel];
    NSLog(@"## times = %f ##", v);
}
- (IBAction)propertyHandler:(id)sender
{
    if([appDelegate respondsToSelector:@selector(showPainterParameterController)])
    {
        [appDelegate showPainterParameterController];

    }
    if([appDelegate respondsToSelector:@selector(showView:)])
    {
        //[appDelegate showPainterParameterController];
        [appDelegate showView:@"param"];
        PainterParameterController* p = [appDelegate painterParameterController];
        p.index = -1;
        p.photoFrameSetting = self;
        [p gotoNext];
    }
    
}
- (NSString*) paramID:(int)i
{
    int c = [paramIDArray count];
    if(i < 0 || i >= c)
        return nil;
    return [paramIDArray objectAtIndex:i];
}
- (PainterParam*) param:(int)i
{
    if(i < 0 || i >= [paramArray count])
        return nil;
    return [paramArray objectAtIndex:i];
}
- (int) paramCount
{
    return [paramArray count];
}
- (void) setCurrentPPM: (NSString*)brushName
{
    self.paper = brushName;
    [appDelegate setImageView:paperImageView withPPMName:brushName];
}
- (IBAction)changePaperHandler:(id)sender
{
    if([appDelegate respondsToSelector:@selector(showBrushTableController)])
    {
        [appDelegate showBrushTableController];
        NSArray* allPaper = [[NSBundle mainBundle] pathsForResourcesOfType:@".ppgm" inDirectory:nil];
        int count = [allPaper count];
        for(int i = 0 ; i < count ; i++)
        {
            NSLog(@"name = %@", [allPaper objectAtIndex:i]);
        }
        BrushTableController* btc = [appDelegate brushTableController];
        btc.brushes = allPaper;
        btc.parent = self;
        btc.type = SE_PPM_TYPE;
        [btc.tableView reloadData];
    }
    if([appDelegate respondsToSelector:@selector(showView:)])
    {
        [appDelegate showView:@"ppm"];
    }       
}
@end
