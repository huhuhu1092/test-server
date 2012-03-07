//
//  PainterParameterController.m
//  PhotoFrame
//
//  Created by 陈勇 on 11-11-2.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import "PainterParameterController.h"
#import "PainterManager.h"
#import "PhotoFrameSettingController.h"
#import "ppmtool.h"
#import "PainterManager.h"
#import "BrushTableController.h"
#import "PainterParameterPickerController.h"
@implementation PainterParameterController
@synthesize appDelegate;
@synthesize photoFrameSetting;
@synthesize index;
@synthesize painter_title;
@synthesize paper_scale;
@synthesize paper_scale_input;
@synthesize paper_relief;
@synthesize paper_relief_input;
@synthesize brush_relief;
@synthesize brush_relief_input;
@synthesize orient_num;
@synthesize orient_num_input;
@synthesize orient_first;
@synthesize orient_first_input;
@synthesize orient_last;
@synthesize orient_last_input;
@synthesize orient_type;
@synthesize size_num;
@synthesize size_num_input;
@synthesize size_last;
@synthesize size_last_input;
@synthesize size_type;
@synthesize size_first;
@synthesize size_first_input;
@synthesize place_type;
@synthesize color_type;
@synthesize bg_type;
@synthesize brush_density;
@synthesize brush_density_input;
@synthesize drawing_speed;
@synthesize drawing_speed_input;
@synthesize wait_time;
@synthesize wait_time_input;
@synthesize brush;
@synthesize brush_image;
@synthesize brush1_image;
@synthesize brush2_image;
@synthesize brush_change;
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
- (void) setImage:(NSString*)name
{
    [appDelegate setImageView:brush_image withPPMName:name];
}
- (void)restoreValue: (PainterParam*)p
{
    paper_scale_input.text = [NSString stringWithFormat:@"%f", p.paper_scale];
    paper_relief_input.text = [NSString stringWithFormat:@"%f", p.paper_relief];
    brush_relief_input.text = [NSString stringWithFormat:@"%f", p.brush_relief];
    orient_num_input.text = [NSString stringWithFormat:@"%d", p.orient_num];
    orient_first_input.text = [NSString stringWithFormat:@"%f", p.orient_first];
    orient_last_input.text = [NSString stringWithFormat:@"%f", p.orient_last];
    size_num_input.text = [NSString stringWithFormat:@"%d", p.size_num];
    size_first_input.text = [NSString stringWithFormat:@"%f", p.size_first];
    size_last_input.text = [NSString stringWithFormat:@"%f", p.size_last];
    brush_density_input.text = [NSString stringWithFormat:@"%f", p.brush_density];
    drawing_speed_input.text = [NSString stringWithFormat:@"%d", p.drawing_speed];
    wait_time_input.text = [NSString stringWithFormat:@"%d", p.wait_time];
    //[brush_change setTitle:p.brushName forState:NSUTF8StringEncoding];
    //[self setImage:p.brushName];
    if([appDelegate respondsToSelector:@selector(setImageView:withPPMName:)])
    {
        [appDelegate setImageView:self.brush_image withPPMName:p.brushName];
        [appDelegate setImageView:self.brush1_image withPPMName:p.brushName1];
        [appDelegate setImageView:self.brush2_image withPPMName:p.brushName2];
    }
    painter_title.text = p.paintid;
}
- (void)saveValue
{
    PainterParam* p = [photoFrameSetting param:index];
    NSString* str = paper_scale_input.text;
    p.paper_scale = [str floatValue];
    str = paper_relief_input.text;
    p.paper_relief = [str floatValue];
    str = brush_relief_input.text;
    p.brush_relief = [str floatValue];
    str = orient_num_input.text;
    p.orient_num = [str intValue];
    str = orient_first_input.text;
    p.orient_first = [str floatValue];
    str = orient_last_input.text;
    p.orient_last = [str floatValue];
    str = size_num_input.text;
    p.size_num = [str intValue];
    str = size_first_input.text;
    p.size_first = [str floatValue];
    str = size_last_input.text;
    p.size_last = [str floatValue];
    str = brush_density_input.text;
    p.brush_density = [str floatValue];
    str = drawing_speed_input.text;
    p.drawing_speed = [str intValue];
    str = wait_time_input.text;
    p.wait_time = [str intValue];
}
- (void) dealloc
{
    [appDelegate release];
    [paper_scale release];
    [paper_relief release];
    [brush_relief release];
    [orient_num release];
    [orient_first release];
    [orient_last release];
    [size_num release];
    [size_first release];
    [size_last release];
    [brush_density release];
    [bg_type release];
    [place_type release];
    [orient_type release];
    [color_type release];
    [size_type release];
    [bgTypeController release];
    //[colorTypeController release];
    //[orientTypeController release];
    //[sizeTypeController release];
    //[placeTypeController release];
    [brush_image release];
    [brush1_image release];
    [brush2_image release];
    [brush release];
    [brush_change release];
    [super dealloc];
}
- (void) gotoNext
{
    int count = [photoFrameSetting paramCount];
    if(index == count - 1)
        return;
    index++;
    PainterParam* p = [photoFrameSetting param: index];
    NSString* sid = [photoFrameSetting paramID:index];
    if(p && sid)
    {
        [p retain];
        [self restoreValue : p];
        [p release];
    }
}
- (void) gotoPrev
{
    if(index == 0)
        return;
    index--;
    PainterParam* p = [photoFrameSetting param: index];
    NSString* sid = [photoFrameSetting paramID:index];
    if(p && sid)
    {
        [self restoreValue : p];
    }   
}
- (IBAction)nextHandler:(id)sender
{
    [self saveValue];
    [self gotoNext];
}
- (IBAction)prevHandler:(id)sender
{
    [self saveValue];
    [self gotoPrev];
}
- (IBAction)backHandler:(id)sender
{
    [self saveValue];
    if([appDelegate respondsToSelector:@selector(hideView:)])
    {
        [appDelegate hideView:@"param"];
    }
}
- (void) handleBrushChangeButton: (int) index
{
    if([appDelegate respondsToSelector:@selector(showBrushTableController)])
    {
        [appDelegate showBrushTableController];
        NSArray* ret = [[NSBundle mainBundle] pathsForResourcesOfType:@".pgm" inDirectory:nil];
        NSArray* ret1 = [[NSBundle mainBundle] pathsForResourcesOfType:@".ppm" inDirectory:nil];
        NSArray* allBrushes = [ret arrayByAddingObjectsFromArray:ret1];
        int count = [allBrushes count];
        for(int i = 0 ; i < count ; i++)
        {
            NSLog(@"name = %@", [allBrushes objectAtIndex:i]);
        }
        BrushTableController* btc = [appDelegate brushTableController];
        btc.brushes = allBrushes;
        btc.parent = self;
        btc.type = SE_PPM_TYPE;
        [btc.tableView reloadData];
    }
    if([appDelegate respondsToSelector:@selector(showView:)])
    {
        [appDelegate showView:@"ppm"];
        
    }    
}
- (IBAction)brushChangeHandler:(id)sender
{
    currentBrushIndex = 0;
    [self handleBrushChangeButton:0];
}
- (IBAction)brush2ChangeHandler:(id)sender
{
    currentBrushIndex = 1;
    [self handleBrushChangeButton:1];
}
- (IBAction)brush3ChangeHandler:(id)sender
{
    currentBrushIndex = 2;
    [self handleBrushChangeButton:2];
}
- (void) setCurrentPPM: (NSString*)brushName
{
    PainterParam* p = [photoFrameSetting param:index];
    if(currentBrushIndex == 0)
    {
        p.brushName = brushName;
        [appDelegate setImageView:brush_image withPPMName:brushName];
    }
    else if(currentBrushIndex == 1)
    {
        p.brushName1 = brushName;
        [appDelegate setImageView:brush1_image withPPMName:brushName];
    }
    else if(currentBrushIndex == 2)
    {
        p.brushName2 = brushName;
        [appDelegate setImageView:brush2_image withPPMName:brushName];
    }
    
}

- (void)getParamController:(PainterParameterPickerController*)c type:(ParameterType)t
{
    c.painterParam = [photoFrameSetting param:index];
        switch (t) {
            case BG_TYPE:
            {
                NSString* str1 = NSLocalizedStringFromTable(@"bg_type_solid",@"ParamType", nil);
                NSString* str2 = NSLocalizedStringFromTable(@"bg_type_keep_original", @"ParamType", nil);
                NSString* str3 = NSLocalizedStringFromTable(@"bg_type_from_paper", @"ParamType", nil);
                NSArray* dataArray = [[NSArray alloc] initWithObjects:str1, str2, str3, nil];
                c.dataArray = dataArray;
                [dataArray release];
            }
                break;
            case ORIENT_TYPE:
            {
                NSString* str1 = NSLocalizedStringFromTable(@"orient_value" , @"ParamType", nil);
                NSString* str2 = NSLocalizedStringFromTable(@"orient_radius", @"ParamType", nil);
                NSString* str3 = NSLocalizedStringFromTable(@"orient_radial", @"ParamType", nil);
                NSString* str4 = NSLocalizedStringFromTable(@"orient_flowing",@"ParamType", nil);
                NSString* str5 = NSLocalizedStringFromTable(@"orient_hue", @"ParamType", nil);
                NSString* str6 = NSLocalizedStringFromTable(@"orient_adaptive", @"ParamType", nil);
                NSString* str7 = NSLocalizedStringFromTable(@"orient_manual", @"ParamType", nil);
                NSArray* dataArray = [[NSArray alloc] initWithObjects: str1, str2, str3, str4, str5, str6, str7, nil];
                c.dataArray = dataArray;
                [dataArray release];
            }
                break; 
            case SIZE_TYPE:
            {
                NSString* str1 = NSLocalizedStringFromTable(@"size_type_value", @"ParamType",nil);
                NSString* str2 = NSLocalizedStringFromTable(@"size_type_radius", @"ParamType", nil);
                NSString* str3 = NSLocalizedStringFromTable(@"size_type_random", @"ParamType", nil);
                NSString* str4 = NSLocalizedStringFromTable(@"size_type_radial", @"ParamType", nil);
                NSString* str5 = NSLocalizedStringFromTable(@"size_type_flowing", @"ParamType", nil);
                NSString* str6 = NSLocalizedStringFromTable(@"size_type_hue", @"ParamType", nil);
                NSString* str7 = NSLocalizedStringFromTable(@"size_type_adaptive", @"ParamType", nil);
                NSString* str8 = NSLocalizedStringFromTable(@"size_type_manual", @"ParamType", nil);
                NSArray* dataArray = [[NSArray alloc] initWithObjects: str1, str2, str3,str4,str5,str6,str7, str8, nil];
                c.dataArray = dataArray;
                [dataArray release];
            }
                break;
            case PLACE_TYPE:
            {
                NSString* str1 = NSLocalizedStringFromTable(@"place_type_random", @"ParamType", nil);
                NSString* str2 = NSLocalizedStringFromTable(@"place_type_even_dist", @"ParamType", nil);
                NSArray* dataArray = [[NSArray alloc] initWithObjects:str1, str2, nil];
                c.dataArray = dataArray;
                [dataArray release];
            }
                break;
            case COLOR_TYPE:
            {
                NSString* str1 = NSLocalizedStringFromTable(@"color_type_brush_avg", @"ParamType", nil);
                NSString* str2 = NSLocalizedStringFromTable(@"color_type_brush_center", @"ParamType", nil);
                NSArray* dataArray = [[NSArray alloc] initWithObjects: str1, str2,nil];
                c.dataArray = dataArray;
                [dataArray release];
            }
                break;
            default:
            {
                NSLog(@"can no suport type : %d ", t);
            }
            break;
        }
}
- (IBAction)placeTypeHandler:(id)sender
{
    [appDelegate showPainterParameterPicker];
    PainterParameterPickerController* pc = [appDelegate painterParameterPicker];
    pc.type = PLACE_TYPE;
    [self getParamController:pc type:PLACE_TYPE];
    [pc.pickerView reloadAllComponents];
    [appDelegate showView:@"picker"];
}
- (IBAction)bgTypeHandler:(id)sender
{
    [appDelegate showPainterParameterPicker];
    PainterParameterPickerController* pc = [appDelegate painterParameterPicker];
    pc.type = BG_TYPE;
    [self getParamController:pc type:BG_TYPE];
    [pc.pickerView reloadAllComponents];
    [appDelegate showView:@"picker"];    
}
- (IBAction)orientTypeHandler:(id)sender
{
    [appDelegate showPainterParameterPicker];
    PainterParameterPickerController* pc = [appDelegate painterParameterPicker];
    pc.type = ORIENT_TYPE;
    [self getParamController:pc type:ORIENT_TYPE];
    [pc.pickerView reloadAllComponents];
    [appDelegate showView:@"picker"];
}
- (IBAction)sizeTypeHandler:(id)sender
{
    [appDelegate showPainterParameterPicker];
    PainterParameterPickerController* pc = [appDelegate painterParameterPicker];
    pc.type = SIZE_TYPE;
    [self getParamController:pc type:SIZE_TYPE];
    [pc.pickerView reloadAllComponents];
    [appDelegate showView:@"picker"];
}
- (IBAction)colorTypeHandler:(id)sender
{
    [appDelegate showPainterParameterPicker];
    PainterParameterPickerController* pc = [appDelegate painterParameterPicker];
    pc.type = COLOR_TYPE;
    [self getParamController:pc type:COLOR_TYPE];
    [pc.pickerView reloadAllComponents];
    [appDelegate showView:@"picker"];
}

@end
