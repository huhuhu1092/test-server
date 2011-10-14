//
//  MyClass.m
//  TestImageView
//
//  Created by 陈勇 on 11-10-10.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import "ParameterSetMainUI.h"
#import "PainterParameterSetController.h"
#import "PainterManager.h"
@interface ParameterSetMainUI (PrivateMethod)
- (void) removeCurrentView:(UIView*)v;
- (void) saveCurrentParam;
@end
@implementation ParameterSetMainUI (PrivateMethod)
- (void) removeCurrentView:(UIView*)v
{
    UIWindow* window = [[UIApplication sharedApplication] keyWindow];
    if([v isDescendantOfView:window])
    {
        [v removeFromSuperview];
    }
}
- (void)restoreParam: (int)index
{
    if(index >= 1 && index <= PARAM_NUM)
    {
        int i = index - 1;
        PainterParam* p = [painterManager.paramArray objectAtIndex:i];
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
    }
    
}
- (void)saveCurrentParam
{
    if(currentParamIndex >= 1 && currentParamIndex < PARAM_NUM)
    {
        int i = currentParamIndex - 1;
        PainterParam* p = [painterManager.paramArray objectAtIndex:i];
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
}
@end

@implementation ParameterSetMainUI
@synthesize currentView;
@synthesize topView;
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

@synthesize painterManager;
- (id)init
{
    self = [super init];
    if (self) {
        // Initialization code here.
        currentParamIndex = 1;
    }
    
    return self;
}

- (IBAction)selectButtonPressed:(id)sender
{
    [self removeCurrentView:currentView];
}
- (IBAction)cancelButtonPressed:(id)sender
{
    [self removeCurrentView:currentView];
}
- (void) addViewToWindow: (UIViewController*)viewController
{
    UIView* v = viewController.view;
    v.frame = [[UIScreen mainScreen] applicationFrame];
    UIWindow* window = [[UIApplication sharedApplication] keyWindow];
    [window addSubview:v];
}
- (PainterParameterSetController*)getParamController:(PainterParameterSetController*)c type:(ParameterType)t
{
    if(!c)
    {
        c = [[PainterParameterSetController alloc] initWithNibName:@"ParamType" bundle:nil];
        c.type = t;
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
    return c;
}
- (IBAction)bgTypeButtonPressed:(id)sender
{
    bgTypeController = [self getParamController:bgTypeController type:BG_TYPE];
    [self addViewToWindow:bgTypeController];
}
- (IBAction)colorTypeButtonPressed:(id)sender
{
    colorTypeController = [self getParamController:colorTypeController type:COLOR_TYPE];
    [self addViewToWindow:colorTypeController];
}
- (IBAction)sizeTypeButtonPressed:(id)sender
{
    sizeTypeController = [self getParamController:sizeTypeController type:SIZE_TYPE];
    [self addViewToWindow:sizeTypeController];
}
- (IBAction)placeTypeButtonPressed:(id)sender
{
    placeTypeController = [self getParamController:placeTypeController type:PLACE_TYPE];
    [self addViewToWindow:placeTypeController];
}
- (IBAction)orientTypeButtonPressed:(id)sender
{
    orientTypeController = [self getParamController:orientTypeController type:ORIENT_TYPE];
    [self addViewToWindow:orientTypeController];
}
- (IBAction)sequenceButtonPressed:(id)sender
{
    UIButton* button = (UIButton*)sender;
    NSLog(@"button = %d", button.tag);
    [self saveCurrentParam];
    [self restoreParam:button.tag];
    currentParamIndex = button.tag;
    switch (button.tag) {
        case 1:
            
            break;
        case 2:
            break;
        case 3:
            break;
        case 4:
            break;
        case 5:
            break;
            
        case 6:
            break;
        case 7:
            break;
        case 8:
            break;
        case 9:
            break;
        case 10:
            break;
        default:
            break;
    }
}
- (void)initViewByParam
{
    [self restoreParam:currentParamIndex];
}
- (void)dealloc
{
    [topView release];
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
    [colorTypeController release];
    [orientTypeController release];
    [sizeTypeController release];
    [placeTypeController release];
    [painterManager release];
    [super dealloc];
}
@end
