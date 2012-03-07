//
//  PickerViewDataSource.m
//  TestImageView
//
//  Created by 陈勇 on 11-10-11.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import "PainterParameterPickerController.h"
#import "PainterManager.h"
@implementation PainterParameterPickerController
@synthesize pickerView;
@synthesize dataArray;
@synthesize type;
@synthesize painterParam;
@synthesize appDelegate;
- (NSInteger) numberOfComponentsInPickerView:(UIPickerView *)pickerView
{
    return 1;
}
- (NSInteger) pickerView:(UIPickerView *)pickerView numberOfRowsInComponent:(NSInteger)component
{
    if(dataArray)
        return [dataArray count];
    return 0;
}
- (NSString*) pickerView:(UIPickerView *)pickerView titleForRow:(NSInteger)row forComponent:(NSInteger)component
{
    if(dataArray)
        return [dataArray objectAtIndex:row];
    else
        return @"";
}
- (void)pickerView:(UIPickerView *)pickerView didSelectRow:(NSInteger)row inComponent:(NSInteger)component
{
    NSLog(@"selected : %d", row);
    PainterParam* p = painterParam;

    switch (type) {
        case BG_TYPE:
        {
            //p.bg_type = row;
            selected_bg_type = row;
        }
        break;
        case SIZE_TYPE:
            //p.size_type = row;
            selected_size_type = row;
            break;
        case COLOR_TYPE:
            //p.color_type = row;
            selected_color_type = row;
            break;
        case ORIENT_TYPE:
            //p.orient_type = row;
            selected_orient_type = row;
            break;
        case PLACE_TYPE:
            //p.place_type = row;
            selected_place_type = row;
            break;
        default:
            break;
    }
}
- (void)viewDidAppear:(BOOL)animated
{
    PainterParam* p = painterParam;
    switch (type) {
        case BG_TYPE:
        {
            [pickerView selectRow: p.bg_type inComponent:0 animated:YES];
            selected_bg_type = p.bg_type;
        }
            break;
        case SIZE_TYPE:
            [pickerView selectRow: p.size_type inComponent:0 animated:YES]; 
            selected_size_type = p.size_type;
            break;
        case COLOR_TYPE:
            [pickerView selectRow: p.color_type inComponent:0 animated:YES];
            selected_color_type = p.color_type;
            break;
        case ORIENT_TYPE:
            [pickerView selectRow: p.orient_type inComponent:0 animated:YES];
            selected_orient_type = p.orient_type;
            break;
        case PLACE_TYPE:
            [pickerView selectRow: p.place_type inComponent:0 animated:YES];
            selected_place_type = p.place_type;
            break;
        default:
            break;
    }
    
}
- (void)dealloc
{
    [pickerView release];
    [dataArray release];
    [super dealloc];
}
- (IBAction)okButtonPressed:(id)sender
{
    PainterParam* p = painterParam;
    switch (type) {
        case BG_TYPE:
        {
            p.bg_type = selected_bg_type;
        }
            break;
        case SIZE_TYPE:
            p.size_type = selected_size_type;
            break;
        case COLOR_TYPE:
            p.color_type = selected_color_type;
            break;
        case ORIENT_TYPE:
            p.orient_type = selected_orient_type;
            break;
        case PLACE_TYPE:
            p.place_type = selected_place_type;
            break;
        default:
            break;
    }
    [appDelegate hideView:@"picker"];
}

@end
