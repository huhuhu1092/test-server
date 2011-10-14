//
//  PickerViewDataSource.m
//  TestImageView
//
//  Created by 陈勇 on 11-10-11.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import "PainterParameterSetController.h"

@implementation PainterParameterSetController
@synthesize pickerView;
@synthesize dataArray;
@synthesize type;
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
    switch (type) {
        case BG_TYPE:
            
            break;
        case SIZE_TYPE:
            break;
        case COLOR_TYPE:
            break;
        case ORIENT_TYPE:
            break;
        case PLACE_TYPE:
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
    UIView* v = self.view;
    [v removeFromSuperview];
}
- (IBAction)cancelButtonPressed:(id)sender
{
    UIView* v = self.view;
    [v removeFromSuperview];
}
@end
