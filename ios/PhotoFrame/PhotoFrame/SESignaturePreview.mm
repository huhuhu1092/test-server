//
//  SESignaturePreview.mm
//  PhotoFrame
//
//  Created by 陈勇 on 12-3-4.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SESignaturePreview.h"
#import "SEViewNavigator.h"
#import "UserInfo.h"
const NSString* siteString[] = {@"Left Top", @"Left Bottom", @"Right Top", @"Right Bottom"};
const NSString* sizeString[] = {@"Big", @"Mid", @"Little"};
#define SITE_PICKER_TAG 102
#define SIZE_PICKER_TAG 103
@implementation SESignaturePreview
@synthesize mViewNav;
@synthesize mCurrentSite;
@synthesize mCurrentSize;
- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        // Initialization code
    }
    return self;
}

/*
// Only override drawRect: if you perform custom drawing.
// An empty implementation adversely affects performance during animation.
- (void)drawRect:(CGRect)rect
{
    // Drawing code
}
*/
- (void) handleChange:(id)sender
{
    [mViewNav moveToView:SIGNATURE_SIGNATURE_PREVIEW :SIGNATURE_VIEW hasAnimation:YES isPush:YES];
}
- (void)initData
{
    UIImageView* imageView = (UIImageView*)[self viewWithTag:101];
    UIButton* changeButton = (UIButton*)[self viewWithTag:104];
    [changeButton addTarget:self action:@selector(handleChange:) forControlEvents:UIControlEventTouchUpInside];
    UserInfo* userInfo = [mViewNav getUserInfo];
    mCurrentSite = [userInfo.currentsignaturesite intValue];
    mCurrentSize = [userInfo.currentsignaturesize intValue];
    NSNumber* sigSeq = userInfo.currentsignature;
    NSSet* signatureList = userInfo.signaturelist;
    NSManagedObject* signature = nil;
    NSEnumerator* sigIt = [signatureList objectEnumerator];
    while((signature = [sigIt nextObject]) != nil)
    {
        NSNumber* seq = [signature valueForKey:@"seq"];
        if([seq isEqualToNumber:sigSeq])
        {
            //NSString* name = [signature valueForKey:@"name"];
            UIImage* image = nil;//[UIImage imageNamed:name];
            imageView.image = image;
        }
    }
    mSiteString = [NSArray arrayWithObjects:siteString count:4];
    mSizeString = [NSArray arrayWithObjects:sizeString count:3];
    [mSiteString retain];
    [mSizeString retain];
    UIPickerView* sitePickerView = (UIPickerView*)[self viewWithTag:102];
    sitePickerView.backgroundColor = [UIColor redColor];
    UIPickerView* sizePickerView = (UIPickerView*)[self viewWithTag:103];
    sitePickerView.dataSource = self;
    sitePickerView.delegate = self;
    [sitePickerView selectRow:mCurrentSite  inComponent:0 animated:YES];
    sizePickerView.dataSource = self;
    sizePickerView.delegate = self;
    [sizePickerView selectRow:mCurrentSize inComponent:0 animated:YES];
}
- (NSInteger) numberOfComponentsInPickerView:(UIPickerView *)pickerView
{
    return 1;
}
- (NSInteger) pickerView:(UIPickerView *)pickerView numberOfRowsInComponent:(NSInteger)component
{
    if(pickerView.tag == SITE_PICKER_TAG)
    {
        return mSiteString.count;
    }
    else if(pickerView.tag == SIZE_PICKER_TAG)
    {
        return mSizeString.count;
    }
    else
        return 0;
}
- (NSString*) pickerView:(UIPickerView *)pickerView titleForRow:(NSInteger)row forComponent:(NSInteger)component
{
    if(pickerView.tag == SITE_PICKER_TAG)
    {
        return [mSiteString objectAtIndex:row];
    }
    else if(pickerView.tag == SIZE_PICKER_TAG)
    {
        return [mSizeString objectAtIndex:row];
    }
    else
        return @"unknown";
}
- (void)pickerView:(UIPickerView *)pickerView didSelectRow:(NSInteger)row inComponent:(NSInteger)component
{
    NSLog(@"selected : %d", row);
    if(pickerView.tag == SITE_PICKER_TAG)
    {
        mCurrentSite = row;
    }
    else if(pickerView.tag == SIZE_PICKER_TAG)
    {
        mCurrentSize = row;
    }
    else
    {
        assert(0);
    }
    
}

@end
