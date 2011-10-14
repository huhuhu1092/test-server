//
//  PickerViewDataSource.h
//  TestImageView
//
//  Created by 陈勇 on 11-10-11.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
typedef enum {
    BG_TYPE = 0,
    PLACE_TYPE,
    SIZE_TYPE,
    COLOR_TYPE,
    ORIENT_TYPE,
} ParameterType;
@interface PainterParameterSetController : UIViewController <UIPickerViewDataSource, UIPickerViewDelegate>
{
    NSArray* dataArray;
    UIPickerView* pickerView;
    ParameterType type;
}
@property (nonatomic, retain) IBOutlet UIPickerView* pickerView;
@property (nonatomic, retain) NSArray* dataArray;
@property (nonatomic) ParameterType type;
- (IBAction)okButtonPressed:(id)sender;
- (IBAction)cancelButtonPressed:(id)sender;
@end
