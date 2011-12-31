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
@class PainterParam;
@interface PainterParameterPickerController : UIViewController <UIPickerViewDataSource, UIPickerViewDelegate>
{
    NSArray* dataArray;
    UIPickerView* pickerView;
    ParameterType type;
    PainterParam* painterParam;
    id appDelegate;
@private
    int selected_bg_type;
    int selected_color_type;
    int selected_place_type;
    int selected_orient_type;
    int selected_size_type;
}
@property (nonatomic, retain) IBOutlet UIPickerView* pickerView;
@property (nonatomic, retain) NSArray* dataArray;
@property (nonatomic, assign) PainterParam* painterParam;
@property (nonatomic) ParameterType type;
@property (nonatomic, assign) id appDelegate;
- (IBAction)okButtonPressed:(id)sender;
@end
