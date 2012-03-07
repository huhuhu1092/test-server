//
//  PainterParameterController.h
//  PhotoFrame
//
//  Created by 陈勇 on 11-11-2.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
@class PhotoFrameSettingController;
@class PainterParameterPickerController;
@interface PainterParameterController : UIViewController
{
    UILabel* paper_scale;
    UILabel* paper_relief;
    UILabel* brush_relief;
    UILabel* orient_num;
    UILabel* orient_first;
    UILabel* orient_last;
    UILabel* size_num;
    UILabel* size_first;
    UILabel* size_last;
    UILabel* brush_density;
    UIButton* bg_type;
    UIButton* place_type;
    UIButton* orient_type;
    UIButton* color_type;
    UIButton* size_type;
    UILabel* drawing_speed;
    UILabel* wait_time;
    UILabel* brush;
    UILabel* painter_title;
    
    UITextField* paper_scale_input;
    UITextField* paper_relief_input;
    UITextField* brush_relief_input;
    UITextField* orient_num_input;
    UITextField* orient_first_input;
    UITextField* orient_last_input;
    UITextField* size_num_input;
    UITextField* size_first_input;
    UITextField* size_last_input;
    UITextField* brush_density_input;
    UITextField* drawing_speed_input;
    UITextField* wait_time_input;
    UIImageView* brush_image;
    UIImageView* brush1_image;
    UIImageView* brush2_image;
    UIButton* brush_change;
    PainterParameterPickerController* bgTypeController;
    //PainterParameterPickerController* placeTypeController;
    //PainterParameterPickerController* colorTypeController;
    //PainterParameterPickerController* orientTypeController;
    //PainterParameterPickerController* sizeTypeController;
@private
    id appDelegate;
    int index;
    int currentBrushIndex;
}
@property (nonatomic, retain) IBOutlet UILabel* painter_title;
@property (nonatomic, retain) IBOutlet UILabel* paper_scale;
@property (nonatomic, retain) IBOutlet UILabel* paper_relief;
@property (nonatomic, retain) IBOutlet UILabel* brush_relief;
@property (nonatomic, retain) IBOutlet UILabel* orient_num;
@property (nonatomic, retain) IBOutlet UILabel* orient_first;
@property (nonatomic, retain) IBOutlet UILabel* orient_last;
@property (nonatomic, retain) IBOutlet UILabel* size_num;
@property (nonatomic, retain) IBOutlet UILabel* size_first;
@property (nonatomic, retain) IBOutlet UILabel* size_last;
@property (nonatomic, retain) IBOutlet UILabel* brush_density;
@property (nonatomic, retain) IBOutlet UIButton* bg_type;
@property (nonatomic, retain) IBOutlet UIButton* place_type;
@property (nonatomic, retain) IBOutlet UIButton* orient_type;
@property (nonatomic, retain) IBOutlet UIButton* color_type;
@property (nonatomic, retain) IBOutlet UIButton* size_type;
@property (nonatomic, retain) IBOutlet UILabel* drawing_speed;
@property (nonatomic, retain) IBOutlet UILabel* wait_time;

@property (nonatomic, retain) IBOutlet UITextField* paper_scale_input;
@property (nonatomic, retain) IBOutlet UITextField* paper_relief_input;
@property (nonatomic, retain) IBOutlet UITextField* brush_relief_input;
@property (nonatomic, retain) IBOutlet UITextField* orient_num_input;
@property (nonatomic, retain) IBOutlet UITextField* orient_first_input;
@property (nonatomic, retain) IBOutlet UITextField* orient_last_input;
@property (nonatomic, retain) IBOutlet UITextField* size_num_input;
@property (nonatomic, retain) IBOutlet UITextField* size_first_input;
@property (nonatomic, retain) IBOutlet UITextField* size_last_input;
@property (nonatomic, retain) IBOutlet UITextField* brush_density_input;
@property (nonatomic, retain) IBOutlet UITextField* drawing_speed_input;
@property (nonatomic, retain) IBOutlet UITextField* wait_time_input;
@property (nonatomic, retain) IBOutlet UILabel* brush;
@property (nonatomic, retain) IBOutlet UIButton* brush_change;
@property (nonatomic, retain) IBOutlet UIImageView* brush_image;
@property (nonatomic, retain) IBOutlet UIImageView* brush1_image;
@property (nonatomic, retain) IBOutlet UIImageView* brush2_image;

///////////////
@property (nonatomic, retain) id appDelegate;
@property (nonatomic) int index;
@property (nonatomic, assign) PhotoFrameSettingController* photoFrameSetting;
- (void) gotoNext;
- (void) setCurrentPPM: (NSString*)brushName;
///////////////
- (IBAction)nextHandler:(id)sender;
- (IBAction)prevHandler:(id)sender;
- (IBAction)backHandler:(id)sender;
- (IBAction)brushChangeHandler:(id)sender;
- (IBAction)brush2ChangeHandler:(id)sender;
- (IBAction)brush3ChangeHandler:(id)sender;

- (IBAction)placeTypeHandler:(id)sender;
- (IBAction)bgTypeHandler:(id)sender;
- (IBAction)orientTypeHandler:(id)sender;
- (IBAction)sizeTypeHandler:(id)sender;
- (IBAction)colorTypeHandler:(id)sender;
@end
