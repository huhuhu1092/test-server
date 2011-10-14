//
//  MyClass.h
//  TestImageView
//
//  Created by 陈勇 on 11-10-10.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
@class PainterParameterSetController;
@class PainterManager;
@interface ParameterSetMainUI : NSObject
{
    UIView* topView;
    UIView* currentView;
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
    PainterParameterSetController* bgTypeController;
    PainterParameterSetController* placeTypeController;
    PainterParameterSetController* colorTypeController;
    PainterParameterSetController* orientTypeController;
    PainterParameterSetController* sizeTypeController;
    PainterManager* painterManager;
@private
    int currentParamIndex;
}
@property (nonatomic, retain) UIView* currentView;
@property (nonatomic, retain) IBOutlet UIView* topView;
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

@property (nonatomic, retain) PainterManager* painterManager;
- (IBAction)selectButtonPressed:(id)sender;
- (IBAction)cancelButtonPressed:(id)sender;
- (IBAction)bgTypeButtonPressed:(id)sender;
- (IBAction)colorTypeButtonPressed:(id)sender;
- (IBAction)sizeTypeButtonPressed:(id)sender;
- (IBAction)placeTypeButtonPressed:(id)sender;
- (IBAction)orientTypeButtonPressed:(id)sender;

- (IBAction)sequenceButtonPressed:(id)sender;

///////
- (void)initViewByParam;
@end
