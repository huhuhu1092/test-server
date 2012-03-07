//
//  PhotoFrameSetting.h
//  PhotoFrame
//
//  Created by 陈勇 on 11-11-1.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
@class PainterParam;
@interface PhotoFrameSettingController : UIViewController
{
    UISlider* percentSlider;
    UISlider* timesSlider;
    UILabel* percentLabel;
    UILabel* timesLabel;
    UIImageView* paperImageView;
@private
    id appDelegate;
@private
    int percent;
    int times;
    NSArray* paramIDArray;
    NSMutableArray* paramArray;
    NSString* paper;
}
@property (nonatomic, retain) IBOutlet UISlider* percentSlider;
@property (nonatomic, retain) IBOutlet UISlider* timesSlider;
@property (nonatomic, retain) IBOutlet UILabel* percentLabel;
@property (nonatomic, retain) IBOutlet UILabel* timesLabel;
@property (nonatomic, retain) IBOutlet UIImageView* paperImageView;
@property (nonatomic, assign) id appDelegate;
@property (nonatomic, retain) NSString* paper;
- (IBAction)okHandler:(id)sender;
- (IBAction)percentSliderHandler:(UISlider*)sender;
- (IBAction)timesSliderHandler:(UISlider*)sender;
- (IBAction)propertyHandler:(id)sender;
- (IBAction)changePaperHandler:(id)sender;
- (NSString*) paramID:(int)i;
- (PainterParam*) param:(int)i;
- (void) setCurrentPPM: (NSString*)brushName;
- (int) paramCount;
@end
