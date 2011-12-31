//
//  PhotoFrameAppDelegate.h
//  PhotoFrame
//
//  Created by 陈勇 on 11-10-26.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "ppmtool.h"
@class ImageDisplayViewController;
@class PhotoFrameSettingController;
@class PainterParameterController;
@class BrushTableController;
@class PainterParameterPickerController;
@class PhotoFrame3DViewController;
@interface PhotoFrameAppDelegate : NSObject <UIApplicationDelegate>
{
    ImageDisplayViewController* imageViewController;
    PhotoFrameSettingController* photoFrameSettingController;
    PainterParameterController* painterParameterController;
    BrushTableController* brushTableController;
    PainterParameterPickerController* painterParameterPicker;
    PhotoFrame3DViewController* photoFrame3DController;
    NSArray* paramIDArray;
}
@property (nonatomic, retain) IBOutlet UIWindow *window;
- (void) showPhotoFrameSettingController;
- (void) showImageDisplayController;
- (void) showPainterParameterController;
- (void) showBrushTableController;
- (void) showPainterParameterPicker;
- (void) showPhotoFrame3DController;
- (void) showView : (NSString*) viewName;
- (void) hideView : (NSString*) viewName;
- (void) setParamIDArray: (NSArray*) paramArray;
- (NSArray*) paramIDArray;
- (PainterParameterController*) painterParameterController;
- (BrushTableController*) brushTableController;
- (PhotoFrameSettingController*) photoFrameSettingController;
- (PainterParameterPickerController*) painterParameterPicker;
- (PhotoFrame3DViewController*) photoFrame3D;
- (void)setImageView:(UIImageView*)imageView withPPMName:(NSString*)name;
//@property (nonatomic, retain) ImageDisplayViewController* imageViewController;
@end
