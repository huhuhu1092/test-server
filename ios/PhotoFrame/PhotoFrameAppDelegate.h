//
//  PhotoFrameAppDelegate.h
//  PhotoFrame
//
//  Created by 陈勇 on 11-10-26.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
#import <CoreData/CoreData.h>
#import "ppmtool.h"
@class SEViewNavigator;
@class ImageDisplayViewController;
@class PhotoFrameSettingController;
@class PainterParameterController;
@class BrushTableController;
@class PainterParameterPickerController;
@class PhotoFrame3DViewController;
@class SEResLoader;
@interface PhotoFrameAppDelegate : NSObject <UIApplicationDelegate>
{
    ImageDisplayViewController* imageViewController;
    PhotoFrameSettingController* photoFrameSettingController;
    PainterParameterController* painterParameterController;
    BrushTableController* brushTableController;
    PainterParameterPickerController* painterParameterPicker;
    PhotoFrame3DViewController* photoFrame3DController;
    NSArray* paramIDArray;
    //////
    SEViewNavigator* mViewNavigator;
    SEResLoader* mResLoader;
@private
    NSManagedObjectContext* managedObjectContext_;
    NSManagedObjectModel* managedObjectModel_;
    NSPersistentStoreCoordinator* persistentStoreCoordinator_;
}
@property (nonatomic, retain) IBOutlet UIWindow *window;
@property (nonatomic, readonly) NSManagedObjectContext* managedObjectContext;
@property (nonatomic, readonly) NSManagedObjectModel* managedObjectModel;
@property (nonatomic, readonly) NSPersistentStoreCoordinator* persistentStoreCoordinator;

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
- (void) saveContext;
//@property (nonatomic, retain) ImageDisplayViewController* imageViewController;
@end
