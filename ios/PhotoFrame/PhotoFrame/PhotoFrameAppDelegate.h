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
@class SEProductManager;
@class SEInAppPurchaseTransactionObserver;
@class GooglePlusShare;
@class GooglePlusSignInButton;
@class GTMOAuth2Authentication;
@interface PhotoFrameAppDelegate : NSObject <UIApplicationDelegate, UIAlertViewDelegate>
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
    SEProductManager* mProductManager;
    SEInAppPurchaseTransactionObserver* mProductTransaction;
    
    //GooglePlusShare* share_;
    //GooglePlusSignInButton* signInButton_;
   // GTMOAuth2Authentication* auth_;
@private
    NSManagedObjectContext* managedObjectContext_;
    NSManagedObjectModel* managedObjectModel_;
    NSPersistentStoreCoordinator* persistentStoreCoordinator_;
}
// The Google+ sign-in button to handle the URL redirect.
//@property (retain, nonatomic) GooglePlusSignInButton *signInButton;
// The OAuth 2.0 authentication used in the application.
//@property (retain, nonatomic) GTMOAuth2Authentication *auth;
// The Google+ share object to handle the URL redirect.
//@property (retain, nonatomic) GooglePlusShare *share;
@property (nonatomic, retain) IBOutlet UIWindow *window;
@property (nonatomic, readonly) NSManagedObjectContext* managedObjectContext;
@property (nonatomic, readonly) NSManagedObjectModel* managedObjectModel;
@property (nonatomic, readonly) NSPersistentStoreCoordinator* persistentStoreCoordinator;
@property (nonatomic, readonly) SEViewNavigator* mViewNavigator;
@property (nonatomic, readonly) SEProductManager* mProductManager;
@property (nonatomic, readonly) SEInAppPurchaseTransactionObserver* mProductTransaction;
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
+ (SEViewNavigator*) getViewNavigator;
+ (SEProductManager*) getProductManager;
+ (SEInAppPurchaseTransactionObserver*) getPurchaseTransactionObserver;
//@property (nonatomic, retain) ImageDisplayViewController* imageViewController;
@end
