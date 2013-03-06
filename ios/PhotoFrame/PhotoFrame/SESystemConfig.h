//
//  SESystemConfig.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-9-21.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
@class FontLabel;
@interface SESystemConfig : NSObject
+ (void) setLabelFont: (FontLabel*) fontLabel text: (NSString*) text color: (UIColor*) color fontName: (NSString*) fontName fontSize: (CGFloat) fontSize;
+ (NSString*) getFontName;
+ (void) setImageWithCap: (UIImageView*)imageView name: (NSString*) name;
+ (NSString*) getDefaultSelectedImageURL;
+ (BOOL) isDefaultSelectedImageURL: (NSString*) url;
+ (UIImage*) getDefaultSelectedImage;
+ (UIImage*) getTextImage: (NSString*) name;
+ (int) getMinDensityValue;
+ (int) getMaxDensityValue;
+ (int) getMinEdgeDetectValue;
+ (int) getMaxEdgeDetectValue;
+ (int) getMinBrushTransparentValue;
+ (int) getMaxBrushTransparentValue;
+ (CGSize) getCurrentDrawViewSize;
+ (CGSize) getMaxDrawViewSize;
+ (CGSize) getMinDrawViewSize;
+ (int) getMinSignatureValue;
+ (int) getMaxSignatureValue;
//+ (CGSize) getCurrentPowerViewSize;
+ (int) getMinPowerViewValue;
+ (int) getMaxPowerViewValue;
+ (NSString*) getDefaultImageListName;
+ (NSString*) getDefaultMusicListName;
+ (NSString*) getITuneConnetionStr;
+ (float) getShareStrFontSize;
+ (NSArray*) getFeedChar;
+ (NSString*) getProductRequectNotificationName;
+ (NSString*) getProductPurchaseSuccessNotificationName;
+ (NSString*) getProductPurchaseRestoreNotificationName;
+ (NSString*) getProductPurchaseFailedNotificationName;
+ (NSString*) getproductPurchaseCancelNotificationName;
+ (NSString*) getBasicSettingExpiredDate;
+ (NSString*) getBasicSettingCalculateExpiredDate;
@end