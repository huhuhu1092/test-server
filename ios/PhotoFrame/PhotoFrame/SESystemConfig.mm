//
//  SESystemConfig.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-9-21.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import "SESystemConfig.h"
#import "FontLabel.h"
#import "SEUtil.h"
#import "PhotoFrameAppDelegate.h"
#import "SEResDefine.h"
#import "SEViewNavigator.h"
#import "UserInfo.h"
@implementation SESystemConfig

+ (void) setLabelFont: (FontLabel*) fontLabel text: (NSString*) text color: (UIColor*) color fontName: (NSString*) fontName fontSize: (CGFloat) fontSize
{
    [fontLabel setZFont:[[FontManager sharedManager] zFontWithName:fontName pointSize:fontSize]];
    fontLabel.text = text;
    fontLabel.backgroundColor = [UIColor clearColor];
    fontLabel.textColor = color;
}
+ (NSString*) getITuneConnetionStr
{
    return @" from \"PrivatePainter\" application on iPad";
}
+ (float) getShareStrFontSize
{
    return 50;
}
+ (NSString*) getFontName
{
    if([SEUtil getSystemVersion] <= 4)
    {
        return @"HelveticaNeue-Bold";
    }
    else 
    {
        return @"HelveticaNeue-CondensedBold";
    }
}
+ (void) setImageWithCap: (UIImageView*)imageView name: (NSString*) name
{
    UIImage* image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader]
                    getImage:name];
    imageView.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
}
+ (NSString*) getDefaultSelectedImageURL
{
    return @"thespeedSUNDefaultImage*";
}
+ (BOOL) isDefaultSelectedImageURL: (NSString*) url
{
    return [url isEqualToString:@"thespeedSUNDefaultImage*"];
}
+ (UIImage*) getDefaultSelectedImage
{
    UIImage* uiImage = [UIImage imageNamed:@"background_001.png"];
    return uiImage;
}
+ (UIImage*) getTextImage:(NSString *)name
{
    UIImage* title = nil;
    if([name isEqualToString:@"save"])
    {
        //title = [mViewNav.mResLoader getImage:@"save.png"];
        title = [UIImage imageNamed:@"save.png"];
    }
    else if([name isEqualToString:@"clear"])
    {
        //title = [mViewNav.mResLoader getImage:@"clean.png"];
        title = [UIImage imageNamed:@"clean.png"];
    }
    else if([name isEqualToString:@"back"])
    {
        //title = [mViewNav.mResLoader getImage:@"back.png"];
        title = [UIImage imageNamed:@"back.png"];
    }
    else if([name isEqualToString:@"add"])
    {
        title = [UIImage imageNamed:@"add.png"];
    }
    else if([name isEqualToString:@"delete"])
    {
        title = [UIImage imageNamed:@"delete.png"];
    }
    else if([name isEqualToString:@"edit"])
    {
        title = [UIImage imageNamed:@"edit.png"];
    }
    else if([name isEqualToString:@"cancel"])
    {
        title = [UIImage imageNamed:@"cancel.png"];
    }
    else if([name isEqualToString:@"share"])
    {
        title = [UIImage imageNamed:@"share.png"];
    }
    else if([name isEqualToString:@"ok"])
    {
        title = [UIImage imageNamed:@"ok.png"];
    }
    else if([name isEqualToString:@"musiclist"])
    {
        title = [UIImage imageNamed:@"musiclist.png"];
    }
    else if([name isEqualToString:@"imagelist"])
    {
        title = [UIImage imageNamed:@"photolist.png"];
    }
    else if([name isEqualToString:@"retry"])
    {
        title = [UIImage imageNamed:@"retry.png"];
    }
    return title;
}
+ (int) getMinDensityValue
{
    return 1;
}
+ (int) getMaxDensityValue
{
    return 10;
}
+ (int) getMinEdgeDetectValue
{
    return 1;
}
+ (int) getMaxEdgeDetectValue
{
    return 10;
}
+ (CGSize) getMaxDrawViewSize
{
    return  CGSizeMake(480, 320);
}
+ (CGSize) getMinDrawViewSize
{
    return CGSizeMake(120, 80);
}
+ (CGSize) getCurrentDrawViewSize
{
    UserInfo* userInfo = [[PhotoFrameAppDelegate getViewNavigator] getUserInfo];
    CGSize maxSize = [SESystemConfig getMaxDrawViewSize];
    CGSize minSize = [SESystemConfig getMinDrawViewSize];
    int minValue = [self getMinSignatureValue];
    int maxValue = [self getMaxSignatureValue];
    int v = [userInfo.signaturesize intValue];
    float ratio = (v - minValue)/ (float)(maxValue - minValue);
    float w = minSize.width + (maxSize.width - minSize.width) * ratio;
    float sizeRatio = maxSize.height / maxSize.width;
    float h = w * sizeRatio;
    return CGSizeMake(w , h);
    /*
    if([userInfo.signaturesize intValue] == 2)
    {
        return CGSizeMake(480, 320);
    }
    else if([userInfo.signaturesize intValue] == 1)
    {
        return CGSizeMake(240, 160);
    }
    else
    {
        return CGSizeMake(120, 80);
    }
     */
}
+ (int) getMinSignatureValue
{
    return 1;
}
+ (int) getMaxSignatureValue
{
    return 10;
}
+ (int) getMinPowerViewValue
{
    return 1;
}
+ (int) getMaxPowerViewValue
{
    return 10;
}
+ (int) getMinBrushTransparentValue
{
    return 1;
}
+ (int) getMaxBrushTransparentValue
{
    return 10;
}
/*
+ (CGSize) getCurrentPowerViewSize
{
    
}
 */
+ (NSString*) getDefaultImageListName
{
    return @"default";
}
+ (NSString*) getDefaultMusicListName
{
    return @"default";
}
+ (NSArray*) getFeedChar
{
    NSArray* ret = [NSArray arrayWithObjects: [NSNumber numberWithUnsignedChar:'S'], [NSNumber numberWithUnsignedChar:'p'], [NSNumber numberWithUnsignedChar:'e'], [NSNumber numberWithUnsignedChar:'e'], [NSNumber numberWithUnsignedChar:'D'], nil];
    return ret;
}
+ (NSString*) getProductRequectNotificationName
{
    return @"SEProductReqeusctNotification";
}
+ (NSString*) getProductPurchaseSuccessNotificationName
{
    return @"SEProductPurchaseSuccessNotification";
}
+ (NSString*) getProductPurchaseRestoreNotificationName
{
    return @"SEProductPurchaseRestoreNotification";
}
+ (NSString*) getProductPurchaseFailedNotificationName
{
    return @"SEProductPurchaseFailedNotification";
}
+ (NSString*) getproductPurchaseCancelNotificationName
{
    return @"SEProductPurchaseCancelNotification";
}
+ (NSString*) getBasicSettingExpiredDate
{
    return @"2013/04/02";
}
+ (NSString*) getBasicSettingCalculateExpiredDate
{
    return @"2013/04/01";
}
@end