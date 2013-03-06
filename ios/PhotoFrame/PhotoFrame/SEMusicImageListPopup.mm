//
//  SEMusicImageListPopup.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-3-8.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEMusicImageListPopup.h"
#import "PhotoFrameAppDelegate.h"
#import "SEViewNavigator.h"
#import "SEPopupViewWidgets.h"
#import "FontLabel.h"
#import "SESystemConfig.h"
#import "SEResDefine.h"
#import "SEUtil.h"
@implementation SEMusicImageListPopupView
@synthesize title;
@synthesize content;
@synthesize errorMsg;
@synthesize okButton;
@synthesize cancelButton;
+ (SEMusicImageListPopupView*) createFromNib: (NSString*)nibFile
{
    float popupWidth = [[PhotoFrameAppDelegate getViewNavigator] mViewPortWidth];
    float popupHeight = [[PhotoFrameAppDelegate getViewNavigator] mViewPortHeight];
    UIImageView* background = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0,popupWidth , popupHeight)];
    [SESystemConfig setImageWithCap:background name:@"OptionsLockBg"];
    UIView* view = [[[NSBundle mainBundle] loadNibNamed:nibFile owner:self options:nil] lastObject];
    SEMusicImageListPopupView* popupView = [[SEMusicImageListPopupView alloc] initWithFrame:CGRectMake(0, 0, [[PhotoFrameAppDelegate getViewNavigator] mViewPortWidth], [[PhotoFrameAppDelegate getViewNavigator] mViewPortHeight])];
    view.frame  = CGRectMake((popupView.frame.size.width - view.frame.size.width) / 2, 13 , view.frame.size.width, view.frame.size.height);
    [popupView addSubview:background];
    [background release];
    [popupView addSubview:view];
    view.backgroundColor = [UIColor clearColor];
    popupView->title = (SEPopupLabel*)[view viewWithTag:104];
    popupView->content = (UITextField*)[view viewWithTag:101];
    popupView->okButton = (SEPopupButton*)[view viewWithTag:102];
    popupView->cancelButton = (SEPopupButton*)[view viewWithTag:103];
    popupView->errorMsg = (FontLabel*)[view viewWithTag:220];
    NSLog(@"errorMSg = %@", popupView->errorMsg);
    [popupView autorelease];
    UIImageView* dialogBg = (UIImageView*)[view viewWithTag:106];
    [SESystemConfig setImageWithCap:dialogBg name:@"OptionsWidgetSettingPowerBg"];
    [SESystemConfig setImageWithCap: popupView->title.background name:@"OptionsUserInfoSettingLevelTitleBg"];
    popupView.okButton.buttonText.text = @"Ok";
    popupView.cancelButton.buttonText.text = @"Cancel";
    [popupView.okButton setButtonBackground:@"SignatureSaveBg" :@"SignatureSaveSelectedBg"];
    [popupView.cancelButton setButtonBackground:@"SignatureSaveBg" :@"SignatureSaveSelectedBg"];
    [SESystemConfig setLabelFont:popupView->errorMsg text:@"" color:[UIColor redColor] fontName:[SESystemConfig getFontName] fontSize:18];
    [SESystemConfig setLabelFont:popupView->title.label text:@"" color:[UIColor whiteColor] fontName:[SESystemConfig getFontName] fontSize:30];
    return popupView;
}
- (void) setTitleText: (NSString*)str
{
    title.label.text = str;
    
}
- (void) setErrorMsgText: (NSString*) str
{
    //errorMsg.text = str;
    errorMsg.text = str;
    //[SESystemConfig setLabelFont:errorMsg text:str color:[UIColor whiteColor] fontName:[SESystemConfig getFontName] fontSize:18];
}
- (NSString*) getContentText
{
    return content.text;
}
- (void) setHandler: (id) target ok: (SEL) okOp cancel : (SEL) cancelOp
{
    [okButton.button addTarget:target action:okOp forControlEvents:UIControlEventTouchUpInside];
    [cancelButton.button addTarget:target action:cancelOp forControlEvents:UIControlEventTouchUpInside];
}
@end

@implementation SEConfirmDlg
@synthesize okButton;
@synthesize cancelButton;
@synthesize message;
@synthesize message2;
+ (SEConfirmDlg*) createFromNib:(NSString *)nibFile  withType: (CONFIRM_DLG_TYPE) t
{
    float width = [[PhotoFrameAppDelegate getViewNavigator] mViewPortWidth];
    float height = [[PhotoFrameAppDelegate getViewNavigator] mViewPortHeight];

    UIImageView* background = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0,width , height)];
    [SESystemConfig setImageWithCap:background name:@"OptionsLockBg"];
    UIView* view = [[[NSBundle mainBundle] loadNibNamed:nibFile owner:self options:nil] lastObject];
    view.backgroundColor = [UIColor clearColor];
    SEConfirmDlg* popupView = [[SEConfirmDlg alloc] initWithFrame:CGRectMake(0, 0, width, height) ];
    view.frame = CGRectMake((width - view.frame.size.width) / 2, (height - view.frame.size.height) / 2, view.frame.size.width, view.frame.size.height);
    popupView->type = t;
    [popupView addSubview:background];
    [background release];
    [popupView addSubview:view];
    /*
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    UIImage* normal = [viewNav.mResLoader getImage:@"AppStoreItemBg"];
    normal = [SEUtil imageWithCap:normal top:0.1 bottom:0.9 left:0.1 right:0.9];
    UIImage* select = [viewNav.mResLoader getImage:@"OptionsStoreItemSelectedBg"];
    select  = [ SEUtil imageWithCap:select top:0.1 bottom:0.9 left:0.1 right:0.9];
     */
    if(t == OK_CANCEL)
    {
        popupView->message = (SEPopupLabel*)[view viewWithTag:101];
        popupView->okButton = (SEPopupButton*)[view viewWithTag:103];
        popupView->cancelButton = (SEPopupButton*)[view viewWithTag:102];
        [SESystemConfig setLabelFont:popupView->message.label text:@"" color:[UIColor whiteColor] fontName:[SESystemConfig getFontName] fontSize:20];
        popupView.okButton.buttonText.text = @"Ok";
        popupView.cancelButton.buttonText.text = @"Cancel";
        [popupView.okButton setButtonBackground:@"SignatureSaveBg" :@"SignatureSaveSelectedBg"];
        [popupView.cancelButton setButtonBackground:@"SignatureSaveBg" :@"SignatureSaveSelectedBg"];
    }
    else if(t == OK)
    {
        popupView->message = (SEPopupLabel*)[view viewWithTag:101];
        popupView->message2 = (SEPopupLabel*)[view viewWithTag:102];
        popupView->okButton = (SEPopupButton*)[view viewWithTag:103];
        [SESystemConfig setLabelFont:popupView->message.label text:@"" color:[UIColor whiteColor] fontName:[SESystemConfig getFontName] fontSize:20];
        [SESystemConfig setLabelFont:popupView->message2.label text:@"" color:[UIColor whiteColor] fontName:[SESystemConfig getFontName] fontSize:20];
        popupView.okButton.buttonText.text = @"Ok";
        popupView.message2.label.lineBreakMode = UILineBreakModeWordWrap;
        popupView.message2.label.numberOfLines = 2;
        popupView.message2.label.textColor = [UIColor colorWithRed:54.0/255 green:54.0/255 blue:54.0/255 alpha:1.0];
        [popupView.okButton setButtonBackground:@"SignatureSaveBg" :@"SignatureSaveSelectedBg"];
    }
    UIImageView* dlgBg = (UIImageView*)[view viewWithTag:104];
    [SESystemConfig setImageWithCap:dlgBg name:@"OptionsWidgetSettingPowerBg"];
    [SESystemConfig setImageWithCap: popupView->message.background name:@"OptionsUserInfoSettingLevelTitleBg"];
    [popupView autorelease];
    return popupView;
}
- (void) setMessageText: (NSString*)str
{
    message.text = str;
}
- (void) setMessage2Text: (NSString*)str
{
    message2.text = str;
}
- (void) setHandler: (id) target ok: (SEL) okOp cancel : (SEL) cancelOp
{
    [okButton.button addTarget:target action:okOp forControlEvents:UIControlEventTouchUpInside];
    [cancelButton.button addTarget:target action:cancelOp forControlEvents:UIControlEventTouchUpInside];
}
@end
