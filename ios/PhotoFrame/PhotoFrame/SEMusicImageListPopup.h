//
//  SEMusicImageListPopup.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-3-8.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
@class FontLabel;
@class SEPopupLabel;
@class SEPopupButton;
@interface SEMusicImageListPopupView : UIView
{
    SEPopupLabel* title;
    SEPopupButton* okButton;
    SEPopupButton* cancelButton;
    UITextField* content;
    FontLabel* errorMsg;
}
@property (nonatomic, readonly)SEPopupLabel* title;
@property (nonatomic, readonly)FontLabel* errorMsg;
@property (nonatomic, readonly)UITextField* content;
@property (nonatomic, readonly)SEPopupButton* okButton;
@property (nonatomic, readonly)SEPopupButton* cancelButton;
+ (SEMusicImageListPopupView*) createFromNib: (NSString*)nibFile;
- (void) setTitleText: (NSString*)str;
- (void) setErrorMsgText: (NSString*) str;
- (NSString*) getContentText;
- (void) setHandler: (id) target ok: (SEL) okOp cancel : (SEL) cancelOp;
@end

@interface SEConfirmDlg : UIView    
{
    enum CONFIRM_DLG_TYPE {OK_CANCEL, OK};
    SEPopupButton* okButton;
    SEPopupButton* cancelButton;
    SEPopupLabel* message;
    SEPopupLabel* message2;
    CONFIRM_DLG_TYPE type;
}
@property (nonatomic, readonly) SEPopupButton* okButton;
@property (nonatomic, readonly) SEPopupButton* cancelButton;
@property (nonatomic, readonly) SEPopupLabel* message;
@property (nonatomic, readonly) SEPopupLabel* message2;
+ (SEConfirmDlg*) createFromNib: (NSString*)nibFile withType: (CONFIRM_DLG_TYPE) t;
- (void) setMessageText: (NSString*)str;
- (void) setMessage2Text: (NSString*)str;
- (void) setHandler: (id) target ok: (SEL) okOp cancel : (SEL) cancelOp;
@end