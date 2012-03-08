//
//  SEMusicImageListPopup.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-3-8.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface SEMusicImageListPopup : UIViewController
{
    UILabel* title;
    UIButton* okButton;
    UIButton* cancelButton;
    UITextField* content;
    UILabel* errorMsg;
}
@property (nonatomic, readonly)UILabel* title;
@property (nonatomic, readonly)UILabel* errorMsg;
@property (nonatomic, readonly)UITextField* content;
@property (nonatomic, readonly)UIButton* okButton;
@property (nonatomic, readonly)UIButton* cancelButton;
@end
