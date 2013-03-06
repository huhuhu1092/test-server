//
//  SEFrameImageView.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-9-28.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface SEFrameImageView : UIView
{
@private
    UIImage* mContentImage;
    UIImage* mFrameImage;
    UIImage* mHighlightedFrameImage;
    CGSize mContentSize; // this is content size which will contain mContentImage
    CGFloat alpha;
    BOOL highlighted;
}
@property (nonatomic, retain) UIImage* image;
@property (nonatomic, retain) UIImage* frameImage;
@property (nonatomic, retain) UIImage* highlightedFrameImage;
@property (nonatomic, assign) CGSize contentSize;
@property (nonatomic, assign) CGFloat alpha;
@property (nonatomic, assign) BOOL highlighted;
@end
