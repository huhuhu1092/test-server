//
//  PHImageView.h
//  PhotoFrame
//
//  Created by 陈勇 on 11-11-10.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
@interface MyClipRect : NSObject
{
@public
    CGRect clipRect;
}
@property (nonatomic) CGRect clipRect;
@end

@interface PHImageView : UIView
{
    NSArray* clippingRectList;
    UIImage* image;
    NSMutableArray* pointArrayList;
    CGFloat lineWidth;
}
@property (nonatomic, retain) UIImage* image;
- (void)drawRect:(CGRect)rect;
- (void) clearClippingList;
- (void) setClipRectList: (const MyClipRect**) clipRect count:(int)size;
- (void) setPoints:(NSArray*)points;
@end
