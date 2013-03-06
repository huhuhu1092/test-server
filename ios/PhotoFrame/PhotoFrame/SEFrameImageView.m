//
//  SEFrameImageView.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-9-28.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEFrameImageView.h"

@implementation SEFrameImageView

@synthesize alpha;
@synthesize highlightedFrameImage = mHighlightedFrameImage;
@synthesize frameImage = mFrameImage;
- (void) dealloc
{
    [mContentImage release];
    [mFrameImage release];
    [mHighlightedFrameImage release];
    [super dealloc];
}

- (BOOL) highlighted
{
    return highlighted;
}
- (void) setHighlighted:(BOOL)h
{
    if(highlighted != h)
    {
        highlighted = h;
        [self setNeedsDisplay];
    }
}
- (UIImage*) image
{
    return mContentImage;
}
- (void) setImage:(UIImage *)image
{
    [mContentImage release];
    mContentImage = [image retain];
    [self setNeedsDisplay];
}
- (id) initWithCoder:(NSCoder *)aDecoder
{
    self = [super initWithCoder:aDecoder];
    if(self)
    {
        mFrameImage = nil;
        mContentImage = nil;
        alpha = 1.0;
    }
    return self;
}
- (id) initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if(self)
    {
        mFrameImage = nil;
        mContentImage = nil;
        alpha = 1.0;
    }
    return self;
}
- (CGSize) contentSize
{
    return mContentSize;//CGSizeMake(self.frame.size.width - 20, self.frame.size.height - 20);
}
- (void) setContentSize:(CGSize)contentSize
{
    mContentSize = contentSize;
}
- (void) drawRect:(CGRect)rect
{
    if(mContentImage == nil)
        return;
    if(mContentImage.size.width == rect.size.width && mContentImage.size.height == rect.size.height)
    {
        if(alpha == 1.0)
        {
            [mContentImage drawInRect:rect];
        }
        else
        {
            [mContentImage drawInRect:rect blendMode:kCGBlendModeSourceAtop alpha:alpha];
        }
        return;
    }
    CGFloat startx = (rect.size.width - mContentImage.size.width) / 2;
    CGFloat starty = (rect.size.height - mContentImage.size.height) / 2;
    CGBlendMode blendMode = kCGBlendModeSourceAtop;
    [mContentImage drawAtPoint:CGPointMake(startx, starty)];
    UIImage* frameImage = nil;
    if(mFrameImage != nil && self.highlighted == NO)
    {
        frameImage = mFrameImage;
    }
    else if(mHighlightedFrameImage != nil && self.highlighted == YES)
    {
        frameImage = mHighlightedFrameImage;
    }
    if(frameImage == nil)
        return;
    //CGFloat topPadding = 3, leftPadding = 3;
    UIGraphicsBeginImageContext(CGSizeMake(9, 9));
    [frameImage drawAtPoint:CGPointMake(0, 0)];
    UIImage* leftTopCornerImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    if(alpha == 1.0)
    {
        [leftTopCornerImage drawAtPoint:CGPointMake(startx - 9, starty - 9)];
    }
    else
    {
        [leftTopCornerImage drawAtPoint:CGPointMake(startx - 9, starty - 9) blendMode:blendMode alpha:alpha];
    }
    
    UIGraphicsBeginImageContext(CGSizeMake(mContentImage.size.width, 9));
    [frameImage drawAtPoint:CGPointMake(-9, 0)];
    UIImage* topImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    if(alpha == 1.0)
    {
        [topImage drawAtPoint:CGPointMake(startx, starty - 9)];
    }
    else
    {
        [topImage drawAtPoint:CGPointMake(startx, starty - 9) blendMode:blendMode alpha:alpha];
    }
    
    UIGraphicsBeginImageContext(CGSizeMake(11, 11));
    [frameImage drawAtPoint:CGPointMake(-frameImage.size.width + 11, 0)];
    UIImage* rightTopImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    if(alpha == 1.0)
    {
        [rightTopImage drawAtPoint:CGPointMake(startx + mContentImage.size.width, starty - 9)];
    }
    else
    {
        [rightTopImage drawAtPoint:CGPointMake(startx + mContentImage.size.width, starty - 9) blendMode:blendMode alpha:alpha];
    }
    
    UIGraphicsBeginImageContext(CGSizeMake(9, mContentImage.size.height));
    [frameImage drawAtPoint:CGPointMake(0, -9)];
    UIImage* leftImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    if(alpha == 1.0)
    {
        [leftImage drawAtPoint:CGPointMake(startx - 9, starty)];
    }
    else
    {
        [leftImage drawAtPoint:CGPointMake(startx - 9, starty) blendMode:blendMode alpha:alpha];
    }
    
    UIGraphicsBeginImageContext(CGSizeMake(11, mContentImage.size.height));
    [frameImage drawAtPoint:CGPointMake(-frameImage.size.width + 11, -9)];
    UIImage* rightImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    if(alpha == 1.0)
    {
        [rightImage drawAtPoint:CGPointMake(startx + mContentImage.size.width, starty)];
    }
    else
    {
        [rightImage drawAtPoint:CGPointMake(startx + mContentImage.size.width, starty) blendMode:blendMode alpha:alpha];
    }
    
    UIGraphicsBeginImageContext(CGSizeMake(9, 11));
    [frameImage drawAtPoint:CGPointMake(0, -frameImage.size.height + 11)];
    UIImage* leftBottomImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    if(alpha == 1.0)
    {
        [leftBottomImage drawAtPoint:CGPointMake(startx - 9, starty + mContentImage.size.height)];
    }
    else
    {
        [leftBottomImage drawAtPoint:CGPointMake(startx - 9, starty + mContentImage.size.height) blendMode:blendMode alpha:alpha];
    }
    
    UIGraphicsBeginImageContext(CGSizeMake(mContentImage.size.width, 11));
    [frameImage drawAtPoint:CGPointMake(-9, -frameImage.size.height + 11)];
    UIImage* bottomImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    if(alpha == 1.0)
    {
        [bottomImage drawAtPoint:CGPointMake(startx, starty + mContentImage.size.height)];
    }
    else
    {
        [bottomImage drawAtPoint:CGPointMake(startx, starty + mContentImage.size.height) blendMode:blendMode alpha:alpha];
    }
    
    UIGraphicsBeginImageContext(CGSizeMake(11, 11));
    [frameImage drawAtPoint:CGPointMake(-frameImage.size.width + 11, -mFrameImage.size.height + 11)];
    UIImage* rightBottomImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    if(alpha == 1.0)
    {
        [rightBottomImage drawAtPoint:CGPointMake(startx + mContentImage.size.width, starty + mContentImage.size.height)];
    }
    else
    {
        [rightBottomImage drawAtPoint:CGPointMake(startx + mContentImage.size.width, starty + mContentImage.size.height) blendMode:blendMode alpha:alpha];
    }
}


@end
