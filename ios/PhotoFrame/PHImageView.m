//
//  PHImageView.m
//  PhotoFrame
//
//  Created by 陈勇 on 11-11-10.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import "PHImageView.h"
@implementation MyClipRect
@synthesize clipRect;
@end

@implementation PHImageView
@synthesize image;
- (id)init
{
    self = [super init];
    if (self) {
        // Initialization code here.
    
    }
    
    return self;
}
- (void) setClippingArea: (CGContextRef) con
{
    
    NSUInteger i;
    for(i = 0 ; i < [clippingRectList count] ; i++)
    {
        MyClipRect* cr = [clippingRectList objectAtIndex:i];
        CGRect r = cr->clipRect;
        CGContextAddRect(con, r);
    }
}
- (void)drawRect:(CGRect)rect
{
    NSLog(@"## ui image drawing rect = %@", rect);
    CGContextRef con = UIGraphicsGetCurrentContext();
    //CGImageRef imageRef = [image CGImage];
    if(clippingRectList)
    {
        CGContextSaveGState(con);
        NSUInteger i;
        for(i = 0 ; i < [clippingRectList count] ; i++)
        {
            MyClipRect* cr = [clippingRectList objectAtIndex:i];
            CGRect r = cr->clipRect;
            CGContextAddRect(con, r);
        }

        //CGContextDrawImage(con, rect, imageRef);
        [image drawInRect:rect];
        CGContextRestoreGState(con);
    }
    else
    {
        //CGContextDrawImage(con, rect, imageRef);
        [image drawInRect:rect];
    }
}
- (void) clearClippingList
{
    [clippingRectList release];
    clippingRectList = nil;
}
- (void) setClipRectList: (const MyClipRect**) clipRect count:(int)size
{
    [clippingRectList release];
    clippingRectList = nil;
    clippingRectList = [NSArray arrayWithObjects: clipRect count: size];
    [clippingRectList retain];
}
- (void)dealloc
{
    [image release];
    [clippingRectList release];
    [super dealloc];
}

@end
