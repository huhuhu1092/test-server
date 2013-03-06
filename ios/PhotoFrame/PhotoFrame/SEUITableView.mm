//
//  SEUITableView.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-2-9.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEUITableView.h"
#import "SEMultiTouchDetect.h"
@implementation SEUITableViewCell
- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"SEUITableViewCell touches began count = %u", [touches count]);
    [super touchesBegan:touches withEvent:event];
}
- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"SEUITableViewCell touches move count = %u", [touches count]);
    
    
    [super touchesMoved:touches withEvent:event];
}
- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"SEUITableViewCell touches end count = %u", [touches count]);
    [super touchesEnded:touches withEvent:event];
}
- (void)touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"SEUITableViewCell touch cancel");
    [super touchesCancelled:touches withEvent:event];
    
}

- (id)initWithStyle:(UITableViewCellStyle)style reuseIdentifier:(NSString *)reuseIdentifier
{
    if (self = [super initWithStyle:style reuseIdentifier:reuseIdentifier])
    {
        //self.accessoryType = UITableViewCellAccessoryDetailDisclosureButton;
        
        // cell's title label
    }
    return self;
}

- (void)layoutSubviews
{
    [super layoutSubviews];
    
    CGRect contentRect = [self.contentView bounds];
    
    //CGRect frame = CGRectMake(contentRect.origin.x + 40.0, 8.0, contentRect.size.width, 100.0);
    //self.textLabel.frame = frame;
    
    // layout the check button image
}

- (void)dealloc
{
    [super dealloc];
}

// called when the checkmark button is touched 
- (void)checkAction:(id)sender
{
    // note: we don't use 'sender' because this action method can be called separate from the button (i.e. from table selection)
    NSLog(@"checkAction");
}

@end

///////////////////////////
@implementation SEUITableView
+ (NSArray*) getFeedChar
{
    NSArray* ret = [NSArray arrayWithObjects:[NSNumber numberWithUnsignedChar:'t'], [NSNumber numberWithUnsignedChar:'h'], [NSNumber numberWithUnsignedChar:'E' ], nil];
    return ret;
}

- (void) longPressHandler: (UILongPressGestureRecognizer*)longPress
{
    NSLog(@"long press");
}
- (id)init
{
    self = [super init];
    if (self) {
        // Initialization code here.
        mGesture = [NSArray array];
        [mGesture retain];
    }
    
    return self;
}
- (void) targetMethod: (NSTimer*)timer
{
    if(mHasLongPress)
    {
        NSLog(@"long press ok");
        CGRect frame = mPressedView.frame;
        CGRect newframe = CGRectMake(frame.origin.x, frame.origin.y - self.contentOffset.y, frame.size.width, frame.size.height);

        NSValue* value = [NSValue valueWithCGRect:newframe];
        [mLongPressTarget performSelector:mLongPressAction withObject:value];
    }
    else
        NSLog(@"long press cancel");
}
- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"SEUITableView touches began count = %u", [touches count]);
    [mMultiTouchDetect touchStateChange:touches];
    if(mMultiTouchDetect.mTouchState == TOUCH1)
    {
        
    }
    else if(mMultiTouchDetect.mTouchState == TOUCH2 && mSameTouchedView)
    {
        mMultiTouchBegan = YES;
    }
    if(mMultiTouchBegan)
    {
        [mTouchBeganTarget performSelector:mTouchBeganAction withObject:mPressedView];
    }
    else
        [super touchesBegan:touches withEvent:event];

}
- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"SEUITableView touches move count = %u", [touches count]);

    [mMultiTouchDetect touchStateChange:touches];
    
    if(mMultiTouchBegan)
    {
        CGPoint p = [mMultiTouchDetect getCurrentDeltaPoint];
        NSValue* v = [NSValue valueWithCGPoint:p];
        [mTouchMoveTarget performSelector:mTouchMoveAction withObject:v];
    }
    else
        [super touchesMoved:touches withEvent:event];
    //mSavePoint = pos;
}
- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"SEUITableView touches end count = %u", [touches count]);
    [mMultiTouchDetect touchStateChange:touches];
    
    if(mMultiTouchBegan)
    {
        mMultiTouchBegan = NO;
        [mTouchEndTarget performSelector:mTouchEndAction];
    }
    else
        [super touchesEnded:touches withEvent:event];
    mSameTouchedView = NO;
    mPressedView = nil;
    //mHasLongPress = NO;
    //[mTouchEndTarget performSelector:mTouchEndAction];
}
- (void)touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"SEUITableView touch cancel");
    [mMultiTouchDetect touchStateChange:touches];
    
    if(mMultiTouchBegan)
    {
        mMultiTouchBegan = NO;
        [mTouchEndTarget performSelector:mTouchEndAction];
    }
    else
        [super touchesEnded:touches withEvent:event];
    mSameTouchedView = NO;
    mPressedView = nil;

}

- (UIView *)hitTest:(CGPoint)point withEvent:(UIEvent *)event
{
    UIView* ret = [super hitTest: point withEvent:event];
    UIView* p = ret.superview;
    if(p != nil)
    {
        //NSLog(@"hit view = %@, %@", p, ret);
        if(mPressedView == nil)
        {
            mPressedView = (UITableViewCell*)p;
            NSSet* touches = [event touchesForView:p];
            int count = [touches count];
            //NSLog(@"hit view touch count = %d", count);
            if(count == 2)
            {
                mSameTouchedView = YES;
            }
            else
                mSameTouchedView = NO;
        }
        else
        {
            if(mPressedView == p)
            {
                mSameTouchedView = YES;
            }
            else
            {
                mSameTouchedView = NO;
            }
        }
    }
    return ret;
}
- (void) setLongPressTarget: (id) obj withAction: (SEL)action
{
    mLongPressTarget = obj;
    mLongPressAction = action;
}
- (void) setTouchBeganTarget:(id)obj withAction:(SEL)action
{
    mTouchBeganTarget = obj;
    mTouchBeganAction = action;
}
- (void) setTouchMoveTarget: (id) obj withAction:(SEL) action
{
    mTouchMoveAction = action;
    mTouchMoveTarget = obj;
}
- (void) setTouchEndTarget: (id) obj withAction:(SEL)action
{
    mTouchEndAction = action;
    mTouchEndTarget = obj;
}
- (void) initData
{
    mMultiTouchDetect = [[SEMultiTouchDetect alloc] init];
    mMultiTouchDetect.mViewForPoint = self;
    [mMultiTouchDetect initData];
}
- (void) dealloc
{
    [mMultiTouchDetect release];
    [super dealloc];
}
- (void) saveGesture
{
}
- (void) disableAllGestures
{
    NSArray* gesArray = self.gestureRecognizers;
    for(UIGestureRecognizer* ges in gesArray)
    {
        ges.enabled = NO;
    }   
}
- (void) enableAllGestures
{
    NSArray* gestures = self.gestureRecognizers;
    for(UIGestureRecognizer* ges in gestures)
    {
        ges.enabled = YES;
    }
    
}
- (void) handleInMid
{}
- (void) handleOnEdge
{}
@end
