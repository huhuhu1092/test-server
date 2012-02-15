//
//  SEUITableView.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-2-9.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEUITableView.h"
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
    [super touchesBegan:touches withEvent:event];
    [NSTimer scheduledTimerWithTimeInterval:0.5 target:self selector:@selector(targetMethod:)  userInfo: nil repeats:NO];
    mOrig = [[touches anyObject] locationInView:self];
    mSavePoint = mOrig;
    mHasLongPress = YES;
}
- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"SEUITableView touches move count = %u", [touches count]);
    
    CGPoint pos = [[touches anyObject] locationInView:self];
    float deltax = pos.x - mOrig.x;
    float deltay = pos.y - mOrig.y;
    if(fabsf(deltax) > 5 || fabsf(deltay) > 5)
    {
        mHasLongPress = NO;
    }
    
    deltax = pos.x - mSavePoint.x;
    deltay = pos.y - mSavePoint.y;
    CGPoint deltaP = CGPointMake(deltax, deltay);
    NSValue* vp = [NSValue valueWithCGPoint:deltaP];
    [mTouchMoveTarget performSelector:mTouchMoveAction withObject:vp];
    [super touchesMoved:touches withEvent:event];
    mSavePoint = pos;
}
- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"SEUITableView touches end count = %u", [touches count]);
    [super touchesEnded:touches withEvent:event];
    mHasLongPress = NO;
    [mTouchEndTarget performSelector:mTouchEndAction];
}
- (void)touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"SEUITableView touch cancel");
    [super touchesCancelled:touches withEvent:event];
    mHasLongPress = NO;
}
- (UIView *)hitTest:(CGPoint)point withEvent:(UIEvent *)event
{
    UIView* ret = [super hitTest: point withEvent:event];
    NSSet* set = [event touchesForView:self];
    UITouch* touch = [set anyObject];
    NSLog(@"touch type = %d", touch.phase);
    UIView* p = ret.superview;
    NSLog(@"hit view = %@, %d", p, ret.tag);
    mPressedView = (UITableViewCell*)p;
    return ret;
}
- (void) setLongPressTarget: (id) obj withAction: (SEL)action
{
    mLongPressTarget = obj;
    mLongPressAction = action;
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

- (void) saveGesture
{
    assert(mGesture ==nil);
    if(mGesture != nil)
        return;
    mGesture = [NSArray array];
    NSArray* gesArray = self.gestureRecognizers;
    for(UIGestureRecognizer* ges in gesArray)
    {
        mGesture = [mGesture arrayByAddingObject:ges];
    }
    [mGesture retain];
}
- (void) removeAllGestures
{
    NSArray* gesArray = self.gestureRecognizers;
    for(UIGestureRecognizer* ges in gesArray)
    {
        NSLog(@"### ges = %@", ges);
        //ges.cancelsTouchesInView = NO;
        [self removeGestureRecognizer:ges];
    }   
}
- (void) restoreGesture
{
    NSArray* oldArray = self.gestureRecognizers;
    assert([oldArray count] == 0);
    if([oldArray count] > 0)
        return;
    for(UIGestureRecognizer* ges in mGesture)
    {
        [self addGestureRecognizer:ges];
    }
    [mGesture release];
    mGesture = nil;
    
}
@end
