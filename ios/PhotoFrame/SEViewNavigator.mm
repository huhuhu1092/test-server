//
//  SEViewNavigator.mm
//  PhotoFrame
//
//  Created by 陈勇 on 12-1-6.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEViewNavigator.h"
#import "SEUIScrollView.h"
#import "SEResDefine.h"
#import "SEUtil.h"
#import "SelectedImage.h"
#import "UserInfo.h"
#import "SEUITableView.h"
@implementation SEUIRootView
- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"root touches began count = %u", [touches count]);
    
}
- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"root touches move count = %u", [touches count]);

    
}
- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"root touches end count = %u", [touches count]);
}
- (void)touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"root touch cancel");
}

@end
/////////
////////////////////
struct ViewRelation
{
    VIEW_TYPE curr;
    VIEW_TYPE prev;
    VIEW_TYPE next;
};
struct ViewRelation gViewRelation[] = {
    {IMAGE_PICKER, INVALID_VIEW, SELECTED_IMAGE_VIEW},
    {SELECTED_IMAGE_VIEW, IMAGE_PICKER, MUSIC_PICKER},
    {MUSIC_PICKER, SELECTED_IMAGE_VIEW, INVALID_VIEW}
}; 
static VIEW_TYPE getPrevView(VIEW_TYPE curr)
{
    int count = sizeof(gViewRelation) / sizeof(ViewRelation);
    for(int i = 0 ; i < count ; i++)
    {
        if(gViewRelation[i].curr == curr)
            return gViewRelation[i].prev;
    }
    return INVALID_VIEW;
}
static VIEW_TYPE getNextView(VIEW_TYPE curr)
{
    int count = sizeof(gViewRelation) / sizeof(ViewRelation);
    for(int i = 0 ; i < count ; i++)
    {
        if(gViewRelation[i].curr == curr)
            return gViewRelation[i].next;
    }
    return INVALID_VIEW;    
}
///

static BarViewType gBarViewType[] = {
    {IMAGE_PICKER, SELECTED_IMAGE_VIEW},
    {SELECTED_IMAGE_VIEW, MUSIC_PICKER},
}; 
static BOOL isBarViewTypeEqual(BarViewType bv1, BarViewType bv2)
{
    return bv1.leftView == bv2.leftView && bv1.rightView == bv2.rightView;
}
static int getBarViewTypeCount()
{
    return sizeof(gBarViewType) / sizeof(BarViewType);
}
static BarViewType getBarViewType(VIEW_TYPE v1, VIEW_TYPE v2)
{
    int count = sizeof(gBarViewType) / sizeof(BarViewType);
    for(int i = 0 ; i < count ; i++)
    {
        BarViewType bvt = gBarViewType[i];
        if(bvt.leftView == v1 && bvt.rightView == v2)
            return bvt;
    }
    BarViewType bvt;
    bvt.leftView = INVALID_VIEW;
    bvt.rightView = INVALID_VIEW;
    return bvt;
}
static BOOL isBarViewTypeValid(BarViewType bvt)
{
    return bvt.leftView != INVALID_VIEW || bvt.rightView != INVALID_VIEW;
}
/////////
VIEW_TYPE gViewSequence[] = {IMAGE_PICKER, SELECTED_IMAGE_VIEW, MUSIC_PICKER};
static int getViewIndexInSequence(VIEW_TYPE vp)
{
    int count = sizeof(gViewSequence) / sizeof(VIEW_TYPE);
    for(int i = 0 ; i < count ; i++)
    {
        if(vp == gViewSequence[i])
            return i;
    }
    return -1;
}
////////
@implementation SEMusicPickerDelegate

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    return 1;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    return 10;
}

// Customize the appearance of table view cells.
- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    static NSString *CellIdentifier = @"Cell";
    
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier];
    if (cell == nil) {
        cell = [[[UITableViewCell alloc] initWithStyle:UITableViewCellStyleDefault reuseIdentifier:CellIdentifier] autorelease];
    }
    cell.textLabel.text = @"aaabbbccccccccc";
    // Configure the cell.
    return cell;
}

/*
 // Override to support conditional editing of the table view.
 - (BOOL)tableView:(UITableView *)tableView canEditRowAtIndexPath:(NSIndexPath *)indexPath
 {
 // Return NO if you do not want the specified item to be editable.
 return YES;
 }
 */

/*
 // Override to support editing the table view.
 - (void)tableView:(UITableView *)tableView commitEditingStyle:(UITableViewCellEditingStyle)editingStyle forRowAtIndexPath:(NSIndexPath *)indexPath
 {
 if (editingStyle == UITableViewCellEditingStyleDelete)
 {
 // Delete the row from the data source.
 [tableView deleteRowsAtIndexPaths:[NSArray arrayWithObject:indexPath] withRowAnimation:UITableViewRowAnimationFade];
 }
 else if (editingStyle == UITableViewCellEditingStyleInsert)
 {
 // Create a new instance of the appropriate class, insert it into the array, and add a new row to the table view.
 }   
 }
 */

/*
 // Override to support rearranging the table view.
 - (void)tableView:(UITableView *)tableView moveRowAtIndexPath:(NSIndexPath *)fromIndexPath toIndexPath:(NSIndexPath *)toIndexPath
 {
 }
 */

/*
 // Override to support conditional rearranging of the table view.
 - (BOOL)tableView:(UITableView *)tableView canMoveRowAtIndexPath:(NSIndexPath *)indexPath
 {
 // Return NO if you do not want the item to be re-orderable.
 return YES;
 }
 */

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath
{
    /*
     <#DetailViewController#> *detailViewController = [[<#DetailViewController#> alloc] initWithNibName:@"<#Nib name#>" bundle:nil];
     // ...
     // Pass the selected object to the new view controller.
     [self.navigationController pushViewController:detailViewController animated:YES];
     [detailViewController release];
     */
}


@end
@implementation SEMusicSelectedViewDelegate

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    return 1;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    return 10;
}

// Customize the appearance of table view cells.
- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    static NSString *CellIdentifier = @"Cell";
    
    SEUITableViewCell *cell = (SEUITableViewCell*)[tableView dequeueReusableCellWithIdentifier:CellIdentifier];
    if (cell == nil) {
        cell = [[[SEUITableViewCell alloc] initWithStyle:UITableViewCellStyleDefault reuseIdentifier:CellIdentifier] autorelease];
    }
    cell.tag = indexPath.row;
    cell.textLabel.text = @"cccadfeasdf";
    // Configure the cell.
    return cell;
}

/*
 // Override to support conditional editing of the table view.
 - (BOOL)tableView:(UITableView *)tableView canEditRowAtIndexPath:(NSIndexPath *)indexPath
 {
 // Return NO if you do not want the specified item to be editable.
 return YES;
 }
 */

/*
 // Override to support editing the table view.
 - (void)tableView:(UITableView *)tableView commitEditingStyle:(UITableViewCellEditingStyle)editingStyle forRowAtIndexPath:(NSIndexPath *)indexPath
 {
 if (editingStyle == UITableViewCellEditingStyleDelete)
 {
 // Delete the row from the data source.
 [tableView deleteRowsAtIndexPaths:[NSArray arrayWithObject:indexPath] withRowAnimation:UITableViewRowAnimationFade];
 }
 else if (editingStyle == UITableViewCellEditingStyleInsert)
 {
 // Create a new instance of the appropriate class, insert it into the array, and add a new row to the table view.
 }   
 }
 */

/*
 // Override to support rearranging the table view.
 - (void)tableView:(UITableView *)tableView moveRowAtIndexPath:(NSIndexPath *)fromIndexPath toIndexPath:(NSIndexPath *)toIndexPath
 {
 }
 */

/*
 // Override to support conditional rearranging of the table view.
 - (BOOL)tableView:(UITableView *)tableView canMoveRowAtIndexPath:(NSIndexPath *)indexPath
 {
 // Return NO if you do not want the item to be re-orderable.
 return YES;
 }
 */

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath
{
    /*
     <#DetailViewController#> *detailViewController = [[<#DetailViewController#> alloc] initWithNibName:@"<#Nib name#>" bundle:nil];
     // ...
     // Pass the selected object to the new view controller.
     [self.navigationController pushViewController:detailViewController animated:YES];
     [detailViewController release];
     */
}


@end
////
@implementation SEContentViewContainer
@synthesize mType;
@synthesize mViewNav;
- (BOOL) hasContent
{
    return [self.subviews count] > 0;
}
- (void) adjustContentViewLayout: (float) contentViewWidth withRestart: (BOOL)restart
{
    UIView<SEAdjustContentView>* contentView = [self.subviews objectAtIndex:0];
    CGRect frame = contentView.frame;
    if(contentViewWidth == frame.size.width)
        return;
    if(contentViewWidth != self.frame.size.width)
    {
        if(mType == IMAGE_PICKER)
        {
            contentView.frame = CGRectMake(self.frame.size.width - contentViewWidth, frame.origin.y, contentViewWidth, frame.size.height);
        }
        else if(mType == SELECTED_IMAGE_VIEW)
        {
            contentView.frame = CGRectMake(0, 0, contentViewWidth, frame.size.height);
        }
    }
    else
    {
        contentView.frame = self.bounds;
    }
    if([contentView isMemberOfClass:[SEUIScrollView class]])
    {
        SEUIScrollView* scrollView = (SEUIScrollView*)contentView;
        scrollView.mViewWidth = contentViewWidth;
        if(restart)
        {
            [scrollView relayout];
        }
    }
}
- (UIView<SEAdjustContentView>*) contentView
{
    if([self.subviews count] > 0)
    {
        return [self.subviews objectAtIndex:0];
    }
    else
        return nil;
}
- (void) saveContext : (NSArray*)userInfoArray
{
    switch (mType) 
    {
        case IMAGE_PICKER:
        {}
            break;
        case SELECTED_IMAGE_VIEW:
        {
            UIView* contentView = [self contentView];
            SEUIScrollView* scrollView = (SEUIScrollView*)contentView;
            NSUInteger imageURLCount = [scrollView getImageURLNum];
            for(NSUInteger i = 0 ; i < imageURLCount ; i++
                )
            {
                SEImageURL* url = [scrollView getImageURL:i];
                SelectedImage* si = [mViewNav getSelectedImageProperty:i];
                NSURL* imageUrl = url.url;
                si.url = [imageUrl absoluteString];
                NSURL* filePath = url.filepath;
                si.filepath = [filePath absoluteString
                               ];
            }
        }
            break;
        default:
            break;
    }
}
- (SEUIImageView*) intersectContentChild: (CGPoint) point outIndex: (int*)childIndex
{
    UIView<SEAdjustContentView>* contentView = [self contentView];
    //point.x -= contentView.frame.origin.x;
    //point.y -= contentView.frame.origin.y;
    SEUIScrollView* scrollView = (SEUIScrollView*)contentView;
    //point.x += scrollView.contentOffset.x;
    //point.y += scrollView.contentOffset.y;
    HitProperty hp = [scrollView hitRect:point];
    SEUIImageView* imageView = hp.imageView;
    *childIndex = hp.index;
    if(imageView)
    {
        imageView.backgroundColor= [UIColor redColor];
        imageView.mImageView.opaque = 0.5;
        
    }
    return imageView;
}
- (BOOL) canStopInMid
{
    return YES;
}
- (void) initContainer
{}
@end
///////////
@interface SEFixedView : UIView <SEAdjustContentView>

@end
@implementation SEFixedView

-(BOOL) canAdjust
{
    return NO;
}

- (void) relayout
{}
@end
//////////////
#define FIRST_CONTENTVIEW_WIDTH 0
#define MID_CONTENTVIEW_WIDTH 1024 / 2

@implementation SEBarView
@synthesize mLeftContentContainer;
@synthesize mRightContentContainer;
@synthesize mViewNav;
@synthesize mBarViewType;
@synthesize mCanStopInMid;
- (BAR_LOC_TYPE) barType
{
    if(mLeftContentContainer.mType == mViewNav.mPrevView)
        return LEFT_BAR;
    else if(mRightContentContainer.mType == mViewNav.mNextView)
        return RIGHT_BAR;
    else
    {
        assert(0);
        return INVALID_BAR;
    }
}
- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{
    //NSLog(@"touches began count = %u", [touches count]);
    mOrig = [[touches anyObject] locationInView:mViewNav.mRootView];
    BAR_LOC_TYPE barType = [self barType];
    if(barType == LEFT_BAR)
    {
        [mViewNav addContentToContentContainer:mLeftContentContainer.mType];
        mLeftContentContainer.hidden = NO;
        
    }
    else if(barType == RIGHT_BAR)
    {
        [mViewNav addContentToContentContainer:mRightContentContainer.mType];
        mRightContentContainer.hidden = NO;
    }
}
- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event
{
    //NSLog(@"touches move count = %u", [touches count]);
    CGPoint loc = [[touches anyObject] locationInView:mViewNav.mRootView];
    CGFloat deltax = loc.x - mOrig.x;
    mOrig = loc;
    if(deltax > 0)
    {
        mDirect = MOVE_RIGHT;
    }
    else if(deltax < 0)
    {
        mDirect = MOVE_LEFT;
    }
    CGPoint p = [self convertPoint:CGPointMake(0, 0) toView:mViewNav.mRootView];
    float currentx = p.x + deltax;
    float currentend = p.x + self.frame.size.width + deltax;
    if(currentx < 0 || currentend > mViewNav.mViewPortWidth)
    {
        return;
    }
    [mLeftContentContainer adjustContentViewLayout:mLeftContentContainer.frame.size.width withRestart:YES];
    [mRightContentContainer adjustContentViewLayout:mRightContentContainer.frame.size.width withRestart:YES];

    CGPoint c = mViewNav.mContentView.center;
    c.x += deltax;
    mViewNav.mContentView.center = c;
}
- (CGPoint) pointInScreen: (CGPoint) point
{
    return [self convertPoint:point toView:mViewNav.mRootView];
}
- (void) barUpHandler
{
    CGPoint currBarStartPoint = [self pointInScreen:CGPointMake(0, 0)];
    BOOL stopInMid = NO;
    switch (mDirect)
    {
        case MOVE_LEFT:
        {
            float dist = 0;
            if(self.mCanStopInMid)
            {
                if(currBarStartPoint.x > MID_CONTENTVIEW_WIDTH && currBarStartPoint.x < mViewNav.mViewPortWidth)
                {
                    dist = currBarStartPoint.x + mViewNav.mBarWidth / 2 - MID_CONTENTVIEW_WIDTH;
                    stopInMid = YES;
                }
                else if(currBarStartPoint.x < MID_CONTENTVIEW_WIDTH && currBarStartPoint.x > 0)
                {
                    dist = currBarStartPoint.x;
                }
            }
            else
            {
                dist = currBarStartPoint.x;
            }
            if(dist != 0)
            {
                CGPoint p = mViewNav.mContentView.center;
                p.x -= dist;
                NSLog(@"move dist = %f", dist);
                void (^animBlock) (void) = ^{
                    mViewNav.mContentView.center = p;
                };
                void (^animEnd) (BOOL) = ^(BOOL f)
                {
                    if(stopInMid)
                    {
                        float contentWidth = (mViewNav.mViewPortWidth - mViewNav.mBarWidth) / 2;
                        if([mLeftContentContainer.contentView canAdjust])
                        {
                            [mLeftContentContainer adjustContentViewLayout:contentWidth withRestart:YES];
                        }
                        if([mRightContentContainer.contentView canAdjust])
                        {
                            [mRightContentContainer adjustContentViewLayout:contentWidth withRestart:YES];
                        }
                    }
                    else
                    {
                        if([mRightContentContainer.contentView canAdjust])
                        {
                            [mRightContentContainer adjustContentViewLayout:mRightContentContainer.frame.size.width withRestart:YES];
                        }
                        [mViewNav setCurrentView:mRightContentContainer.mType];
                    }
                    
                };
                [UIView animateWithDuration:0.5 delay: 0 options: UIViewAnimationOptionCurveLinear animations:animBlock completion:animEnd];
            }
            else
            {
                NSLog(@"dist = 0");
                [mViewNav setCurrentView:mRightContentContainer.mType];
            }
            
        }
            break;
        case MOVE_RIGHT:
        {
            float dist = 0;
            if(self.mCanStopInMid)
            {
                if(currBarStartPoint.x > FIRST_CONTENTVIEW_WIDTH && currBarStartPoint.x < MID_CONTENTVIEW_WIDTH)
                {
                    dist = MID_CONTENTVIEW_WIDTH - (currBarStartPoint.x + mViewNav.mBarWidth / 2);
                    stopInMid = YES;
                }
                else if(currBarStartPoint.x > MID_CONTENTVIEW_WIDTH && (currBarStartPoint.x + mViewNav.mBarWidth)<= mViewNav.mViewPortWidth)
                {
                    dist = mViewNav.mViewPortWidth - (currBarStartPoint.x + mViewNav.mBarWidth);
                }
            }
            else
            {
                dist = mViewNav.mViewPortWidth - (currBarStartPoint.x + mViewNav.mBarWidth);
            }
            if(dist != 0)
            {
                
                CGPoint p = mViewNav.mContentView.center;
                p.x += dist;
                void (^animBlock) (void) = ^{
                    mViewNav.mContentView.center = p;
                };
                void (^animEnd) (BOOL) = ^(BOOL f){
                    if(stopInMid)
                    {
                        float contentWidth = (mViewNav.mViewPortWidth - mViewNav.mBarWidth) / 2;
                        if([mLeftContentContainer.contentView canAdjust])
                        {
                            [mLeftContentContainer adjustContentViewLayout:contentWidth withRestart:YES];
                        }
                        if([mRightContentContainer.contentView canAdjust])
                        {
                            [mRightContentContainer adjustContentViewLayout:contentWidth withRestart:YES];
                        }
                    }
                    else
                    {
                        if([mLeftContentContainer.contentView canAdjust])
                        {
                            [mLeftContentContainer adjustContentViewLayout:mLeftContentContainer.frame.size.width withRestart:YES];
                        }
                        [mViewNav setCurrentView:mLeftContentContainer.mType];
                    }
                    
                };
                [UIView animateWithDuration:0.5 delay: 0
                                    options: UIViewAnimationOptionCurveLinear animations:animBlock
                                 completion:animEnd];
            }
            else
            {
                NSLog(@"dist = 0");
                [mViewNav setCurrentView:mLeftContentContainer.mType];
            }
        }
            break;
        case NO_MOVE:
        {}
            break;
        default:
            break;
    }
    
}

- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event
{
    //NSLog(@"touches end count = %u", [touches count]);
    [self barUpHandler];
}
- (void)touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event
{
    ///NSLog(@"touch cancel");
}
@end
////////////////////
//////
/////////////////
@interface SEMusicPickerContainer : SEContentViewContainer
{
    SEUITableView* picker;
    SEUITableView* selectedMusicView;
    SEUITableViewCell* mSelectedTableViewCell;
}

- (void) createSelectedTableViewCell: (NSValue*) frame;
- (void) touchMoveHandler: (NSValue*) vDeltap;
- (void) touchEndHandler;
@end

@implementation SEMusicPickerContainer
- (void) initContainer
{
    UIView* v = [self.subviews objectAtIndex:0];
    picker = [v.subviews objectAtIndex:0];
    selectedMusicView = [v.subviews objectAtIndex:1];
}
- (void) touchMoveHandler: (NSValue*) vDeltap
{
    CGPoint deltap = [vDeltap CGPointValue];
    if(mSelectedTableViewCell)
    {
        CGPoint c = mSelectedTableViewCell.center;
        c.x += deltap.x;
        c.y += deltap.y;
        mSelectedTableViewCell.center = c;
    }
}
- (void) touchEndHandler
{
    if(mSelectedTableViewCell)
    {
        [picker restoreGesture];
    }
    [mSelectedTableViewCell removeFromSuperview];
    //[mSelectedTableViewCell release];
    mSelectedTableViewCell = nil;
    
}
- (void) createSelectedTableViewCell: (NSValue*) frame
{
    [picker saveGesture];
    [picker removeAllGestures];
    CGRect sFrame = [frame CGRectValue];
    sFrame = CGRectMake(sFrame.origin.x, sFrame.origin.y, sFrame.size
                        .width, 120);
    UIView* parent = self;
    SEUITableViewCell* cell = [[[SEUITableViewCell alloc] initWithStyle:UITableViewCellStyleDefault reuseIdentifier:@"dd"] autorelease];
    cell.textLabel.text = @"lkkasdfkkkdfsdf";
    cell.textLabel.frame = CGRectMake(80, 0, 200, 120);
    cell.textLabel.backgroundColor = [UIColor redColor];
    cell.frame = sFrame;
    mSelectedTableViewCell = cell;
    [parent addSubview:cell];
}

@end
///////////////////
@interface SEUIFloatView : UIImageView
{
    CGPoint p;
    CGPoint origC;
    NSTimeInterval time;
    NSMutableDictionary* d;
}
@property (nonatomic, assign) CGPoint p;
@property (nonatomic, assign) CGPoint origC;
/*
- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event;
- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event;
- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event;
- (void)touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event;
 */
@end
@implementation SEUIFloatView
@synthesize p;
@synthesize origC;
- (id) init
{
    self = [super init];
    if(self)
    {
        self.multipleTouchEnabled = YES;
    }
    return self;
}
/*
- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"touches began count = %u", [touches count]);
    p = [[touches anyObject] locationInView:self.superview];
    origC = self.center;
    time = event.timestamp;
    if(!self->d)
    {
        self->d = [[NSMutableDictionary alloc] init];
    }
    for(UITouch* t in touches)
    {
        CGPoint initTouch = [t locationInView: self.superview];
        CGPoint delta = CGPointMake(initTouch.x - self.center.x , initTouch.y - self.center.y);
        [d setObject:[NSValue valueWithCGPoint:delta] forKey:[t uid]];
    }
    
}
- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"touches move count = %u", [touches count]);
    CGPoint loc = [[touches anyObject] locationInView:self.superview];
    CGFloat deltax = loc.x - p.x;
    CGFloat deltay = loc.y - p.y;
    CGPoint c = self.center;
    c.x = origC.x + deltax;
    c.y = origC.y + deltay;
    self.center = c;
    self->p = [[touches anyObject] locationInView:self.superview];
    self->origC = self.center;
    CGFloat elapsed = event.timestamp - self->time;
    BOOL move = YES;
    NSLog(@"## touch in event is: %d", [[event touchesForView:self] count]);
    for(UITouch*t in [event touchesForView:self])
    {
        if(t.phase == UITouchPhaseStationary)
            move = NO;
    }
    
}
- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"touches end count = %u", [touches count]);
}
- (void)touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"touch cancel");
}
 */
@end


//////////////////////////
@interface SEViewNavigator (Private)
- (UIView*) loadViewFromNib: (NSString*)name;
- (void) addViewToRoot;
- (void) setupRootView;
- (void) longPressHandler:(UILongPressGestureRecognizer*) longpress;

- (UIView<SEAdjustContentView>*) createImagePickerView: (CGRect)rect;
- (UIView<SEAdjustContentView>*) createImageSelectedView : (CGRect) rect;
- (UIView<SEAdjustContentView>*) createMusicPickerView: (CGRect) rect;
- (UIView<SEAdjustContentView>*) create: (VIEW_TYPE) type withFrame: (CGRect) rect;
- (UIImage*) getDefaultImage;


- (BOOL) isFloatViewShow;
- (void) moveFloatViewToPoint : (CGPoint)p;
- (void) intersectFloatView: (UIView*)floatView withContentContainer: (SEContentViewContainer*)contentContainter;
@end
@implementation SEViewNavigator (Private)
- (UIView<SEAdjustContentView>*) create: (VIEW_TYPE) viewType withFrame:(CGRect)rect
{
    switch (viewType) 
    {
        case IMAGE_PICKER:
            return [self createImagePickerView: rect];
        case SELECTED_IMAGE_VIEW:
            return [self createImageSelectedView: rect];
        case MUSIC_PICKER:
            return [self createMusicPickerView:rect];
        default:
            return nil;
    }
}
- (UIImage*) getDefaultImage
{
    return [UIImage imageNamed:@"Koala.jpg"];
}
- (UIView<SEAdjustContentView>*) createMusicPickerView: (CGRect) rect
{
    SEFixedView* parentView = [[SEFixedView alloc] initWithFrame:rect];
    parentView.backgroundColor = [UIColor greenColor];
    CGRect leftRect = CGRectMake(0, 0, rect.size.width / 2 - 5, rect.size.height);
    CGRect rightRect = CGRectMake(rect.size.width / 2 + 10, 0 , rect.size.width / 2 - 5, rect.size.height);
    SEUITableView* tableViewLeft = [[SEUITableView alloc] initWithFrame:leftRect];
    tableViewLeft.dataSource = mMusicPickerDelegate;
    tableViewLeft.delegate = mMusicPickerDelegate;
    SEUITableView* tableViewRight = [[SEUITableView alloc] initWithFrame:rightRect];
    tableViewRight.dataSource = mMusicSelectedViewDelegate;
    tableViewRight.delegate = mMusicSelectedViewDelegate;
    [parentView addSubview:tableViewLeft];
    [parentView addSubview:tableViewRight];
    [tableViewLeft release];
    [tableViewRight release];
    return parentView;
}
- (UIView<SEAdjustContentView>*) createImagePickerView : (CGRect) rect
{
    SEResLoader* resLoader = mResLoader;
    SEUIScrollView* scrollView = [[SEUIScrollView alloc] init];
    scrollView.mName = @"image picker";
    //scrollView.mNotStartThread = YES;
    UILongPressGestureRecognizer* lpges = [[UILongPressGestureRecognizer alloc] initWithTarget:self action:@selector(longPressHandler:)];
    lpges.cancelsTouchesInView = NO;
    [scrollView addGestureRecognizer:lpges];
    [lpges release];
    scrollView.backgroundColor = [UIColor redColor];
    scrollView.frame = rect;
    scrollView.mViewWidth = rect.size.width;
    scrollView.mViewHeight = rect.size.height;
    scrollView.mPhotoWidth = [resLoader getInt: @"PhotoWidth"];
    scrollView.mPhotoHeight = [resLoader getInt:@"PhotoHeight"];
    scrollView.mVMargin = [resLoader getInt:@"VMargin"];
    scrollView.mHMargin = [resLoader getInt:@"HMargin"];
    scrollView.mViewNavigator = self;
    scrollView.mCanTouchResponse = YES;
    SEUIPhotoLibLoaderDelegete* photoLibDelegate = [[SEUIPhotoLibLoaderDelegete alloc] init];
    scrollView.mPhotoLoaderDelegate = photoLibDelegate;
    photoLibDelegate.mScrollView = scrollView;
    [scrollView initState];
    [scrollView initPhotoLibUrl];
    return scrollView;
}
- (UIView<SEAdjustContentView>*) createImageSelectedView : (CGRect) rect
{
    SEResLoader* resLoader = mResLoader;
    SEUIScrollView* scrollView= [[SEUIScrollView alloc] init];
    scrollView.mName = @"image selected view";
    scrollView.frame = rect;
    scrollView.backgroundColor = [UIColor redColor];
    scrollView.mViewWidth = rect.size.width;
    scrollView.mViewHeight = rect.size.height;
    scrollView.mPhotoWidth = [resLoader getInt:@"PhotoWidth"];
    scrollView.mPhotoHeight = [resLoader getInt:@"PhotoHeight"];
    scrollView.mVMargin = [resLoader getInt:@"VMargin"];
    scrollView.mHMargin = [resLoader getInt:@"HMargin"];
    scrollView.mViewNavigator = self;
    scrollView.mDefaultImage = [self getDefaultImage];
    SEUIPhotoFileLoaderDelegate* photoFileDelegate = [[SEUIPhotoFileLoaderDelegate alloc] init];
    photoFileDelegate.mNavView = self;
    photoFileDelegate.mScrollView = scrollView;
    scrollView.mPhotoLoaderDelegate= photoFileDelegate;
    [scrollView initState];
    [scrollView initPhotoLibUrl];
    [scrollView createContent];
    return scrollView;
}

- (void) intersectFloatView: (UIView*)floatView withContentContainer: (SEContentViewContainer*)contentContainter
{
    if(contentContainter.contentView == nil)
        return;
    CGPoint c = floatView.center;
    c = [mRootView convertPoint:c toView:contentContainter.contentView];
    NSLog(@"c = %f, %f", c.x, c.y);
    int imageViewIndex = -1;
    SEUIImageView* imageView = [contentContainter intersectContentChild:c outIndex:&imageViewIndex];
    mPlacedViewIndex = imageViewIndex;
    NSLog(@"placeimageview = %@", imageView);
    NSLog(@"place image index = %d", mPlacedViewIndex);
    if(imageView)
    {
        if(mPlacedView != nil && mPlacedView != imageView)
        {
            mPlacedView.mImageView.opaque = YES;
            mPlacedView.mImageView.alpha = 1.0;
            [mPlacedView setNeedsDisplay];
        }
        imageView.backgroundColor = [UIColor redColor];
        imageView.mImageView.opaque = NO;
        imageView.mImageView.alpha = 0.5;
        [imageView setNeedsDisplay];
        mPlacedView = imageView;
    }
    else
    {
        if(mPlacedView)
        {
            //mPlacedView.backgroundColor = [UIColor blueColor];
            mPlacedView.mImageView.opaque = YES;
            mPlacedView.mImageView.alpha = 1.0;
            [mPlacedView setNeedsDisplay];
        }
        mPlacedView = nil;
    }
}
- (BOOL) isFloatViewShow
{
    return mFloatView != nil;
}

- (void) moveFloatViewToPoint : (CGPoint)p
{
    if(mFloatView)
    {
        CGPoint delta;
        delta.x = p.x - mFloatView.p.x;
        delta.y = p.y - mFloatView.p.y;
        CGPoint c = mFloatView.center;
        c.x += delta.x;
        c.y += delta.y;
        mFloatView.center = c;
        mFloatView.p = p;
        SEContentViewContainer* sc = mViewArray[mNextView];
        [self intersectFloatView: mFloatView withContentContainer: sc];
        //test
        //end
    }
    else
    {
        NSLog(@"error: float view is nil");
    }
}
- (UIView*) loadViewFromNib: (NSString*)name
{
    NSBundle* mainBundle = [NSBundle mainBundle];
    UIView* view = [[mainBundle loadNibNamed:name owner:self options:NULL] lastObject];
    return view;
    
}

- (void) addViewToRoot
{
    UIView* currView = mViewArray[mCurrView];
    [mRootView addSubview:currView];
    [mRootView addSubview:mCurrentLeftBarView];
    [mRootView addSubview:mCurrentRightBarView];
}
- (void) setupRootView
{
    if(mRootView == NULL)
    {
        mRootView = [[SEUIRootView alloc] initWithFrame:CGRectMake(0, 0, mViewPortWidth, mViewPortHeight)];
        mRootView.backgroundColor = [UIColor greenColor];
    }
    [self removeAllViewFromRoot];
    self.view = mRootView;
}
- (void) longPressHandler:(UILongPressGestureRecognizer*) longpress
{
    SEContentViewContainer* imagePicker = mViewArray[mCurrView];
    if(longpress.state == UIGestureRecognizerStateBegan)
    {
        NSLog(@"long press");
        UIView* view = longpress.view;
        NSLog(@"long pressed view = %@", view);
        NSLog(@"long pressed view super view = %@", view.superview);
        //Class uiscrollView = [SEUIScrollView class];
        CGPoint p = [longpress locationInView:view];
        NSLog(@"p = %f, %f", p.x, p.y);
        if(view == imagePicker.contentView)
        {    
            SEUIScrollView* currView = (SEUIScrollView*)view;
            HitProperty hp = [currView hitRect:p];
            CGRect r = hp.rect;
            SEUIImageView* imageView = hp.imageView;
            mFloatView = [[SEUIFloatView alloc] initWithFrame:r];
            mFloatView.backgroundColor = [UIColor greenColor];
            mFloatView.contentMode = UIViewContentModeCenter;
            mFloatView.clipsToBounds = YES;
            mFloatView.image = imageView.image;
            mFloatView.p = p;
            mFloatView.origC = mFloatView.center;
            [mRootView addSubview:mFloatView];
            [mFloatView release];
            mSelectedPhotoURL = [currView getImageURL:hp.index];
            NSLog(@"selected url = %@", mSelectedPhotoURL);
        }
    }
    else if(longpress.state == UIGestureRecognizerStateEnded)
    {
        NSLog(@"long press up");
        
        if(mPlacedView)
        {
            SEContentViewContainer* selectedContainer = mViewArray[SELECTED_IMAGE_VIEW];
            SEUIScrollView* selectedScrollView = (SEUIScrollView*)selectedContainer.contentView;
            SEImageURL* imageURL = [selectedScrollView getImageURL:mPlacedViewIndex];
            if(imageURL.filepath == nil)
            {
                imageURL.url = mSelectedPhotoURL.url;
                mPlacedView.image = mFloatView.image;
                [selectedScrollView removeFromPhotoDict:mPlacedViewIndex];
            }
            mPlacedView.mImageView.alpha = 1.0;
            mPlacedView.mImageView.opaque = YES;
            [mPlacedView.mImageView setNeedsDisplay];
        }
        [mFloatView removeFromSuperview];
        mFloatView = nil;
    }
    
}
@end
//////////////////////////////////
@implementation SEViewNavigator
@synthesize mResLoader;
@synthesize managedObjectContext;
@synthesize mUserInfoProperty;
@synthesize mMusicPickerDelegate;
@synthesize mMusicSelectedViewDelegate;
@synthesize mViewPortWidth;
@synthesize mViewPortHeight;
@synthesize mPrevView;
@synthesize mNextView;
@synthesize mCurrView;
@synthesize mBarWidth;
@synthesize mRootView;
@synthesize mContentView;
- (void) initContentContainer
{
    for(int i = 0 ; i < VIEW_NUM ; i++)
    {
        if(i != INVALID_VIEW)
        {
            VIEW_TYPE vp = (VIEW_TYPE)i;
            VIEW_TYPE prev = getPrevView(vp);
            VIEW_TYPE next = getNextView(vp);
            float contentWidth = mViewPortWidth;
            if(prev != INVALID_VIEW)
                contentWidth -= mBarWidth;
            if(next != INVALID_VIEW)
                contentWidth -= mBarWidth;
            CGRect frame = CGRectMake(0, 0, contentWidth, mViewPortHeight);
            SEContentViewContainer* viewContainer = [[SEContentViewContainer alloc] initWithFrame:frame];
            viewContainer.backgroundColor = [UIColor redColor];
            viewContainer.mViewNav = self;
            viewContainer.mType = vp;
            mViewArray[i] = viewContainer;
        }
    }
}
- (void) initBarView
{
    mBarViewArray = [NSMutableArray array];
    [mBarViewArray retain];
    int count = getBarViewTypeCount();
    for(int i = 0 ; i < count ; i++)
    {
        SEBarView* barView = [[SEBarView alloc] initWithFrame:CGRectMake(0, 0, mBarWidth, mViewPortHeight)];
        barView.backgroundColor = [UIColor grayColor];
        BarViewType bvt = gBarViewType[i];
        barView.mBarViewType = bvt;
        if(bvt.leftView == IMAGE_PICKER && bvt.rightView == SELECTED_IMAGE_VIEW)
            barView.mCanStopInMid = YES;
        barView.mLeftContentContainer = mViewArray[bvt.leftView];
        barView.mRightContentContainer = mViewArray[bvt.rightView];
        barView.mViewNav = self;
        [mBarViewArray addObject:barView];
    }
}
- (void) initContentView
{
    mContentView = [[UIView alloc] init];
    mContentView.backgroundColor = [UIColor redColor];
    [mRootView addSubview:mContentView];
    int count = sizeof(gViewSequence) / sizeof(VIEW_TYPE);
    float firstWidth = mViewPortWidth - mBarWidth;
    float lastWidth = mViewPortWidth - mBarWidth;
    float midWidth = mViewPortWidth - 2 * mBarWidth;
    float totalWidth = firstWidth + lastWidth + midWidth * (count - 2) + mBarWidth * (count - 1);
    mContentView.frame = CGRectMake(0, 0, totalWidth, mViewPortHeight);
    float startx = 0;
    for(int i = 0 ; i < count - 1 ; i++)
    {
        VIEW_TYPE vp = gViewSequence[i];
        VIEW_TYPE vpNext = gViewSequence[i + 1];
        SEContentViewContainer* viewContainer = mViewArray[vp];
        SEBarView* barView = [self getBarView:vp : vpNext];
        viewContainer.frame = CGRectMake(startx, 0, viewContainer.frame.size.width, viewContainer.frame.size.height);
        startx += viewContainer.frame.size.width;
        barView.frame = CGRectMake(startx, 0, barView.frame.size.width, barView.frame.size.height);
        startx += barView.frame.size.width;
        [mContentView addSubview:viewContainer];
        [mContentView addSubview:barView];
    }
    SEContentViewContainer* viewContainer = mViewArray[count - 1];
    viewContainer.frame = CGRectMake(startx, 0, viewContainer.frame.size.width, viewContainer.frame.size.height);
    [mContentView addSubview:viewContainer];
}
- (float) getBarWidth
{
    UIImage* leftImage = [mResLoader getImage:@"LeftBarBg"];
    return leftImage.size.width;
}
- (id) initWithResLoader: (SEResLoader*) resLoader
{
    self = [super init];
    if(self)
    {
        mCurrView = mPrevView = mNextView = INVALID_VIEW;
        mResLoader = resLoader;
        mBarWidth = [self getBarWidth];
        mMusicPickerDelegate = [[SEMusicPickerDelegate alloc] init];
        mMusicSelectedViewDelegate = [[SEMusicSelectedViewDelegate alloc] init];
    }
    return self;
}
- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)didReceiveMemoryWarning
{
    // Releases the view if it doesn't have a superview.
    [super didReceiveMemoryWarning];
    
    // Release any cached data, images, etc that aren't in use.
}

#pragma mark - View lifecycle


// Implement loadView to create a view hierarchy programmatically, without using a nib.
- (void)loadView
{
    [self setupRootView];
    [self initContentContainer];
    [self initBarView];
    [self initContentView];
    [self setCurrentView:IMAGE_PICKER];
}
- (void)viewWillAppear:(BOOL)animated
{
    [super viewWillAppear:animated];
}

/*
// Implement viewDidLoad to do additional setup after loading the view, typically from a nib.
- (void)viewDidLoad
{
    [super viewDidLoad];
}
*/

- (void)viewDidUnload
{
    [super viewDidUnload];
    // Release any retained subviews of the main view.
    // e.g. self.myOutlet = nil;
}

- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    // Return YES for supported orientations
    //return (interfaceOrientation == UIInterfaceOrientationPortrait);
    return (interfaceOrientation == UIInterfaceOrientationLandscapeLeft) || (interfaceOrientation == UIInterfaceOrientationLandscapeRight);
}
- (void) dealloc
{
    for(int i = 0 ; i < VIEW_NUM ; i++)
    {
        [mViewArray[i] release];
    }
    for(int i = 0 ; i < [mBarViewArray count]; i++)
    {
        [[mBarViewArray objectAtIndex:i] release];
    }
    [mRootView release];
    [managedObjectContext release];
    [mUserInfoProperty release];
    [mMusicPickerDelegate release];
    [mMusicSelectedViewDelegate release];
    [super dealloc];
}
- (SEBarView*) getBarView: (VIEW_TYPE)leftView :(VIEW_TYPE)rightView
{
    BarViewType bvt = getBarViewType(leftView, rightView);
    if(isBarViewTypeValid(bvt))
    {
        for(SEBarView* v in mBarViewArray)
        {
            if(isBarViewTypeEqual(bvt, v.mBarViewType))
                return v;
        }
        return nil;
    }
    else
    {
        return nil;
    }
}
- (SEContentViewContainer*) getContainer : (VIEW_TYPE) vp;
{
    if(vp == INVALID_VIEW)
        return nil;
    return mViewArray[vp];
}
- (void) addContentToContentContainer: (VIEW_TYPE) vp
{
    if(vp == INVALID_VIEW)
        return;
    SEContentViewContainer* viewContainer = [self getContainer:vp];
    if([viewContainer hasContent])
        return;
    CGRect frame = viewContainer.bounds;
    UIView* view = [self createSettingView:vp withFrame:frame];
    [viewContainer addSubview:view];
    [viewContainer initContainer];
}
/*
 when set current view, it will display current view and left bar or right bar. it will not create prev view
     or next view.
 pre view and next view will be created when user touch the left bar or right bar
 */
- (void)setCurrentView: (enum VIEW_TYPE) view_type
{
    if(mCurrView == view_type)
        return;
    mCurrentLeftBarView = nil;
    mCurrentRightBarView = nil;
    mCurrView = view_type;
    mPrevView = getPrevView(mCurrView);
    mNextView = getNextView(mCurrView);
    mCurrentLeftBarView = [self getBarView:mPrevView :mCurrView];
    mCurrentRightBarView = [self getBarView:mCurrView :mNextView];
    SEContentViewContainer* currentContainer = [self getContainer:mCurrView];
    currentContainer.hidden = NO;
    int viewIndex = getViewIndexInSequence(mCurrView);
    if(viewIndex != -1)
    {
        float deltax = 0;
        for(int i = 0 ; i < viewIndex ; i++)
        {
            VIEW_TYPE vp = gViewSequence[i];
            SEContentViewContainer* view = mViewArray[vp];
            deltax += view.frame.size.width + mBarWidth;
        }
        if(deltax > 0)
            deltax -= mBarWidth;
        CGRect frame = mContentView.frame;
        mContentView.frame = CGRectMake(-deltax, 0, frame.size.width, frame.size.height);
        [self addContentToContentContainer: mCurrView];
    }
    SEContentViewContainer* prevContainer = [self getContainer:mPrevView];
    prevContainer.hidden = YES;
    SEContentViewContainer* nextContainer = [self getContainer:mNextView];
    nextContainer.hidden = YES;
    
}
- (void) removeAllViewFromRoot
{
    NSArray* subviews = mRootView.subviews;
    for(UIView* v in subviews)
    {
        [v removeFromSuperview];
    }
}
- (NSArray*) fetchUserInfo
{
    NSFetchRequest* fetchRequest = [[NSFetchRequest alloc] init];
    NSEntityDescription* entity = [NSEntityDescription entityForName:@"UserInfo" inManagedObjectContext:self.managedObjectContext];
    [fetchRequest setEntity:entity];
    NSError* error = nil;
    NSArray* userInfoObjects = [self.managedObjectContext executeFetchRequest:fetchRequest error:&error];
    if(error)
    {
        NSLog(@"Unresolved error when init data : %@", [error userInfo]);
        abort();
    }
    [fetchRequest release];
    return userInfoObjects;
}
- (void) initData
{
    NSArray* userInfoObjects = [self fetchUserInfo];
    NSUInteger count = [userInfoObjects count];
    if(count == 0)
    {
        NSEntityDescription* selectedImageEntity = [NSEntityDescription entityForName:@"SelectedImage" inManagedObjectContext:self.managedObjectContext];
        NSEntityDescription* userInfoEntity = [NSEntityDescription entityForName:@"UserInfo" inManagedObjectContext:self.managedObjectContext];
        NSManagedObject* newUserInfo = [NSEntityDescription insertNewObjectForEntityForName:[userInfoEntity name] inManagedObjectContext:self.managedObjectContext];
        int levelOneImageNum = [mResLoader getInt:@"LevelOneImageNum"];
        NSNumber* num = [NSNumber numberWithInt:levelOneImageNum];
        [newUserInfo setValue:num forKey:@"level"];
        NSMutableSet* selectedImageSet = [newUserInfo mutableSetValueForKey:@"imageinfo"];
        for(int i = 0 ; i < levelOneImageNum ; i++)
        {
            SelectedImage* si = [NSEntityDescription insertNewObjectForEntityForName:[selectedImageEntity name] inManagedObjectContext:self.managedObjectContext];
            NSNumber* seq = [NSNumber numberWithInt:i];
            si.seq = seq;
            [selectedImageSet addObject:si];
        }
        NSError* error = nil;
        if(![self.managedObjectContext save:&error])
        {
            NSLog(@"intialize user info error: %@", [error userInfo]);
            abort();
        }
    }
    else
    {
        mUserInfoProperty = userInfoObjects;
    }
    if(count == 0)
    {
        mUserInfoProperty = [self fetchUserInfo];
        
    }
    [mUserInfoProperty retain];
    //debug
    for(NSUInteger i = 0 ; i < [mUserInfoProperty count] ; i++)
    {
        NSManagedObject* ui = [mUserInfoProperty objectAtIndex:i];
        NSSet* siSet = [ui valueForKey:@"imageinfo"];
        NSEnumerator* it = [siSet objectEnumerator];
        NSManagedObject* si = nil;
        while((si = [it nextObject]) != nil)
        {
            NSLog(@"seq = %@", [si valueForKey:@"seq"]);
            NSLog(@"url = %@", [si valueForKey:@"url"]);
            NSLog(@"filepath = %@", [si valueForKey:@"filepath"]);
        }
        
    }
    //end
}
- (SelectedImage*) getSelectedImageProperty: (int)index
{
    NSUInteger count = [mUserInfoProperty count];
    if(count != 1)
        return nil;
    UserInfo* ui = [mUserInfoProperty objectAtIndex:0];
    NSSet* siSet = [ui valueForKey:@"imageinfo"];
    NSEnumerator* it = [siSet objectEnumerator];
    SelectedImage* si = nil;
    while((si = [it nextObject]) != nil)
    {
        NSNumber* num = [si valueForKey:@"seq"];
        int n = [num intValue];
        if(n == index)
            return si;
    }
    return nil;
}
- (NSMutableArray*) getUserImageProperty
{
    NSMutableArray* newArray = [NSMutableArray array];
    for(NSUInteger i = 0 ; i < [mUserInfoProperty count] ; i++)
    {
        NSManagedObject* ui = [mUserInfoProperty objectAtIndex:i];
        NSSet* siSet = [ui valueForKey:@"imageinfo"];
        NSEnumerator* it = [siSet objectEnumerator];
        NSManagedObject* si = nil;
        while((si = [it nextObject]) != nil)
        {
            NSLog(@"seq = %@", [si valueForKey:@"seq"]);
            NSLog(@"url = %@", [si valueForKey:@"url"]);
            NSLog(@"filepath = %@", [si valueForKey:@"filepath"]);
            [newArray addObject:si];
        }
    }
    
    [newArray sortUsingComparator:^NSComparisonResult(id obj1, id obj2) {
        NSManagedObject* mo1 = (NSManagedObject*)obj1;
        NSManagedObject* mo2 = (NSManagedObject*)obj2;
        NSNumber* left = [mo1 valueForKey:@"seq"];
        NSNumber* right = [mo2 valueForKey:@"seq"];
        return [left compare:right];
    }]; 
    return [newArray retain];
}
- (void) saveContext
{
    for(int i = 0 ;i < VIEW_NUM ; i++)
    {
        SEContentViewContainer* c = mViewArray[i];
        [c saveContext: mUserInfoProperty];
    }
}
- (UIView<SEAdjustContentView>*) createSettingView: (VIEW_TYPE) viewType withFrame: (CGRect)r
{
    if(viewType != INVALID_VIEW)
    {
        UIView<SEAdjustContentView>* view = nil;
        switch (viewType) {
            case IMAGE_PICKER:
                view = [self createImagePickerView:r];
                break;
            case SELECTED_IMAGE_VIEW:
                view = [self createImageSelectedView:r];
                break;
            case MUSIC_PICKER:
                view = [self createMusicPickerView:r];
                break;
            default:
                break;
        }
        return view;
    }
    else
        return nil;
}
- (void) addSettingView: (UIView*)view
{
    [mRootView addSubview:view];
}
@end
