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
#import "SEMainDisplay.h"
#import "PainterManager.h"
#import "SEImageMusicListView.h"
#import "SEOptionsView.h"
#import "SEMusicPickerView.h"
#import "SESignatureView.h"
#import "Signature.h"
#import "MusicList.h"
#import "ImageList.h"
#import "SESignaturePreview.h"
#import <QuartzCore/CALayer.h>

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
static BOOL isHintFrame(CGRect hintFrame, CGRect frame)
{
    if(hintFrame.size.width != frame.size.width || hintFrame.size.height != frame.size.height)
        return NO;
    else
        return YES;
}
////////////////////
struct ViewRelation
{
    VIEW_TYPE curr;
    VIEW_TYPE prev;
    VIEW_TYPE next;
};
struct ViewRelation gViewRelation[] = {
    {IMAGE_PICKER, MAIN_DISPLAY, SELECTED_IMAGE_VIEW},
    {SELECTED_IMAGE_VIEW, IMAGE_PICKER, INVALID_VIEW},
    {MUSIC_PICKER, MAIN_DISPLAY, INVALID_VIEW},
    {OPTIONS, MAIN_DISPLAY, INVALID_VIEW},
    {MUSIC_IMAGE_LIST_ATTACH, MAIN_DISPLAY, INVALID_VIEW},
    {SIGNATURE_PREVIEW, OPTIONS, INVALID_VIEW},
    {SIGNATURE_VIEW, SIGNATURE_PREVIEW, INVALID_VIEW}
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
    {IMAGE_PICKER, SELECTED_IMAGE_VIEW, YES, YES},
    {MAIN_DISPLAY, IMAGE_PICKER, YES, NO},
    {MAIN_DISPLAY, OPTIONS, YES, NO},
    {MAIN_DISPLAY, MUSIC_PICKER, YES, NO},
    {MAIN_DISPLAY, MUSIC_IMAGE_LIST_ATTACH, YES, NO},
    {OPTIONS, SIGNATURE_PREVIEW, YES, NO},
    {SIGNATURE_PREVIEW, SIGNATURE_VIEW, YES, NO}
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
VIEW_TYPE gViewSequenceMainDisplayOnly[] = {MAIN_DISPLAY};
VIEW_TYPE gViewSequenceImagePicker[] = {MAIN_DISPLAY, IMAGE_PICKER, SELECTED_IMAGE_VIEW};
VIEW_TYPE gViewSequenceMusicPicker[] = {MAIN_DISPLAY, MUSIC_PICKER};
VIEW_TYPE gViewSequenceOption[] = {MAIN_DISPLAY, OPTIONS};
VIEW_TYPE gViewSequenceMusicImageListAttach[] = {MAIN_DISPLAY, MUSIC_IMAGE_LIST_ATTACH};
VIEW_TYPE gViewSequenceOptionImagePicker[] = {OPTIONS, IMAGE_PICKER, SELECTED_IMAGE_VIEW};
VIEW_TYPE gViewSequenceOptionMusicPicker[] = {OPTIONS, MUSIC_PICKER};
VIEW_TYPE gViewSequenceOptionsSignaturePreview[] = {OPTIONS, SIGNATURE_PREVIEW};
VIEW_TYPE gViewSequenceSignatureViewPreview[] = {SIGNATURE_PREVIEW, SIGNATURE_VIEW};
struct ViewSeqProperty
{
    VIEW_TYPE* viewseq;
    VIEW_SEQ_TYPE type;
    int count;
};
ViewSeqProperty gViewSeqProps[] = {
    {gViewSequenceMainDisplayOnly, MAIN_DISPLAY_ONLY, sizeof(gViewSequenceMainDisplayOnly) / sizeof(VIEW_TYPE)},
    {gViewSequenceImagePicker, MAIN_DISPLAY_IMAGE_PICKER, sizeof(gViewSequenceImagePicker) / sizeof(VIEW_TYPE)},
    {gViewSequenceMusicPicker, MAIN_DISPLAY_MUSIC_PICKER, sizeof(gViewSequenceMusicPicker) / sizeof(VIEW_TYPE)},
    {gViewSequenceMusicImageListAttach, MAIN_DISPLAY_MUSIC_IMAGE_LIST_ATTACH, sizeof(gViewSequenceMusicImageListAttach) / sizeof(VIEW_TYPE)},
    {gViewSequenceOption, MAIN_DISPLAY_OPTIONS, sizeof(gViewSequenceOption) / sizeof(VIEW_TYPE)},
    {gViewSequenceOptionImagePicker, OPTIONS_IMAGE_PICKER, sizeof(gViewSequenceOptionImagePicker)/ sizeof(VIEW_TYPE)},
    {gViewSequenceOptionMusicPicker, OPTIONS_MUSIC_PICKER, sizeof(gViewSequenceOptionMusicPicker)/sizeof(VIEW_TYPE)},
    {gViewSequenceOptionsSignaturePreview, OPTIONS_SIGNATURE, sizeof(gViewSequenceOptionsSignaturePreview) / sizeof(VIEW_TYPE)},
    {gViewSequenceSignatureViewPreview, SIGNATURE_SIGNATURE_PREVIEW, sizeof(gViewSequenceSignatureViewPreview) / sizeof(VIEW_TYPE)}
};
static int getViewIndexInSequence(ViewSeqProperty vsp, VIEW_TYPE vp)
{
    for(int i = 0 ; i < vsp.count ; i++)
    {
        if(vp == vsp.viewseq[i])
            return i;
    }
    return -1;
}
static ViewSeqProperty getViewSeqProperty(VIEW_SEQ_TYPE type)
{
    int count = sizeof(gViewSeqProps) / sizeof(ViewSeqProperty);
    for(int i = 0 ; i < count ; i++)
    {
        if(gViewSeqProps[i].type == type)
            return gViewSeqProps[i];
    }
    ViewSeqProperty vsp;
    vsp.type = INVALID_SEQ_TYPE;
    vsp.viewseq = NULL;
    vsp.count = 0;
    return vsp;
}
static int getViewIndexInSequence(VIEW_SEQ_TYPE seqType, VIEW_TYPE vp)
{
    ViewSeqProperty vsp = getViewSeqProperty(seqType);
    if(vsp.type != INVALID_SEQ_TYPE)
    {
        return getViewIndexInSequence(vsp, vp);
    }
    else
        return -1;
}
static BOOL isValidViewSeqProp(ViewSeqProperty vsp)
{
    return  vsp.type != INVALID_SEQ_TYPE;
}
////////
struct ViewBarBackground
{
    VIEW_TYPE vp;
    NSString* key;
};
ViewBarBackground gViewBarBackground[] = {
    {MAIN_DISPLAY, @"MainDisplayBarBackground"},
    {IMAGE_PICKER, @"ImagePickerBarBackground"},
    {SELECTED_IMAGE_VIEW, @"ImageSelectedViewBarBackground"}, 
    {MUSIC_PICKER, @"MusicPickerBarBackground"},
    {OPTIONS, @"OptionBarBackground"},
    {MUSIC_IMAGE_LIST_ATTACH, @"MusicImageAttachBarBackground"},
    {SIGNATURE_VIEW, @"MainDisplayBarBackground"},
    {SIGNATURE_PREVIEW, @"MainDisplayBarBackground"}
    
};
static NSString* getViewBarBackgroundKey(VIEW_TYPE vp)
{
    int count = sizeof(gViewBarBackground) / sizeof(ViewBarBackground);
    for(int i = 0 ; i < count ; i++)
    {
        if(vp == gViewBarBackground[i].vp)
            return gViewBarBackground[i].key;
    }
    return nil;
}
////////
@implementation SEImageListProperty
@synthesize imageCount;
@synthesize name;
@synthesize firstURLString;
- (void)dealloc
{
    [firstURLString release];
    [name release];
    [super dealloc];
}
@end
@implementation SEMusicListProperty

@synthesize musicCount;
@synthesize name;
@synthesize firstURLString;
- (void) dealloc
{
    [name release];
    [firstURLString release];
    [super dealloc];
}
@end

@implementation SEContentViewContainer
@synthesize mHintRect;
@synthesize mType;
@synthesize mViewNav;
@synthesize mBarBackgroundKey;
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
        
        case MUSIC_PICKER:
        {
        }
            break;
        case SIGNATURE_PREVIEW:
        {
            SESignaturePreview* sigPreView = (SESignaturePreview*)[self contentView];
            int site = sigPreView.mCurrentSite;
            int size = sigPreView.mCurrentSize;
            UserInfo* userInfo = [userInfoArray objectAtIndex:0];
            userInfo.currentsignaturesite = [NSNumber numberWithInt: site];
            userInfo.currentsignaturesize = [NSNumber numberWithInt: size];
        }
            break;
        case SIGNATURE_VIEW:
        {
            SESignatureView* sigView = (SESignatureView*)[self contentView];
            UserInfo* userInfo = [userInfoArray objectAtIndex:0];
            [sigView saveContext:userInfo];
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
- (void)dealloc
{
    [mBarBackgroundKey release];
    [super dealloc];
}
@end

//////////////
#define FIRST_CONTENTVIEW_WIDTH 0
#define MID_CONTENTVIEW_WIDTH 1024 / 2

@implementation SEBarView
@synthesize mHintRect;
@synthesize mLeftContentContainer;
@synthesize mRightContentContainer;
@synthesize mViewNav;
@synthesize mBarViewType;
@synthesize mCanStopInMid;
@synthesize mResLoader;
@synthesize mContainerAnimHandler;
- (void) initBackground
{
    CGRect bounds = self.bounds;
    NSString* leftBgKey = mLeftContentContainer.mBarBackgroundKey;
    NSString* rightBgKey = mRightContentContainer.mBarBackgroundKey;
    UIImage* leftImage = [mResLoader getImage:leftBgKey];
    UIImage* rightImage = [mResLoader getImage:rightBgKey];
    mLeftImageView = [[UIImageView alloc] initWithFrame:bounds];
    mRightImageView = [[UIImageView alloc] initWithFrame:bounds];
    [self addSubview:mLeftImageView];
    [self addSubview:mRightImageView];
    [mLeftImageView release];
    [mRightImageView release];
    mLeftImageView.image = leftImage;
    mRightImageView.image = rightImage;
    //CGAffineTransform reverse = CGAffineTransformMakeScale(-1, -1);
    //mLeftImageView.transform = reverse;
}
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
        mLeftContentContainer.hidden = NO;
        [mViewNav addContentToContentContainer:mLeftContentContainer.mType];
    }
    else if(barType == RIGHT_BAR)
    {
        mRightContentContainer.hidden = NO;
        [mViewNav addContentToContentContainer:mRightContentContainer.mType];
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

    CGPoint c = mViewNav.mContentContainerParent.center;
    c.x += deltax;
    mViewNav.mContentContainerParent.center = c;
}
- (CGPoint) pointInScreen: (CGPoint) point
{
    return [self convertPoint:point toView:mViewNav.mRootView];
}
- (void) barUpHandler
{
    CGPoint currBarStartPoint = [self pointInScreen:CGPointMake(0, 0)];
    BOOL stopInMid = NO;
    [mViewNav clearPlacedView];
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
                if(mBarViewType.canSeenAsLeftBar)
                    dist = currBarStartPoint.x;
                else
                    dist = currBarStartPoint.x + mViewNav.mBarWidth;
            }
            if(dist != 0)
            {
                CGPoint p = mViewNav.mContentContainerParent.center;
                p.x -= dist;
                NSLog(@"move dist = %f", dist);
                void (^animBlock) (void) = ^{
                    mViewNav.mContentContainerParent.center = p;
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
                        [mViewNav setCurrentView:mRightContentContainer.mType isAnimation:NO];
                        mLeftContentContainer.hidden = YES;
                    }
                    [mContainerAnimHandler handleAnimEnd:stopInMid withDirection:mDirect leftContainer:mLeftContentContainer rightContainer:mRightContentContainer];
                };
                [UIView animateWithDuration:0.5 delay: 0 options: UIViewAnimationOptionCurveLinear animations:animBlock completion:animEnd];
            }
            else
            {
                NSLog(@"dist = 0");
                [mViewNav setCurrentView:mRightContentContainer.mType isAnimation:NO];
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
                if(mBarViewType.canSeenAsRightBar)
                    dist = mViewNav.mViewPortWidth - (currBarStartPoint.x + mViewNav.mBarWidth);
                else
                    dist = mViewNav.mViewPortWidth - currBarStartPoint.x;
            }
            if(dist != 0)
            {
                
                CGPoint p = mViewNav.mContentContainerParent.center;
                p.x += dist;
                void (^animBlock) (void) = ^{
                    mViewNav.mContentContainerParent.center = p;
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
                        if(mBarViewType.canSeenAsRightBar == NO)
                        {
                            VIEW_SEQ_TYPE seqType = [mViewNav popViewSeqType];
                            [mViewNav moveToView:seqType :mLeftContentContainer.mType hasAnimation:NO isPush:NO];
                        }
                        else
                        {
                            [mViewNav setCurrentView:mLeftContentContainer.mType isAnimation:NO];
                        }
                        [mContainerAnimHandler handleAnimEnd:stopInMid withDirection:mDirect leftContainer:mLeftContentContainer rightContainer:mRightContentContainer];
                    }
                    
                };
                [UIView animateWithDuration:0.5 delay: 0
                                    options: UIViewAnimationOptionCurveLinear animations:animBlock
                                 completion:animEnd];
            }
            else
            {
                NSLog(@"dist = 0");
                [mViewNav setCurrentView:mLeftContentContainer.mType isAnimation:NO];
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
- (UIView<SEAdjustContentView>*) createOptionsView: (CGRect) rect;
- (UIView<SEAdjustContentView>*) createSignatureView:(CGRect) rect;
- (UIView<SEAdjustContentView>*) createSignaturePreview: (CGRect) rect;
- (UIView<SEAdjustContentView>*) createMusicImageListAttachView: (CGRect)rect;
- (UIImage*) getDefaultImage;
- (void) intersectFloatView: (UIView*)floatView withContentContainer: (SEContentViewContainer*)contentContainter;
- (void) initContentContainer;
- (void) initBarView;
- (void) initContentContainerParent;
- (SEBarView*) getBarView: (VIEW_TYPE)leftView :(VIEW_TYPE)rightView;
- (void) setCurrentViewSequenceFrame;
- (void) handleToolBarButtonClick: (TOOLBAR_BUTTON_TYPE*) typePtr;
- (void) playImage;
@end
@implementation SEViewNavigator (Private)
- (void)playImage
{
    NSMutableArray* imagePropertyArray = [self getUserImageProperty];
    NSArray* imageURLArray = [NSArray array];
    for(SelectedImage* si in imagePropertyArray)
    {
        if(si.url != nil)
        {
            imageURLArray = [imageURLArray arrayByAddingObject:si.url];
        }
    }
    mPainterManager.imageArray = imageURLArray;
    if([imageURLArray count] > 0)
    {
        [self displayNextImage];
        //[mPainterManager paintImage];
    }    
}
- (void) handleToolBarButtonClick: (TOOLBAR_BUTTON_TYPE*) typePtr
{
    TOOLBAR_BUTTON_TYPE type = *typePtr;
    switch (type) 
    {
    case PLAY_PAUSE:
    {
        NSLog(@"play drawing\n");
        [self playImage];
    }
    break;
    case MUSIC_SELECT:
    {
        NSLog(@"music select\n");
        [self moveToView:MAIN_DISPLAY_MUSIC_PICKER :MUSIC_PICKER hasAnimation:YES isPush:YES];
        /*
        [self pushViewSeqType:mCurrentViewSeqType];
        mCurrentViewSeqType = MAIN_DISPLAY_MUSIC_PICKER;
        [self setCurrentViewSequenceFrame];
        [self initContentContainerParent];
        mCurrView = INVALID_VIEW;
        [self setCurrentView: MUSIC_PICKER isAnimation:YES];
         */
    }
    break;
    case IMAGE_SELECT:
    {
        NSLog(@"image select\n");
        [self moveToView:MAIN_DISPLAY_IMAGE_PICKER :IMAGE_PICKER hasAnimation:YES isPush:YES];
        /*
        [self pushViewSeqType:mCurrentViewSeqType];
        mCurrentViewSeqType = MAIN_DISPLAY_IMAGE_PICKER;
        [self setCurrentViewSequenceFrame];
        [self initContentContainerParent];
        mCurrView = INVALID_VIEW;
        [self setCurrentView:IMAGE_PICKER isAnimation:YES];
         */
    }
    break;
    case OPTION:
    {
        NSLog(@"option select\n");
        [self moveToView:MAIN_DISPLAY_OPTIONS :OPTIONS hasAnimation:YES isPush:YES];
        /*
        [self pushViewSeqType:mCurrentViewSeqType];
        mCurrentViewSeqType = MAIN_DISPLAY_OPTIONS;
        [self setCurrentViewSequenceFrame];
        [self initContentContainerParent];
        mCurrView = INVALID_VIEW;
        [self setCurrentView:OPTIONS isAnimation:YES];
         */
    }
        break;
    case PREVIEW:
        NSLog(@"preview select\n");
        break;
    case MUSIC_IMAGE_LIST:
        {
            NSLog(@"music image list select\n");
            [self moveToView:MAIN_DISPLAY_MUSIC_IMAGE_LIST_ATTACH :MUSIC_IMAGE_LIST_ATTACH hasAnimation:YES isPush:YES];
            /*
            [self pushViewSeqType:mCurrentViewSeqType];
            mCurrentViewSeqType = MAIN_DISPLAY_MUSIC_IMAGE_LIST_ATTACH;
            [self setCurrentViewSequenceFrame];
            [self initContentContainerParent];
            mCurrView = INVALID_VIEW;
            [self setCurrentView: MUSIC_IMAGE_LIST_ATTACH isAnimation:YES];
             */
        }     
        break;
     default:
     break;
     }
}
- (void) setCurrentViewSequenceFrame
{
    ViewSeqProperty vsp = getViewSeqProperty(mCurrentViewSeqType);
    for(int i = 0 ; i < VIEW_NUM ; i++)
    {
        SEContentViewContainer* vc = mViewArray[i];
        //vc.frame = CGRectMake(0, 0, 0, 0);
        vc.hidden = YES;
    }
    for(int i = 0 ; i < vsp.count; i++)
    {
        VIEW_TYPE vp = vsp.viewseq[i];
        SEContentViewContainer* vc = mViewArray[vp];
        vc.frame = vc.mHintRect;
        vc.hidden = NO;
    }
    for(int i = 0 ; i < vsp.count - 1 ; i++)
    {
        VIEW_TYPE vp  = vsp.viewseq[i];
        VIEW_TYPE vpNext = vsp.viewseq[i + 1];
        SEBarView* barView = [self getBarView:vp  :vpNext];
        barView.frame = barView.mHintRect;
        barView.hidden = NO;
    }
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
            viewContainer.mBarBackgroundKey = getViewBarBackgroundKey(vp);
            viewContainer.mHintRect = frame;
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
        CGRect frame = CGRectMake(0, 0, mBarWidth, mViewPortHeight);
        SEBarView* barView = [[SEBarView alloc] initWithFrame:frame];
        barView.mResLoader = mResLoader;
        barView.mHintRect = frame;
        barView.mContainerAnimHandler = self;
        barView.backgroundColor = [UIColor grayColor];
        BarViewType bvt = gBarViewType[i];
        barView.mBarViewType = bvt;
        if(bvt.leftView == IMAGE_PICKER && bvt.rightView == SELECTED_IMAGE_VIEW)
            barView.mCanStopInMid = YES;
        barView.mLeftContentContainer = mViewArray[bvt.leftView];
        barView.mRightContentContainer = mViewArray[bvt.rightView];
        barView.mViewNav = self;
        [barView initBackground];
        [mBarViewArray addObject:barView];
    }
}
- (void)removeAllViewFromContentContainer
{
    for(UIView* v in mContentContainerParent.subviews)
    {
        [v removeFromSuperview];
    }
}
/*
 *before invoke this function. we must first adjust viewContainer's frame to appropiate size
 */
- (void) initContentContainerParent
{
    [self removeAllViewFromContentContainer];
    ViewSeqProperty currViewSeqProp = getViewSeqProperty(mCurrentViewSeqType);
    if(!isValidViewSeqProp(currViewSeqProp))
        return;
    int count = currViewSeqProp.count;
    if(count >= 2)
    {
        float firstWidth = mViewPortWidth - mBarWidth;
        float lastWidth = mViewPortWidth - mBarWidth;
        float midWidth = mViewPortWidth - 2 * mBarWidth;
        float totalWidth = firstWidth + lastWidth + midWidth * (count - 2) + mBarWidth * (count - 1);
        mContentContainerParent.frame = CGRectMake(0, 0, totalWidth, mViewPortHeight);
    }
    else
    {
        mContentContainerParent.frame = CGRectMake(0, 0, mViewPortWidth, mViewPortHeight);
    }
    float startx = 0;
    for(int i = 0 ; i < count - 1 ; i++)
    {
        VIEW_TYPE vp = currViewSeqProp.viewseq[i];
        VIEW_TYPE vpNext = currViewSeqProp.viewseq[i + 1];
        SEContentViewContainer* viewContainer = mViewArray[vp];
        SEBarView* barView = [self getBarView:vp : vpNext];
        viewContainer.frame = CGRectMake(startx, 0, viewContainer.frame.size.width, viewContainer.frame.size.height);
        startx += viewContainer.frame.size.width;
        barView.frame = CGRectMake(startx, 0, barView.frame.size.width, barView.frame.size.height);
        startx += barView.frame.size.width;
        [mContentContainerParent addSubview:viewContainer];
        [mContentContainerParent addSubview:barView];
    }
    VIEW_TYPE vp = currViewSeqProp.viewseq[count - 1];
    SEContentViewContainer* viewContainer = mViewArray[vp];
    viewContainer.frame = CGRectMake(startx, 0, viewContainer.frame.size.width, viewContainer.frame.size.height);
    [mContentContainerParent addSubview:viewContainer];
}

- (UIImage*) getDefaultImage
{
    return [UIImage imageNamed:@"Koala.jpg"];
}
- (UIView<SEAdjustContentView>*) createMusicPickerView: (CGRect) rect
{
    SEMusicPickerView* view  = (SEMusicPickerView*)[[[NSBundle mainBundle] loadNibNamed:@"MusicPicker" owner:self options:nil] lastObject];
    view.mViewNav = self;
    [view initMusicPicker];
    
    return view;
}
- (UIView<SEAdjustContentView>*) createMusicImageListAttachView: (CGRect)r
{
    UIImage* image = [mResLoader getImage:@"MusicImageAttachBackground"];
    SEMusicImageListView* miv = (SEMusicImageListView*)[[[NSBundle mainBundle] loadNibNamed:@"MusicImageListView" owner:self options:nil] lastObject];
    miv.mBackground = image;
    miv.mResLoader = mResLoader;
    miv.mViewNav = self;
    [miv initData];
    
    return miv;
}
- (UIView<SEAdjustContentView>*) createSignaturePreview: (CGRect) rect
{
    SESignaturePreview* view = (SESignaturePreview*)[[[NSBundle mainBundle] loadNibNamed:@"SignaturePreview" owner:self options:nil] lastObject];
    view.mViewNav = self;
    [view initData];
    return view;
}
- (UIView<SEAdjustContentView>*) createSignatureView:(CGRect) rect
{
    SESignatureView* view = (SESignatureView*)[[[NSBundle mainBundle] loadNibNamed:@"SignatureView" owner:self options:nil] lastObject];
    view.backgroundColor = [UIColor redColor];
    view.mViewNav = self;
    view.mResLoader = mResLoader;
    [view initData];
    return view;
}
- (UIView<SEAdjustContentView>*) createOptionsView: (CGRect) rect
{
    SEOptionsView* view = [[SEOptionsView alloc] initWithFrame:rect byViewNav:self withResLoader:mResLoader];
    [view initData];
    return view;
}
- (UIView<SEAdjustContentView>*) createMainDisplayView : (CGRect) rect
{
    SEMainDisplay* mainDisplay = [[SEMainDisplay alloc] initWithResLoader:mResLoader withFrame:rect];
    [mainDisplay loadView];
    [mainDisplay setToolBarButtonHandleTarget:self withAction:@selector(handleToolBarButtonClick:)];
    return mainDisplay;
}
- (UIView<SEAdjustContentView>*) createImagePickerView : (CGRect) rect
{
    SEResLoader* resLoader = mResLoader;
    SEUIScrollView* scrollView = [[SEUIScrollView alloc] init];
    scrollView.mResLoader = resLoader;
    scrollView.mName = @"image picker";
    scrollView.mHandleMultiTouchDelegate = self;
    //scrollView.mNotStartThread = YES;
    //UILongPressGestureRecognizer* lpges = [[UILongPressGestureRecognizer alloc] initWithTarget:self action:@selector(longPressHandler:)];
    //lpges.cancelsTouchesInView = NO;
    //[scrollView addGestureRecognizer:lpges];
    //[lpges release];
    scrollView.backgroundColor = [UIColor whiteColor];
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
    scrollView.mResLoader = resLoader;
    scrollView.mName = @"image selected view";
    scrollView.mHandleMultiTouchDelegate = self;
    scrollView.frame = rect;
    scrollView.backgroundColor = [UIColor whiteColor];
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
        //[mPlacedView release];
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
        //[mPlacedView release];
        mPlacedView = nil;
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
    }
    [self removeAllViewFromRoot];
    mContentContainerParent = [[UIView alloc] init];
    [mRootView addSubview:mContentContainerParent];
    [mContentContainerParent release];
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
@synthesize mViewPortWidth;
@synthesize mViewPortHeight;
@synthesize mPrevView;
@synthesize mNextView;
@synthesize mCurrView;
@synthesize mBarWidth;
@synthesize mRootView;
@synthesize mContentContainerParent;
- (float) getBarWidth
{
    //return 64;
    UIImage* leftImage = [mResLoader getImage:@"LeftBarBg"];
    return leftImage.size.width;
}
- (id) initWithResLoader: (SEResLoader*) resLoader
{
    self = [super init];
    if(self)
    {
        mCurrView = mPrevView = mNextView = INVALID_VIEW;
        mCurrentViewSeqType = MAIN_DISPLAY_ONLY;
        mResLoader = resLoader;
        mBarWidth = [self getBarWidth];
        mViewSeqTypeStack = [NSMutableArray array];
        [mViewSeqTypeStack retain];
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

-(void) initPainterManager
{
    mPainterManager.bgWidth = 1024;
    mPainterManager.bgHeight =768;
}
// Implement loadView to create a view hierarchy programmatically, without using a nib.
- (void)loadView
{
    mPainterManager = [PainterManager painterManager];
    mPainterManager.displayDelegate = self;
    [self initPainterManager];
    [self setupRootView];
    [self initContentContainer];
    [self initBarView];
    [self setCurrentViewSequenceFrame];
    [self initContentContainerParent];
    [self setCurrentView:MAIN_DISPLAY isAnimation:NO];
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
    [super dealloc];
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
    //debug
    if(vp == SELECTED_IMAGE_VIEW)
    {
        viewContainer.backgroundColor = [UIColor redColor];
    }
    NSLog(@"add content to %d , frame = %f, %f, %f, %f", vp, viewContainer.frame.origin.x, viewContainer.frame.origin.y, viewContainer.frame.size.width, viewContainer.frame.size.height);
    //end
    UIView* view = [self createContentView:vp withFrame:frame];
    [viewContainer addSubview:view];
    [viewContainer initContainer];
}
- (BOOL) isSelectedImageViewMid
{
    SEContentViewContainer* container = mViewArray[SELECTED_IMAGE_VIEW];
    UIView* v = container.contentView;
    float w = (self.mViewPortWidth - self.mBarWidth) / 2;
    if(v.frame.size.width == w)
        return true;
    else
        return false;
}
/*
 when set current view, it will display current view and left bar or right bar. it will not create prev view
     or next view.
 pre view and next view will be created when user touch the left bar or right bar
 */
- (void)setCurrentView: (enum VIEW_TYPE) view_type isAnimation: (BOOL)bAnim
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
    currentContainer.backgroundColor = [UIColor redColor];
    ViewSeqProperty currViewSeqProp = getViewSeqProperty(mCurrentViewSeqType);
    int viewIndex = getViewIndexInSequence(currViewSeqProp, mCurrView);
    mPlacedView = nil;
    if(viewIndex != -1)
    {
        float deltax = 0;
        for(int i = 0 ; i < viewIndex ; i++)
        {
            VIEW_TYPE vp = currViewSeqProp.viewseq[i];
            SEContentViewContainer* view = mViewArray[vp];
            deltax += view.frame.size.width + mBarWidth;
        }
        if(deltax > 0)
            deltax -= mBarWidth;
        CGRect frame = mContentContainerParent.frame;
        mContentContainerParent.frame = CGRectMake(-deltax, 0, frame.size.width, frame.size.height);
        [self addContentToContentContainer: mCurrView];
        if(bAnim)
        {
            SEContentViewContainer* prevContainer = [self getContainer:mPrevView];
            prevContainer.hidden = NO;
            SEContentViewContainer* nextContainer = [self getContainer:mNextView];
            nextContainer.hidden = NO;
            frame = mContentContainerParent.frame;
            mContentContainerParent.frame = CGRectMake(0, 0, frame.size.width, frame.size.height);
            void (^animBlock) (void) = ^{
                mContentContainerParent.frame = frame;
            };
            void (^animEnd) (BOOL) = ^(BOOL) {
                SEContentViewContainer* prevContainer = [self getContainer:mPrevView];
                prevContainer.hidden = YES;
                SEContentViewContainer* nextContainer = [self getContainer:mNextView];
                nextContainer.hidden = YES;
            };
            [UIView animateWithDuration:0.5 animations:animBlock completion:animEnd];
        }
        else
        {
            SEContentViewContainer* prevContainer = [self getContainer:mPrevView];
            prevContainer.hidden = YES;
            SEContentViewContainer* nextContainer = [self getContainer:mNextView];
            nextContainer.hidden = YES;
        }
    }
    else
    {
        SEContentViewContainer* prevContainer = [self getContainer:mPrevView];
        prevContainer.hidden = YES;
        SEContentViewContainer* nextContainer = [self getContainer:mNextView];
        nextContainer.hidden = YES;
    }
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
- (void) initState
{
    if([mUserInfoProperty count] > 1)
        return;
    NSManagedObject* userInfo = [mUserInfoProperty objectAtIndex:0];
    mCurrentImageList = [userInfo valueForKey:@"currentimagelist"];
    mCurrentMusicList = [userInfo valueForKey:@"currentmusiclist"];
}
- (void) initData
{
    NSArray* userInfoObjects = [self fetchUserInfo];
    NSUInteger count = [userInfoObjects count];
    if(count == 0)
    {
        NSEntityDescription* selectedImageEntity = [NSEntityDescription entityForName:@"SelectedImage" inManagedObjectContext:self.managedObjectContext];
        NSEntityDescription* imageListEntity = [NSEntityDescription entityForName:@"ImageList" inManagedObjectContext:self.managedObjectContext];
        NSEntityDescription* musicListEntity = [NSEntityDescription entityForName:@"MusicList" inManagedObjectContext:self.managedObjectContext];
        NSEntityDescription* userInfoEntity = [NSEntityDescription entityForName:@"UserInfo" inManagedObjectContext:self.managedObjectContext];
        NSEntityDescription* signatureEntity = [NSEntityDescription entityForName:@"Signature" inManagedObjectContext:self.managedObjectContext];
        NSManagedObject* newUserInfo = [NSEntityDescription insertNewObjectForEntityForName:[userInfoEntity name] inManagedObjectContext:self.managedObjectContext];
        int levelOneImageNum = [mResLoader getInt:@"LevelOneImageNum"];
        NSNumber* num = [NSNumber numberWithInt:levelOneImageNum];
        [newUserInfo setValue:num forKey:@"level"];
        int quality = [mResLoader getInt:@"InitQuality"];
        num = [NSNumber numberWithInt:quality];
        [newUserInfo setValue:num forKey:@"currentimagequality"];
        int times = [mResLoader getInt:@"InitTimes"];
        num = [NSNumber numberWithInt:times];
        [newUserInfo setValue:num forKey:@"currentimagetimes"];
        [newUserInfo setValue:@"default" forKey:@"currentimagelist"];
        [newUserInfo setValue:@"default" forKey:@"currentmusiclist"];
        [newUserInfo setValue:[NSNumber numberWithInt:0] forKey:@"currentsignaturesite"];
        [newUserInfo setValue:[NSNumber numberWithInt:0] forKey:@"currentsignaturesize"];
        [newUserInfo setValue:[NSNumber numberWithInt:0] forKey:@"currentsignature"];
        NSManagedObject* imageList = [NSEntityDescription insertNewObjectForEntityForName:[imageListEntity name] inManagedObjectContext:self.managedObjectContext];
        [imageList setValue:@"default" forKey:@"name"];
        [imageList setValue:[NSNumber numberWithInt:0] forKey:@"seq"];
        NSMutableSet* imageListSet = [newUserInfo valueForKey:@"imagelist"];
        [imageListSet addObject:imageList];
        NSMutableSet* selectedImageSet = [imageList valueForKey:@"selectedimage"];
        for(int i = 0 ; i < levelOneImageNum ; i++)
        {
            SelectedImage* si = [NSEntityDescription insertNewObjectForEntityForName:[selectedImageEntity name] inManagedObjectContext:self.managedObjectContext];
            NSNumber* seq = [NSNumber numberWithInt:i];
            si.seq = seq;
            [selectedImageSet addObject:si];
        }
        NSManagedObject* musicList = [NSEntityDescription insertNewObjectForEntityForName:[musicListEntity name] inManagedObjectContext:self.managedObjectContext];
        [musicList setValue:@"default" forKey:@"name"];
        [musicList setValue:[NSNumber numberWithInt:0] forKey:@"seq"];
        NSMutableSet* musicListSet = [newUserInfo valueForKey:@"musiclist"];
        [musicListSet addObject:musicList];
        ////// init signature
        NSMutableSet* signatureList = [newUserInfo valueForKey:@"signaturelist"];
        NSManagedObject* signature = [NSEntityDescription insertNewObjectForEntityForName:[signatureEntity name] inManagedObjectContext:self.managedObjectContext];
        [signature setValue:[NSNumber numberWithInt:0] forKey:@"seq"];
        [signatureList addObject:signature];
        [signature setValue:@"Koala.jpg" forKey:@"name"];
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
    [self initState];
    //debug
    for(NSUInteger i = 0 ; i < [mUserInfoProperty count] ; i++)
    {
        NSManagedObject* ui = [mUserInfoProperty objectAtIndex:i];
        NSSet* imageListSet = [ui valueForKey:@"imagelist"];
        NSSet* musicListSet = [ui valueForKey:@"musiclist"];
        NSEnumerator* it = [imageListSet objectEnumerator];
        NSManagedObject* imageList = nil;
        while((imageList = [it nextObject]) != nil)
        {
            NSString* name = [imageList valueForKey:@"name"];
            NSLog(@"name = %@", name);
            NSSet* selectedImageSet = [imageList valueForKey:@"selectedimage"];
            NSEnumerator* itSelectedImage = [selectedImageSet objectEnumerator];
            NSManagedObject* si = nil;
            while((si = [itSelectedImage nextObject]) != nil)
            {
                NSLog(@"seq = %@", [si valueForKey:@"seq"]);
                NSLog(@"url = %@", [si valueForKey:@"url"]);
                NSLog(@"urldate = %@", [si valueForKey:@"urldate"]);
                NSLog(@"filepath = %@", [si valueForKey:@"filepath"]);
            }
        }
        /////
        it = [musicListSet objectEnumerator];
        NSManagedObject* musicList = nil;
        while((musicList = [it nextObject]) != nil)
        {
            NSSet* selectedMusicSet = [musicList valueForKey:@"selectedmusic"];
            NSEnumerator* itSelectedMusic = [selectedMusicSet objectEnumerator];
            NSManagedObject* mi = nil;
            while((mi = [itSelectedMusic nextObject]) != nil)
            {
            }
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
    NSSet* imageListSet = [ui valueForKey:@"imagelist"];
    NSEnumerator* it = [imageListSet objectEnumerator];
    NSManagedObject* imageList = nil;
    while((imageList = [it nextObject]) != nil)
    {
        NSString* imageListName = [imageList valueForKey:@"name"];
        if([imageListName isEqualToString:mCurrentImageList])
        {
            NSSet* siSet = [imageList valueForKey:@"selectedimage"];
            NSEnumerator* itSelectedImage = [siSet objectEnumerator];
            SelectedImage* si = nil;
            while((si = [itSelectedImage nextObject]) != nil)
            {
                NSNumber* num = [si valueForKey:@"seq"];
                int n = [num intValue];
                if(n == index)
                    return si;
            }
        }
    }
    return nil;
}
- (NSMutableArray*) getUserImageProperty
{
    NSMutableArray* newArray = [NSMutableArray array];
    for(NSUInteger i = 0 ; i < [mUserInfoProperty count] ; i++)
    {
        NSManagedObject* ui = [mUserInfoProperty objectAtIndex:i];
        NSSet* imageListSet = [ui valueForKey:@"imagelist"];
        NSEnumerator* it = [imageListSet objectEnumerator];
        NSManagedObject* imageList = nil;
        while((imageList = [it nextObject]) != nil)
        {
            NSString* name = [imageList valueForKey:@"name"];
            if([name isEqualToString:mCurrentImageList])
            {
                NSSet* siSet = [imageList valueForKey:@"selectedimage"];
                NSManagedObject* si = nil;
                NSEnumerator* itSelectedImage = [siSet objectEnumerator];
                while((si = [itSelectedImage nextObject]) != nil)
                {
                    NSLog(@"seq = %@", [si valueForKey:@"seq"]);
                    NSLog(@"url = %@", [si valueForKey:@"url"]);
                    NSLog(@"urldate = %@", [si valueForKey:@"urldate"]);
                    NSLog(@"filepath = %@", [si valueForKey:@"filepath"]);
                    [newArray addObject:si];
                }
            }
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

    NSError* error = nil;
    if(self.managedObjectContext != nil)
    {
        if([self.managedObjectContext hasChanges] && ![self.managedObjectContext save:&error])
        {
            NSLog(@"Unresolved error %@, %@", error , [error userInfo]);
            abort();
        }
    }
}
- (UIView<SEAdjustContentView>*) createContentView: (VIEW_TYPE) viewType withFrame: (CGRect)r
{
    if(viewType != INVALID_VIEW)
    {
        UIView<SEAdjustContentView>* view = nil;
        switch (viewType) {
            case MAIN_DISPLAY:
                view = [self createMainDisplayView:r];
                break;
            case IMAGE_PICKER:
                view = [self createImagePickerView:r];
                break;
            case SELECTED_IMAGE_VIEW:
                view = [self createImageSelectedView:r];
                break;
            case MUSIC_PICKER:
                view = [self createMusicPickerView:r];
                break;
            case MUSIC_IMAGE_LIST_ATTACH:
                view = [self createMusicImageListAttachView:r];
                break;
            case OPTIONS:
                view = [self createOptionsView:r];
                break;
            case SIGNATURE_VIEW:
                view = [self createSignatureView:r];
                break;
            case SIGNATURE_PREVIEW:
                view = [self createSignaturePreview:r];
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
- (BOOL) isFloatViewShow
{
    return mFloatView != nil;
}

- (void) moveFloatViewToPoint : (CGPoint)p
{
    if(mFloatView)
    {
        mFloatView.center = p;
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
- (void)touchBegin:(NSArray *)points1 : (NSArray*)points2 withPointDelta: (CGPoint)deltap withScrollView:(SEUIScrollView *)scrollView
{
    SEContentViewContainer* imagePicker = mViewArray[IMAGE_PICKER];
    NSValue* v = [points1 lastObject];
    CGPoint p = [v CGPointValue];
    NSLog(@"p = %f, %f", p.x, p.y);
    if(imagePicker.contentView == scrollView && mFloatView == nil)
    {
        SEUIScrollView* currView = scrollView;
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
        NSLog(@"select index = %d", hp.index);
        mSelectedPhotoURL = [currView getImageURL:hp.index];
        NSLog(@"selected url = %@", mSelectedPhotoURL);
        [currView saveGesture];
        [currView removeAllGestures];
        mPlacedView = nil;
    }
}
- (void) touchMove:(NSArray *)points1 : (NSArray*)points2 withPointDelta: (CGPoint)deltap withScrollView:(SEUIScrollView *)scrollView
{
    SEContentViewContainer* imagePicker = mViewArray[IMAGE_PICKER];
    if(scrollView == imagePicker.contentView)
    {
        if([self isFloatViewShow])
        {
            CGPoint p;
            CGPoint c = mFloatView.center;
            p.x = c.x + deltap.x;
            p.y = c.y + deltap.y;
            NSLog(@"touch in scrollview : p = %f, %f", p.x, p.y);
            [self moveFloatViewToPoint:p];
        }
    }
}
- (void)saveImageURL: (SEImageURL*)imageURL withSeq:(int) seq
{
    SelectedImage* si = [self getSelectedImageProperty:seq];
    if(imageURL.url)
    {
        NSString* str = [imageURL.url absoluteString];
        si.url = str;
    }
    if(imageURL.filepath)
    {
        NSString* str = [imageURL.filepath absoluteString];
        si.filepath = str;
    }
}

- (void) touchEnd:(NSArray *)points1 : (NSArray*)points2 withPointDelta: (CGPoint)deltap withScrollView:(SEUIScrollView *)scrollView
{
    SEContentViewContainer* imagePicker = mViewArray[IMAGE_PICKER];
    if(imagePicker.contentView == scrollView)
    {
        if(mPlacedView)
        {
            SEContentViewContainer* selectedContainer = mViewArray[SELECTED_IMAGE_VIEW];
            SEUIScrollView* selectedScrollView = (SEUIScrollView*)selectedContainer.contentView;
            SEImageURL* imageURL = [selectedScrollView getImageURL:mPlacedViewIndex];
            if(imageURL.filepath == nil && imageURL.url == nil)
            {
                imageURL.url = mSelectedPhotoURL.url;
                mPlacedView.image = mFloatView.image;
                [selectedScrollView removeFromPhotoDict:mPlacedViewIndex];
                [self saveImageURL: imageURL withSeq:mPlacedViewIndex];
            }
            mPlacedView.mImageView.alpha = 1.0;
            mPlacedView.mImageView.opaque = YES;
            [mPlacedView.mImageView setNeedsDisplay];
        }
        if(mFloatView)
        {
            [mFloatView removeFromSuperview];
            mFloatView = nil; 
            [scrollView restoreGesture];
        }
    }
}
- (int) getImageListCount
{
    NSManagedObject* userInfo = [mUserInfoProperty objectAtIndex:0];
    NSSet* imageListSet = [userInfo valueForKey:@"imagelist"];
    return [imageListSet count];
}
- (int) getMusicListCount
{
    NSManagedObject* userInfo = [mUserInfoProperty objectAtIndex:0];
    NSSet* musicListSet = [userInfo valueForKey:@"musiclist"];
    return [musicListSet count];
}
- (int) getImageCount: (NSSet*) selectedImageSet
{
    NSEnumerator* it = [selectedImageSet objectEnumerator];
    SelectedImage* si = nil;
    int count = 0;
    while ((si = [it nextObject]) != nil) 
    {
        if(si.url != nil || si.filepath != nil) 
            count++;
    }
    return count;
}
- (NSArray*) getAllImageList
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* imageListSet = userInfo.imagelist;
    NSArray* imageListArray = [imageListSet allObjects];
    if(imageListArray)
    {
        imageListArray = [imageListArray sortedArrayUsingComparator:^NSComparisonResult(id obj1, id obj2) {
            ImageList* il1 = (ImageList*)obj1;
            ImageList* il2 = (ImageList*)obj2;
            return [il1.seq compare:il2.seq];
            
        }];
        return imageListArray;
    }
    else
        return nil;
    
}

- (NSArray*) getAllMusicList
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* musicListSet = userInfo.musiclist;
    NSArray* musicListArray = [musicListSet allObjects];
    if(musicListArray)
    {
        musicListArray = [musicListArray sortedArrayUsingComparator:^NSComparisonResult(id obj1, id obj2) {
            MusicList* ml1 = (MusicList*)obj1;
            MusicList* ml2 = (MusicList*)obj2;
            return [ml1.seq compare: ml2.seq];
        }];
        return musicListArray;
    }
    else
        return nil;
}
/*
- (NSArray*) getImageListProperty
{
    NSManagedObject* userInfo = [mUserInfoProperty objectAtIndex:0];
    NSSet* imageListSet = [userInfo valueForKey:@"imagelist"];
    NSEnumerator* it = [imageListSet objectEnumerator];
    NSManagedObject* imageList = nil;
    NSArray* imagePropertyArray = [NSArray array];
    while ((imageList = [it nextObject]) != nil) 
    {
        NSSet* siSet = [imageList valueForKey:@"selectedimage"];
        NSString* name = [imageList valueForKey:@"name"];
        SEImageListProperty* ip = [[SEImageListProperty alloc] init];
        ip.imageCount = [self getImageCount:siSet];
        ip.name = name;
        imagePropertyArray = [imagePropertyArray arrayByAddingObject:ip];
        [ip release];
    }
    return imagePropertyArray;
}
- (NSArray*) getMusicListProperty
{
    NSManagedObject* userInfo = [mUserInfoProperty objectAtIndex:0];
    NSSet* musicListSet = [userInfo valueForKey:@"musiclist"];
    NSEnumerator* it = [musicListSet objectEnumerator];
    NSManagedObject* musicList = nil;
    NSArray* musicPropertyArray = [NSArray array];
    while ((musicList = [it nextObject]) != nil) 
    {
        NSSet* miSet = [musicList valueForKey:@"selectedmusic"];
        NSString* name = [musicList valueForKey:@"name"];
        SEMusicListProperty* ip = [[SEMusicListProperty alloc] init];
        ip.musicCount = [miSet count];
        ip.name = name;
        musicPropertyArray = [musicPropertyArray arrayByAddingObject:ip];
        [ip release];
    }
    return musicPropertyArray;
}
 */
- (UserInfo*)getUserInfo
{
    if([mUserInfoProperty count] == 1)
        return [mUserInfoProperty objectAtIndex:0];
    else
        return nil;
}
- (int) getImageQuality
{
    UserInfo* userInfo = [self getUserInfo];
    int ret = [userInfo.currentimagequality intValue];
    return ret;
}
- (int) getImageTimes
{
    UserInfo* userInfo = [self getUserInfo];
    int ret = [userInfo.currentimagetimes intValue];
    return ret;
}
- (NSArray*) getCurrentSelectedMusicArray
{
    NSArray* currentSelectedMusic = [NSArray array];
    UserInfo* userInfo = [mUserInfoProperty objectAtIndex:0];
    NSSet* musicListSet = [userInfo valueForKey:@"musiclist"];
    NSManagedObject* musicList = nil;
    NSEnumerator* itMusicList = [musicListSet objectEnumerator];
    while((musicList = [itMusicList nextObject]) != nil)
    {
        NSString* name = [musicList valueForKey:@"name"];
        if([name isEqualToString:mCurrentMusicList])
        {
            NSSet* smSet = [musicList valueForKey:@"selectedmusic"];
            NSManagedObject* sm = nil;
            NSEnumerator* itSelectedMusic = [smSet objectEnumerator];
            while((sm = [itSelectedMusic nextObject]) != nil)
            {
                SEMusicItemProperty* item = [[SEMusicItemProperty alloc] init];
                NSString* title = [sm valueForKey:@"title"];
                NSString* singer = [sm valueForKey:@"singer"];
                NSString* album = [sm valueForKey:@"album"];
                NSNumber* seqNum = [sm valueForKey:@"seq"];
                item.title = title;
                item.artist = singer;
                item.album = album;
                item.seq = seqNum;
                [item release];
                currentSelectedMusic = [currentSelectedMusic arrayByAddingObject:item];
            }
        }
        
    }
    return currentSelectedMusic;
}
- (void)displayNextImage
{
    int quality = [self getImageQuality];
    int times = [self getImageTimes];
    [mPainterManager initPainterState:quality withTimes:times];
    [mPainterManager nextDisplayStage];
}
- (void) handleAnimEnd:(BOOL)stopInMid withDirection:(int)direct leftContainer:(SEContentViewContainer *)leftContainer rightContainer:(SEContentViewContainer *)rightContianer
{
    if(stopInMid)
        return;
    [self saveContext];
}
- (void) moveToView:(VIEW_SEQ_TYPE)vst : (VIEW_TYPE)vp hasAnimation:(BOOL)isAnim isPush:(BOOL) bPush
{
    if(bPush == YES)
    {
        [self pushViewSeqType:mCurrentViewSeqType];
    }
    mCurrentViewSeqType = vst;
    [self setCurrentViewSequenceFrame];
    [self initContentContainerParent];
    mCurrView = INVALID_VIEW;
    [self setCurrentView:vp isAnimation:isAnim];
    
}
- (VIEW_SEQ_TYPE) popViewSeqType
{
    NSNumber* v = [mViewSeqTypeStack lastObject];
    VIEW_SEQ_TYPE ret =  (VIEW_SEQ_TYPE)[v intValue];
    [mViewSeqTypeStack removeObject:v];
    mCurrentViewSeqType = ret;
    return ret;
}
- (void) pushViewSeqType:(VIEW_SEQ_TYPE)vst
{
    [mViewSeqTypeStack addObject:[NSNumber numberWithInt:vst]];
}
- (NSManagedObject*)newObjectByEntityName: (NSString*) entityName
{
    NSEntityDescription* entity = [NSEntityDescription entityForName:entityName inManagedObjectContext:self.managedObjectContext];
    NSManagedObject* obj = [NSEntityDescription insertNewObjectForEntityForName:[entity name] inManagedObjectContext:self.managedObjectContext];
    return obj;

}
- (Signature*) getCurrentSignature
{
    UserInfo* userInfo = [self getUserInfo];
    NSNumber* sigSeq = userInfo.currentsignature;
    NSSet* signatureList = userInfo.signaturelist;
    Signature* signature = nil;
    NSEnumerator* sigIt = [signatureList objectEnumerator];
    while((signature = [sigIt nextObject]) != nil)
    {
        NSNumber* seq = [signature valueForKey:@"seq"];
        if([seq isEqualToNumber:sigSeq])
        {
            return signature;
        }
    }
    return nil;
}
- (NSArray*) getAllSignatures
{
    
    NSArray* signatureArray = [NSArray array];
    UserInfo* userInfo = [self getUserInfo];
    NSSet* signatureList = userInfo.signaturelist;
    Signature* signature = nil;
    NSEnumerator* sigIt = [signatureList objectEnumerator];
    while((signature = [sigIt nextObject]) != nil)
    {
        signatureArray = [signatureArray arrayByAddingObject:signature];
    }
    signatureArray = [signatureArray sortedArrayUsingComparator:^NSComparisonResult(id obj1, id obj2) 
    {
        Signature* s1 = (Signature*)obj1;
        Signature* s2 = (Signature*) obj2;
        NSNumber* seq1 = s1.seq;
        NSNumber* seq2 = s2.seq;
        return [seq1 compare:seq2];
    }];
    return signatureArray;
}
- (NSArray*) getMusicAttachedImage: (NSString*)name
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* musicListSet = userInfo.musiclist;
    MusicList* musicList = nil;
    NSEnumerator* it = [musicListSet objectEnumerator];
    while ((musicList = [it nextObject]) != nil)
    {
        NSString* musicListName = musicList.name;
        if([musicListName isEqualToString:name])
        {
            NSSet* imageSet = musicList.attachimagelist;
            NSArray* imageArray = [imageSet allObjects];
            imageArray = [imageArray sortedArrayUsingComparator:^NSComparisonResult(id obj1, id obj2) {
                ImageList* list1 = (ImageList*)obj1;
                ImageList* list2 = (ImageList*)obj2;
                return [list1.seq compare:list2.seq];
            }];
            return imageArray;
        }
    }
    return nil;
}
- (void) attachImageToMusic: (NSString*)musicListName imageListName:(NSString*) imageListName
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* musicListSet = userInfo.musiclist;
    MusicList* ml = nil;
    NSEnumerator* itMusicList = [musicListSet objectEnumerator];
    while((ml = [itMusicList nextObject]) != nil)
    {
        if([ml.name isEqualToString:musicListName])
        {
            NSMutableSet* attachImageSet = (NSMutableSet*)ml.attachimagelist;
            for(ImageList* i in attachImageSet)
            {
                if([i.name isEqualToString:imageListName])
                    return;
            }
            ImageList* imageList = [self getImageListByName:imageListName];
            [attachImageSet addObject:imageList];
        }
    }
}
- (ImageList*) getImageListByName: (NSString*)imageListName
{
    UserInfo* userInfo = [self getUserInfo];
    
    NSSet* imageListSet = userInfo.imagelist;
    ImageList* imageList = nil;
    NSEnumerator* it = [imageListSet objectEnumerator];
    while((imageList = [it nextObject]) != nil)
    {
        if([imageList.name isEqualToString:imageListName])
            return imageList;
    }
    return nil;
}
- (MusicList*) getMusicListByName: (NSString*)musicListName
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* musicListSet = userInfo.musiclist;
    NSEnumerator* it = [musicListSet objectEnumerator];
    MusicList* musicList = nil;
    while((musicList = [it nextObject]) != nil)
    {
        if([musicList.name isEqualToString:musicListName])
            return musicList;
    }
    return nil;
}

- (MusicList*) addMusicList:(NSString*) musicListName
{
    MusicList* ml = [self getMusicListByName:musicListName];
    if(ml)
    {
        return ml;
    }
    
    ml = (MusicList*)[self newObjectByEntityName:@"MusicList"];
    ml.name = musicListName;
    NSArray* musicListArray = [self getAllMusicList];
    if(musicListArray)
    {
        MusicList* lastObj = [musicListArray lastObject];
        NSNumber* seq = [NSNumber numberWithInt:[lastObj.seq intValue] + 1];
        ml.seq = seq;
    }
    else
        ml.seq = [NSNumber numberWithInt:0];
    UserInfo* userInfo = [self getUserInfo];
    NSMutableSet* musicListSet = (NSMutableSet*)userInfo.musiclist;
    [musicListSet addObject:ml];
    return ml;
}
- (ImageList*) addImageList: (NSString*) imageListName
{
    ImageList* il = [self getImageListByName:imageListName];
    if(il)
        return il;
    il = (ImageList*)[self newObjectByEntityName:@"ImageList"];
    il.name = imageListName;
    NSArray* imageListArray = [self getAllImageList];
    if(imageListArray)
    {
        ImageList* lastObj = [imageListArray lastObject];
        NSNumber* seq = [NSNumber numberWithInt:[lastObj.seq intValue] + 1];
        il.seq = seq;
    }
    else
        il.seq = [NSNumber numberWithInt:0];
    UserInfo* userInfo = [self getUserInfo];
    NSMutableSet* imageListSet = (NSMutableSet*)userInfo.imagelist;
    [imageListSet addObject:il];
    return il;
}
- (NSArray*) getAttachedMusicListByImageListName: (NSString*)imageListName
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* musicListSet = userInfo.musiclist;
    MusicList* ml = nil;
    NSEnumerator* it = [musicListSet objectEnumerator];
    NSArray* array = [NSArray array];
    while((ml = [it nextObject]) != nil)
    {
        NSSet* attachImageList = ml.attachimagelist;
        NSEnumerator* itImageList = [attachImageList objectEnumerator];
        ImageList* imageList = nil;
        while((imageList = [itImageList nextObject]) != nil)
        {
            if([imageList.name isEqualToString:imageListName])
            {
                array = [array arrayByAddingObject:ml];
            }
        }
    }
    return array;
    
}
- (void) removeImageListByName : (NSString*)imageListName
{
    ImageList* il = [self getImageListByName:imageListName];
    if(il == nil)
        return;
    UserInfo* userInfo = [self getUserInfo];
    [userInfo removeImagelistObject:il];
    NSArray* musicListArray = [self getAttachedMusicListByImageListName:imageListName];
    if(musicListArray)
    {
        for(MusicList* ml in musicListArray)
        {
            NSMutableSet* imageSet = (NSMutableSet*)ml.attachimagelist;
            ImageList* imageList = nil;
            NSEnumerator* it = [imageSet objectEnumerator];
            NSArray* deleteArray = [NSArray array];
            while((imageList = [it nextObject]) != nil)
            {
                if([imageList.name isEqualToString:imageListName])
                {
                    deleteArray = [deleteArray arrayByAddingObject:imageList];
                }
            }
            assert(deleteArray.count <= 1);
            for(ImageList* il in deleteArray)
            {
                NSNumber* seq = il.seq;
                [imageSet removeObject:il];
                NSArray* imageArray = [imageSet allObjects];
                for(ImageList* i in imageArray)
                {
                    NSComparisonResult ret = [i.seq compare:seq];
                    if(ret == NSOrderedDescending)
                    {
                        i.seq = [NSNumber numberWithInt:[i.seq intValue] - 1];
                    }
                    else if(ret == NSOrderedSame)
                    {
                        assert(0);
                        NSLog(@"the same image seq error\n");
                    }
                }

            }
        }
    }
}
- (void) removeMusicListByName : (NSString*)musicListName
{
    UserInfo* userInfo = [self getUserInfo];
    NSMutableSet* musicListSet = (NSMutableSet*)userInfo.musiclist;
    MusicList* musicList = [self getMusicListByName:musicListName];
    if(musicList)
    {
        NSNumber* seq = musicList.seq;
        [musicListSet removeObject:musicList];
        NSArray* allMusicList = [self getAllMusicList];
        for(MusicList* ml in allMusicList)
        {
            NSComparisonResult result = [ml.seq compare:seq];
            if(result == NSOrderedDescending)
            {
                ml.seq = [NSNumber numberWithInt:[ml.seq intValue] - 1];
            }
            else if(result == NSOrderedSame)
            {
                assert(0);
                NSLog(@"the same music seq error\n");
            }
        }
    }
}
- (ImageList*) getImageListBySeq:(NSNumber*)seq
{
    UserInfo* userInfo = [self getUserInfo];
    
    NSSet* imageListSet = userInfo.imagelist;
    ImageList* imageList = nil;
    NSEnumerator* it = [imageListSet objectEnumerator];
    while((imageList = [it nextObject]) != nil)
    {
        if([imageList.seq isEqualToNumber:seq])
            return imageList;
    }
    return nil;    
}
- (MusicList*) getMusicListBySeq: (NSNumber*)seq
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* musicListSet = userInfo.musiclist;
    NSEnumerator* it = [musicListSet objectEnumerator];
    MusicList* musicList = nil;
    while((musicList = [it nextObject]) != nil)
    {
        if([musicList.seq isEqualToNumber:seq])
            return musicList;
    }
    return NO;
}
- (BOOL) musicListContainImageList: (MusicList*)musicList :(NSString*) imageListName
{
    NSSet* attachSet = musicList.attachimagelist;
    for(ImageList* il in attachSet)
    {
        if([il.name isEqualToString:imageListName])
            return YES;
    }
    return NO;
}
- (void) clearPlacedView
{
    mPlacedView = nil;
}
@end
