//
//  SESignatureView.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-2-28.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SESignatureView.h"
#import "SEDrawTouchView.h"
#import "SEUIScrollView.h"
#import "SEViewNavigator.h"
#import "SEUIProgressView.h"
#import "UserInfo.h"
#import "SEUtil.h"
#import "SEResDefine.h"
#import "UserInfo.h"
#import "SESignaturePopupViewController.h"
#import "PhotoFrameAppDelegate.h"
#import "Signature.h"
#import "SESystemConfig.h"
#import "SEPopupViewWidgets.h"
#import "SEMusicImageListPopup.h"
#import "PainterManager.h"
#import "FontLabel.h"
enum ALERT_TYPE {FOR_ADD, FOR_CHANGE};
enum SIGNATURE_VIEW_MODE {EDIT_MODE, VIEW_MODE};  
#define  SIG_SCROLLVIEW_ITEM_DRAWVIEW_HEIGHT 145
#define  SIG_SCROLLVIEW_ITEM_LABEL_HEIGHT 42
#define TOTAL_SIG_LEN 8000
/*
struct SignaturePointData
{
    CGFloat lineWidth;
    NSMutableArray* points;
};
 */

///////
static int convertInt(int num)
{
    int v = num << 24 || num << 16 || num << 8 || (num & 0xFF);
    return v;
}
static void setUISliderBg(UISlider* slider, SEViewNavigator* viewNav)
{
    UIImage* sliderThumb = [viewNav.mResLoader getImage:@"SliderBarThumbImage"];
    [slider setThumbImage:sliderThumb forState:UIControlStateNormal];
    [slider setThumbImage:sliderThumb forState:UIControlStateHighlighted];
    UIImage* sliderMinImage =[viewNav.mResLoader getImage:@"SliderMinTrackImage"];
    sliderMinImage = [SEUtil imageStretchForSlider:sliderMinImage];
    UIImage* sliderMaxImage = [viewNav.mResLoader getImage:@"SliderMaxTrackImage"];
    sliderMaxImage = [SEUtil imageStretchForSlider:sliderMaxImage];
    [slider setMaximumTrackImage:sliderMinImage forState:UIControlStateNormal];
    [slider setMinimumTrackImage:sliderMaxImage forState:UIControlStateNormal];  
}
////////////
@implementation SESignatureButton
- (void) createChild: (CGRect) frame
{
    button = [UIButton buttonWithType:UIButtonTypeCustom];
    button.frame = CGRectMake(0, 0, frame.size.width, frame.size.height);
    [self addSubview: button];
    
    
    CGRect indicateViewRect = CGRectMake(20, 20, 50, 50);
    indicateView = [[UIImageView alloc] initWithFrame:indicateViewRect];
    [self addSubview:indicateView];
    [indicateView release];
    
    CGRect textViewRect = CGRectMake(indicateViewRect.origin.x + indicateViewRect.size.width + 10, indicateViewRect.origin.y, 50, 50);
    
    textImageView = [[UIImageView alloc] initWithFrame:textViewRect];
    [self addSubview:textImageView];
    [textImageView release];
    self.backgroundColor = [UIColor clearColor];
}

- (void) setTextImage:(NSString*)textImageStr indicateImage: (NSString*)indicateImageStr alignment: (UITextAlignment) alignment
{
    if(indicateImageStr == nil)
    {
        int rightpadding = 20;
        int leftpadding = 20;
        UIImage* image = [SESystemConfig getTextImage:textImageStr];
        float startx = 0;
        float starty = (self.frame.size.height - image.size.height) / 2;
        switch (alignment) {
            case UITextAlignmentLeft:
            {
                startx = leftpadding;
            }
                break;
            case UITextAlignmentRight:
            {
                startx = self.frame.size.width - image.size.width - rightpadding;
            }
                break;
            case UITextAlignmentCenter:
            {
                startx = (self.frame.size.width - image.size.width) / 2;
            }
                break;
            default:
                break;
        }
        
        
        textImageView.frame = CGRectMake(startx, starty, image.size.width, image.size.height);
        textImageView.image = image;
        return;
    }
    UIImage* image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:indicateImageStr];
    float indicatorWidth = image.size.width;//self.frame.size.height;
    float indicatorHeight = image.size.height;//self.frame.size.height;
    float paddingx = 10;
    indicateView.frame = CGRectMake(paddingx, (self.frame.size.height - indicatorHeight) / 2, indicatorWidth, indicatorHeight);
    CGSize s = CGSizeMake(indicatorWidth, indicatorHeight);
    CGSize dstS = [SEUtil computeFitSize:CGSizeMake(image.size.width, image.size.height) toDst:s];
    image = [SEUtil drawImage:image toSize:dstS];
    indicateView.image = image;
    
    UIImage* textImage = [SESystemConfig getTextImage:textImageStr];
    float textStartx = 0;
    float textStarty = (self.frame.size.height - textImage.size.height) / 2;
    switch (alignment) {
        case UITextAlignmentLeft:
        {
            textStartx = indicateView.frame.origin.x + indicateView.frame.size.width + 5;
        }
            break;
        case UITextAlignmentRight:
        {
            textStartx = self.frame.size.width - textImage.size.width - 10;
        }
            break;
        case UITextAlignmentCenter:
        {
            textStartx = (self.frame.size.width - image.size.width - textImage.size.width) / 2;
        }
            break;
        default:
            break;
    }
    textImageView.frame = CGRectMake(textStartx, textStarty, textImage.size.width, textImage.size.height);
    textImageView.image = textImage;
}
- (void) setButtonHandler: (id) target action: (SEL)action
{
    [button addTarget:target action:action forControlEvents:UIControlEventTouchUpInside];
}
- (void) setButtonBackground: (NSString*)normal select: (NSString*)select
{
    UIImage* normalImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:normal];
    [button setBackgroundImage:[SEUtil imageWithCap:normalImage top:0.1 bottom:0.9 left:0.1 right:0.9] forState:UIControlStateNormal];
    UIImage* selectImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:select];
    [button setBackgroundImage:[SEUtil imageWithCap:selectImage top:0.1 bottom:0.9 left:0.1 right:0.9] forState:UIControlStateHighlighted];
    
}
- (id) initWithCoder:(NSCoder *)aDecoder
{
    self = [super initWithCoder:aDecoder];
    if(self)
    {
        [self createChild:self.frame];
    }
    return self;
}

- (id) initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if(self)
    {
        [self createChild:frame];
    }
    return self;
}
@end
///////////
@interface SEStrokeView : UIView
{
    CGFloat mLineWidth;
    UIColor* mColor;
}
@property (nonatomic, assign) CGFloat mLineWidth;
@property (nonatomic, retain) UIColor* mColor;
@end
@implementation SEStrokeView
@synthesize mLineWidth;
@synthesize mColor;
- (void) dealloc
{
    [mColor release];
    [super dealloc];
}
- (id) initWithCoder:(NSCoder *)aDecoder
{
    self = [super initWithCoder:aDecoder];
    if(self)
    {
        self.mColor = [UIColor blackColor];
        mLineWidth = 10;
        self.backgroundColor = [UIColor clearColor];
    }
    return self;
}
- (void) drawRect:(CGRect)rect
{
    float width = self.frame.size.width * 2 / 3;
    float startx = self.frame.size.width / 6;
    float starty = (self.frame.size.height - mLineWidth) / 2 + mLineWidth / 2;;
    CGContextRef context = UIGraphicsGetCurrentContext();
    [mColor set];
    CGContextSetLineWidth(context, mLineWidth);
    CGContextSetLineCap(context, kCGLineCapRound);
    CGContextMoveToPoint(context, startx, starty);
    CGContextAddLineToPoint(context, startx + width, starty);
    CGContextStrokePath(context);
}

@end
/////////////
@interface SEColorPadCell : UIView
{
    UIImageView* foregroundView;
    UIView* backgroundView;
    int first;
    int second;
}
@property (nonatomic, assign) int first;
@property (nonatomic, assign) int second;
@property (nonatomic, readonly) UIImageView* foregroundView;
@property (nonatomic, readonly) UIView* backgroundView;
@end
@implementation SEColorPadCell
@synthesize foregroundView;
@synthesize backgroundView;
@synthesize first;
@synthesize second;
- (void) createChild: (CGRect) frame
{
    float paddingx = 5;
    float paddingy = 5 ; 
    backgroundView = [[UIView alloc] initWithFrame:CGRectMake(paddingx, paddingy, frame.size.width - 2 * paddingx, frame.size.height - 2 * paddingy)];
    [self addSubview:backgroundView];
    [backgroundView release];
    
    foregroundView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, frame.size.width, frame.size.height)];
    [self addSubview:foregroundView];
    [foregroundView release];
}
- (id) initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if(self)
    {
        [self createChild:frame];

    }
    return self;
}
- (id) initWithCoder:(NSCoder *)aDecoder
{
    self = [super initWithCoder:aDecoder];
    NSLog(@"color pad rect  = %f, %f, %f, %f", self.frame.origin.x, self.frame.origin.y, self.frame.size.width, self.frame.size.height);
    if(self)
    {
        /*
        foregroundView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, self.frame.size.width, self.frame.size.height)];
        [self addSubview:foregroundView];
        [foregroundView release];
         */
        [self createChild:self.frame];
    }
    return self;
}
@end
////
static UIColor* gColors[4][6];
static UIColor* getColorByIndex(int index)
{
    int first = index / 6;
    int second = index % 6;
    return gColors[first][second];
}
static BOOL whitespaceLine(NSString* line)
{
    NSCharacterSet* cs = [NSCharacterSet whitespaceAndNewlineCharacterSet];
    NSUInteger i;
    for(i = 0 ; i < [line length]; i++)
    {
        unichar c = [line characterAtIndex:i];
        BOOL b = [cs characterIsMember:c];
        if(!b)
            return false;
    }
    return true;
}

static void loadColors()
{
    NSString* fileName = @"colortable.txt";
    NSArray* fileNameArray = [fileName componentsSeparatedByString:@"."];
    if([fileNameArray count] != 2)
    {
        return;
    }
    NSString* s = [fileNameArray objectAtIndex:0];
    NSString* ext = [fileNameArray objectAtIndex:1];
    NSString* filePath = [[NSBundle mainBundle] pathForResource:s ofType:ext];
    NSString* dataContent = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
    NSArray* dataLines = [dataContent componentsSeparatedByString:@"\n"];
    NSUInteger i;
    NSMutableArray* allData = [NSMutableArray array];
    for(i = 0 ; i < [dataLines count] ; i++)
    {
        NSString* line = [dataLines objectAtIndex:i];
        if(whitespaceLine(line) == YES)
            continue;
        NSCharacterSet* cs = [NSCharacterSet whitespaceAndNewlineCharacterSet];
        NSArray* tokens = [line componentsSeparatedByCharactersInSet:cs];
        NSUInteger j;
        NSMutableArray* numberArray = [NSMutableArray array];
        for(j = 0 ; j < [tokens count] ; j++)
        {
            NSString* s = [tokens objectAtIndex:j];
            NSString* tok = [s stringByTrimmingCharactersInSet:cs];
            if(whitespaceLine(tok) == NO)
            {
                [numberArray addObject:[NSNumber numberWithInt:[tok intValue]]];
            }
        }
        assert(numberArray.count == 3);
        [allData addObject:numberArray];
    }
    assert(allData.count == 24);
    for(int i = 0 ; i < allData.count ; i++)
    {
        int first = i / 6;
        int second = i % 6;
        NSArray* a = [allData objectAtIndex:i];
        float r = [[a objectAtIndex:0] intValue] / 255.0f;
        float  g = [[a objectAtIndex:1] intValue] / 255.0f;
        float b = [[a objectAtIndex:2] intValue] / 255.0f;
        NSLog(@"## r = %f, g = %f, b = %f ##\n", r, g, b);
        UIColor* color = [UIColor colorWithRed:r green:g blue:b alpha:1];
        gColors[first][second] = [color retain];
    }

}
@interface SEColorPad : UIView
{
    SEColorPadCell* colorCells[4][6];
    UIImageView* backgroundView;
    SEViewNavigator* mViewNav;
    SESignatureView* sigView;
}
@property (nonatomic, assign) SESignatureView* sigView;
@end
@implementation SEColorPad
@synthesize sigView;
- (void) clickHandler: (UITapGestureRecognizer*) ges
{
    NSLog(@"cell click");
    SEColorPadCell* cell = (SEColorPadCell*)ges.view;
    if([sigView respondsToSelector:@selector(clickColorCell::)])
    {
        NSLog(@"cell call back");
        [sigView clickColorCell:cell.first : cell.second];
    }
}
- (id) initWithFrame:(CGRect)frame withViewNav: (SEViewNavigator*)viewNav;
{
    self = [super initWithFrame:frame];
    if(self)
    {
        mViewNav = viewNav;
        backgroundView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, self.frame.size.width, self.frame.size.height)];
        UIImage* image = [mViewNav.mResLoader getImage:@"SignagureColorPadBg"];
        image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
        backgroundView.image = image;
        [self addSubview:backgroundView];
        [backgroundView release];
        float startx = 10;
        float starty = 10;
        float vspacing = 10;
        float hspacing = 10;
        float width = 96;
        float height = 67;
        image = [mViewNav.mResLoader getImage:@"SignatureColorPadFrame"];
        for(int i = 0  ; i < 4 ; i++)
        {
            startx = 10;
            for(int j = 0 ; j < 6 ; j++)
            {
                colorCells[i][j] = [[SEColorPadCell alloc] initWithFrame:CGRectMake(startx, starty, width, height)];
                startx += hspacing + width;
                colorCells[i][j].backgroundView.backgroundColor = gColors[i][j];
                colorCells[i][j].first = i;
                colorCells[i][j].second = j;
                colorCells[i][j].foregroundView.image = image;
                UITapGestureRecognizer* ges = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(clickHandler:)];
                [colorCells[i][j] addGestureRecognizer:ges];
                [ges release];
                [self addSubview:colorCells[i][j]];
                [colorCells[i][j] release];
            }
            starty += vspacing + height;
        }
    }
    return self;
}

@end
//////
@interface SEColorPadController : UIView
{
    SEViewNavigator* mViewNav;
    SESignatureView* mSigView;
}
@property (nonatomic, assign) SESignatureView* mSigView;
@end

@implementation SEColorPadController
@synthesize mSigView;
- (id) initWithViewNav: (SEViewNavigator*)viewNav
{
    self = [super init];
    if(self)
    {
        mViewNav = viewNav;
    }
    return self;
}
- (void)loadView
{
    SEColorPad* colorPad = [[SEColorPad alloc] initWithFrame:CGRectMake(0, 0, 646, 317) withViewNav:mViewNav];
    colorPad.sigView = mSigView;
    [colorPad autorelease];
}

@end
////////////
@interface SEGroupViewTranslateAnim : NSObject
{
    NSMutableArray* mViewArray;
    CGPoint mTranslate;
    id mTarget;
    SEL mSelector;
    NSMutableArray* mViewAnimEnd;
}
@property (nonatomic, assign) CGPoint mTranslate;

- (void) addView: (UIView*) v;
- (void) startAnim: (id) target endAction: (SEL) selector time:(NSTimeInterval) time;
@end
@implementation SEGroupViewTranslateAnim
@synthesize mTranslate;
- (void) addView:(UIView *)v
{
    [mViewArray addObject:v];
    [mViewAnimEnd addObject:[NSNumber numberWithBool:NO]];
}
- (id) init
{
    self = [super init];
    if(self)
    {
        mViewArray = [NSMutableArray array];
        [mViewArray retain];
        mViewAnimEnd = [NSMutableArray array];
        [mViewAnimEnd retain];
        
    }
    return self;
}
- (void) dealloc
{
    [mViewArray release];
    [mViewAnimEnd release];
    [super dealloc];
}
- (BOOL) isAllAnimEnd
{
    BOOL ret = YES;
    for(NSNumber* n in mViewAnimEnd)
    {
        if([n boolValue] == NO)
        {
            ret = NO;
            break;
        }
    }
    return ret;
}
- (int) getViewIndex: (UIView*)v
{
    for(int i = 0 ; i < mViewArray.count; i++)
    {
        UIView* view = [mViewArray objectAtIndex:i];
        if(view == v)
            return i;
    }
    return -1;
}
- (void) startAnim:(id)target endAction:(SEL)selector time:(NSTimeInterval)time
{
    mTarget = target;
    mSelector = selector;
    for(UIView* view in mViewArray)
    {
        CGPoint p;
        p.x = view.center.x + mTranslate.x;
        p.y = view.center.y + mTranslate.y;
        void (^animBlock)(void) = ^ {
            view.center = p;
        };
        void (^animEndBlock) (BOOL) = ^(BOOL) {
            int index = [self getViewIndex:view];
            assert(index != -1);
            NSLog(@"anim %d end", index);
            [mViewAnimEnd replaceObjectAtIndex:index withObject:[NSNumber numberWithBool:YES]];
            if([self isAllAnimEnd])
            {
                [mTarget performSelector:selector withObject:self];
            }
        };
        [UIView animateWithDuration:time animations:animBlock completion:animEndBlock];
    }
}

@end
///////////////////////////////
@implementation SEScrollViewItem
@synthesize mViewNav;
@synthesize bg;
@synthesize content;
@synthesize textLabel;
- (id) initWithFrame:(CGRect)frame withViewNav:viewNav
{
    self = [super initWithFrame:frame];
    if(self)
    {
        mViewNav = viewNav;
        float width = self.frame.size.width;
        float height = SIG_SCROLLVIEW_ITEM_DRAWVIEW_HEIGHT;
        bg = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, self.frame.size.width, height)];
        content = [[SEDrawTouchView alloc] initWithFrame:CGRectMake(6, 4, self.frame.size.width - 12, height - 12)];
        [content initData];
        height = SIG_SCROLLVIEW_ITEM_LABEL_HEIGHT;
        int padding = 5;
        textLabel = [[SEPopupLabel alloc] initWithFrame:CGRectMake(4, SIG_SCROLLVIEW_ITEM_DRAWVIEW_HEIGHT - padding, frame.size.width - 8, height)];
        UIImage* image = [mViewNav.mResLoader getImage:@"SignatureScrollViewItemTextBg"];
        textLabel.background.image = [SEUtil imageWithCap:image top:0.1 bottom:0.1 left:0.1 right:0.1];
        [self addSubview:bg];
        [self addSubview:content];
        [self addSubview:textLabel];
        [bg release];
        [content release];
        [textLabel release];
        content.userInteractionEnabled = NO;
        [textLabel setTextCenter:YES];
    }
    return self;
}
@end
/////////////////////////////////
@interface SESignatureView (Private)
- (void) setButtonHandler;
- (void) addButtonHandler:(id)sender;
- (void) removeHandler: (id)sender;
- (void) initScrollView;
- (void) savePoints : (NSNumber*)seq;
//- (Signature*) getSignature: (NSNumber*)seq;
- (void)initSignature;
//- (SignaturePointData) getSignaturePoints:(NSNumber*)seq;
- (BOOL) isSignaturePointsEqual:(NSMutableArray*)src : (NSMutableArray*)dst;
- (void) popupAlertViewForSave;
- (void) setSignature: (NSNumber*)seq;
//- (void) setCurrentSelectedSignatureImageView;
- (void) setCurrentSignatureSeq;
- (void) updateInkProgressBar;
- (void) clearDrawView;
//-(SignaturePointData)getSignaturePointsWithSeqName: (NSString*)name;
///////////////////////////////////////////////////////////////////////
- (SEScrollViewItem*) createScrollViewItem: (CGRect) rect background : (UIImage*)background;
- (SEScrollViewItem*) getScrollViewItemBySeq: (int)seq;
- (void) dismissColorPadController;
@end
/////////////////////////
@implementation SESignatureView (Private)
- (void) updateInkProgressBar
{
    float dist = [mDrawView getPointsLength];
    float left = TOTAL_SIG_LEN - dist;
    [mInkProgressView setPercent: left / TOTAL_SIG_LEN];
}
- (void) clearDrawView
{
    [self setSignature:[NSNumber numberWithInt: mCurrentSeq]];
    Signature* currentSig = [mViewNav getSignature:[NSNumber numberWithInt:mCurrentSeq]];
    mDrawingTotalTime = [currentSig.totaltime intValue] * 1000;
    mDrawingConsumeTime = 0;
    float dist = [mDrawView getPointsLength];
    float v = (TOTAL_SIG_LEN - dist) / TOTAL_SIG_LEN;
    mInkProgressView.percent = v;
}
- (void) setCurrentSignatureSeq
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    userInfo.currentsignature = [NSNumber numberWithInt:mCurrentSeq];
}
- (void) savePoints : (NSNumber*)seq;
{
    NSMutableArray* pointsList = [mDrawView getAllNormalizedPoints];
    NSMutableArray* colorList = [mDrawView getPointColorArray];
    assert(pointsList.count == colorList.count);
    if(pointsList.count <= 0)
        return;
    int size = sizeof(int);
    for(NSArray* obj in pointsList)
    {
        size += sizeof(int); //for point count
        for(NSValue* v in obj)
        {
            size += 3 * sizeof(CGFloat); // for line width and x, y coordinate
            size += sizeof(long); //for time
            size += sizeof(long) + sizeof(long);//for sec, usec
        }
    }
    for(int i = 0 ; i < colorList.count ; i++)
    {
        size += 4 * sizeof(CGFloat);
    }
    UInt8* data = (UInt8*)malloc(size);
    NSOutputStream* output = [NSOutputStream outputStreamToBuffer:data capacity:size];
    [output open];
    //NSStreamStatus ss = output.streamStatus;
    int num = pointsList.count;
    int ret = [output write:(const uint8_t*)&num maxLength:sizeof(int)];
    assert(ret != -1);
    for(NSArray* obj in pointsList)
    {
        int count = obj.count;
        ret = [output write:(const uint8_t *)&count maxLength:sizeof(int)];
        assert(ret != -1);
        for(SEDrawTouchPoint* point in obj)
        {
            CGPoint p = point.point;
            CGFloat lw = point.lineWidth;
            long time = point.milliTime;
            long sec = point.sec;
            long usec = point.usec;
            ret = [output write:(const uint8_t *)(&p.x) maxLength:sizeof(CGFloat)];
            assert(ret != -1);
            ret = [output write:(const uint8_t *) (&p.y) maxLength:sizeof(CGFloat)];
            assert(ret != -1);
            ret = [output write:(const uint8_t*)&lw maxLength:sizeof(CGFloat)];
            assert(ret != -1);
            ret = [output write: (const uint8_t*)&time maxLength:sizeof(long)];
            assert(ret != -1);
            
            ret = [output write: (const uint8_t*)&sec maxLength:sizeof(long)];
            assert(ret != -1);
            ret = [output write: (const uint8_t*)&usec maxLength:sizeof(long)];
            assert(ret != -1);
        }
    }
    for(int i = 0 ; i < colorList.count ; i++)
    {
        UIColor* c = [colorList objectAtIndex:i];
        CGFloat r = 0, g = 0, b = 0, a = 1;
        CGColorRef cgColor = [c CGColor];
        int version = [SEUtil getSystemVersion];
        if(version <= 4)
        {
            int numComponents = CGColorGetNumberOfComponents(cgColor);
            if(numComponents == 4)
            {
                const CGFloat *components = CGColorGetComponents(cgColor);
                r = components[0];
                g = components[1];
                b = components[2];
            }
        }
        else
        {
            [c getRed:&r green:&g blue:&b alpha:&a];
        }
        ret = [output write:(const uint8_t*)(&r) maxLength:sizeof(CGFloat)];
        assert(ret != -1);
        ret = [output write:(const uint8_t*)(&g) maxLength:sizeof(CGFloat)];
        assert(ret != -1);
        ret = [output write:(const uint8_t*)(&b) maxLength:sizeof(CGFloat)];
        assert(ret != -1);
        ret = [output write:(const uint8_t*)(&a) maxLength:sizeof(CGFloat)];
        assert(ret != -1);
        
    }
    [output close];
    NSData* nsdata = [NSData dataWithBytes:data length:size];
    free(data);
    Signature* sig = [mViewNav getSignature:seq];
    sig.data = nsdata;
    [mViewNav saveCoreDataContext];
}
/*
- (void) popupSignatureSave
{
    SESignaturePopupViewController* sigDlg= [[SESignaturePopupViewController alloc] init];
    sigDlg.modalInPopover = YES;
    sigDlg.mSignatureView = self;
    UIPopoverController* pop = [[UIPopoverController alloc] initWithContentViewController:sigDlg];
    pop.popoverContentSize = CGSizeMake(320, 200);
    self.mSigViewController = sigDlg;
    [sigDlg release];
    mPopup = pop;
    [pop presentPopoverFromRect:CGRectMake(640, 300, 200, 200) inView:self permittedArrowDirections: UIPopoverArrowDirectionAny animated:YES];
}
- (void)dismissPopup
{
    [mSigViewController release];
    mSigViewController = nil;
    [mPopup dismissPopoverAnimated:YES];
}
 */
- (void) removeHandler:(id)sender
{
}
- (void) saveToDocument: (NSString*) fileName
{
    
}
- (void) scrollViewItemTapHandler: (UITapGestureRecognizer*)tap
{
    NSLog(@"scroll view item tap");
    if(mCurrentMode == EDIT_MODE)
        return;
    SEScrollViewItem* item = (SEScrollViewItem*)tap.view;
    if(mCurrentSeq == item.tag)
        return;
    NSLog(@"mCurrentSeq = %d, select seq = %d", mCurrentSeq, item.tag);
    SEScrollViewItem* currItem = [self getScrollViewItemBySeq:mCurrentSeq];
    currItem.bg.image = [SEUtil imageWithCap:mScrollViewNormalItemImage top:0.1 bottom:0.9 left:0.1 right:0.9];
    //currItem.textField.background = [SEUtil imageWithCap: mScrollViewNormalItemImage top:0.1 bottom:0.9 left:0.1 right:0.9];;
    currItem.textLabel.background.image = [SEUtil imageWithCap: mScrollViewNormalItemImage top:0.1 bottom:0.9 left:0.1 right:0.9];
    item.bg.image = [SEUtil imageWithCap: mScrollViewSelectedItemImage top:0.1 bottom:0.9 left:0.1 right:0.9]; 
    //item.textField.background = [SEUtil imageWithCap: mScrollViewSelectedItemImage top:0.1 bottom:0.9 left:0.1 right:0.9];;
    item.textLabel.background.image = [SEUtil imageWithCap: mScrollViewSelectedItemImage top:0.1 bottom:0.9 left:0.1 right:0.9];

    mCurrentSeq = item.tag;
    UserInfo* userInfo = [mViewNav getUserInfo];
    userInfo.currentsignature = [NSNumber numberWithInt:item.tag];
    [self setSignature:[NSNumber numberWithInt: mCurrentSeq]];
    [mViewNav saveCoreDataContext];
    Signature* currentSig = [mViewNav getSignature:[NSNumber numberWithInt:mCurrentSeq]];
    mDrawingTotalTime = [currentSig.totaltime intValue] * 1000;
    mDrawingConsumeTime = 0;
    float dist = [mDrawView getPointsLength];
    float v = (TOTAL_SIG_LEN - dist) / TOTAL_SIG_LEN;
    mInkProgressView.percent = v;
}
- (SEScrollViewItem*) createScrollViewItem : (CGRect) rect background : (UIImage*)background
{
    SEScrollViewItem* item = [[SEScrollViewItem alloc] initWithFrame:rect withViewNav:mViewNav];
    //item.mViewNav = mViewNav;
    item.userInteractionEnabled = YES;
    item.bg.image = [SEUtil imageWithCap: background top:0.1 bottom:0.9 left:0.1 right:0.9];
    item.textLabel.background.image = [SEUtil imageWithCap: background top:0.1 bottom:0.9 left:0.1 right:0.9];
    item.content.backgroundColor = [UIColor clearColor];
    [item autorelease];
    UITapGestureRecognizer* tap = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(scrollViewItemTapHandler:)];
    [item addGestureRecognizer:tap];
    [tap release];
    return item;
}
- (BOOL) isSigNameExist: (NSString*) str
{
    NSArray* allSignatures = [mViewNav getAllSignatures];
    for(Signature* sig in allSignatures)
    {
        NSLog(@"sig name = %@", sig.name);
        if([sig.name isEqualToString:str])
            return YES;
    }
    return NO;
}
- (void) initScrollView
{
    NSArray* allSignatures = [mViewNav getAllSignatures];
    float startx = 0;
    float starty = 0;
    float width = mScrollView.frame.size.width;
    float height = SIG_SCROLLVIEW_ITEM_DRAWVIEW_HEIGHT + SIG_SCROLLVIEW_ITEM_LABEL_HEIGHT;
    for(Signature* sig in allSignatures)
    {
        NSString* str = [NSString stringWithFormat:@"%d", [sig.seq intValue]];
        int seq = [sig.seq intValue];
        SignaturePointData sd = [mViewNav getSignaturePointsWithSeqName:str];
        UIImage* background = nil;
        if(seq == mCurrentSeq)
            background = mScrollViewSelectedItemImage;
        else
            background = mScrollViewNormalItemImage;
        
        SEScrollViewItem* item = [self createScrollViewItem: CGRectMake(startx, starty, width, height) background:background];
        item.tag = seq;
        [item.content setLineWidthRatio:mLineWidthRatio];
        [item.content setStep:0.2];
        if(sd.points.count > 0)
        {
            //image = [SEUtil createUIImageFrom: background  withLineWidth: (CGFloat) sd.lineWidth drawPoints: sd.points];
            [item.content setPointColorArray:sd.colors];
            [item.content setNormalizePoints:sd.points];
            
        }
        item.textLabel.label.text = sig.name;
        [SESystemConfig setLabelFont:item.textLabel.label text:sig.name color:[UIColor whiteColor] fontName:[SESystemConfig getFontName] fontSize:18];
        [mScrollView addSubview:item];
        starty += height;
        
    }
    mScrollView.contentSize = CGSizeMake(width, starty);
    mScrollView.alwaysBounceVertical = YES;
}
- (void) realAdd:(id)sender
{
    NSLog(@"realSave\n");
    NSString* text = [mSigViewController textString];
    if([text isEqualToString:@""])
        return;
    mSavedSignatureName = text;
    UserInfo* userInfo = [mViewNav getUserInfo];
    NSSet* signatureList = userInfo.signaturelist;
    NSManagedObject* signature = nil;
    NSEnumerator* it = [signatureList objectEnumerator];
    NSNumber* maxseq = [NSNumber numberWithInt:-1];
    while((signature = [it nextObject]) != nil)
    {
        NSNumber* seq = [signature valueForKey:@"seq"];
        if([seq compare:maxseq] == NSOrderedDescending)
        {
            maxseq = seq;
        }
    }
    maxseq = [NSNumber numberWithInt:([maxseq intValue] + 1)];
    NSManagedObject* object = [mViewNav newObjectByEntityName: @"Signature"];
    [object setValue:maxseq forKey:@"seq"];
    [object setValue:mSavedSignatureName forKey:@"name"];
    [self saveToDocument:mSavedSignatureName];
    userInfo.currentsignature = maxseq;
    //[userInfo addSignaturelistObject:object];
    NSMutableSet* signatureSet = (NSMutableSet*)userInfo.signaturelist;
    [signatureSet addObject:object];
    [mViewNav saveCoreDataContext];
    [self dismissPopup];
    mCurrentSeq = [maxseq intValue];
    
    CGRect r = mScrollView.frame;
    [mScrollView removeFromSuperview];
    mScrollView = [[SEUIScrollView alloc] init];
    mScrollView.frame = r;
    [self initScrollView];
    [self addSubview:mScrollView];
    //[self setCurrentSelectedSignatureImageView];
    //mDrawView.lineWidth = 8.0f;
    [mDrawView clearPoints];
    [mDrawView setNeedsDisplay];
}
- (void) cancel:(id)sender
{
    NSLog(@"cancel\n");
    [self dismissPopup];
}
- (BOOL) isSignaturePointsEqual:(NSMutableArray*)src : (NSMutableArray*)dst
{
    if(dst == nil && src == nil)
        return YES;
    if(src == nil && dst.count == 0)
        return YES;
    if(dst == nil && src.count == 0)
        return YES;
    if(dst.count == 0 && src.count == 0 )
        return YES;
    if(src == nil && dst != nil)
        return NO;
    if(dst == nil && src != nil)
        return NO;
    if(src.count != dst.count)
        return NO;
    assert(src.count == dst.count);
    NSUInteger i;
    for(i = 0; i < src.count; i++)
    {
        NSArray* srcArray = [src objectAtIndex:i];
        NSArray* dstArray = [dst objectAtIndex:i];
        if(srcArray.count != dstArray.count)
            return NO;
        for(int j = 0 ; j < srcArray.count ; j++)
        {
            SEDrawTouchPoint* v1 = [srcArray objectAtIndex:j];
            SEDrawTouchPoint* v2 = [dstArray objectAtIndex:j];
            CGPoint p1 = v1.point;
            CGPoint p2 = v2.point;
            CGFloat lw1 = v1.lineWidth;
            CGFloat lw2 = v2.lineWidth;
            if(fabsf(p1.x - p2.x) > 0.001 || fabsf(p1.y - p2.y) > 0.001 || fabsf(lw1 - lw2) > 0.001)
                return NO;
        }
    }
    return YES;
}
- (void) setSignature: (NSNumber*)seq
{
    SignaturePointData sd = [mViewNav getSignaturePoints:seq];
    if(sd.points && sd.points.count > 0)
    {
        [mDrawView setPointColorArray:sd.colors];
        [mDrawView setNormalizePoints:sd.points];
    }
    else
    {
        [mDrawView clearPoints];
    }    
    [mDrawView setNeedsDisplay];
}
- (void) popupAlertViewForSave
{
    UIAlertView* alert = [[UIAlertView alloc] initWithTitle:@"Save Current Work" message:@"Do you want to save your work?" delegate:self cancelButtonTitle:@"YES" otherButtonTitles:@"NO", nil];
    [alert show];
    [alert release];

}
- (void) realDeleteHandler
{
    NSArray* sigs = [mViewNav getAllSignatures];
    UserInfo* userInfo = [mViewNav getUserInfo];
    if(sigs.count == 0)
    {
        mCurrentSeq = -1;
        userInfo.currentsignature = [NSNumber numberWithInt: mCurrentSeq];
        [mViewNav dismissConfirmDlg];
        [mInkProgressView setPercent:1];
        return;
    }
    int oldCount = sigs.count;
    int deleteSeq = mCurrentSeq;
    Signature* deleteSig = nil;
    for(Signature* sig in sigs)
    {
        if(deleteSeq == [sig.seq intValue])
        {
            deleteSig = sig;
            break;
        }
    }
    //assert(deleteSig != nil);
    
    if(deleteSig == nil)
    {
        NSLog(@"can not find deleted signature");
        [mViewNav dismissConfirmDlg];
        return;
    }
    
    NSLog(@"delete signatuer = %d ", deleteSeq);
    
    [userInfo removeSignaturelistObject:deleteSig];
    sigs = [mViewNav getAllSignatures];
    assert(oldCount > sigs.count);
    for(int i = 0 ; i < sigs.count ; i++)
    {
        Signature* sig = [sigs objectAtIndex:i];
        if([sig.seq intValue] > deleteSeq)
        {
            sig.seq = [NSNumber numberWithInt:[sig.seq intValue] - 1];
        }
    }
    for(int i = 0 ; i < mScrollView.subviews.count ; i++)
    {
        UIView* v = [mScrollView.subviews objectAtIndex:i];
        if([v isMemberOfClass:[SEScrollViewItem class]])
        {
            if(v.tag > deleteSeq)
            {
                v.tag -= 1;
                v.frame = CGRectMake(v.frame.origin.x, v.frame.origin.y - v.frame.size.height, v.frame.size.width, v.frame.size.height);
            }
        }
    }
    SEScrollViewItem* deleteScrollViewItem = nil;
    for(int i = 0 ; i < mScrollView.subviews.count ; i++)
    {
        UIView* v = [mScrollView.subviews objectAtIndex:i];
        if([v isMemberOfClass:[SEScrollViewItem class]])
        {
            if(v.tag == deleteSeq)
            {
                deleteScrollViewItem = (SEScrollViewItem*)v;
                break;
            }
        }
    }    
    assert(deleteScrollViewItem != nil);
    [deleteScrollViewItem removeFromSuperview];
    
    if(sigs.count == 0)
    {
        mCurrentSeq = -1;
        userInfo.currentsignature = [NSNumber numberWithInt: mCurrentSeq];
        [self setSignature:[NSNumber numberWithInt:mCurrentSeq]];
        [mViewNav saveCoreDataContext];
        [mViewNav dismissConfirmDlg];
        mDrawingTotalTime = 10 * 1000;
        mDrawingConsumeTime = 0;
        [mInkProgressView setPercent:1];
        return;
    }
    if(deleteSeq > 0)
    {
        if(sigs.count == 1)
        {
            mCurrentSeq = 0;
        }
        else
        {
            if(deleteSeq == sigs.count)
            {
                mCurrentSeq = sigs.count - 1;
            }
        }
    }
    userInfo.currentsignature = [NSNumber numberWithInt: mCurrentSeq];
    SEScrollViewItem* currItem = [self getScrollViewItemBySeq:mCurrentSeq];
    if(currItem)
    {
        currItem.bg.image = [SEUtil imageWithCap: mScrollViewSelectedItemImage top:0.1 bottom:0.9 left:0.1 right:0.9];
        currItem.textLabel.background.image = [SEUtil imageWithCap:mScrollViewSelectedItemImage top:0.1 bottom:0.9 left:0.1 right:0.9];
        
    }
    /*
     else
     {
     [mDrawView clearPoints];  
     [mDrawView setNeedsDisplay];
     }
     */
    [self setSignature:[NSNumber numberWithInt: mCurrentSeq]];
    [mViewNav saveCoreDataContext];
    [mScrollView setNeedsDisplay];
    [mViewNav dismissConfirmDlg];
    Signature* currentSig = [mViewNav getSignature:[NSNumber numberWithInt:mCurrentSeq]];
    mDrawingTotalTime = [currentSig.totaltime intValue] * 1000;
    assert(mDrawingTotalTime > 0);
    mDrawingConsumeTime = 0;
    [self updateInkProgressBar];
}
- (void) deleteCancelHandler
{
    [mViewNav dismissConfirmDlg];
}
- (void) deleteButtonHandler: (id)sender
{
    NSLog(@"delete button handler");
    //for signature anim test
    //[mDrawView startAnimDraw];
    //end
    
    [mViewNav createConfirmDlg:self ok:@selector(realDeleteHandler) cancel:@selector(deleteCancelHandler)];
    SEConfirmDlg* dlg = [mViewNav getConfirmDlg];
    dlg.message.text = @"Do you want to delete this signature?";
    [mViewNav showConfirmDlg];
    
}
/*
- (void) completeButtonHandler: (id)sender
{
    [self savePoints:[NSNumber numberWithInt: mCurrentSeq]];
    NSArray* points = [mDrawView getAllNormalizedPoints];
    if(points.count > 0)
    {
        CGRect r = mScrollView.frame;
        [mScrollView removeFromSuperview];
        mScrollView = [[SEUIScrollView alloc] init];
        mScrollView.frame = r;
        [self initScrollView];
        [self addSubview:mScrollView];
    }
    [self setCurrentSignatureSeq];
    [mViewNav saveCoreDataContext];
}
 */
- (CGRect) getNextScrollViewItem
{
    NSArray* subviews = mScrollView.subviews;
    CGRect r = CGRectMake(0, -999, 0, 0);
    for(int i = 0 ;i < subviews.count ; i++)
    {
        UIView* v = [subviews objectAtIndex:i];
        if([v isMemberOfClass:[SEScrollViewItem class]])
        {
            if(r.origin.y < v.frame.origin.y)
            {
                r = v.frame;
            }
        }
    }
    r = CGRectMake(r.origin.x, r.origin.y + r.size.height, r.size.width, r.size.height);
    return r;
}
- (SEScrollViewItem*) getScrollViewItemBySeq: (int)seq
{
    NSArray* subviews = mScrollView.subviews;
    int j = 0;
    for(int i = 0 ;i < subviews.count ; i++)
    {
        UIView* v = [subviews objectAtIndex:i];
        if([v isMemberOfClass:[SEScrollViewItem class]])
        {
             if(j == seq)
                 return (SEScrollViewItem*)v;
             else 
             {
                 j++;
             }
        }
    }    
    return nil;
}
- (void) cancelHandler
{
    [mViewNav hideMusicImageAttachInputView];
}
- (void) realAddHandler
{
    SEMusicImageListPopupView* popupView = [mViewNav getMusicImageListPopupView] ;
    NSString* text = [popupView getContentText];
    if(text == nil || [text isEqualToString:@""])
    {
        [mViewNav hideMusicImageAttachInputView];
        return;
    }
    if([self isSigNameExist:text])
    {
        SEMusicImageListPopupView* popupView = [mViewNav getMusicImageListPopupView];
        popupView.errorMsg.text = @"This signature name has existed. Please input a new name";
        return;
    }
    CGRect r = [self getNextScrollViewItem];
    NSLog(@"new rect = %f, %f, %f, %f", r.origin.x, r.origin.y, r.size.width, r.size.height);
    if(r.origin.y == -999)
    {
        r = CGRectMake(0, 0, mScrollView.frame.size.width, SIG_SCROLLVIEW_ITEM_DRAWVIEW_HEIGHT + SIG_SCROLLVIEW_ITEM_LABEL_HEIGHT);
    }
    SEScrollViewItem* item = [self createScrollViewItem:r background:mScrollViewSelectedItemImage];
    [mScrollView addSubview:item];
    mScrollView.contentSize = CGSizeMake(item.frame.size.width, item.frame.origin.y + item.frame.size.height);
    [mScrollView setNeedsDisplay];
    NSArray* sigs = [mViewNav getAllSignatures];
    item.tag = sigs.count;
    NSManagedObject* object = [mViewNav newObjectByEntityName: @"Signature"];
    [object setValue:[NSNumber numberWithInt: sigs.count] forKey:@"seq"];
    [object setValue:text forKey:@"name"];
    UserInfo* userInfo = [mViewNav getUserInfo];
    [userInfo addSignaturelistObject:(Signature*)object];
    
    SEScrollViewItem* currItem = [self getScrollViewItemBySeq:mCurrentSeq];
    if(currItem)
    {
        currItem.bg.image = [SEUtil imageWithCap: mScrollViewNormalItemImage top:0.1 bottom:0.9 left:0.1 right:0.9 ] ;
        currItem.textLabel.background.image = [SEUtil imageWithCap: mScrollViewNormalItemImage top:0.1 bottom:0.9 left:0.1 right:0.9 ] ;
        //currItem.textField.background = [SEUtil imageWithCap: mScrollViewNormalItemImage top:0.1 bottom:0.9 left:0.1 right:0.9 ] ;
    }
    [SESystemConfig setLabelFont:item.textLabel.label text:text color:[UIColor whiteColor] fontName:[SESystemConfig getFontName] fontSize:20];
    mCurrentSeq = sigs.count;
    userInfo.currentsignature = [NSNumber numberWithInt: mCurrentSeq];
    [self setSignature:[NSNumber numberWithInt:mCurrentSeq]];
    [mViewNav saveCoreDataContext];
    [mViewNav hideMusicImageAttachInputView];
    Signature* currentSig = [mViewNav getSignature:[NSNumber numberWithInt:mCurrentSeq]];
    mDrawingTotalTime = [currentSig.totaltime intValue] * 1000;
    assert(mDrawingTotalTime > 0);
    mDrawingConsumeTime = 0;
    [self updateInkProgressBar];
}
- (void) addButtonConfirmDlgHandler
{
    [mViewNav dismissConfirmDlg];
}
- (void) addButtonHandler:(id)sender
{
    NSLog(@" signature add button handler  ");
    NSArray* allSignatures = [mViewNav getAllSignatures];
    int maxCount = 10;
    if(allSignatures.count >= maxCount)
    {
        [mViewNav dismissConfirmDlg];
        [mViewNav createConfirmDlg:self ok:@selector(addButtonConfirmDlgHandler)];
        SEConfirmDlg* dlg = [mViewNav getConfirmDlg];
        [dlg setMessageText:@"Warning!"];
        NSString* str = [NSString stringWithFormat:@"You exceed the max signature count %d .", maxCount];
        [dlg setMessage2Text:str];
        [mViewNav showConfirmDlg];
    }
    else 
    {
        [mViewNav createMusicImageAttachInputView];
        SEMusicImageListPopupView* popupView = [mViewNav getMusicImageListPopupView];
        [popupView setHandler:self ok:@selector(realAddHandler) cancel:@selector(cancelHandler)];
        popupView.title.text = @"Please input signature name";
        NSArray* allSignatures = [mViewNav getAllSignatures];
        NSMutableArray* sigNames = [NSMutableArray array];
        for(int i = 0 ; i < allSignatures.count ; i++)
        {
            Signature* s = [allSignatures objectAtIndex:i];
            [sigNames addObject:s.name];
        }
        NSString* str = [SEUtil createNextName:@"Signature" :sigNames :allSignatures.count + 1];
        NSString* contentText = str;//[NSString stringWithFormat:@"Signature%d", allSignatures.count + 1];
        popupView.content.text = contentText;
        [mViewNav showMusicImageAttachInputView];
    }
}
- (IBAction) outputSignature:(id)sender
{
    NSLog(@"outputsignature");
    Signature* sig = [mViewNav getSignature: [NSNumber numberWithInt: mCurrentSeq]];
    NSData* data = sig.data;
    NSArray* strArray = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES);
    NSString* docs = [strArray objectAtIndex:0];
    NSString* fileName = @"signature";
    NSString* path = [docs stringByAppendingFormat:@"%@%@%@", @"/", fileName, @".dat"];
    NSLog(@"## saved image path = %@ ##", path);
    NSError* writeError = nil;
    BOOL ret = [data writeToFile:path options:NSDataWritingAtomic error:&writeError];
    if(ret == NO && writeError != nil)
    {
        NSLog(@" Error save data : %@", path);
    }
}
- (void) editInterfaceAnimHandler: (id) animSelf
{
    [animSelf release];
    mCurrentMode = EDIT_MODE;
}
- (void) clearButtonHandler: (id)sender
{
    [self clearDrawView];
    /*
    [self setSignature:[NSNumber numberWithInt: mCurrentSeq]];
    Signature* currentSig = [mViewNav getSignature:[NSNumber numberWithInt:mCurrentSeq]];
    mDrawingTotalTime = [currentSig.totaltime intValue] * 1000;
    mDrawingConsumeTime = 0;
    float dist = [mDrawView getPointsLength];
    float v = (TOTAL_SIG_LEN - dist) / TOTAL_SIG_LEN;
    mInkProgressView.percent = v;
    //[mDrawView clearPoints];
    //[mDrawView setNeedsDisplay];
     */
}
- (void) animEndHandler1: (id)animSelf
{
    [animSelf release];
    mCurrentMode = VIEW_MODE;
}
- (void) editInterfaceAnimHandler1: (id)animSelf
{
    [animSelf release];
    SEGroupViewTranslateAnim* anim = [[SEGroupViewTranslateAnim alloc] init];
    anim.mTranslate = CGPointMake(0, -mAddDeleteBg.frame.size.height);
    [anim addView:mEditButton];
    [anim addView:mAddDeleteBg];
    [anim addView:mAddButton];
    [anim addView:mDeleteButton];
    [anim startAnim:self endAction:@selector(animEndHandler1:) time:0.2];
    mDrawView.userInteractionEnabled = NO;
}
- (void) departEditMode
{
    [mViewNav dismissConfirmDlg];
    SEGroupViewTranslateAnim* anim = [[SEGroupViewTranslateAnim alloc] init];
    anim.mTranslate = CGPointMake(0, mAddDeleteBg.frame.size.height);
    
    [anim addView:mEditPannel];
    [anim startAnim:self endAction:@selector(editInterfaceAnimHandler1:) time:0.2];
}
- (void) realSavePoints
{
    NSMutableArray* data = [mDrawView getAllNormalizedPoints];
    if(data == nil || data.count == 0)
        return;
    NSMutableArray* colors = [mDrawView getPointColorArray];
    SEScrollViewItem* item =[self getScrollViewItemBySeq : mCurrentSeq];
    [item.content setLineWidthRatio:mLineWidthRatio];
    [item.content setStep:0.2];
    [item.content setPointColorArray:colors];
    [item.content setNormalizePoints:data];
    [item.content setNeedsDisplay];
    [item setNeedsDisplay];
    [self savePoints:[NSNumber numberWithInt:mCurrentSeq]];
    [mViewNav saveCoreDataContext];
    [self departEditMode];
}
- (void) backButtonCancelHandler
{
    [self clearDrawView];
    [self departEditMode];
}
- (void) backButtonHandler: (id) sender
{
    NSMutableArray* drawViewPoints = [mDrawView getAllNormalizedPoints];
    SignaturePointData sd = [mViewNav getSignaturePoints:[NSNumber numberWithInt: mCurrentSeq]];
    NSMutableArray* scrollItemPoints = sd.points;
    BOOL equal = [self isSignaturePointsEqual:drawViewPoints : scrollItemPoints];
    if(equal)
    {
        [self departEditMode];
    }
    else
    {
        [mViewNav createConfirmDlg:self ok:@selector(realSavePoints) cancel:@selector(backButtonCancelHandler)];
        SEConfirmDlg* dlg = [mViewNav getConfirmDlg];
        dlg.message.text = @"Do you want to save this signature?";
        [mViewNav showConfirmDlg];
    }
    
    //NSMutableArray* scrollItemPoints = [mViewNav getSignaturePoints:[NSNumber numberWithInt: mCurrentSeq];
    /*
    SEGroupViewTranslateAnim* anim = [[SEGroupViewTranslateAnim alloc] init];
    anim.mTranslate = CGPointMake(0, mAddDeleteBg.frame.size.height);

    [anim addView:mEditPannel];
    [anim startAnim:self endAction:@selector(editInterfaceAnimHandler1:) time:0.2];
     */
}

- (void) saveButtonHandler: (id) sendler
{
    [self realSavePoints];
}
- (void) addTitleForView: (UIView*) view title: (NSString*)str
{
    UIImage* title = nil;
    if([str isEqualToString:@"save"])
    {
        //title = [mViewNav.mResLoader getImage:@"save.png"];
        title = [UIImage imageNamed:@"save.png"];
    }
    else if([str isEqualToString:@"clear"])
    {
        //title = [mViewNav.mResLoader getImage:@"clean.png"];
        title = [UIImage imageNamed:@"clean.png"];
    }
    else if([str isEqualToString:@"back"])
    {
        //title = [mViewNav.mResLoader getImage:@"back.png"];
        title = [UIImage imageNamed:@"back.png"];
    }
    else 
    {
        return;
    }
    if(title == nil)
        return;
    float startx = view.frame.origin.x + (view.frame.size.width - title.size.width) / 2;
    float starty = view.frame.origin.y + (view.frame.size.height - title.size.height) / 2;
    UIImageView* titleView = [[UIImageView alloc] initWithFrame:CGRectMake(startx, starty, title.size.width, title.size.height)];
    titleView.image = title;
    [mEditPannel addSubview:titleView];
    [titleView release];
}
- (void) clickColorCell: (int) first : (int)second
{
    UIColor* c = gColors[first][second];
    mColorPad.backgroundView.backgroundColor = c;
    mDrawView.mCurrentColor = c;
    UserInfo* userInfo = [mViewNav getUserInfo];
    userInfo.signaturecolorindex = [NSNumber numberWithInt:(first * 6 + second)];
    [self dismissColorPadController];
}
- (void) dismissColorPadController
{
    [mColorPadPopupView removeFromSuperview];
    mColorPadPopupView = nil;
    [mColorPadPopupBgView removeFromSuperview];
    mColorPadPopupBgView = nil;
}
- (void) colorPadBgTap: (UITapGestureRecognizer*)ges
{
    NSLog(@"color pad bg tap");
    [mColorPadPopupView removeFromSuperview];
    [mColorPadPopupBgView removeFromSuperview];
    mColorPadPopupView = nil;
    mColorPadPopupBgView = nil;
}
- (void) colorPadHandler: (UITapGestureRecognizer*)sender
{
    NSLog(@"color pad handler");
    mColorPadPopupBgView = [[UIView alloc] initWithFrame:CGRectMake(0, 0, self.frame.size.width, self.frame.size.height)];
    mColorPadPopupBgView.backgroundColor = [UIColor clearColor];
    mColorPadPopupBgView.userInteractionEnabled = YES;
    UITapGestureRecognizer* ges = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(colorPadBgTap:)];
    [mColorPadPopupBgView addGestureRecognizer:ges];
    [ges release];
    [self addSubview:mColorPadPopupBgView];
    [mColorPadPopupBgView release];
    
    float padWidth = 646;
    float padHeight = 317;
    float starty = mSignatureBg.frame.origin.y + mSignatureBg.frame.size.height - padHeight;
    float startx = 0;
    SEColorPad* colorPad = [[SEColorPad alloc] initWithFrame:CGRectMake(startx, starty, padWidth, padHeight) withViewNav:mViewNav];
    colorPad.sigView = self;
    [self addSubview:colorPad];
    [colorPad release];
    mColorPadPopupView = colorPad;
}
- (void) updateColorPad
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    int index = [userInfo.signaturecolorindex intValue];
    UIColor* c = getColorByIndex(index);
    mColorPad.backgroundView.backgroundColor = c;
    mColorPad.backgroundColor = [UIColor clearColor];
    mDrawView.mCurrentColor = c;
}
- (void) lineWidthSliderHandler: (UISlider*) sender
{
    float v = sender.value;
    UserInfo* userInfo = [mViewNav getUserInfo];
    if(v != [userInfo.signaturelinewidth floatValue])
    {
        userInfo.signaturelinewidth = [NSNumber numberWithFloat:v];
        mStokeStyle.mLineWidth = v;
        [mStokeStyle setNeedsDisplay];
        mDrawView.maxLineWidth = v;
    }
}
- (void) createSignatureEditPanel
{
    if(mEditPannel != nil)
        return;
    mEditPannel.userInteractionEnabled = YES;
    mEditPannel = [[[NSBundle mainBundle] loadNibNamed:@"SignatureEditPanel" owner:self options:nil] lastObject];
    mEditPannel.frame = CGRectMake(mEditButton.frame.origin.x, mEditButton.frame.origin.y, mEditPannel.frame.size.width, mEditPannel.frame.size.height);
    mEditPannel.backgroundColor = [UIColor clearColor];
    mLeftBg = (UIImageView*)[mEditPannel viewWithTag:101];
    mRightBg = (UIImageView*)[mEditPannel viewWithTag:102];
    mColorPadBg = (UIImageView*)[mEditPannel viewWithTag:103];
    mColorPad = (SEColorPadCell*)[mEditPannel viewWithTag:104];
    mStokeStyleBg = (UIImageView*)[mEditPannel viewWithTag:105];
    mStokeStyle = (SEStrokeView*)[mEditPannel viewWithTag:106];
    mSave = (SESignatureButton* )[mEditPannel viewWithTag:108];
    mStokeLineWidth = (UISlider*) [mEditPannel viewWithTag:107];
    mClearButton = (SESignatureButton*)[mEditPannel viewWithTag:109];
    mBack = (SESignatureButton*)[mEditPannel viewWithTag:110];
    mLeftTimeLabel = (FontLabel*)[mEditPannel viewWithTag:111];
    UIButton* outputButton = (UIButton*)[mEditPannel viewWithTag:555];
    [outputButton addTarget:self action:@selector(outputSignature:) forControlEvents:UIControlEventTouchUpInside];
    [mLeftTimeLabel setZFont:[[FontManager sharedManager] zFontWithName:[SESystemConfig getFontName] pointSize:30]];
    mLeftTimeLabel.backgroundColor = [UIColor clearColor];
    mLeftTimeLabel.textColor = [UIColor whiteColor];
    mLeftTimeLabel.text = @"0";
    [self addSubview:mEditPannel];
    
    setUISliderBg(mStokeLineWidth, mViewNav);
    mStokeLineWidth.minimumValue = 10;
    mStokeLineWidth.maximumValue = 30;
    UserInfo* userInfo = [mViewNav getUserInfo];
    mStokeLineWidth.value = [userInfo.signaturelinewidth floatValue];
    [mStokeLineWidth addTarget:self action:@selector(lineWidthSliderHandler:) forControlEvents:UIControlEventValueChanged];
    UIImage* image = [mViewNav.mResLoader getImage:@"SignatureEditGroupBg"];
    mLeftBg.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
    
    image = [mViewNav.mResLoader getImage:@"SignatureEditGroupBg"];
    mRightBg.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
    
    
    image = [mViewNav.mResLoader getImage:@"SignatureColorPadBg"];
    image = [SEUtil imageWithCap:image top:0.1 bottom:0.1 left:0.1 right:0.1];
    mColorPadBg.image = image;
    
    //[mColorPad addTarget:self action:@selector(colorPadHandler:) forControlEvents:UIControlEventTouchUpInside];
    image = [mViewNav.mResLoader getImage:@"SignatureColorPadFrame1"];
    mColorPad.foregroundView.image = image;
    mColorPad.userInteractionEnabled = YES;
    UITapGestureRecognizer* ges = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(colorPadHandler:)];
    [mColorPad addGestureRecognizer:ges];
    [ges release];
    
    image = [mViewNav.mResLoader getImage:@"SignatureColorPadBg"];
    image = [SEUtil imageWithCap:image top:0.1 bottom:0.1 left:0.1 right:0.1];
    mStokeStyleBg.image = image;

    mStokeStyle.mLineWidth = mStokeLineWidth.value;
    [mSave setButtonBackground:@"SignatureSaveBg" select:@"SignatureSaveSelectedBg"];
    [mSave setTextImage:@"save" indicateImage:@"SignatureSaveIndicator" alignment:UITextAlignmentRight];
    [mSave setButtonHandler:self action:@selector(saveButtonHandler:)];
    
    [mClearButton setButtonBackground:@"SignatureClearBg" select:@"SignatureClearSelectedBg"];
    [mClearButton setTextImage:@"clear" indicateImage:@"SignatureCleanIndicator" alignment:UITextAlignmentRight];
    [mClearButton setButtonHandler:self action:@selector(clearButtonHandler:)];
    
    [mBack setButtonBackground:@"SignatureBackBg" select:@"SignatureBackSelectedBg"];
    [mBack setTextImage:@"back" indicateImage:@"SignatureBackIndicator" alignment:UITextAlignmentRight];
    [mBack setButtonHandler:self action:@selector(backButtonHandler:)];
    /*
    image = [mViewNav.mResLoader getImage:@"SignatureSaveBg"];
    image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
    [mSave setBackgroundImage:image forState:UIControlStateNormal];
    image = [mViewNav.mResLoader getImage:@"SignatureSaveSelectedBg"];
    image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
    [mSave setBackgroundImage:image forState:UIControlStateHighlighted];
    //[mSave setTitle:@"Save" forState:UIControlStateNormal];
    [mSave addTarget:self action:@selector(saveButtonHandler:) forControlEvents:UIControlEventTouchUpInside];
    [self addTitleForView:mSave title:@"save"];
    */
    /*
    image = [mViewNav.mResLoader getImage:@"SignatureClearBg"];
    image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
    [mClearButton setBackgroundImage:image forState:UIControlStateNormal];
    
    image = [mViewNav.mResLoader getImage:@"SignatureClearSelectedBg"];
    image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
    [mClearButton setBackgroundImage:image forState:UIControlStateHighlighted];
    [mClearButton addTarget:self action:@selector(clearButtonHandler:) forControlEvents:UIControlEventTouchUpInside];
    [self addTitleForView:mClearButton title:@"clear"];
    */
    /*
    image = [mViewNav.mResLoader getImage:@"SignatureBackBg"];
    image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
    [mBack setBackgroundImage:image forState:UIControlStateNormal];
    
    image = [mViewNav.mResLoader getImage:@"SignatureBackSelectedBg"];
    image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
    [mBack setBackgroundImage:image forState:UIControlStateHighlighted];
    [mBack addTarget:self action:@selector(backButtonHandler:) forControlEvents:UIControlEventTouchUpInside];
    [self addTitleForView:mBack title:@"back"];
     */
    [self updateColorPad];
        
}
- (void) animEndHandler: (id) animSelf
{
    NSLog(@"edit press anim end");
    [animSelf release];
    [self createSignatureEditPanel];
    SEGroupViewTranslateAnim* anim = [[SEGroupViewTranslateAnim alloc] init];
    anim.mTranslate = CGPointMake(0, -mAddDeleteBg.frame.size.height);
    [anim addView:mEditPannel];
    /*
    [anim addView:mEditBg];
    [anim addView:mColorPad];
    [anim addView:mStokeLineWidth];
    [anim addView:mSave];
    [anim addView:mClearButton];
    [anim addView:mStrokeImagePreviewBg];
     */
    [anim startAnim:self endAction:@selector(editInterfaceAnimHandler:) time:0.2];
}
- (void) realEditButtonHandler
{
    mDrawView.userInteractionEnabled = YES;
    SEGroupViewTranslateAnim* anim = [[SEGroupViewTranslateAnim alloc] init];
    anim.mTranslate = CGPointMake(0, mAddDeleteBg.frame.size.height);
    [anim addView:mEditButton];
    [anim addView:mAddDeleteBg];
    [anim addView:mAddButton];
    [anim addView:mDeleteButton];
    [anim startAnim:self endAction:@selector(animEndHandler:) time:0.2];
    
}
- (void) editButtonHandler: (id)sender
{
    NSLog(@"edit button pressed ");
    if(mCurrentSeq == -1)
    {
        [mViewNav createMusicImageAttachInputView];
        SEMusicImageListPopupView* popupView = [mViewNav getMusicImageListPopupView];
        popupView.title.text = @"Please input signature name";
        popupView.errorMsg.text = @"There has no signature! Please add one signature firstly";
        [popupView setHandler:self ok:@selector(realAddHandler) cancel:@selector(cancelHandler)];
        NSArray* allSignatures = [mViewNav getAllSignatures];
        NSMutableArray* sigNames = [NSMutableArray array];
        for(int i = 0 ; i < allSignatures.count ; i++)
        {
            Signature* s = [allSignatures objectAtIndex:i];
            [sigNames addObject:s.name];
        }
        NSString* str = [SEUtil createNextName:@"Signature" :sigNames :allSignatures.count + 1];
        NSString* contentText = str;
        popupView.content.text = contentText;
        [mViewNav showMusicImageAttachInputView];
        return;
    }
    else 
    {
        [self realEditButtonHandler];
    }

    
}
- (void) setButtonHandler
{
    [mAddButton setButtonBackground:@"SignatureAddButtonNormal" select:@"SignatureAddButtonSelected"];
    [mAddButton setTextImage:@"add" indicateImage:nil alignment:UITextAlignmentRight];
    [mAddButton setButtonHandler:self action:@selector(addButtonHandler:)];
    [mEditButton setButtonBackground:@"SignatureEditButtonNormal" select:@"SignatureEditButtonSelected"];
    [mEditButton setTextImage:@"edit" indicateImage:nil alignment:UITextAlignmentRight];
    [mEditButton setButtonHandler:self action:@selector(editButtonHandler:)];
    
    [mDeleteButton setButtonBackground:@"SignatureDeleteButtonNormal" select:@"SignatureDeleteButtonSelected"];
    [mDeleteButton setTextImage:@"delete" indicateImage:nil alignment:UITextAlignmentRight];
    [mDeleteButton setButtonHandler:self action:@selector(deleteButtonHandler:)];
    /*
    UIImage* image = [mViewNav.mResLoader getImage:@"SignatureAddButtonNormal"];
    image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
    [mAddButton setBackgroundImage:image forState:UIControlStateNormal];
    image = [mViewNav.mResLoader getImage:@"SignatureAddButtonSelected"];
    image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
    [mAddButton setBackgroundImage:image forState:UIControlStateHighlighted];
    [mAddButton setTitle:@"Add" forState:UIControlStateNormal];
    [mAddButton setTitle:@"Add" forState:UIControlStateSelected];
    //mAddButton.backgroundColor = [UIColor clearColor];
    image = [mViewNav.mResLoader getImage:@"SignatureDeleteButtonNormal"];
    image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
    [mDeleteButton setBackgroundImage:image forState:UIControlStateNormal];
    image = [mViewNav.mResLoader getImage:@"SignatureDeleteButtonSelected"];
    image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
    [mDeleteButton setBackgroundImage:image forState:UIControlStateHighlighted];
    [mDeleteButton setTitle:@"Delete" forState:UIControlStateSelected];
    [mDeleteButton setTitle:@"Delete" forState:UIControlStateNormal];
    
    image = [mViewNav.mResLoader getImage:@"SignatureEditButtonNormal"];
    image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
    [mEditButton setBackgroundImage:image forState:UIControlStateNormal];
    image = [mViewNav.mResLoader getImage:@"SignatureEditButtonSelected"];
    image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
    [mEditButton setBackgroundImage:image forState:UIControlStateHighlighted];
    [mEditButton setTitle:@"Edit" forState:UIControlStateNormal];
    [mEditButton setTitle:@"Edit" forState:UIControlStateSelected];
    
    [mAddButton addTarget:self action:@selector(addButtonHandler:) forControlEvents:UIControlEventTouchUpInside];
    [mDeleteButton addTarget:self  action:@selector(deleteButtonHandler:) forControlEvents:UIControlEventTouchUpInside];
    [mEditButton addTarget:self action:@selector(editButtonHandler:) forControlEvents:UIControlEventTouchUpInside];
     */
}
/*
- (SignaturePointData) getSignaturePointsWithSeqName:(NSString*)name
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    NSSet* signatures = userInfo.signaturelist;
    Signature* sig = nil;
    NSEnumerator* it = [signatures objectEnumerator];
    int inputSeq = [name intValue];
    SignaturePointData sd;
    sd.points = [NSMutableArray array];
    sd.lineWidth = 0;
    while((sig = [it nextObject]) != nil)
    {
        if([sig.seq isEqualToNumber:[NSNumber numberWithInt:inputSeq]])
        {
            return [self getSignaturePoints:sig.seq];
        }
    }
    return sd;
}
- (SignaturePointData) getSignaturePoints:(NSNumber*)seq
{
    Signature* sig = [self getSignature:seq];
    NSData* data = sig.data;
    SignaturePointData sd;
    sd.lineWidth = 0;
    sd.points = [NSMutableArray array];
    if(data)
    {
        NSMutableArray* pointsArray = [NSMutableArray array];
        NSInputStream* input = [NSInputStream inputStreamWithData:data];
        [input open];
        int num;
        int ret = [input read:(uint8_t*)&num maxLength:sizeof(int)];
        assert(ret != -1);
        CGFloat lineWidth;
        ret = [input read: (uint8_t*)&lineWidth maxLength:sizeof(CGFloat)];
        assert(ret != -1);
        //num = convertInt(num);
        //lineWidth = (CGFloat)convertInt((int)lineWidth);
        for(int i = 0 ; i  < num ; i++)
        {
            int pointNum;
            ret = [input read:(uint8_t *)&pointNum maxLength:sizeof(int)];
            assert(ret != -1);
            //pointNum = convertInt(pointNum);
            NSMutableArray* a = [NSMutableArray array];
            for(int j = 0 ; j < pointNum ; j++)
            {
                CGFloat x, y;
                ret = [input read:(uint8_t *)&x maxLength:sizeof(CGFloat)];
                assert(ret != -1);
                ret = [input read:(uint8_t *)&y maxLength:sizeof(CGFloat)];
                assert(ret != -1);
                //x = (CGFloat)convertInt((int)x);
                //y = (CGFloat)convertInt((int)y);
                [a addObject:[NSValue valueWithCGPoint:CGPointMake(x, y)]];
            }
            [pointsArray addObject:a];
        }
        [input close];
        sd.points = pointsArray;
        sd.lineWidth = lineWidth;
        return sd;
    }
    else
        return sd;
    
}
*/
- (void) initSignature
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    NSNumber* currentseq = userInfo.currentsignature;
    mCurrentSeq = [currentseq intValue];
    mDrawView.maxLineWidth = [userInfo.signaturelinewidth floatValue];
    [self setSignature:currentseq];
    if(mCurrentSeq == -1)
    {
        mDrawingTotalTime = 10 * 1000;
    }
    else
    {
        Signature* sig = [mViewNav getSignature:currentseq];
        mDrawingTotalTime = [sig.totaltime intValue] * 1000;
    }
    mDrawingConsumeTime = 0;
}

- (void) alertView:(UIAlertView *)alertView clickedButtonAtIndex:(NSInteger)buttonIndex
{
    /*
    if(mAlertType == FOR_ADD)
    {
        if(buttonIndex == 0)
        {
            [self savePoints:[NSNumber numberWithInt:mCurrentSeq]];
        }
        [self popupSignatureSave];
    }
    else if(mAlertType == FOR_CHANGE)
    {
        if(buttonIndex == 0)
        {
            [self savePoints:[NSNumber numberWithInt:mCurrentSeq]];
        }
        HitProperty hitP = [mScrollView hitRect:mCurrentPoint];
        //hitP.imageView.image = mSelectedSignatureImage;
        //mCurrentSelectedSignatureImageView.image = mNormalSignatureImage;
        //mCurrentSelectedSignatureImageView = hitP.imageView;
        int prevIndex = mCurrentSeq;
        mCurrentSeq = hitP.index;
        [self setSignature: [NSNumber numberWithInt:mCurrentSeq]];
        [mScrollView updateImage:prevIndex];
        [mScrollView updateImage:mCurrentSeq];
        
        
        //[mScrollView relayout];
    }
     */
}

@end
@implementation SESignatureView
@synthesize mSignatureBg;
@synthesize mBg;
@synthesize mSignatureTitleBg;
@synthesize mAddButton;
@synthesize mDeleteButton;
@synthesize mAddDeleteBg;
@synthesize mDrawView;
@synthesize mScrollViewBg;
@synthesize mEditButton;
@synthesize mScrollView;
@synthesize mViewNav;
@synthesize mSigViewController;
@synthesize mResLoader;
@synthesize mCurrentSeq;
@synthesize mScrollViewNormalItemImage;
@synthesize mScrollViewSelectedItemImage;
- (void) dealloc
{
    [mScrollViewNormalItemImage release];
    [mScrollViewSelectedItemImage release];
    [mSigViewController release];
    [super dealloc];
}

- (void) tapHandler:(UITapGestureRecognizer*)ges
{
}
- (void) setImage: (UIImageView*)imageView name: (NSString*) name
{
    UIImage* image = [mViewNav.mResLoader getImage:name];
    imageView.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
}
- (void) drawViewTimerUpdateHandler
{
    /*
    int frame = [mDrawView getCurrentFrame];
    NSLog(@"frame=  %d", frame);
    if(frame % 30 == 0)
    {
        NSString* str = [NSString stringWithFormat:@"%f", frame * 0.03];
        mLeftTimeLabel.text = str;
    }
     */
    /*
    mDrawingConsumeTime += 30;
    if(mDrawingConsumeTime > mDrawingTotalTime)
    {
        mLeftTimeLabel.text = @"0";
    }
    else 
    {
        int leftTime = (mDrawingTotalTime - mDrawingConsumeTime) / 1000;
        mLeftTimeLabel.text = [NSString stringWithFormat:@"%d", leftTime];
    }
     */
    /*
    mDrawingTotalTime -= 30;
    if(mDrawingTotalTime < 0)
    {
        mDrawingTotalTime = 0;
    }
    int leftTime = mDrawingTotalTime / 1000;
    mLeftTimeLabel.text = [NSString stringWithFormat:@"%d", leftTime];
    if(mCurrentSeq != -1)
    {
        Signature* sig = [mViewNav getSignature:[NSNumber numberWithInt:mCurrentSeq]];
    }
     */
    /*
    if(mCurrentSeq != -1)
    {
        Signature* sig = [mViewNav getSignature:[NSNumber numberWithInt:mCurrentSeq]];
        mDrawingTotalTime = [sig.totaltime intValue] * 1000;
    }
     */
    NSLog(@"## mDrawingTotalTime = %d ##", mDrawingTotalTime);
    float dist = [mDrawView getPointsLength];
    float totalDist =  TOTAL_SIG_LEN;
    if(dist < totalDist)
    {
        float left = totalDist - dist;
        float v = left / totalDist;
        [mInkProgressView setPercent:v];
        mLeftTimeLabel.text = [NSString stringWithFormat:@"%f", left];
    }
    else 
    {
        mLeftTimeLabel.text = [NSString stringWithFormat:@"%f", 0];
        [mInkProgressView setPercent:0];
    }
    /*
    if(mDrawingTotalTime <= 0)
        return;
    mDrawingTotalTime -= 30;
    if(mDrawingTotalTime < 0)
    {
        mDrawingTotalTime = 0;
    }
    int leftTime = mDrawingTotalTime / 1000;
    mLeftTimeLabel.text = [NSString stringWithFormat:@"%d", leftTime];
     */
    /*
    if(mCurrentSeq != -1)
    {
        Signature* sig = [mViewNav getSignature:[NSNumber numberWithInt:mCurrentSeq]];
        sig.totaltime = [NSNumber numberWithInt:mDrawingTotalTime / 1000];
    }
     */
}
- (void) drawViewTouchesBeganHandler
{
    /*
    int leftTime = (mDrawingTotalTime - mDrawingConsumeTime) / 1000;
    mLeftTimeLabel.text = [NSString stringWithFormat:@"%d", leftTime];
    struct timeval t;
    gettimeofday(&t, NULL);
    mTouchStartTime = t.tv_sec * 1000 + t.tv_usec / 1000;
     */
    int leftTime = (mDrawingTotalTime - mDrawingConsumeTime) / 1000;
    mLeftTimeLabel.text = [NSString stringWithFormat:@"%d", leftTime];
}
- (void) drawViewTouchesMoveHandler
{
    /*
    struct timeval t;
    gettimeofday(&t, NULL);
    int endTime = t.tv_sec * 1000 + t.tv_usec / 1000;
    int interval = endTime - mTouchStartTime;
    mDrawingConsumeTime += interval;
    */
}
- (void) drawViewTouchesEndHandler
{
    /*
    struct timeval t;
    gettimeofday(&t, NULL);
    int endTime = t.tv_sec * 1000 + t.tv_usec / 1000;
    */
}
- (NSNumber*) canConsumePoint
{
    /*
    if(mCurrentSeq != -1)
    {
        Signature* sig = [mViewNav getSignature:[NSNumber numberWithInt:mCurrentSeq]];
        mDrawingTotalTime = [sig.totaltime intValue] * 1000;
    }
     */
    NSLog(@"## canConsumePoint mDrawingTotalTime = %d ###\n", mDrawingTotalTime);
    float dist = [mDrawView getPointsLength];
    float totalDist = TOTAL_SIG_LEN;
    if(dist >= totalDist)
    {
        return [NSNumber numberWithBool: NO];
    }
    else {
        return [NSNumber numberWithBool: YES];
    }
    /*
    if(mDrawingTotalTime <= 0)
    {
        return [NSNumber numberWithBool: NO];
    }
    else 
    {
        return [NSNumber numberWithBool: YES];
    }
     */
}
- (void) initData
{
    self.userInteractionEnabled = YES;
    [self addSubview:mBg];
    [self addSubview:mSignatureTitleBg];
    [self addSubview:mSignatureBg];
    //UIImageView* drawViewBg = [[UIImageView alloc] initWithFrame:mDrawView.frame];
    //UIImage* drawViewImage = [mViewNav.mResLoader getImage:@"SignatureDrawBg"];
    //drawViewBg.image = [SEUtil imageWithCap:drawViewImage top:0.1 bottom:0.1 left:0.1 right:0.1];
    //[self addSubview:drawViewBg];
    //[drawViewBg release];
    [self addSubview:mDrawView];
    [self addSubview:mEditButton];
    [self addSubview:mAddDeleteBg];
    [self addSubview:mAddButton];
    [self addSubview:mDeleteButton];
    [self addSubview:mScrollViewBg];
    [self addSubview:mScrollView];
    [self setImage:mBg name:@"SignatureAllBg"];
    [self setImage:mSignatureBg name:@"SignatureBg"];
    [self setImage:mSignatureTitleBg name:@"SignatureTitleBg"];
    [self setImage:mScrollViewBg name:@"SignatureScrollViewBg"];
    [self setImage:mAddDeleteBg name:@"SignatureAddDeleteBg"];
    if(gColors[0][0] == nil)
    {
        loadColors();
    }
    mEditButtonRect = mEditButton.frame;
    mAddDeleteBgRect = mAddDeleteBg.frame;
    
    [mDrawView initData];
    [mDrawView setTimerUpdateHandler:self action:@selector(drawViewTimerUpdateHandler)];
    [mDrawView setTouchBeganHandler:self action:@selector(drawViewTouchesBeganHandler)];
    [mDrawView setTouchMoveHandler:self action:@selector(drawViewTouchesMoveHandler)];
    [mDrawView setTouchEndHandler:self action:@selector(drawViewTouchesEndHandler)];
    [mDrawView setHandlerForConsumePoint:self action:@selector(canConsumePoint)];
    [mDrawView setIsDrawSamplePoint:YES];
    mDrawView.userInteractionEnabled = NO;
    //mDrawView.backgroundColor = [UIColor clearColor];
    UIImage* drawViewImage = [mViewNav.mResLoader getImage:@"SignatureDrawBg"];
    mDrawView.background = [SEUtil imageWithCap:drawViewImage top:0.1 bottom:0.1 left:0.1 right:0.1]; 
    self.userInteractionEnabled = YES;
    UIImage* image = [mViewNav.mResLoader getImage:@"SignatureScrollViewItemNormalBg"];
    self.mScrollViewNormalItemImage = image;//[SEUtil imageWithCap:image top:0.1 bottom:0.1 left:0.1 right:0.1];
    image = [mViewNav.mResLoader getImage:@"SignatureScrollViewItemSelectedBg"];
    //image = [image resizableImageWithCapInsets:UIEdgeInsetsMake(0.1 * image.size.height, 0.1 * image.size.width, 0.1*image.size.height, 0.1 * image.size.width)];
    //image = [SEUtil imageWithCap:image top:0.1 bottom:0.1 left:0.1 right:0.1];
    self.mScrollViewSelectedItemImage = image;//[SEUtil imageWithCap:image top:0.1 bottom:0.1 left:0.1 right:0.1];
    mLineWidthRatio = mScrollView.frame.size.width / mDrawView.frame.size.width;
    [self setButtonHandler];
    [self initSignature];
    [self initScrollView];
    mCurrentMode = VIEW_MODE;
    mDrawingTotalTime = 10 * 1000;
    //mSignatureTime = 20;
    
    
    mInkProgressView = [[SEUIProgressView alloc] initWithFrame:mSignatureTitleBg.frame];
    [self addSubview: mInkProgressView];
    [mInkProgressView release];
    mInkProgressView.mForegroundImageStr = @"InkProgressForegroundImage";
    [mInkProgressView initData:[PhotoFrameAppDelegate getViewNavigator]];
    
    float dist = [mDrawView getPointsLength ];
    float v = (TOTAL_SIG_LEN - dist) / TOTAL_SIG_LEN;
    [mInkProgressView setPercent:v];
    
    UIImage* inkImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"InkImage"];
    CGSize inkSize = CGSizeMake(inkImage.size.width / 2, inkImage.size.height / 2);
    mInkImageView = [[UIImageView alloc] initWithFrame:CGRectMake(mSignatureTitleBg.frame.origin.x, mSignatureTitleBg.frame.origin.y, inkSize.width, inkSize.height)];
    mInkImageView.image = inkImage;
    [self addSubview:mInkImageView];
    [mInkImageView release];
}
- (void) saveContext:(UserInfo*) userInfo
{
    
}
+ (NSArray*) getFeedChar
{
    NSArray* ret = [NSArray arrayWithObjects:[NSNumber numberWithUnsignedChar:'S'], [NSNumber numberWithUnsignedChar:'u'], [NSNumber numberWithUnsignedChar:'N'], nil];
    return ret;
}
@end
