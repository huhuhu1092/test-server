//
//  SEOptionsView.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-2-22.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEOptionsView.h"
#import "SEUtil.h"
#import "SEResDefine.h"
#import "PainterManager.h"
#import "SEViewNavigator.h"
#import "UserInfo.h"
#import "SEDataUploadManager.h"
#import "ppmtool.h"
#import "SEImageShareManager.h"
#import "FontLabel.h"
#import "SEMusicImageListPopup.h"
//#import "FontManager.h"
#import "SEUIProgressView.h"
#import "SEUISwitch.h"
#import "SEUserUpgrate.h"
#import "Signature.h"
#import "SEDrawTouchView.h"
#import "SEInAppPurchaseManager.h"
#import "PhotoFrameAppDelegate.h"
#import "SEUserDefaultManager.h"
#import "SESystemConfig.h"
#import "SEPopupViewWidgets.h"
#import "SEConfigFileParser.h"
#import "SESystemDataManager.h"
#import <QuartzCore/QuartzCore.h>
#define LEFT_PADDING 10
#define TOP_PADDING 10
#define LEFT_BAR_WIDTH 300
#define LEFT_BAR_HEIGHT 50
#define LEFT_BAR_VMARGIN 20
#define LEFT_RIGTH_MARGIN 100 
#define PROGRESS_BAR_WIDTH 320
#define PROGRESS_BAR_HEIGHT 42
////////////////////////
#define OPTIONS_LABEL_FONT_NAME @"EyadBold"
#define OPTIONS_LABEL_FONT_SIZE 30
///////////////////////////////////
#define OPTIONS_STORE_ITEM_CONTENT_HEIGHT 66
#define OPTIONS_STORE_ITEM_CONTENT_WIDTH 103

#define OPTIONS_STORE_ITEM_LABEL_HEIGHT 50
#define OPTIONS_STORE_ITEM_LABEL_WIDTH  290

#define OPTIONS_STORE_ITEM_LABEL1_WIDTH 71
#define OPTIONS_STORE_ITEM_LABEL1_HEIGHT 50

#define OPTIONS_STORE_ITEM_LABEL2_WIDTH 195
#define OPTIONS_STORE_ITEM_LABEL2_HEIGHT 50

#define OPTIONS_STORE_ITEM_BUTTON_WIDTH 155
#define OPTIONS_STORE_ITEM_BUTTON_HEIGHT  50

#define OPTIONS_LOCK_WIDTH 50
#define OPTIONS_LOCK_HEIGHT 100
#define LABEL_COLOR 
#define VIEW_COLOR
#define SELECTED_SCROLLVIEW_PADDINGY 0
#define BRUSH_CHILD_VIEW_START_TAG 655350
#define SEMAX_CONNECTION_TIME 60
static UIColor* getLabelColor()
{
    float v = 215;
    UIColor* labelColor = [UIColor colorWithRed:v / 255 green:v / 255 blue:v / 255 alpha:1];
    return labelColor;
}
static UIColor* getViewColor()
{
    UIColor* labelColor = [UIColor colorWithRed:54.0/255 green:54.0/255 blue:54.0/255 alpha:1];
    return labelColor;
}
static NSString* gAchievementMedalDescription[] = {
    @"",
    @"BRONZE AWARD FOR PERFECT ATTENDANCE",
    @"SILVER AWARD FOR PERFECT ATTENDANCE",
    @"GOLD AWARD FOR PERFECT ATTENDANCE",
    
    @"",
    @"BRONZE AWARD FOR ROOKIE",
    @"SILVER AWARD FOR ROOKIE",
    @"GOLD AWARD FOR ROOKIE",
    
    @"",
    @"BRONZE AWARD FOR PAINTING",
    @"SILVER AWARD FOR PAINTING",
    @"GOLD AWARD FOR PAINTING",
    
    @"",
    @"BRONZE AWARD FOR HAPPY SHARING",
    @"SILVER AWARD FOR HAPPY SHARING",
    @"GOLD AWARD FOR HAPPY SHARING",
    
    @"",
    @"BRONZE AWARD FOR LOYAL USER",
    @"SILVER AWARD FOR LOYAL USER",
    @"GOLD AWARD FOR LOYAL USER"
};
static NSString* gAchievementDefineString[] = 
{
    @"",
    @"If you use the application for two consecutive days,"
    "you can get one perfect attendance point. If you do not use it continuously, one perfect "
    "attendance point will be deducted every other day. Perfect attendance points are not deducted if you have won a perfect attendance award.",
    @"If you use the application for two consecutive days, you can get one perfect attendance point. If you do not use it continuously, one perfect attendance point will be deducted every other day. Perfect attendance points are not deducted if you have won a perfect attendance award.",
    @"If you use the application for two consecutive days, you can get one perfect attendance point. If you do not use it continuously, one perfect attendance point will be deducted every other day. Perfect attendance points are not deducted if you have won a perfect attendance award.",
    @"",
    @"You are rewarded if you have completed 200 paintings within one day. If you fail to do so within one day, points will be cleared (according to the date).",
    @"You are rewarded if you have completed 400 paintings within one day. If you fail to do so within one day, points will be cleared (according to the date).",
    @"You are rewarded if you have completed 1200 paintings within one day. If you fail to do so within one day, points will be cleared (according to the date).",
    @"",
    @"Accumulative 1000 paintings (1 point/image)",
    @"Accumulative 3000 paintings (1 point/image)",
    @"Accumulative 6000 paintings (1 point/image)",
    @"",
    @"Accumulative 100 sharing times (1 points/times)",
    @"Accumulative 500 sharing times (1 points/times)",
    @"Accumulative 1500 sharing times (1 points/times)",
    @"",
    @"You have accumulatively drawn 300 paintings (1 point/image), shared 100 times (1 points/times), played 500 songs (1 points/song), and buy 1 product (100 points/times).",
    @"You have accumulatively drawn 2000 paintings (1 point/image), shared 250 times (1 points/times), played 1500 songs (1 points/song), and buy 2 products (100 points/times).",
    @"You have accumulatively drawn 4200 paintings (1 point/image), shared 700 times (1 points/times), played 2500 songs (1 points/song), and buy 3 products (100 points/times).",
};
static NSString* gAchieveBackground[] = {@"PresentOnDutyBackground", @"NewPersonBackground",@"DrawingBackground",@"ShareBackground",@"FansBackground"};
//////////////////////////
struct UserMedalData
{
    NSString* imageNameNoMedal;
    NSString* imageNameHasMedal;
};
UserMedalData gUserMedalData[] = {
    {@"UserUpgradePresentOnDutyNoCopper",@"UserUpgradePresentOnDutyCopper"},
    {@"UserUpgradePresentOnDutyNoSilver", @"UserUpgradePresentOnDutySilver"},
    {@"UserUpgradePresentOnDutyNoGold", @"UserUpgradePresentOnDutyGold"},
    {@"UserUpgradeNewPersonNoCopper", @"UserUpgradeNewPersonCopper"},
    {@"UserUpgradeNewPersonNoSilver", @"UserUpgradeNewPersonSilver"},
    {@"UserUpgradeNewPersonNoGold", @"UserUpgradeNewPersonGold"},
    {@"UserUpgradeDrawingNoCopper", @"UserUpgradeDrawingCopper"},
    {@"UserUpgradeDrawingNoSilver", @"UserUpgradeDrawingSilver"},
    {@"UserUpgradeDrawingNoGold", @"UserUpgradeDrawingGold"},
    {@"UserUpgradeShareNoCopper", @"UserUpgradeShareCopper"}, 
    {@"UserUpgradeShareNoSilver", @"UserUpgradeShareSilver"},
    {@"UserUpgradeShareNoGold", @"UserUpgradeShareGold"},
    {@"UserUpgradeFansNoCopper", @"UserUpgradeFansCopper"},
    {@"UserUpgradeFansNoSilver", @"UserUpgradeFansSilver"},
    {@"UserUpgradeFansNoGold", @"UserUpgradeFansGold"}
};
////////////////////////
struct OptionsBarViewData
{
    NSString* barLabel;
    NSString* viewNibName;
    NSString* className;
};
OptionsBarViewData gOptionsBarViewData[] = {
    {@"drawing setting", @"OptionsDrawingSetting", @"SEOptionsMainSettingController"}, 
    {@"user info", nil, @"SEOptionsUserUpgradeInfoController"},
    {@"issue report", @"UserBugReporter", @"SEIssueReportController"}
};
NSString* gOptionsViewControllerClassName[] = {
    @"SEOptionsPlaySetting",
    @"SEOptionsWidgetSetting",
    @"SEOptionsUserInfoSetting",
    @"SEOptionsSignatureSetting",
    @"SEOptionsAboutSetting"
};
NSString* gPlayModeOK[] = {
    @"OptionsPlaySettingPlayModeSequenceOK",
    @"OptionsPlaySettingPlayModeRandomOK",
    @"OptionsPlaySettingPlayModeIntelOK"
};
NSString* gPlayModeNoOK[] = {
    @"OptionsPlaySettingPlayModeSequenceNoOK",
    @"OptionsPlaySettingPlayModeRandomNoOK",
    @"OptionsPlaySettingPlayModeIntelNoOK"    
};
struct LeftLabelData
{
    NSString* str;
    NSString* imageName;
};
LeftLabelData gLeftLabelText[] = {
    {@"PLAY", @"play_b.png"},{ @"WIGET", @"widget_b.png"}, {@"USER INFO", @"userinfo_b.png"},  {@"APP STORE", @"store.png"}, {@"ABOUT", @"about_b.png"}, {@"HELP", @"help.png"}
};
///////
/*
NSString* brushArray[] = {@"paintingbrush_001.pgm", @"paintingbrush_002.pgm", @"paintingbrush_003.pgm", @"paintingbrush_004.pgm"};
 */
//////
static void setUISliderBg(UISlider* slider, SEViewNavigator* viewNav)
{
    UIImage* sliderThumb = [viewNav.mResLoader getImage:@"SliderBarThumbImage"];
    [slider setThumbImage:sliderThumb forState:UIControlStateNormal];
    [slider setThumbImage:sliderThumb forState:UIControlStateHighlighted];
    UIImage* sliderMinImage =[viewNav.mResLoader getImage:@"SliderMinTrackImage"];
    sliderMinImage = [SEUtil imageStretchForSlider:sliderMinImage];
    //sliderMinImage = [sliderMinImage resizableImageWithCapInsets:UIEdgeInsetsMake(0, 150, 0, 150)];
    //[sliderMinImage stretchableImageWithLeftCapWidth:7 topCapHeight:0];
    UIImage* sliderMaxImage = [viewNav.mResLoader getImage:@"SliderMaxTrackImage"];
    sliderMaxImage = [SEUtil imageStretchForSlider:sliderMaxImage];
    //sliderMaxImage = [sliderMaxImage resizableImageWithCapInsets:UIEdgeInsetsMake(0, 150, 0, 150)];
    [slider setMaximumTrackImage:sliderMinImage forState:UIControlStateNormal];
    [slider setMinimumTrackImage:sliderMaxImage forState:UIControlStateNormal];  
}

static void setLabelFont(FontLabel* fontLabel, NSString* text, UIColor* color ,NSString* fontName, CGFloat fontSize)
{
    [fontLabel setZFont:[[FontManager sharedManager] zFontWithName:fontName pointSize:fontSize]];
    fontLabel.text = text;
    fontLabel.backgroundColor = [UIColor clearColor];
    fontLabel.textColor = color;
}
static UIImage* getLockImage(LOCK_TYPE lockType)
{
    UIImage* image = nil;
    switch (lockType)
    {
        case LOCK_BUY:
        {
            image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"OptionsLockBuy"];
        }
            break;
        case LOCK_ACHIEVE:
        {
            image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"OptionsLockArchieve"];
        }
            break;
        case LOCK_LEVELUP:
        {
            image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"OptionsLockLevelUp"];
        }
            break;
        default:
            break;
    }
    return image;
}
////////
@interface SEOptionsRadioButton ()
- (int) getIndexByValue: (int)value;
- (int) getIndexByView: (UIView*)view;
@property (nonatomic, retain) NSNumber* mCurrentValue;
@end

@implementation SEOptionsRadioButton
- (void) setMCurrentValue:(NSNumber *)v
{
    [mCurrentValue release];
    mCurrentValue = [v retain];
}
- (NSNumber*) mCurrentValue
{
    return mCurrentValue;
}
- (int) getIndexByView: (UIView*)view
{
    for(int i = 0 ; i < mButtonView.count ; i++)
    {
        UIView* tmp = [mButtonView objectAtIndex:i];
        if(tmp == view)
            return i; 
    }
    return 0;
}
- (void) dealloc
{
    [mCurrentValue release];
    [mButtonView release];
    [mButtonViewValue release];
    [mNormalResStr release];
    [mSelectedResStr release];
    [super dealloc];
}
- (int) getIndexByValue: (int)value
{
    for(int i = 0 ; i < mButtonViewValue.count ; i++)
    {
        NSNumber* n = [mButtonViewValue objectAtIndex:i];
        if([n intValue] == value)
            return i;
    }
    return 0;
}
- (void) handleImageTap: (UITapGestureRecognizer*)tap
{
    SEOptionsImageView* tapView = (SEOptionsImageView*)tap.view;
    int tapViewIndex = [self getIndexByView:tapView];
    int currentIndex = [self getIndexByValue:[mCurrentValue intValue]];
    if(currentIndex == tapViewIndex)
        return;
    SEOptionsImageView* currentView = [mButtonView objectAtIndex:currentIndex];
    UIImage* normalImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:mNormalResStr];
    UIImage* selectedImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:mSelectedResStr];
    currentView.imageView.image = normalImage;
    tapView.imageView.image = selectedImage;
    self.mCurrentValue = [mButtonViewValue objectAtIndex:tapViewIndex];
    [tapView setNeedsDisplay];
    [currentView setNeedsDisplay];
    [mValueChangeHandler performSelector:mValueChangeAction withObject:mCurrentValue];
}
- (id) initWithViews:  (NSArray*) views values: (NSArray*)values normal: (NSString*)normalStr selected: (NSString*)selectedStr currentValue: (int) value
{
    self = [super init];
    if(self)
    {
        mButtonView = [[NSMutableArray array] retain];
        mButtonViewValue = [[NSMutableArray array] retain];
        assert(values.count == views.count);
        mNormalResStr = [normalStr retain];
        mSelectedResStr = [selectedStr retain];
        UIImage* normalImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:mNormalResStr];
        UIImage* selectedImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:mSelectedResStr];
        for(SEOptionsImageView* v in views)
        {
            [mButtonView addObject:v];
            UITapGestureRecognizer* tap = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(handleImageTap:)];
            v.userInteractionEnabled = YES;
            [v addGestureRecognizer:tap];
            [tap release];
            v.imageView.image = normalImage;
        }
        for(NSNumber* n in values)
        {
            [mButtonViewValue addObject:n];
        }
        mCurrentValue = [NSNumber numberWithInt:value];
        
        SEOptionsImageView* currentImage = [mButtonView objectAtIndex:[self getIndexByValue:value]];
        currentImage.imageView.image = selectedImage;
    }
    return self;
}

+ (id) createRadioButton: (NSArray*) views values: (NSArray*)values normal: (NSString*)normalStr selected: (NSString*)selectedStr currentValue: (int) value
{
    SEOptionsRadioButton* radioButton = [[SEOptionsRadioButton alloc] initWithViews:views values:values normal:normalStr selected:selectedStr currentValue:value];
    return [radioButton autorelease];
}

- (void) setCurrentValue: (int)value
{
    int currentValue = [mCurrentValue intValue];
    if(currentValue == value)
        return;
    int index = [self getIndexByValue:[mCurrentValue intValue]];
    SEOptionsImageView* currentView = [mButtonView objectAtIndex:index];
    UIImage* normalImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:mNormalResStr];
    currentView.imageView.image = normalImage;
    [currentView setNeedsDisplay];
    index = [self getIndexByValue:value];
    currentView = [mButtonView objectAtIndex:index];
    UIImage* selectedImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:mSelectedResStr];
    currentView.imageView.image = selectedImage;
    self.mCurrentValue = [NSNumber numberWithInt:value];
    [currentView setNeedsDisplay];
    [mValueChangeHandler performSelector:mValueChangeAction withObject:mCurrentValue];
    
}
- (void) setValueChangeHandler: (id)handler action: (SEL)action
{
    mValueChangeAction = action;
    mValueChangeHandler = handler;
}

@end
//////////
@implementation SEOptionsLockView
- (void) createChild: (CGRect)frame : (CGSize)size
{
    lockbg = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, frame.size.width, frame.size.height)];
    [self addSubview:lockbg];
    [lockbg release];
    UIImage* image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"OptionsLockBg"];
    image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
    lockbg.image = image;
    image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"OptionsLockBuy"];
    float width = 0;
    float height = OPTIONS_LOCK_HEIGHT;
    if(size.height > 0)
    {
        height = size.height;
    }
    width = image.size.width * height / image.size.height;
    float startx = (frame.size.width - width) / 2;
    if(size.width > 0)
    {
        startx = size.width;
    }
    lock = [[UIImageView alloc] initWithFrame:CGRectMake(startx, (frame.size.height - height) / 2, width, height)];
    [self addSubview:lock];
    [lock release];
}
- (id) initWithFrame: (CGRect) rect lockSize: (CGSize) size
{
    self = [super initWithFrame:rect];
    if(self)
    {
        [self createChild:rect:size];
    }
    return self;
}
- (id) initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if(self)
    {
        [self createChild:frame: CGSizeMake(0, 0)];
    }
    return self;
}
- (void) setText: (NSString*) text
{
    if(textLabel == nil)
    {
        textLabel = [[FontLabel alloc] initWithFrame:CGRectMake(lock.frame.size.width + 30, lock.frame.origin.y + 30, 400, lock.frame.size.height) zFont:[[FontManager sharedManager] zFontWithName:[SESystemConfig getFontName] pointSize:20]];
        [self addSubview:textLabel];
        [textLabel release];
        textLabel.backgroundColor = [UIColor clearColor];
        textLabel.textColor = [UIColor colorWithRed:150.0/255 green:150.0/255 blue:150.0/255 alpha:1];
    }
    textLabel.text = text;
    
}
- (void) setLockStartPoint: (CGPoint)p
{
    lock.frame = CGRectMake(p.x, (self.frame.size.height - lock.frame.size.height) / 2, lock.frame.size.width, lock.frame.size.height);
}
- (void) setText:(NSString *)text lines: (int)lineNum
{
    if(textLabel == nil)
    {
        textLabel = [[FontLabel alloc] initWithFrame:CGRectMake(lock.frame.size.width + 0, 0,self.frame.size.width - lock.frame.size.width , self.frame.size.height) zFont:[[FontManager sharedManager] zFontWithName:[SESystemConfig getFontName] pointSize:20]];
        [self addSubview:textLabel];
        [textLabel release];
        textLabel.numberOfLines = lineNum;
        textLabel.backgroundColor = [UIColor clearColor];
        textLabel.textColor = [UIColor colorWithRed:150.0/255 green:150.0/255 blue:150.0/255 alpha:1];
    }
    textLabel.text = text;
    

}
- (void) setLock: (BOOL) b type: (LOCK_TYPE) lockType
{
    if(b == NO)
    {
        self.hidden = YES;
    }
    else
    {
        self.hidden = NO;
        UIImage* image = getLockImage(lockType);
        if(image)
            lock.image = image;
        /*
        switch (lockType) {
            case LOCK_BUY:
            {
                image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"OptionsLockBuy"];
                lock.image = image;
            }
                break;
            case LOCK_ACHIEVE:
            {
                image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"OptionsLockArchieve"];
                
            }
                break;
            case LOCK_LEVELUP:
            {
                image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"OptionsLockLevelUp"];
                lock.image = image;
            }
                break;
            default:
                break;
        }
         */
    }
}

@end

///////////
@implementation SEOptionsButton

@synthesize buttonText;
@synthesize button;
- (void) createChild:(CGRect)frame
{
    button = [UIButton buttonWithType:UIButtonTypeCustom];
    button.frame = CGRectMake(0, 0, frame.size.width, frame.size.height);
    UIImage* normalImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"AppStoreButtonNormalBg"];
    normalImage = [SEUtil imageWithCap:normalImage top:0.1 bottom:0.9 left:0.1 right:0.9];
    //UIImage* selectImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"AppStoreButtonSelectBg"];
    UIImage* selectImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"OptionsStoreItemSelectedBg"];
    selectImage = [SEUtil imageWithCap:selectImage top:0.1 bottom:0.9 left:0.1 right:0.9];
    [button setBackgroundImage:normalImage forState:UIControlStateNormal];
    [button setBackgroundImage:selectImage forState:UIControlStateHighlighted];
    [self addSubview:button];
    
    buttonText = [[FontLabel alloc] initWithFrame:CGRectMake(0, 0, frame.size.width, frame.size.height) fontName:OPTIONS_LABEL_FONT_NAME pointSize:OPTIONS_LABEL_FONT_SIZE];
    [self addSubview:buttonText];
    [buttonText release];
    buttonText.textAlignment = UITextAlignmentCenter;
    self.backgroundColor = [UIColor clearColor];
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
    if(self)
    {
        CGRect frame = self.frame;
        [self createChild:frame];
    }
    return self;
}

@end
///////
#define STOREITEM_TOUCH_STATE_BEGIN 0
#define STOREITEM_TOUCH_STATE_END 1
#define STOREITEM_TOUCH_STATE_MOVED 2
@interface SEStoreItemParam : NSObject
{
    int state;
    SEOptionsStoreItem* item;
}
@property (nonatomic, assign) int state;
@property (nonatomic, assign) SEOptionsStoreItem* item;
@end
@implementation SEStoreItemParam
@synthesize state;
@synthesize item;

@end
////
@interface SEDrawLineView : UIView

@end
@implementation SEDrawLineView

- (void) drawRect:(CGRect)rect
{
    CGContextRef context = UIGraphicsGetCurrentContext();
    CGContextSaveGState(context);
    CGContextSetShouldAntialias(context, YES);
    float lineWidth = 4;
    float startx = rect.origin.x + 35;
    float starty = rect.origin.y + 5;
    float endx = rect.origin.x + rect.size.width - 35;
    float endy = rect.origin.y + rect.size.height - 5;
    CGContextSetLineWidth(context, lineWidth);
    CGContextSetRGBStrokeColor(context, 115.0/255, 115.0/255, 115.0/255, 255/255.0);
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, startx, starty);
    CGContextAddLineToPoint(context, endx, endy);
    CGContextStrokePath(context);
    CGContextRestoreGState(context);
}

@end
///////////
@implementation SEOptionsStoreItem
@synthesize label;
@synthesize background;
@synthesize content;
@synthesize button;
@synthesize buttonText;
@synthesize productId;
@synthesize productType;
@synthesize buttonText2;
@synthesize connectionImageView;
@synthesize hasUpdatePrice;
- (void) dealloc
{
    [productId release];
    [connectionTimer invalidate];
    connectionTimer  = nil;
    [super dealloc];
}
- (void) setDiscountImage: (UIImage*)image
{
    discountImageView.image  = image;
}
- (void) setContentImage: (UIImage*)image
{
    float width = image.size.width;
    float height = image.size.height;
    float topPadding = 1;
    float bottomPadding = 1;
    float leftPadding = 10;
    float dstHeight = self.frame.size.height - topPadding - bottomPadding;
    float dstWidth = width * dstHeight / height;
    content.frame = CGRectMake(leftPadding, topPadding, dstWidth, dstHeight);
    content.image = image;
    float labelLeftSpacing = 10;
    float labelRightSpacing = 10;
    float labelWidth = self.frame.size.width - content.frame.size.width - button.frame.size.width - labelLeftSpacing - labelRightSpacing;
    label.frame = CGRectMake(content.frame.origin.x + content.frame.size.width + labelLeftSpacing, label.frame.origin.y , labelWidth, label.frame.size.height);
}
- (void) addDiscountText
{
    /*
    [buttonText removeFromSuperview];
    [buttonText2 removeFromSuperview];
    [drawLineView removeFromSuperview];
    discountImageView.hidden = NO;
    CGRect r1 = CGRectMake(button.frame.origin.x, button.frame.origin.y + 5, button.frame.size.width, button.frame.size.height / 3);
    buttonText = [[FontLabel alloc] initWithFrame:r1 fontName:[SESystemConfig getFontName] pointSize:OPTIONS_LABEL_FONT_SIZE];
    [self addSubview:buttonText];
    [buttonText release];
    buttonText.textAlignment = UITextAlignmentCenter;
    buttonText.textColor = [UIColor colorWithRed:115.0/255 green:115.0/255 blue:115.0/255 alpha:1];
    
    drawLineView = [[SEDrawLineView alloc] initWithFrame:r1];
    [self addSubview:drawLineView];
    [drawLineView release];
    drawLineView.clipsToBounds = NO;
    drawLineView.backgroundColor = [UIColor clearColor];
    
    
    CGRect r2 = CGRectMake(button.frame.origin.x, button.frame.origin.y + r1.size.height, button.frame.size.width, button.frame.size.height * 2 / 3);
    buttonText2 = [[FontLabel alloc] initWithFrame:r2 fontName:[SESystemConfig getFontName] pointSize:30];
    [self addSubview:buttonText2];
    [buttonText2 release];
    buttonText2.textAlignment = UITextAlignmentCenter;
    buttonText2.textColor = [UIColor colorWithRed:255.0/255 green:246.0/255 blue:0/255 alpha:1];
     */
    /*
    NSString* oldPriceStr = [NSString stringWithFormat:@"%1.2f $", oldPrice];
    setLabelFont(buttonText, oldPriceStr,  [UIColor colorWithRed:115.0/255 green:115.0/255 blue:115.0/255 alpha:1], [SESystemConfig getFontName], 20);
    
    NSString* newPriceStr = [NSString stringWithFormat:@"%1.2f $", newPrice];
    setLabelFont(buttonText2, newPriceStr, [UIColor colorWithRed:255.0/255 green:246.0/255 blue:0/255 alpha:1], [SESystemConfig getFontName], 20 + 10);
    */
    /*
    CGSize dateLabelSize = CGSizeMake(200, self.frame.size.height / 3);
    CGRect dateLabelRect = CGRectMake(discountImageView.frame.origin.x - dateLabelSize.width, self.frame.size.height * 2 / 3, dateLabelSize.width, dateLabelSize.height);
    dateLabelView = [[FontLabel alloc] initWithFrame:dateLabelRect fontName: [SESystemConfig getFontName] pointSize:20];
    [self addSubview:dateLabelView];
    [dateLabelView release];
    dateLabelView.textColor = [UIColor whiteColor];
    */
    UIImage* discountImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"DiscountImage"];
    discountImageView.image = discountImage;
}
- (void) createChildWithFrame: (CGRect) frame : (SEProduct*)product
{
    self.backgroundColor = [UIColor clearColor];
    background = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, frame.size.width, frame.size.height)];
    [self addSubview:background];
    [background release];
    
    //CGRect contentFrame = CGRectMake(0, (frame.size.height - OPTIONS_STORE_ITEM_CONTENT_HEIGHT) / 2, OPTIONS_STORE_ITEM_CONTENT_WIDTH, OPTIONS_STORE_ITEM_CONTENT_HEIGHT);
    CGRect contentFrame = CGRectMake(0, 0, OPTIONS_STORE_ITEM_CONTENT_WIDTH, frame.size.height);
    content = [[UIImageView alloc] initWithFrame:contentFrame];
    [self addSubview:content];
    [content release];
    
    float buttonTopPadding = 11;
    float buttonBottomPadding = 11;
    
    CGRect buttonRect = CGRectMake(frame.size.width - OPTIONS_STORE_ITEM_BUTTON_WIDTH - 10, buttonTopPadding + 2, OPTIONS_STORE_ITEM_BUTTON_WIDTH, self.frame.size.height - buttonTopPadding - buttonBottomPadding);
    
    CGRect labelRect = CGRectMake(contentFrame.size.width + 10, (frame.size.height - OPTIONS_STORE_ITEM_LABEL_HEIGHT) / 2, OPTIONS_STORE_ITEM_LABEL_WIDTH, OPTIONS_STORE_ITEM_LABEL_HEIGHT);
    label = [[FontLabel alloc] initWithFrame:labelRect fontName:OPTIONS_LABEL_FONT_NAME pointSize:OPTIONS_LABEL_FONT_SIZE];
    [self addSubview:label];
    [label release];
    label.textAlignment = UITextAlignmentCenter;
    
    
    button = [UIButton buttonWithType: UIButtonTypeCustom];
    button.frame =  buttonRect;
    UIImage* normalImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"AppStoreButtonNormalBg"];
    normalImage = [SEUtil imageWithCap:normalImage top:0.1 bottom:0.9 left:0.1 right:0.9];
    UIImage* selectImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"AppStoreButtonSelectBg"];
    selectImage = [SEUtil imageWithCap:selectImage top:0.1 bottom:0.9 left:0.1 right:0.9];
    [button setBackgroundImage:normalImage forState:UIControlStateNormal];
    [button setBackgroundImage:selectImage forState:UIControlStateHighlighted];
    [self addSubview:button];
    
    UIImage* discountImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"DiscountImage"];
    //NSLog(@"discount image size = %f, %f", discountImage.size.width, discountImage.size.height);
    //NSLog(@"item size = %f, %f", self.frame.size.width, self.frame.size.height);
    CGSize discountImageSize = CGSizeMake(discountImage.size.width / 2, discountImage.size.height / 2);
    CGRect discountImageRect = CGRectMake(button.frame.origin.x - discountImageSize.width * 2 / 3 ,(frame.size.height - discountImageSize.height) / 2, discountImageSize.width, discountImageSize.height);
    discountImageView = [[UIImageView alloc] initWithFrame:discountImageRect];
    [self addSubview:discountImageView];
    [discountImageView release];
    
    
    //
    BOOL hasTwoButton = NO;
    SEProductManager* productManager = [PhotoFrameAppDelegate getProductManager];
    if([product isDiscount])
    {
        NSString* productID = nil;
        productID = [productManager getProductIdFromDiscountId:product.productId];
        SEProduct* newProduct = [productManager getProduct:productID];
        if(newProduct)
        {
            hasTwoButton = YES;
        }
        else 
        {
            hasTwoButton = NO;
        }
    }
    
    if([product isDiscount] == NO || hasTwoButton == NO)
    {
        buttonText = [[FontLabel alloc] initWithFrame:button.frame fontName:OPTIONS_LABEL_FONT_NAME pointSize:OPTIONS_LABEL_FONT_SIZE + 40];
        [self addSubview:buttonText];
        [buttonText release];
        buttonText.textAlignment = UITextAlignmentCenter;
    }
    else
    {
        CGRect r1 = CGRectMake(button.frame.origin.x, button.frame.origin.y + 5, button.frame.size.width, button.frame.size.height / 3);
        buttonText = [[FontLabel alloc] initWithFrame:r1 fontName:[SESystemConfig getFontName] pointSize:OPTIONS_LABEL_FONT_SIZE];
        [self addSubview:buttonText];
        [buttonText release];
        buttonText.textAlignment = UITextAlignmentCenter;
        buttonText.textColor = [UIColor colorWithRed:115.0/255 green:115.0/255 blue:115.0/255 alpha:1];
        
        drawLineView = [[SEDrawLineView alloc] initWithFrame:r1];
        [self addSubview:drawLineView];
        [drawLineView release];
        drawLineView.clipsToBounds = NO;
        drawLineView.backgroundColor = [UIColor clearColor];
        
        
        CGRect r2 = CGRectMake(button.frame.origin.x, button.frame.origin.y + r1.size.height, button.frame.size.width, button.frame.size.height * 2 / 3);
        buttonText2 = [[FontLabel alloc] initWithFrame:r2 fontName:[SESystemConfig getFontName] pointSize:30];
        [self addSubview:buttonText2];
        [buttonText2 release];
        buttonText2.textAlignment = UITextAlignmentCenter;
        buttonText2.textColor = [UIColor colorWithRed:255.0/255 green:246.0/255 blue:0/255 alpha:1];
    }
    
    CGSize dateLabelSize = CGSizeMake(200, frame.size.height / 3);
    CGRect dateLabelRect = CGRectMake(discountImageView.frame.origin.x - dateLabelSize.width, frame.size.height * 2 / 3, dateLabelSize.width, dateLabelSize.height);
    dateLabelView = [[FontLabel alloc] initWithFrame:dateLabelRect fontName: [SESystemConfig getFontName] pointSize:20];
    [self addSubview:dateLabelView];
    [dateLabelView release];
    dateLabelView.textColor = [UIColor whiteColor];
    
    float failTextHeight = button.frame.size.height / 4;
    float failTextY = button.frame.origin.y + button.frame.size.height *  3 / 4 - 10;
    CGRect updateFailTextRect = CGRectMake(button.frame.origin.x, failTextY, button.frame.size.width, failTextHeight);
    updatePriceFailText = [[FontLabel alloc] initWithFrame:updateFailTextRect fontName:[SESystemConfig getFontName] pointSize:14];
    updatePriceFailText.textAlignment = UITextAlignmentCenter;
    [self addSubview:updatePriceFailText];
    [updatePriceFailText release];
    
    float connectionWidth = 60;
    float connectionHeight = 60;
    connectionImageView = [[UIImageView alloc] initWithFrame:CGRectMake(button.frame.origin.x + (button.frame.size.width - connectionWidth)/ 2, button.frame.origin.y + (button.frame.size.height - connectionHeight) / 2, connectionWidth, connectionHeight)];
    [self addSubview:connectionImageView];
    [connectionImageView release];
}
- (void) setExpiredDate :(NSString*)str
{
    NSString* dateStr = [NSString stringWithFormat:@"Expired: %@", str];
    dateLabelView.text = dateStr;
}
- (id) initWithFrame:(CGRect)frame  product: (SEProduct*)product
{
    self = [super initWithFrame:frame];
    if(self)
    {
        [self createChildWithFrame:frame:product];
        touchState = STOREITEM_TOUCH_STATE_END;
    }
    return self;
}
/*
- (id) initWithCoder:(NSCoder *)aDecoder
{
    self = [super initWithCoder:aDecoder];
    if(self)
    {
        [self createChildWithFrame:self.frame];
        touchState =  STOREITEM_TOUCH_STATE_END;
    }
    return self;
}
 */
- (void) setLock:(BOOL)b type:(LOCK_TYPE)lockType
{
    if(lockView == nil)
    {
        lockView = [[SEOptionsLockView alloc] initWithFrame:CGRectMake(0, 0, self.frame.size.width, self.frame.size.height) lockSize:CGSizeMake(20, 80)];
        [lockView setText:@"You should buy Basic Setting firstly"];
        [self addSubview:lockView];
        [lockView release];
    }
    [lockView setLock:b type:lockType];
}
- (BOOL) isLocked
{
    return lockView != nil && lockView.hidden == NO;
}
- (BOOL) isBuied
{
    return buiedView != nil && buiedView.hidden == NO;
}
- (void)timerUpdate : (NSTimer*) timer
{
    connectionViewAngle += 30;
    if(connectionViewAngle >= 360)
    {
        connectionViewAngle = 0;
    }
    //[mLoadingView setStage:(LOADING_STAGE_TYPE)(rand() % 3)];
    CGAffineTransform t = CGAffineTransformMakeRotation(connectionViewAngle * 3.1415926 / 180.0);
    connectionImageView.transform = t;

}
- (void) startConnection
{
    connectionTimer = [NSTimer timerWithTimeInterval:0.3 target:self selector:@selector(timerUpdate:) userInfo:nil repeats:YES];
    [[NSRunLoop currentRunLoop] addTimer:connectionTimer forMode:NSDefaultRunLoopMode];
    UIImage* image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"ProductPriceLoading"];
    connectionImageView.image = image;
    connectionViewAngle = 0;
    connectionImageView.hidden = NO;
}
- (void) endConnection
{
    [connectionTimer invalidate];
    connectionTimer = nil;
    connectionViewAngle = 0;
    connectionImageView.hidden = YES;
}
- (void) setUpdatePriceFailed: (NSString*)text
{
    updatePriceFailText.text = text;
}
- (void) setBuied: (BOOL) b
{
    if(buiedView == nil)
    {
        UIImage* image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"yes"];
        CGSize srcSize = image.size;
        CGSize s = srcSize;//[SEUtil computeFitSize:srcSize toDst:CGSizeMake(button.frame.size.width, button.frame.size.height)];
        float x = self.frame.origin.x + self.frame.size.width - 20 - s.width;
        CGRect rect = CGRectMake(x, (self.frame.size.height - s.height) / 2, s.width, s.height);
        buiedView = [[UIImageView alloc] initWithFrame:rect];
        [self addSubview:buiedView];
        [buiedView release];
        buiedView.image = image;
    }
    if(b)
    {
        buiedView.hidden = NO;
        button.hidden = YES;
        buttonText.hidden = YES;
        buttonText2.hidden = YES;
        discountImageView.hidden = YES;
        drawLineView.hidden = YES;
        dateLabelView.hidden = YES;
    }
    else 
    {
        buiedView.hidden = YES;
        button.hidden = NO;
        buttonText.hidden = NO;
        buttonText2.hidden = NO;
        discountImageView.hidden = NO;
        drawLineView.hidden = NO;
        dateLabelView.hidden = NO;
    }
}
- (BOOL) isSelected
{
    return bSelected;
}
- (UIImage*) getImage:(NSString*) str
{
    UIImage* image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:str];
    image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
    return image;
}
- (void) setButtonBg:(UIButton*)tmpButton normal: (NSString*)normal selected: (NSString*)selected
{
    UIImage* normalImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:normal];
    normalImage = [SEUtil imageWithCap:normalImage top:0.1 bottom:0.9 left:0.1 right:0.9];
    UIImage* selectImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:selected];
    selectImage = [SEUtil imageWithCap:selectImage top:0.1 bottom:0.9 left:0.1 right:0.9];
    [tmpButton setBackgroundImage:normalImage forState:UIControlStateNormal];
    [tmpButton setBackgroundImage:selectImage forState:UIControlStateSelected];
}
- (void) setSelected: (BOOL) b
{
    if(b)
    {
        background.image = [self getImage: @"OptionsStoreItemSelectedBg"];
        [self setButtonBg:button normal:@"OptionsStoreItemSelectedButtonBg" selected:@"OptionsStoreItemSelectedButtonHBg"];
        bSelected = YES;
    }
    else 
    {
        background.image = [self getImage:@"AppStoreItemBg"];
        //background.image = [self getImage:@"AppStoreAddImageBg"];
        [self setButtonBg:button normal:@"AppStoreButtonNormalBg" selected:@"AppStoreButtonSelectBg"];
        bSelected = NO;
    }
}
- (void) setTarget: (id) t action: (SEL)a
{
    target = t;
    action = a;
}
- (void) touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{
    if(touchState == STOREITEM_TOUCH_STATE_END && [self isLocked] == NO)
    {
        touchState = STOREITEM_TOUCH_STATE_BEGIN;
        beginPoint = [[touches anyObject] locationInView:self];
        [self setSelected:YES];
        SEStoreItemParam* param = [[SEStoreItemParam alloc] init];
        param.state = touchState;
        param.item = self;
        [target performSelector:action withObject:param];
    }
}
- (void) touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event
{
    if(touchState == STOREITEM_TOUCH_STATE_BEGIN)
    {
        CGPoint p = [[touches anyObject] locationInView:self];
        float deltax = fabsf(p.x - beginPoint.x);
        float deltay = fabsf(p.y - beginPoint.y);
        if(deltax > 5 || deltay > 5)
        {
            touchState = STOREITEM_TOUCH_STATE_MOVED;
            [self setSelected:NO];
            SEStoreItemParam* param = [[SEStoreItemParam alloc] init];
            param.state = touchState;
            param.item = self;
            [target performSelector:action withObject:param];
        }
    }
}
- (void) touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event
{
    if(touchState == STOREITEM_TOUCH_STATE_BEGIN)
    {
        touchState = STOREITEM_TOUCH_STATE_END;
        SEStoreItemParam* param = [[SEStoreItemParam alloc] init];
        param.state = touchState;
        param.item = self;
        [target performSelector:action withObject:param];
    }
    touchState = STOREITEM_TOUCH_STATE_END;
}
- (void) touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event
{
    if(touchState == STOREITEM_TOUCH_STATE_BEGIN)
    {
        touchState = STOREITEM_TOUCH_STATE_END;
        SEStoreItemParam* param = [[SEStoreItemParam alloc] init];
        param.state = touchState;
        param.item = self;
        [target performSelector:action withObject:param];
    }
    touchState = STOREITEM_TOUCH_STATE_END;
}
@end

@implementation SEOptionsStoreTwoLabelItem
@synthesize label2;

- (void) setContentImage:(UIImage *)image
{
    [super setContentImage:image];
    float leftLabelSpacing = 10;
    float rightLabelSpacing = 10;
    float midSpacing = 10;
    float labelWidth = (self.frame.size.width - content.frame.size.width - button.frame.size.width - leftLabelSpacing - rightLabelSpacing - midSpacing) / 2;
    label.frame = CGRectMake(content.frame.origin.x + content.frame.size.width + leftLabelSpacing, label.frame.origin.y, labelWidth, label.frame.size.height);
    //label.backgroundColor = [UIColor blueColor];
    label2.frame = CGRectMake(label.frame.origin.x + label.frame.size.width, label.frame.origin.y, labelWidth, label.frame.size.height);
    //label2.backgroundColor = [UIColor redColor];
}
- (id) initWithFrame:(CGRect)frame product:(SEProduct *)product
{
    self = [super initWithFrame:frame product:product];
    if(self)
    {
        //[self createChildWithFrame:frame];
        CGRect contentFrame = CGRectMake(0, (frame.size.height - OPTIONS_STORE_ITEM_CONTENT_HEIGHT) / 2, OPTIONS_STORE_ITEM_CONTENT_WIDTH, OPTIONS_STORE_ITEM_CONTENT_HEIGHT);
        
        CGRect buttonRect = CGRectMake(frame.size.width - OPTIONS_STORE_ITEM_BUTTON_WIDTH, (frame.size.height - OPTIONS_STORE_ITEM_BUTTON_HEIGHT) / 2, OPTIONS_STORE_ITEM_BUTTON_WIDTH, OPTIONS_STORE_ITEM_BUTTON_HEIGHT);
        CGRect label1Rect = label.frame;
        CGRect label2Rect = CGRectMake(contentFrame.size.width + 10 + label1Rect.size.width + 10, (frame.size.height - OPTIONS_STORE_ITEM_LABEL2_HEIGHT) / 2, OPTIONS_STORE_ITEM_LABEL2_WIDTH, OPTIONS_STORE_ITEM_LABEL2_HEIGHT);  
        label2 = [[FontLabel alloc] initWithFrame:label2Rect fontName:OPTIONS_LABEL_FONT_NAME pointSize:OPTIONS_LABEL_FONT_SIZE];
        [self addSubview:label2];
        [label2 release];
    }
    return self;
}
/*
- (id) initWithCoder:(NSCoder *)aDecoder
{
    self = [super initWithCoder:aDecoder];
    if(self)
    {
        [self createChildWithFrame:self.frame];
    }
    return self;
}
 */
@end

////////////
//////
@interface SEOptionsStoreItemLevelUpMaxItem : SEOptionsStoreTwoLabelItem
- (void) setContentImage:(UIImage *)image;
@end
@implementation SEOptionsStoreItemLevelUpMaxItem
- (void) setContentImage:(UIImage *)image
{
    [super setContentImage:image];
    label.frame = CGRectMake(content.frame.origin.x + content.frame.size.width + 100, 10, 140, 40);
    label2.frame = CGRectMake(label.frame.origin.x + label.frame.size.width - 30, label.frame.origin.y + label.frame.size.height, 132, 42);
    label2.textAlignment = UITextAlignmentLeft;
    label2.backgroundColor = [UIColor clearColor];
}
/*
- (void) createChild:(CGRect)frame
{
    label2 = [[FontLabel alloc] initWithFrame:label.frame fontName:OPTIONS_LABEL_FONT_NAME pointSize:OPTIONS_LABEL_FONT_SIZE];
    [self addSubview:label2];
    [label2 release];
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
 */
/*
- (id) initWithFrame:(CGRect)frame product:(SEProduct *)product
{
    self = [super initWithFrame:frame product:product];
    if(self)
    {
        [self createChild:frame :product];
    }
    return self;
}
 */
@end
////////////////
@interface SEOptionsStoreTwoVerticalLabel : SEOptionsStoreTwoLabelItem

@end
@implementation SEOptionsStoreTwoVerticalLabel

- (void) setContentImage:(UIImage *)image
{
    [super setContentImage:image];
    float label1Width = 154;
    float label1Height = 27;
    float label2Width = 154;
    float label2Height = 27;
    
    float label1x = content.frame.origin.x + content.frame.size.width + 50;
    float label1y = (self.frame.size.height - label1Height - label2Height)  / 2;
    label.frame = CGRectMake(label1x, label1y, label1Width, label1Height);
    float label2x = label1x;
    float label2y = label.frame.origin.y + label.frame.size.height + 1;
    label2.frame = CGRectMake(label2x, label2y, label2Width, label2Height);
    label.textAlignment = UITextAlignmentLeft;
    label2.textAlignment = UITextAlignmentLeft;
}

@end
//////////////////
@interface SEOptionsStoreAddCountItem : SEOptionsStoreTwoLabelItem
{
    FontLabel* label3;
}
@property (nonatomic, readonly) FontLabel* label3;
@end
@implementation SEOptionsStoreAddCountItem
@synthesize label3;
- (void) setContentImage:(UIImage *)image
{
    [super setContentImage:image];
    float label1Width = 116;
    float label1Height = 50;
    label.frame = CGRectMake(content.frame.origin.x + content.frame.size.width + 10, (self.frame.size.height - label1Height) / 2, label1Width, label1Height);
    float label2Width = 143;
    float label2Height = 27;
    float label3Width = 143;
    float label3Height = 27;
    float label2x = label.frame.origin.x + label.frame.size.width + 10;
    float label2y = (self.frame.size.height - label2Height - label3Height) / 2;
    
    label2.frame = CGRectMake(label2x, label2y, label2Width, label2Height);
    float label3x = label2x;
    float label3y = label2.frame.origin.y + label2Height + 2;
    label3.frame = CGRectMake(label3x, label3y, label3Width, label3Height);
}
- (void) createChild:(CGRect)frame
{
    label3 = [[FontLabel alloc] initWithFrame:label.frame fontName:OPTIONS_LABEL_FONT_NAME pointSize:OPTIONS_LABEL_FONT_SIZE];
    [self addSubview:label3];
    [label3 release];
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
////////////////
//65, 
//215
@implementation SEOptionsImageView
@synthesize imageView;
- (void) createChildWithFrame: (CGRect) frame
{
    imageView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, frame.size.width, frame.size.height)];
    [self addSubview:imageView];
    [imageView release];
    self.backgroundColor = [UIColor clearColor];
}
- (id) initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if(self)
    {
        [self createChildWithFrame: frame];
    }
    return self;
}
-(id) initWithCoder:(NSCoder *)aDecoder
{
    self = [super initWithCoder:aDecoder];
    if(self)
    {
        [self createChildWithFrame: self.frame];
    }
    return self;
}
- (void) setImageViewSize: (CGSize)s
{
    imageView.frame = CGRectMake((self.frame.size.width - s.width) / 2, (self.frame.size.height - s.height) / 2, s.width, s.height);
}
@end
/////
@implementation SEOptionsLabel
@synthesize label;
@synthesize background;
- (void) setText:(NSString *)text
{
    label.text = text;
}
- (NSString*) text
{
    return label.text;
}
- (void) createChild: (CGRect)frame
{
    //NSLog(@"frame = %f, %f, %f, %f", frame.origin.x, frame.origin.y, frame.size.width, frame.size.height);
    //int tag = self.tag;
    //NSLog(@"tag = %d", tag);
    background = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, frame.size.width, frame.size.height)];
    [self addSubview:background];
    [background release];
    float width = frame.size.width - 10 - 10;
    label = [[FontLabel alloc] initWithFrame:CGRectMake(10, 0, width, frame.size.height) fontName:OPTIONS_LABEL_FONT_NAME pointSize:OPTIONS_LABEL_FONT_SIZE];
    [self addSubview:label];
    [label release];
    self.backgroundColor = [UIColor clearColor];
}
- (void) setTextCenter: (BOOL)b
{
    if(b)
    {
        label.frame = CGRectMake(0, 0, self.frame.size.width, self.frame.size.height);
        label.textAlignment = UITextAlignmentCenter;
    }
    else
    {
        label.frame = CGRectMake(10, 0, self.frame.size.width, self.frame.size.height);
        label.textAlignment = UITextAlignmentLeft;
    }
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
    if(self)
    {
        //NSLog(@"SEOptionsLabel initWithCoder");
        CGRect frame = self.frame;
        [self createChild:frame];
        //label.textAlignment = UITextAlignmentCenter;
    }
    return self;
}

@end
///
@interface SESlider : UISlider
{
    float width;
}
@property (nonatomic, assign) float width;
@end
@implementation SESlider
@synthesize width;
/*
- (CGRect) minimumValueImageRectForBounds:(CGRect)bounds
{
    CGRect result = [super minimumValueImageRectForBounds:bounds];
    result = CGRectOffset(result, -12, 0);
    return result;
}
- (CGRect) maximumValueImageRectForBounds:(CGRect)bounds
{
    CGRect result = [super maximumValueImageRectForBounds:bounds];
    result = CGRectOffset(result, 12, 0);
    return result;
}
- (CGRect) trackRectForBounds:(CGRect)bounds
{
    CGRect result = [super trackRectForBounds:bounds];
    result.origin.x = 0;
    result.size.width = bounds.size.width;
    return result;
}
- (CGRect) thumbRectForBounds:(CGRect)bounds trackRect:(CGRect)rect value:(float)value
{
    CGRect result = [super thumbRectForBounds: bounds trackRect:rect value:value];
    if(value == self.minimumValue)
        result = CGRectOffset(result, -13, 0);   
    else if(value == self.maximumValue)
        result = CGRectOffset(result, 13, 0);
    return result;
}
 */
- (id) initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if(self)
    {
        width = frame.size.width;
    }
    return self;
}
@end
@implementation SEOptionsSlider
@synthesize slider;
- (void) createChildWithFrame: (CGRect) frame
{
    //float sliderHeight = 23;
    slider = [[SESlider alloc] initWithFrame:CGRectMake(0, 0, frame.size.width, frame.size.height)];
    [self addSubview:slider];
    [slider release];
    self.backgroundColor = [UIColor clearColor];
}
- (id) initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if(self)
    {
        [self createChildWithFrame:frame];
        self.userInteractionEnabled = YES;
    }
    return self;
}
- (id) initWithCoder:(NSCoder *)aDecoder
{
    self = [super initWithCoder:aDecoder];
    if(self)
    {
        [self createChildWithFrame:self.frame];
    }
    return self;
}
- (void) setLock: (BOOL) l lockType: (LOCK_TYPE) lockType
{
    if(lockView == nil)
    {
        lockView = [[SEOptionsLockView alloc] initWithFrame:CGRectMake(0, 0, self.frame.size.width, self.frame.size.height)];
        [self addSubview:lockView];
        [lockView release];
    }
    [lockView setLock:l type:lockType];
}

@end
///////
@implementation SESelectScrollChildView
@synthesize mViewNav;
@synthesize mContentSize;
@synthesize mLockImageView;
@synthesize mText;
@synthesize mLockBgView;
- (CGSize) getContentSize
{
    return mContentView.frame.size;
}
- (void) hideShell:(BOOL)b
{
    mHideShell = b;
    if(mHideShell)
    {
        mShellImageView.hidden = YES;
    }
    else 
    {
        mShellImageView.hidden = NO;
    }
}
- (BOOL) isShellHidden
{
    return mShellImageView.hidden;
}
- (UIImage *)image
{
    return mContentView.image;
}
- (void) setImage:(UIImage *)image
{
    mContentView.image = image;
}
- (void) setImageCenter : (CGSize)size;
{
    //mContentView.contentMode = UIViewContentModeCenter;
    mContentView.frame = CGRectMake((self.frame.size.width - size.width) / 2, (self.frame.size.height - size.height) / 2, size.width, size.height);
    mContentView.clipsToBounds = YES;
    mLockBgView.frame = CGRectMake(mContentView.frame.origin.x, mContentView.frame.origin.y, mContentSize.width, mContentSize.height);
    mLockImageView.frame = CGRectMake(mLockBgView.frame.origin.x , mLockBgView.frame.origin.y, mLockBgView.frame.size.width, mLockBgView.frame.size.height);
}
- (void) setLockBg : (BOOL)b
{
    if(b)
    {
        mLockBgView.hidden = NO;
        UIImage* lockBg = [mViewNav.mResLoader getImage:@"OptionsSelectScrollViewLock"];
        mLockBgView.image = lockBg;
    }
    else 
    {
        mLockBgView.hidden = YES;
        mLockBgView.image = nil;
    }
}
- (void) changeChildFrame
{
    mShellImageView.frame = CGRectMake(0, 0, self.frame.size.width, self.frame.size.height);
}
- (void) changeContentFrame: (CGSize)s : (CGSize) contentSize
{
    float x = mContentView.frame.origin.x;
    mContentView.frame = CGRectMake((self.frame.size.width - s.width) / 2, (self.frame.size.height - s.height) / 2, s.width, s.height);
    mShellImageView.frame = CGRectMake((self.frame.size.width - mShellImageView.frame.size.width) / 2, (self.frame.size.height - mShellImageView.frame.size.height) / 2, mShellImageView.frame.size.width, mShellImageView.frame.size.height);
    mText.frame = CGRectMake(0, self.frame.size.height * 2/3 - 10, self.frame.size.width, self.frame.size.height / 3);
    float lockBgViewWidth = s.width;
    /*
    if(s.width < contentSize.width)
    {
        lockBgViewWidth = s.width;
    }
    else 
    {
        lockBgViewWidth = contentSize.width;
    }
     */
    mLockBgView.frame = CGRectMake(x, mContentView.frame.origin.y, lockBgViewWidth, mContentSize.height);
    mLockImageView.frame = CGRectMake(mLockBgView.frame.origin.x , mLockBgView.frame.origin.y, mLockBgView.frame.size.width, mLockBgView.frame.size.height);
}
- (id) initWithViewNav: (SEViewNavigator*)viewNav;
{
    //self = [super initWithFrame:frame];
    self = [super init];
    if(self)
    {
        mViewNav = viewNav;
        //self.clipsToBounds = YES;
        UIImage* bg = [mViewNav.mResLoader getImage:@"OptionsSelectScrollViewBg"];
        self.frame = CGRectMake(0, 0, bg.size.width, bg.size.height - SELECTED_SCROLLVIEW_PADDINGY * 2);
        mContentSize = CGSizeMake(bg.size.width, bg.size.height - 36);
        //NSLog(@"mContentSize = %f, %f", mContentSize.width, mContentSize.height);
        mContentView = [[UIImageView alloc] initWithFrame:CGRectMake((bg.size.width - mContentSize.width) / 2, (bg.size.height - mContentSize.height )/ 2, mContentSize.width, mContentSize.height)];

        [self addSubview:mContentView];
        [mContentView release];
        
        CGRect r1 = CGRectMake(mContentView.frame.origin.x, mContentView.frame.origin.y + mContentView.frame.size.height * 2 / 3, mContentView.frame.size.width, mContentView.frame.size.height  / 3);
        mText =  [[FontLabel alloc] initWithFrame:r1 fontName:[SESystemConfig getFontName] pointSize:20];
        mText.textAlignment = UITextAlignmentCenter;
        mText.textColor = [UIColor whiteColor];
        mText.backgroundColor = [UIColor clearColor];
        [self addSubview:mText];
        [mText release];
        
        mLockBgView = [[UIImageView alloc] initWithFrame:CGRectMake(mContentView.frame.origin.x, mContentView.frame.origin.y, mContentSize.width, mContentSize.height)];
        [self addSubview:mLockBgView];
        [mLockBgView release];
        UIImage* lockBg = [mViewNav.mResLoader getImage:@"OptionsSelectScrollViewLock"];
        mLockBgView.image = lockBg;
        
        //UIImage* lock = [mViewNav.mResLoader getImage:@"OptionsSelectScrollViewLock"];
        mLockImageView = [[UIImageView alloc] initWithFrame:CGRectMake((bg.size.width - lockBg.size.width) / 2, (bg.size.height - lockBg.size.height) / 2, lockBg.size.width, lockBg.size.height)];
        //mLockImageView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, lock.size.width, lock.size.height)];
        [self addSubview:mLockImageView];
        [mLockImageView release];
        //mLockImageView.image = lock;
        
        //float startx = (w - bg.size.width ) /2;
        //float starty = (h - bg.size.height) / 2;
        mShellImageView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, bg.size.width, bg.size.height)];
        [self addSubview:mShellImageView];
        [mShellImageView release];
        mShellImageView.image = bg;
    }
    return self;
}
- (void) setLock : (BOOL) b lockType:(LOCK_TYPE)lockType
{
    if(b)
    {
        mLockImageView.hidden = NO;
        UIImage* image = getLockImage(lockType);
        //CGSize dstSize = mContentSize;
        CGSize s = [SEUtil computeFitSize:CGSizeMake(image.size.width, image.size.height) toDst:mContentSize];
        //UIImage* newImage = [SEUtil drawImage:image inRect:CGRectMake(0, 0, s.width, s.height)];
        mLockImageView.image = image;
        mLockImageView.frame = CGRectMake((self.frame.size.width - s.width) / 2, (self.frame.size.height - s.height) / 2, s.width, s.height);
        
        [self setLockBg:b];
    }
    else
    {
        mLockImageView.hidden = YES;
        mLockImageView.image = nil;    
        [self setLockBg:b];
    }
}
- (BOOL) isLocked
{
    return mLockImageView.hidden == NO && mLockImageView.image != nil;
}
@end
///////
@implementation SESelectScrollView
@synthesize mScrollView;
@synthesize mContentParent;
- (void) setData
{
    
}

- (void) initView: (SEViewNavigator*)viewNav
{
    mViewNav = viewNav;
    self.userInteractionEnabled = YES;
    self.backgroundColor = [UIColor clearColor];
    if(mBgView == nil)
    {
        mBgView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, self.frame.size.width, self.frame.size.height)];
        UIImage* image = [viewNav.mResLoader getImage:@"OptionsSelectScrollViewBackground"];
        mBgView.image = image;
        [self addSubview:mBgView];
        [mBgView release];
    }
    float offsetx = 20;
    float offsety = SELECTED_SCROLLVIEW_PADDINGY;
    if(mScrollView == nil)
    {
        //mScrollView = [[UIScrollView alloc] initWithFrame:CGRectMake(offsetx, offsety, 618, 72)];
        mScrollView = [[UIScrollView alloc] initWithFrame:CGRectMake(offsetx, offsety, self.frame.size.width - offsetx - offsetx, self.frame.size.height - offsety * 2)];
        mScrollView.clipsToBounds = YES;
        mScrollView.backgroundColor = [UIColor clearColor];
        [self addSubview:mScrollView];
        [mScrollView release];
        mContentParent = [[UIView alloc] initWithFrame:CGRectMake(0, 0, mScrollView.frame.size.width, mScrollView.frame.size.height)];
        mContentParent.userInteractionEnabled = YES;
        [mScrollView addSubview:mContentParent];
        [mContentParent release];
        //mScrollView.backgroundColor = [UIColor blueColor];
    }
    
    if(mMaskView == nil)
    {
        mMaskView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, self.frame.size.width, self.frame.size.height)];
        UIImage* image = [viewNav.mResLoader getImage:@"OptionsSelectScrollViewMask"];
        mMaskView.image = image;
        [self addSubview:mMaskView];
        [mMaskView release];
        mMaskView.userInteractionEnabled = NO;
    }
     
}
- (void) clearAllChild
{
    /*
    for(int i = 0 ; i < mContentParent.subviews.count; i++)
    {
        UIView* v = [mContentParent.subviews objectAtIndex:i];
        [v removeFromSuperview];
        NSLog(@"mConetParent subviews count = %d", mContentParent.subviews.count);
    }
    */
    [mContentParent removeFromSuperview];
    mContentParent = [[UIView alloc] initWithFrame:CGRectMake(0, 0, mScrollView.frame.size.width, mScrollView.frame.size.height)];
    mContentParent.userInteractionEnabled = YES;
    [mScrollView addSubview:mContentParent];
    [mContentParent release];
    
    mContentParent.frame = CGRectMake(0, 0, mScrollView.frame.size.width, mScrollView.frame.size.height);
    [mScrollView setNeedsDisplay];
    [mContentParent setNeedsDisplay];
    startx = 0;
}
- (SESelectScrollChildView*) addChildItem: (UIImage*) contentImage viewNav:(SEViewNavigator*)viewNav text:(NSString *)text
{
    //UIImage* shellImage = [mViewNav.mResLoader getImage:@"OptionsSelectScrollViewBg"];
    SESelectScrollChildView* childView = [[SESelectScrollChildView alloc] initWithViewNav:viewNav];
    childView.mText.text = text;
    float  starty = (mScrollView.frame.size.height - childView.frame.size.height) / 2;

    childView.frame = CGRectMake(startx, starty, childView.frame.size.width, childView.frame.size.height);
    [mContentParent addSubview:childView];
    [childView release];
    CGSize dstS = CGSizeMake(childView.frame.size.width, childView.frame.size.height);
    CGSize srcS = CGSizeMake(contentImage.size.width, contentImage.size.height);
    //NSLog(@"## src s = %f, %f", srcS.width, srcS.height);
    //if(fabsf(dstS.height - srcS.height) < 20)
    {
        dstS.height = dstS.height - 40;
        dstS.width = dstS.width - 25;
    }
    
    float height = dstS.height;
    float width = height * srcS.width / srcS.height;
    
    childView.frame = CGRectMake(startx, starty, width, childView.frame.size.height);
    [childView changeContentFrame:CGSizeMake(width, height) : CGSizeMake(dstS.width, dstS.height)];
    CGSize s = CGSizeMake(width, height);
    //NSLog(@"sss = %f , %f", s.width, s.height);
    UIImage* newImage = [SEUtil drawImage:contentImage toSize:s];
    if(s.width == srcS.width && s.height == srcS.height)
    {
        childView.image = contentImage;
    }
    else
    {
        childView.image = newImage;// contentImage;
    }
    //[childView setImageCenter: s];
    float spacing = 50;
    startx += childView.frame.size.width + spacing;
    return childView;
}
- (void) setContentSize
{
    mScrollView.contentSize = CGSizeMake(startx , mScrollView.frame.size.height);
    mContentParent.frame = CGRectMake(mContentParent.frame.origin.x, mContentParent.frame.origin.y, mScrollView.contentSize.width, mScrollView.contentSize.height);
}
- (void) setSelected: (SESelectScrollChildView*)v
{
    NSLog(@"content size = %d", mContentParent.subviews.count);
    for(SESelectScrollChildView* view in mContentParent.subviews)
    {
        if(v == view)
        {
            [view hideShell:NO];
        }
        else
        {
            [view hideShell:YES];
        }
    }
}
- (void) setLock:(BOOL)l lockType:(LOCK_TYPE)lockType
{
    if(mLockView == nil)
    {
        mLockView = [[SEOptionsLockView alloc] initWithFrame:CGRectMake(0, 0, self.frame.size.width, self.frame.size.height)];
        [self addSubview:mLockView];
        [mLockView release];
    }
    [mLockView setLock:l  type:lockType];
}
@end
///////
/*
@interface SEBrushScrollViewImpl : UIScrollView
{
    UIImage* mBgImage;
}
@property (nonatomic, retain) UIImage* mBgImage;
@end
@implementation SEBrushScrollViewImpl
@synthesize mBgImage;
- (void)drawRect:(CGRect)rect
{
    [mBgImage drawInRect:rect];
}

@end
////
@interface SEBrushView : UIView
{
    UIImageView* mBgImageView;
    UIImageView* mContentView;
    UIImageView* mLockImageView;
    SEViewNavigator* mViewNav;
    BOOL mHideBg;
}
@property (nonatomic, assign) SEViewNavigator* mViewNav;
@property (nonatomic, assign) UIImage* image;
- (void) hideBackground: (BOOL) b;
@end
////
@implementation SEBrushScrollView
@synthesize mScrollView;
@synthesize mViewNav;
- (void)dealloc
{
    [mBgImage release];
    [super dealloc];
}
- (UIImage*) createScrollViewBg: (UIImage*)image rect: (CGRect) rect
{
    UIGraphicsBeginImageContext(rect.size);
    [image drawAtPoint:CGPointMake(-rect.origin.x, -rect.origin.y)];
    UIImage* ret = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    return ret;

}
- (void) clearSelectedBg
{
    for(UIView* v in mScrollView.subviews)
    {
        if([v isMemberOfClass:[SEBrushView class]])
        {
            SEBrushView* bv = (SEBrushView*)v;
            [bv hideBackground:YES];
        }
    }
}
- (void) initScrollView : (SEViewNavigator*)viewNav
{
    if(mBgImage == nil)
    {
        mBgImage = [viewNav.mResLoader getImage:@"OptionsSelectScrollViewBackground"];
    }
    float offset = 20;
    mScrollView = [[SEBrushScrollViewImpl alloc] initWithFrame:CGRectMake(offset, 0, self.frame.size.width - 2 * offset, self.frame.size.height)];
    mScrollView.mBgImage = [self createScrollViewBg:mBgImage rect:CGRectMake(offset, 0, mScrollView.frame.size.width, mScrollView.frame.size.height)];
    [self addSubview:mScrollView];
    [mScrollView release];
}

- (void)drawRect:(CGRect)rect
{
    if(mBgImage == nil)
    {
        mBgImage = [mViewNav.mResLoader getImage:@"OptionsSelectScrollViewBackground"];
    }
    [mBgImage drawInRect:rect];
}

@end
///////
////////////

@implementation SEBrushView
@synthesize mViewNav;
- (void) hideBackground:(BOOL)b
{
    if(mHideBg != b)
    {
        mHideBg = b;
        if(mHideBg)
        {
            mBgImageView.hidden = YES;
        }
        else 
        {
            mBgImageView.hidden = NO;
        }
    }
}
- (UIImage *)image
{
    return mContentView.image;
}
- (void) setImage:(UIImage *)image
{
    mContentView.image = image;
}
- (id) initWithFrame:(CGRect)frame withViewNav: (SEViewNavigator*)viewNav;
{
    self = [super initWithFrame:frame];
    if(self)
    {
        mViewNav = viewNav;
        UIImage* bg = [mViewNav.mResLoader getImage:@"OptionsSelectScrollViewBg"];
        self.frame = CGRectMake(frame.origin.x, frame.origin.y, bg.size.width, bg.size.height);
        float w = 150, h = 75;
        mContentView = [[UIImageView alloc] initWithFrame:CGRectMake((bg.size.width - w) / 2, (bg.size.height - h )/ 2, w, h)];
        [self addSubview:mContentView];
        [mContentView release];
        
        UIImage* lock = [mViewNav.mResLoader getImage:@"OptionsSelectScrollViewLock"];
        mLockImageView = [[UIImageView alloc] initWithFrame:CGRectMake((bg.size.width - w) / 2, (bg.size.height - h) / 2, lock.size.width, lock.size.height)];
        [self addSubview:mLockImageView];
        [mLockImageView release];
        
        
        mLockImageView.image = lock;
        mBgImageView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, bg.size.width, bg.size.height)];
        [self addSubview:mBgImageView];
        [mBgImageView release];
        mBgImageView.image = bg;
    }
    return self;
}
@end
 */
//////////////
@implementation SEOptionsSettingViewController
@synthesize mViewNav;
@synthesize mOptionsView;
- (void) addViewToParent: (UIView*) parent
{
    if(mView != nil)
    {
        [parent addSubview: mView];
    }
}
-(UIView*) getView
{
    return nil;
}


- (void) removeFromParent : (UIView*)parent
{
    [mView removeFromSuperview];
    //[mView release];
    //mView = nil;
}
- (void) dealloc
{
    [mView release];
    [super dealloc];
}
- (void) setImage: (UIImageView*)imageView name: (NSString*) name
{
    UIImage* image = [mViewNav.mResLoader getImage:name];
    imageView.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
}
- (void) setData
{
}
- (float) getHeight
{
    return 0;
}
- (void) update
{}
@end
///////////
@implementation SEOptionsPlaySetting
@synthesize mDrawingSettingBg;
@synthesize mWaitingTimeBg;
@synthesize  mDrawSettingTitle;
@synthesize mApple;
@synthesize  mQualityTitle;
@synthesize  mTimesTitle;
@synthesize  mQualitySlider;
@synthesize  mTimesSlider;
@synthesize  mQualityValue;
@synthesize  mTimesValue;
@synthesize  mWaitingTimeTitle;
@synthesize  mSecondValue;
@synthesize  mSecondLabel;
@synthesize  mWaitingTimeSlider;
@synthesize  mPlayModeTitle;
@synthesize  mPlayModeBg;
@synthesize  mPlaySequenceTitle;
@synthesize  mPlayRandomTitle;
@synthesize  mPlayIntelTitle;
@synthesize  mPlaySequence;
@synthesize  mPlayRandom;
@synthesize  mPlayIntelligence;
@synthesize  mBrushTitle;
@synthesize  mBrushScrollView;
@synthesize  mAngleAdjust;
@synthesize  mTransparentAdjust;
@synthesize  mAngleLabel;
@synthesize  mTransparentLabel;
@synthesize mCurrentTimes;
@synthesize mCurrentQuality;
@synthesize mAngleAdjustValue;
@synthesize mTransparentValue;
@synthesize mBrushBg;
@synthesize mImageSizeFilterTitle;
@synthesize mImageSizeFilterSwitch;
@synthesize mImageSizeFilterTitleLabel;
@synthesize mImageSizeFilterBg;
@synthesize mDensityNum;
@synthesize mDensityLabel;
@synthesize mDensitySlider;
@synthesize mEdgeDetectLabel;
@synthesize mEdgeDetectValue;
@synthesize mEdgeDetectSlider;
@synthesize mRotateLockTitle;
@synthesize mRotateLockSwitch;
@synthesize mDrawBrushDefault;
@synthesize mDrawBrushEveryTime;
@synthesize mDrawBrushEveryImage;
@synthesize mDrawBrushDefaultTitle;
@synthesize mDrawBrushEveryTimeTitle;
@synthesize mDrawBrushEveryImageTitle;
@synthesize mBrushSetingBg;
- (void) dealloc
{
    [mApple release];
    [mQualityTitle release];
      [mTimesTitle release];
      [mQualitySlider release];
       [mTimesSlider release];
       [mQualityValue release];
       [mTimesValue release];
       [mWaitingTimeTitle release];
       [mSecondValue release];
       [mSecondLabel release];
       [mWaitingTimeSlider release];
       [mPlayModeTitle release];
       [mPlayModeBg release];
       [mPlaySequenceTitle release];
       [mPlayRandomTitle release];
       [mPlayIntelTitle release];
       [mPlaySequence release];
       [mPlayRandom release];
       [mPlayIntelligence release];
       [mBrushTitle release];
       [mBrushScrollView release];
       [mAngleAdjust release];
       [mTransparentAdjust release];
       [mAngleLabel release];
       [mTransparentLabel release];
      [mCurrentTimes release];
      [mCurrentQuality release];
      [mDrawSettingTitle release];
    
    [mImageSizeFilterBg release];
    [mImageSizeFilterTitle release];
    [mImageSizeFilterTitleLabel release];
    [mImageSizeFilterSwitch release];
    
    [mDensityNum release];
    [mDensityLabel release];
    [mDensitySlider release];
    
    [mEdgeDetectValue release];
    [mEdgeDetectLabel release];
    [mEdgeDetectSlider release];
    [mRotateLockTitle release];
    [mRotateLockSwitch release];
    
    [mPlayModeRadioButton release];
    [mDrawBrushModeRadioButton release];
    
    [mDrawBrushEveryTimeTitle release];
    [mDrawBrushEveryTime release];
    [mDrawBrushEveryImage release];
    [mDrawBrushEveryImageTitle release];
    [mDrawBrushDefaultTitle release];
    [mDrawBrushDefault release];
    [mBrushSetingBg release];
    [super dealloc] ;
}
- (float) getHeight
{
    CGRect r = mView.frame;
    mView.frame = CGRectMake(r.origin.x, r.origin.y, r.size.width, mBrushBg.frame.origin.y + mBrushBg.frame.size.height);
    return mView.frame.origin.y + mView.frame.size.height;
}
- (void) setSampleImageWithQuality:(int)percent withResLoader:(SEResLoader*)resLoader
{
    UIImage* uiImage = nil;
    switch (percent) 
    {
        case 4:
        {
            uiImage  = [resLoader getImage:@"Apple4"];
        }
            break;
        case 5:
        {
            uiImage = [resLoader getImage:@"Apple5"];
        }
            break;
        case 6:
        {
            uiImage = [resLoader getImage:@"Apple6"];
        }
            break;
            
        case 7:
        {
            uiImage = [resLoader getImage:@"Apple7"];
        }
            break;
        case 8:
        {
            uiImage = [resLoader getImage:@"Apple8"];
        }
            break;
        case 9:
        {
            uiImage = [resLoader getImage:@"Apple9"];
        }
            break;
        case 10:
        {
            uiImage = [resLoader getImage:@"Apple10"];
        }
            break;
        default:
            break;
    }
    mApple.image = [resLoader getImage:@"appleBg"];
    mApple.imageView.image = uiImage;
    [mApple setImageViewSize: uiImage.size];
}
- (void) handleDrawBrushModeChange:(NSNumber*)num
{
    NSLog(@"brush mod = %@", num);
    int v = [num intValue];
    UserInfo* userInfo = [mViewNav getUserInfo];
    userInfo.drawbrushmode = [NSNumber numberWithInt:v];
}
- (void) handlePlayModeChange: (NSNumber*) num //(UITapGestureRecognizer*) tap
{
    NSLog(@"handleplaymode change");
    /*
    UIImageView* view = (UIImageView*)tap.view;
    UIImageView* images[] = {mPlaySequence.imageView, mPlayRandom.imageView, mPlayIntelligence.imageView};
    for(int i = 0 ; i < 3  ; i++)
    {
        [self setImage:images[i] name:gPlayModeNoOK[i]];
    }
    //PLAY_MODE oldMode = mPlayMode;
    if(view == mPlaySequence && mPlayMode != SEQUENCE)
    {
        mPlayMode = SEQUENCE;
        
    }
    else if(view == mPlayRandom && mPlayMode != RANDOM)
    {
        mPlayMode = RANDOM;
        //[self setImage:mPlayRandom name:@"OptionsPlaySettingPlayModeRandomOK"];
    }
    else if (view == mPlayIntelligence && mPlayMode != INTEL)
    {
        mPlayMode = INTEL;
        //[self setImage:mPlayIntelligence name:@"OptionsPlaySettingPlayModeIntelOK"];
    }
    [self setImage:images[mPlayMode] name: gPlayModeOK[mPlayMode]];
     */
    mPlayMode = (PLAY_MODE)[num intValue];
    NSLog(@" mode = %d ", mPlayMode);
    UserInfo* userInfo = [mViewNav getUserInfo];
    userInfo.imageplaymode = [NSNumber numberWithInt:mPlayMode];
}
- (void) rotateLockSwitchHandler: (NSNumber*) bOk
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    userInfo.rotatescreen = [NSNumber numberWithBool:[bOk boolValue]];
    /*
    if([s isOn])
    {
        userInfo.rotatescreen = [NSNumber numberWithBool:YES];
    }
    else
    {
        userInfo.rotatescreen = [NSNumber numberWithBool:NO];    
    }
     */
}
- (void) setLabel
{
    NSString* fontName = [SEUtil getFontName];
    CGFloat fontSize = 30;
    setLabelFont(mDrawSettingTitle.label, @"Draw Set", getLabelColor(),fontName, fontSize);
    setLabelFont(mQualityTitle.label, @"Quality", getViewColor(), fontName, fontSize);
    setLabelFont(mTimesTitle.label, @"Layer", getViewColor(), fontName, fontSize);
    setLabelFont(mWaitingTimeTitle.label, @"Interval", getLabelColor(), fontName, fontSize);
    setLabelFont(mSecondLabel.label, @"Sec", getViewColor(), fontName, fontSize);
    setLabelFont(mPlayModeTitle.label, @"Play Mode", getLabelColor(), fontName, fontSize);
    setLabelFont(mPlaySequenceTitle.label, @"Order", getViewColor(), fontName, fontSize);
    setLabelFont(mPlayRandomTitle.label, @"Random", getViewColor(), fontName, fontSize);
    setLabelFont(mPlayIntelTitle.label, @"Auto", getViewColor(), fontName, fontSize);
    setLabelFont(mBrushTitle.label, @"Brush", getLabelColor(), fontName, fontSize);
    setLabelFont(mQualityValue.label, @"0", getLabelColor(), fontName, 60);
    [mQualityValue setTextCenter:YES];
    [mTimesValue setTextCenter:YES];
    setLabelFont(mTimesValue.label, @"0", getLabelColor(), fontName, 60);
    setLabelFont(mSecondValue.label, @"0", getLabelColor(), fontName, 60);
    [mSecondValue setTextCenter:YES];
    setLabelFont(mAngleAdjustValue.label, @"0", getViewColor(), fontName, fontSize);
    mAngleAdjustValue.label.textAlignment = UITextAlignmentCenter;
    setLabelFont(mAngleLabel.label, @"Jitter", getViewColor(), fontName, fontSize);
    setLabelFont(mTransparentValue.label, @"50", getViewColor(), fontName, fontSize);
    mTransparentValue.label.textAlignment = UITextAlignmentCenter;
    setLabelFont(mTransparentLabel.label, @"Humidity", getViewColor(), fontName, fontSize);
    
    setLabelFont(mImageSizeFilterTitleLabel.label, @"Other", getLabelColor(), fontName, fontSize);
    setLabelFont(mImageSizeFilterTitle.label, @"Start Image Size Filter", getViewColor(), fontName, fontSize);
    
    setLabelFont(mDensityLabel.label, @"Density", getViewColor(), fontName, 25);
    setLabelFont(mEdgeDetectLabel.label, @"Refinement", getViewColor(), fontName, 25);
    setLabelFont(mRotateLockTitle.label, @"Rotate Screen", getViewColor(), fontName, fontSize);
    UserInfo* userInfo = [mViewNav getUserInfo];
    int densityValue = [mViewNav.mSystemDataManager.brushdensity intValue];
    setLabelFont(mDensityNum.label, [NSString stringWithFormat:@"%d", densityValue], getViewColor(), fontName, fontSize);
    mDensityNum.label.textAlignment = UITextAlignmentCenter;
    
    int edgeDetectValue = [mViewNav.mSystemDataManager.brushedgedetect intValue];
    setLabelFont(mEdgeDetectValue.label, [NSString stringWithFormat:@"%d", edgeDetectValue], getViewColor(), fontName, fontSize);
    mEdgeDetectValue.label.textAlignment = UITextAlignmentCenter;
    
    setLabelFont(mDrawBrushDefaultTitle.label, @"Painting by Selected Brush", getViewColor(), fontName, fontSize);
    setLabelFont(mDrawBrushEveryImageTitle.label, @"Random Brush Selection by Picture", getViewColor(), fontName, fontSize);
    setLabelFont(mDrawBrushEveryTimeTitle.label, @"Random Brush Selection by Layer", getViewColor(), fontName, fontSize);
    
}
- (NSUInteger) getViewIndex: (UIView*) dstView
{
    const int INVALID_INDEX = 999999;
    NSUInteger destIndex = INVALID_INDEX;
    for(int i = 0 ; i < mView.subviews.count ; i++)
    {
        UIView* v = [mView.subviews objectAtIndex:i];
        if(v == dstView)
        {
            destIndex = i;
            break;
        }
    }
    assert(destIndex != INVALID_INDEX);
    return destIndex;
}
- (void) exchangeSliderViewZ
{
    //[mView sendSubviewToBack:mQualityTitle];
    //[mView sendSubviewToBack:mTimesTitle];
    /*
    NSUInteger timesTitleIndex = [self getViewIndex:mTimesTitle];
    NSUInteger qualitySliderIndex = [self getViewIndex:mQualitySlider];
    NSUInteger timesSliderIndex = [self getViewIndex:mTimesSlider];
    [mView exchangeSubviewAtIndex:timesTitleIndex withSubviewAtIndex:qualitySliderIndex];
    [mView exchangeSubviewAtIndex:qualitySliderIndex withSubviewAtIndex:timesSliderIndex];
    */
    [mView bringSubviewToFront:mQualitySlider];
    [mView bringSubviewToFront:mTimesSlider];
}
- (void )imageSizeFilterHandler: (NSNumber*) bOk
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    userInfo.imagesizefilter = [NSNumber numberWithBool:[bOk boolValue]];
    NSLog(@"image size filter = %@", userInfo.imagesizefilter);
}
- (void) brushRandomHandler:(NSNumber*) bOk
{
    [mViewNav setBrushRandom:bOk];
}
- (UIView*) getView
{
    if(mView == nil)
    {
        mView.userInteractionEnabled = YES;
        mView = [[[NSBundle mainBundle] loadNibNamed:@"Options1" owner:self options:nil] lastObject];
        mView.backgroundColor = [UIColor clearColor];
        [mView retain];
        NSLog(@"## view frame = %f, %f, %f, %f ##", mView.frame.origin.x, mView.frame.origin.y, mView.frame.size.width, mView.frame.size.height);
        UIImage* image = [mViewNav.mResLoader getImage:@"OptionsPlaySettingDrawingTitleBg"];
        //mDrawSettingTitleBg.image = [SEUtil imageWithCap:image top:0.5 bottom:0.6 left:0.5 right:0.6];
        [self setImage:mDrawSettingTitle.background name:@"OptionsPlaySettingDrawingTitleBg"];
        
        //image = [mViewNav.mResLoader getImage:@"OptionsPlaySettingDrawingSetBg"];
        //mDrawSettingBg.image = [SEUtil imageWithCap:image top: 0.1 bottom:0.9 left:0.1 right:0.9];
        [self setImage:mDrawSettingBg.imageView name:@"OptionsPlaySettingDrawingSetBg"];
        
        //image = [mViewNav.mResLoader getImage:@"OptionsPlaySettingDrawQualityNumBg"];
        //mQualityBg.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
        [self setImage:mQualityValue.background name:@"OptionsPlaySettingDrawQualityNumBg"];
        
        //image = [mViewNav.mResLoader getImage:@"OptionsPlaySettingDrawTimesNumBg"];
        //mTimesBg.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
        
        [self setImage:mTimesValue.background name:@"OptionsPlaySettingDrawTimesNumBg"];
        
        //image = [mViewNav.mResLoader getImage:@"OptionsPlaySettingDrawQualityTitleBg"];
        //mQualityTitle.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
        [self setImage:mQualityTitle.background name:@"OptionsPlaySettingDrawQualityTitleBg"];
        
        //image = [mViewNav.mResLoader getImage:@"OptionsPlaySettingDrawTimesTitleBg"];
        //mTimesTitle.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
        [self setImage:mTimesTitle.background name:@"OptionsPlaySettingDrawTimesTitleBg"];
        
        //image = [mViewNav.mResLoader getImage:@"OptionsPlaySettingWaitingTimeTitleBg"];
        //mWaitingTimeTitleBg.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
        [self setImage:mWaitingTimeTitle.background name:@"OptionsPlaySettingWaitingTimeTitleBg"];
        
        //image = [mViewNav.mResLoader getImage:@"OptionsPlaySettingWaitingTimeBg"];
        //mWaitingTimeBg.image = image;
        //[self setImage:mWaitingTime name:@"OptionsPlaySettingWaitingTimeBg"];
        [self setImage:mWaitingTimeBg.imageView name:@"OptionsPlaySettingWaitingTimeBg"];
        [self setImage:mSecondValue.background name:@"OptionsPlaySettingSecondBg"];
        [self setImage:mSecondLabel.background name:@"OptionsPlaySettingSecondTitleBg"];
        //[self setImage:mWaitingTimeContent name:@"OptionsPlaySettingWaitingContentBg"];
        
        [self setImage: mPlayModeTitle.background name:@"OptionsPlaySettingPlayModeTitleBg"];
        [self setImage: mPlayModeBg.imageView name:@"OptionsPlaySettingPlayModeBg"];
        [self setImage: mBrushTitle.background name:@"OptionsPlaySettingBrushTitleBg"];
        [self setImage: mPlaySequenceTitle.background name:@"OptionsPlaySettingPlayModeSequenceBg"];
        [self setImage:mPlayRandomTitle.background name:@"OptionsPlaySettingPlayModeRandomBg"];
        [self setImage:mPlayIntelTitle.background name:@"OptionsPlaySettingPlayModeIntelBg"];
        [self setImage:mAngleLabel.background name:@"OptionsPlaySettingPlayModeIntelBg"];
        [self setImage:mTransparentLabel.background name:@"OptionsPlaySettingPlayModeIntelBg"];
        
        [self setImage:mTransparentValue.background name:@"OptionsPlaySettingTransValueBg"];
        [self setImage:mAngleAdjustValue.background name:@"OptionsPlaySettingAngleValueBg"];
        
        
        
        [self setImage:mBrushBg name:@"OptionsPlaySettingDrawingSetBg"];
        [self setImage:mBrushSetingBg name:@"OptionsPlaySettingDrawingSetBg"];
        
        [self setImage:mImageSizeFilterTitleLabel.background name:@"OptionsPlaySettingDrawingTitleBg"];
        [self setImage:mImageSizeFilterBg.imageView name:@"OptionsPlaySettingPlayModeBg"];
        //[self setImage:mImageSizeFilterTitle.background name:@""
        [self setImage:mImageSizeFilterTitle.background name:@"OptionsSignatureSettingSigSizeTextBg"];
        
        [self setImage:mDensityLabel.background name:@"OptionsPlaySettingPlayModeIntelBg"];
        [self setImage:mEdgeDetectLabel.background name:@"OptionsPlaySettingPlayModeIntelBg"];
        
        [self setImage: mDrawBrushDefaultTitle.background name:@"OptionsPlaySettingPlayModeIntelBg"];
        [self setImage: mDrawBrushEveryImageTitle.background name:@"OptionsPlaySettingPlayModeIntelBg"];
        [self setImage:mDrawBrushEveryTimeTitle.background name:@"OptionsPlaySettingPlayModeIntelBg"];
        
        [self setImage:mDensityNum.background name:@"OptionsPlaySettingAngleValueBg"];
        [self setImage:mEdgeDetectValue.background name:@"OptionsPlaySettingAngleValueBg"];
        [self setImage:mRotateLockTitle.background name:@"OptionsSignatureSettingSigSizeTextBg"];
        
        //[mView bringSubviewToFront:mImageSizeFilterTitle];
        //[mView bringSubviewToFront:mImageSizeFilterSwitch];
        UserInfo* userInfo = [mViewNav getUserInfo];
        [mImageSizeFilterSwitch initImageView:mViewNav];
        [mImageSizeFilterSwitch setTarget:self action:@selector(imageSizeFilterHandler:)];
        NSLog(@"ImageSizeSwitch size = %f, %f", mImageSizeFilterSwitch.frame.size.width, mImageSizeFilterSwitch.frame.size.height);
        NSLog(@"filter switch parent = %@", mImageSizeFilterSwitch.superview);
        [mRotateLockSwitch initImageView:mViewNav];
        [mRotateLockSwitch setTarget:self action:@selector(rotateLockSwitchHandler:)];
        NSArray* playModeViews = [NSArray arrayWithObjects:mPlaySequence, mPlayRandom, mPlayIntelligence, nil];
        NSArray* playModeValues = [NSArray arrayWithObjects:[NSNumber numberWithInt:SEQUENCE], [NSNumber numberWithInt:RANDOM], [NSNumber numberWithInt:INTEL], nil];
        mPlayMode = (PLAY_MODE)[userInfo.imageplaymode intValue];
        mPlayModeRadioButton = [SEOptionsRadioButton createRadioButton:playModeViews values:playModeValues normal:@"OptionsPlaySettingPlayModeSequenceNoOK" selected:@"OptionsPlaySettingPlayModeSequenceOK" currentValue:mPlayMode];
        [mPlayModeRadioButton retain];
        [mPlayModeRadioButton setValueChangeHandler:self action:@selector(handlePlayModeChange:)];
        
        int brushMode = [userInfo.drawbrushmode intValue];
        NSArray* drawBrushModeViews = [NSArray arrayWithObjects: mDrawBrushDefault, mDrawBrushEveryImage, mDrawBrushEveryTime, nil];
        NSArray* drawBrushModeValues = [NSArray arrayWithObjects: [NSNumber numberWithInt:USE_SELECT], [NSNumber numberWithInt:EVERY_IMAGE], [NSNumber numberWithInt:EVERY_TIME], nil];
        mDrawBrushModeRadioButton = [SEOptionsRadioButton createRadioButton:drawBrushModeViews values:drawBrushModeValues normal:@"OptionsPlaySettingPlayModeSequenceNoOK"  selected:@"OptionsPlaySettingPlayModeSequenceOK" currentValue:brushMode];
        [mDrawBrushModeRadioButton retain];
        [mDrawBrushModeRadioButton setValueChangeHandler:self action:@selector(handleDrawBrushModeChange:)];
        
       // mPlaySequence.userInteractionEnabled = YES;
       // mPlaySequence.imageView.userInteractionEnabled = YES;
       // mPlayRandom.userInteractionEnabled = YES;
       // mPlayRandom.imageView.userInteractionEnabled = YES;
       // mPlayIntelligence.userInteractionEnabled = YES;
       // mPlayIntelligence.imageView.userInteractionEnabled = YES;
       // UITapGestureRecognizer* ges1 = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(handlePlayModeChange:)];
      //  UITapGestureRecognizer* ges2 = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(handlePlayModeChange:)];
      //  UITapGestureRecognizer* ges3 = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(handlePlayModeChange:)];
       // NSLog(@"sequence parent = %@", mPlaySequence.superview);
        //[mPlaySequence addGestureRecognizer:ges1];
        //[mPlayRandom addGestureRecognizer:ges2];
        //[mPlayIntelligence addGestureRecognizer:ges3];
        //[ges1 release];
        //[ges2 release];
        //[ges3 release];
        
        if([userInfo.imagesizefilter boolValue] == YES)
        {
            [mImageSizeFilterSwitch setOn:YES animated:NO];
        }
        else
        {
            [mImageSizeFilterSwitch setOn:NO animated:NO];
        }
        if([userInfo.rotatescreen boolValue] == YES)
        {
            [mRotateLockSwitch setOn:YES animated:NO];
        }
        else
        {
            [mRotateLockSwitch setOn:NO animated:NO];    
        }
        [mBrushScrollView initView:mViewNav];
        [self setLabel];
        [self exchangeSliderViewZ];
    }
    return mView;
}
- (void) setPlayModeImage
{
    
    SEOptionsImageView* images[] = {mPlaySequence, mPlayRandom, mPlayIntelligence};
    for(int i = 0 ; i < 3 ; i++)
    {
        [self setImage:images[i].imageView name:gPlayModeNoOK[i]];
    }
    [self setImage:images[mPlayMode].imageView name:gPlayModeOK[mPlayMode]];

}
- (int) createViewTagForBrush: (int) brushID
{
    return BRUSH_CHILD_VIEW_START_TAG + brushID;
}
- (int) getBrushIDFromViewTag: (int)tag
{
    return tag - BRUSH_CHILD_VIEW_START_TAG;
}
- (void) brushImageViewHandler: (UITapGestureRecognizer*) tap
{
    
    SESelectScrollChildView* v = (SESelectScrollChildView*)tap.view;
    if(v.isLocked == NO)
    {
        NSLog(@"## selected brush is %d ##", v.tag);
        [mBrushScrollView setSelected: v];
        int brushID = [self getBrushIDFromViewTag: v.tag];
        //[[PainterManager painterManager] setCurrentBrushID:brushID];
        UserInfo* userInfo = [mViewNav getUserInfo];
        mViewNav.mSystemDataManager.currentbrushid = [NSNumber numberWithInt:brushID];
    }
}
/*
- (void) update
{
    PainterManager* pm = [PainterManager painterManager];
    NSArray* allBrushID = [pm getAllBrushID];
    NSMutableArray* brushDefineArray = [NSMutableArray array];
    for(NSNumber* brushID in allBrushID)
    {
        int i = [brushID intValue];
        NSArray* brushes = [pm getBrushesById:i];
        BrushDefine* bd = [pm getBrushDefine:i];
        if(brushes == nil || brushes.count == 0)
            continue;
        [brushDefineArray addObject:bd];
    }
    NSArray* currentLevelBrushIDArray = [[mViewNav getUserUpgrade] getAllBrushIDByUserCurrentLevel];
    NSArray* currentAchieveBrushIDArray = [[mViewNav getUserUpgrade] getAllBrushIDByUserCurrentAchieve];

    BrushDefine* (^getBrushDefine)(NSArray*,NSString*) = ^BrushDefine*(NSArray* bdArray, NSString* brushName)
    {
        for(int i = 0 ; i < bdArray.count ; i++)
        {
            BrushDefine* bd = [bdArray objectAtIndex:i];
            if([bd.brushOutName isEqualToString:brushName])
                return bd;
        }
        return nil;
    };
    for(int i = 0 ; i < mBrushScrollView.mContentParent.subviews.count; i++)
    {
        UIView* v = [mBrushScrollView.mContentParent.subviews objectAtIndex:i];
        if([v isMemberOfClass:[SESelectScrollChildView class]])
        {
            SESelectScrollChildView* imageView = (SESelectScrollChildView*)v;
            NSString* name = imageView.mText.text;
            BrushDefine* bd = getBrushDefine(brushDefineArray, name);
            if(bd)
            {
                switch (bd.brushGettingWay)
                {
                    case ITEM_DEFAULT:
                    {
                        [imageView setLock:NO lockType:LOCK_BUY];
                    }
                        break;
                    case ITEM_LEVELUP:
                    {
                        NSUInteger index = [currentLevelBrushIDArray indexOfObjectPassingTest:^BOOL(id obj, NSUInteger idx, BOOL *stop) 
                                            {
                                                NSNumber* num = (NSNumber*)obj;
                                                *stop = NO;
                                                if([num intValue] == bd.brushID)
                                                {
                                                    return YES;
                                                }
                                                else 
                                                {
                                                    return NO;
                                                }
                                                
                                            }];
                        if(index == NSNotFound)
                        {
                            [imageView setLock:YES lockType:LOCK_LEVELUP];
                        }
                        else 
                        {
                            [imageView setLock:NO lockType:LOCK_LEVELUP];
                        }
                    }
                        break;
                    case ITEM_ARCHIEVE:
                    {
                        NSUInteger index = [currentAchieveBrushIDArray indexOfObjectPassingTest:^BOOL(id obj, NSUInteger idx, BOOL *stop) 
                                            {
                                                NSNumber* num = (NSNumber*)obj;
                                                *stop = NO;
                                                if([num intValue] == bd.brushID)
                                                {
                                                    return YES;
                                                }
                                                else 
                                                {
                                                    return NO;
                                                }
                                                
                                            }];
                        if(index == NSNotFound)
                        {
                            [imageView setLock:YES lockType:LOCK_ACHIEVE];
                        }
                        else {
                            [imageView setLock:NO lockType:LOCK_ACHIEVE];
                        }
                    }
                        
                        break;
                    case ITEM_BUY:
                    {
                        NSString* brushOutName = bd.brushOutName;
                        NSString* productId = [[PhotoFrameAppDelegate getProductManager] getProductIdByBrushOutName:brushOutName];
                        NSString* discountId = [[PhotoFrameAppDelegate getProductManager] getDiscountIdByBrushOutName:brushOutName];
                        BOOL isBuied = [SEUserDefaultManager isProductBuied:productId] || [SEUserDefaultManager isProductBuied:discountId];
                        if(isBuied == NO)
                        {
                            [imageView setLock:YES lockType:LOCK_BUY];
                        }
                        else
                        {
                            [imageView setLock:NO lockType:LOCK_BUY];    
                        }
                    }
                        break;
                    default:
                        break;
                }

            }
        }
            
    }
}
 */
- (void) setBrushData
{
    NSLog(@"##########\n");
    //[self testData];
    NSLog(@"##########\n");
    PainterManager* pm = [PainterManager painterManager];
    NSArray* allBrushID = [pm getAllBrushID];
    NSMutableArray* imageArray = [NSMutableArray array];
    NSMutableArray* brushDefineArray = [NSMutableArray array];
    //for test
    for(int i = 0 ; i < allBrushID.count; i++)
    {
        int num = [[allBrushID objectAtIndex:i] intValue];
        //NSLog(@"brush id = %d", num);
    }
    //end
    for(NSNumber* brushID in allBrushID)
    {
        int i = [brushID intValue];
        NSArray* brushes = [pm getBrushesById:i];
        BrushDefine* bd = [pm getBrushDefine:i];
        if(brushes == nil || brushes.count == 0)
            continue;
        NSMutableArray* data = [NSMutableArray array];
        [brushDefineArray addObject:bd];
        [data addObject:[NSNumber numberWithInt:i]];
        NSString* first = [brushes objectAtIndex:0];
        //NSLog(@"first brush = %@", first);
        const char* str = [first cStringUsingEncoding:NSASCIIStringEncoding];
        //NSLog(@"## brush name len = %lu ##", strlen(str));
        ppm_t brush = {0, 0, NULL};
        ppm_load(str, &brush);
        CGImageRef imageRef = [SEUtil create32BitsCGimageWithCopy:brush];//[SEUtil createCGImage:brush];
        UIImage* image = [UIImage imageWithCGImage:imageRef];
        CGImageRelease(imageRef);
        [data addObject:image];
        [imageArray addObject:data];
        ppm_kill(&brush);
    }
    [mBrushScrollView clearAllChild];
    mBrushScrollView.backgroundColor = [UIColor clearColor];
    UserInfo* userInfo = [mViewNav getUserInfo];
    int currentID = [mViewNav.mSystemDataManager.currentbrushid intValue];//[[PainterManager painterManager] currentBrushID];
    SESelectScrollChildView* currentSelectedView = nil;
    NSArray* currentLevelBrushIDArray = [[mViewNav getUserUpgrade] getAllBrushIDByUserCurrentLevel];
    NSArray* currentAchieveBrushIDArray = [[mViewNav getUserUpgrade] getAllBrushIDByUserCurrentAchieve];
    //for test
    for(int i = 0 ; i < currentLevelBrushIDArray.count ; i++)
    {
        NSNumber* num = [currentLevelBrushIDArray objectAtIndex:i];
        NSLog(@"level up brush id = %d", [num intValue]);
    }
    for(int i = 0 ; i < currentAchieveBrushIDArray.count ; i++)
    {
        NSNumber* num = [currentAchieveBrushIDArray objectAtIndex:i];
        NSLog(@"achievement brush id = %d", [num intValue]);
    }
    //end
    for(int i = 0 ; i < imageArray.count ; i++)
    {
        NSArray* data = [imageArray objectAtIndex:i];
        BrushDefine* bd = [brushDefineArray objectAtIndex:i];
        int brushID = [[data objectAtIndex:0] intValue];
        if(i == 5)
        {
            NSLog(@"brush ID = %d", brushID);
        }
        UIImage* image = [data objectAtIndex:1];
        SESelectScrollChildView* imageView = [mBrushScrollView addChildItem:image viewNav:mViewNav text:bd.brushOutName];
        imageView.tag = [self createViewTagForBrush: brushID];
        imageView.userInteractionEnabled = YES;
        //imageView.backgroundColor = [UIColor redColor];
        //imageView.clipsToBounds = YES;
        [imageView setNeedsDisplay];
        if(currentID != brushID)
        {
            [imageView hideShell:YES];
        }
        else 
        {
            [imageView hideShell:NO];
            currentSelectedView = imageView;
        }
        switch (bd.brushGettingWay)
        {
            case ITEM_DEFAULT:
            {
                [imageView setLock:NO lockType:LOCK_BUY];
            }
                break;
            case ITEM_LEVELUP:
            {
                NSUInteger index = [currentLevelBrushIDArray indexOfObjectPassingTest:^BOOL(id obj, NSUInteger idx, BOOL *stop) 
                {
                    NSNumber* num = (NSNumber*)obj;
                    *stop = NO;
                    if([num intValue] == brushID)
                    {
                        return YES;
                    }
                    else 
                    {
                        return NO;
                    }
                    
                }];
                if(index == NSNotFound)
                {
                    if(brushID == 6)
                    {
                        NSLog(@"brushID = 6");
                    }
                    [imageView setLock:YES lockType:LOCK_LEVELUP];
                }
                else 
                {
                    NSLog(@"level up %d, brushID = %d", i, brushID);
                    [imageView setLock:NO lockType:LOCK_LEVELUP];
                }
            }
                break;
            case ITEM_ARCHIEVE:
            {
                NSUInteger index = [currentAchieveBrushIDArray indexOfObjectPassingTest:^BOOL(id obj, NSUInteger idx, BOOL *stop) 
                                    {
                                        NSNumber* num = (NSNumber*)obj;
                                        *stop = NO;
                                        if([num intValue] == brushID)
                                        {
                                            return YES;
                                        }
                                        else 
                                        {
                                            return NO;
                                        }
                                        
                                    }];
                if(index == NSNotFound)
                {
                    if(brushID == 6)
                    {
                        NSLog(@"brushID = 6 ");
                    }
                    if(brushID == 7)
                    {
                        NSLog(@"brushID = 7 ");
                    }
                    [imageView setLock:YES lockType:LOCK_ACHIEVE];
                }
                else 
                {
                    NSLog(@"achieve %d, brushID = %d", i, brushID);
                    [imageView setLock:NO lockType:LOCK_ACHIEVE];
                    //imageView.mLockImageView.backgroundColor = [UIColor blueColor];
                    //imageView.mLockBgView.backgroundColor = [UIColor yellowColor];
                    //imageView.image = nil;
                }
            }
                
                break;
            case ITEM_BUY:
            {
                NSString* brushOutName = bd.brushOutName;
                NSString* productId = [[PhotoFrameAppDelegate getProductManager] getProductIdByBrushOutName:brushOutName];
                NSString* discountId = [[PhotoFrameAppDelegate getProductManager] getDiscountIdByBrushOutName:brushOutName];
                BOOL isBuied = [SEUserDefaultManager isProductBuied:productId] || [SEUserDefaultManager isProductBuied:discountId];
                if(isBuied == NO)
                {
                    [imageView setLock:YES lockType:LOCK_BUY];
                }
                else
                {
                    [imageView setLock:NO lockType:LOCK_BUY];    
                }
            }
                break;
            default:
                break;
        }
        UITapGestureRecognizer* tap = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(brushImageViewHandler:)];
        [imageView addGestureRecognizer:tap];
        [tap release];
    }
    mBrushScrollView.userInteractionEnabled = YES;
    [mBrushScrollView setContentSize];
    if(currentSelectedView)
    {
        [mBrushScrollView.mScrollView setContentOffset:CGPointMake(currentSelectedView.frame.origin.x, currentSelectedView.frame.origin.y) animated:NO];
    }
}
- (void) setLockView
{
    //for test
    //[mLockViewForDrawing setLock:NO type:LOCK_BUY];
    //[mLockViewForBrush setLock:NO type:LOCK_BUY];
    //return;
    //end
    if([SEUserDefaultManager isFunctionOK:DRAWING_BASIC_FUNC] == NO)
    {
        if(mLockViewForDrawing == nil)
        {
            mLockViewForDrawing = [[SEOptionsLockView alloc] initWithFrame:mDrawSettingBg.frame lockSize:CGSizeMake(20, 100)];
            [mLockViewForDrawing setText:@"You should buy Basic Setting firstly"];
            [mView addSubview:mLockViewForDrawing];
            [mLockViewForDrawing release];
        }
        [mLockViewForDrawing setLock:YES type:LOCK_BUY];
        //[mLockViewForBrush setLock:YES type:LOCK_BUY];
    }
    else
    {
        [mLockViewForDrawing setLock:NO type:LOCK_BUY];
    }
    //NSString* brushSettingProductId = 
    //[SEUserDefaultManager isProductBuied:brushSettingProductId];
    BOOL isBrushSettingBuy = [SEUserDefaultManager isFunctionBuied:BRUSH_SETTING_FUNC];
    if(isBrushSettingBuy == NO)
    {
        if(mLockViewForBrush == nil)
        {
            CGRect rect = CGRectMake(mBrushBg.frame.origin.x, mBrushBg.frame.origin.y, mBrushBg.frame.size.width, mBrushBg.frame.size.height);
            mLockViewForBrush = [[SEOptionsLockView alloc] initWithFrame:rect lockSize:CGSizeMake(20, 100)];
            [mLockViewForBrush setText:@"You should buy Brush Setting firstly"];
            [mView addSubview:mLockViewForBrush];
            [mLockViewForBrush release];
        }
        [mLockViewForBrush setLock:YES type:LOCK_BUY];
    }
    else 
    {
        [mLockViewForBrush setLock:NO type:LOCK_BUY];
    }
    /*
    if([SEUserDefaultManager isFunctionOK:BRUSH_SELECT_FUNC] == NO)
    {
        if(mLockViewForBrushSetting == nil)
        {
            CGRect rect = CGRectMake(mBrushBg.frame.origin.x, mBrushBg.frame.origin.y + mTransparentLabel.frame.origin.y - 2, mBrushBg.frame.size.width, mEdgeDetectSlider.frame.origin.y + mEdgeDetectSlider.frame.size.height - mTransparentLabel.frame.origin.y + 12 + 4);
            mLockViewForBrushSetting = [[SEOptionsLockView alloc] initWithFrame:rect lockSize:CGSizeMake(20, 100)];
            [mLockViewForBrushSetting setText:@"You should buy Brush Setting firstly"];
            [mView addSubview:mLockViewForBrushSetting];
            [mLockViewForBrushSetting release];
        }
        [mLockViewForBrushSetting setLock:NO type:LOCK_BUY];
    }
    else 
    {
        [mLockViewForBrushSetting setLock:NO type:LOCK_BUY];
    }
     */
    //for test
    //[mLockViewForBrushSetting setLock:NO type:LOCK_BUY];
    //end
}
- (void) densitySliderHandler: (UISlider*)slider
{
    NSLog(@"density slider handler");
    int v = (int)slider.value;
    UserInfo* userInfo = [mViewNav getUserInfo];
    if(v != [mViewNav.mSystemDataManager.brushdensity intValue])
    {
        mViewNav.mSystemDataManager.brushdensity = [NSNumber numberWithInt:v];
        mDensityNum.label.text = [NSString stringWithFormat:@"%d", v];
    }
}
- (void) edgeDetectSliderHandler: (UISlider*)slider
{
    NSLog(@"edge detect slider handler");
    int v = (int)slider.value;
    UserInfo* userInfo = [mViewNav getUserInfo];
    if(v != [mViewNav.mSystemDataManager.brushedgedetect intValue])
    {
        mViewNav.mSystemDataManager.brushedgedetect = [NSNumber numberWithInt:v];
        mEdgeDetectValue.label.text = [NSString stringWithFormat:@"%d", v];
    }
}
- (void) setData
{
    PainterManager* pm = [PainterManager painterManager];
    self.mCurrentTimes = [NSString stringWithFormat:@"%d", [mViewNav getImageTimes]];//[NSString stringWithFormat:@"%d", pm.painterProperty.times];
    self.mCurrentQuality = [NSString stringWithFormat:@"%d", [mViewNav getImageQuality]];//[NSString stringWithFormat:@"%d", pm.painterProperty.percent];
    mQualitySlider.slider.minimumValue = 4;
    mQualitySlider.slider.maximumValue = 10;
    
    int maxTimes, minTimes;
    [pm getMinMaxTimesValue: [mViewNav getImageQuality] outMin:&minTimes outMax:&maxTimes];
    mTimesSlider.slider.minimumValue = minTimes;
    mTimesSlider.slider.maximumValue = maxTimes;
    int percent = [mViewNav getImageQuality];//pm.painterProperty.percent;
    int times = [mViewNav getImageTimes];//pm.painterProperty.times;
    [mQualitySlider.slider setValue:percent];
    [mTimesSlider.slider setValue:times];

    mQualityValue.label.text = [self.mCurrentQuality stringByAppendingFormat:@"%d%@", 0, @"%"];
    mTimesValue.label.text = self.mCurrentTimes;
    setUISliderBg(mQualitySlider.slider, mViewNav);
    setUISliderBg(mTimesSlider.slider, mViewNav);
    setUISliderBg(mWaitingTimeSlider.slider, mViewNav);
    setUISliderBg(mAngleAdjust.slider, mViewNav);
    setUISliderBg(mTransparentAdjust.slider, mViewNav);
    
    //[mView bringSubviewToFront:mAngleAdjust];
    [mQualitySlider.slider addTarget:self action:@selector(qualitySliderHandler:) forControlEvents:UIControlEventValueChanged];
    [mTimesSlider.slider addTarget:self action:@selector(timesSliderHandler:) forControlEvents:UIControlEventValueChanged];
    [mWaitingTimeSlider.slider addTarget:self action:@selector(waitingTimeSliderHandler:) forControlEvents:UIControlEventValueChanged];
    [mAngleAdjust.slider addTarget:self action:@selector(angleAdjustHandler:) forControlEvents:UIControlEventValueChanged];
    [mTransparentAdjust.slider addTarget:self action:@selector(transparentAdjustHandler:) forControlEvents:UIControlEventValueChanged];
    mWaitingTimeSlider.slider.minimumValue = 5;
    mWaitingTimeSlider.slider.maximumValue = 120;
    UserInfo* userInfo = [mViewNav getUserInfo];
    ///////////
    mPlayMode = (PLAY_MODE)[userInfo.imageplaymode intValue];
    [self setPlayModeImage];
    mSecond = [userInfo.imageplaywaitingtime intValue];
    if(mSecond == 0)
        mSecond = 5;
    mSecondValue.label.text = [NSString stringWithFormat:@"%d", mSecond];
    mWaitingTimeSlider.slider.value = mSecond;
    [self setSampleImageWithQuality:percent withResLoader:mViewNav.mResLoader];
    [self setBrushData];
    
    mAngleAdjust.slider.minimumValue = 0;
    mAngleAdjust.slider.maximumValue = 60;
    int angle = [mViewNav.mSystemDataManager.currentangle intValue];
    mAngleAdjustValue.label.text = [NSString stringWithFormat:@"%d", angle];
    [mAngleAdjust.slider setValue:angle];
    
    mTransparentAdjust.slider.minimumValue = [SESystemConfig getMinBrushTransparentValue];
    mTransparentAdjust.slider.maximumValue = [SESystemConfig getMaxBrushTransparentValue];
    int transparent = [mViewNav.mSystemDataManager.currentbrushtransparent intValue];
    mTransparentValue.label.text = [NSString stringWithFormat:@"%d", transparent];
    [mTransparentAdjust.slider setValue:transparent];
    [self setLockView];
    
    setUISliderBg(mDensitySlider.slider, mViewNav);
    setUISliderBg(mEdgeDetectSlider.slider, mViewNav);
    mDensitySlider.slider.minimumValue = [SESystemConfig getMinDensityValue];
    mDensitySlider.slider.maximumValue = [SESystemConfig getMaxDensityValue];;
    
    mEdgeDetectSlider.slider.minimumValue = [SESystemConfig getMinEdgeDetectValue];
    mEdgeDetectSlider.slider.maximumValue = [SESystemConfig getMaxEdgeDetectValue];
    int densityValue = [mViewNav.mSystemDataManager.brushdensity intValue];
    [mDensitySlider.slider setValue:densityValue];
    
    int edgeDetectValue = [mViewNav.mSystemDataManager.brushedgedetect intValue];
    [mEdgeDetectSlider.slider setValue:edgeDetectValue];
    
    [mDensitySlider.slider addTarget:self action:@selector(densitySliderHandler:) forControlEvents:UIControlEventValueChanged];
    [mEdgeDetectSlider.slider addTarget:self action:@selector(edgeDetectSliderHandler:) forControlEvents:UIControlEventValueChanged];
}
- (IBAction)transparentAdjustHandler:(UISlider*)sender
{
    NSLog(@"transparent adjust");
    int v = (int)sender.value;
    UserInfo* userInfo = [mViewNav getUserInfo];
    int t = [mViewNav.mSystemDataManager.currentbrushtransparent intValue];
    if(v != t)
    {
        mViewNav.mSystemDataManager.currentbrushtransparent = [NSNumber numberWithInt:v];
        mTransparentValue.label.text = [NSString stringWithFormat:@"%d", v];
    }
}
- (IBAction) angleAdjustHandler:(UISlider*)sender
{
    NSLog(@"angle adjust");
    int v = (int)sender.value;
    UserInfo* userInfo = [mViewNav getUserInfo];
    int angle = [mViewNav.mSystemDataManager.currentangle intValue];
    if(v != angle)
    {
        mViewNav.mSystemDataManager.currentangle = [NSNumber numberWithInt:v];
        mAngleAdjustValue.label.text = [NSString stringWithFormat:@"%d", v];
    }
}
- (IBAction) waitingTimeSliderHandler:(UISlider*)sender
{
    NSLog(@"waiting time slider");
    int v = (int)sender.value;
    UserInfo* info = [mViewNav getUserInfo];
    int old = [info.imageplaywaitingtime intValue];
    if(v != old)
    {
        info.imageplaywaitingtime = [NSNumber numberWithInt:v];
        mSecond = v;
        mSecondValue.label.text = [NSString stringWithFormat:@"%d", mSecond];
    }
}
- (void) afterWork: (NSNumber*) bMaxTimesNum;
{
    int v = [self.mCurrentQuality intValue];
    PainterManager* pm = [PainterManager painterManager];
    int minTimes, maxTimes;
    [pm getMinMaxTimesValue:(int)v outMin:&minTimes outMax:&maxTimes];
    NSLog(@"current min = %d, max = %d", minTimes, maxTimes);
    NSLog(@"min = %f, max = %f", mTimesSlider.slider.minimumValue, mTimesSlider.slider.maximumValue);
    mTimesSlider.slider.minimumValue = minTimes;
    mTimesSlider.slider.maximumValue = maxTimes;
    int times = [self.mCurrentTimes intValue];
    NSLog(@"times = %d", times);
    //BOOL bMaxTimes = [bMaxTimesNum boolValue];
    mTimesSlider.slider.value = minTimes;
    if(times < maxTimes)
        [mTimesSlider.slider setValue:times animated: NO];
    else
        [mTimesSlider.slider setValue:maxTimes animated: NO];
    [mTimesSlider.slider setNeedsDisplay];
}
- (IBAction)qualitySliderHandler:(UISlider*)sender
{
    NSLog(@"quality slider handler");
    int v = (int)sender.value;
    if(v != [self.mCurrentQuality intValue])
    {
        int minTimes, maxTimes;
        PainterManager* pm = [PainterManager painterManager];
        [pm getMinMaxTimesValue:(int)v outMin:&minTimes outMax:&maxTimes];
        //mTimesSlider.slider.minimumValue = minTimes;
        //mTimesSlider.slider.maximumValue = maxTimes;
        NSLog(@"time slider min = %d, max = %d", minTimes, maxTimes);
        NSLog(@"current time s = %@", self.mCurrentTimes);
        if([self.mCurrentTimes intValue] <= maxTimes)
        {
            //[mTimesSlider.slider setValue:[self.mCurrentTimes intValue] animated: YES];
            mTimesValue.label.text = self.mCurrentTimes;
            [mViewNav setImageTimes:[self.mCurrentTimes intValue]];
            [self performSelectorOnMainThread:@selector(afterWork:) withObject:[NSNumber numberWithBool:NO] waitUntilDone:NO];
        }
        else 
        {
            //[mTimesSlider.slider setValue:maxTimes];
            mTimesValue.label.text = [NSString stringWithFormat:@"%d", maxTimes];
            [mViewNav setImageTimes:maxTimes];
            [self performSelectorOnMainThread:@selector(afterWork:) withObject:[NSNumber numberWithBool:YES] waitUntilDone:NO];
        }
        [self setSampleImageWithQuality:v withResLoader:mViewNav.mResLoader];
        self.mCurrentQuality = [NSString stringWithFormat:@"%d", v];    
        [mViewNav setImageQuality:[self.mCurrentQuality intValue]];
        mQualityValue.label.text = [self.mCurrentQuality stringByAppendingFormat:@"%d%s", 0, "%"];
        //[mTimesSlider.slider setNeedsDisplay];
    }

}
- (IBAction)timesSliderHandler:(UISlider*)sender
{
    NSLog(@"times slider");
    int v = (int)sender.value;
    if(v != [self.mCurrentTimes intValue])
    {
        self.mCurrentTimes = [NSString stringWithFormat:@"%d",v];
        [mViewNav setImageTimes:[self.mCurrentTimes intValue]];
        mTimesValue.label.text = self.mCurrentTimes;
    }
    
}

@end

//////
@implementation SEOptionsWidgetSetting
@synthesize mTimeLabel;
@synthesize  mTimeViewTitle;
@synthesize  mTimeViewDisplayBg;
@synthesize  mTimeDisplaySwitch;
@synthesize  mTimeStyleSelectScrollView;
@synthesize  mPowerTitle;
@synthesize  mPowerViewTitle;
@synthesize  mBigIconTitle;
@synthesize  mPowerViewSwitch;
@synthesize  mBigIconSwitch;
@synthesize mPowerBg;

@synthesize mSignatureBg;
@synthesize mSigSizeSlider;
@synthesize mSigViewSwitch;
@synthesize mSignatureTitle;
@synthesize mSignatureSizeText;
@synthesize mSignatureViewText;
@synthesize mAutoColorText;
@synthesize mAutoColorSwitch;
@synthesize mDrawView;
@synthesize mSignatureSizeResult;
@synthesize  mPreviewBg;
@synthesize mSignatureEditButton;
@synthesize mPowerViewSize;
@synthesize mPowerViewSizeSlider;
- (void) dealloc
{
    [mPowerBg release];
    [mTimeLabel release];
      [mTimeViewTitle release];
      [mTimeViewDisplayBg release];
      [mTimeDisplaySwitch release];
      [mTimeStyleSelectScrollView release];
      [mPowerTitle release];
      [mPowerViewTitle release];
      [mBigIconTitle release];
      [mPowerViewSwitch release]; 
      [mBigIconSwitch release];
    
    [mSignatureBg release]; 
    [mSigSizeSlider release];
    [mSigViewSwitch release];
    [mSignatureTitle release];
    [mSignatureSizeText release];
    [mSignatureSizeResult release];
    [mSignatureViewText release];
    [mAutoColorText release];
    [mAutoColorSwitch release];
    [mDrawView release];
    //[mSignatureSizeResult release];
    [mPreviewBg release];
    [mPowerViewSize release];
    [mPowerViewSizeSlider release];
    [super dealloc];
}
- (void) setLock
{
    if([SEUserDefaultManager isFunctionOK:DRAWING_BASIC_FUNC] == NO)
    {
        if(mLockViewForSignatureEdit == nil)
        {
            mLockViewForSignatureEdit = [[SEOptionsLockView alloc] initWithFrame:mSignatureEditButton.frame lockSize:CGSizeMake(60, 60)];
            [mLockViewForSignatureEdit setText:@"You should buy Basic Setting firstly" lines:2];
            [mLockViewForSignatureEdit setLockStartPoint:CGPointMake(5, 5)];
            [mView addSubview:mLockViewForSignatureEdit];
            [mLockViewForSignatureEdit release];
        }
        [mLockViewForSignatureEdit setLock:YES type:LOCK_BUY];
    }
    else
    {
        [mLockViewForSignatureEdit setLock:NO type:LOCK_BUY];
    }
}
- (void) setLabelFont
{
    NSString* fontName = [SEUtil getFontName];
    float fontSize = 30;
    setLabelFont(mTimeLabel.label, @"Time", getLabelColor(), fontName, fontSize);
    setLabelFont(mTimeViewTitle.label, @"Clock View", getViewColor(), fontName, fontSize);
    //setLabelFont(mSleepTitleLabel, @"Brightness Setting", getLabelColor() , fontName, fontSize);
    //setLabelFont(mSleepTextLabel, @"Light View", [UIColor blackColor], fontName, fontSize);
    //setLabelFont(mAutocontrolTextLabel, @"Auto Control", [UIColor blackColor], fontName, fontSize);
    setLabelFont(mPowerTitle.label, @"Power", getLabelColor(), fontName, fontSize);
    setLabelFont(mPowerViewTitle.label, @"Power View", getViewColor() , fontName, fontSize);
    setLabelFont(mBigIconTitle.label, @"Icon Size", getViewColor(), fontName, fontSize);
    
    setLabelFont(mSignatureTitle.label,@"Signature", getLabelColor(), fontName, fontSize);
    setLabelFont(mSignatureViewText.label, @"SIG View", getViewColor(), fontName, fontSize);
    setLabelFont(mAutoColorText.label , @"Auto Color", getViewColor(), fontName, fontSize);
    setLabelFont(mSignatureSizeText.label, @"SIG Size", getViewColor(), fontName, fontSize);
    setLabelFont(mPowerViewSize.label, @"1", getViewColor(), fontName, fontSize);
    mPowerViewSize.label.textAlignment = UITextAlignmentCenter;
}
- (IBAction)brightnessHandler:(UISlider*)sender
{
    int version = [SEUtil getSystemVersion];
    if(version <= 4)
        return;
    int v = (int)sender.value;
    float m = v / 255.0f;
    UIScreen* screen = [UIScreen mainScreen];
    screen.brightness = m;
    mViewNav.mCurrentSystemBright = m;
}
- (void) powerViewSizeChangeHandler: (UISlider*)slider
{
    int v = (int) slider.value;
    mPowerViewSize.text = [NSString stringWithFormat:@"%d", v];
    UserInfo* userInfo = [mViewNav getUserInfo];
    userInfo.powerviewsize = [NSNumber numberWithInt:v];
}
- (void)signaturePreviewHandler:(UITapGestureRecognizer*)tap
{
    //[mViewNav moveToView:OPTIONS_SIGNATURE:SIGNATURE_VIEW hasAnimation:YES];
    [mViewNav setViewRelationType:TYPE1];
    [mViewNav moveToView:OPTIONS_SIGNATURE :SIGNATURE_VIEW hasAnimation:YES isPush:YES];
}
- (void) signatureButtonHandler: (UIButton*)sender
{
    NSLog(@"signatureButtonHandler");
    [mViewNav setViewRelationType:TYPE1];
    [mViewNav moveToView:OPTIONS_SIGNATURE :SIGNATURE_VIEW hasAnimation:YES isPush:YES];
}
- (void) setSignatureButton
{
    [mSignatureEditButton setButtonBackground:@"SignatureEditButtonNormal" select:@"SignatureEditButtonSelected"];
    [mSignatureEditButton setScaleTextImage:YES];
    [mSignatureEditButton setTextImage:@"edit" indicateImage:nil alignment:UITextAlignmentCenter];
    [mSignatureEditButton setButtonHandler:self action:@selector(signatureButtonHandler:)];

    //[mSignatureEditButton.button addTarget:self action:@selector(signatureButtonHandler:) forControlEvents:UIControlEventTouchUpInside];
}
- (UIView*) getView
{
    if(mView == nil)
    {
        mView = [[[NSBundle mainBundle] loadNibNamed:@"Options2" owner:self options:nil] lastObject];
        [mView retain];
        mView.backgroundColor = [UIColor clearColor];
        mView.userInteractionEnabled = YES;
        [self setImage:mTimeLabel.background name:@"OptionsWidgetSettingTimeTitleBg"];
        [self setImage:mTimeViewTitle.background name:@"OptionsWidgetSettingTimeViewTitleBg"];
        //[self setImage:mTimeViewDisplayBg.imageView name:@"OptionsWidgetSettingTimeBg"];
        [self setImage:mTimeViewDisplayBg name:@"OptionsWidgetSettingTimeBg"];
        [mTimeDisplaySwitch initImageView: mViewNav];
        [mTimeStyleSelectScrollView initView:mViewNav];
        [self setImage:mPowerTitle.background name:@"OptionsWidgetSettingPowerTitleBg"];
        [self setImage:mPowerBg name:@"OptionsWidgetSettingPowerBg"];
        [self setImage:mPowerViewTitle.background name:@"OptionsWidgetSettingPowerTextBg"];
        [self setImage:mBigIconTitle.background name:@"OptionsWidgetSettingBigIconTitleBg"];
        
        [self setImage:mSignatureTitle.background name:@"OptionsSignatureSettingTitleBg"];
        [self setImage:mSignatureBg name:@"OptionsSignatureSettingBg"];
        [self setImage:mSignatureViewText.background name:@"OptionsSignatureSettingSigViewTextBg"];
        [self setImage:mAutoColorText.background name:@"OptionsSignatureSettingAutoColorTextBg"];
        [self setImage:mSignatureSizeText.background name:@"OptionsSignatureSettingSigSizeTextBg"];
        [self setImage:mSignatureSizeResult.background name:@"OptionsPlaySettingAngleValueBg"];//@"OptionsSignatureSettingSigSizeResultTextBg"];
        [self setImage:mPowerViewSize.background name:@"OptionsPlaySettingAngleValueBg"];
        
        [self setSignatureButton];
        [mPowerViewSwitch initImageView:mViewNav];
        [mBigIconSwitch initImageView:mViewNav];
        [mPowerViewSwitch setTarget:self action:@selector(setPowerViewShow:)];
        [mTimeDisplaySwitch setTarget:self action:@selector(setTimeViewShow:)];
        [mBigIconSwitch setTarget:self action:@selector(setPowerBigIcon:)];

        [mAutoColorSwitch initImageView:mViewNav];
        [mSigViewSwitch initImageView:mViewNav];
        setUISliderBg(mSigSizeSlider, mViewNav);
        setUISliderBg(mPowerViewSizeSlider, mViewNav);
        [mPowerViewSizeSlider addTarget:self action:@selector(powerViewSizeChangeHandler:) forControlEvents:UIControlEventValueChanged];
        mPowerViewSizeSlider.minimumValue = [SESystemConfig getMinPowerViewValue];
        mPowerViewSizeSlider.maximumValue = [SESystemConfig getMaxPowerViewValue];
        
        [mSigSizeSlider addTarget:self action:@selector(signatureSizeSliderHandler:) forControlEvents:UIControlEventValueChanged];
        mSigSizeSlider.minimumValue = [SESystemConfig getMinSignatureValue];
        mSigSizeSlider.maximumValue = [SESystemConfig getMaxSignatureValue];
        [mSigViewSwitch setTarget:self action:@selector(setSignatureShow:)];
        [mAutoColorSwitch setTarget:self action:@selector(setSignatureAutoColor:)];
        UIImage* image = [mViewNav.mResLoader getImage:@"SignatureDrawBg"];
        [mDrawView initData];
        //[mDrawView setDrawInMainDisplay:YES];
        [mDrawView setStep:0.2];
        //mDrawView.mDrawInMainScreen = YES;
        mDrawView.background = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
        [mDrawView setLineWidthRatio:(mDrawView.frame.size.width / 717.0)];
        //[mDrawView setBackgroundImage:[SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9]];
        mDrawView.backgroundColor = [UIColor clearColor];
        mPreviewBg.image = nil;
        [self setLabelFont];
        [mView bringSubviewToFront:mSigSizeSlider];
    }
    return mView;
}
- (void) setPowerViewSizeText: (int)v
{
    NSString* fontName = [SESystemConfig getFontName];
    float fontSize = 30;
    NSString* str = [NSString stringWithFormat:@"%d", v];
    setLabelFont(mPowerViewSize.label, str, getViewColor(), fontName, fontSize);
}
- (void) setSignatureSizeReslutText: (int)v
{
    NSString* fontName = [SEUtil getFontName];
    float fontSize = 30;
    NSString* str = [NSString stringWithFormat:@"%d", v];
    setLabelFont(mSignatureSizeResult.label, str , getViewColor(),fontName, fontSize);
    /*
    if(v == 0)
    {
        setLabelFont(mSignatureSizeResult.label, str , [UIColor whiteColor],fontName, fontSize);
    }
    else if(v == 1)
    {
        setLabelFont(mSignatureSizeResult.label, str, [UIColor whiteColor],fontName, fontSize);
    }
    else
    {
        setLabelFont(mSignatureSizeResult.label, str, [UIColor whiteColor],fontName, fontSize);
    }
     */
    mSignatureSizeResult.label.textAlignment = UITextAlignmentCenter;
}
- (void) signatureSizeSliderHandler:(UISlider*)sender
{
    int v = (int)sender.value;
    UserInfo* userInfo = [mViewNav getUserInfo];
    
    if(v != [userInfo.signaturesize intValue])
    {
        userInfo.signaturesize = [NSNumber numberWithInt:v];
        [self setSignatureSizeReslutText:v];
    }
}
- (void) setTimeViewShow: (NSNumber*) bOk
{
    NSLog(@"time show = %@", bOk);
    UserInfo* userInfo = [mViewNav getUserInfo];
    userInfo.showtime = [NSNumber numberWithBool:[bOk boolValue]];
}
- (void) setPowerViewShow: (NSNumber*) bOk
{
    NSLog(@"power view show = %@", bOk);
    UserInfo* userInfo = [mViewNav getUserInfo];
    userInfo.showpowerview = [NSNumber numberWithBool:[bOk boolValue]];
}
- (void) setPowerBigIcon: (NSNumber*) bOk
{
    NSLog(@"big icon show = %@", bOk);
    UserInfo* userInfo = [mViewNav getUserInfo];
    if([bOk boolValue])
    {
        userInfo.powerviewsize = [NSNumber numberWithInt:1];
        //[userInfo setValue: [NSNumber numberWithInt:1] forKey:@"powerviewstyle"];
    }
    else 
    {
        userInfo.powerviewsize = [NSNumber numberWithInt:0];
        //[userInfo setValue: [NSNumber numberWithInt:0] forKey:@"powerviewstyle"];
    }
}
- (void) setLightViewShow: (NSNumber*)bOk
{
    NSLog(@"light view show = %@", bOk);
    UserInfo* userInfo = [mViewNav getUserInfo];
    userInfo.sleep = [NSNumber numberWithBool:[bOk boolValue]];
}

- (void) timeImageViewHandler:(UITapGestureRecognizer*)tap
{
    SESelectScrollChildView* selectView = (SESelectScrollChildView*)tap.view;
    if(selectView.isLocked == NO)
    {
        [mTimeStyleSelectScrollView setSelected:selectView];
        UserInfo* userInfo = [mViewNav getUserInfo];
        mViewNav.mSystemDataManager.timetextstyle = [NSNumber numberWithInt:selectView.tag];
    }
}
- (float) getHeight
{
    return mSignatureBg.frame.origin.y + mSignatureBg.frame.size.height;
}
- (UIImage*) getImageByTimeStyle: (int)style
{
    UIImage* image = [mViewNav.mFontLoader getImage:@"am" style:style size:FONT_NORMAL_SIZE];
    return image;
}
- (void) update
{
    Signature* sig = [mViewNav getCurrentSignature];
    if(sig != nil)
    {
        NSString* str = [NSString stringWithFormat:@"%d", [sig.seq intValue]];
        SignaturePointData sd = [mViewNav getSignaturePointsWithSeqName:str];
        if(sd.points != nil && sd.points.count > 0)
        {
            [mDrawView setPointColorArray:sd.colors];
            [mDrawView setNormalizePoints:sd.points];
            
        }
        else
        {
            [mDrawView clearPoints];
            [mDrawView setNeedsDisplay];
        }
    }
    else
    {
        [mDrawView clearPoints];
        [mDrawView setNeedsDisplay];
    }
}

- (void) setData
{
    //int timeSytleNum = [mViewNav.mFontLoader getFontNum];
    [mTimeStyleSelectScrollView clearAllChild];
    UserInfo* userInfo = [mViewNav getUserInfo];
    int timeStyle = [mViewNav.mSystemDataManager.timetextstyle intValue];
    NSArray* fontStyles = [mViewNav.mFontLoader getAllFontStyles];
    SESelectScrollChildView* currentTimeSytle = nil;
    NSArray* currentAchieveTimeFontArray = [[mViewNav getUserUpgrade] getAllTimeFontByCurrentAchieve];
    NSArray* currentLevelTimeFontArray = [[mViewNav getUserUpgrade] getAllTimeFontByCurrentLevel];
    //for test
    for(int i = 0 ; i < currentAchieveTimeFontArray.count ; i++)
    {
        NSNumber* num = [currentAchieveTimeFontArray objectAtIndex:i];
        NSLog(@"achieve time style = %@", num);
    }
    for(int i = 0 ; i < currentLevelTimeFontArray.count ; i++)
    {
        NSNumber* num = [currentLevelTimeFontArray objectAtIndex:i];
        NSLog(@"level up time style = %@", num);
    }
    //end
    for(int i = 0 ; i < fontStyles.count ; i++)
    {
        int style = [[fontStyles objectAtIndex:i] intValue];
        UIImage* image = [self getImageByTimeStyle:style];
        NSString* timeName = [NSString stringWithFormat:@"STYLE00%d", style];
        SESelectScrollChildView* imageView = [mTimeStyleSelectScrollView addChildItem:image viewNav:mViewNav text:timeName];
        //imageView.mText.textColor = [UIColor redColor];
        imageView.userInteractionEnabled = YES;
        [imageView hideShell:YES];
        imageView.tag = style;
        UITapGestureRecognizer* tap = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(timeImageViewHandler:)];
        [imageView addGestureRecognizer:tap];
        [tap release];
    
        if(timeStyle == style)
        {
            [imageView hideShell:NO];
            currentTimeSytle = imageView;
        }
        SETimeStyle* ts = [[PhotoFrameAppDelegate getProductManager] getTimeStyleById:style];
        assert(ts != nil);
        switch (ts.gettingWay) {
            case ITEM_DEFAULT:
            {
                [imageView setLock:NO lockType:LOCK_BUY];
            }
                break;
            case ITEM_ARCHIEVE:
            {
                NSUInteger index = [currentAchieveTimeFontArray indexOfObjectPassingTest:^BOOL(id obj, NSUInteger idx, BOOL *stop) {
                    NSNumber* num = (NSNumber*)obj;
                    *stop = NO;
                    if([num intValue] == style)
                        return YES;
                    else {
                        return NO;
                    }
                }];
                if(index == NSNotFound)
                {
                    [imageView setLock:YES  lockType:LOCK_ACHIEVE];
                }
                else 
                {
                    [imageView setLock:NO lockType:LOCK_ACHIEVE];
                }
            }
                break;
            case ITEM_LEVELUP:
            {
                NSUInteger index = [currentLevelTimeFontArray indexOfObjectPassingTest:^BOOL(id obj, NSUInteger idx, BOOL *stop) {
                    NSNumber* num = (NSNumber*)obj;
                    *stop = NO;
                    if([num intValue] == style)
                        return YES;
                    else {
                        return NO;
                    }
                }];
                if(index == NSNotFound)
                    [imageView setLock:YES lockType:LOCK_LEVELUP];
                else {
                    [imageView setLock:NO lockType:LOCK_LEVELUP];
                }
            }
                break;
            case ITEM_BUY:
            {
                NSString* productId = [[PhotoFrameAppDelegate getProductManager] getProductIdByTimeStyleOutName:ts.fontOutName];
                //Just for test
                //assert(productId != nil);
                //
                if(productId == nil)
                {
                    [imageView setLock:YES lockType:LOCK_BUY];
                }
                else
                {
                    if([SEUserDefaultManager isProductBuied:productId] == NO)
                    {
                        [imageView setLock:YES lockType:LOCK_BUY];
                    }
                    else
                    {
                        [imageView setLock:NO lockType:LOCK_BUY];
                    }
                }
            }
                break;
            default:
                break;
        }
    }
    //mBrushScrollView.mScrollView.backgroundColor = [UIColor redColor];

    mTimeStyleSelectScrollView.userInteractionEnabled = YES;
    [mTimeStyleSelectScrollView setContentSize];
    mTimeStyleSelectScrollView.mScrollView.alwaysBounceHorizontal = YES;
    if(currentTimeSytle)
    {
        [mTimeStyleSelectScrollView.mScrollView setContentOffset:CGPointMake(currentTimeSytle.frame.origin.x, currentTimeSytle.frame.origin.y) animated:NO];
    }
    //set switch data
    if([userInfo.showtime boolValue] == YES)
    {
        [mTimeDisplaySwitch setOn:YES animated:NO];
    }
    else 
    {
        [mTimeDisplaySwitch setOn:NO animated:NO];
    }
    if([userInfo.showpowerview boolValue] == YES)
    {
        [mPowerViewSwitch setOn:YES animated:NO];
    }
    else 
    {
        [mPowerViewSwitch setOn:NO animated:NO];
    }
    mPowerViewSizeSlider.value = [userInfo.powerviewsize intValue];
    [self setPowerViewSizeText:mPowerViewSizeSlider.value];
    /*
    if([userInfo.powerviewstyle intValue] == 1)
    {
        [mBigIconSwitch setOn:YES animated:NO];
    }
    else {
        [mBigIconSwitch setOn:NO animated:NO];
    }
     */
    /*
    if([userInfo.sleep boolValue])
    {
        [mSleepSwitch setOn:YES animated:NO];
    }
    else 
    {
        [mSleepSwitch setOn:NO animated:NO];
    }
     */
    
    //UITapGestureRecognizer* tap = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(signaturePreviewHandler:)];
    //[mPreviewBg addGestureRecognizer:tap];
    //[tap release];
    mPreviewBg.userInteractionEnabled = YES;
    mDrawView.userInteractionEnabled = NO;
    Signature* sig = [mViewNav getCurrentSignature];
    if(sig != nil)
    {
        NSString* str = [NSString stringWithFormat:@"%d", [sig.seq intValue]];
        int seq = [sig.seq intValue];
        SignaturePointData sd = [mViewNav getSignaturePointsWithSeqName:str];
        if(sd.points != nil && sd.points.count > 0)
        {
            [mDrawView setPointColorArray:sd.colors];
            [mDrawView setNormalizePoints:sd.points];
            
        }
    }
    if([userInfo.showsignatureview boolValue])
    {
        [mSigViewSwitch setOn:YES animated:NO];
    }
    else 
    {
        [mSigViewSwitch setOn:NO animated:NO];
    }
    if([userInfo.signatureautocolor boolValue])
    {
        [mAutoColorSwitch setOn:YES animated:NO];
    }
    else 
    {
        [mAutoColorSwitch setOn:NO animated:NO];
    }
    
    mSigSizeSlider.value = [userInfo.signaturesize intValue];
    [self setSignatureSizeReslutText:(int)mSigSizeSlider.value];
    [self setLock];
}
- (void) setSignatureShow:(NSNumber *)bOk
{
    NSLog(@"signatuer = %d", [bOk boolValue]);
    UserInfo* info = [mViewNav getUserInfo];
    info.showsignatureview = [NSNumber numberWithBool:[bOk boolValue]];
}
- (void) setSignatureAutoColor:(NSNumber *)bOk
{
    NSLog(@"signature auto color = %d", [bOk boolValue]);
    UserInfo* info = [mViewNav getUserInfo];
    info.signatureautocolor = [ NSNumber numberWithBool:[bOk boolValue]];
}

@end
/////
@interface SEOptionsUserNextGetView : UIView
{
    UIImageView* mBgView;
    UIImageView* mIconView;
    FontLabel* mGetLabel;
}
@property (nonatomic , readonly) FontLabel* mGetLabel;
- (void) setUserIcon: (UIImage*) icon;
- (void) setUserValue: (int) value;
@end
@implementation SEOptionsUserNextGetView
@synthesize mGetLabel;
- (void) setImage: (UIImageView*)imageView name: (NSString*) name
{
    UIImage* image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:name];
    imageView.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
}
- (void) createChild: (CGRect) frame
{
    mBgView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, frame.size.width, frame.size.height)];
    [self addSubview:mBgView];
    [mBgView release];
    [self setImage:mBgView name:@"OptionsUserInfoNextGetIconBg"];
    
    float iconWidth = 103;
    float iconHeight = 64;
    float vspacing = 0;
    mIconView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, iconWidth, iconHeight)];

    [self addSubview:mIconView];
    [mIconView release];
    
    float labelHeight = frame.size.height - vspacing - iconHeight;
    mGetLabel = [[FontLabel alloc] initWithFrame:CGRectMake(0, iconHeight + vspacing - 5, frame.size.width, labelHeight )];
    [self addSubview:mGetLabel];
    [mGetLabel release];
    mGetLabel.textAlignment = UITextAlignmentCenter;
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

- (void) setUserIcon: (UIImage*) icon
{
    CGSize s = mIconView.frame.size ;
    s.width -= 5;
    s.height -= 5;
    CGSize dstS = [SEUtil computeFitSize:CGSizeMake(icon.size.width, icon.size.height) toDst:s];
    UIImage* newImage = [SEUtil drawImage:icon toSize:dstS];
    mIconView.image = newImage;
    mIconView.frame = CGRectMake((s.width - dstS.width) / 2, (s.height - dstS.height) / 2 + 3, dstS.width, dstS.height);
}
- (void) setUserValue: (int) value
{
    NSString* fontName = [SEUtil getFontName];
    float fontSize = 30;
    NSString* str = [NSString stringWithFormat:@"+%d", value];
    setLabelFont(mGetLabel, str, getLabelColor(), fontName, fontSize);
}
@end
/////////////////
@interface SEOptionsUserGetView : UIView
{
    SEOptionsImageView* mUserGetIconView;
    UIImageView* mProgressBg;
    SEUIProgressView* mProgressView;
    FontLabel* mUserCurrentFutureValue;
    SEOptionsImageView* mPlusIcon;
    id mPlusIconTarget;
    SEL mPlusIconAction;
}
@property (nonatomic, readonly) SEUIProgressView* mProgressView;
- (void) setUserIcon: (UIImage*) image;
- (void) setUserValue: (int) currentVlaue future: (int)futureValue;
- (void) setPlusIconHandler: (id) target action: (SEL)action;
@end
@implementation SEOptionsUserGetView
@synthesize mProgressView;
- (void) setImage: (UIImageView*)imageView name: (NSString*) name
{
    UIImage* image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:name];
    imageView.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
}
- (void)plusIconTapHandler: (UITapGestureRecognizer*)ges
{
    NSLog(@"perform plus icon tap handler");
    [mPlusIconTarget performSelector:mPlusIconAction];
}
- (void) createChild: (CGRect)frame
{
    float paddingx = 10, paddingy = 10;
    float startx = paddingx;
    float starty = paddingy;
    float hspacing = 10;
    float userGetInfoIconWidth = 104;
    float userGetInfoIconHeight = 64;
    //float userGetInfoProgressBgWidth = 376;
    float userGetInfoProgressBgHeight = 64;
    //float userGetInfoProgressWidth = 330;
    //float userGetInfoProgressHeight = 45;
    float userGetInfoLastBgWidth = 64;
    float userGetInfoLastBgHeight = 64;
    mUserGetIconView = [[SEOptionsImageView alloc] initWithFrame:CGRectMake(startx, (self.frame.size.height - userGetInfoIconHeight) / 2, userGetInfoIconWidth, userGetInfoIconHeight)];
    [self addSubview:mUserGetIconView];
    [mUserGetIconView release];
    [self setImage: mUserGetIconView name:@"OptionsUserInfoSettingImageGetBg"];

    
    mPlusIcon = [[SEOptionsImageView alloc] initWithFrame:CGRectMake(self.frame.size.width - paddingx - userGetInfoLastBgWidth, (self.frame.size.height - userGetInfoLastBgHeight) / 2, userGetInfoLastBgWidth, userGetInfoLastBgHeight)];
    [self addSubview: mPlusIcon];
    [mPlusIcon release];
    mPlusIcon.userInteractionEnabled = YES;
    UITapGestureRecognizer* ges = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(plusIconTapHandler:)];
    [mPlusIcon addGestureRecognizer:ges];
    [ges release];
    [self setImage:mPlusIcon name:@"OptionsUserInfoSettingPlusBg"];
    UIImage* image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"OptionsUserInfoPlusIcon"];
    mPlusIcon.imageView.image = image;
    [mPlusIcon setImageViewSize:CGSizeMake(image.size.width, image.size.height)];
    float progressBgX = mUserGetIconView.frame.origin.x + mUserGetIconView.frame.size.width + hspacing;
    float progressBgWidth = self.frame.size.width - mUserGetIconView.frame.size.width - mPlusIcon.frame.size.width - hspacing - hspacing - paddingx - paddingx;
    mProgressBg = [[UIImageView alloc] initWithFrame:CGRectMake(progressBgX, (self.frame.size.height - userGetInfoProgressBgHeight) / 2, progressBgWidth, userGetInfoProgressBgHeight)];
    [self addSubview:mProgressBg];
    [mProgressBg release];
    [self setImage:mProgressBg name:@"OptionsUserInfoSettingProgressBg"];
    
    float progressBarHPadding = 10;
    float progressBarVPadding = 10;
    mProgressView = [[SEUIProgressView alloc] initWithFrame:CGRectMake(mProgressBg.frame.origin.x + progressBarHPadding + 2, mProgressBg.frame.origin.y + progressBarVPadding, mProgressBg.frame.size.width - 2 * progressBarHPadding - 4, mProgressBg.frame.size.height - 2 * progressBarVPadding)];
    [self addSubview:mProgressView];
    [mProgressView release];
    [mProgressView initData:[PhotoFrameAppDelegate getViewNavigator]];
    
    mUserCurrentFutureValue = [[FontLabel alloc] initWithFrame:mProgressView.frame];
    [self addSubview:mUserCurrentFutureValue];
    [mUserCurrentFutureValue release];
    mUserCurrentFutureValue.textAlignment = UITextAlignmentCenter;
    mPlusIcon.hidden = YES;
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
- (void) setUserIcon:(UIImage *)image
{
    CGSize s = mUserGetIconView.imageView.frame.size ;
    s.width -= 5;
    s.height -= 5;
    CGSize dstS = [SEUtil computeFitSize:CGSizeMake(image.size.width, image.size.height) toDst:s];
    UIImage* newImage = [SEUtil drawImage:image toSize:dstS];
    mUserGetIconView.imageView.image = newImage;
    mUserGetIconView.imageView.frame = CGRectMake((mUserGetIconView.frame.size.width - dstS.width) / 2, (mUserGetIconView.frame.size.height - dstS.height) / 2, dstS.width, dstS.height);
}
- (void) setUserValue:(int)currentVlaue future:(int)futureValue
{
    NSString* fontName = [SEUtil getFontName];
    float fontSize = 30;
    NSString* str = [NSString stringWithFormat:@"%d / %d", currentVlaue, futureValue];
    setLabelFont(mUserCurrentFutureValue, str, getLabelColor(), fontName, fontSize);
}
- (void) setPlusIconHandler: (id) target action: (SEL)action
{
    mPlusIconTarget = target;
    mPlusIconAction = action;
}
@end
/////////
@implementation SEOptionsUserInfoSetting
@synthesize mLevelProgessBarBg;
@synthesize  mLevelProgressView;
@synthesize  mUserGetBg;
@synthesize  mUserNextGetBg;
@synthesize  mUserNextGetScrollView;
@synthesize  mAchiveScrollView;
////
@synthesize  mLevelTitle;
@synthesize  mLevelBg;
@synthesize  mLevelTextBg;
@synthesize  mLevelValue;
@synthesize  mLevelText;
@synthesize  mLevelPlusIcon;
@synthesize  mNextLevelGetTitle;
@synthesize  mAchieveTitle;
@synthesize mLevelPointView;
- (void) dealloc
{
    [mLevelProgessBarBg release];
    [mLevelProgressView release];
    [mUserGetBg release];
    [mUserNextGetBg release];
    [mUserNextGetScrollView release];
    [mAchiveScrollView release];
    ////
    [mLevelTitle release];
    [mLevelBg release];
    [mLevelTextBg release];
    [mLevelValue release];
    [mLevelText release];
    [mLevelPlusIcon release];
    [mNextLevelGetTitle release];
    [mAchieveTitle release];
    [mLevelPointView release];

    [super dealloc];
}
- (void) setLabel
{
    NSString* fontName = [SEUtil getFontName];
    CGFloat fontSize = 18;
    setLabelFont(mLevelTitle.label, @"Level", getLabelColor(), fontName, 30);
    setLabelFont(mLevelValue, @"0", getLabelColor(), fontName, 120);
    setLabelFont(mLevelText, @"Level", getLabelColor(), fontName, 60);
    setLabelFont(mNextLevelGetTitle.label, @"Next Level Get", getViewColor(), fontName, 30);
    setLabelFont(mAchieveTitle.label, @"Achievement", getLabelColor(), fontName, 30);
    setLabelFont(mLevelPointView, @"", getLabelColor(), fontName, 30);
}
- (float) getHeight
{
    return myHeight;
}
- (void) updateAchievement
{
    
}
- (void) updateInfo
{
    [self setLockView];
    UserInfo* userInfo = [mViewNav getUserInfo];
    int level = [[mViewNav getUserUpgrade] getLevelFromExpNum:[mViewNav.mSystemDataManager.exppointnum intValue]];
    SEUserData* maxUserData = [[mViewNav getUserUpgrade] getMaxUserData];
    int nextLevel = level + 1;
    NSString* str = nil;
    if(nextLevel <= maxUserData.level)
    {
        SEUserData* currentLevelData = [[mViewNav getUserUpgrade] getUserData:level];
        SEUserData* nextLevelData = [[mViewNav getUserUpgrade] getUserData:nextLevel];
        int expNum = [mViewNav.mSystemDataManager.exppointnum intValue];
        
        str = [NSString stringWithFormat:@"%d / %d", expNum - currentLevelData.expNum, nextLevelData.expNum - currentLevelData.expNum];
        
    }
    else
    {
        //str = [NSString stringWithFormat:@"%d / %d", maxUserData.expNum, maxUserData.expNum];
        str = @"MAX";
    }
    mLevelPointView.text = str;
    mLevelValue.text = [NSString stringWithFormat:@"%d", level];
    mLevelProgressView.percent = [mViewNav getCurrentLevelPercent];
    int selectedImageNum = [mViewNav getCurrentAllSelectedImageCount ];
    int selectedMusicNum = [mViewNav getCurrentAllSelectedMusicCount];
    //SEUserData* userData = [mViewNav getUserData: level];
    int currentLevelImageNum = [mViewNav getCurrentLevelImageNum];
    int currentLevelMusicNum = [mViewNav getCurrentLevelMusicNum];
    if(selectedImageNum < currentLevelImageNum)
    {
        mUserGetProgressView[IMAGE_GET].mProgressView.percent = ((float)selectedImageNum) / currentLevelImageNum;
        
    }
    else
    {
        mUserGetProgressView[IMAGE_GET].mProgressView.percent = 1;
        //[mUserGetProgressView[IMAGE_GET] setUserValue:selectedImageNum future:userData.imageListNum];
    }
    [mUserGetProgressView[IMAGE_GET] setUserValue:selectedImageNum future:currentLevelImageNum];
    if(selectedMusicNum < currentLevelMusicNum)
    {
        mUserGetProgressView[MUSIC_GET].mProgressView.percent = ((float)selectedMusicNum) / currentLevelMusicNum;
    }
    else
    {
        mUserGetProgressView[MUSIC_GET].mProgressView.percent = 1;
    }
    [mUserGetProgressView[MUSIC_GET] setUserValue:selectedMusicNum future:currentLevelMusicNum];
    [self addNextUserGetInfo];
    for(int i = 0 ; i < USER_MEDAL_COUNT ; i++)
    {
        for(int j = COPPER_MEDAL ; j < MEDAL_LEVEL_COUNT ; j++)
        {
            float v;
            float value[4];
            if(i == FANS_MEDAL)
            {
                NSLog(@"## fans medal ##");
            }
            [mViewNav getMedalPercent:i: j: &v: value];

            NSLog(@"achievement percent = %f", v);
            if(v > 1)
                v = 1;
            if(v < 0)
                v = 0;
            if(i == FANS_MEDAL)
            {
                NSArray* valueArray = [NSArray array];
                for(int k = 0 ; k < 4 ; k++)
                {
                    valueArray = [valueArray arrayByAddingObject:[NSNumber numberWithFloat:value[k]]];
                }
                [mAchievementProcessView[i][j] setGroupPercent:valueArray];    
            }
            else
            {
                mAchievementProcessView[i][j].percent = v;    
            }
            UIImage* image1 = [mViewNav.mResLoader getImage:gAchieveBackground[i]];
            int currentMedal = [[mViewNav getUserUpgrade] getCurrentMedal:i];
            if(currentMedal >= j)
            {
                mAchievementIconBgView[i][j].image = image1;
            }
            else
            {
                mAchievementIconBgView[i][j].image = nil;    
            }
            UIImage* image = [self getImage:i medalLevel:j];
            mAchievementIconFgView[i][j].image = image;
            
            int currentPoint = 0, totalPoint = 0;
            int currentPointArray[4], totalPointArray[4];
            [[mViewNav getUserUpgrade] getMedalPointValue:i :j :&currentPoint :&totalPoint :currentPointArray: totalPointArray];
            
            if(currentPoint == totalPoint)
            {
                mAchievementPointView[i][j].text = @"Achieved";
            }
            else
            {
                mAchievementPointView[i][j].text = [NSString stringWithFormat:@"%d / %d", currentPoint, totalPoint];
            }

        }
    }
}
- (UIImage*) getImageByGetInfoType: (int) type
{
    UIImage* image = nil;
    switch (type) {
        case IMAGE_GET:
            image = [mViewNav.mResLoader getImage:@"OptionsUserInfoImageGetIcon"];
            break;
        case MUSIC_GET:
            image = [mViewNav.mResLoader getImage:@"OptionsUserInfoMusciGetIcon"];
            break;
        default:
            break;
    }
    return image;
}
- (void)moveViewByDeltay: (float) deltay
{
    float y = mUserGetBg.frame.origin.y + mUserGetBg.frame.size.height;
    for(UIView* v in mView.subviews)
    {
        if(v.frame.origin.y >= y)
        {
            v.frame = CGRectMake(v.frame.origin.x, v.frame.origin.y + deltay, v.frame.size.width, v.frame.size.height);
        }
    }
}
- (void) addImagePlusIconHandler
{
    NSLog(@"add image plus icon tap");
    [mOptionsView setCurrentBarView:APP_STORE_SETTING];
    [mOptionsView setAppStoreItemSelected:PRODUCT_ADD_IMAGE];
}
- (void) addMusicPlusIconHandler
{
    NSLog(@"add music plus icon tap");
    [mOptionsView setCurrentBarView:APP_STORE_SETTING];
    [mOptionsView setAppStoreItemSelected:PRODUCT_ADD_MUSIC];
}
- (void) addUserGetInfo
{
    float vspacing = 5;
    float paddingx = 0;
    float paddingy = 10;
    float userGetInfoViewWidth = mUserGetBg.frame.size.width - paddingx - paddingx;
    float userGetInfoViewHeight = 64;
    float starty = mUserGetBg.frame.origin.y + paddingy + 2;
    float startx = mUserGetBg.frame.origin.x + paddingx;
    for(int i = 0 ; i < USER_GET_TYPE_NUM ;i++)
    {
        SEOptionsUserGetView* getView = [[SEOptionsUserGetView alloc] initWithFrame:CGRectMake(startx, starty, userGetInfoViewWidth, userGetInfoViewHeight)];
        getView.userInteractionEnabled = YES;
        [mView addSubview:getView];
        [getView release];
        starty += getView.frame.size.height + vspacing;
        mUserGetProgressView[i] = getView;
        UIImage* image = [self getImageByGetInfoType: i];
        [getView setUserIcon:image];
        switch (i) {
            case IMAGE_GET:
            {
                [getView setPlusIconHandler:self action:@selector(addImagePlusIconHandler)];
            }
                break;
            case MUSIC_GET:
            {
                [getView setPlusIconHandler:self action:@selector(addMusicPlusIconHandler)];
            }
                break;
            default:
                break;
        }
    }
    starty += 10;
    float currentHeight = starty - mUserGetBg.frame.origin.y;
    float deltay = 0;
    deltay = currentHeight - mUserGetBg.frame.size.height;
    [self moveViewByDeltay: deltay];
    NSLog(@"user get height = %f", starty);
    mUserGetBg.frame = CGRectMake(mUserGetBg.frame.origin.x, mUserGetBg.frame.origin.y, mUserGetBg.frame.size.width, currentHeight);
}
- (void) addNextGetView: (USER_NEXT_GET_TYPE) type startx: (float) startx starty : (float) starty
{
    float bgWidth = 103;
    float bgHeight = 103;
    float iconWidth = 70;
    float iconHeight = 50;
    SEOptionsUserNextGetView* nextGetView = [[SEOptionsUserNextGetView alloc] initWithFrame:CGRectMake(startx, starty, bgWidth, bgHeight)];
    [mNextGetContentParent addSubview:nextGetView];
    [nextGetView release];
    if(type == BRUSH_NEXT_GET || type == TIMEFONT_NEXT_GET)
    {
        int num = mNextGetNum[type] > 0 ? 1 : 0;
        [nextGetView setUserValue:num];
    }
    else 
    {
        [nextGetView setUserValue:mNextGetNum[type]];
    }
    UIImage* image = nil;
    switch (type) {
        case IMAGE_NEXT_GET:
        {
            image = [mViewNav.mResLoader getImage:@"OptionsUserInfoNextImageGetIcon"];
            nextGetView.mGetLabel.textColor = [UIColor yellowColor];
            
        }
            break;
        case MUSIC_NEXT_GET:
        {
            image = [mViewNav.mResLoader getImage:@"OptionsUserInfoNextMusicGetIcon"];
            nextGetView.mGetLabel.textColor = [UIColor greenColor];
        }
            break;
        case BRUSH_NEXT_GET:
        {
            image = [mViewNav.mResLoader getImage:@"OptionsUserInfoNextBrushGetIcon"];
        }
            break;
        case TIMEFONT_NEXT_GET:
        {
            assert(mNextGetNum[TIMEFONT_NEXT_GET] > 0);
            int style = mNextGetNum[TIMEFONT_NEXT_GET];
            image = [mViewNav.mFontLoader getImage:@"am" style:style size:FONT_NORMAL_SIZE];
            //image = [mViewNav.mResLoader getImage:@"OptionsUserInfoNextTimeFontGetIcon"];

        }
            break;
        default:
            break;
    }
    [nextGetView setUserIcon:image];

}
- (UIImage*) getAchieveMedal: (int)medalType medalLevel: (int) medal gotted: (BOOL) gotted;
{
    UIImage* image = nil;
    switch (medalType) 
    {
        case NEWPERSON_MEDAL:
        {
            //int medal = [userInfo.newpersonmedal intValue];
            switch (medal) {
                case COPPER_MEDAL:
                    if(gotted)
                    {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradeNewPersonCopper"];
                    }
                    else
                    {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradeNewPersonNoCopper"];    
                    }
                    break;
                case SILVER_MEDAL:
                    if(gotted)
                    {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradeNewPersonSilver"];
                    }
                    else {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradeNewPersonNoSilver"];
                    }
                    break;
                case GOLD_MEDAL:
                    if(gotted)
                    {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradeNewPersonGold"];
                    }
                    else {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradeNewPersonNoGold"];
                    }
                    break;
                default:
                    break;
            }
        }        
            break;
        case PRESENTONDUTY_MEDAL:
        {
            //int medal = [userInfo.presentondutymedal intValue];
            switch (medal) {
                case COPPER_MEDAL:
                    if(gotted)
                    {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradePresentOnDutyCopper"];
                    }
                    else {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradePresentOnDutyNoCopper"];
                    }
                    break;
                case SILVER_MEDAL:
                    if(gotted)
                    {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradePresentOnDutySilver"];
                    }
                    else
                    {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradePresentOnDutyNoSilver"];
                        
                    }
                    break;
                case GOLD_MEDAL:
                    if(gotted)
                    {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradePresentOnDutyGold"];
                    }
                    else {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradePresentOnDutyNoGold"];
                    }
                    break;
                default:
                    break;
            }
        }
            break;
        case DRAWING_MEDAL:
        {
            //int medal = [userInfo.drawingmedal intValue];
            switch (medal) {
                case COPPER_MEDAL:
                    if(gotted)
                    {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradeDrawingCopper"];
                    }
                    else 
                    {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradeDrawingNoCopper"];
                    }
                    break;
                case SILVER_MEDAL:
                    if(gotted)
                    {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradeDrawingSilver"];
                    }
                    else 
                    {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradeDrawingNoSilver"];
                    }
                    break;
                case GOLD_MEDAL:
                    if(gotted)
                    {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradeDrawingGold"];
                    }
                    else
                    {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradeDrawingNoGold"];

                    }
                    break;
                default:
                    break;
            }
        }
            break;
        case SHARE_MEDAL:
        {
            //int medal = [userInfo.sharemedal intValue];
            switch (medal) {
                case COPPER_MEDAL:
                    if(gotted)
                    {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradeShareCopper"];
                    }
                    else
                    {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradeShareNoCopper"];
                    }
                    break;
                case SILVER_MEDAL:
                    if(gotted)
                    {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradeShareSilver"];
                    }
                    else
                    {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradeShareNoSilver"];
                    }
                    break;
                case GOLD_MEDAL:
                    if(gotted)
                    {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradeShareGold"];
                    }
                    else
                    {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradeShareNoGold"];
                    }
                    break;
                default:
                    break;
            }
        }
            break;
        case FANS_MEDAL:
        {
            //int medal = [userInfo.fansmedal intValue];
            switch (medal) {
                case COPPER_MEDAL:
                    if(gotted)
                    {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradeFansCopper"];
                    }
                    else
                    {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradeFansNoCopper"];
                    }
                    break;
                case SILVER_MEDAL:
                    if(gotted)
                    {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradeFansSilver"];
                    }
                    else
                    {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradeFansNoSilver"];
                    }
                    break;
                case GOLD_MEDAL:
                    if(gotted)
                    {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradeFansGold"];
                    }
                    else
                    {
                        image = [mViewNav.mResLoader getImage:@"UserUpgradeFansNoGold"];
                    }
                    break;
                default:
                    break;
            }
        }
            break;
        default:
            break;
    }
    return image;
}
- (UIImage*) getImage: (int) medalType medalLevel: (int)medalLevel
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    int currentLevel = INVALID_MEDAL_LEVEL;
    switch (medalType) {
        case NEWPERSON_MEDAL:
        {
            currentLevel = [mViewNav.mSystemDataManager.newpersonmedal intValue];
        }
            break;
        case PRESENTONDUTY_MEDAL:
        {
            currentLevel = [mViewNav.mSystemDataManager.presentondutymedal intValue];
        }
            break;
        case DRAWING_MEDAL:
        {
            currentLevel = [mViewNav.mSystemDataManager.drawingmedal intValue];
        }
            break;
        case SHARE_MEDAL:
        {
            currentLevel = [mViewNav.mSystemDataManager.sharemedal intValue];
        }
            break;
        case FANS_MEDAL:
        {
            currentLevel = [mViewNav.mSystemDataManager.fansmedal intValue];
        }
            break;
        default:
            break;
    }
    if(medalType == FANS_MEDAL)
    {
        NSLog(@"fans medal");
    }
    if(currentLevel == INVALID_MEDAL_LEVEL)
    {
        return [self getAchieveMedal:medalType medalLevel:medalLevel gotted:NO];
    }
    else
    {
    
        if(currentLevel >= medalLevel)
        {
            return [self getAchieveMedal:medalType medalLevel:medalLevel gotted:YES];
        }
        else
        {
            return [self getAchieveMedal:medalType medalLevel:medalLevel gotted:NO];
        }
    }
}

- (NSString*) getAchievementString: (int) achieveType : (int)medal
{
    //return [mViewNav getAchievementDescription:achieveType :medal];
    return gAchievementDefineString[achieveType * MEDAL_LEVEL_COUNT + medal];
}
- (int) getAchievementCurrentPoint: (int)achieveType
{
    int n = 0;
    UserInfo* userInfo = [mViewNav getUserInfo];
    switch (achieveType) {
        case PRESENTONDUTY_MEDAL:
        {
            n = [mViewNav.mSystemDataManager.presentondutypoint intValue];
        }
            break;
        case NEWPERSON_MEDAL:
        {
            n = [mViewNav.mSystemDataManager.newpersonpoint intValue];
        }
            break;
        case DRAWING_MEDAL:
        {
            n = [mViewNav.mSystemDataManager.drawingpoint intValue];
        }
            
            break;
        case SHARE_MEDAL:
        {
            n = [mViewNav.mSystemDataManager.sharepoint intValue];
        }
            break;
        case FANS_MEDAL:
        {
            n = [mViewNav.mSystemDataManager.fanspoint intValue];
        }
            break;
        default:
            break;
            
    }
    return n;
}
- (void) addAchievement
{
    if(mAchievementParentView == nil)
    {
        mAchievementParentView = [[UIView alloc] init];
        mAchievementParentView.userInteractionEnabled = YES;
        [mView addSubview:mAchievementParentView];
        [mAchievementParentView release];
        float vspacing = 5;
        float hspacing = 5;
        float achieveIconBgWidth = 170;
        float achieveIconBgHeight = 170;
        float starty = mAchieveTitle.frame.origin.y + mAchieveTitle.frame.size.height + vspacing;
        float startx = mAchieveTitle.frame.origin.x + hspacing;
        float achieveProcessViewBgWidth = mAchieveTitle.frame.size.width - achieveIconBgWidth- hspacing - hspacing -  hspacing;
        float achieveProcessViewBgHeight = 80;
        float achieveProcessSpacing = 10;
        float achieveProcessViewWidth = achieveProcessViewBgWidth - achieveProcessSpacing * 2;
        float achieveProcessViewHeight = 44;
        NSString* fontName = [SEUtil getFontName];
        float fontSize = 18;
        UIColor* medalColors[] = {nil, [UIColor colorWithRed:112.0 / 255 green:53.0/255 blue:21.0/255 alpha:1], [UIColor colorWithRed:255.0/255 green:255.0/255 blue:255.0/255 alpha:1], [UIColor colorWithRed:255.0/255 green:216.0/255 blue:0 alpha:1]};
        for(int i = 0 ; i < USER_MEDAL_COUNT ; i++)
        {
            for(int j = COPPER_MEDAL ; j < MEDAL_LEVEL_COUNT ; j++)
            {
                UIImage* image = [self getImage:i medalLevel:j];
                UIImageView* achieveIconBg = [[UIImageView alloc] initWithFrame:CGRectMake(startx, starty, achieveIconBgWidth, achieveIconBgHeight)];
                [mView addSubview:achieveIconBg];
                [achieveIconBg release];
                [self setImage:achieveIconBg name:@"OptoinsUserInfoAchieveIconBg"];
                
                UIImageView* achieveBg = [[UIImageView alloc] initWithFrame:CGRectMake(startx + 5, starty + 5, 160, 160)];
                [mView addSubview:achieveBg];
                [achieveBg release];
                mAchievementIconBgView[i][j] = achieveBg;
                UIImage* image1 = [mViewNav.mResLoader getImage:gAchieveBackground[i]];
                int currentMedal = [[mViewNav getUserUpgrade] getCurrentMedal:i];
                if(currentMedal >= j)
                {
                    achieveBg.image = image1;
                }
                
                UIImageView* achieveIcon = [[UIImageView alloc] initWithFrame:CGRectMake(startx + 10, starty + 10, 150, 150)];
                [mView addSubview:achieveIcon];
                [achieveIcon release];
                mAchievementIconFgView[i][j] = achieveIcon;
                achieveIcon.image = image;
                float processViewStartx = startx  + achieveIconBg.frame.size.width + hspacing;
                //float processViewStarty = achieveProcessBg.frame.origin.y + (achieveProcessBg.frame.size.height - achieveProcessViewHeight) / 2;
                float processViewStarty = starty;
                
                SEUIProgressView* processView = [[SEUIProgressView alloc] initWithFrame:CGRectMake(processViewStartx, processViewStarty, achieveProcessViewWidth - 6, achieveProcessViewHeight)];
                [processView initData:mViewNav];
                [mView addSubview:processView];
                [processView release];
                if(i == FANS_MEDAL)
                {
                    NSArray* strArray = [NSArray arrayWithObjects:@"AchieveProgress1", @"AchieveProgress2",@"AchieveProgress3",@"AchieveProgress4", nil];
                    [processView setForegroundGroupImageStr:strArray];
                }
                FontLabel* achievePointText = [[FontLabel alloc] initWithFrame:CGRectMake(processViewStartx, processViewStarty, achieveProcessViewWidth - 6, achieveProcessViewHeight) zFont:[[FontManager sharedManager] zFontWithName:fontName pointSize:fontSize]];
                int currentPoint = 0, totalPoint = 0;
                int currentPointArray[4], totalPointArray[4];
                [[mViewNav getUserUpgrade] getMedalPointValue:i :j :&currentPoint :&totalPoint :currentPointArray: totalPointArray];
                
                if(currentPoint == totalPoint)
                {
                    achievePointText.text = @"Achieved";
                }
                else
                {
                    achievePointText.text = [NSString stringWithFormat:@"%d / %d", currentPoint, totalPoint];
                }
                achievePointText.textColor = getLabelColor();
                achievePointText.textAlignment = UITextAlignmentCenter;
                [mView addSubview:achievePointText];
                [achievePointText release];
                mAchievementPointView[i][j] = achievePointText;

                mAchievementProcessView[i][j] = processView;
                float achieveTitleBgHeight = achieveIconBg.frame.size.height - processView.frame.size.height - 10;
                UIImageView* achieveTitleBg = [[UIImageView alloc] initWithFrame:CGRectMake(processView.frame.origin.x, processView.frame.origin.y + processView.frame.size.height + 10, achieveProcessViewBgWidth, achieveTitleBgHeight)];
                [mView addSubview:achieveTitleBg];
                [achieveTitleBg release];
                [self setImage:achieveTitleBg name:@"OptionsUserInfoAchieveContentTextBg"];

                FontLabel* achieveNameLabel = [[FontLabel alloc] initWithFrame:CGRectMake(achieveTitleBg.frame.origin.x, achieveTitleBg.frame.origin.y, achieveTitleBg.frame.size.width, 30) fontName:[SESystemConfig getFontName] pointSize:20];
                [mView addSubview:achieveNameLabel];
                [achieveNameLabel release];
                achieveNameLabel.textColor = medalColors[j];
                achieveNameLabel.textAlignment = UITextAlignmentCenter;
                achieveNameLabel.backgroundColor = [UIColor clearColor];
                achieveNameLabel.text = gAchievementMedalDescription[i * MEDAL_LEVEL_COUNT + j];
                
                UIScrollView* scrollView = [[UIScrollView alloc] initWithFrame:CGRectMake(achieveTitleBg.frame.origin.x + 5, achieveNameLabel.frame.origin.y + achieveNameLabel.frame.size.height, achieveTitleBg.frame.size.width - 10, achieveTitleBg.frame.size.height - achieveNameLabel.frame.size.height)];
                scrollView.userInteractionEnabled = NO;
                
                FontLabel* label = [[FontLabel alloc] initWithFrame:CGRectMake(0, 0, achieveTitleBg.frame.size.width - 10, achieveTitleBg.frame.size.height) zFont:[[FontManager sharedManager] zFontWithName:fontName pointSize:fontSize]];
                [scrollView addSubview:label];
                [mView addSubview:scrollView];
                //scrollView.backgroundColor = [UIColor redColor];
                [scrollView release];

                NSString* achieveDescript = [self getAchievementString:i :j];
                int achievePoint = [self getAchievementCurrentPoint:i];
                //achieveDescript = [NSString stringWithFormat:@"%@ : %d", achieveDescript, achievePoint];
                
                setLabelFont(label, achieveDescript, getViewColor(), fontName, fontSize);
                CGSize s = [achieveDescript sizeWithFont:label.font constrainedToSize:CGSizeMake(label.frame.size.width, 10000) lineBreakMode:UILineBreakModeWordWrap];
                //label.backgroundColor = [UIColor redColor];
                label.numberOfLines = 8;
                label.lineBreakMode = UILineBreakModeWordWrap;
                label.frame = CGRectMake(label.frame.origin.x, label.frame.origin.y, s.width, s.height);
                if(i == FANS_MEDAL)
                {
                    float startx = 0;
                    float starty = label.frame.size.height;
                    //1. 62, 170, 201
                    //2. 222, 79, 24
                    //3. 222, 135, 24
                    //4. 109, 190, 59
                    UIColor* colors[] = {[UIColor colorWithRed:62.0/255 green:170.0/255 blue:201.0/255 alpha:1], [UIColor colorWithRed:222.0/255 green:79.0/255 blue:24.0/255 alpha:1], [UIColor colorWithRed:222.0/255 green:135.0/255 blue:24.0/255 alpha:1], [UIColor colorWithRed:109.0/255 green:190.0/255 blue:59.0/255 alpha:1]};
                    NSString* texts[] = {@"draw", @"share", @"music", @"buy"};
                    CGSize widths[] = {{0, 0}, {0, 0}, {0, 0}, {0, 0}};
                    for(int k = 0 ; k < 4 ; k++)
                    {
                        widths[k] = [texts[k] sizeWithFont:label.font constrainedToSize:CGSizeMake(label.frame.size.width, 10000) lineBreakMode:UILineBreakModeWordWrap];
                    }
                    for(int k = 0 ; k < 4 ; k++)
                    {
                        FontLabel* achievePointText1 = [[FontLabel alloc] initWithFrame:CGRectMake(startx, starty, label.frame.size.width, 30) zFont:[[FontManager sharedManager] zFontWithName:fontName pointSize:fontSize]];
                        setLabelFont(achievePointText1, texts[k], colors[k], fontName, fontSize);
                        [scrollView addSubview:achievePointText1];
                        [achievePointText1 release];
                        startx += widths[k].width + 10;
                    }
                    
                }
                scrollView.contentSize = CGSizeMake(s.width, s.height);
                if(i == FANS_MEDAL)
                {
                    scrollView.contentSize = CGSizeMake(s.width, s.height + 30);
                }
                if(scrollView.contentSize.height > scrollView.frame.size.height)
                {
                    scrollView.contentOffset = CGPointMake(0, -scrollView.contentSize.height);
                    CGPoint p = CGPointMake(0, scrollView.contentSize.height);
                    void (^animBlock) (void) = ^{
                        scrollView.contentOffset = p;
                    };
                    void (^animEnd) (BOOL) = ^(BOOL f)
                    {
                        NSLog(@"animation end;");
                        

                    };
                    int opts = UIViewAnimationOptionCurveLinear |UIViewAnimationOptionRepeat|UIViewAnimationOptionAllowUserInteraction ;
                    [UIView animateWithDuration:28 delay: 0 options: opts animations:animBlock completion:animEnd];
                }
                //achieveTitleBg.frame = CGRectMake(achieveTitleBg.frame.origin.x, achieveTitleBg.frame.origin.y, label.frame.size.width + 20, label.frame.size.height + 20);
                //[mView addSubview:label];
                [label release];
                label.textAlignment = UITextAlignmentLeft;
                float newStarty = achieveTitleBg.frame.origin.y + achieveTitleBg.frame.size.height;
                starty =  newStarty + 8;
            }
        }
        myHeight = starty;
    }
}
- (void) addNextUserGetInfo
{
    [mNextGetContentParent removeFromSuperview];
    mNextGetContentParent = nil;
    if(mNextGetContentParent == nil)
    {
        mNextGetContentParent = [[UIView alloc] init];
        mNextGetContentParent.userInteractionEnabled = YES;
        [mUserNextGetScrollView addSubview:mNextGetContentParent];
        [mNextGetContentParent release];
    }
    for(int i = 0 ; i < NEXT_GET_TYPE_NUM ; i++)
    {
        mNextGetNum[i] = 0;
    }
    UserInfo* userInfo = [mViewNav getUserInfo];
    int level = [[mViewNav getUserUpgrade] getLevelFromExpNum:[mViewNav.mSystemDataManager.exppointnum intValue]];
    int nextLevel = level + 1;
    if(nextLevel > [mViewNav getMaxUserLevel])
    {
        nextLevel = level;
    }
    if(nextLevel == level)
    {
        NSLog(@" you are at max level");
        return;
    }
    SEUserData* nextUserData = [mViewNav getUserData:nextLevel];
    SEUserData* currUserData = [mViewNav getUserData:level];
    mNextGetNum[IMAGE_NEXT_GET] = nextUserData.imageListNum - currUserData.imageListNum;
    mNextGetNum[MUSIC_NEXT_GET] = nextUserData.musicListNum - currUserData.musicListNum;
     
    if(nextUserData.brushNum > 0)
    {
        mNextGetNum[BRUSH_NEXT_GET] = nextUserData.brushNum;
    }
    if(nextUserData.timeFontNum > 0)
    {
        mNextGetNum[TIMEFONT_NEXT_GET] = nextUserData.timeFontNum;
    }
     
    float startx = 0;
    float starty = 0;
    float hspacing = 10;
    float iconWidth = 103;
    for(int i = 0 ; i < NEXT_GET_TYPE_NUM ; i++)
    {
        if(mNextGetNum[i] > 0)
        {
            [self addNextGetView: (USER_NEXT_GET_TYPE)i startx:startx starty:starty];
            startx += hspacing + iconWidth;
        }
    }
    mUserNextGetScrollView.contentSize = CGSizeMake(startx, mUserNextGetScrollView.frame.size.height);
}
- (void) setLockView
{
    if(mLevelLockView == nil)
    {
        float y = mUserNextGetBg.frame.origin.y + mUserNextGetBg.frame.size.height;
        mLevelLockView = [[SEOptionsLockView alloc] initWithFrame:CGRectMake(mLevelBg.frame.origin.x, mLevelBg.frame.origin.y, mLevelBg.frame.size.width, y) lockSize:CGSizeMake(20, 100)];
        [mLevelLockView setText:@"You should buy Basic Setting firstly"];
        [mView addSubview:mLevelLockView];
        [mLevelLockView release];
    }
    if([SEUserDefaultManager isFunctionOK:USERINFO_FUNC] == NO)
    {
        [mLevelLockView setLock:YES type:LOCK_BUY];
    }
    else
    {
        [mLevelLockView setLock:NO type:LOCK_BUY];
    }
    
    
    if(mArchieveLockView == nil)
    {
        float starty = mAchieveTitle.frame.origin.y;
        float endy = myHeight;
        mArchieveLockView = [[SEOptionsLockView alloc] initWithFrame:CGRectMake(mAchieveTitle.frame.origin.x, starty, mAchieveTitle.frame.size.width, endy) lockSize:CGSizeMake(20, 100)];
        [mArchieveLockView setText:@"You should buy Basic Setting firstly"];
        [mView addSubview:mArchieveLockView];
        [mArchieveLockView release];
    }
    if([SEUserDefaultManager isFunctionOK:USERINFO_FUNC] == NO)
    {
        [mArchieveLockView setLock:YES type:LOCK_BUY];
    }
    else {
        [mArchieveLockView setLock:NO type:LOCK_BUY];
    }
    
}
- (void) plusIconTapHandler: (UITapGestureRecognizer*)ges
{
    NSLog(@"pluse icon tap");
    [mOptionsView setCurrentBarView:APP_STORE_SETTING];
    [mOptionsView setAppStoreItemSelected: PRODUCT_LEVELUP_MAX];
}
- (UIView*) getView
{
    if(mView == nil)
    {
        mView = [[[NSBundle mainBundle] loadNibNamed:@"Options3" owner:self options:nil] lastObject];
        [mView retain];
        mView.backgroundColor = [UIColor clearColor];
        [self setImage:mLevelTitle.background name:@"OptionsUserInfoSettingLevelTitleBg"];
        [self setImage:mLevelBg name:@"OptionsUserInfoSettingLevelBg"];
        [self setImage:mLevelTextBg name:@"OptionsUserInfoSettingLevelTextBg"];
        [self setImage:mLevelPlusIcon name:@"OptionsUserInfoSettingPlusBg"];
        [self setImage:mLevelProgessBarBg name:@"OptionsUserInfoProgressBarBg"];
        [mLevelProgressView initData: mViewNav];
        [self setImage:mUserGetBg name:@"OptionsUserInfoUserGetBg"];
        [self setImage:mUserNextGetBg name:@"OptionsUserInfoUserNextGetBg"];
        [self setImage:mNextLevelGetTitle.background name:@"OptionsUserInfoUserNextGetTitleBg"];
        [self setImage:mAchieveTitle.background name:@"OptionsUserInfoAchieveTitleBg"];
        UIImage* image = [mViewNav.mResLoader getImage:@"OptionsUserInfoBigPlusIcon"];
        mLevelPlusIcon.hidden = YES;
        mLevelPlusIcon.imageView.image = image;
        mLevelPlusIcon.userInteractionEnabled = YES;
        UITapGestureRecognizer* plusIconGes = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(plusIconTapHandler:)];
        [mLevelPlusIcon addGestureRecognizer:plusIconGes];
        [plusIconGes release];
        [mLevelPlusIcon setImageViewSize:CGSizeMake(image.size.width, image.size.height)];
        [self setLabel];
        [self addUserGetInfo];
        [self addNextUserGetInfo];
        [self addAchievement];
        [self setLockView];
    }
    return mView;
}
- (void) setData
{
    [self updateInfo];
}
@end
/////////
/*
@implementation SEOptionsSignatureSetting
//@synthesize mBg;
@synthesize mSigBg;
@synthesize mSigTitleBg;
@synthesize mAutoColorTextBg;
@synthesize mPreviewBg;
@synthesize mSigSizeSlider;
@synthesize mSigSizeTextBg;
@synthesize mSigViewTextBg;
@synthesize mSigSizeResultTextBg;
@synthesize mSigViewSwitch;
@synthesize mAutoColorSwitch;
@synthesize mSigResultLabel;
@synthesize mSigSizeTextLabel;
@synthesize mSigViewTextLabel;
@synthesize mSignatureTitleLabel;
@synthesize mAutoColorTextLabel;
@synthesize mDrawView;

- (void) dealloc
{
    [mSigBg release];
     [mSigTitleBg release];
     [mAutoColorTextBg release];
     [mPreviewBg release];
     [mSigSizeSlider release];
     [mSigSizeTextBg release];
     [mSigViewTextBg release];
     [mSigSizeResultTextBg release];
     [mSigViewSwitch release];
     [mAutoColorSwitch release];
     [mSigResultLabel release];
     [mSigSizeTextLabel release];
     [mSigViewTextLabel release];
     [mSignatureTitleLabel release];
     [mAutoColorTextLabel release];
     [mDrawView release];    
    [super dealloc];
}
- (void) setLabel
{
    NSString* fontName = [SEUtil getFontName];
    float fontSize = 30;
    setLabelFont(mSignatureTitleLabel, @"Signature", [UIColor whiteColor], fontName, fontSize);
    setLabelFont(mSigViewTextLabel, @"Signatue View", [UIColor blackColor], fontName, fontSize);
    setLabelFont(mAutoColorTextLabel, @"Auto Color", [UIColor blackColor], fontName, fontSize);
    setLabelFont(mSigSizeTextLabel, @"Signature Size", [UIColor blackColor], fontName, fontSize);
    setLabelFont(mSigResultLabel, @"Medium", [UIColor blackColor], fontName
                 , fontSize);
}
- (void) createFontDisplay
{
    float starty = mSigBg.frame.origin.y + mSigBg.frame.size.height;
    float startx = 10;
    NSArray* fontFamilyNames = [UIFont familyNames];
    for(NSString* fontFamily in fontFamilyNames)
    {
        NSArray* fontNames = [UIFont fontNamesForFamilyName:fontFamily];
        NSLog(@"font family name = %@", fontFamily);
        for(NSString* fontName in fontNames)
        {
            NSLog(@"font names = %@", fontName);
            float size = [UIFont systemFontSize] + 10;
            int i = 0;
            //for(int i = 0 ; i < 5 ; i++)
            {
                UIFont* font = [UIFont fontWithName:fontName size:size + i];
                float fontHeight = font.pointSize;
                UILabel* label = [[UILabel alloc] initWithFrame:CGRectMake(startx, starty, mSigBg.frame.size.width - 20, fontHeight)];
                [mView addSubview:label];
                [label release];
                label.font = font;
                label.text = [NSString stringWithFormat:@"%@ / %@", fontFamily, fontName];
                starty += 10 + fontHeight;
            }
        }
    }
    myHeight = starty;
}
- (float) getHeight
{
    return myHeight;
}
- (IBAction) signatureSizeSliderHandler:(UISlider*)sender
{
    int v = (int)sender.value;
    UserInfo* userInfo = [mViewNav getUserInfo];
    
    if(v != [userInfo.signaturesize intValue])
    {
        userInfo.signaturesize = [NSNumber numberWithInt:v];
        if(v == 0)
        {
            mSigResultLabel.text = @"Small";
        }
        else if(v == 1)
        {
            mSigResultLabel.text = @"Medium";
        }
        else
        {
             mSigResultLabel.text = @"Big";
        }
    }
}
- (UIView*) getView
{
    if(mView == nil)
    {
        mView = [[[NSBundle mainBundle] loadNibNamed:@"Options4" owner:self options:nil] lastObject];
        [mView retain];
        mView.backgroundColor = [UIColor clearColor];
        //[self setImage:mBg name:@"OptionsRightBackground"];
        //[mBg removeFromSuperview];
        [self setImage:mSigTitleBg name:@"OptionsSignatureSettingTitleBg"];
        [self setImage:mSigBg name:@"OptionsSignatureSettingBg"];
        [self setImage:mSigViewTextBg name:@"OptionsSignatureSettingSigViewTextBg"];
        [self setImage:mAutoColorTextBg name:@"OptionsSignatureSettingAutoColorTextBg"];
        [self setImage:mSigSizeTextBg name:@"OptionsSignatureSettingSigSizeTextBg"];
        [self setImage:mSigSizeResultTextBg name:@"OptionsSignatureSettingSigSizeResultTextBg"];
        //[self setImage:mPreviewBg name:@"OptionsSignatureSettingPreviewBg"];
        mPreviewBg.image = nil;
        mPreviewBg.backgroundColor = [UIColor clearColor];
        //mPreviewBg.hidden = YES;
        [mAutoColorSwitch initImageView:mViewNav];
        [mSigViewSwitch initImageView:mViewNav];
        setUISliderBg(mSigSizeSlider, mViewNav);
        mSigSizeSlider.minimumValue = 0;
        mSigSizeSlider.maximumValue = 2;
        [mSigViewSwitch setTarget:self action:@selector(setSignatureShow:)];
        [mAutoColorSwitch setTarget:self action:@selector(setSignatureAutoColor:)];
        UIImage* image = [mViewNav.mResLoader getImage:@"SignatureDrawBg"];
        [mDrawView setBackgroundImage:[SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9]];
        mDrawView.backgroundColor = [UIColor clearColor];
        
        [self setLabel];
        [self createFontDisplay];
    }
    return mView;
}
- (void)signaturePreviewHandler:(UITapGestureRecognizer*)tap
{
    //[mViewNav moveToView:OPTIONS_SIGNATURE:SIGNATURE_VIEW hasAnimation:YES];
    [mViewNav setViewRelationType:TYPE1];
    [mViewNav moveToView:OPTIONS_SIGNATURE :SIGNATURE_VIEW hasAnimation:YES isPush:YES];
}
- (void) setData
{
    UITapGestureRecognizer* tap = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(signaturePreviewHandler:)];
    [mPreviewBg addGestureRecognizer:tap];
    [tap release];
    mPreviewBg.userInteractionEnabled = YES;
    mView.userInteractionEnabled = YES;
    mDrawView.userInteractionEnabled = NO;
    [mDrawView setLineWidthRatio:(mDrawView.frame.size.width / 717.0)];
    Signature* sig = [mViewNav getCurrentSignature];
    if(sig != nil)
    {
        NSString* str = [NSString stringWithFormat:@"%d", [sig.seq intValue]];
        int seq = [sig.seq intValue];
        SignaturePointData sd = [mViewNav getSignaturePointsWithSeqName:str];
        if(sd.points != nil && sd.points.count > 0)
        {
            [mDrawView setNormalizePoints:sd.points];
            [mDrawView setPointColorArray:sd.colors];
        }
    }
    UserInfo* userInfo = [mViewNav getUserInfo];
    if([userInfo.showsignatureview boolValue])
    {
        [mSigViewSwitch setOn:YES animated:NO];
    }
    else {
        [mSigViewSwitch setOn:NO animated:NO];
    }
    if([userInfo.signatureautocolor boolValue])
    {
        [mAutoColorSwitch setOn:YES animated:NO];
    }
    else {
        [mAutoColorSwitch setOn:NO animated:NO];
    }

}
- (void) setSignatureShow:(NSNumber *)bOk
{
    NSLog(@"signatuer = %d", [bOk boolValue]);
    UserInfo* info = [mViewNav getUserInfo];
    info.showsignatureview = [NSNumber numberWithBool:[bOk boolValue]];
}
- (void) setSignatureAutoColor:(NSNumber *)bOk
{
    NSLog(@"signature auto color = %d", [bOk boolValue]);
    UserInfo* info = [mViewNav getUserInfo];
    info.signatureautocolor = [ NSNumber numberWithBool:[bOk boolValue]];
}
@end
 */
/////
NSString* develper[] = 
{@"Programmer : ",  @"Wei Lu",
@"Designer : "                  ,                      @"Haiqing Zhao",
@"Artist : "                    ,                      @"Haiqing Zhao",
@"Testers : "                   ,                      @"Wei Lu,Haiqing Zhao",
@"Product manager : "           ,                      @"Haiqing Zhao",
@"Text translation support : "  ,                      @"Beijing Boyu Translation Co. ,Ltd",
@"Image copyright provided : ",                   @"www.123rf.net\n"};
@implementation SEOptionsAboutSetting
@synthesize mAboutBg;
@synthesize mAboutTitleBg;
@synthesize mBrand;
@synthesize mSubmit;
@synthesize mSuggestBg;
@synthesize mStatusBg;
@synthesize mSuggestTitleBg;
@synthesize mTextView;
@synthesize mDownloadLabel;
@synthesize mDownloadButton;
@synthesize mCopyRightText;
@synthesize mSubmitButton;
@synthesize mSuggestTitle;
@synthesize mAboutTitle;
@synthesize mThanksTitle;
@synthesize mDeveloperText;
@synthesize mDeveloperBg;
@synthesize mFacebookButton;
@synthesize mFacebookText;
@synthesize mAppVersionText;
//
@synthesize mClearDataButton;
- (void) dealloc
{
    [mAboutBg release];
    [mAboutTitleBg release];
    [mBrand release];
    [mSubmit release];
    [mSuggestBg release];
    [mStatusBg release];
    [mSuggestTitleBg release];
    [mTextView release];   
    [mDownloadLabel release];
    [mDownloadButton release];
    [mCopyRightText release];
    [mSubmitButton release];
    [mAboutTitle release];
    [mSuggestTitle release];
    [mThanksTitle release];
    [mDeveloperBg release];
    [mDeveloperText release];
    [mFacebookButton release];
    [mFacebookText release];
    [mAppVersionText release];
    [mClearDataButton release];
    [super dealloc];
}
- (void) setLabel
{
    NSString* fontName = [SEUtil getFontName];
    float fontSize = 30;
    setLabelFont(mCopyRightText, @"Copyright 2013 Gidea (Beijing) Technology Co., Ltd. All rights reserved.", getViewColor(), fontName, 18);
    mCopyRightText.textAlignment = UITextAlignmentCenter;
    setLabelFont(mSubmitButton.buttonText, @"Submit", getViewColor(), fontName, 30);
    setLabelFont(mAboutTitle, @"About Gidea (Beijing) Technology Co., Ltd", getLabelColor(), fontName, 30);
    setLabelFont(mSuggestTitle, @"Your Suggestions", getLabelColor(), fontName, 30);
    setLabelFont(mThanksTitle, @"Thanks for your support", getViewColor(), fontName, 30);
    NSString* version = [NSString stringWithFormat:@"%@  %@", @"PrivatePainter", @"v1.0.0"];
    setLabelFont(mAppVersionText, version, [UIColor blackColor], fontName, 30);
    mAppVersionText.textAlignment = UITextAlignmentCenter;
}
- (IBAction) clearData:(id)sende
{
    NSLog(@"clear data");
    [SEUserDefaultManager restoreProductAndFunction];
}
+ (NSArray*) getFeedChar
{
    NSArray* ret = [NSArray arrayWithObject:[NSNumber numberWithUnsignedChar:'c']];
    return ret;
}
- (void) submitHandler: (id)sender
{
    NSLog(@"submit handler");
    //[mViewNav notificationShow];
    NSString* text = mTextView.text;
    /*
    if([text isEqualToString:@"#*888888*#"])
    {
        [mViewNav popupUpgradeInfoTest];
    }
    else if([text isEqualToString:@"#*666666*#"])
    {
        NSLog(@"## download start ##");
        [mViewNav downloadParamConfig:mThanksTitle textName:@"paramset_url.txt"];
    }
    else if([text isEqualToString:@"#*555555*#"])
    {
        NSLog(@"clear twitter signIn information");
        [[NSUserDefaults standardUserDefaults] removeObjectForKey:@"UserTwitterKey"];
    }
    else
    */
    {
        if(text != nil && [text isEqualToString:@""] == NO)
        {
            [mViewNav sendComment:text label:mThanksTitle];
        }
    }
}
- (IBAction) facebookURLButtonHandler: (UIButton*)button
{
    NSLog(@"facebook button");
    NSString* urlStr = @"https://www.facebook.com/pages/Privatepainter/403544433065491";
    [[UIApplication sharedApplication] openURL:[NSURL URLWithString:urlStr]];
}
- (UIView*) getView
{
    if(mView == nil)
    {
        mView = [[[NSBundle mainBundle] loadNibNamed:@"Options5" owner:self options:nil] lastObject];
        [mView retain];
        mView.backgroundColor = [UIColor clearColor];
        [self setImage:mAboutBg name:@"OptionAboutSettingAboutBg"];
        [self setImage:mAboutTitleBg name:@"OptionAboutSettingAboutTitleBg"];
        UIImage* image = [mViewNav.mResLoader getImage:@"OptionAboutSettingBrandImage"];
        CGSize srcSize = CGSizeMake(image.size.width, image.size.height);
        CGSize dstSize = CGSizeMake(mBrand.frame.size.width, mBrand.frame.size.height);
        dstSize = [SEUtil computeFitSize:srcSize toDst:dstSize];
        UIGraphicsBeginImageContext(CGSizeMake(dstSize.width, dstSize.height));
        [image drawInRect:CGRectMake(0, 0, dstSize.width, dstSize.height)];
        UIImage* newImage = UIGraphicsGetImageFromCurrentImageContext();
        UIGraphicsEndImageContext();
        mBrand.image = newImage;
        mBrand.contentMode =  UIViewContentModeCenter;
        [self setImage:mSuggestBg name:@"OptionAboutSettingSuggestBg"];
        [self setImage:mDeveloperBg name:@"OptionAboutSettingSuggestBg"];
        [self setImage:mSuggestTitleBg name:@"OptionAboutSettingSuggestTitleBg"];
        [self setImage:mStatusBg name:@"OptionAboutSettingStatusBg"];
        //[self setImage:mCopyRightText.background name:@"OptionsAboutSettingCopyRightTextBg"];
        //mCopyRightText.backgroundColor = [UIColor clearColor];
        image = [mViewNav.mResLoader getImage:@"OptionAboutSettingSubmitBg"];
        image = [SEUtil imageWithCap:image top:0.1 bottom:0.1 left:0.1 right:0.1];
        [mSubmitButton.button setBackgroundImage:image forState:UIControlStateNormal];
        image = [mViewNav.mResLoader getImage:@"OptionsStoreItemSelectedBg"];
        image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
        [mSubmitButton.button setBackgroundImage:image forState:UIControlStateHighlighted];
        [mSubmitButton.button addTarget:self action:@selector(submitHandler:) forControlEvents:UIControlEventTouchUpInside];
        mTextView.backgroundColor = [UIColor clearColor];
        [self setLabel];
        mDeveloperText.editable = NO;
        mDeveloperText.backgroundColor = [UIColor clearColor];
        //mDeveloperText.text = develper;
        int count = sizeof(develper) / sizeof(NSString*);
        count = count / 2;
        float startx = mDeveloperText.frame.origin.x;
        float starty = mDeveloperText.frame.origin.y;
        float width = mDeveloperText.frame.size.width / 2;
        for(int i = 0 ; i < count ; i++)
        {
            UILabel* label1 = [[UILabel alloc] initWithFrame:CGRectMake(startx, starty,width, 30)];
            UILabel* label2 = [[UILabel alloc] initWithFrame:CGRectMake(startx + width, starty, width, 30)];
            label1.text = develper[2 * i];
            label2.text = develper[2 * i + 1];
            label1.backgroundColor = [UIColor clearColor];
            label2.backgroundColor = [UIColor clearColor];
            label1.textAlignment = UITextAlignmentRight;
            label1.font = [UIFont fontWithName:[SESystemConfig getFontName] size:20];
            label2.font = [UIFont fontWithName:[SESystemConfig getFontName] size:20];
            [mView addSubview:label1];
            [mView addSubview:label2];
            [label1 release];
            [label2 release];
            starty += 30;
        }
        mDeveloperText.font = [UIFont fontWithName:[SESystemConfig getFontName] size:20];
        UIImage* facebookNormal = [mViewNav.mResLoader getImage:@"FacebookLinkImageNormal"];
        UIImage* facebookH = [mViewNav.mResLoader getImage:@"FacebookLinkImageH"];
        [mFacebookButton setImage:facebookNormal forState:UIControlStateNormal];
        [mFacebookButton setImage:facebookH  forState:UIControlStateHighlighted];
        setLabelFont(mFacebookText, @"Click here to follow the status and development progress of our apps. Join us in our development!", [UIColor blackColor], [SESystemConfig getFontName], 20);
        mFacebookText.numberOfLines = 3;
        mFacebookText.lineBreakMode = UILineBreakModeWordWrap;
    }
    return mView;
}
- (void) setData
{
    
}
- (float) getHeight
{
    return mDeveloperBg.frame.origin.y + mDeveloperBg.frame.size.height;
}
- (IBAction) downloadHandler:(id)sender
{
    NSLog(@"## download start ##");
    [mViewNav downloadParamConfig:mDownloadLabel texName: @"paramset_url.txt"];
}
@end
//////////
@interface SESELValue : NSObject    
{
    SEL selector;
    enum USER_FUNCTION func;
}
@property (nonatomic, assign) enum USER_FUNCTION func;
- (void) setSelector: (SEL) s;
- (SEL) getSelector;
@end
@implementation SESELValue
@synthesize func;
- (void) setSelector:(SEL)s
{
    selector = s;
}
- (SEL) getSelector
{
    return selector;
}
@end
////
@implementation SEOptionsAppStoreSetting2
/*
- (void) submitButtonHandler: (UIButton*) sender
{
    NSLog(@"submit button pressed");
    NSMutableArray* productIdArray = [NSMutableArray arrayWithArray:[NSArray array]];
    for(SEOptionsStoreItem* item in mStoreItems)
    {
        if([item isSelected])
        {
            [productIdArray addObject:item.productId];
        }
    }
    SEInAppPurchaseTransactionObserver* transaction = [PhotoFrameAppDelegate getPurchaseTransactionObserver];
    for(int i = 0 ; i < productIdArray.count ; i++)
    {
        NSLog(@"product : %@", [productIdArray objectAtIndex:i]);
        [transaction addProduct:[productIdArray objectAtIndex:i]];
        //[SEUserDefaultManager setProductProcessing:[productIdArray objectAtIndex:i]];
    }
    [transaction requestAllProduct];
}
 */
- (void) changeAllItemNoSelected
{
    for(SEOptionsStoreItem* item in mStoreItems)
    {
        if([item isSelected])
            [item setSelected:NO];
    }
}
- (void) selectStoreItem: (int) productType
{
    [self changeAllItemNoSelected ];
    for(int i = 0 ; i < mStoreItems.count ; i++)
    {
        SEOptionsStoreItem* sitem = [mStoreItems objectAtIndex:i];
        if(sitem.productType == productType)
        {
            [sitem setSelected:YES];
        }
    }
    
}
- (void) storeItemTouchHandler: (SEStoreItemParam*)param
{
    int state = param.state;
    if(state == STOREITEM_TOUCH_STATE_END)
    {
        //for(SEOptionsStoreItem* sitem in mStoreItems)
        for(int i = 0 ; i < mStoreItems.count ; i++)
        {
            SEOptionsStoreItem* sitem = [mStoreItems objectAtIndex:i];
            if(sitem != param.item && [sitem isSelected])
            {
                [sitem setSelected:NO];
            }
        }
    }
    [param release];
}
- (void) confirmDlgHandler
{
    [[PhotoFrameAppDelegate getViewNavigator] dismissConfirmDlg];
}
- (void) showAlert:(NSString*) title msg: (NSString*)msg
{
    /*
    UIAlertView *alertView = [[UIAlertView alloc] initWithTitle:title
                                                        message:msg
                                                       delegate:nil
                                              cancelButtonTitle:@"OK"
                                              otherButtonTitles:nil];
    [alertView show];
     */
    [[PhotoFrameAppDelegate getViewNavigator] createConfirmDlg: self ok:@selector(confirmDlgHandler)];
    SEConfirmDlg* dlg = [mViewNav getConfirmDlg];
    dlg.message.text = title;
    dlg.message2.text = msg;
}
- (void) loadingTimerUpdate2: (NSTimer*)timer
{
    mConnectionTime--;
    if(mConnectionTime == 0)
    {
        mConnectionTime = SEMAX_CONNECTION_TIME;
    }
    SELoadingView* loadingView = [[PhotoFrameAppDelegate getViewNavigator] getLoadingView];
    NSString* str = [NSString stringWithFormat:@"%d", mConnectionTime];
    [loadingView setText:str];
}
- (void) continueHandler: (UIButton*)b
{
    mConnectionTime = SEMAX_CONNECTION_TIME;
    [mConnectionTimer invalidate];
    mConnectionTimer = nil;
    mConnectionTimer =  [NSTimer timerWithTimeInterval:1 target:self selector:@selector(loadingTimerUpdate2:) userInfo:nil repeats:YES];
    [[NSRunLoop currentRunLoop] addTimer:mConnectionTimer forMode:NSDefaultRunLoopMode];
}
- (void) exitHandler: (UIButton*)b
{
    [[PhotoFrameAppDelegate getViewNavigator] hideLoadingView];
}
- (void) changeLoadingContent
{
    SELoadingView* loadingView = [[PhotoFrameAppDelegate getViewNavigator] getLoadingView];
    [loadingView showButton : SE_LOADINGVIEW_ONE_BUTTON];
    //SELoadingButton* buttonContinue =[ loadingView getContinueButton];
    SELoadingButton* buttonExit = [loadingView getExitButton];
    buttonExit.buttonText.text = @"OK";
    //buttonContinue.buttonText.text = @"Continue";
    //[buttonContinue.button addTarget:self action:@selector(continueHandler:) forControlEvents:UIControlEventTouchUpInside];
    [buttonExit.button addTarget:self action:@selector(exitHandler:) forControlEvents:UIControlEventTouchUpInside];
    [loadingView setText:@"You can not connect to AppStore or you don't confirm the product information. Please confirm the product or try it again. It will not consume additional expense!"];
}
- (void) loadingTimerUpdate: (NSTimer*)timer
{
    mConnectionTime--;
    if(mConnectionTime == 0)
    {
        [mConnectionTimer invalidate];
        mConnectionTimer = nil;
        [self changeLoadingContent];
        return;
    }
    SELoadingView* loadingView = [[PhotoFrameAppDelegate getViewNavigator] getLoadingView];
    NSString* str = [NSString stringWithFormat:@"%d", mConnectionTime];
    [loadingView setText:str];
}
- (void) showDialogForNotGetPrice
{
    [self showAlert:@"Warning!" msg:@"You have not gotten the lastest proudct price. You can not purchase the product"];
}
- (void) itemButtonHandler: (UIButton*) sender
{
    NSLog(@"store item button pressed");
    if([SEUtil isWifiConnectionOK] == NO)
    {
        [self showAlert:@"Warning!" msg:@"Wifi is not connected, You can not connect to AppStore."];
        return;
    }
    SEOptionsStoreItem* item = (SEOptionsStoreItem*)sender.superview;
    if(item.hasUpdatePrice == NO)
    {
        [self showDialogForNotGetPrice];
        return;
    }
    [[PhotoFrameAppDelegate getViewNavigator] showLoadingView];
    SELoadingView* loadingView = [[PhotoFrameAppDelegate getViewNavigator] getLoadingView];
    [loadingView useTextView];
    [loadingView showStageView];
    [loadingView setStage:LOADING_STAGE3];
    [loadingView setText2:@"Connect to AppStore"];
    mConnectionTime = SEMAX_CONNECTION_TIME;
    NSString* str = [NSString stringWithFormat:@"%d", mConnectionTime];
    [loadingView setText:str];
    mConnectionTimer =  [NSTimer timerWithTimeInterval:1 target:self selector:@selector(loadingTimerUpdate:) userInfo:nil repeats:YES];
    [[NSRunLoop currentRunLoop] addTimer:mConnectionTimer forMode:NSDefaultRunLoopMode];
    
    SEProductManager* productManager = [PhotoFrameAppDelegate getProductManager];
    NSDate* date = [NSDate date];
    SEProduct* product = [productManager getProductWithDate:item.productId date:date];
    NSLog(@"product id = %@", product.productId);
    //float price = product.productPrice;
    if([item isSelected] == NO)
    {
        [self changeAllItemNoSelected];
        [item setSelected:YES];
    }
    SEInAppPurchaseTransactionObserver* transaction = [PhotoFrameAppDelegate getPurchaseTransactionObserver];
    [transaction purchaseProduct:product.productId];
    //[transaction addProduct:product.productId];
    //[transaction requestAllProduct];

}
- (SEOptionsStoreItem*) createTwoLabelItem: (enum PRODUCT_TYPE)type rect: (CGRect) rect dest1: (NSString*)str1 dest2 : (NSString*) str2 product:(SEProduct*)product
{
    NSString* fontName = [SEUtil getFontName];
    UIColor* labelColor = [UIColor colorWithRed:65.0/255 green:65.0/255 blue:65.0/255 alpha:1];
    UIColor* valueColor = [UIColor colorWithRed:1 green:156.0/255 blue:0 alpha: 1];
    SEOptionsStoreItem* retItem = nil;
    switch (type) {
        case PRODUCT_LEVELUP_MAX:
        {
            SEOptionsStoreItemLevelUpMaxItem* item = [[SEOptionsStoreItemLevelUpMaxItem alloc] initWithFrame:rect product:product];
            //setLabelFont(item.label, str1, labelColor, fontName, 20);
            //setLabelFont(item.label2, str2, labelColor, fontName, 40);
            retItem = item;
        }
            break;
        case PRODUCT_ADD_MUSIC:
        {
            SEOptionsStoreAddCountItem* item = [[SEOptionsStoreAddCountItem alloc] initWithFrame:rect product:product];
            //setLabelFont(item.label, str1, valueColor, fontName, 50);
            //setLabelFont(item.label2, @"ADD MUSIC", labelColor, fontName, 20);
            //setLabelFont(item.label3, @"UPPER LIMIT", labelColor, fontName, 20);
            retItem = item;
        }
            break;
        case PRODUCT_ADD_IMAGE:
        {
            SEOptionsStoreAddCountItem* item = [[SEOptionsStoreAddCountItem alloc] initWithFrame:rect product:product];
            //setLabelFont(item.label, str1, valueColor, fontName, 50);
            //setLabelFont(item.label2, @"ADD IMAGE", labelColor, fontName, 20);
            //setLabelFont(item.label3, @"UPPER LIMIT", labelColor, fontName, 20);
            retItem = item;
        }
            break;
        case PRODUCT_BRUSH:
        {
            SEOptionsStoreTwoVerticalLabel* item = [[SEOptionsStoreTwoVerticalLabel alloc] initWithFrame:rect product:product];
            //setLabelFont(item.label, @"ADD BRUSH", labelColor, fontName, 20);
            //setLabelFont(item.label2, str1, labelColor, fontName, 20);
            retItem = item;
        }
            break;
        case PRODUCT_TIME_STYLE:
        {
            SEOptionsStoreTwoVerticalLabel* item = [[SEOptionsStoreTwoVerticalLabel alloc] initWithFrame:rect product:product];
            //setLabelFont(item.label, @"ADD TIME", labelColor, fontName, 20);
            //setLabelFont(item.label2, str1, labelColor, fontName, 20);
            retItem =  item;
        }
            break;
        default:
        {
            SEOptionsStoreItem* newItem = [[SEOptionsStoreItem alloc] initWithFrame:rect product:product];
            //setLabelFont(newItem.label, str1, labelColor, fontName, 30);
            retItem = newItem;
        }
            break;
    }
    
    if(product.startDate != nil)
    {
        
    }
    retItem.clipsToBounds = NO;
    return retItem;
}
- (void) createChild
{
    int leftPadding = 13;
    int rightPadding = 10;
    int topPadding = 10;
    float starty = topPadding;
    float startx = leftPadding;

    float width = mView.frame.size.width;
    //float height = mView.frame.size.height;
    float labelHeight = 57;
    float storeItemHeight = 105;
    float itemSpacing = 5;
    float groupSpacing = 10;
    NSString* fontName = [SEUtil getFontName];
    float fontSize = 30;
    SEProductManager* productManager = [PhotoFrameAppDelegate getProductManager];
    int categoryNum = [productManager getCatetoryNum];
    for(int i = 0 ; i < categoryNum ; i++)
    {
        float v = 215.0 / 255.0;
        UIColor* labelColor = [UIColor colorWithRed:v green:v blue:v alpha:1];
        SEProductCategory* category = [productManager getCategoryByIndex:i];
        PRODUCT_CATEGORY_TYPE categoryType = category.type;
        NSArray* origProducts = [productManager getProductArrayByCatetoryType:categoryType];
        NSArray* products = [productManager getProductArrayByCatetoryType:categoryType];
        products = [productManager getProductAndDiscountArrayByCatetoryType:categoryType];
        assert(products.count > 0);
        float groupWidth = width - leftPadding - rightPadding;
        float startxInGroup = 10;
        float startyInGroup = 0;
        float itemWidth = groupWidth - 20;
        SEOptionsImageView* groupBackground = [[SEOptionsImageView alloc] initWithFrame:CGRectMake(startx, starty, groupWidth, 10)];
        groupBackground.userInteractionEnabled = YES;
        [mView addSubview:groupBackground];
        [groupBackground release];
        //groupBackground.backgroundColor = [UIColor redColor];
        [self setImage:groupBackground name:@"AppStoreGroupBg"];
        
        SEOptionsLabel* label = [[SEOptionsLabel alloc] initWithFrame:CGRectMake(0, 0, groupWidth, labelHeight)];
        setLabelFont(label.label, category.description, labelColor, fontName, fontSize);
        [self setImage:label.background name:@"AppStoreLabelBg"];
        [groupBackground addSubview:label];
        [label release];
        //starty += label.frame.size.height + itemSpacing;
        startyInGroup = label.frame.size.height + itemSpacing;
        
        for(SEProduct* product in products)
        {
            SEOptionsStoreItem* item = nil;
            BOOL isBasicSettingItem = product.func == BASIC_FUNC;
            SEOptionsStoreItem* newItem = [self createTwoLabelItem:product.productType rect:CGRectMake(startxInGroup, startyInGroup, itemWidth, storeItemHeight) dest1:product.productDescript1 dest2:product.productDescript2 product:product];
            newItem.productType = product.productType;
            [self setImage:newItem.background name:@"AppStoreItemBg"];
            //setLabelFont(newItem.label, product.productDescript1, labelColor, fontName, fontSize);
            
            if([product isDiscount] == NO)
            {
                NSString* priceStr = [NSString stringWithFormat:@"%1.2f $", product.productPrice];
                setLabelFont(newItem.buttonText, priceStr, [UIColor whiteColor], fontName, fontSize);
            }
            else
            {
                NSString* productId = [productManager getProductIdFromDiscountId:product.productId];
                //NSString* origProductId = [productManager getProductIdFromDiscountId:product.productId];
                SEProduct* oldProduct = [productManager getProduct:productId];
                if(oldProduct)
                {
                    NSString* oldPrice = [NSString stringWithFormat:@"%1.2f $", oldProduct.productPrice];
                    setLabelFont(newItem.buttonText, oldPrice,  [UIColor colorWithRed:115.0/255 green:115.0/255 blue:115.0/255 alpha:1], fontName, 20);
                    
                    NSString* newPrice = [NSString stringWithFormat:@"%1.2f $", product.productPrice];
                    setLabelFont(newItem.buttonText2, newPrice, [UIColor colorWithRed:255.0/255 green:246.0/255 blue:0/255 alpha:1], fontName, fontSize + 10);
                }
                else 
                {
                    NSString* newPrice = [NSString stringWithFormat:@"%1.2f $", product.productPrice];
                    setLabelFont(newItem.buttonText, newPrice, [UIColor colorWithRed:255.0/255 green:246.0/255 blue:0/255 alpha:1], fontName, fontSize);
                }

            }
            [newItem setContentImage:[product getIcon]];
            startyInGroup += newItem.frame.size.height + itemSpacing;
            item = newItem;

            [item.button addTarget:self action:@selector(itemButtonHandler:) forControlEvents:UIControlEventTouchUpInside];
            if([product isDiscount])
            {
                UIImage* image = [mViewNav.mResLoader getImage:@"DiscountImage"];
                NSString* productID = [productManager getProductIdFromDiscountId:product.productId];
                SEProduct* newProduct = [productManager getProduct:productID];
                if(newProduct == nil)
                {
                    image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"PromotionIcon"];
                }
                [item setDiscountImage:image];
            }
            /*
            if(product.startDate != nil)
            {
                UIImage* normalImage = nil;
                UIImage* selectImage = nil;
                [item.button setBackgroundImage:normalImage forState:UIControlStateNormal];
                [item.button setBackgroundImage:selectImage forState:UIControlStateHighlighted];
            }
             */
            [groupBackground addSubview:item];
            [item release];
            item.productId = product.productId;
            [item setTarget:self action:@selector(storeItemTouchHandler:)];
            //item.backgroundColor = [UIColor redColor];
            [mStoreItems addObject:item];
            if([SEUserDefaultManager isFunctionOK:product.func] == NO && isBasicSettingItem == NO)
            {
                [item setLock:YES type:LOCK_BUY];
            }
            else
            {
                if(isBasicSettingItem == YES && [SEUserDefaultManager isFunctionOK:product.func] == YES)
                {
                    [item setBuied:YES];
                    item.hasUpdatePrice = YES;
                }
                if([SEUserDefaultManager isProductBuied:product.productId] && [[PhotoFrameAppDelegate getProductManager] isProductTypeOneTimeBuy:product.productType])
                {
                    [item setBuied:YES];
                    item.hasUpdatePrice = YES;
                }
                else 
                {
                    
                }
            }
            
        }
        //startyInGroup -= itemSpacing;
        groupBackground.frame = CGRectMake(groupBackground.frame.origin.x, groupBackground.frame.origin.y, groupBackground.frame.size.width, startyInGroup + 7);
        starty += groupBackground.frame.size.height + groupSpacing;
    }
    /*
    mTotalLabel = [[SEOptionsLabel alloc] initWithFrame:CGRectMake(startx, starty, width - leftPadding - rightPadding, labelHeight)];
    [mView addSubview:mTotalLabel];
    [mTotalLabel release];
    [self setImage:mTotalLabel.background name:@"AppStoreLabelBg"];
    setLabelFont(mTotalLabel.label, @"Total", [UIColor whiteColor], fontName, fontSize);
    starty += mTotalLabel.frame.size.height;
    
    mTotalBlank = [[SEOptionsImageView alloc] initWithFrame:CGRectMake(startx, starty, 390, 82)];
    [self setImage:mTotalBlank.imageView name:@"AppStoreTotalBlank"];
    [mView addSubview:mTotalBlank];
    [mTotalBlank release];
    
    mTotalPriceLabel = [[SEOptionsLabel alloc] initWithFrame:CGRectMake(startx + mTotalBlank.frame.size.width + 10, starty, 220, 82)];
    [mView addSubview:mTotalPriceLabel];
    [mTotalPriceLabel release];
    [self setImage:mTotalPriceLabel.background name:@"AppStoreTotalBlank"];
    mTotalPriceLabel.label.textAlignment = UITextAlignmentCenter;
    setLabelFont(mTotalPriceLabel.label, @"0", [UIColor whiteColor], fontName, fontSize);
    starty += 82 + 10;
    
    mSupportLabel = [[SEOptionsLabel alloc] initWithFrame:CGRectMake(startx, starty, 390, 82)];
    [mView addSubview:mSupportLabel];
    [mSupportLabel release];
    [self setImage:mSupportLabel.background name:@"AppStoreTotalBlank"];
    setLabelFont(mSupportLabel.label, @"Thank you for your support", [UIColor blackColor], fontName, fontSize);
    
    mSubmitButton = [[SEOptionsButton alloc] initWithFrame:CGRectMake(startx + mSupportLabel.frame.size.width + 10, starty, 220, 82)];
    [mView addSubview:mSubmitButton];
    [mSubmitButton release];
    setLabelFont(mSubmitButton.buttonText, @"Submit", [UIColor blackColor], fontName
                 , fontSize);
    [mSubmitButton.button addTarget:self action:@selector(submitButtonHandler:) forControlEvents:UIControlEventTouchUpInside];
    starty += mSubmitButton.frame.size.height + 10;
     */
    if(mView.frame.size.height < starty)
    {
        mView.frame = CGRectMake(mView.frame.origin.x, mView.frame.origin.y, mView.frame.size.width, starty );
    }
}
- (void) handleBasicSettingFunc: (SEOptionsStoreItem*)item
{
    NSLog(@"handle basic func");
    SEProductManager* productManager = [PhotoFrameAppDelegate getProductManager];
    for(SEOptionsStoreItem* sitem in mStoreItems)
    {
        if(item != sitem)
        {
            SEProduct* p = [productManager getProduct:sitem.productId];
            if(p == nil)
            {
                p = [productManager getDiscount:sitem.productId];
            }
            if(p)
            {
                enum USER_FUNCTION f = p.func;
                if([SEUserDefaultManager isFunctionOK:f] && [sitem isLocked] == YES)
                {
                    [sitem setLock:NO type:LOCK_BUY];
                }
            }
        }
    }
    [item setBuied:YES];
}
- (void) handleBrushSettingFunc: (SEOptionsStoreItem*)item
{
    NSLog(@"handle brush seting");
    [item setBuied:YES];
}
- (void) handleAddMusicFunc: (SEOptionsStoreItem*)item
{
    NSLog(@"handle add music func");
}
- (void) handleLevelUpMaxFunc: (SEOptionsStoreItem*)item
{
    NSLog(@"handle level up max");
    SEUserData* maxData = [[mViewNav getUserUpgrade] getMaxUserData];
    UserInfo* userInfo = [mViewNav getUserInfo];
    int currentLevel = [[mViewNav getUserUpgrade] getLevelFromExpNum:[mViewNav.mSystemDataManager.exppointnum intValue]];
    if(currentLevel >= maxData.level)
    {
        [item setBuied:YES];
    }
    else
    {
        [[mViewNav getUserUpgrade] addExp:20000];
    }
}
- (void) handleBuyBrushFunc: (SEOptionsStoreItem*)item
{
    NSLog(@"handle buy brush");
    [item setBuied:YES];
    SEProductManager* productManager = [PhotoFrameAppDelegate getProductManager];
    for(SEOptionsStoreItem* sitem in mStoreItems)
    {
        if(item != sitem)
        {
            SEProduct* p = [productManager getProduct:sitem.productId];
            NSLog(@"product = %@, %d", p.productId, p.productType);
            if(p.productType == PRODUCT_BRUSH && [SEUserDefaultManager isProductBuied:p.productId])
            {
                [sitem setBuied:YES];
            }
        }
    }
    SEProduct* product = [[PhotoFrameAppDelegate getProductManager] getProduct:item.productId];
    //NSArray* array = [NSArray arrayWithObjects:product.imageName, nil];
    NSArray* contentArray = product.contentNames;
    for(int i = 0 ; i < contentArray.count ; i++)
    {
        NSString* str = [contentArray objectAtIndex:i];
        NSLog(@"brush = %@", str);
    }
    [mViewNav handleBrushBuied:contentArray];
}
- (void) handleBuyTimeStyleFunc: (SEOptionsStoreItem*)item
{
    NSLog(@"handle buy time style");
    [item setBuied:YES];
    SEProduct* product = [[PhotoFrameAppDelegate getProductManager] getProduct:item.productId];
    NSArray* array = [product getStringSepByStar:product.imageName];
    [mViewNav handleTimeBuied:array];
}
- (void) handleAddImageFunc: (SEOptionsStoreItem*)item
{
    NSLog(@"handle add image func");
}
- (SEL) getHandler:(enum USER_FUNCTION)func
{
    for(int i = 0 ; i < mFunctionHandlerArray.count ; i++)
    {
        SESELValue* v = [mFunctionHandlerArray objectAtIndex:i];
        if(v.func == func)
            return [v getSelector];
    }
    assert(0);
    return NULL;
}
- (void) handleBuiedItem: (SEOptionsStoreItem*)item
{
    SEProductManager* productManager = [PhotoFrameAppDelegate getProductManager];
    SEProduct* product = [productManager getProduct:item.productId];
    if(product == nil)
    {
        product = [productManager getDiscount:item.productId];
    }
    if(product)
    {
        enum USER_FUNCTION func = product.func;
        SEL selector = [self getHandler:func];
        [self performSelector:selector withObject:item];
    }
    else
    {
        NSLog(@"can not find product : %@",item.productId);
    }
}
- (void) handleTransactionOK: (NSString*)productId
{
    for(SEOptionsStoreItem* item in mStoreItems)
    {
        if([item.productId isEqualToString:productId])
        {
            [self handleBuiedItem:item];
            break;
        }
    }
    //SEInAppPurchaseTransactionObserver* transaction = [PhotoFrameAppDelegate getPurchaseTransactionObserver];
    //[[PhotoFrameAppDelegate getViewNavigator] hideLoadingView];
}
- (SKProduct*) getBrushSettingProduct: (NSArray*)products
{
    SEProductManager* productManager = [PhotoFrameAppDelegate getProductManager];
    NSArray* brushSettingProductArray = [productManager getProductByFunc:BRUSH_SETTING_FUNC];
    assert(brushSettingProductArray.count == 1);
    SEProduct* brushSettingProduct = [brushSettingProductArray objectAtIndex:0];
    for(int i = 0 ; i < products.count ; i++)
    {
        SKProduct* product = [products objectAtIndex:i];
        if([product.productIdentifier isEqualToString:brushSettingProduct.productId])
            return product;
    }
    return nil;
}
- (BOOL) isDiscountPrice: (SKProduct*)skProduct : (SEProduct*)product : (SKProduct*)skBrushSettingProduct
{
    if(product != nil && product.func == BASIC_FUNC)
    {
        double basicSettingUpdatePrice = [skProduct.price doubleValue];
        double basicSettingDiscountPrice = [skBrushSettingProduct.price doubleValue];
        double e = fabs(basicSettingDiscountPrice - basicSettingUpdatePrice);
        if(e < 0.1)
            return YES;
        else
            return NO;
    }
    return NO;
}
- (void) handleRequstProductOK: (NSArray*)products : (BOOL) requestOK
{
    if(requestOK == NO)
    {
        
        for(SEOptionsStoreItem* item in mStoreItems)
        {
            [item endConnection];
            if([SEUserDefaultManager isProductBuied:item.productId] == NO)
            {
                [item setUpdatePriceFailed:@"update price failed"];
            }
        }
    }
    else
    {
        BOOL (^productIdInProducts)(NSString*) = ^BOOL(NSString* str){
            for(int i = 0 ; i < products.count ; i++)
            {
                SKProduct* p = [products objectAtIndex:i];
                if([p.productIdentifier isEqualToString:str])
                    return YES;
            }
            return NO;
        };
        
        SKProduct* (^findProductById)(NSString*) = ^SKProduct*(NSString* str)
        {
            for(int i = 0 ; i < products.count ; i++)
            {
                SKProduct* p = [products objectAtIndex:i];
                if([p.productIdentifier isEqualToString:str])
                    return p;
            }
            return nil;
        };
        SEProductManager* productManager = [PhotoFrameAppDelegate getProductManager];

        for(SEOptionsStoreItem* item in mStoreItems)
        {
            [item endConnection];
            item.hasUpdatePrice = YES;
            SKProduct* product = findProductById(item.productId);
            SEProduct* p = [productManager getDiscount:item.productId];
            if(p == nil)
            {
                p = [productManager getProduct:item.productId];
            }
            if(product != nil && p != nil)
            {
                BOOL isDiscount = NO;
                if(p.func == BASIC_FUNC)
                {
                    SKProduct* brushSettingProduct = [self getBrushSettingProduct:products];
                    assert(brushSettingProduct != nil);
                    isDiscount = [self isDiscountPrice:product :p :brushSettingProduct];
                }
                
                double updatePrice = [product.price doubleValue];
                double origPrice = 0;
                NSLocale* priceLocale = product.priceLocale;
        
                NSNumberFormatter *numberFormatter = [[NSNumberFormatter alloc] init];
                [numberFormatter setFormatterBehavior:NSNumberFormatterBehavior10_4];
                [numberFormatter setNumberStyle:NSNumberFormatterCurrencyStyle];
                [numberFormatter setLocale:priceLocale];
                if(isDiscount)
                {
                    NSString *updatePriceStr = [numberFormatter stringFromNumber:[NSDecimalNumber numberWithDouble:updatePrice]];
                    //NSString* origPriceStr = [numberFormatter stringFromNumber:[NSDecimalNumber numberWithDouble:origPrice]];
                    [item addDiscountText];
                    item.buttonText.text = updatePriceStr;
                    NSDate* currentDate = [NSDate date];
                    NSDate* expiredDate = [productManager stringToDate:[SESystemConfig getBasicSettingCalculateExpiredDate]];
                    NSTimeInterval t = [currentDate timeIntervalSince1970];
                    NSTimeInterval t1 = [expiredDate timeIntervalSince1970];
                    if(t < t1)
                    {
                        [item setExpiredDate:[SESystemConfig getBasicSettingExpiredDate]];
                    }
                    //item.buttonText2.text = updatePriceStr;
                }
                else
                {
                    NSString *formattedString = [numberFormatter stringFromNumber:[NSDecimalNumber numberWithDouble:updatePrice]];
                    item.buttonText.text = formattedString;
                }
                [numberFormatter release];
                
            }
            
        }
    }
}
/*
- (void) handleRequstProductOK: (NSArray*)productIdArray : (NSArray*)priceArray : (BOOL) requestOK : (NSArray*)priceLocaleArray
{
    SEProductManager* productManager = [PhotoFrameAppDelegate getProductManager];
    if(requestOK == NO)
    {
        for(SEOptionsStoreItem* item in mStoreItems)
        {
            if([SEUserDefaultManager isProductBuied:item.productId] == NO)
            {
                [item setUpdatePriceFailed:@"update price failed"];
            }
        }
    }
    else
    {
        BOOL (^productInProductIdArray)(NSString*, NSArray*) = ^BOOL(NSString* str, NSArray* idArray){
            for(int i = 0 ; i < idArray.count ; i++)
            {
                NSString* pid = [idArray objectAtIndex:i];
                if([pid isEqualToString:str])
                    return YES;
            }
            return NO;
        };
        for(SEOptionsStoreItem* item in mStoreItems)
        {
            if(productInProductIdArray(item.productId, productIdArray))
            {
                [item endConnection];
                SEProduct* p = [productManager getDiscount:productId];
                if(p == nil)
                {
                    p = [productManager getProduct:productId];
                }
                if(requestOK)
                {
                    double updatePrice = price.doubleValue;
                    double currentPrice = p.productPrice;
                    NSNumberFormatter *numberFormatter = [[NSNumberFormatter alloc] init];
                    [numberFormatter setFormatterBehavior:NSNumberFormatterBehaviorDefault];
                    [numberFormatter setNumberStyle:NSNumberFormatterCurrencyStyle];
                    [numberFormatter setLocale:priceLocale];
                    NSString *formattedString = [numberFormatter stringFromNumber:[NSDecimalNumber numberWithDouble:currentPrice]];
                    currentPrice = [formattedString doubleValue];
                    double e = fabsf(currentPrice - updatePrice);
                    if(e < 0.01)
                    {
                        [item setUpdatePriceFailed:@""];
                    }
                    else
                    {
                        [item addDiscountText: updatePrice : currentPrice];
                    }
                }
                else
                {
                    [item setUpdatePriceFailed:@"update price failed"];
                }
            }
        }
    }
}
 */
- (void) initFunctionHandler
{
    SESELValue* v = [[SESELValue alloc] init];
    v.func = BASIC_FUNC;
    [v setSelector:@selector(handleBasicSettingFunc:)];
    [mFunctionHandlerArray addObject:v];
    [v release];
    
    v = [[SESELValue alloc] init];
    v.func = BRUSH_SETTING_FUNC;
    [v setSelector:@selector(handleBrushSettingFunc:)];
    [mFunctionHandlerArray addObject:v];
    [v release];
    
    v = [[SESELValue alloc] init];
    v.func = LEVELUP_MAX_FUNC;
    [v setSelector:@selector(handleLevelUpMaxFunc:)];
    [mFunctionHandlerArray addObject:v];
    [v release];
    
    v = [[SESELValue alloc] init];
    v.func = ADD_MUSIC_NUMBER_FUNC;
    [v setSelector:@selector(handleAddMusicFunc:)];
    [mFunctionHandlerArray addObject:v];
    [v release];
    
    v = [[SESELValue alloc] init];
    v.func = ADD_IMAGE_NUMBER_FUNC;
    [v setSelector:@selector(handleAddImageFunc:)];
    [mFunctionHandlerArray addObject:v];
    [v release];
    
    v = [[SESELValue alloc] init];
    v.func = BUY_BRUSHES_FUNC;
    [v setSelector:@selector(handleBuyBrushFunc:)];
    [mFunctionHandlerArray addObject:v];
    [v release];
    
    v = [[SESELValue alloc] init];
    v.func = BUY_TIME_STYLE_FUNC;
    [v setSelector:@selector(handleBuyTimeStyleFunc:)];
    [mFunctionHandlerArray addObject:v];
    [v release];
}
- (void) requestProductNotificationHandler: (NSNotification*)n
{
    NSLog(@"requestProductNotificationHandler");
    NSDictionary* userInfo = n.userInfo;
    NSArray* products = [userInfo valueForKey:@"products"];
    //NSArray* priceArray = [userInfo valueForKey:@"price"];
    NSNumber* requestOK = [userInfo valueForKey:@"requestOK"];
    //NSArray* priceLocaleArray = [userInfo valueForKey:@"priceLocale"];
    BOOL ok = [requestOK boolValue];
    //[self handleRequstProductOK: productIdArray : priceArray : ok : priceLocaleArray];
    [self handleRequstProductOK: products: ok];
}
- (void) hideLoadingView
{
    [[PhotoFrameAppDelegate getViewNavigator] hideLoadingView];
    [mConnectionTimer invalidate];
    mConnectionTimer = nil;
}
- (void) purchaseProductFailedNotificationHandler: (NSNotification*) n
{
    [self hideLoadingView];
    NSDictionary* userInfo = n.userInfo;
    NSString* productId = [userInfo valueForKey:@"productId"];
    NSString* alertTitle = @"Warning!";
    NSString* alertMsg = @"Your purchase product error, please try again!";
    /*
    UIAlertView *alertView = [[UIAlertView alloc] initWithTitle:alertTitle
                                                        message:alertMsg
                                                       delegate:nil
                                              cancelButtonTitle:@"OK"
                                              otherButtonTitles:nil];
    [alertView show];
     */
    [self showAlert:alertTitle msg:alertMsg];
}
- (void) purchaseProductRestoreNotificationHandler: (NSNotification*)n
{
    NSDictionary* userInfo = n.userInfo;
    NSString* productId = [userInfo valueForKey:@"productId"];
    [self handleTransactionOK:productId];
    [self hideLoadingView];
}
- (void) purchaseProductSuccessNotificationHandler: (NSNotification*)n
{
    NSDictionary* userInfo = n.userInfo;
    NSString* productId = [userInfo valueForKey:@"productId"];
    [self handleTransactionOK:productId];
    [self hideLoadingView];
}
- (void) purchaseProductCancelNotificationHandler: (NSNotification*)n
{
    [self hideLoadingView];
}
- (void) updateItemPrice
{
    SEProductManager* productManager = [PhotoFrameAppDelegate getProductManager];
    NSMutableSet* productIdSets = [NSMutableSet set];
    NSArray* brushSettingProductArray = [productManager getProductByFunc:BRUSH_SETTING_FUNC];
    assert(brushSettingProductArray.count == 1);
    SEProduct* brushSettingProduct = [brushSettingProductArray objectAtIndex:0];
    assert(brushSettingProduct != nil);
    for(SEOptionsStoreItem* sitem in mStoreItems)
    {
        SEProduct* p = [productManager getDiscount:sitem.productId];
        if(p == nil)
        {
            p = [productManager getProduct:sitem.productId];
        }
        if([SEUserDefaultManager isProductBuied:p.productId] == NO)
        {
            [sitem startConnection];
            sitem.hasUpdatePrice = NO;
            if(p != nil && p.func == BASIC_FUNC && [SEUserDefaultManager isFunctionOK:p.func] == NO)
            {
                [productIdSets addObject:p.productId];
                [productIdSets addObject:brushSettingProduct.productId];
            }
            else
            {
                [productIdSets addObject:p.productId];
            }
        }
    }

    if(productIdSets.count > 0)
    {
        NSArray* arrayObjects = [productIdSets allObjects];
        for(int i = 0 ; i < productIdSets.count ; i++)
        {
            NSString* str = [arrayObjects objectAtIndex:i];
            NSLog(@"product str = %@", str);
        }
        SEInAppPurchaseProduct* productRequect = [[SEInAppPurchaseProduct alloc] initWithProductId:nil];
        productRequect.mProductIdArray = [productIdSets allObjects];
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(requestProductNotificationHandler:) name:[SESystemConfig getProductRequectNotificationName] object:productRequect];
        [productRequect requestProductData];
    }
}
-(UIView*) getView
{
    if(mView == nil)
    {
        mStoreItems = [[NSMutableArray arrayWithArray:[NSArray array]] retain];;
        mFunctionHandlerArray = [[NSMutableArray arrayWithArray:[NSArray array]] retain];
        [self initFunctionHandler];
        mView = [[[NSBundle mainBundle] loadNibNamed:@"Options7" owner:self options:nil] lastObject];
        [mView retain];
        mView.userInteractionEnabled = YES;
        mView.backgroundColor = [UIColor clearColor];
        [self createChild];
        //[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(requestProductNotificationHandler:) name:[SESystemConfig getProductRequectNotificationName] object:nil];
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(purchaseProductFailedNotificationHandler:) name:[SESystemConfig getProductPurchaseFailedNotificationName] object:nil];
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(purchaseProductSuccessNotificationHandler:) name:[SESystemConfig getProductPurchaseSuccessNotificationName] object:nil];
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(purchaseProductRestoreNotificationHandler:) name:[SESystemConfig getProductPurchaseRestoreNotificationName] object:nil];
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(purchaseProductCancelNotificationHandler:) name:[SESystemConfig getproductPurchaseCancelNotificationName] object:nil];
        [self updateItemPrice];
    }
    return mView;
}
- (void) setData
{
    
}
- (float) getHeight
{
    return mView.frame.size.height;
}
- (void)dealloc
{
    NSLog(@"AppStore SettingView dealloc");
    [mStoreItems release];
    [mFunctionHandlerArray release];
    [[NSNotificationCenter defaultCenter] removeObserver:self];
    [mConnectionTimer invalidate];
    mConnectionTimer = nil;
    [super dealloc];
}
@end
//////////
/*
@implementation SEOptionsAppStoreSetting
@synthesize mPlayLabel;
@synthesize mBrushLabel;
@synthesize mlevelUpToMaxItem;
@synthesize mAddImageNumberItem;
@synthesize mAddMusicNumberItem;
@synthesize mTotalBlank;
@synthesize mTotalLabel;
@synthesize mTotalPriceLabel;
@synthesize mSubmitButton;
@synthesize mBasicItem;
@synthesize mBasicLabel;
@synthesize mSupportLabel;
@synthesize mBrushFunctionItem;
//- (BOOL) 
- (void) updateTotalPrice
{
    mTotalPriceLabel.label.text = [NSString stringWithFormat:@"%f", mTotalPrice];
}
- (void) addProduct: (NSString*) productId
{
    [mProductIdArray addObject:productId];
}
- (void) removeProduct: (NSString*) productId
{
    [mProductIdArray removeObject:productId];
}
- (int) getBrushIndex: (UIButton*) brushbutton
{
    UIView* v = brushbutton.superview;
    for(NSArray* array in mBrushList)
    {
        NSNumber* n = [array objectAtIndex:0];
        UIView* parent = [array objectAtIndex:1];
        if(parent == v)
        {
            return [n intValue];
        }
    }
    return -1;
}
- (void) setButtonBackground: (UIButton*)button normal:(NSString*) normal selected: (NSString*) selected
{
    UIImage* normalImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:normal];
    normalImage = [SEUtil imageWithCap:normalImage top:0.1 bottom:0.9 left:0.1 right:0.9];
    UIImage* selectImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:selected];
    selectImage = [SEUtil imageWithCap:selectImage top:0.1 bottom:0.9 left:0.1 right:0.9];
    [button setBackgroundImage:normalImage forState:UIControlStateNormal];
    [button setBackgroundImage:selectImage forState:UIControlStateSelected];
}
- (void) changeStoreItemBackground: (UIButton*)sender selected: (BOOL)b
{
    UIView* parent = sender.superview;
    if([parent isMemberOfClass:[SEOptionsStoreItem class]])
    {
        SEOptionsStoreItem* item = (SEOptionsStoreItem*)parent;
        if(b)
        {
            [self setImage:item.background name:@"OptionsStoreItemSelectedBg"];
            [self setButtonBackground:item.button normal:@"OptionsStoreItemSelectedButtonBg" selected:@"OptionsStoreItemSelectedButtonHBg"];
        }
        else
        {
            [self setImage: item.background name:@"AppStoreAddImageBg"];
            [self setButtonBackground:item.button normal:@"AppStoreButtonNormalBg" selected:@"AppStoreButtonSelectBg"];
        }
    }
    else if([parent isMemberOfClass:[SEOptionsStoreTwoLabelItem class]])
    {
        SEOptionsStoreTwoLabelItem* item = (SEOptionsStoreTwoLabelItem*)parent;
        if(b)
        {
            [self setImage:item.background name:@"OptionsStoreItemSelectedBg"];
            [self setButtonBackground:item.button normal:@"OptionsStoreItemSelectedButtonBg" selected:@"OptionsStoreItemSelectedButtonHBg"];
        }
        else
        {
            [self setImage: item.background name:@"AppStoreAddImageBg"];
            [self setButtonBackground:item.button normal:@"AppStoreButtonNormalBg" selected:@"AppStoreButtonSelectBg"];
        }
    }
}
- (void) brushSettingHandler: (UIButton*) sender
{
    NSString* brushFunc = @"com.thespeedsun.PhotoFrame.BrushSetting" ;
    if(sender.tag == 0)
    {
        sender.tag = 1;
        mTotalPrice += 0.99;
        [self addProduct:brushFunc];
        [self changeStoreItemBackground:sender selected:YES];
    }
    else {
        sender.tag = 0;
        mTotalPrice -= 0.99;
        [self removeProduct:brushFunc];
        [self changeStoreItemBackground:sender selected:NO];
    }
    [self updateTotalPrice];
}
- (void) basicInfoHandler: (UIButton*) sender
{
    NSLog(@"basic item view tap");
    NSString* basicFunc = @"com.thespeedsun.PhotoFrame.BasicSetting" ;
    if(sender.tag == 0)
    {
        sender.tag = 1;
        mTotalPrice += 0.99;
        [self addProduct:basicFunc];
        [self changeStoreItemBackground:sender selected:YES];
    }
    else {
        sender.tag = 0;
        mTotalPrice -= 0.99;
        [self removeProduct:basicFunc];
        [self changeStoreItemBackground:sender selected:NO];
    }
     [self updateTotalPrice];
}
- (void) brushHandler : (UIButton*) sender
{
    NSLog(@"tap view");
    int brushIndex = [self getBrushIndex:sender];
    NSLog(@"brush index = %d", brushIndex);
    assert(brushIndex != -1);
    NSString* newString = @"com.thespeedsun.PhotoFrame.brush00";
    NSString* ns = [newString stringByAppendingFormat:@"%d", brushIndex];
    if(sender.tag == 0)
    {
        sender.tag = 1;
        mTotalPrice += 0.99;
        [self addProduct:ns];
        [self changeStoreItemBackground:sender selected:YES];
    }
    else
    {
        sender.tag = 0;
        mTotalPrice -= 0.99;
        [self removeProduct:ns];
        [self changeStoreItemBackground:sender selected:NO];
    }
    [self updateTotalPrice];
}
- (void) levelUpHandler: (UIButton*)sender
{
    NSLog(@"level up");
    if(sender.tag == 0)
    {
        sender.tag = 1;
        mTotalPrice += 0.99;
        [self addProduct:@"com.thespeedsun.PhotoFrame.levelUpMax"];
        [self changeStoreItemBackground:sender selected:YES];
    }
    else {
        sender.tag = 0;
        mTotalPrice -= 0.99;
        [self removeProduct:@"com.thespeedsun.PhotoFrame.levelUpMax"];
        [self changeStoreItemBackground:sender selected:NO];
    }
    [self updateTotalPrice];
}
- (void) addImageHandler: (UIButton*)sender
{
    NSLog(@"add image");
    if(sender.tag == 0)
    {
        sender.tag = 1;
        mTotalPrice += 0.99;
        [self addProduct:@"com.thespeedsun.PhotoFrame.addImage101"];
        [self changeStoreItemBackground:sender selected:YES];
    }
    else {
        sender.tag = 0;
        mTotalPrice -= 0.99;
        [self removeProduct:@"com.thespeedsun.PhotoFrame.addImage101"];
        [self changeStoreItemBackground:sender selected:NO];
    }
    [self updateTotalPrice];
}
- (void) addMusicHandler: (UIButton*)sender
{
    NSLog(@"add music");
    if(sender.tag == 0)
    {
        sender.tag = 1;
        mTotalPrice += 0.99;
        [self addProduct:@"com.thespeedsun.PhotoFrame.addMusic101"];
        [self changeStoreItemBackground:sender selected:YES];
    }
    else {
        sender.tag = 0;
        mTotalPrice -= 0.99;
         [self removeProduct:@"com.thespeedsun.PhotoFrame.addMusic101"];
        [self changeStoreItemBackground:sender selected:NO];
    }
    [self updateTotalPrice];
}
- (void) updateStoreItemState
{

    NSString* str = @"Processing";
    if([mBasicItem isSelected])
    {
        mBasicItem.buttonText.text = str;
    }
    if([mBrushFunctionItem isSelected])
    {
        mBrushFunctionItem.buttonText.text = str;
    }
    if([mAddImageNumberItem isSelected])
    {
        mAddImageNumberItem.buttonText.text = str;
    }
    if([mAddMusicNumberItem isSelected])
    {
        mAddMusicNumberItem.buttonText.text = str;
    }
    for(NSArray* array in mBrushList)
    {
        SEOptionsStoreItem* item = [array objectAtIndex:1];
        if([item isSelected])
        {
            item.buttonText.text = str;
        }
    }
}
- (void) submitHandler: (UIButton*)sender
{
    NSLog(@"submit");
    for(int i = 0 ; i < mProductIdArray.count ; i++)
    {
        NSLog(@"product : %@", [mProductIdArray objectAtIndex:i]);
        [mProductTransaction addProduct:[mProductIdArray objectAtIndex:i]];
        [SEUserDefaultManager setProductProcessing:[mProductIdArray objectAtIndex:i]];
    }
    [self updateStoreItemState];
    [mProductTransaction requestAllProduct];
}
- (float) getHeight
{
    mView.frame = CGRectMake(mView.frame.origin.x, mView.frame.origin.y, mSubmitButton.frame.origin.y + mSubmitButton.frame.size.height, mView.frame.size.width);
    return mSubmitButton.frame.origin.y + mSubmitButton.frame.size.height;
}
 */
/*
- (UIView*) createBrush: (NSString*) brushName withStart: (CGRect) startRect
{
    float width = startRect.size.width - 10;
    float height = mAddImageNumberItem.frame.size.height;
    UIView* parentView = [[UIView alloc] initWithFrame:CGRectMake(5, startRect.origin.y + 5, width,height)];
    UIImageView* brushBg = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, width, height)];
    [parentView addSubview:brushBg];
    [brushBg release];
    [self setImage:brushBg name:@"AppStoreLevelUpToMaxButton"];
    
    UIImageView* brushIcon = [[UIImageView alloc] initWithFrame:CGRectMake(5, 5, 120, 60)];
    [parentView addSubview:brushIcon];
    [brushIcon release];
    //brushIcon.userInteractionEnabled = YES;
    //UITapGestureRecognizer* tap = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(tapHandler:)];
    //[brushIcon addGestureRecognizer:tap];
    //[tap release];
    const char* str = [brushName cStringUsingEncoding:NSUTF8StringEncoding];
    NSLog(@"## brush name len = %lu ##", strlen(str));
    ppm_t brush = {0, 0, NULL};
    ppm_load(str, &brush);
    CGImageRef imageRef = [SEUtil createCGImage:brush];
    UIImage* image = [UIImage imageWithCGImage:imageRef];
    CGImageRelease(imageRef);
    brushIcon.image = image;
    
    FontLabel* brushNameLabel = [[FontLabel alloc] initWithFrame:CGRectMake(125, 5, 200, 60)];
    brushNameLabel.text = brushName;
    [parentView addSubview:brushNameLabel];
    [brushNameLabel release];
    NSString* fontName = @"EyadBold";
    float fontSize = 20;
    setLabelFont(brushNameLabel, brushName, [UIColor whiteColor], fontName, fontSize);
    
    UIButton* priceButton = [[UIButton alloc] initWithFrame:CGRectMake(330, 5, 90, 60)];
    UIImage* normalImage = [mViewNav.mResLoader getImage:@"AppStoreButtonNormalBg"];
    UIImage* selectImage = [mViewNav.mResLoader getImage:@"AppStoreButtonSelectBg"];
    [priceButton setImage:normalImage forState:UIControlStateNormal];
    [priceButton setImage:selectImage forState:UIControlStateSelected];
    [parentView addSubview:priceButton];
    [priceButton addTarget:self action:@selector(brushHandler:) forControlEvents:UIControlEventTouchUpInside];
    [priceButton release];
    [parentView autorelease];
    //[self setImage:priceButton name:@"AppStoreLevelUpToMaxButton"];
    return parentView;
}
 */
/*
- (void) addBrush
{
    int brushCount = 4;
    
    CGRect startRect = mBrushLabel.frame;
    float height = mAddImageNumberItem.frame.size.height;
    startRect.origin.y += mBrushLabel.frame.size.height;
    for(int i = 0 ; i < brushCount ; i++)
    {
        SEOptionsStoreItem* view = [[SEOptionsStoreItem alloc] initWithFrame:startRect];
        [mView addSubview:view];
        [self setImage:view.background name:@"AppStoreLevelUpToMaxButton"];
        
        //const char* str = [brushArray[i] cStringUsingEncoding:NSUTF8StringEncoding];
        //NSLog(@"## brush name len = %lu ##", strlen(str));
        ppm_t brush = {0, 0, NULL};
        ppm_load(str, &brush);
        CGImageRef imageRef = [SEUtil createCGImage:brush];
        UIImage* image = [UIImage imageWithCGImage:imageRef];
        CGImageRelease(imageRef);
        view.content.image = image;
        
        setLabelFont(view.label, brushArray[i], [UIColor whiteColor], OPTIONS_LABEL_FONT_NAME, OPTIONS_LABEL_FONT_SIZE);
        
        setLabelFont(view.buttonText, @"0.99$", [UIColor whiteColor], OPTIONS_LABEL_FONT_NAME, OPTIONS_LABEL_FONT_SIZE);
        
        NSArray* array = [NSArray arrayWithObjects:[NSNumber numberWithInt:i], view, nil];
        [mBrushList addObject:array];
        startRect.origin.y += view.frame.size.height;
    }
}
 */
/*
- (void) adjustView: (UIView*) view delta: (float) delta
{
    view.frame = CGRectMake(view.frame.origin.x, view.frame.origin.y + delta, view.frame.size.width, view.frame.size.height);
}
- (void) adjustTotalView
{
    UIView* view = [[mBrushList objectAtIndex:mBrushList.count - 1] objectAtIndex:1];
    float starty = view.frame.origin.y + view.frame.size.height;
    float delta = starty - mBrushLabel.frame.origin.y - mBrushLabel.frame.size.height;
    [self adjustView:mTotalPriceLabel delta:delta];
    [self adjustView:mTotalBlank delta:delta];
    [self adjustView:mTotalLabel delta:delta];
    [self adjustView:mSupportLabel delta:delta];
    [self adjustView:mSubmitButton delta:delta];
}
- (void) setLabel
{
    NSString* fontName = [SEUtil getFontName];
    float fontSize = 30;
    setLabelFont(mPlayLabel.label, @"Play", [UIColor whiteColor], fontName, fontSize);
    setLabelFont(mlevelUpToMaxItem.label, @"Level up To", [UIColor grayColor], fontName, fontSize);
    setLabelFont(mlevelUpToMaxItem.label2, @" Max ", [UIColor grayColor], fontName, fontSize);
    setLabelFont(mAddImageNumberItem.label2, @"Add Image Upper Limit", [UIColor grayColor], fontName, fontSize);
    setLabelFont(mAddImageNumberItem.label, @"+10", [UIColor yellowColor], fontName, fontSize);
    setLabelFont(mAddMusicNumberItem.label2, @"Add Music Upper Limit", [UIColor grayColor], fontName, fontSize);
    setLabelFont(mAddMusicNumberItem.label, @"+10", [UIColor yellowColor], fontName, fontSize);
    setLabelFont(mBrushLabel.label, @"Brush", [UIColor whiteColor], fontName, fontSize);
    setLabelFont(mTotalLabel.label, @"Total", [UIColor whiteColor], fontName, fontSize);
    setLabelFont(mSupportLabel.label, @"Thank you for your support", [UIColor blackColor], fontName, fontSize);
    setLabelFont(mSubmitButton.buttonText, @"Submit", [UIColor blackColor], fontName, fontSize);
    setLabelFont(mTotalPriceLabel.label, @"0", [UIColor blackColor], fontName, fontSize);
    setLabelFont(mAddImageNumberItem.buttonText, @"0.99$", [UIColor whiteColor], fontName, fontSize);
    setLabelFont(mlevelUpToMaxItem.buttonText, @"0.99$", [UIColor whiteColor], fontName, fontSize);
    setLabelFont(mAddMusicNumberItem.buttonText, @"0.99$", [UIColor whiteColor], fontName, fontSize);
    setLabelFont(mBasicLabel.label, @"Basic Function", [UIColor whiteColor], fontName, fontSize);
    setLabelFont(mBasicItem.label, @"start up basic setting", [UIColor whiteColor], fontName, fontSize);
    setLabelFont(mBasicItem.buttonText, @"0.99$", [UIColor whiteColor], fontName, fontSize);
    setLabelFont(mBrushFunctionItem.buttonText, @"0.99$", [UIColor whiteColor], fontName, fontSize);
    setLabelFont(mBrushFunctionItem.label, @"brush setting", [UIColor whiteColor], fontName, fontSize);
}
- (void) handleTransaction
{
    NSLog(@"handle Transaction");
    if([mBasicItem isSelected])
    {
        [mBasicItem setBuied: YES];
        [self setImage:mBasicItem.background name:@"AppStorePlayBg"];
        [mlevelUpToMaxItem setLock:NO type:LOCK_BUY];
        [mBrushFunctionItem setLock:NO type:LOCK_BUY];
        [mAddImageNumberItem setLock:NO type:LOCK_BUY];
        [mAddMusicNumberItem setLock:NO type:LOCK_BUY];
        for(NSArray* array in mBrushList)
        {
            SEOptionsStoreItem* item = [array objectAtIndex:1];
            [item setLock:NO type:LOCK_BUY];
        }
        
    }
    if([mBrushFunctionItem isSelected])
    {
        [mBrushFunctionItem setBuied:YES];
        [self setImage:mBrushFunctionItem.background name:@"AppStorePlayBg"];
    }
    if([mAddImageNumberItem isSelected])
    {
        [mAddImageNumberItem setBuied:YES];
        [self setImage:mAddImageNumberItem.background name:@"AppStorePlayBg"];
    }
    if([mAddMusicNumberItem isSelected])
    {
        [mAddMusicNumberItem setBuied:YES];
        [self setImage:mAddMusicNumberItem.background name:@"AppStorePlayBg"];
    }
    for(NSArray* array in mBrushList)
    {
        SEOptionsStoreItem* item = [array objectAtIndex:1];
        if([item isSelected])
        {
            [item setBuied:YES];
            [self setImage:item.background name:@"AppStorePlayBg"];
        }
    }
    [mBasicItem setNeedsDisplay];
    [mBrushFunctionItem setNeedsDisplay];
    [mAddImageNumberItem setNeedsDisplay];
    [mAddMusicNumberItem setNeedsDisplay];
    for(NSArray* array in mBrushList)
    {
        SEOptionsStoreItem* item = [array objectAtIndex:1];
        [item setNeedsDisplay];
    }
}
-(UIView*) getView
{
    if(mView == nil)
    {
        //BOOL s = [[NSUserDefaults standardUserDefaults] boolForKey:@"com.thespeedsun.PhotoFrame.addMusic101" ];
        mBrushList = [[NSMutableArray alloc] initWithArray:[NSArray array]];
        mProductIdArray = [[NSMutableArray alloc] initWithArray:[NSArray array]];
        mProductTransaction = [[SEInAppPurchaseTransactionObserver alloc] init];
        [mProductTransaction setTarget:self action:@selector(handleTransaction)];
        [mProductTransaction loadStore];
        mView = [[[NSBundle mainBundle] loadNibNamed:@"Options6" owner:self options:nil] lastObject];
        [mView retain];
        mView.userInteractionEnabled = YES;
        //[self setImage:mPlayBg name:@"AppStorePlayBg"];
        [self setImage:mPlayLabel.background name:@"AppStorePlayBg"];
        [self setImage:mlevelUpToMaxItem.background name:@"AppStoreLevelUpToMaxBg"];
        [self setImage: mlevelUpToMaxItem.content name:@"AppStoreLevelUpToMaxIcon"];
        //[self setImage:mLevelUpToMaxButton name:@"AppStoreLevelUpToMaxButton"];
        [self setImage:mAddImageNumberItem.background name:@"AppStoreAddImageBg"];
        [self setImage:mAddImageNumberItem.content name:@"AppStoreAddImageIcon"];
        //[self setImage:mAddImageButton name:@"AppStoreAddImageButton"];
        [self setImage:mAddMusicNumberItem.background name:@"AppStoreAddMusicBg"];
        [self setImage:mAddMusicNumberItem.content name:@"AppStoreAddMusicIcon"];
        //[self setImage:mAddMusicButton name:@"AppStoreAddMusicButton"];
        [self setImage:mBrushLabel.background name:@"AppStoreBrushBg"];
        [self setImage:mTotalLabel.background name:@"AppStoreTotalBg"];
        [self setImage:mTotalBlank name:@"AppStoreTotalBlank"];
        [self setImage:mTotalPriceLabel.background name:@"AppStoreTotalNumber"];
        [self setImage:mSupportLabel.background name:@"AppStoreSuppoertBg"];
        [self setImage:mBasicLabel.background name:@"AppStorePlayBg"];
        [self setImage:mBasicItem.background name:@"AppStorePlayBg"];
        [self setImage:mBrushFunctionItem.background name:@"AppStorePlayBg"];
        //[self setImage:mSubmitButton name:@"AppStoreSubmit"];
        //[self addBrush];
        [self adjustTotalView];
        [self setLabel];
    }
    return mView;
}
- (void) dealloc
{
    [mPlayLabel release];
    [mlevelUpToMaxItem release];
    [mAddImageNumberItem release];
    [mAddMusicNumberItem release];
    [mBrushLabel release];
    
    [mTotalLabel release];
    [mSupportLabel release];
    [mSubmitButton release ];
    [mTotalPriceLabel release];
    [mTotalBlank release];
    [mBrushList release];
    [mProductIdArray release];
    [mProductTransaction release];
    [mBasicItem release];
    [mBasicLabel release];
    [super dealloc];
}
- (void) setData
{
    [mBasicItem setLock:NO type:LOCK_BUY];
    if([SEUserDefaultManager isFunctionOK:LEVELUP_MAX_FUNC] == NO)
        [mlevelUpToMaxItem setLock:YES type:LOCK_BUY];
    else {
        [mlevelUpToMaxItem setLock:NO type:LOCK_BUY];
    }
    //UIImage* image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"OptionsLockBuy"];
    //[mlevelUpToMaxItem setLockImage:image];
    if([SEUserDefaultManager isFunctionOK:ADD_IMAGE_NUMBER_FUNC] == NO)
    {
        [mAddImageNumberItem setLock:YES type:LOCK_BUY];
    }
    else {
        [mAddImageNumberItem setLock:NO type:LOCK_BUY];
    }

    if([SEUserDefaultManager isFunctionOK:ADD_MUSIC_NUMBER_FUNC] == NO)
    {
        [mAddMusicNumberItem setLock:YES type:LOCK_BUY];
    }
    else 
    {
        [mAddMusicNumberItem setLock:NO type:LOCK_BUY];
    }
    if([SEUserDefaultManager isFunctionOK:BASIC_FUNC] == NO)
    {
        [mBrushFunctionItem setLock:YES type:LOCK_BUY];
    }
    else
    {
        [mBrushFunctionItem setLock:NO type:LOCK_BUY];    
    }

    BOOL canBuyBrushes = [SEUserDefaultManager isFunctionOK:BUY_BRUSHES_FUNC];
    for(NSArray* array in mBrushList)
    {
        SEOptionsStoreItem* item = [array objectAtIndex:1];
        [item.button addTarget:self action:@selector(brushHandler:) forControlEvents:UIControlEventTouchUpInside];
        if(canBuyBrushes == NO)
        {
            [item setLock:YES type:LOCK_BUY];
        }
        else 
        {
            [item setLock:NO type:LOCK_BUY];
        }
    }
    [mBasicItem.button addTarget:self action:@selector(basicInfoHandler:) forControlEvents:UIControlEventTouchUpInside];
    [mlevelUpToMaxItem.button addTarget:self action:@selector(levelUpHandler:) forControlEvents:UIControlEventTouchUpInside];
    [mAddImageNumberItem.button addTarget:self action:@selector(addImageHandler:) forControlEvents:UIControlEventTouchUpInside];
    [mAddMusicNumberItem.button addTarget:self action:@selector(addMusicHandler:) forControlEvents:UIControlEventTouchUpInside];
    [mSubmitButton.button addTarget:self action:@selector(submitHandler:) forControlEvents:UIControlEventTouchUpInside];
    [mBrushFunctionItem.button addTarget:self action:@selector(brushSettingHandler:) forControlEvents:UIControlEventTouchUpInside];
    if([SEUserDefaultManager isFunctionOK:BASIC_FUNC] == YES)
    {
        [mBasicItem setBuied:YES];
    }
}
@end
 */
////

/////
@implementation SEHelpCategoryBlock
@synthesize categoryName;

- (id) init
{
    self = [super init];
    if(self)
    {
        details = [[NSMutableArray arrayWithArray:[NSArray array]] retain];
    }
    return self;
}
- (void) dealloc
{
    [details release];
    [categoryName release];
    [super dealloc];
}
- (void) addDetail: (NSString*)detail
{
    [details addObject:detail];
}
- (NSArray*) getDetails
{
    NSArray* array = [NSArray arrayWithArray:details];
    return array;
}
- (BOOL) hasDetail: (NSString*)detail
{
    for(NSString* str in details)
    {
        if([str isEqualToString:detail])
            return YES;
    }
    return NO;
}
@end
//////
@implementation SEHelpCategory
- (BOOL) hasCategory:(NSString*)category
{
    for(SEHelpCategoryBlock* block in mCategoryArray)
    {
        if([block.categoryName isEqualToString:category])
            return YES;
    }
    return NO;
}
- (void) addCagetory: (NSString*)categoryName
{
    if([self hasCategory:categoryName] == NO)
    {
        SEHelpCategoryBlock* categoryBlock = [[SEHelpCategoryBlock alloc] init];
        categoryBlock.categoryName = categoryName;
        [mCategoryArray addObject:categoryBlock];
        [categoryBlock release];
    }
}
- (SEHelpCategoryBlock*) getCategoryBlock: (NSString*)categoryName
{
    for(SEHelpCategoryBlock* block in mCategoryArray)
    {
        if([block.categoryName isEqualToString:categoryName])
            return block;
    }
    return nil;
}
- (void) configFileHandler: (NSArray*) param
{
    NSString* categoryName = [param objectAtIndex:0];
    NSArray* tokens = [param objectAtIndex:1];
    [self addCagetory:categoryName];
    assert(tokens.count == 1);
    SEHelpCategoryBlock* block = [self getCategoryBlock:categoryName];
    assert(block != nil);
    [block addDetail:[tokens objectAtIndex:0]];

}
- (void) loadData: (NSString*)configName
{
    SEConfigFileParser* parser = [[SEConfigFileParser alloc] init];    
    NSString* configs[] = {@"Basic", @"Image List", @"Music List", @"Share", @"List Manager", @"Settings", @"Store", @"Signature"};
    NSArray* configArray = [NSArray arrayWithObjects:configs count:sizeof(configs) / sizeof(NSString*)];
    [parser setBlockDefine:configArray];
    [parser setTarget:self action:@selector(configFileHandler:)];
    [parser parse: configName];
    [parser release];
}
- (id) initWithConfig: (NSString*) configName
{
    self = [super init];
    if(self)
    {
        mCategoryArray = [[NSMutableArray arrayWithArray:[NSArray array]] retain];
        [self loadData:configName];
    }
    return self;
}
- (void) dealloc
{
    [mCategoryArray release];
    [super dealloc];
}
- (NSArray*) getAllCategoryName
{
    NSArray* names = [NSArray array];
    for(SEHelpCategoryBlock* block in mCategoryArray)
    {
        names = [names arrayByAddingObject:block.categoryName];
    }
    return names;
}
- (NSArray*) getCategoryDetail:(NSString *)categoryName
{
    SEHelpCategoryBlock* block = [self getCategoryBlock:categoryName];
    return [block getDetails];
}
- (void) print
{
    for(SEHelpCategoryBlock* block in mCategoryArray)
    {
        NSLog(@"category name = %@", block.categoryName);
        NSArray* details = [block getDetails];
        for(NSString* d in details)
        {
            NSLog(@"    %@", d);
        }
    }
}
@end
////
@interface SECategoryItemButton : UIButton
{
    NSString* categoryName;
}
@property (nonatomic,retain) NSString* categoryName;
@end
@implementation SECategoryItemButton
@synthesize categoryName;


@end
@implementation SEOptionsHelpCategoryView
@synthesize mOptionsHelp;
- (SECategoryItemButton*) createCategoryItem
{
    SECategoryItemButton* button = [[[NSBundle mainBundle] loadNibNamed:@"HelpCategoryItem" owner:nil options:nil] lastObject];
    //SECategoryItemButton* button = [[SECategoryItemButton alloc] initWithFrame:CGRectMake(0, 0, 256, 64)];
    //SECategoryItemButton* button = [SECategoryItemButton buttonWithType:UIButtonTypeCustom];
    //button.frame = CGRectMake(0, 0, 256, 64);
    //[button autorelease];
    return button;
}
- (void) setButtonBg: (UIButton*) button
{
    UIImage* normal = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"OptionsHelpCategoryItemBg"];
    normal = [SEUtil imageWithCap:normal top:0.11 bottom:0.9 left:0.1 right:0.9];
    [button setBackgroundImage:normal forState:UIControlStateNormal];
    UIImage* selected = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"OptionsHelpCategoryItemHBg"];
    selected = [SEUtil imageWithCap:selected top:0.1 bottom:0.9 left:0.1 right:0.9];
    [button setBackgroundImage:selected forState:UIControlStateHighlighted];
    
}
- (void) buttonPressHandler: (SECategoryItemButton*)button
{
    [mOptionsHelp hideCategoryView];
    [mOptionsHelp setDetails:button.categoryName];
}
- (void) createChild: (CGRect)frame category:(SEHelpCategory *)category
{
    NSArray* categoryNameArray = [category getAllCategoryName];
    float startx = 0;
    float starty = 0;
    float vspacing = 4;
    float width = 0;
    float buttonWidth = frame.size.width;
    NSLog(@"category button width = %f", buttonWidth);
    for(int i = 0 ; i < categoryNameArray.count ; i++)
    {
        NSString* name = [categoryNameArray objectAtIndex:i];
        NSArray* details = [category getCategoryDetail:name];
        SECategoryItemButton* button = [self createCategoryItem];
        button.frame = CGRectMake(startx, starty, buttonWidth, button.frame.size.height);
        CGRect labelRect = CGRectMake(button.frame.origin.x + 12, button.frame.origin.y, button.frame.size.width - 10, button.frame.size.height);
        float imageViewWidth = button.frame.size.width * 2 / 3;
        float imageViewHeight = button.frame.size.height;
        UIImageView* imageView = [[UIImageView alloc] initWithFrame:CGRectMake(startx, starty, imageViewWidth, imageViewHeight)];
        UIImage* bgImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"HelpButtonBg"];
        imageView.image = bgImage;
        FontLabel* label = [[FontLabel alloc] initWithFrame:labelRect fontName:[SESystemConfig getFontName] pointSize:40];
        button.categoryName = name;
        width = button.frame.size.width;
        label.text = name;
        label.textAlignment = UITextAlignmentLeft;
        label.textColor = getLabelColor();
        float label2Width = 200;
        FontLabel* label2 = [[FontLabel alloc] initWithFrame:CGRectMake(button.frame.origin.x + button.frame.size.width - label2Width - 12, label.frame.origin.y, label2Width, label.frame.size.height) fontName:[SESystemConfig getFontName] pointSize:30];
        label2.text = [NSString stringWithFormat:@"%d/pages", details.count];
        label2.textAlignment = UITextAlignmentRight;
        label2.textColor = getViewColor();
        [self addSubview:button];
        [self addSubview:imageView];
        [self addSubview:label];
        [self addSubview:label2];
        [label release];
        [label2 release];
        [imageView release];
        starty += button.frame.size.height + vspacing;
        [self setButtonBg:button];
        button.backgroundColor = [UIColor clearColor];
        [button addTarget:self action:@selector(buttonPressHandler:) forControlEvents:UIControlEventTouchUpInside];
    }
    starty -= vspacing;
    self.frame = CGRectMake(0, 0, width, starty);
}
- (id) initWithFrame:(CGRect)frame category:(SEHelpCategory*)category
{
    self = [super initWithFrame:frame];
    if(self)
    {
        [self createChild:frame category: category];
    }
    return self;
}
- (void) dealloc
{
    //[mHelpCatgory release];
    [super dealloc];
}
@end
/////
#define ARROW_OFFSET 20
@implementation SEOptionsHelpButton
- (void) buttonPressHandler: (UIButton*)button
{
    /*
    NSLog(@"button down handler");
    CGPoint p = mLeftArrow.center;
    mLeftArrow.center = CGPointMake(p.x - ARROW_OFFSET, p.y);
    
    p = mRightArrow.center;
    mRightArrow.center = CGPointMake(p.x + ARROW_OFFSET, p.y);
     */
}
- (void) buttonUpHandler: (UIButton*)button
{
    NSLog(@"button up handler");
    /*
    CGPoint p = mLeftArrow.center;
    mLeftArrow.center = CGPointMake(p.x + ARROW_OFFSET, p.y);
    
    p = mRightArrow.center;
    mRightArrow.center = CGPointMake(p.x - ARROW_OFFSET, p.y);
     */
}
- (void) leftArrowHandler: (UITapGestureRecognizer*)ges
{
    NSLog(@"left arrow handler");
    [mTarget performSelector:mLeftArrowHandler];
}
- (void) rightArrowHandler: (UITapGestureRecognizer*)ges
{
    NSLog(@"right arrow handler");
    [mTarget performSelector:mRightArrowHandler];
}
- (void) createChild:(CGRect)frame : (BOOL) hasArrow
{
    mButton = [UIButton buttonWithType:UIButtonTypeCustom];
    [self addSubview:mButton];
    mButton.frame = CGRectMake(0, 0, frame.size.width, frame.size.height);
    [mButton addTarget:self action:@selector(buttonPressHandler:) forControlEvents:UIControlEventTouchDown];
    [mButton addTarget:self action:@selector(buttonUpHandler:) forControlEvents:UIControlEventTouchUpInside];
    self.userInteractionEnabled = YES;
    float startArrowOffset = 50;
    if(hasArrow)
    {
        SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
        UIImage* leftArrow = [viewNav.mResLoader getImage:@"HelpArrowLeft"];
        UIImage* rightArrow = [viewNav.mResLoader getImage:@"HelpArrowRight"];
        float starty = (self.frame.size.height - leftArrow.size.height) / 2;
        float startx = (self.frame.size.width - leftArrow.size.width) / 2 - startArrowOffset;
        mLeftArrow = [[UIImageView alloc] initWithFrame:CGRectMake(startx, starty, leftArrow.size.width, leftArrow.size.height)];
        mLeftArrow.userInteractionEnabled = YES;
        mLeftArrow.image = leftArrow;
        [self addSubview:mLeftArrow];
        [mLeftArrow release];
        UITapGestureRecognizer* ges = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(leftArrowHandler:)];
        [mLeftArrow addGestureRecognizer:ges];
        [ges release];
        
        starty = (self.frame.size.height - rightArrow.size.height) / 2;
        //startx = (self.frame.size.width - rightArrow.size.width) / 2;
        mRightArrow = [[UIImageView alloc] initWithFrame:CGRectMake(frame.size.width - rightArrow.size.width - 2, starty, rightArrow.size.width, rightArrow.size.height)];
        [self addSubview:mRightArrow];
        [mRightArrow release];
        mRightArrow.image = rightArrow;
        ges = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(rightArrowHandler:)];
        [mRightArrow addGestureRecognizer:ges];
        mRightArrow.userInteractionEnabled = YES;
        [ges release];
    }
}
- (void) setButtonHandler: (id)target : (SEL)action
{
    [mButton addTarget:target action:action forControlEvents:UIControlEventTouchUpInside];
}
- (void) setBackgroundImage: (NSString*) normal : (NSString*)highlighted
{
    UIImage* normalImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:normal];
    UIImage* selectImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:highlighted];

    [mButton setBackgroundImage:normalImage forState:UIControlStateNormal];
    [mButton setBackgroundImage:selectImage forState:UIControlStateSelected];
}
- (void) setArrowHandlerTarget: (id)target leftArrowAction: (SEL)leftAction rightArrowAction: (SEL)rightAction
{
    mTarget = target;
    mLeftArrowHandler = leftAction;
    mRightArrowHandler = rightAction;
}
- (void) setText: (NSString*)str textColor: (UIColor*)color
{
    if(mText == nil)
    {
        float startx = mLeftArrow.frame.origin.x + mLeftArrow.frame.size.width + 5;
        float starty = 0;
        float w = mRightArrow.frame.origin.x - mLeftArrow.frame.origin.x - mLeftArrow.frame.size.width;
        float h = self.frame.size.height;
        if(mLeftArrow == nil && mRightArrow == nil)
        {
            SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
            UIImage* leftArrow = [viewNav.mResLoader getImage:@"HelpArrowLeft"];
            UIImage* rightArrow = [viewNav.mResLoader getImage:@"HelpArrowRight"];
            startx = (self.frame.size.width - leftArrow.size.width) / 2 + leftArrow.size.width + 5;
            starty = 0;
            float endx = self.frame.size.width - rightArrow.size.width;
            w = endx - startx;
            h = self.frame.size.height;
        }
        NSString* fontName = [SESystemConfig getFontName];
        float fontSize = 40;
        mText = [[FontLabel alloc] initWithFrame:CGRectMake(startx, starty, w, h) zFont:[[FontManager sharedManager] zFontWithName:fontName pointSize:fontSize]];
        mText.backgroundColor = [UIColor clearColor];
        mText.textColor = color;
        if(mLeftArrow == nil && mRightArrow == nil)
        {
            mText.textAlignment = UITextAlignmentRight;
        }
        else
        {
            mText.textAlignment = UITextAlignmentCenter;
        }
        [self addSubview:mText];
        [mText release];
    }
    mText.text = str;
}
- (id) initWithFrame:(CGRect)frame withArrow: (BOOL) hasArrow
{
    self = [super initWithFrame:frame];
    if(self)
    {
        [self createChild:frame : hasArrow];
    }
    return self;
}

@end
///////
@implementation SEOptionsHelpDetailView
@synthesize mOptionsHelp;
- (void) setButtonBg
{
    /*
    UIImage* normal = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"OptionsHelpBackButtonBg"];
    normal = [SEUtil imageWithCap:normal top:0.11 bottom:0.9 left:0.1 right:0.9];
    [mButton setBackgroundImage:normal forState:UIControlStateNormal];
    UIImage* selected = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"OptionsHelpBackButtonHBg"];
    selected = [SEUtil imageWithCap:selected top:0.1 bottom:0.9 left:0.1 right:0.9];
    [mButton setBackgroundImage:selected forState:UIControlStateHighlighted];
    */
}
- (void) backButtonHandler: (UIButton*)button
{
    [mOptionsHelp hideDetailView];
}
- (void) scrollViewDidEndDecelerating:(UIScrollView *)scrollView
{
    CGFloat x = scrollView.contentOffset.x;
    CGFloat w = scrollView.bounds.size.width;
    mPageControl.currentPage = x / w;
}
- (void) leftArrowHandler
{
    if(mPageControl.currentPage > 0)
    {
        mPageControl.currentPage = mPageControl.currentPage - 1;
        CGFloat x = mContentScrollView.bounds.size.width * mPageControl.currentPage;
        CGRect r = CGRectMake(x, 0, mContentScrollView.bounds.size.width, mContentScrollView.bounds.size.height);
        [mContentScrollView scrollRectToVisible:r animated:YES];
    }
}
- (void) rightArrowHandler
{
    if(mPageControl.currentPage < mPageControl.numberOfPages - 1)
    {
        mPageControl.currentPage = mPageControl.currentPage + 1;
        CGFloat x = mContentScrollView.bounds.size.width * mPageControl.currentPage;
        CGRect r = CGRectMake(x, 0, mContentScrollView.bounds.size.width, mContentScrollView.bounds.size.height);
        [mContentScrollView scrollRectToVisible:r animated:YES];
    }
}
- (void) createChild: (CGRect)frame
{
    mBackgroundView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, frame.size.width, frame.size.height)];
    [self addSubview:mBackgroundView];
    [mBackgroundView release];
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    mBackgroundView.image = [viewNav.mResLoader getImage:@"HelpBackground"];
    mBackgroundView.backgroundColor = [UIColor blackColor];
    mContentScrollView = [[UIScrollView alloc] initWithFrame:CGRectMake(0, 0, frame.size.width, frame.size.height)];
    [self addSubview:mContentScrollView];
    [mContentScrollView release];
    mContentScrollView.pagingEnabled = YES;
    mContentScrollView.delegate = self;
    //UIButton* button = [UIButton buttonWithType:UIButtonTypeCustom];
    //mButton = button;
    [self setButtonBg];
    float buttonWidth = 518;
    float buttonHeight = 52;
    float paddingx = 0;
    float buttonStartx = self.frame.size.width - buttonWidth - paddingx;
    float buttonStarty = 0;
    //button.frame = CGRectMake(buttonStartx, buttonStarty, buttonWidth, buttonHeight);
    //[button addTarget:self action:@selector(backButtonHandler:) forControlEvents:UIControlEventTouchUpInside];
    //[self addSubview:button];
    mCloseButton = [[SEOptionsHelpButton alloc] initWithFrame:CGRectMake(buttonStartx, buttonStarty, buttonWidth, buttonHeight) withArrow:NO];
    [self addSubview:mCloseButton];
    [mCloseButton release];
    [mCloseButton setBackgroundImage:@"HelpButtonBackNormal" :@"HelpButtonBackH"];
    [mCloseButton setButtonHandler:self :@selector(backButtonHandler:)];
    [mCloseButton setText:@"CLOSE" textColor:[UIColor yellowColor]];
    
    mNextButton = [[SEOptionsHelpButton alloc] initWithFrame:CGRectMake(buttonStartx, self.frame.size.height - buttonHeight, buttonWidth, buttonHeight) withArrow:YES];
    [self addSubview:mNextButton];
    [mNextButton release];
    [mNextButton setBackgroundImage:@"HelpButtonNextNormal" :@"HelpButtonNextH"];
    [mNextButton setText:@"TURN PAGE" textColor:[UIColor whiteColor]];
    [mNextButton setArrowHandlerTarget:self leftArrowAction:@selector(leftArrowHandler) rightArrowAction:@selector(rightArrowHandler)];
    /*
    FontLabel* label = [[FontLabel alloc] initWithFrame:button.frame fontName:[SESystemConfig getFontName] pointSize:30]; 
    label.textAlignment = UITextAlignmentCenter;
    label.text = @"Back";
    [self addSubview:label];
    */
    //////////////////////////
    float pageControlWidth = 256;
    UIPageControl* pageControl = [[UIPageControl alloc] initWithFrame:CGRectMake((self.frame.size.width - pageControlWidth) / 2, self.frame.size.height - 50, pageControlWidth, 40)];
    [self addSubview:pageControl];
    [pageControl release];
    mPageControl = pageControl;
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
- (void) setDetails: (NSArray*)details
{
    float startx = 0;
    float starty = 0;
    float height = mContentScrollView.frame.size.height;
    for(UIView* child in mContentScrollView.subviews)
    {
        [child removeFromSuperview];
    }
    assert(mContentScrollView.subviews.count == 0);
    for(int i = 0 ; i < details.count ; i++)
    {
        NSString* detialName  = [details objectAtIndex:i];
        UIImage* image = [UIImage imageNamed:detialName];
        UIImageView* imageView = [[UIImageView alloc] initWithFrame:CGRectMake(startx, starty, mContentScrollView.frame.size.width, image.size.height)];
        startx += imageView.frame.size.width;
        imageView.image = image;
        [mContentScrollView addSubview:imageView];
        [imageView release];
    }
    mContentScrollView.contentSize = CGSizeMake(startx, height);
    mContentScrollView.contentOffset = CGPointMake(0, 0);
    mPageControl.numberOfPages = details.count;
    mPageControl.currentPage = 0;
}
@end
///////

@implementation SEOptionsHelpSetting
- (void) dealloc
{
    [mDetialView release];
    [mHelpCatgory release];
    [super dealloc];
}
- (UIView*) getView
{
    if(mView == nil)
    {
        mHelpCatgory = [[SEHelpCategory alloc] initWithConfig:@"help_setting.txt"];
        [mHelpCatgory print];
        mView = [[[NSBundle mainBundle] loadNibNamed:@"Options8" owner:self options:nil] lastObject];
        mView.backgroundColor = [UIColor clearColor];
        [mView retain];
        mView.userInteractionEnabled = YES;

        SEOptionsHelpCategoryView* view = [[SEOptionsHelpCategoryView alloc] initWithFrame:CGRectMake(0, 0, mView.frame.size.width - 40, 105) category:mHelpCatgory];
        [mView addSubview:view];
        [view release];
        view.mOptionsHelp = self;
        mCategoryView = view;
        view.frame = CGRectMake((mView.frame.size.width - view.frame.size.width)/2, 13, view.frame.size.width, view.frame.size.height);
        if(view.frame.size.height > mView.frame.size.height)
        {
            mView.frame = CGRectMake(mView.frame.origin.x, mView.frame.origin.y, mView.frame.size.width, view.frame.size.height + 13);
        }
        SEOptionsHelpDetailView* detailView = [[SEOptionsHelpDetailView alloc] initWithFrame:CGRectMake(0, 0, mViewNav.mViewPortWidth, mViewNav.mViewPortHeight)];
        //[mView addSubview:detailView];
        mDetialView = detailView;
        detailView.mOptionsHelp = self;
        detailView.hidden = YES;
        
    }
    return mView;
}
- (void) setData
{
    
}
- (float) getHeight
{
    return mView.frame.size.height;
}
- (void) hideCategoryView
{
    //mCategoryView.hidden = YES;
    mDetialView.hidden = NO;
}
- (void) hideDetailView
{
    //mCategoryView.hidden = NO;
    mDetialView.hidden = YES;
    [mDetialView removeFromSuperview];
}
- (void) setDetails: (NSString*)categoryName
{
    NSArray* details = [mHelpCatgory getCategoryDetail:categoryName];
    [mDetialView setDetails:details];
    [mViewNav.mRootView addSubview:mDetialView];
}
@end
///
@implementation  SEOptionsInvalidSetting


@end
/////
@interface SEOptionLeftLabel : UIView
{
    enum {NORMAL, SELECTED, HIGHLIGHTED};
    UIImage* mSelectedImage;
    UIImage* mNormalImage;
    UIImage* mHighlightedImage;
    UIImage* mContentImage;
    UIImageView* mBackgroundView;
    UIImageView* mForegroundView;
    //BOOL mSelected;
    OPTION_LEFT_BAR_TYPE type;
    SEOptionsView* mOptionsView;
    CGPoint mBeginPoint;
    int mState;
}
@property (nonatomic, retain) UIImage* mContentImage;
@property (nonatomic, retain) UIImage* mHighlightedImage;
@property (nonatomic, retain) UIImage* mSelectedImage;
@property (nonatomic, retain) UIImage* mNormalImage;
@property (nonatomic, assign) BOOL mSelected;
@property (nonatomic, assign) OPTION_LEFT_BAR_TYPE mType;
@property (nonatomic, assign) SEOptionsView* mOptionsView;
- (void) toNormalState;
- (void) toHighlightedState;
- (void) setLock : (BOOL) b;
@end
//////////
@implementation SEOptionLeftLabel
@synthesize mContentImage;
@synthesize mSelectedImage;
@synthesize mNormalImage;
@synthesize mHighlightedImage;
@synthesize mSelected;
@synthesize mType;
@synthesize mOptionsView;
- (void) toHighlightedState
{
    mState = HIGHLIGHTED;
    mBackgroundView.image = [SEUtil imageWithCap:mHighlightedImage top:0.1 bottom:0.9 left:0.1 right:0.9];
    [self setNeedsDisplay];
}
- (void) toSelectedState
{
    mState = SELECTED;
    mBackgroundView.image = [SEUtil imageWithCap:mSelectedImage top:0.1 bottom:0.9 left:0.1 right:0.9];
}
- (void) toNormalState
{
    mState = NORMAL;
    mBackgroundView.image = [SEUtil imageWithCap:mNormalImage top:0.1 bottom:0.9 left:0.1 right:0.9];
    [self setNeedsDisplay];
}
- (void) dealloc
{
    [mNormalImage release];
    [mSelectedImage release];
    [mHighlightedImage release];
    [mContentImage release];
    [super dealloc];
}
/*
- (void)drawRect:(CGRect)rect
{
    if(mSelected)
    {
        [mSelectedImage drawInRect:rect];
    }
    else
    {
        [mNormalImage drawInRect:rect];
    }
    switch (mState) {
        case NORMAL:
        {
            [mNormalImage drawInRect:rect];
        }
            break;
        case SELECTED:
        {
            [mSelectedImage drawInRect:rect];
        }
            break;
        case HIGHLIGHTED:
        {
            [mHighlightedImage drawInRect:rect];
        }
            break;
        default:
            break;
    }
    float startx = (self.frame.size.width - mContentImage.size.width) / 2;
    float starty = (self.frame.size.height - mContentImage.size.height) /2;
    [mContentImage drawAtPoint:CGPointMake(startx, starty)];
}

*/
- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{
    mBeginPoint = [[touches anyObject] locationInView:self];
    if(mState == NORMAL)
    {
        [self toSelectedState];
    }
}

- (void) touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event
{
    if(mState != SELECTED)
        return;
    CGPoint point = [[touches anyObject] locationInView:self];
    //float deltax = point.x - mBeginPoint.x;
    //float deltay = point.y - mBeginPoint.y;
    if([self pointInside:point withEvent:[touches anyObject]] == NO)
    {
        [self toNormalState];
    }
}
- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event
{
    if(mState == SELECTED)
    {
        mState = NORMAL;
        [mOptionsView setCurrentBarView:mType]; 
    }
}
- (void) touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event
{
    if(mState == SELECTED)
    {
        [self toNormalState];
    }
}
- (void) createChild: (CGRect)frame bg: (UIImage*)bg fg: (UIImage*) fg
{
    mBackgroundView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, frame.size.width, frame.size.height)];
    [self addSubview:mBackgroundView];
    [mBackgroundView release];
    
    mBackgroundView.image = [SEUtil imageWithCap:bg top:0.1 bottom:0.9 left:0.1 right:0.9];
    
    mForegroundView = [[UIImageView alloc] initWithFrame:CGRectMake((self.frame.size.width - fg.size.width) / 2, (self.frame.size.height - fg.size.height)  / 2, fg.size.width, fg.size.height)];
    [self addSubview:mForegroundView];
    [mForegroundView release];
    mForegroundView.image = fg;
}
- (id) initWithFrame:(CGRect)frame background: (UIImage*)bg foreground : (UIImage*)fg;
{
    self = [super initWithFrame:frame];
    if(self)
    {
        [self createChild:frame bg:bg fg:fg];
        self.mNormalImage = bg;
        self.mContentImage = fg;
    }
    return self;
}
@end
///////
@interface SEOptionsView (Private)
//- (void) leftBarTapHandler: (UITapGestureRecognizer*)tapGes;
//- (void) setDataToViews:(OPTION_LEFT_BAR_TYPE) barType;
@end
@implementation SEOptionsView(Private)

@end
/////////////////////////////
@implementation SEOptionsView

// Only override drawRect: if you perform custom drawing.
// An empty implementation adversely affects performance during animation.
/*
- (void)drawRect:(CGRect)rect
{
    // Drawing code
    //[super drawRect:rect];
    CGRect leftRect = CGRectMake(0, 0, mLeftBackground.size.width, mLeftBackground.size.height);
    [mLeftBackground drawInRect:leftRect];
    CGRect rightRect = CGRectMake(leftRect.size.width, 0, mRightBackground.size.width, mRightBackground.size.height);
    [mRightBackground drawInRect:rightRect];
}
*/
- (id) initWithFrame:(CGRect)frame byViewNav: (SEViewNavigator*)viewNav
{
    self = [super initWithFrame:frame];
    if(self)
    {
        mViewNav = viewNav;
        mCurrentLeftBar = INVALID_LEFT_BAR_VIEW;
        for(int i = 0 ; i < OPTION_LEFT_BAR_NUM ; i++)
            mRightViewController[i] = nil;
        self.userInteractionEnabled = YES;
        float leftViewWidth = 270;
        float rightViewWidth = 690;
        mLeftView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, leftViewWidth, mViewNav.mViewPortHeight)];
        mRightView = [[UIScrollView alloc] initWithFrame:CGRectMake(leftViewWidth, 0, rightViewWidth, mViewNav.mViewPortHeight)];
        mLeftView.backgroundColor = [UIColor clearColor];
        mRightView.backgroundColor = [UIColor clearColor];
        mRightContentParent = [[UIView alloc] init];
        mRightContentParent.userInteractionEnabled = YES;
        [mRightView addSubview:mRightContentParent];
        [mRightContentParent release];
        mRightView.contentSize = CGSizeMake(mRightView.frame.size.width, mRightView.frame.size.height);
        mRightContentParent.frame = CGRectMake(0, 0, mRightView.frame.size.width, mRightView.frame.size.height);
        mRightView.bounces = YES;
        mRightView.alwaysBounceVertical = YES;
        mRightView.directionalLockEnabled = YES;
        mRightBackgroundView = [[UIImageView alloc] initWithFrame:CGRectMake(leftViewWidth, 0, rightViewWidth, mViewNav.mViewPortHeight)];
        UIImage* image = [mViewNav.mResLoader getImage:@"OptionsRightBackground"];
        image = [SEUtil imageWithCap:image top:0.1 bottom:0.1 left:0.1 right:0.1];
        mRightBackgroundView.image = image;
        [self addSubview:mRightBackgroundView];
        [mRightBackgroundView release];
        [self addSubview:mLeftView];
        [self addSubview:mRightView];
        mRightView.backgroundColor = [UIColor clearColor];
        self.backgroundColor = [UIColor blackColor];
    }
    return self;
}
/*
- (void) leftLabelClick: (id)sender
{
    SEOptionLeftLabel* label = (SEOptionLeftLabel*)sender;
    [self setCurrentBarView:label.mType];
}
 */
- (void) initLeftView
{
    UIImage* labelNormalImage = [mViewNav.mResLoader getImage:@"OptionLabelNormalBackground"];
    UIImage* labelSelectedImage = [mViewNav.mResLoader getImage:@"OptionLabelSelectedBackground"];
    UIImage* labelHighlightedImage = [mViewNav.mResLoader getImage:@"OptionsLabelHighlightedBackground"];
    float leftPadding = 5, rightPadding = 15;
    float topPadding = 30;
    float startx = leftPadding , starty = topPadding;
    float leftViewWidth = mLeftView.frame.size.width;
    float labelWidth = leftViewWidth - leftPadding - rightPadding;
    float labelHeight = 80;
    for(int i = 0 ; i < INVALID_LEFT_BAR_VIEW ; i++)
    {
        NSString* imagename = gLeftLabelText[i].imageName;
        UIImage* contentImage = [UIImage imageNamed:imagename];
        SEOptionLeftLabel* label = [[SEOptionLeftLabel alloc] initWithFrame:CGRectMake(startx, starty, labelWidth, labelHeight) background:labelNormalImage foreground:contentImage];
        label.mOptionsView = self;
        label.mType = (OPTION_LEFT_BAR_TYPE)i;
        //label.text = gOptionsBarViewData[i].barLabel;
        label.userInteractionEnabled = YES;
        label.mSelectedImage = labelSelectedImage;
        //label.mNormalImage = labelNormalImage;
        label.mHighlightedImage = labelHighlightedImage;
        //label.mContentImage = [UIImage imageNamed:imagename];
        label.backgroundColor = [UIColor clearColor];
        //[label addTarget:self action:@selector(leftLabelClick:) forControlEvents:UIControlEventTouchUpInside];
        mLeftViewBars[i] = label;
        [self addSubview:label];
        [label release];
        starty += labelHeight + LEFT_BAR_VMARGIN;
    }

}
- (SEOptionsSettingViewController*) createOptionsViewController: (OPTION_LEFT_BAR_TYPE)barType
{
    /*
    NSString* className = gOptionsViewControllerClassName[barType];
    Class controllerClass = NSClassFromString(className);
    
    SEOptionsSettingViewController* c = [controllerClass new];
    c.mViewNav = mViewNav;
    return c;
     */
    SEOptionsSettingViewController* c = nil;
    switch (barType) {
        case PLAY_SETTING:
        {
            c = [[SEOptionsPlaySetting alloc] init];
        }
            break;
        case WIDGET_SETTING:
        {
            c = [[SEOptionsWidgetSetting alloc] init];
        }
            break;
            /*
        case SIGNATURE_SETTING:
        {
            c = [[SEOptionsSignatureSetting alloc] init];
        }
            break;
             */
        case USER_INFOR_SETTING:
        {
            c = [[SEOptionsUserInfoSetting alloc] init];
        }
            break;
        case ABOUT_SETTING:
        {
            c = [[SEOptionsAboutSetting alloc] init];
        }
            break;
        case APP_STORE_SETTING:
        {
            c = [[SEOptionsAppStoreSetting2 alloc] init];
        }
            break;
        case HELP_SETTING:
        {
            c = [[SEOptionsHelpSetting alloc] init];
        }
            break;
        case INVALID_LEFT_BAR_VIEW:
        {
            c = [[SEOptionsInvalidSetting alloc] init];
        }
            break;
        default:
            break;
    }
    c.mViewNav = mViewNav;
    return c;
}
- (BOOL) isRightNeedLock
{
    return NO;
    /*
    if(mCurrentLeftBar == SIGNATURE_SETTING)
    {
        if([SEUserDefaultManager isFunctionOK:SIGNATURE_FUNC] == NO)
            return YES;
        else 
        {
            return NO;
        }
    }
    else if(mCurrentLeftBar == USER_INFOR_SETTING)
    {
        if([SEUserDefaultManager isFunctionOK:USERINFO_FUNC] == NO)
            return YES;
        else {
            return NO;
        }
    }
    else
    {
        return NO;
    }
     */
}
- (void) setCurrentBarView: (OPTION_LEFT_BAR_TYPE) barType
{
    if(mCurrentLeftBar == barType)
        return;
    if(barType < 0 || barType >= OPTION_LEFT_BAR_NUM)
        return;
    SEOptionsSettingViewController* rightViewController = mRightViewController[barType];
    if(rightViewController == nil)
    {
        mRightViewController[barType] = [self createOptionsViewController: barType]; 
        rightViewController = mRightViewController[barType];
        rightViewController.mOptionsView = self;
    }
    [rightViewController getView];
    SEOptionsSettingViewController* old = mRightViewController[mCurrentLeftBar];
    [old removeFromParent: mRightContentParent];
    [rightViewController addViewToParent:mRightContentParent];
    [rightViewController setData];
    if([rightViewController getHeight] > 0)
    {
        //NSLog(@"## height = %f ##", [rightViewController getHeight]);
        mRightView.contentSize = CGSizeMake(mRightView.frame.size.width, [rightViewController getHeight]);
        mRightContentParent.frame = CGRectMake(mRightContentParent.frame.origin.x, mRightContentParent.frame.origin.y, mRightView.contentSize.width, mRightView.contentSize.height);
    }
    else
    {
        mRightView.contentSize = CGSizeMake(mRightView.frame.size.width, mRightView.frame.size.height);
    }
    SEOptionLeftLabel* currentLabel = mLeftViewBars[barType];
    SEOptionLeftLabel* oldLabel = mLeftViewBars[mCurrentLeftBar];
    [oldLabel toNormalState];
    mCurrentLeftBar = barType;
    [currentLabel toHighlightedState];
    [mLockView removeFromSuperview];
    mLockView = nil;
    if([self isRightNeedLock])
    {
        if(mLockView == nil)
        {
            mLockView = [[SEOptionsLockView alloc] initWithFrame:CGRectMake(0, 0, mRightContentParent.frame.size.width, mRightContentParent.frame.size.height)];
        }
        [mLockView setLock:YES type:LOCK_BUY];
        [mRightContentParent addSubview:mLockView];
        [mLockView release];
    }
}
- (void) initData
{
    if(mLeftBackground == nil)
    {
        UIImage* leftBackgroundImage = [mViewNav.mResLoader getImage:@"OptionsLeftBackground"];
        mLeftBackground = [SEUtil imageWithCap:leftBackgroundImage top:0.1 bottom:0.9 left:0.1 right:0.9];
        [mLeftBackground retain];
    }
    /*
    if(mRightBackground == nil)
    {
        UIImage* uiImage = [mViewNav.mResLoader getImage:@"OptionsRightBackground"];
        mRightBackground = [SEUtil imageWithCap:uiImage top:0.1 bottom:0.9 left:0.1 right:0.9];
    }
     */
    mLeftView.image = mLeftBackground;
    
    //mRightView.image = mRightBackground;
    [self initLeftView];
}
- (BOOL)canAdjust
{
    return NO;
}
- (void)relayout
{}
- (void) updateAllView
{
    for(int i = 0 ; i < OPTION_LEFT_BAR_NUM; i++)
    {
        SEOptionsSettingViewController* c = mRightViewController[i];
        [c update];
    }
}
- (void) update
{
    [self updateUserUpgradeInfo];
    [self updateAllView];
}
- (void) updateUserUpgradeInfo
{
    SEOptionsUserInfoSetting* c = (SEOptionsUserInfoSetting*)mRightViewController[USER_INFOR_SETTING];
    [c updateInfo];
}
- (void) setAppStoreItemSelected: (int) productType
{
    SEOptionsAppStoreSetting2* c = (SEOptionsAppStoreSetting2*)mRightViewController[APP_STORE_SETTING];
    [c selectStoreItem:productType];
}
- (void)dealloc
{

    [mLeftBackground release];
    [mRightBackground release];
    [mLeftView release];
    [mRightView release];
    for(int i = 0 ; i < OPTION_LEFT_BAR_NUM ; i++)
    {
        [mRightViewController[i] release];
    }

    [super dealloc];
}
@end
