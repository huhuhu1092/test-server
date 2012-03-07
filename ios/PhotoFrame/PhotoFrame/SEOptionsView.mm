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
#define LEFT_PADDING 10
#define TOP_PADDING 10
#define LEFT_BAR_WIDTH 300
#define LEFT_BAR_HEIGHT 50
#define LEFT_BAR_VMARGIN 20
#define LEFT_RIGTH_MARGIN 100 
///////
@implementation SEOptionsRightViewCreator

- (NSArray*)createViews:(UIView *)parent withResLoader:(SEResLoader *)resLoader
{
    NSMutableArray* viewArray = [NSMutableArray array];
    UILabel* label = [[UILabel alloc] initWithFrame:CGRectMake(350, 30, 300, 30)];
    label.text = @"test kk";
    [viewArray addObject:label];
    [label release];
    return viewArray;
}
@end
@implementation SEOptionsMainSettingCreator
@synthesize mViewNav;
@synthesize mSampleView;
@synthesize mCurrentTimes;
@synthesize mCurrentQuality;
@synthesize mQualitySlider;
@synthesize mTimesSlider;
@synthesize mSignature;
@synthesize mQualityLabel;
@synthesize mTimesLabel;
@synthesize mResLoader;
- (IBAction)signatureButtonHandler:(id)sender
{
    //[mViewNav moveToView:OPTIONS_SIGNATURE:SIGNATURE_VIEW hasAnimation:YES];
    [mViewNav moveToView:OPTIONS_SIGNATURE :SIGNATURE_PREVIEW hasAnimation:YES isPush:YES];
}
- (NSArray*)createViews:(UIView *)parent withResLoader:(SEResLoader *)resLoader
{
    NSMutableArray* viewArray = [NSMutableArray array];
    UIImage* uiImage = [resLoader getImage:@"Apple1"];
    mSampleView.image = uiImage;
    [viewArray addObject:mSampleView];
    [viewArray addObject:mCurrentTimes];
    [viewArray addObject:mCurrentQuality];
    [viewArray addObject:mQualitySlider];
    [viewArray addObject:mTimesSlider];
    [viewArray addObject:mSignature];
    [viewArray addObject:mQualityLabel];
    [viewArray addObject:mTimesLabel];
    for(UIView* v in viewArray)
    {
        NSLog(@"v retain count = %d", [v retainCount]);
        [v release];
    }
    return viewArray;
}

- (id) init
{
    self = [super init];
    if(self)
    {
        NSArray* output  = [[NSBundle mainBundle] loadNibNamed:@"OptionsDrawingSetting" owner:self options:nil];
        NSUInteger count = [output count];
        for(NSUInteger i = 0 ; i < count ; i++)
        {
            id v = [output objectAtIndex:i];
            NSLog(@"nib obj = %@", v);
        }
    }
    return self;
}
- (IBAction)qualitySliderHandler:(UISlider*)sender
{
    int v = (int)sender.value;
    if(v != [mQualityLabel.text intValue])
    {
        int minTimes, maxTimes;
        PainterManager* pm = [PainterManager painterManager];
        [pm getMinMaxTimesValue:(int)v outMin:&minTimes outMax:&maxTimes];
        mTimesSlider.minimumValue = minTimes;
        mTimesSlider.maximumValue = maxTimes;
        [mTimesSlider setValue:maxTimes];
        mCurrentTimes.text = [NSString stringWithFormat:@"%d", maxTimes];
        [self setSampleImageWithQuality:v withResLoader:mResLoader];
    }
    mCurrentQuality.text = [NSString stringWithFormat:@"%d", v];    
}
- (IBAction)timesSliderHandler:(UISlider*)sender
{
    int v = (int)sender.value;
    mCurrentTimes.text = [NSString stringWithFormat:@"%d",v];

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
    mSampleView.image = uiImage;
}
@end
////////

/////
@interface SEOptionLeftLabel : UILabel
{
    UIImage* mSelectedImage;
    UIImage* mNormalImage;
    BOOL mSelected;
    OPTION_LEFT_BAR_TYPE type;
    SEOptionsView* mOptionsView;
}
@property (nonatomic, retain) UIImage* mSelectedImage;
@property (nonatomic, retain) UIImage* mNormalImage;
@property (nonatomic, assign) BOOL mSelected;
@property (nonatomic, assign) OPTION_LEFT_BAR_TYPE mType;
@property (nonatomic, assign) SEOptionsView* mOptionsView;
@end
@implementation SEOptionLeftLabel
@synthesize mSelectedImage;
@synthesize mNormalImage;
@synthesize mSelected;
@synthesize mType;
@synthesize mOptionsView;
- (void) dealloc
{
    [mNormalImage release];
    [mSelectedImage release];
    [super dealloc];
}
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
    [super drawTextInRect:rect];
}
- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{
    [mOptionsView setCurrentLeftBar:mType];    
}
- (void) touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event
{}
- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event
{}
- (void) touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event
{}
@end
///////
@implementation SEOptionsView(Private)

- (void) leftBarTapHandler: (UITapGestureRecognizer*)tapGes
{
    NSLog(@"label tap handler\n");
    //if(tapGes.state == UIGestureRecognizerStateBegan)
    {
        UILabel* label = (UILabel*)tapGes.view;
        label.backgroundColor = [UIColor blueColor];
    }
}

- (void) setDataToViews: (NSArray*)views withType:(OPTION_LEFT_BAR_TYPE) barType
{
    switch (barType)
    {
        case MAIN_SETTING:
        {
            SEOptionsMainSettingCreator* mainSetting = [mRightViewsCreator objectAtIndex:barType];
            PainterManager* pm = [PainterManager painterManager];
            mainSetting.mCurrentTimes.text = [NSString stringWithFormat:@"%d", pm.painterProperty.times];
            mainSetting.mCurrentQuality.text = [NSString stringWithFormat:@"%d", pm.painterProperty.percent];
            mainSetting.mQualitySlider.minimumValue = 4;
            mainSetting.mQualitySlider.maximumValue = 10;
            
            int maxTimes, minTimes;
            [pm getMinMaxTimesValue: pm.painterProperty.percent outMin:&minTimes outMax:&maxTimes];
            mainSetting.mTimesSlider.minimumValue = minTimes;
            mainSetting.mTimesSlider.maximumValue = maxTimes;
            int percent = pm.painterProperty.percent;
            int times = pm.painterProperty.times;
            [mainSetting.mQualitySlider setValue:percent];
            [mainSetting.mTimesSlider setValue:times];
            UIImage* sliderThumb = [mResLoader getImage:@"SliderBarThumbImage"];
            [mainSetting.mQualitySlider setThumbImage:sliderThumb forState:UIControlStateNormal];
            [mainSetting.mQualitySlider setThumbImage:sliderThumb forState:UIControlStateHighlighted];
            [mainSetting.mTimesSlider setThumbImage:sliderThumb forState:UIControlStateNormal];
            [mainSetting.mTimesSlider setThumbImage:sliderThumb forState:UIControlStateHighlighted];
            UIImage* sliderMinImage =[mResLoader getImage:@"SliderMinTrackImage"];
            UIImage* sliderMaxImage = [mResLoader getImage:@"SliderMaxTrackImage"];
            [mainSetting.mQualitySlider setMaximumTrackImage:sliderMinImage forState:UIControlStateNormal];
            [mainSetting.mQualitySlider setMinimumTrackImage:sliderMaxImage forState:UIControlStateNormal];
            [mainSetting.mTimesSlider setMaximumTrackImage:sliderMinImage forState:UIControlStateNormal];
            [mainSetting.mTimesSlider setMinimumTrackImage:sliderMaxImage forState:UIControlStateNormal];
            [mainSetting setSampleImageWithQuality:percent withResLoader:mResLoader];
        }
        break;
        case USER_INFO:
        {
        }
        break;
        default:
            break;
    }
}
@end
/////////////////////////////
@implementation SEOptionsView

// Only override drawRect: if you perform custom drawing.
// An empty implementation adversely affects performance during animation.
- (void)drawRect:(CGRect)rect
{
    // Drawing code
    //[super drawRect:rect];
    if(mBackground == nil)
    {
        UIImage* uiImage = [mResLoader getImage:@"MainSettingBackground"];
        mBackground = uiImage;
    }
    [mBackground drawInRect:rect];
}

- (id) initWithFrame:(CGRect)frame byViewNav: (SEViewNavigator*)viewNav withResLoader: (SEResLoader*)resLoader
{
    self = [super initWithFrame:frame];
    if(self)
    {
        mViewNav = viewNav;
        mResLoader = resLoader;
        mCurrentLeftBar = INVALID_LEFT_BAR_VIEW;
        self.userInteractionEnabled = YES;
    }
    return self;
}
- (void) initLeftView: (NSArray*)barStrings withRightViewCreator: (NSArray*)rightViewCreator
{
    if([barStrings count] == 0)
        return;
    if([rightViewCreator count] == 0)
        return;
    if([barStrings count] != [rightViewCreator count])
        return;
    float startx = LEFT_PADDING, starty = TOP_PADDING;
    float labelWidth = LEFT_BAR_WIDTH;
    float labelHeight = LEFT_BAR_HEIGHT;
    NSUInteger i = 0;
    for(NSString* labelStr in barStrings)
    {
        SEOptionLeftLabel* label = [[SEOptionLeftLabel alloc] initWithFrame:CGRectMake(startx, starty, labelWidth, labelHeight)];
        label.mOptionsView = self;
        label.mType = (OPTION_LEFT_BAR_TYPE)i;
        label.text = labelStr;
        label.textAlignment = UITextAlignmentCenter;
        label.userInteractionEnabled = YES;
        label.mSelectedImage = [mResLoader getImage:@"OptionLabelSelectedBackground"];
        label.mNormalImage = [mResLoader getImage:@"OptionLabelNormalBackground"];
        mLeftViewBars[i] = label;
        [self addSubview:label];
        [label release];
        starty += labelHeight + LEFT_BAR_VMARGIN;
        i++;
    }
    [mRightViewsCreator release];
    mRightViewsCreator = rightViewCreator;
    [mRightViewsCreator retain];
}
- (void) setCurrentLeftBar: (OPTION_LEFT_BAR_TYPE) barType
{
    if(mCurrentLeftBar == barType)
        return;
    if(barType < 0 || barType >= OPTION_LEFT_BAR_NUM)
        return;
    NSArray* views = mRightViews[barType];
    if(views == nil)
    {
        SEOptionsRightViewCreator* rightViewCreator = [mRightViewsCreator objectAtIndex:barType];
        views = [rightViewCreator createViews:self withResLoader:mResLoader];
        mRightViews[barType] = [views retain];
    }
    if(views == nil)
        return;
    NSArray* oldViews = mRightViews[mCurrentLeftBar];
    for(UIView* v in oldViews)
    {
        [v removeFromSuperview];    
    }
    for(UIView* v in views)
    {
        [self addSubview:v];
    }
    [self setDataToViews:views withType: barType];
    SEOptionLeftLabel* currentLabel = mLeftViewBars[barType];
    SEOptionLeftLabel* oldLabel = mLeftViewBars[mCurrentLeftBar];
    oldLabel.mSelected = NO;
    currentLabel.mSelected = YES;
    mCurrentLeftBar = barType;
    [currentLabel setNeedsDisplay];
    [oldLabel setNeedsDisplay];
}
- (void) initData
{
    NSArray* leftBarName = [NSArray arrayWithObjects:@"drawing setting", @"user info", nil];
    SEOptionsMainSettingCreator* creator = [[SEOptionsMainSettingCreator alloc] init];
    creator.mResLoader = mResLoader;
    creator.mViewNav = mViewNav;
    SEOptionsRightViewCreator* creator2 = [[SEOptionsRightViewCreator alloc] init];
    
    NSArray* creators = [NSArray arrayWithObjects:creator, creator2, nil];
    [creator release];
    [creator2 release];
    [self initLeftView:leftBarName withRightViewCreator:creators];
    [self setCurrentLeftBar:MAIN_SETTING];
}
- (BOOL)canAdjust
{
    return NO;
}
- (void)relayout
{}
- (void)dealloc
{
    for(int i = 0 ; i < OPTION_LEFT_BAR_NUM ; i++)
    {
        [mRightViews[i] release];
    }
    [mRightViewsCreator release];
    [super dealloc];
}
@end
