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
#define LEFT_PADDING 10
#define TOP_PADDING 10
#define LEFT_BAR_WIDTH 300
#define LEFT_BAR_HEIGHT 50
#define LEFT_BAR_VMARGIN 20
#define LEFT_RIGTH_MARGIN 100 
#define PROGRESS_BAR_WIDTH 320
#define PROGRESS_BAR_HEIGHT 42
////////////////////////

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
///////
@interface SEUserUpgradeProgressView : UIView
{
@private
    UIImage* mBackground;
    UIImage* mForeground;
    CGFloat percent;//value is 0 -- 1
}
@property (nonatomic, retain) UIImage* mBackground;
@property (nonatomic, retain) UIImage* mForeground;
@property (nonatomic, assign) CGFloat percent;
@end
@implementation SEUserUpgradeProgressView
@synthesize mBackground;
@synthesize mForeground;
- (void)dealloc
{
    [mBackground release];
    [mForeground release];
    [super dealloc];
}
- (void) drawRect:(CGRect)rect
{
    [mBackground drawInRect:rect];
    if(percent > 0)
    {
        CGRect fgRect = CGRectMake(rect.origin.x, rect.origin.y, rect.size.width * percent, rect.size.height);
        [mForeground drawInRect:fgRect];
    }
}
- (CGFloat) percent
{
    return percent;
}
- (void) setPercent:(CGFloat)p
{
    if(fabsf(percent - p) <= 0.00001)
        return;
    percent = p;
    [self setNeedsDisplay];
}
@end
////////
@implementation SEOptionsRightViewController
@synthesize mViewNav;
- (void) addViewsToParent: (UIView*) parent
{

}
- (void) removeViewsFromParent
{}
- (void) createViews: (NSString*)nibName frame: (CGRect) frame
{}

@end
/////////////
@implementation SEIssueReportController
@synthesize title;
@synthesize description;
@synthesize sendButton;
@synthesize titleStatic;
@synthesize descriptionStatic;
@synthesize outputLabel;
- (void) dealloc
{
    [title release];
    [description release];
    [sendButton release];
    [titleStatic release];
    [descriptionStatic release];
    [outputLabel release];
    [mRecvData release];
    [super dealloc];
}
- (void) addViewsToParent:(UIView *)parent
{
    [parent addSubview:title];
    [parent addSubview:description];
    [parent addSubview:sendButton];
    [parent addSubview:titleStatic];
    [parent addSubview:descriptionStatic];
    [parent addSubview:outputLabel];
}
- (void)removeViewsFromParent
{
    [title removeFromSuperview];
    [description removeFromSuperview];
    [sendButton removeFromSuperview];
    [titleStatic removeFromSuperview];
    [descriptionStatic removeFromSuperview];
    [outputLabel removeFromSuperview];
}
- (void) createViews:(NSString *)nibName frame:(CGRect)frame
{
    [[NSBundle mainBundle] loadNibNamed:nibName owner:self options:nil];
}
- (id)init
{
    self = [super init];
    if(self)
    {
        mRecvData = [NSMutableData data];
        mRecvData = [mRecvData retain];
    }
    return self;
}
- (void) setStatusMessage: (NSString*) msg
{
    outputLabel.text = msg;
}
//
- (IBAction)sendAction:(id)sender
{
    NSLog(@"send issue report");
    
    NSString* titleText = title.text;
    NSString* descriptionText = description.text;
    if([titleText isEqualToString:@""])
    {
        outputLabel.text = @"error: title is empty";
        return;
    }
    if([descriptionText isEqualToString:@""])
    {
        outputLabel.text = @"error: description is empty";
        return;
    }
    SEIssueReportData* ird = [[SEIssueReportData alloc] init];
    ird.title = titleText;
    ird.description = descriptionText;
    UIDevice* device = [UIDevice currentDevice];
    NSString* deviceName = [device name];
    ird.deviceName = deviceName;
    ird.mViewNav = mViewNav;
    ird.delegate = self;
    SEDataUploadManager* manager = [mViewNav getDataUploadManager];
    [manager upload:ird];
    [ird release];
    /*
    if([SEUtil reachabilityWithLocalWifi] == NO)
    {
        outputLabel.text = @"error: wifi network is not ready";
        return;
    }
    NSUInteger titleTextLen = [titleText length];
    NSUInteger desTextLen = [descriptionText length];
    NSLog(@"title len = %d, des text len = %d", titleTextLen, desTextLen);
    NSLog(@"title = %@, description = %@", titleText, descriptionText);
    NSString* strURL = @"http://mobilefly.sinaapp.com/sendmessage.php";
    NSURL* url = [NSURL URLWithString:strURL];
    NSMutableURLRequest* req = [NSMutableURLRequest requestWithURL:url];
    [req setHTTPMethod:@"POST"];
    //NSString* contentType = @"application/x-www-form-urlencoded";
    //[req setValue:contentType forHTTPHeaderField:@"Content-Type"];
    NSMutableData* postBody = [NSMutableData data];
    NSString* deviceSysName = [device systemName];
    CFUUIDRef deviceRef = CFUUIDCreate(NULL);
    CFStringRef deviceId =  CFUUIDCreateString (NULL, deviceRef);
    NSString* deviceIdStr = [NSString stringWithFormat:@"%@", (NSString*)deviceId];
    CFRelease(deviceRef);
    CFRelease(deviceId);
    NSLog(@"device name = %@, system name = %@, deivce id = %@", deviceName, deviceSysName, deviceIdStr);
    NSDate* date = [NSDate date];
    NSTimeInterval timeInterv = [date timeIntervalSince1970];
    [postBody appendData:[[NSString stringWithFormat:@"devname=%@&", deviceName] dataUsingEncoding:NSUTF8StringEncoding]];
    [postBody appendData:[[NSString stringWithFormat:@"date=%f&", timeInterv] dataUsingEncoding:NSUTF8StringEncoding]];
    [postBody appendData:[[NSString stringWithFormat:@"title=%@&", titleText] dataUsingEncoding:NSUTF8StringEncoding]];
    [postBody appendData:[[NSString stringWithFormat:@"description=%@", descriptionText] dataUsingEncoding:NSUTF8StringEncoding]];
    [req setHTTPBody:postBody];
    NSHTTPURLResponse* response = nil;
    NSError* error = nil;
    NSData* returnData = [NSURLConnection sendSynchronousRequest:req returningResponse:&response error:&error];
    //NSURLConnection* conn = [NSURLConnection connectionWithRequest:req delegate:self];
    //[conn retain];
    int statusCode = response.statusCode;
    NSString* str = [[[NSString alloc] initWithData:returnData encoding:NSUTF8StringEncoding] autorelease];
    NSLog(@"return str = %@", str);
    if([str isEqualToString:@"OK"] && statusCode == 200 && error == nil)
    {
        outputLabel.text =  @"send message ok";
    }
    else 
    {
        outputLabel.text = @"send message failed";
    }
    */
}
/*
- (void)connection:(NSURLConnection *)connection didReceiveResponse:(NSURLResponse *)response
{
    NSLog(@"start response");
    NSString* encode = [response textEncodingName];
    NSLog(@"## encode name = %@ ##", encode);
    NSDictionary* dict = [response allHeaderFields];
    NSArray* allkeys = [dict allKeys];
    NSArray* allValues = [dict allValues];
    for(NSString* str in allkeys)
    {
        NSLog(@"key = %@\n", str);
    }
    for(NSString* str in allValues)
    {
        NSLog(@"value = %@\n",str);
    }
}
- (void)connection:(NSURLConnection *)connection didReceiveData:(NSData *)data
{
    NSLog(@"receive data");
    [mRecvData appendData:data];
}
- (void)connection:(NSURLConnection *)connection didFailWithError:(NSError *)error
{
    NSLog(@"error response");
    [connection release];
}
- (void)connectionDidFinishLoading:(NSURLConnection *)connection
{
    NSLog(@"finished connection\n");
    int len = [mRecvData length];
    const char* bytes = (const char*)[mRecvData bytes];
    char* data = (char*)malloc(len + 1);
    memset(data, 0, len + 1);
    memcpy(data, bytes, len);
    NSString* str = [NSString stringWithCString:data encoding:NSUTF8StringEncoding];
    NSLog(@"%@", str);
    [connection release];
    free(data);
    if([str isEqualToString:@"OK"])
    {
        
    }
}  
*/
@end
/////////
struct TowImagePair
{
    UIImage* hasMedal;
    UIImage* hasNoMedal;
};
@implementation SEOptionsUserUpgradeInfoController

- (void) addViewsToParent: (UIView*) parent
{
    [parent addSubview:mUserInfoScrollView];
}
- (struct TowImagePair) getImage: (int)medalType :(int)medalLevel
{
    int index = medalType * MEDAL_LEVEL_COUNT + medalLevel;
    UserMedalData umd = gUserMedalData[index];
    TowImagePair p;
    p.hasMedal = [mViewNav.mResLoader getImage:umd.imageNameHasMedal];
    p.hasNoMedal = [mViewNav.mResLoader getImage:umd.imageNameNoMedal];
    return p;
}
- (void)dealloc
{
    [mUserInfoScrollView release];
    [super dealloc];
}
- (void) removeViewsFromParent
{
    [mUserInfoScrollView removeFromSuperview];
}
- (void) createViews: (NSString*)nibName frame: (CGRect) frame
{
    [mUserInfoScrollView release];
    mUserInfoScrollView = [[UIScrollView alloc] initWithFrame:frame];
    CGFloat hSpacing = 20;
    CGFloat vSpacing = 30;
    CGFloat startx = 0;
    CGFloat starty = 0;
    UIImage* medalBackground = [mViewNav.mResLoader getImage:@"OptionsUserMedalBackground"];
    CGFloat medalBgWidth = medalBackground.size.width;
    CGFloat medalBgHeight = medalBackground.size.height;
    UIImage* upgradeBackground = [mViewNav.mResLoader getImage:@"OptionsUserUpgradeInfoBackground"];
    CGFloat userInfoBgWidth = upgradeBackground.size.width;
    CGFloat userInfoBgHeight = upgradeBackground.size.height;
    UIImage* medalImage = [mViewNav.mResLoader getImage:@"UserUpgradePresentOnDutyNoCopper"];
    CGFloat medalImageWidth = medalImage.size.width;
    CGFloat medalImageHeight = medalImage.size.height;
    CGFloat contentWidth = 0, contentHeight = 0;
    UIView* v = [[UIView alloc] init];
    [mUserInfoScrollView addSubview:v];
    [v release];
    UserInfo* userInfo = [mViewNav getUserInfo];
    assert(userInfo);
    
    for(int i = 0 ;i < USER_MEDAL_COUNT * MEDAL_LEVEL_COUNT ; i++)
    {
        UIImageView* leftBackground = [[UIImageView alloc] init];
        CGFloat currentStartx = startx;
        CGFloat currentStarty = starty + i * medalBgWidth;
        leftBackground.frame = CGRectMake(currentStartx, currentStarty, medalBgWidth, medalBgHeight);
        leftBackground.image = medalBackground;
        [v addSubview:leftBackground];
        [leftBackground release];
        
        UIImageView* medalView = [[UIImageView alloc] init];
        CGFloat medalStartx = currentStartx + (medalBgWidth - medalImageWidth) / 2;
        CGFloat medalStarty = currentStarty + (medalBgHeight - medalImageHeight) / 2;
        medalView.frame = CGRectMake(medalStartx, medalStarty, medalImageWidth, medalImageHeight);
        medalView.tag = 110 + i;
        [v addSubview:medalView];
        [medalView release];
        
        CGFloat rightBgStartx = startx + medalBgWidth + hSpacing;
        CGFloat rightBgStarty = currentStarty + (medalBgHeight - userInfoBgHeight) / 2;
        UIImageView* rightBackground = [[UIImageView alloc] init];
        rightBackground.image = upgradeBackground;
        rightBackground.frame = CGRectMake(rightBgStartx, rightBgStarty, userInfoBgWidth, userInfoBgHeight);
        [v addSubview:rightBackground];
        [rightBackground release];
        
        SEUserUpgradeProgressView* pv = [[SEUserUpgradeProgressView alloc] initWithFrame:CGRectMake(rightBgStartx + 50, rightBgStarty + 56, PROGRESS_BAR_WIDTH, PROGRESS_BAR_HEIGHT)];
        UIImage* pvBg = [mViewNav.mResLoader getImage:@"UserUpgradeProgressViewBackground"];
        UIImage* pvFg = [mViewNav.mResLoader getImage:@"UserUpgradeProgressViewForeground"];
        pv.mBackground = pvBg;
        pv.mForeground = pvFg;
        pv.percent = 0.1;
        [v addSubview:pv];
        [pv release];
    }
    NSNumber* medalLevel[USER_MEDAL_COUNT];
    medalLevel[PRESENTONDUTY_MEDAL] = userInfo.presentondutymedal;
    medalLevel[NEWPERSON_MEDAL] = userInfo.newpersonmedal;
    medalLevel[DRAWING_MEDAL] = userInfo.drawingmedal;
    medalLevel[SHARE_MEDAL] = userInfo.sharemedal;
    medalLevel[FANS_MEDAL] = userInfo.fansmedal;
    for(int i = 0 ; i < USER_MEDAL_COUNT ; i++)
    {
        for(int j = 0 ; j < MEDAL_LEVEL_COUNT ; j++)
        {
            UIImageView* imageView = (UIImageView*)[v viewWithTag:110 + i * MEDAL_LEVEL_COUNT + j];
            TowImagePair imagePair = [self getImage:i :j];
            if([medalLevel[i] intValue] == INVALID_MEDAL_LEVEL)
                imageView.image = imagePair.hasNoMedal;
            else
            {
                if(j <= [medalLevel[i] intValue])
                {
                    imageView.image = imagePair.hasMedal;
                }
                else
                    imageView.image = imagePair.hasNoMedal;
            }
        }
    }
    contentHeight = MEDAL_LEVEL_COUNT * USER_MEDAL_COUNT * medalBgHeight + (USER_MEDAL_COUNT - 1) * vSpacing + starty;
    contentWidth = startx + medalBgWidth + hSpacing + userInfoBgWidth;
    mUserInfoScrollView.contentSize = CGSizeMake(contentWidth, contentHeight);
}

@end
///////
@implementation SEOptionsMainSettingController
@synthesize mDownloadIndicate;
@synthesize mChangeConfig;
@synthesize mSampleView;
@synthesize mCurrentTimes;
@synthesize mCurrentQuality;
@synthesize mQualitySlider;
@synthesize mTimesSlider;
@synthesize mSignature;
@synthesize mQualityLabel;
@synthesize mTimesLabel;
@synthesize mImageBackground;
- (IBAction)signatureButtonHandler:(id)sender
{
    //[mViewNav moveToView:OPTIONS_SIGNATURE:SIGNATURE_VIEW hasAnimation:YES];
    [mViewNav setViewRelationType:TYPE1];
    [mViewNav moveToView:OPTIONS_SIGNATURE :SIGNATURE_PREVIEW hasAnimation:YES isPush:YES];
}
- (IBAction)changeConfigHandler:(id)sender
{
    [mViewNav downloadParamConfig: mDownloadIndicate];
}
- (void) addViewsToParent: (UIView*) parent
{
    [parent addSubview:mImageBackground];
    [parent addSubview:mSampleView];
    [parent addSubview: mCurrentTimes];
    [parent addSubview: mCurrentQuality];
    [parent addSubview: mQualitySlider];
    [parent addSubview: mTimesSlider];
    [parent addSubview: mSignature];
    [parent addSubview: mQualityLabel];
    [parent addSubview: mTimesLabel];
    [parent addSubview: mChangeConfig];
    [parent addSubview:mDownloadIndicate];
}
- (void) removeViewsFromParent
{
    [mSampleView removeFromSuperview];
    [mCurrentTimes removeFromSuperview];
    [mCurrentQuality removeFromSuperview];
    [mQualitySlider removeFromSuperview];
    [mTimesSlider removeFromSuperview];
    [mSignature removeFromSuperview];
    [mQualityLabel removeFromSuperview];
    [mTimesLabel removeFromSuperview];
    [mImageBackground removeFromSuperview];
    [mChangeConfig removeFromSuperview];
    [mDownloadIndicate removeFromSuperview];
}
- (void)createViews:(NSString*)nibName frame: (CGRect) frame
{
    [[NSBundle mainBundle] loadNibNamed:nibName owner:self options:nil];
    
}
- (void) dealloc
{
    [mDownloadIndicate release];
    [mSampleView release];
    [mCurrentTimes release];
    [mCurrentQuality release];
    [mQualitySlider release];
    [mTimesSlider release];
    [mSignature release];
    [mQualityLabel release];
    [mTimesLabel release];
    [mImageBackground release];
    [mChangeConfig release];
    [super dealloc];
}
- (id) init
{
    self = [super init];
    if(self)
    {
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
        [self setSampleImageWithQuality:v withResLoader:mViewNav.mResLoader];
    }
    mCurrentQuality.text = [NSString stringWithFormat:@"%d", v];    
    [mViewNav setImageQuality:[mCurrentQuality.text intValue]];
    [mViewNav setImageTimes:[mCurrentTimes.text intValue]];
}
- (IBAction)timesSliderHandler:(UISlider*)sender
{
    int v = (int)sender.value;
    mCurrentTimes.text = [NSString stringWithFormat:@"%d",v];
    [mViewNav setImageTimes:[mCurrentTimes.text intValue]];
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
//////////
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
    [mOptionsView setCurrentBarView:mType];    
}
- (void) touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event
{}
- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event
{}
- (void) touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event
{}
@end
///////
@interface SEOptionsView (Private)
- (void) leftBarTapHandler: (UITapGestureRecognizer*)tapGes;
- (void) setDataToViews:(OPTION_LEFT_BAR_TYPE) barType;
@end
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

- (void) setDataToViews:(OPTION_LEFT_BAR_TYPE) barType
{
    switch (barType)
    {
        case MAIN_SETTING:
        {
            SEOptionsMainSettingController* mainSetting = (SEOptionsMainSettingController*)mRightViewController[barType];
            PainterManager* pm = [PainterManager painterManager];
            mainSetting.mCurrentTimes.text = [NSString stringWithFormat:@"%d", [mViewNav getImageTimes]];//[NSString stringWithFormat:@"%d", pm.painterProperty.times];
            mainSetting.mCurrentQuality.text = [NSString stringWithFormat:@"%d", [mViewNav getImageQuality]];//[NSString stringWithFormat:@"%d", pm.painterProperty.percent];
            mainSetting.mQualitySlider.minimumValue = 4;
            mainSetting.mQualitySlider.maximumValue = 10;
            
            int maxTimes, minTimes;
            [pm getMinMaxTimesValue: pm.painterProperty.percent outMin:&minTimes outMax:&maxTimes];
            mainSetting.mTimesSlider.minimumValue = minTimes;
            mainSetting.mTimesSlider.maximumValue = maxTimes;
            int percent = [mViewNav getImageQuality];//pm.painterProperty.percent;
            int times = [mViewNav getImageTimes];//pm.painterProperty.times;
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
    CGRect leftRect = CGRectMake(0, 0, mLeftBackground.size.width, mLeftBackground.size.height);
    [mLeftBackground drawInRect:leftRect];
    CGRect rightRect = CGRectMake(leftRect.size.width, 0, mRightBackground.size.width, mRightBackground.size.height);
    [mRightBackground drawInRect:rightRect];
}

- (id) initWithFrame:(CGRect)frame byViewNav: (SEViewNavigator*)viewNav withResLoader: (SEResLoader*)resLoader
{
    self = [super initWithFrame:frame];
    if(self)
    {
        mViewNav = viewNav;
        mResLoader = resLoader;
        mCurrentLeftBar = INVALID_LEFT_BAR_VIEW;
        for(int i = 0 ; i < OPTION_LEFT_BAR_NUM ; i++)
            mRightViewController[i] = nil;
        self.userInteractionEnabled = YES;
    }
    return self;
}
- (void) initLeftView
{
    UIImage* labelNormalImage = [mResLoader getImage:@"OptionLabelNormalBackground"];
    UIImage* labelSelectedImage = [mResLoader getImage:@"OptionLabelSelectedBackground"];
    float startx = 0, starty = 0;
    float labelWidth = labelNormalImage.size.width;
    float labelHeight = labelNormalImage.size.height;
    for(int i = 0 ; i < INVALID_LEFT_BAR_VIEW ; i++)
    {
        SEOptionLeftLabel* label = [[SEOptionLeftLabel alloc] initWithFrame:CGRectMake(startx, starty, labelWidth, labelHeight)];
        label.mOptionsView = self;
        label.mType = (OPTION_LEFT_BAR_TYPE)i;
        label.text = gOptionsBarViewData[i].barLabel;
        label.textAlignment = UITextAlignmentCenter;
        label.userInteractionEnabled = YES;
        label.mSelectedImage = labelSelectedImage;
        label.mNormalImage = labelNormalImage;
        mLeftViewBars[i] = label;
        [self addSubview:label];
        [label release];
        starty += labelHeight + LEFT_BAR_VMARGIN;
    }

}
- (SEOptionsRightViewController*) createRightViewController: (OPTION_LEFT_BAR_TYPE)barType
{
    OptionsBarViewData bvd = gOptionsBarViewData[barType];
    Class controllerClass = NSClassFromString(bvd.className);
    
    SEOptionsRightViewController* c = [controllerClass new];
    c.mViewNav = mViewNav;
    return c;
}
- (void) setCurrentBarView: (OPTION_LEFT_BAR_TYPE) barType
{
    if(mCurrentLeftBar == barType)
        return;
    if(barType < 0 || barType >= OPTION_LEFT_BAR_NUM)
        return;
    SEOptionsRightViewController* rightViewController = mRightViewController[barType];
    if(rightViewController == nil)
    {
        mRightViewController[barType] = [self createRightViewController: barType]; 
        rightViewController = mRightViewController[barType];
        assert(rightViewController != nil);
        CGRect frame = CGRectMake(0 + mLeftBackground.size.width, 0, mRightBackground.size.width, mRightBackground.size.height);
        [rightViewController createViews:gOptionsBarViewData[barType].viewNibName frame:frame];
    }
    SEOptionsRightViewController* old = mRightViewController[mCurrentLeftBar];
    [old removeViewsFromParent];
    [rightViewController addViewsToParent:self];
    
    [self setDataToViews: barType];
    SEOptionLeftLabel* currentLabel = mLeftViewBars[barType];
    currentLabel.mSelected = YES;
    SEOptionLeftLabel* oldLabel = mLeftViewBars[mCurrentLeftBar];
    oldLabel.mSelected = NO;
    
    mCurrentLeftBar = barType;
    [currentLabel setNeedsDisplay];
    [oldLabel setNeedsDisplay];
}
- (void) initData
{
    if(mLeftBackground == nil)
    {
        UIImage* leftBackgroundImage = [mResLoader getImage:@"OptionsLeftBackground"];
        mLeftBackground =leftBackgroundImage;
    }
    if(mRightBackground == nil)
    {
        UIImage* uiImage = [mResLoader getImage:@"MainSettingBackground"];
        mRightBackground = uiImage;
    }
    [self initLeftView];
    /*
    for(int i = 0 ; i < OPTION_LEFT_BAR_NUM ; i++)
    {
        Class controllerClass = NSClassFromString(gOptionsBarViewData[i].className);
        mRightViewController[i] = [controllerClass new];
        assert(mRightViewController[i]);
    }
     */
    //[self setCurrentBarView:MAIN_SETTING];
}
- (BOOL)canAdjust
{
    return NO;
}
- (void)relayout
{}
- (void) update
{}
- (void)dealloc
{
    for(int i = 0 ; i < OPTION_LEFT_BAR_NUM ; i++)
    {
        [mRightViewController[i] release];
    }
    [super dealloc];
}
@end
