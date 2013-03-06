//
//  SEUserUpdateViewController.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-7-7.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEUserUpgradeViewController.h"
#import "SEUtil.h"
#import "SEViewNavigator.h"
#import "SEResDefine.h"
//#import "FontManager.h"
#import "FontLabel.h"
#import "SEUIProgressView.h"
#import "SEUserUpgrate.h"
#import "SESystemConfig.h"
#import "PhotoFrameAppDelegate.h"
#import "SEPopupViewWidgets.h"
#import "UpgradeInfo.h"
#import "SEUserDefaultManager.h"
#import "SETestNotification.h"
const CGFloat gImageLabelWidth = 115;
const CGFloat gImageLabelHeight = 104;
const CGFloat gBottomViewOffsetY = 36;
const CGFloat gBottomViewOffsetX = 105;
const CGFloat gBottomViewHeight = 126;

NSString* gFontName = [SEUtil getFontName];
enum IMAGE_LABEL_TYPE {IMAGE_TYPE, MUSIC_TYPE, BRUSH_TYPE, TIME_FONT_TYPE, EXP_TYPE};
static void setImage(UIImageView*imageView , SEViewNavigator* viewNav, NSString* name)
{
    UIImage* image = [viewNav.mResLoader getImage:name];
    imageView.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
}
static void setBottomImage(UIImageView*imageView , SEViewNavigator* viewNav, NSString* name)
{
    UIImage* image = [viewNav.mResLoader getImage:name];
    imageView.image = [SEUtil imageWithCapInset:image top:0 bottom:0 left:123 right:123];
}
static void setLabelFont(FontLabel* fontLabel, UIColor* color, CGFloat fontSize)
{
    [fontLabel setZFont:[[FontManager sharedManager] zFontWithName:[SESystemConfig getFontName] pointSize:fontSize]];
    fontLabel.textColor = color;
}
/////////////////////////////
@interface SEImageLabel : UIImageView    
{
    UIImageView* mTopImageView;
    FontLabel* mBottomLabel;
    IMAGE_LABEL_TYPE mType;
}
//@property (nonatomic, readonly) UIImageView* mBackground;
@property (nonatomic, assign) IMAGE_LABEL_TYPE mType;
@property (nonatomic, readonly) UIImageView* mTopImageView;
@property (nonatomic, readonly) FontLabel* mBottomLabel;
-(id) initWithFrame:(CGRect)frame textColor: (UIColor*) color;
@end
@implementation SEImageLabel
@synthesize mBottomLabel;
@synthesize mTopImageView;
@synthesize mType;
-(id) initWithFrame:(CGRect)frame textColor: (UIColor*) color
{
    self = [super initWithFrame:frame];
    if(self)
    {
        float imageHeight = 62;
        //mBackground = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, 128, 128)];
        mTopImageView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 8, 115, imageHeight)];
        mBottomLabel = [[FontLabel alloc] initWithFrame:CGRectMake(0, 65, frame.size.width, 31) zFont:[[FontManager  sharedManager] zFontWithName:[SEUtil getFontName] pointSize:25]];
        //mBottomLabel = [[FontLabel alloc] initWithFrame:CGRectMake(2, 97, 126, 30)];
        mBottomLabel.textColor = color;
        mBottomLabel.backgroundColor = [UIColor clearColor];
        //[self addSubview:mBackground];
        [self addSubview:mTopImageView];
        [self addSubview:mBottomLabel];
        //[mBackground release];
        [mTopImageView release];
        [mBottomLabel release];
        self.backgroundColor = [UIColor clearColor];
    }
    return self;
}

@end
//////////
struct LevelUpViewAndController
{
    SELeveUpViewController* controller;
    UIView* levelUpView;
};
struct AchieveViewAndController
{
    SEAchieveViewController* controller;
    UIView* achieveView;
};
/////////////////////
static SEImageLabel* getTypeView(UIView* contentParent, IMAGE_LABEL_TYPE type)
{
    NSArray* subviews = contentParent.subviews;
    for(UIView* v in subviews)
    {
        SEImageLabel* imageLabel = (SEImageLabel*)v;
        if(imageLabel.mType == type)
            return imageLabel;
    }
    return nil;
}
static UIImage* getImageByType(IMAGE_LABEL_TYPE type, SEViewNavigator* viewNav, NSArray* contents)
{
    UIImage* image = nil;
    switch (type) {
        case IMAGE_TYPE:
        {
            image = [viewNav.mResLoader getImage:@"LevelUpImageTypeIcon"];
        }
            break;
        case MUSIC_TYPE:
        {
            image = [viewNav.mResLoader getImage:@"LevelUpMusicTypeIcon"];
        }
            break;
        case BRUSH_TYPE:
        {
            image = [viewNav.mResLoader getImage:@"LevelUpBrushTypeIcon"];
        }
            break;
        case EXP_TYPE:
        {
            image = [UIImage imageNamed:@"leveupicon_001_b.png"];
        }
            break;
        case TIME_FONT_TYPE:
        {
            //image = [viewNav.mResLoader getImage:@"LevelUpTimeFo
            //image = [viewNav.mFontLoader getImage:@"am" style:1 size:FONT_NORMAL_SIZE];
            assert(contents.count > 0);
            if(contents.count > 0)
            {
                NSNumber* num = [contents objectAtIndex:0];
                int style = [num intValue];
                image = [[[PhotoFrameAppDelegate getViewNavigator] mFontLoader] getImage:@"am" style:style size:FONT_NORMAL_SIZE];
            }
        }
            break;
        default:
            break;
    }
    return image;
}
static void  addImageLabel(SEViewNavigator* viewNav, float& startx, UIView* contentParent , NSString* str, IMAGE_LABEL_TYPE type,  UIColor* textColor, NSArray* contents)
{
    SEImageLabel* imageLabel = getTypeView(contentParent, type);
    if(imageLabel == nil)
    {
        imageLabel = [[SEImageLabel alloc] initWithFrame:CGRectMake(startx, 0, gImageLabelWidth, gImageLabelHeight) textColor:textColor];
        startx += gImageLabelWidth;
    }
    UIImage* image = getImageByType(type, viewNav, contents);
    float imageHeight = 62;
    CGSize dstSize = CGSizeMake(105, 62);
    CGSize s = [SEUtil computeFitSize:CGSizeMake(image.size.width, image.size.height) toDst:dstSize];
    UIImage* newImage = [SEUtil drawImage:image toSize:s];
    setImage(imageLabel, viewNav, @"UpgradeViewAchieveIconBg");
    imageLabel.mTopImageView.image = newImage   ;
    imageLabel.mTopImageView.frame = CGRectMake((imageLabel.frame.size.width - newImage.size.width) / 2, 0, newImage.size.width, newImage.size.height);
    imageLabel.mBottomLabel.text = str;
    imageLabel.mBottomLabel.textAlignment = UITextAlignmentCenter;
    [contentParent addSubview:imageLabel];
    [imageLabel release];
}

///////////////////
/////////////////////
@implementation SELeveUpViewController
@synthesize mViewNav;
@synthesize mArrow;
@synthesize mLevelBg;
@synthesize mToLevelBg;
@synthesize mFromLevelBg;
@synthesize mScrollView;
@synthesize mProgressView;
@synthesize mProgressViewBg;
@synthesize mLevelToLabel;
@synthesize mLevelFromLabel;
@synthesize mContentParent;
@synthesize mLevelIcon;
@synthesize mLevelIconBg;
- (void) dealloc
{
    [mLevelBg release];
    [mLevelIconBg release];
    [mLevelIcon release];
    [mFromLevelBg release];
    [mToLevelBg release];
    [mArrow release];
    [mProgressView release];
    [mProgressViewBg release];
    [mScrollView release];
    [mLevelFromLabel release];
    [mLevelToLabel release];
    [super dealloc];
}
- (void) initView
{
    setImage(mArrow, mViewNav, @"UpgradeViewLevelArrow");
    setImage(mLevelIconBg, mViewNav, @"UpgradeViewLevelUpBg");
    
    //setImage(mLevelIcon, mViewNav, @"UpgradeLevelUpIcon");
    mLevelIcon.image = [mViewNav.mResLoader getImage:@"UpgradeLevelUpIcon"];
    setImage(mLevelBg, mViewNav, @"UpgradeViewLevelUpBg");
    setImage(mToLevelBg, mViewNav, @"UpgradeViewLevelUpTo");
    setImage(mFromLevelBg, mViewNav, @"UpgradeViewLevelUpFrom");
    setImage(mProgressViewBg, mViewNav, @"UpgradeViewProgressViewBg");
    [mProgressView initData:mViewNav];
    mContentParent = [[UIView alloc] init];
    [mScrollView addSubview:mContentParent];
    [mContentParent release];
    setLabelFont(mLevelFromLabel, [UIColor whiteColor], 50);
    setLabelFont(mLevelToLabel, [UIColor whiteColor], 50);
}
- (SEImageLabel*) getTypeView :(IMAGE_LABEL_TYPE)type
{
    NSArray* subviews = mContentParent.subviews;
    for(UIView* v in subviews)
    {
        SEImageLabel* imageLabel = (SEImageLabel*)v;
        if(imageLabel.mType == type)
            return imageLabel;
    }
    return nil;
}
- (UIImage*) getImageByType: (IMAGE_LABEL_TYPE)type : (NSArray*)contents
{
    UIImage* image = nil;
    switch (type) {
        case IMAGE_TYPE:
        {
            image = [mViewNav.mResLoader getImage:@"LevelUpImageTypeIcon"];
        }
            break;
        case MUSIC_TYPE:
        {
            image = [mViewNav.mResLoader getImage:@"LevelUpMusicTypeIcon"];
        }
            break;
        case BRUSH_TYPE:
        {            
            image = [mViewNav.mResLoader getImage:@"LevelUpBrushTypeIcon"];
        }
            break;
        case TIME_FONT_TYPE:
        {
            assert(contents.count > 0);
            NSNumber* num = [contents objectAtIndex:0];
            int style = [num intValue];
            image = [mViewNav.mFontLoader getImage:@"am" style:style size:FONT_NORMAL_SIZE];
            /*
            SEUserUpgrade* upgrade = [mViewNav getUserUpgrade];
            NSArray* timeArray = [upgrade getAllBrushIDByUserCurrentLevel];
            if(timeArray.count > 0)
            {
                NSNumber* num = [timeArray objectAtIndex:0];
                int style = [num intValue];
                image = [mViewNav.mFontLoader getImage:@"am" style:style size:FONT_NORMAL_SIZE];
                
            }
            else
            {
                image = [mViewNav.mResLoader getImage:@"LevelUpTimeFontTypeIcon"];
            }
             */
        }
            break;
        default:
            break;
    }
    return image;
}
- (void) addImageLabel: (NSString*) str type: (int)type color : (UIColor*)textColor : (NSArray*) contents
{
    SEImageLabel* imageLabel = [self getTypeView:(IMAGE_LABEL_TYPE)type];
    if(imageLabel == nil)
    {
        imageLabel = [[SEImageLabel alloc] initWithFrame:CGRectMake(startx, 0, gImageLabelWidth, gImageLabelHeight) textColor:textColor];
        startx += gImageLabelWidth + 4;
    }
    UIImage* image = [self getImageByType:(IMAGE_LABEL_TYPE)type : contents];
    ////
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    float imageHeight = 62;
    CGSize dstSize = CGSizeMake(105, 62);
    CGSize s = [SEUtil computeFitSize:CGSizeMake(image.size.width, image.size.height) toDst:dstSize];
    UIImage* newImage = [SEUtil drawImage:image toSize:s];
    setImage(imageLabel, viewNav, @"UpgradeViewAchieveIconBg");
    imageLabel.mTopImageView.image = newImage   ;
    imageLabel.mTopImageView.frame = CGRectMake((imageLabel.frame.size.width - newImage.size.width) / 2, 6, newImage.size.width, newImage.size.height);
    imageLabel.mBottomLabel.text = str;
    imageLabel.mBottomLabel.textAlignment = UITextAlignmentCenter;
    [mContentParent addSubview:imageLabel];
    [imageLabel release];
}
- (void) setFromLevel: (int) level
{
    self.mLevelFromLabel.text = [NSString stringWithFormat:@"%d", level];
}
- (void) setTolevel: (int) level
{
    self.mLevelToLabel.text = [NSString stringWithFormat:@"%d", level];
}
- (void) setProgressViewPercent: (double) percent
{
    self.mProgressView.percent  = percent;
}
- (void) setImageNum: (int) num
{
    NSString* str = [NSString stringWithFormat:@"+%d", num];
    [self addImageLabel:str type:IMAGE_TYPE color:[UIColor yellowColor]:nil];
}
- (void) setMusicNum: (int) num
{
    NSString* str = [NSString stringWithFormat:@"+%d", num];
    [self addImageLabel:str type:MUSIC_TYPE color:[UIColor greenColor]:nil];
}
- (void) setBrushNum: (int) num : (NSArray*) brushArray
{
    NSString* str = [NSString stringWithFormat:@"+%d", num];
    [self addImageLabel:str type:BRUSH_TYPE color:[UIColor greenColor]:nil];
}
- (void) setTimeFontNum: (int) num : (NSArray*)timeFontArray
{
    NSString* str = [NSString stringWithFormat:@"+%d", num];
    [self addImageLabel:str type:TIME_FONT_TYPE color:[UIColor greenColor]:timeFontArray];
}
@end

@implementation SEAchieveViewController
@synthesize mViewNav;
@synthesize mAchieveBg;
@synthesize mAchieveIcon;
@synthesize mAchieveDetailBg;
@synthesize mAchieveDetailScrollView;
@synthesize mAchieveIconBg;
@synthesize mContentParent;
@synthesize mAchieveDesc;
@synthesize startx;
- (void) dealloc
{
    [mAchieveIcon release];
    [mAchieveIconBg release];
    [mAchieveBg release];
    [mAchieveDetailBg release];
    [mAchieveDetailScrollView release];
    [mAchieveDesc release];
    [super dealloc];
    
}
- (void) initView
{
    setImage(mAchieveBg, mViewNav, @"UpgradeViewAchieveBg");
    setImage(mAchieveIconBg, mViewNav, @"UpgradeViewAchieveIconBg");
    //setImage(mAchieveDetailBg, mViewNav, @"UpgradeViewAchieveDetailBg");
    setImage(mAchieveDetailBg, mViewNav, @"UpgradeViewLevelUpBg");
    setLabelFont(mAchieveDesc, [UIColor whiteColor], 20);
    mContentParent = [[UIView alloc] init];
    [mAchieveDetailScrollView addSubview:mContentParent];
    [mContentParent release];
    //setImage(mAchieveTitleBg, mViewNav, @"UpgradeViewAchieveTitleBg");
}
- (void) setAchieve:(UIImage*) achiveImage imageNum: (int) imageNum musicNum : (int) musicNum brushNum: (int) brushNum : (int)timeFontNum: (int) expNum
{
    //float startx = self.startx;
    mAchieveIcon.image = achiveImage;
    addImageLabel(mViewNav, startx, self.mContentParent, [NSString stringWithFormat:@"+%d",imageNum], IMAGE_TYPE, [UIColor yellowColor], nil);
    startx += 10;
    addImageLabel(mViewNav, startx, self.mContentParent, [NSString stringWithFormat:@"+%d",musicNum], MUSIC_TYPE, [UIColor greenColor], nil);
    
    if(brushNum > 0)
    {
        startx += 10;
        addImageLabel(mViewNav, startx, self.mContentParent, [NSString stringWithFormat:@"+%d", 1], BRUSH_TYPE, [UIColor greenColor], nil);
    }
    if(timeFontNum > 0)
    {
        startx += 10;
        NSArray* timeStyles = [NSArray arrayWithObject:[NSNumber numberWithInt:timeFontNum]];
        addImageLabel(mViewNav, startx, self.mContentParent, [NSString stringWithFormat:@"+%d",timeStyles.count], TIME_FONT_TYPE, [UIColor greenColor], timeStyles);
    }
    if(expNum > 0)
    {
        startx += 10;
        addImageLabel(mViewNav, startx, self.mContentParent, [NSString stringWithFormat:@"+%d", expNum], EXP_TYPE, [UIColor greenColor], nil);
    }
    //controller.startx = startx;
    self.mAchieveDetailScrollView.contentSize = CGSizeMake(startx + 200, self.mAchieveDetailScrollView.frame.size.height);
}
@end

@implementation SEUserUpgradeTestController
@synthesize mReviewNum;
@synthesize mDrawImageNum;
@synthesize mPlayMusicNum;
@synthesize mReviewButton;
@synthesize mShareImageNum;
@synthesize mDrawImageButton;
@synthesize mPlayMusicButton;
@synthesize mShareImageButton;
@synthesize mContentData;
@synthesize mExpNum;
@synthesize mBrushSeq;
@synthesize mTimeStyleSeq;
@synthesize mDownloadLabel;
- (int) getNumFromTextField: (UITextField*)textField
{
    NSString* numStr  = [textField text];
    int num = 0;
    if([numStr isEqualToString:@""] == NO)
    {
        num = [numStr intValue];
    }
    return num;
}
- (IBAction) shareImageHandler:(id)sender
{
    NSLog(@"share image handler");
    int num = [self getNumFromTextField:mShareImageNum];
    for (int i = 0; i < num ; i++) {
        [[mViewNav getUserUpgrade] shareOneImage];
    }
}
- (IBAction) playMusicHandler:(id)sender
{
    NSLog(@"play music handler");
    int num = [self getNumFromTextField:mPlayMusicNum];
    for(int i = 0 ; i < num ; i++)
    {
        [[mViewNav getUserUpgrade] finishOneMusicPlay];
    }
}
- (IBAction) reviewHandler:(id)sender
{
    NSLog(@"review handler");
    int num = [self getNumFromTextField:mReviewNum];
    for(int i = 0 ; i < num ; i++)
    {
        [[mViewNav getUserUpgrade] finishOneComment];
    }
}
- (IBAction) showNotificationTest:(id)sender
{
    [mViewNav notificationShow];
}
- (IBAction) batterLevelChangeTest:(id)sender
{
    NSString* notifStr = [SETestNotification getTestLevelChangeNotificationStr];
    [[NSNotificationCenter defaultCenter] postNotificationName:notifStr object:nil];
}
- (IBAction) drawImageHandler:(id)sender
{
    NSLog(@"draw image handler");
    int num = [self getNumFromTextField:mDrawImageNum];
    for(int i = 0 ; i < num ; i++)
    {
        [[mViewNav getUserUpgrade] finishOneImageDrawing];
    }
    NSArray* upgradeInfoArray = [mViewNav getUpgradeInfoArray];
    for(int i = 0 ; i < upgradeInfoArray.count ; i++)
    {
        UpgradeInfo* upgradeInfo = [upgradeInfoArray objectAtIndex:i];
        NSLog(@"up from level = %@", upgradeInfo.fromlevel);
        NSLog(@"up to level = %@", upgradeInfo.tolevel);
        NSLog(@"up achieve type = %@", upgradeInfo.achievementtype);
        NSLog(@"up medal = %@", upgradeInfo.medal);
    }
    //[self performSelector:@selector(buyMaxLevel:) withObject:nil afterDelay:10];
}
- (IBAction) buyMaxLevel:(id)sender
{
    NSLog(@"buy max level");
    [SEUserDefaultManager buyProduct: @"com.thespeedsun.PhotoFrame.LevelUpMax"];
    [[NSUserDefaults standardUserDefaults] synchronize];
    [[mViewNav getUserUpgrade] setToMaxLevel];

}
- (IBAction)downloadBrushData:(id)sender
{
    NSLog(@"download brush data");
    [mViewNav downloadParamConfig:mDownloadLabel textName:@"paramset_url.txt"];
}
- (IBAction) buyBasicSetting:(id)sender
{
    NSLog(@"buy basic setting");
    [SEUserDefaultManager buyProduct: @"com.thespeedsun.PhotoFrame.BasicSetting2"];
    [[NSUserDefaults standardUserDefaults] synchronize];

}
- (IBAction) buyBrushSetting:(id)sender
{
    NSLog(@"buy brush setting");
    [SEUserDefaultManager buyProduct: @"com.thespeedsun.PhotoFrame.BrushSetting2"];
    [[NSUserDefaults standardUserDefaults] synchronize];
}
- (IBAction) restoreProduct:(id)sender
{
    NSLog(@"restore product");
    SEProductManager* productManager = [PhotoFrameAppDelegate getProductManager];
    int num = [productManager getProductNum];
    NSArray* discountProduct = [productManager getAllDiscount];
    for(int i = 0 ; i < num ; i++)
    {
        SEProduct* product = [productManager getProductByIndex:i];
        BOOL isBasicSettingItem = product.func == BASIC_FUNC;
        [[NSUserDefaults standardUserDefaults] setValue:[NSNumber numberWithBool:NO] forKey: product.productId];
        if(isBasicSettingItem == NO)
        {
            [SEUserDefaultManager setFunction:product.func value:NO];
        }
    }
    for(SEProduct* product in discountProduct)
    {
        BOOL isBasicSettingItem = product.func == BASIC_FUNC;
        [[NSUserDefaults standardUserDefaults] setValue:[NSNumber numberWithBool:NO] forKey: product.productId];
        if(isBasicSettingItem == NO)
        {
            [SEUserDefaultManager setFunction:product.func value:NO];
        }
    }
}
- (IBAction) buyBrush:(id)sender
{
    NSString* num = mBrushSeq.text;
    NSString* productID = nil;
    if([num isEqualToString:@"1"])
    {
        productID = @"com.thespeedsun.PhotoFrame.Brush.NEWSBP001";
    }
    else if([num isEqualToString:@"2"])
    {
        productID = @"com.thespeedsun.PhotoFrame.Brush.NEWSBP002";
    }
    else if([num isEqualToString:@"3"])
    {
        productID = @"com.thespeedsun.PhotoFrame.Brush.NEWSBP003";
    }
    else if([num isEqualToString:@"4"])
    {
        productID = @"com.thespeedsun.PhotoFrame.Brush.NEWSBP004";
    }
    else if([num isEqualToString:@"5"])
    {
        productID = @"com.thespeedsun.PhotoFrame.Brush.NEWSBP005";
    }
    else if([num isEqualToString:@"6"])
    {
        productID = @"com.thespeedsun.PhotoFrame.Brush.NEWSBP006";
    }
    else if([num isEqualToString:@"60"])
    {
        productID = @"com.thespeedsun.PhotoFrame.Brush.NEWSBP001_Discount";
    }
    if(productID != nil)
    {
        NSLog(@"buy brush = %@", productID);
        [SEUserDefaultManager buyProduct: productID];
        [[NSUserDefaults standardUserDefaults] synchronize];
        
    }
}
- (IBAction) shouldRotate:(id)sender
{
    BOOL b = [mViewNav isShouldRotate];
    [mViewNav setShouldRotate: !b];
}
- (IBAction) buyTimeStyle:(id)sender
{
    NSString* num = mTimeStyleSeq.text;
    NSString* productID = nil;
    if([num isEqualToString:@"1"])
    {
        productID = @"com.thespeedsun.PhotoFrame.TimeStyle.SFP001";
    }
    else if([num isEqualToString:@"2"])
    {
        productID = @"com.thespeedsun.PhotoFrame.TimeStyle.SFP002";
    }
    if(productID != nil)
    {
        NSLog(@"buy time = %@", productID);
        [SEUserDefaultManager buyProduct: productID];
        [[NSUserDefaults standardUserDefaults] synchronize];
    }
}
- (IBAction) addExp:(id)sender
{
    int num = [self getNumFromTextField:mExpNum];
    [[mViewNav getUserUpgrade] addExp:num];
}
- (IBAction) clearAllData:(id)sender
{
    [[mViewNav getUserUpgrade] clearAllData];
}
- (IBAction) okButton:(id)sender
{
    [mViewNav dismissPopupTest];
}
- (void) initView
{
    mViewNav = [PhotoFrameAppDelegate getViewNavigator];    
    mContentData.text = [[mViewNav getUserUpgrade] getAllData];
}
- (UIView*) getView
{
    UIView* view = [[[NSBundle mainBundle] loadNibNamed:@"UpgradeInfoTest" owner:self options:nil] lastObject];
    [self initView];
    return view;
}
- (void)dealloc
{
    [mShareImageNum release];
    [mDrawImageNum release];
    [mPlayMusicNum release];
    [mReviewNum release];
    [mShareImageButton release];
    [mDrawImageButton release];
    [mPlayMusicButton release];
    [mReviewButton release];
    [mExpNum release];
    [mContentData release];
    [mBrushSeq release];
    [mTimeStyleSeq release];
    [mDownloadLabel release];
    [super dealloc];
}
@end

@implementation SEUserUpgradeViewController
//@synthesize mBottomBg;
//@synthesize mTopBg;
//@synthesize mAchieveView;
//@synthesize mLevelUpView;
@synthesize mViewNav;
//@synthesize mAchieveViewController;
//@synthesize mLevelUpViewController;
//@synthesize mContentScrollView;
//@synthesize mOkButton;
//@synthesize mUpgradeType;
- (void) update
{
    
}
/*
- (void) addToParent: (UIView*)parent
{
    //[parent addSubview: mUserUpgradeView];
}
 */
/*
- (void) setData
{
    setImage(mTopBg, mViewNav, @"UpgradeViewTopBg");
    //setImage(mBottomBg, mViewNav, @"UpgradeViewBottomBg");
    UIImage* image = [mViewNav.mResLoader getImage:@"UpgradeViewBottomBg"];
    mBottomBg.image = [SEUtil imageWithCapInset:image top:0 bottom:0 left:123 right:123];
    mContentParentView = [[UIView alloc] init];
    [mContentScrollView addSubview:mContentParentView];
    mContentScrollView.backgroundColor = [UIColor clearColor];
    [mContentParentView release];
    mAchieveTitleBg = [[UIImageView alloc] initWithFrame:CGRectMake(10, 0, mUserUpgradeView.frame.size.width - 20, 55)];
    setImage(mAchieveTitleBg, mViewNav, @"UpgradeViewAchieveTitleBg");
    mAchieveTitleLabel = [[FontLabel alloc] initWithFrame:CGRectMake(20, 0, mUserUpgradeView.frame.size.width - 20, 55) fontName:[SESystemConfig getFontName] pointSize:30];
    mAchieveTitleLabel.backgroundColor = [UIColor clearColor];
    setLabelFont(mAchieveTitleLabel, [UIColor grayColor], 30);
    mAchieveTitleLabel.text = @"New Achievement";
    
    mLevelUpTitleBg = [[UIImageView alloc] initWithFrame:CGRectMake(10, 0, mUserUpgradeView.frame.size.width - 20, 55)];
    setImage(mLevelUpTitleBg, mViewNav, @"UpgradeViewLevelUpTitleBg");
    mLevelUpTitleLabel = [[FontLabel alloc] initWithFrame:CGRectMake(20, 0, mUserUpgradeView.frame.size.width - 20, 55)];
    mLevelUpTitleLabel.backgroundColor = [UIColor clearColor];
    setLabelFont(mLevelUpTitleLabel, [UIColor grayColor], 30);
    mLevelUpTitleLabel.text = @"Level Up";
    mUserUpgradeView.backgroundColor = [UIColor clearColor];
}
 */
/*
- (void) initView
{
    mUserUpgradeView = [[[NSBundle mainBundle] loadNibNamed:@"UserLevelView" owner:self options:nil] lastObject];
    switch (mUpgradeType) {
        case ACHIEVE_ONLY:
        {
            
        }
            break;
        case LEVEL_UP_ONLY:
        {}
            break;
        case ACHIEVE_LEVE_UP:
        {}
            break;
        default:
            break;
    }
    [self setData];
    [mOkButton setButtonBackground:@"ShareButtonNormalImage" select:@"ShareButtonHImage"];
    [mOkButton setTextImage:@"ok" indicateImage:nil alignment:UITextAlignmentCenter];
}
 */
- (void) dealloc
{
    //[mAchieveTitleLabel release];
    //[mAchieveTitleBg release];
    //[mLevelUpTitleBg release];
    //[mLevelUpTitleLabel release];
    //[mTopBg release];
    //[mBottomBg release];
    //[mContentScrollView release];
    //[mOkButton release];
    [super dealloc];
}
/*
- (void) clearAll
{
    NSArray* subviews = mContentParentView.subviews;
    for(UIView* v in subviews)
    {
        [v removeFromSuperview];
    }
}
 */
- (struct LevelUpViewAndController) createLevelUpViewContorller
{
    SELeveUpViewController* levelUpViewController = [[[SELeveUpViewController alloc] init] autorelease];
    levelUpViewController.mViewNav = mViewNav;
    UIView* levelUpView = [[[NSBundle mainBundle] loadNibNamed:@"LevelUpView" owner:levelUpViewController options:nil] lastObject];
    levelUpView.backgroundColor = [UIColor clearColor];
    SEPopupLabel* levelUpLabel = [[SEPopupLabel alloc] initWithFrame:CGRectMake(0, 0, levelUpView.frame.size.width, 55)];
    UIView* view = [[[UIView alloc] initWithFrame:CGRectMake(0, 0, levelUpView.frame.size.width, levelUpView.frame.size.height + levelUpLabel.frame.size.height)] autorelease];
    [view addSubview:levelUpLabel];
    [levelUpLabel release];
    
    levelUpView.frame = CGRectMake(0, levelUpLabel.frame.size.height, levelUpView.frame.size.width, levelUpView.frame.size.height);
    [view addSubview:levelUpView];
    
    [levelUpLabel setText:@"Level Up"];
    levelUpLabel.label.textColor = [UIColor whiteColor];
    setImage(levelUpLabel.background, mViewNav, @"UpgradeViewLevelUpTitleBg");
    [levelUpViewController initView];
    LevelUpViewAndController data;
    data.controller = levelUpViewController;
    data.levelUpView = view;
    return data;
}
/*
- (SELeveUpViewController*) getLevelUpViewController
{
    if(mLevelUpViewController == nil)
    {
        mLevelUpViewController = [[SELeveUpViewController alloc] init];
        mLevelUpViewController.mViewNav = mViewNav;
        mLevelUpView = [[[NSBundle mainBundle] loadNibNamed:@"LevelUpView" owner:mLevelUpViewController options:nil]  lastObject];
        [mLevelUpViewController initView];
        mLevelUpTitleBg.frame = CGRectMake(0, starty, mLevelUpTitleBg.frame.size.width, mLevelUpTitleBg.frame.size.height);
        mLevelUpTitleLabel.frame = CGRectMake(0, starty, mLevelUpTitleLabel.frame.size.width, mLevelUpTitleLabel.frame.size.height);
        starty += mLevelUpTitleBg.frame.size.height;
        mLevelUpView.frame = CGRectMake(0, starty, mLevelUpView.frame.size.width, mLevelUpView.frame.size.height);
        [mContentParentView addSubview:mLevelUpTitleBg];
        [mContentParentView addSubview:mLevelUpTitleLabel];
        [mContentParentView addSubview:mLevelUpView];
        starty += mLevelUpView.frame.size.height;
        mLevelUpView.backgroundColor = [UIColor clearColor];
        mContentScrollView.contentSize = CGSizeMake(mContentScrollView.frame.size.width, starty);
        [mLevelUpViewController.mProgressView initData:mViewNav];
    }
    return mLevelUpViewController;
}
 */
- (struct AchieveViewAndController) createAchieveViewController
{
    SEAchieveViewController* controller = [[[SEAchieveViewController alloc] init] autorelease];
    UIView* achieveView = [[[NSBundle mainBundle] loadNibNamed:@"AchieveView" owner:controller options:nil] lastObject];
    achieveView.backgroundColor = [UIColor clearColor];
    
    /////////////////////////////////////////////////////////
    AchieveViewAndController data;
    data.controller = controller;
    data.achieveView = achieveView;
    return data;
}
/*
- (SEAchieveViewController*) getNewAchieveController
{
    mAchieveViewController = [[SEAchieveViewController alloc] init];
    mAchieveViewController.mViewNav = mViewNav;
    mAchieveView = [[[NSBundle mainBundle] loadNibNamed:@"AchieveView" owner:mAchieveViewController options:nil] lastObject];
    mAchieveView.backgroundColor = [UIColor clearColor];
    [mAchieveViewController initView];
    return mAchieveViewController;
}
 */
/*
- (void) setFromLevel: (int) level
{
    SELeveUpViewController* controller = [self getLevelUpViewController];
    controller.mLevelFromLabel.text = [NSString stringWithFormat:@"%d", level];
}
 */
/*
- (void) setTolevel: (int) level
{
    SELeveUpViewController* controller = [self getLevelUpViewController];
    controller.mLevelToLabel.text = [NSString stringWithFormat:@"%d", level];
}
 */
/*
- (void) setProgressViewPercent: (double) percent
{
    SELeveUpViewController* controller = [self getLevelUpViewController];
    controller.mProgressView.percent  = percent;
}
*/
/*
- (void) setImageNum: (int) num
{
    SELeveUpViewController* controller = [self getLevelUpViewController];
    NSString* str = [NSString stringWithFormat:@"+%d", num];
    [controller addImageLabel:str type:IMAGE_TYPE color:[UIColor yellowColor]];
}
 */
/*
- (void) setMusicNum: (int) num
{
    SELeveUpViewController* controller = [self getLevelUpViewController];
    NSString* str = [NSString stringWithFormat:@"+%d", num];
    [controller addImageLabel:str type:MUSIC_TYPE color:[UIColor greenColor]];
}
 */
/*
- (void) setBrushNum: (int) num
{
    SELeveUpViewController* controller = [self getLevelUpViewController];
    NSString* str = [NSString stringWithFormat:@"+%d", num];
    [controller addImageLabel:str type:BRUSH_TYPE color:[UIColor greenColor]];
}
- (void) setTimeFontNum: (int) num
{
    SELeveUpViewController* controller = [self getLevelUpViewController];
    NSString* str = [NSString stringWithFormat:@"+%d", num];
    [controller addImageLabel:str type:TIME_FONT_TYPE color:[UIColor greenColor]];
}
 */
/*
- (BOOL) hasAchieveTitle
{
    NSArray* subviews = mContentParentView.subviews;
    for(UIView* v in subviews)
    {
        if(v == mAchieveTitleBg)
        {
            return YES;
        }
        
    }
    return NO;
}
*/
/*
- (void) addAchieve:(UIImage*) achiveImage imageNum: (int) imageNum musicNum : (int) musicNum brushNum: (int) brushNum
{
    SEAchieveViewController* controller = [self getNewAchieveController];
    if([self hasAchieveTitle] == NO)
    {
        mAchieveTitleBg.frame = CGRectMake(0, starty, mAchieveTitleBg.frame.size.width, mAchieveTitleBg.frame.size.height);
        mAchieveTitleLabel.frame = CGRectMake(0, starty, mAchieveTitleLabel.frame.size.width, mAchieveTitleLabel.frame.size.height);
        starty += mAchieveTitleBg.frame.size.height;
        [mContentParentView addSubview:mAchieveTitleBg];
        [mContentParentView addSubview:mAchieveTitleLabel];
    }
    [mContentParentView addSubview:mAchieveView];
    float startx = controller.startx;
    addImageLabel(mViewNav, startx, controller.mContentParent, [NSString stringWithFormat:@"+%d",imageNum], IMAGE_TYPE, [UIColor yellowColor]);
    startx += 10;
    addImageLabel(mViewNav, startx, controller.mContentParent, [NSString stringWithFormat:@"+%d",musicNum], MUSIC_TYPE, [UIColor greenColor]);
    controller.startx = startx;
    controller.mAchieveDetailScrollView.contentSize = CGSizeMake(startx + 200, controller.mAchieveDetailScrollView.frame.size.height);
    mAchieveView.frame = CGRectMake(0, starty, mAchieveView.frame.size.width, mAchieveView.frame.size.height);
    [mContentParentView addSubview:mAchieveView];
    starty += mAchieveView.frame.size.height;
    mContentScrollView.contentSize = CGSizeMake(mContentScrollView.frame.size.width, mUserUpgradeView.frame.size.height);
    mContentParentView.userInteractionEnabled = YES;
    controller.mAchieveIcon.image = achiveImage;
}
 */
- (IBAction)okButtonHandler:(id)sender
{
    [mViewNav hideUserUpgradeView];
}
- (SETextImageButton*) createOKButton:(UIView*)parentView
{
    float buttonWidth  = 177;
    float buttonHeight = 61;
    SETextImageButton* button = [[SETextImageButton alloc] initWithFrame:CGRectMake(parentView.frame.size.width - buttonWidth - 20, parentView.frame.size.height - 76, buttonWidth, buttonHeight)];
    [button setTextImage:@"ok" indicateImage:nil alignment:UITextAlignmentCenter];
    [button setButtonHandler:self action:@selector(okButtonHandler:)];
    [button setButtonBackground:@"ShareButtonNormal" select:@"ShareButtonH"];
    return [button autorelease];
}
- (NSArray*) sortLevelArray: (NSArray*)srcArray
{
    return [srcArray sortedArrayUsingComparator:^NSComparisonResult(id obj1, id obj2) {
        NSNumber* num1 = (NSNumber*)obj1;
        NSNumber* num2 = (NSNumber*)obj2;
        return [num1 compare:num2];
    }];
}
- (void) getFromToLevel:(int*)fromLevel : (int*) toLevel
{
    NSArray* upgradeInfoArray = [mViewNav getUpgradeInfoArray];
    NSArray* fromLevelArray = [NSArray array];
    NSArray* toLevelArray = [NSArray array];
    for(UpgradeInfo* upgradeInfo in upgradeInfoArray)
    {
        if([upgradeInfo.fromlevel intValue] != INVALID_LEVEL && [upgradeInfo.tolevel intValue] != INVALID_LEVEL)
        {
            fromLevelArray = [ fromLevelArray arrayByAddingObject:upgradeInfo.fromlevel];
            toLevelArray = [toLevelArray arrayByAddingObject:upgradeInfo.tolevel];
        }
    }
    if(fromLevelArray.count > 0 && toLevelArray.count > 0)
    {
        fromLevelArray = [self sortLevelArray:fromLevelArray];
        toLevelArray = [self sortLevelArray:toLevelArray];
        *fromLevel = [[fromLevelArray objectAtIndex:0] intValue];
        *toLevel = [[toLevelArray objectAtIndex:toLevelArray.count - 1] intValue];
    }
}
- (void) getBrushAndTimeFontDataFromUserData: (int)fromeLevel : (int)toLevel : (int*)brushNum  : (int*)timeFontNum : (NSMutableArray*)brushArray : (NSMutableArray*)timeFontArray;
{
    int tmpBrushNum = 0;
    int tmpTimeFontNum = 0;
    for(int level = (fromeLevel + 1); level <= toLevel ; level++)
    {
        SEUserData* userData = [mViewNav getUserData:level];
        if(userData.brushNum > 0)
        {
            tmpBrushNum++;
            [brushArray addObject:[NSNumber numberWithInt:userData.brushNum]];
        }
        if(userData.timeFontNum > 0)
        {
            NSLog(@"font id = %d", userData.timeFontNum);
            tmpTimeFontNum++;
            [timeFontArray addObject:[NSNumber numberWithInt:userData.timeFontNum]];
        }
    }
    *brushNum = tmpBrushNum;
    *timeFontNum = tmpTimeFontNum;
}
- (void) setLevelupContorllerValue:(SELeveUpViewController*)controller
{
    int fromLevel = INVALID_LEVEL;
    int toLevel = INVALID_LEVEL;
    [self getFromToLevel:&fromLevel :&toLevel];
    if(fromLevel != INVALID_LEVEL)
        [controller setFromLevel:fromLevel];
    if(toLevel != INVALID_LEVEL)
        [controller setTolevel:toLevel];
    if(fromLevel != INVALID_LEVEL)
    {
        SEUserData* prevUserData = [mViewNav getUserData:fromLevel];
        SEUserData* currUserData = [mViewNav getUserData:toLevel];
        int maxLevel =  [mViewNav getMaxUserLevel];
        [controller setProgressViewPercent:toLevel /(float)maxLevel];
        int deltaImage = currUserData.imageListNum - prevUserData.imageListNum;
        [controller setImageNum:deltaImage];
        int deltaMusic = currUserData.musicListNum - prevUserData.musicListNum;
        [controller setMusicNum:deltaMusic];
        int brushNum = 0 ;
        int timeFontNum = 0;
        NSMutableArray* brushArray = [NSMutableArray array];
        NSMutableArray* timeFontArray = [NSMutableArray array];
        [self getBrushAndTimeFontDataFromUserData: fromLevel : toLevel : &brushNum : &timeFontNum : brushArray: timeFontArray];
        if(brushNum > 0)
        {
            [controller setBrushNum:brushNum : brushArray];
        }            
        if(timeFontNum > 0)
        {
            [controller setTimeFontNum:timeFontNum : timeFontArray];
        }
    }
}
- (UIView*) createLevelupOnlyView
{
    LevelUpViewAndController data = [self createLevelUpViewContorller];
    [self setLevelupContorllerValue:data.controller];
    UIView* allView = [[[UIView alloc] init ] autorelease];
    allView.backgroundColor = [UIColor clearColor];
    allView.userInteractionEnabled = YES;
    float paddingx = 10;
    float paddingy = 10;
    UIImageView* topView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, data.levelUpView.frame.size.width + 2 * paddingx, data.levelUpView.frame.size.height + 2 * paddingy)];
    float bottomStarty = topView.frame.size.height;
    float offsety = gBottomViewOffsetY - 2;
    float offsetx = gBottomViewOffsetX;
    float width = topView.frame.size.width - offsetx;
    float height = gBottomViewHeight;
    UIImageView* bottomView = [[UIImageView alloc] initWithFrame:CGRectMake(offsetx, bottomStarty - offsety, width, height)];
    allView.frame = CGRectMake(0, 0, topView.frame.size.width, bottomView.frame.size.height + bottomView.frame.origin.y);
    
    //[topView addSubview:data.levelUpView];
    
    data.levelUpView.frame = CGRectMake(paddingx, paddingy, data.levelUpView.frame.size.width, data.levelUpView.frame.size.height);
    [allView addSubview:topView];
    [allView addSubview:bottomView];
    [allView addSubview:data.levelUpView];
    [bottomView release];
    [topView release];
    setImage(topView, mViewNav, @"UpgradeViewTopBg");
    setBottomImage(bottomView, mViewNav, @"UpgradeViewBottomBg");
    SETextImageButton* button = [self createOKButton:allView];
    [allView addSubview:button];
    
    return allView;
}
- (NSArray*) getAchieveArray
{
    NSArray* array = [mViewNav getUpgradeInfoArray];
    NSArray* achieveArray = [NSArray array];
    NSLog(@"##### all get achievement ####");
    for(UpgradeInfo* up in array)
    {
        if([up.fromlevel intValue] == INVALID_LEVEL)
        {
            achieveArray = [achieveArray arrayByAddingObject:up];
            NSLog(@"achieve type = %@", up.achievementtype);
            NSLog(@"medal type = %@", up.medal);
        }
    }
    return achieveArray;
}
-(UIView*) createAchieveView: (UpgradeInfo*)upgradeInfo
{
    int achieveType = [upgradeInfo.achievementtype intValue];
    int medal = [upgradeInfo.medal intValue];
    SEAchieveData data = [[mViewNav getUserUpgrade] getAchieveData:achieveType medal:medal];
    UIImage* image = [[mViewNav getUserUpgrade] getAchieveMedalImage:achieveType medal:medal];
    SEAchieveViewController* controller = [[[SEAchieveViewController alloc] init] autorelease];
    controller.mViewNav = mViewNav;
    UIView* achieveView = [[[NSBundle mainBundle] loadNibNamed:@"AchieveView" owner:controller options:nil] lastObject];
    achieveView.backgroundColor = [UIColor clearColor];
    [controller initView];
    controller.mAchieveDesc.text = [[[PhotoFrameAppDelegate getViewNavigator] getUserUpgrade] getAchieveDescription:achieveType medal:medal];
    [controller setAchieve:image imageNum:data.imageListNumDelta musicNum:data.musicListNumDelta brushNum:data.brushNumDelta : data.timeFontDelta: data.expNum];
    return achieveView;
}
- (UIView*) createAchieveOnlyView
{
    //UIImageView* topView = [[[UIImageView alloc] initWithFrame:CGRectMake(0, 0, 0, 0)] autorelease];
    float allViewWidth = 694;
    float topViewWidth = 694;
    float topViewHeight = 598;
    float allViewHeight = 689;
    float paddingx = 10;
    float paddingy = 10;
    float titleWidth = allViewWidth - 2 * paddingx;
    //float scrollViewWidth = 679;
    //float scrollViewHeight = 579;
    //float achieveViewHeight = 196 ;
    UIView* allView = [[[UIView alloc] init] autorelease];
    allView.frame = CGRectMake(0, 0, allViewWidth, allViewHeight);
    UIImageView* topView = [[[UIImageView alloc] initWithFrame:CGRectMake(0, 0, topViewWidth, topViewHeight)] autorelease];
    topView.userInteractionEnabled = YES;
    topView.backgroundColor = [UIColor clearColor];
    setImage(topView, mViewNav, @"UpgradeViewTopBg");
    [allView addSubview:topView];
    float tmpStarty = paddingy;
    SEPopupLabel* levelUpLabel = [[SEPopupLabel alloc] initWithFrame:CGRectMake(paddingx, tmpStarty, titleWidth, 55)];
    [levelUpLabel setText:@"New Achieve"];
    levelUpLabel.label.textColor = [UIColor whiteColor];
    setImage(levelUpLabel.background, mViewNav, @"UpgradeViewLevelUpTitleBg");
    [topView addSubview:levelUpLabel];
    [levelUpLabel release];
    
    tmpStarty += levelUpLabel.frame.size.height;
    NSArray* achieveArray = [self getAchieveArray];
    SEAchieveViewController* tmpController = [[[SEAchieveViewController alloc] init] autorelease];
    UIView* achieveView = [[[NSBundle mainBundle] loadNibNamed:@"AchieveView" owner:tmpController options:nil] lastObject];
    float achieveScrollViewHeight = achieveView.frame.size.height * 1.5;
    UIScrollView* achieveScrollView = [[UIScrollView alloc] initWithFrame: CGRectMake(paddingx, tmpStarty, achieveView.frame.size.width, achieveScrollViewHeight)];
    [topView addSubview:achieveScrollView];
    [achieveScrollView release];
    achieveScrollView.clipsToBounds = YES;
    float scrollStartY = 0;
    for(int i = 0 ; i < achieveArray.count ; i++)
    {
        UpgradeInfo* upgradeInfo = [achieveArray objectAtIndex:i];
        UIView* achieveView = [self createAchieveView:upgradeInfo];
        achieveView.frame = CGRectMake(0, scrollStartY, achieveView.frame.size.width, achieveView.frame.size.height);
        [achieveScrollView addSubview:achieveView];
        scrollStartY += achieveView.frame.size.height;
    }
    achieveScrollView.contentSize = CGSizeMake(achieveView.frame.size.width, scrollStartY);
    tmpStarty += achieveScrollViewHeight;
    tmpStarty += 10;
    topView.frame = CGRectMake(0, 0, topViewWidth, tmpStarty);
    UIImageView* bottomView = [[[UIImageView alloc] initWithFrame:CGRectMake(gBottomViewOffsetX, tmpStarty - gBottomViewOffsetY, topViewWidth - gBottomViewOffsetX, gBottomViewHeight)] autorelease];
    setBottomImage(bottomView, mViewNav, @"UpgradeViewBottomBg");
    allView.frame = CGRectMake(0, 0, allViewWidth, bottomView.frame.origin.y + bottomView.frame.size.height);
    
    [allView addSubview:bottomView];
    [allView addSubview:topView];
         
    SETextImageButton* button = [self createOKButton:allView];
    [allView addSubview:button];
    return allView;
}
-(UIView*) createLevelUpAchieveView
{
    float allViewWidth = 694;
    float allViewHeight = 689;
    float topViewWidth = 694;
    float topViewHeight = 598;
    float scrollViewWidth = 679;
    float scrollViewHeight = 579;
    UIView* allView = [[[UIView alloc] initWithFrame:CGRectMake(0, 0, allViewWidth, allViewHeight)] autorelease];
    allView.userInteractionEnabled = YES;
    allView.backgroundColor = [UIColor clearColor];
    UIImageView* topView = [[[UIImageView alloc] initWithFrame:CGRectMake(0, 0, topViewWidth, topViewHeight)] autorelease];
    topView.userInteractionEnabled = YES;
    topView.backgroundColor = [UIColor clearColor];
    setImage(topView, mViewNav, @"UpgradeViewTopBg");
    UIScrollView* scrollView = [[[UIScrollView alloc] initWithFrame:CGRectMake(8, 9, scrollViewWidth, scrollViewHeight)] autorelease];
    scrollView.backgroundColor = [UIColor clearColor];
    UIView* parentView = [[[UIView alloc] init] autorelease];
    parentView.userInteractionEnabled = YES;
    parentView.backgroundColor = [UIColor clearColor];
    [topView addSubview:scrollView];
    [scrollView addSubview:parentView];
    
    LevelUpViewAndController data = [self createLevelUpViewContorller];
    [self setLevelupContorllerValue:data.controller];
    [parentView addSubview:data.levelUpView];
    float tmpStartY = data.levelUpView.frame.size.height;
    
    SEPopupLabel* achieveLabel = [[SEPopupLabel alloc] initWithFrame:CGRectMake(0, tmpStartY, data.levelUpView.frame.size.width, 55)];
    [achieveLabel setText:@"NEW Achievement"];
    achieveLabel.label.textColor = [UIColor whiteColor];
    setImage(achieveLabel.background, mViewNav, @"UpgradeViewLevelUpTitleBg");
    [parentView addSubview:achieveLabel];
    [achieveLabel release];
    tmpStartY += achieveLabel.frame.size.height;
    
    NSArray* achieveArray = [self getAchieveArray];
    for(int i = 0 ; i < achieveArray.count ; i++)
    {
        UpgradeInfo* upgradeInfo = [achieveArray objectAtIndex:i];
        UIView* achieveView = [self createAchieveView:upgradeInfo];
        achieveView.frame = CGRectMake(0, tmpStartY, achieveView.frame.size.width, achieveView.frame.size.height);
        [parentView addSubview:achieveView];
        tmpStartY += achieveView.frame.size.height;
    }
    parentView.frame = CGRectMake(0, 0, scrollView.frame.size.width, tmpStartY);
    scrollView.contentSize = CGSizeMake(parentView.frame.size.width, parentView.frame.size.height);
    NSLog(@"achieve array count = %d, scrollView height = %f", achieveArray.count, parentView.frame.size.height);
    float bottomViewY = topView.frame.origin.y + topView.frame.size.height;
    UIImageView* bottomView= [[[UIImageView alloc] initWithFrame:CGRectMake(gBottomViewOffsetX, bottomViewY - gBottomViewOffsetY, topViewWidth - gBottomViewOffsetX, gBottomViewHeight)] autorelease];
    setBottomImage(bottomView, mViewNav, @"UpgradeViewBottomBg");
    [allView addSubview:bottomView];
    [allView addSubview:topView];
    SETextImageButton* button = [self createOKButton:allView];
    [allView addSubview:button];
    return allView;
}
- (UIView*) getView
{
    BOOL hasLevelUpInfo = NO;
    BOOL hasAchieveInfo = NO;
    NSArray* upgradeInfoArray = [mViewNav getUpgradeInfoArray];
    for(UpgradeInfo* upgradeInfo in upgradeInfoArray)
    {
        if([upgradeInfo.fromlevel intValue] != INVALID_LEVEL)
        {
            hasLevelUpInfo = YES;
            break;
        }
    }
    NSArray* achieveArray = [self getAchieveArray];
    if(achieveArray.count > 0)
        hasAchieveInfo = YES;
    if(hasLevelUpInfo == YES && hasAchieveInfo == YES)
        return [self createLevelUpAchieveView];
    else if(hasLevelUpInfo == YES && hasAchieveInfo == NO)
        return [self createLevelupOnlyView];
    else if(hasLevelUpInfo == NO && hasAchieveInfo == YES)
        return [self createAchieveOnlyView];
    else 
        return nil;
}
- (UIView*) getTestPopup
{
    return nil;
}

@end
