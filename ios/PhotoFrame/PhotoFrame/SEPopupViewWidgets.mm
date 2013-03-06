//
//  SEOptionsViewWidgets.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-9-21.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEPopupViewWidgets.h"
#import "PhotoFrameAppDelegate.h"
#import "SEUtil.h"
#import "SEResDefine.h"
#import "SEViewNavigator.h"
#import "FontLabel.h"
#import "SESystemConfig.h"
///////////
static void setLabelFont(FontLabel* fontLabel, NSString* text, UIColor* color ,NSString* fontName, CGFloat fontSize)
{
    [fontLabel setZFont:[[FontManager sharedManager] zFontWithName:fontName pointSize:fontSize]];
    fontLabel.text = text;
    fontLabel.backgroundColor = [UIColor clearColor];
    fontLabel.textColor = color;
}
@implementation SEPopupButton

@synthesize buttonText;
@synthesize button;
- (void) createChild:(CGRect)frame
{
    button = [UIButton buttonWithType:UIButtonTypeCustom];
    button.frame = CGRectMake(0, 0, frame.size.width, frame.size.height);
    UIImage* normalImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"AppStoreButtonNormalBg"];
    normalImage = [SEUtil imageWithCap:normalImage top:0.1 bottom:0.9 left:0.1 right:0.9];
    UIImage* selectImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"AppStoreButtonSelectBg"];
    selectImage = [SEUtil imageWithCap:selectImage top:0.1 bottom:0.9 left:0.1 right:0.9];
    [button setBackgroundImage:normalImage forState:UIControlStateNormal];
    [button setBackgroundImage:selectImage forState:UIControlStateHighlighted];
    [self addSubview:button];
    
    buttonText = [[FontLabel alloc] initWithFrame:CGRectMake(0, 0, frame.size.width, frame.size.height) fontName:[SEUtil getFontName] pointSize:30];
    [self addSubview:buttonText];
    [buttonText release];
    buttonText.textAlignment = UITextAlignmentCenter;
    self.backgroundColor = [UIColor clearColor];
    buttonText.backgroundColor = [UIColor clearColor];
}
- (void) setButtonBackground: (NSString*) normalStr : (NSString*) highlightedStr
{
    UIImage* normalImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:normalStr];
    normalImage = [SEUtil imageWithCap:normalImage top:0.1 bottom:0.9 left:0.1 right:0.9];
    UIImage* selectImage = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:highlightedStr];
    selectImage = [SEUtil imageWithCap:selectImage top:0.1 bottom:0.9 left:0.1 right:0.9];
    [button setBackgroundImage:normalImage forState:UIControlStateNormal];
    [button setBackgroundImage:selectImage forState:UIControlStateHighlighted];
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
@implementation SEPopupImageView
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
/////
@implementation SEPopupImageTextLabel
- (void) createChild: (CGRect) frame
{
    mBackground = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, frame.size.width, frame.size.height)];
    [self addSubview:mBackground];
    [mBackground release];
    
    mTextImage = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, frame.size.width, frame.size.height)];
    [self addSubview:mTextImage];
    [mTextImage release];
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
        [self createChild:self.frame];
    }
    return self;
}
- (void) setText: (NSString*)text
{
    UIImage* image = [SESystemConfig getTextImage:text];
    float x = (self.frame.size.width - image.size.width) / 2;
    float y = (self.frame.size.height - image.size.height) / 2;
    mTextImage.frame = CGRectMake(x, y , image.size.width, image.size.height);
    mTextImage.image = image;
}
- (void) setBackground: (NSString*) imageName
{
    UIImage* image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:imageName];
    mBackground.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
}

@end

//////
@implementation SEPopupLabel
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
    NSLog(@"frame = %f, %f, %f, %f", frame.origin.x, frame.origin.y, frame.size.width, frame.size.height);
    int tag = self.tag;
    NSLog(@"tag = %d", tag);
    background = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, frame.size.width, frame.size.height)];
    [self addSubview:background];
    [background release];
    label = [[FontLabel alloc] initWithFrame:CGRectMake(10, 0, frame.size.width, frame.size.height) fontName:[SEUtil getFontName] pointSize:30];
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
        NSLog(@"SEOptionsLabel initWithCoder");
        CGRect frame = self.frame;
        [self createChild:frame];
        //label.textAlignment = UITextAlignmentCenter;
    }
    return self;
}
- (void) setAppearance
{
    
}
@end
////////
@implementation SETextImageButton
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
    self.clipsToBounds = YES;
}
- (void) setScaleTextImage: (BOOL) b
{
    mUseScaleTextImage = b;
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
        float width = image.size.width;
        float height = image.size.height;
        float toppadding = 10;
        float bottompadding = 10;
        if(mUseScaleTextImage)
        {
            float ratio = image.size.width / image.size.height;
            height = self.frame.size.height - toppadding - bottompadding;
            float w = height * ratio;
            width = w;//image.size.width;
            starty = toppadding;
        }
        switch (alignment) {
            case UITextAlignmentLeft:
            {
                startx = leftpadding;
            }
                break;
            case UITextAlignmentRight:
            {
                startx = self.frame.size.width - width;
            }
                break;
            case UITextAlignmentCenter:
            {
                startx = (self.frame.size.width - width) / 2;
            }
                break;
            default:
                break;
        }

        
        textImageView.frame = CGRectMake(startx, starty, width, height);
        textImageView.image = image;
        return;
    }
    float indicatorWidth = 32;//self.frame.size.height;
    float indicatorHeight = 32;//self.frame.size.height;
    float paddingx = 10;
    indicateView.frame = CGRectMake(paddingx, (self.frame.size.height - indicatorHeight) / 2, indicatorWidth, indicatorHeight);
    UIImage* image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:indicateImageStr];
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


