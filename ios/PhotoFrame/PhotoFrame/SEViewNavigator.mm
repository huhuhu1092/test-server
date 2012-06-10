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
//#import "SEUITableView.h"
#import "SEMainDisplay.h"
#import "PainterManager.h"
#import "SEImageMusicListView.h"
#import "SEOptionsView.h"
#import "SEMusicPickerView.h"
#import "SESignatureView.h"
#import "Signature.h"
#import "MusicList.h"
#import "ImageList.h"
#import "SelectedImage.h"
#import "SelectedMusic.h"
#import "SESignaturePreview.h"
#import "SEPageScrollView.h"
#import "SEUserUpgrate.h"
#import "SE3DPreview.h"
#import "SEOperationView.h"
#import "SEPopupView.h"
#import "FinishedImage.h"
#import "SEDataUploadManager.h"
#import <QuartzCore/CALayer.h>
#import <MediaPlayer/MediaPlayer.h>
/// for weibo
#import "SEImageShareManager.h"
//#import "OAuthEngine.h"
/////////
#define DBG_VIEW_BG(view, color) view.backgroundColor = [UIColor color];
///////////
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

static enum VIEW_RELATION_TYPE gViewRelationType = TYPE1;
struct ViewRelation gViewRelation[] = {
    {MAIN_DISPLAY, INVALID_VIEW, INVALID_VIEW},
    {IMAGE_PICKER, MAIN_DISPLAY, SELECTED_IMAGE_VIEW},
    {SELECTED_IMAGE_VIEW, IMAGE_PICKER, MAIN_DISPLAY_MIRROR},
    {MUSIC_PICKER, MAIN_DISPLAY, SELECTED_MUSIC_VIEW},
    {SELECTED_MUSIC_VIEW, MUSIC_PICKER, MAIN_DISPLAY_MIRROR},
    {OPTIONS, MAIN_DISPLAY, INVALID_VIEW},
    {MUSIC_IMAGE_LIST_ATTACH, MAIN_DISPLAY, INVALID_VIEW},
    {SIGNATURE_PREVIEW, OPTIONS, INVALID_VIEW},
    {SIGNATURE_VIEW, SIGNATURE_PREVIEW, INVALID_VIEW},
    
    {PREVIEW_3D, MAIN_DISPLAY, INVALID_VIEW}
    
}; 
struct ViewRelation gViewRelation2[] = {
    {MUSIC_IMAGE_LIST_ATTACH, MAIN_DISPLAY, INVALID_VIEW},
    {MUSIC_PICKER, MUSIC_IMAGE_LIST_ATTACH, SELECTED_MUSIC_VIEW},
    {IMAGE_PICKER, MUSIC_IMAGE_LIST_ATTACH, SELECTED_IMAGE_VIEW},
    {SELECTED_IMAGE_VIEW, IMAGE_PICKER, INVALID_VIEW},
    {SELECTED_MUSIC_VIEW, MUSIC_PICKER, INVALID_VIEW}
};
static VIEW_TYPE getPrevView(VIEW_TYPE curr)
{
    if(gViewRelationType == TYPE1)
    {
        int count = sizeof(gViewRelation) / sizeof(ViewRelation);
        for(int i = 0 ; i < count ; i++)
        {
            if(gViewRelation[i].curr == curr)
                return gViewRelation[i].prev;
        }
        return INVALID_VIEW;
    }
    else if(gViewRelationType == TYPE2)
    {
        int count = sizeof(gViewRelation2) / sizeof(ViewRelation);
        for(int i = 0 ; i < count ; i++)
        {
            if(gViewRelation2[i].curr == curr)
                return gViewRelation2[i].prev;
        }
        return INVALID_VIEW;
    }
    else 
        return INVALID_VIEW;
}
static VIEW_TYPE getNextView(VIEW_TYPE curr)
{
    if(gViewRelationType == TYPE1)
    {
        int count = sizeof(gViewRelation) / sizeof(ViewRelation);
        for(int i = 0 ; i < count ; i++)
        {
            if(gViewRelation[i].curr == curr)
                return gViewRelation[i].next;
        }
        return INVALID_VIEW; 
    }
    else if(gViewRelationType == TYPE2)
    {
        int count = sizeof(gViewRelation2) / sizeof(ViewRelation);
        for(int i = 0 ; i < count ; i++)
        {
            if(gViewRelation2[i].curr == curr)
                return gViewRelation2[i].next;
        }
        return INVALID_VIEW; 
    }
    else
        return INVALID_VIEW;
    
}
///

static BarViewType gBarViewType[] = {
    {IMAGE_PICKER, SELECTED_IMAGE_VIEW, YES, YES, YES},
    {MAIN_DISPLAY, IMAGE_PICKER, YES, NO, NO},
    {MAIN_DISPLAY, OPTIONS, YES, NO, NO},
    {MAIN_DISPLAY, PREVIEW_3D, NO, NO, NO},
    {MAIN_DISPLAY, MUSIC_PICKER, YES, NO, NO},
    {MAIN_DISPLAY, MUSIC_IMAGE_LIST_ATTACH, YES, NO, NO},
    {MUSIC_PICKER, SELECTED_MUSIC_VIEW, YES, YES, YES},
    {OPTIONS, SIGNATURE_PREVIEW, YES, NO, NO},
    {SIGNATURE_PREVIEW, SIGNATURE_VIEW, YES, NO, NO},
    {SELECTED_IMAGE_VIEW, MAIN_DISPLAY_MIRROR, NO, YES, NO},
    {SELECTED_MUSIC_VIEW, MAIN_DISPLAY_MIRROR, NO, YES, NO},
    //
    {MUSIC_IMAGE_LIST_ATTACH, IMAGE_PICKER, YES, NO, NO},
    {MUSIC_IMAGE_LIST_ATTACH, MUSIC_PICKER, YES, NO, NO}
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
VIEW_TYPE gViewSequenceImagePicker[] = {MAIN_DISPLAY, IMAGE_PICKER, SELECTED_IMAGE_VIEW, MAIN_DISPLAY_MIRROR};
VIEW_TYPE gViewSequenceMusicPicker[] = {MAIN_DISPLAY, MUSIC_PICKER, SELECTED_MUSIC_VIEW, MAIN_DISPLAY_MIRROR};
VIEW_TYPE gViewSequenceOption[] = {MAIN_DISPLAY, OPTIONS};
VIEW_TYPE gViewSequenceMusicImageListAttach[] = {MAIN_DISPLAY, MUSIC_IMAGE_LIST_ATTACH};
VIEW_TYPE gViewSequenceOptionImagePicker[] = {OPTIONS, IMAGE_PICKER, SELECTED_IMAGE_VIEW};
VIEW_TYPE gViewSequenceOptionMusicPicker[] = {OPTIONS, MUSIC_PICKER};
VIEW_TYPE gViewSequenceOptionsSignaturePreview[] = {OPTIONS, SIGNATURE_PREVIEW};
VIEW_TYPE gViewSequenceSignatureViewPreview[] = {SIGNATURE_PREVIEW, SIGNATURE_VIEW};
VIEW_TYPE gViewSequenceMusicListAttachImagePicker[] = {MUSIC_IMAGE_LIST_ATTACH, IMAGE_PICKER, SELECTED_IMAGE_VIEW};
VIEW_TYPE gViewSequenceMusicListAttachMusicPicker[] = {MUSIC_IMAGE_LIST_ATTACH, MUSIC_PICKER, SELECTED_MUSIC_VIEW};
VIEW_TYPE gViewSequencePreview3DOnly[] = {MAIN_DISPLAY, PREVIEW_3D};
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
    {gViewSequenceSignatureViewPreview, SIGNATURE_SIGNATURE_PREVIEW, sizeof(gViewSequenceSignatureViewPreview) / sizeof(VIEW_TYPE)},
    {gViewSequenceMusicListAttachImagePicker, MUSIC_IMAGE_LIST_ATTACH_IMAGE_PICKER, sizeof(gViewSequenceMusicListAttachImagePicker) / sizeof(VIEW_TYPE)},
    {gViewSequenceMusicListAttachMusicPicker, MUSIC_IMAGE_LIST_ATTACH_MUSIC_PICKER, sizeof(gViewSequenceMusicListAttachMusicPicker) / sizeof(VIEW_TYPE)},
    {gViewSequencePreview3DOnly, PREVIEW_3D_ONLY, sizeof(gViewSequencePreview3DOnly) / sizeof(VIEW_TYPE)}
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
struct ViewMirrorTypeData
{
    VIEW_TYPE origType;
    VIEW_TYPE mirrorType;
};
ViewMirrorTypeData gViewMirrorTypeData[] ={
    {MAIN_DISPLAY, MAIN_DISPLAY_MIRROR}
};
static VIEW_TYPE getMirrorType(VIEW_TYPE vp)
{
    int count = sizeof(gViewMirrorTypeData) / sizeof(ViewMirrorTypeData);
    for(int i = 0 ;i < count ; i++)
    {
        if(gViewMirrorTypeData[i].origType == vp)
            return gViewMirrorTypeData[i].mirrorType;
        else if(gViewMirrorTypeData[i].mirrorType == vp)
            return gViewMirrorTypeData[i].origType;
    }
    return INVALID_VIEW;
}
///////////
struct ViewLayoutData
{
    VIEW_TYPE vp;
    BOOL fullScreen;
};
ViewLayoutData gViewLayoutData[] = {
    {PREVIEW_3D, YES},
    {MAIN_DISPLAY, YES},
    {MAIN_DISPLAY_MIRROR, YES}
};
static BOOL isViewLayoutFullScreen(VIEW_TYPE vp)
{
    int count = sizeof(gViewLayoutData) / sizeof(ViewLayoutData);
    for(int i = 0 ; i < count ; i++)
    {
        if(gViewLayoutData[i].vp == vp)
            return gViewLayoutData[i].fullScreen;
    }
    return NO;
}
///////////////
struct ViewBarBackground
{
    VIEW_TYPE vp;
    VIEW_TYPE vpNext;
    NSString* key;
};
ViewBarBackground gViewBarBackground[] = {
    {MAIN_DISPLAY, IMAGE_PICKER, @"MainDisplayImagePickerBarBackground"},
    {IMAGE_PICKER, SELECTED_IMAGE_VIEW,@"ImagePickerSelectedImageViewBarBackground"},
    {SELECTED_IMAGE_VIEW, MAIN_DISPLAY_MIRROR,@"SelectedImageViewMainDisplayBarBackground"}, 
    {MAIN_DISPLAY, MUSIC_PICKER, @"MainDisplayMusicPickerBarBackground"},
    {MUSIC_PICKER, SELECTED_MUSIC_VIEW, @"MusicPickerSelectedMusicViewBarBackground"},
    {SELECTED_MUSIC_VIEW, MAIN_DISPLAY_MIRROR, @"SelectedMusicViewMainDisplayBarBackground"},
    {MAIN_DISPLAY, OPTIONS, @"OptionBarBackground"},
    {MAIN_DISPLAY, MUSIC_IMAGE_LIST_ATTACH, @"MainDisplayMusicImageAttachBarBackground"},
    {MUSIC_IMAGE_LIST_ATTACH, IMAGE_PICKER, @"MusicImageAttachImagePickerBarBackground"},
    {MUSIC_IMAGE_LIST_ATTACH, MUSIC_PICKER, @"MusicImageAttachMusicPickerBarBackground"},
    /////
    {MAIN_DISPLAY, SIGNATURE_VIEW, @"MainDisplayBarBackground"},
    {MAIN_DISPLAY, SIGNATURE_PREVIEW, @"MainDisplayBarBackground"}
};
static NSString* getViewBarBackgroundKey(VIEW_TYPE vp, VIEW_TYPE vpNext)
{
    int count = sizeof(gViewBarBackground) / sizeof(ViewBarBackground);
    for(int i = 0 ; i < count ; i++)
    {
        if(vp == gViewBarBackground[i].vp && vpNext == gViewBarBackground[i].vpNext)
            return gViewBarBackground[i].key;
    }
    return nil;
}
////////
enum {LEFT_PART, RIGHT_PART};
/////
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
@synthesize mBackgroundImage;
- (void) stop
{
    /*
    UIView<SEAdjustContentView>* content = [self contentView];
    if(content == nil)
        return;
    if([content respondsToSelector:@selector(stopAsync:::)])
    {
        
    }
    else
    {
        [content removeFromSuperview];
    }
     */
}
- (void) removeContentViewLater: (id) obj
{
    UIView* v = (UIView*)obj;
    [v release];
}
- (void) removeContentView
{
    UIView<SEAdjustContentView>* content = [self contentView];
    if(content == nil)
        return;
    Class cl = [SEPageUIScrollView class];
    if([content isMemberOfClass:cl])
    {
        SEPageUIScrollView* scrollView = (SEPageUIScrollView*)content;
        [scrollView stopLoadImage];
    }
    [content removeFromSuperview];

}
- (void) drawRect:(CGRect)rect
{
    if(mBackgroundImage)
    {
        if(mBackgroundImage.size.width == rect.size.width && mBackgroundImage.size.height == rect.size.height)
            [mBackgroundImage drawInRect:rect];
        else
        {
            UIImage* image = [SEUtil drawImage:mBackgroundImage inRect:CGRectMake(0, 0, rect.size.width, rect.size.height)];
            [image drawInRect:rect];
        }
    }
}
- (BOOL) hasContent
{
    return [self.subviews count] > 0;
}
- (void) updateContent
{
    UIView<SEAdjustContentView>* content = [self contentView];
    if(content)
        [content update];
}
- (void) adjustContentViewLayout: (float) contentViewWidth part: (int)partType barStopInMid:(BOOL)bStopInMid
{
    UIView<SEAdjustContentView>* contentView = [self.subviews objectAtIndex:0];
    if([contentView isMemberOfClass:[SEPageUIScrollView class]])
    {
        //SEPageUIScrollView* pageScrollView = (SEPageUIScrollView*)contentView;
        if(bStopInMid)
        {
            [self decreaseContent:contentViewWidth part:partType];
        }
    }
    else if([contentView respondsToSelector:@selector(handleWhenStop:)])
    {
        [contentView handleWhenStop:bStopInMid];
    }
    else
    {
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
            /*
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
             */
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

- (BOOL) canStopInMid
{
    return YES;
}
- (void) initContainer
{}
- (void)dealloc
{
    [mBackgroundImage release];
    [mBarBackgroundKey release];
    [super dealloc];
}
- (void) expandContent:(float) contentWidth part: (int)partType
{
    UIView<SEAdjustContentView>* v = [self contentView];
    if([v isMemberOfClass:[SEPageUIScrollView class]])
    {
        SEPageUIScrollView* pageScrollView = (SEPageUIScrollView*)v;
        if([pageScrollView.mName isEqualToString:@"image_picker"])
        {
            [pageScrollView changeContentToMultipleColumn:contentWidth movePart:LEFT_PAGE];
        }
        else if([pageScrollView.mName isEqualToString:@"image_selected_view"])
        {
            [pageScrollView changeContentToMultipleColumn:contentWidth movePart:RIGHT_PAGE];
        }
    }    
}
- (void) decreaseContent: (float) contentWidth part: (int)partType
{
    UIView<SEAdjustContentView>* v = [self contentView];
    if([v isMemberOfClass:[SEPageUIScrollView class]])
    {
        SEPageUIScrollView* pageScrollView = (SEPageUIScrollView*)v;
        if([pageScrollView.mName isEqualToString:@"image_picker"])
        {
            [pageScrollView changeContentToSingleColumn:contentWidth movePart:LEFT_PAGE];
        }
        else if([pageScrollView.mName isEqualToString:@"image_selected_view"])
        {
            [pageScrollView changeContentToSingleColumn:contentWidth movePart:RIGHT_PAGE];
        }
    }
}
- (BOOL) isContentAnimEnd
{
    UIView* v = [self contentView];
    if([v isMemberOfClass:[SEPageUIScrollView class]] )
    {
        SEPageUIScrollView* scrollView = (SEPageUIScrollView*)v;
        return scrollView.mIsAnimEnd;
    }
    else
        return YES;
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
@synthesize mStayOnMid;
@synthesize mIsBarAnimationEnd;

- (void)drawRect:(CGRect)rect
{
    if(mLeftContentContainer.mType == SELECTED_IMAGE_VIEW)
    {
        NSLog(@"## selected image view left ###");
    }
    if(mLeftContentContainer.mBackgroundImage)
    {
        CGSize s = CGSizeMake(self.frame.size.width, self.frame.size.height);
        UIGraphicsBeginImageContext(s);
        CGFloat imageWidth = mLeftContentContainer.mBackgroundImage.size.width;
        //CGFloat imageHeight = mLeftContentContainer.mBackgroundImage.size.height;
        [mLeftContentContainer.mBackgroundImage drawAtPoint:CGPointMake(s.width - imageWidth, 0)];
        UIImage* image = UIGraphicsGetImageFromCurrentImageContext();
        UIGraphicsEndImageContext();
        [image drawInRect:rect];
    }
}
- (void) initBackground
{
    CGRect bounds = self.bounds;
    NSString* bgKey = getViewBarBackgroundKey(mLeftContentContainer.mType, mRightContentContainer.mType);
    //NSString* leftBgKey = mLeftContentContainer.mBarBackgroundKey;
    //NSString* rightBgKey = mRightContentContainer.mBarBackgroundKey;
    UIImage* leftImage = [mResLoader getImage:bgKey];
    //UIImage* rightImage = [mResLoader getImage:rightBgKey];
    mLeftImageView = [[UIImageView alloc] initWithFrame:bounds];
    //mRightImageView = [[UIImageView alloc] initWithFrame:bounds];
    [self addSubview:mLeftImageView];
    //[self addSubview:mRightImageView];
    [mLeftImageView release];
    //[mRightImageView release];
    mLeftImageView.image = leftImage;
    //mRightImageView.image = rightImage;
    //self.backgroundColor = [UIColor clearColor];
    
    //CGAffineTransform reverse = CGAffineTransformMakeScale(-1, -1);
    //mLeftImageView.transform = reverse;
}
- (BAR_LOC_TYPE) barLocationType
{
    if(mLeftContentContainer.mType == mViewNav.prevView && !mStayOnMid )
        return LEFT_BAR;
    else if(mRightContentContainer.mType == mViewNav.nextView && !mStayOnMid)
        return RIGHT_BAR;
    else if(mStayOnMid)
    {
        return MID_BAR;
    }
    else
    {
        assert(0);
        return INVALID_BAR;
    }
}
- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"bar view touches began, mIsBarAnimationEnd = %d, leftContainer = %d, rightContainer = %d, bar type = %d, %d", mIsBarAnimationEnd, [mLeftContentContainer isContentAnimEnd], [mRightContentContainer isContentAnimEnd], mBarViewType.leftView, mBarViewType.rightView);
    if(mIsBarAnimationEnd == NO || [mLeftContentContainer isContentAnimEnd] == NO || [mRightContentContainer isContentAnimEnd] == NO )
        return;
    mTouchStart = YES;
    mOrig = [[touches anyObject] locationInView:mViewNav.mRootView];
    BAR_LOC_TYPE barLocType = [self barLocationType];
    mFirstMove = YES;
    if(barLocType == LEFT_BAR)
    {
        mLeftContentContainer.hidden = NO;
        [mViewNav addContentToContentContainer:mLeftContentContainer.mType];
    }
    else if(barLocType == RIGHT_BAR)
    {
        mRightContentContainer.hidden = NO;
        [mViewNav addContentToContentContainer:mRightContentContainer.mType];
    }
    else if(barLocType == MID_BAR)
    {
        NSLog(@"touch at mid bar\n");
        
    }
    else
    {
        NSLog(@"bar type error\n");
        assert(0);
    }
}
- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event
{
    //NSLog(@"bar view touches move count = %u", [touches count]);
    if(mTouchStart == NO)
        return;
    CGPoint loc = [[touches anyObject] locationInView:mViewNav.mRootView];
    CGFloat deltax = loc.x - mOrig.x;
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
    if(currentx < 0)
    {
        deltax = -p.x;
    }
    if(currentend > mViewNav.mViewPortWidth)
    {
        deltax = mViewNav.mViewPortWidth - p.x - self.frame.size.width;
    }
    if(fabsf(deltax) < 5 && mFirstMove)
        return;
    mOrig = loc;
    CGPoint c = mViewNav.mContentContainerParent.center;
    c.x += deltax;
    mViewNav.mContentContainerParent.center = c;
    BAR_LOC_TYPE barLocType = [self barLocationType];
    if(barLocType == MID_BAR && mFirstMove)
    {
        NSLog(@"expand page \n");
        [mLeftContentContainer expandContent: (mViewNav.mViewPortWidth - mViewNav.mBarWidth) / 2 part:LEFT_PART];
        [mRightContentContainer expandContent:(mViewNav.mViewPortWidth - mViewNav.mBarWidth) / 2 part:RIGHT_PART];
    }
    mFirstMove = NO;
}
- (CGPoint) pointInScreen: (CGPoint) point
{
    return [self convertPoint:point toView:mViewNav.mRootView];
}
- (void) handleContentContainerWhenTouchEndAtLeft: (BOOL)stopInMid
{
    if(stopInMid)
    {
        self.mStayOnMid = YES;
        float contentWidth = (mViewNav.mViewPortWidth - mViewNav.mBarWidth) / 2;
        if([mLeftContentContainer.contentView canAdjust])
        {
            [mLeftContentContainer adjustContentViewLayout:contentWidth part:LEFT_PART barStopInMid:YES];
        }
        if([mRightContentContainer.contentView canAdjust])
        {
            [mRightContentContainer adjustContentViewLayout:contentWidth part:RIGHT_PART barStopInMid:YES];
        }
        
    }
    else
    {
        self.mStayOnMid = NO;
        if([mRightContentContainer.contentView canAdjust])
        {
            [mRightContentContainer adjustContentViewLayout:mRightContentContainer.frame.size.width part:RIGHT_PART barStopInMid:NO];
        }
        VIEW_TYPE realType = getMirrorType(mRightContentContainer.mType);
        NSLog(@"### realType = %d ####", realType);
        if(mBarViewType.canSeenAsLeftBar == NO)
        {
           // VIEW_TYPE currViewType = mRightContentContainer.mType;
            assert(realType != INVALID_VIEW);
            VIEW_SEQ_TYPE seqType = [mViewNav popViewSeqType];
            NSLog(@"### popuped seqType = %d ####", seqType);
            [mViewNav moveToView:seqType :realType hasAnimation:NO isPush:NO];
        }
        else
        {
            if(realType != INVALID_VIEW)
            {
                VIEW_SEQ_TYPE seqType = [mViewNav popViewSeqType];
                NSLog(@"### popuped seqType = %d ####", seqType);
                [mViewNav moveToView:seqType :realType hasAnimation:NO isPush:NO];
            }
            else
                [mViewNav setCurrentView:mRightContentContainer.mType isAnimation:NO];
            mLeftContentContainer.hidden = YES;
        }
        
    }
    
}
- (void) handleContentContainerWhenTouchEndAtRight: (BOOL)stopInMid
{
    if(stopInMid)
    {
        self.mStayOnMid = YES;
        float contentWidth = (mViewNav.mViewPortWidth - mViewNav.mBarWidth) / 2;
        if([mLeftContentContainer.contentView canAdjust])
        {
            [mLeftContentContainer adjustContentViewLayout:contentWidth part:LEFT_PART barStopInMid:YES];
        }
        if([mRightContentContainer.contentView canAdjust])
        {
            [mRightContentContainer adjustContentViewLayout:contentWidth part:RIGHT_PART barStopInMid:YES];
        }
        
    }
    else
    {
        self.mStayOnMid = NO;
        if([mLeftContentContainer.contentView canAdjust])
        {
            [mLeftContentContainer adjustContentViewLayout:mLeftContentContainer.frame.size.width part:LEFT_PART barStopInMid:NO];
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
        mRightContentContainer.hidden = YES;
    }

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
                else if(currBarStartPoint.x == MID_CONTENTVIEW_WIDTH)
                {
                    dist = mViewNav.mBarWidth / 2;
                    stopInMid = YES;
                }
            }
            else
            {
                if(mBarViewType.canSeenAsLeftBar)
                    dist = currBarStartPoint.x;
                else
                    dist = currBarStartPoint.x + mViewNav.mBarWidth;
            }
            NSLog(@"move left dist = %f", dist);
            if(dist != 0)
            {
                CGPoint p = mViewNav.mContentContainerParent.center;
                p.x -= dist;
                
                void (^animBlock) (void) = ^{
                    mViewNav.mContentContainerParent.center = p;
                };
                void (^animEnd) (BOOL) = ^(BOOL f)
                {
                    [self handleContentContainerWhenTouchEndAtLeft:stopInMid];
                    [mContainerAnimHandler handleAnimEnd:stopInMid withDirection:mDirect leftContainer:mLeftContentContainer rightContainer:mRightContentContainer];
                    mIsBarAnimationEnd = YES;
                    self.userInteractionEnabled = YES;
                };
                self.userInteractionEnabled = NO;
                mIsBarAnimationEnd = NO;
                [UIView animateWithDuration:0.2 delay: 0 options: UIViewAnimationOptionCurveLinear animations:animBlock completion:animEnd];
                
            }
            else
            {
                NSLog(@"warning : ################## dist = 0   ##################################");
                [self handleContentContainerWhenTouchEndAtLeft:stopInMid];
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
                else if(currBarStartPoint.x == MID_CONTENTVIEW_WIDTH)
                {
                    dist = mViewNav.mBarWidth / 2;
                    stopInMid = YES;
                }
            }
            else
            {
                if(mBarViewType.canSeenAsRightBar)
                    dist = mViewNav.mViewPortWidth - (currBarStartPoint.x + mViewNav.mBarWidth);
                else
                    dist = mViewNav.mViewPortWidth - currBarStartPoint.x;
            }
            NSLog(@"move right dist = %f", dist);
            if(dist != 0)
            {
                
                CGPoint p = mViewNav.mContentContainerParent.center;
                p.x += dist;
                void (^animBlock) (void) = ^{
                    mViewNav.mContentContainerParent.center = p;
                };
                void (^animEnd) (BOOL) = ^(BOOL f){
                    [self handleContentContainerWhenTouchEndAtRight:stopInMid];
                    mIsBarAnimationEnd = YES;
                    self.userInteractionEnabled = YES;
                };
                self.userInteractionEnabled = NO;
                mIsBarAnimationEnd = NO;
                [UIView animateWithDuration:0.2 delay: 0
                                    options: UIViewAnimationOptionCurveLinear animations:animBlock
                                 completion:animEnd];
            }
            else
            {
                NSLog(@"warning: ################## dist = 0  #######################");
                [self handleContentContainerWhenTouchEndAtRight:stopInMid];
            }
        }
            break;
        case NO_MOVE:
        {
            NSLog(@"################ NO_MOVE ##########");
        }
            
            break;
        default:
            break;
    }
    
}

- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"bar view touches end count = %u", [touches count]);
    if(mTouchStart == NO)
        return;
    mTouchStart = NO;
    
    [self barUpHandler];
}
- (void)touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"bar view touch cancel");
    if(mTouchStart == NO)
        return;
    mTouchStart = NO;
    [self barUpHandler];
    
}
@end
////////////////////
@interface SEViewNavigator (Private)
//- (void) resetSharedImageArray;
//- (void) addSharedImage:(UIImage*)image;
//- (void) loadShareInterface;
- (void)popupOperationViewGroup;
- (void) disappearOperationViewGroup;
- (void)createFloatView: (SEPageHitProperty)hp scrollView:(SEPageUIScrollView*)currView;
- (void) setCurrentContentContainerFrame;
- (CGRect) calculateContentContainerFrame: (VIEW_TYPE)vp;
- (UIView*) loadViewFromNib: (NSString*)name;
- (void) setupRootView;
- (UIView<SEAdjustContentView>*) createImagePickerView: (CGRect)rect;
- (UIView<SEAdjustContentView>*) createImageSelectedView : (CGRect) rect;
- (UIView<SEAdjustContentView>*) createMusicPickerView: (CGRect) rect;
- (UIView<SEAdjustContentView>*) createOptionsView: (CGRect) rect;
- (UIView<SEAdjustContentView>*) createSignatureView:(CGRect) rect;
- (UIView<SEAdjustContentView>*) createSignaturePreview: (CGRect) rect;
- (UIView<SEAdjustContentView>*) createMusicImageListAttachView: (CGRect)rect;
- (UIView<SEAdjustContentView>*) createSelectedMusicView: (CGRect) rect;
- (UIView<SEAdjustContentView>*) createPreview3D: (CGRect)rect;
- (UIImage*) getDefaultImage;
- (void) initContentContainer;
- (void) initBarView;
- (void) initContentContainerParent;
- (SEBarView*) getBarView: (VIEW_TYPE)leftView :(VIEW_TYPE)rightView;
- (void) setCurrentViewSequenceFrame;
- (void) handleToolBarButtonClick: (TOOLBAR_BUTTON_TYPE*) typePtr;
- (void) playImage;
- (void) playMusic;
- (MPMediaItem*) getMediaItem: (NSArray*)mediaArray title: (NSString*)title artist: (NSString*) artist album: (NSString*)album;
@property (nonatomic, retain) NSString* mDrawingImageList;
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
        self.multipleTouchEnabled = NO;
    }
    return self;
}
@end
///////////
@interface SEImageViewLoader : NSObject<SELoadedImageHandler>
{
    UIImage* currImage;
    UIImageView* imageView;
    SEViewNavigator* viewNav;
}
@property (nonatomic, retain) UIImageView* imageView;
@property (nonatomic, assign) SEViewNavigator* viewNav;
@end
@implementation SEImageViewLoader
@synthesize imageView;
@synthesize viewNav;
- (void) setImage:(UIImage *)image
{
    [currImage release];
    currImage = [image retain];
}
- (void) preHandleImage
{}
- (void) dealloc
{
    [currImage release];
    [imageView release];
    [super dealloc];
}
- (void) handleImage
{
    imageView.image = currImage;
    [viewNav addShareImage:currImage];
    //[viewNav addSharedImage:currImage];
}

@end
////////
@interface SEOperationHandlerConcrete : NSObject <SEOperationHandler> 
{
@private
    SEViewNavigator* mViewNav;
}
@property (nonatomic, assign) SEViewNavigator* mViewNav;
@end
@implementation SEOperationHandlerConcrete
@synthesize mViewNav;
- (void)handleNotify:(NOTIFICATION_TYPE)notify view:(SEOperationView *)notifyView
{}
- (void) handleDeleteOp
{
    if(mViewNav.mOperationContainer.mType == SELECTED_IMAGE_VIEW)
    {
        SEPageUIScrollView* scrollView = (SEPageUIScrollView*)[mViewNav.mOperationContainer contentView];
        NSMutableArray* selectedIndex = mViewNav.mSelectedImageViewIndex;
        for(NSNumber* n in selectedIndex)
        {
            SEPageUIImageView* v = [scrollView imageView:[n intValue]];
            v.highlighted = NO;
        }
        [scrollView removeFromPhotoURLAsset:mViewNav.mSelectedImageViewIndex];
    }
}
- (void)handleShareCancel: (id)sender
{
    [mViewNav dismissPopup];
}
- (void) handleShareOk: (id)sender
{
    //[mViewNav loadShareInterface];
    [mViewNav shareImage];
    //[mViewNav dismissPopup];
}
- (void)handleShareOp
{
    //[mViewNav resetSharedImageArray];
    SEPageUIScrollView* scrollView = nil;
    if(mViewNav.mOperationContainer.mType == SELECTED_IMAGE_VIEW)
    {
        scrollView = (SEPageUIScrollView*)[mViewNav.mOperationContainer contentView];
    }
    else if(mViewNav.mOperationContainer.mType == IMAGE_PICKER)
    {
        scrollView = (SEPageUIScrollView*)[mViewNav.mOperationContainer contentView];
    }
    UIView* v = [[[NSBundle mainBundle] loadNibNamed:@"UserShareView" owner:self options:nil] lastObject];
    int tags[] = {102, 103, 104, 105, 106};
    int imageNum = [mViewNav.mSelectedImageViewIndex count];
    imageNum = MIN(5, imageNum);

    UIImageView* imageView = (UIImageView*)[v viewWithTag:101];
    imageView.contentMode = UIViewContentModeCenter;
    SEImageViewLoader* imageViewLoader = [[SEImageViewLoader alloc] init];
    imageViewLoader.imageView = imageView;
    NSNumber* num = [mViewNav.mSelectedImageViewIndex objectAtIndex:0];
    
    //[scrollView getImageURL:[num intValue] withBlock:handler];
    SEPageImageURL* url = [scrollView getImageURL:[num intValue]];
    [scrollView loadImageFromPhotoLib:url size:CGSizeMake(imageView.frame.size.width - 20, imageView.frame.size.height - 20) withHandler:imageViewLoader];
    for(int i = 0 ; i < imageNum ; i++)
    {
        int tag = tags[i];
        int index = [[mViewNav.mSelectedImageViewIndex objectAtIndex:i] intValue];
        UIImageView* imageView = (UIImageView*)[v viewWithTag:tag];
        imageView.contentMode = UIViewContentModeCenter;
        //[scrollView getImageURL:index withBlock:handler];
        SEImageViewLoader* imageViewLoader = [[SEImageViewLoader alloc] init];
        imageViewLoader.imageView = imageView;
        imageViewLoader.viewNav = mViewNav;
        SEPageImageURL* url = [scrollView getImageURL:index];
        [scrollView loadImageFromPhotoLib:url size:CGSizeMake(imageView.frame.size.width - 20, imageView.frame.size.height - 20) withHandler:imageViewLoader];
    }
    UIButton* cancelButton = (UIButton*)[v viewWithTag:107];
    UIButton* okButton = (UIButton*)[v viewWithTag:108];
    [cancelButton addTarget:self action:@selector(handleShareCancel:) forControlEvents:UIControlEventTouchUpInside];
    [okButton addTarget:self action:@selector(handleShareOk:) forControlEvents:UIControlEventTouchUpInside];
    [mViewNav popupView: v];
}
- (void)handleOperation:(OPERATION_TYPE)op view:(SEOperationView *)opView
{
    switch (op) 
    {
        case SHARE_OP:
        {
            NSLog(@"share operation\n");
            if([mViewNav operationEnable:SHARE_OP])
            {
                [self handleShareOp];
            }
        }
            break;
        case DELETE_OP:
        {
            NSLog(@"delete operation\n");
            if([mViewNav operationEnable:DELETE_OP])
                [self handleDeleteOp];
        }
            break;
        default:
            break;
    }
}

@end
//////////////////////////
@implementation SEViewNavigator (Private)

- (NSString*) mDrawingImageList
{
    return mDrawingImageList;
}
- (void) setMDrawingImageList:(NSString *)d
{
    [mDrawingImageList release];
    mDrawingImageList = [d retain];
}
- (MPMediaItem*) getMediaItem: (NSArray*)mediaArray title:(NSString*)title artist: (NSString*) artist album: (NSString*)album
{
    for(MPMediaItemCollection* collection in mediaArray)
    {
        for(MPMediaItem* item in [collection items])
        {
            NSString* tmpTitle  = [item valueForKey:MPMediaItemPropertyTitle];
            NSString* tmpAlbum = [item valueForKey:MPMediaItemPropertyAlbumArtist];
            NSString* tmpArtist = [item valueForKey:MPMediaItemPropertyArtist];
            if(tmpArtist == nil)
                tmpArtist = @"unknown";
            if(tmpAlbum == nil)
                tmpAlbum = @"unknown";
            if([tmpTitle isEqualToString:title] && [tmpAlbum isEqualToString:album] && [tmpArtist isEqualToString:artist])
                return item;
        }
    }
    return nil;
}
- (void) playMusic
{
    UserInfo* userInfo = [self getUserInfo];
    NSString* imageListName = userInfo.currentimagelist;
    MusicList* musicList  = [self getMusicListByImageList:imageListName];
    NSLog(@"#### imageListName = %@ ######\n", imageListName);
    if(musicList)
    {
        NSSet* selectedMusic = musicList.selectedmusic;
        MPMediaQuery* query = [MPMediaQuery songsQuery];
        NSArray* result = [query collections];
        NSMutableArray* itemArray = [NSMutableArray array];
        for(SelectedMusic* sm in selectedMusic)
        {
            NSLog(@"sm title = %@, artist = %@, album = %@", sm.title, sm.singer, sm.album);
            NSString* title = sm.title;
            if(title == nil)
                title = @"unknown";
            NSString* artist = sm.singer;
            if(artist == nil)
                artist = @"unknown";
            NSString* album = sm.album;
            if(album == nil)
                album = @"unknown";
            MPMediaItem* item = [self getMediaItem:result title:title artist:artist album:album];
            if(item)
            {
                [itemArray addObject:item];
            }
        }
        NSLog(@"#### music array count = %d ########\n", itemArray.count);
        MPMediaItemCollection* queue = [MPMediaItemCollection collectionWithItems:itemArray];
        if(queue)
        {
            MPMusicPlayerController* player = [MPMusicPlayerController applicationMusicPlayer];
            [player setQueueWithItemCollection:queue];
            player.shuffleMode = MPMusicShuffleModeSongs;
            [player play];
        }
    }
}
- (void)playImage
{
    if(mPainterManager.isPause)
    {
        if(mPainterManager.imageArray == nil)
        {
            [self displayNextImage];
        }
        else
        {
            [mPainterManager startDrawing];
        }
    }
    else
    {
        if(mPainterManager.imageArray != nil)
        {
            [mPainterManager pauseDrawing];
        }
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
        [self playMusic];
    }
    break;
    case MUSIC_SELECT:
    {
        NSLog(@"music select\n");
        [self setViewRelationType:TYPE1];
        [self moveToView:MAIN_DISPLAY_MUSIC_PICKER :MUSIC_PICKER hasAnimation:YES isPush:YES];

    }
    break;
    case IMAGE_SELECT:
    {
        NSLog(@"image select\n");
        [self setViewRelationType:TYPE1];
        [self moveToView:MAIN_DISPLAY_IMAGE_PICKER :IMAGE_PICKER hasAnimation:YES isPush:YES];
    }
    break;
    case OPTION:
    {
        NSLog(@"option select\n");
        [self setViewRelationType:TYPE1];
        [self moveToView:MAIN_DISPLAY_OPTIONS :OPTIONS hasAnimation:YES isPush:YES];
    }
        break;
    case PREVIEW:
    {
        NSLog(@"preview select\n");
        [self setViewRelationType:TYPE1];
        [self moveToView:PREVIEW_3D_ONLY :PREVIEW_3D hasAnimation:YES isPush:YES];
    }        
        break;
    case MUSIC_IMAGE_LIST:
        {
            NSLog(@"music image list select\n");
            [self setViewRelationType:TYPE1];
            [self moveToView:MAIN_DISPLAY_MUSIC_IMAGE_LIST_ATTACH :MUSIC_IMAGE_LIST_ATTACH hasAnimation:YES isPush:YES];
        }     
        break;
     default:
     break;
     }
}
- (void) setCurrentViewSequenceFrame
{
    ViewSeqProperty vsp = getViewSeqProperty(mCurrentViewSeqType);
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
- (CGRect) calculateContentContainerFrame: (VIEW_TYPE)vp
{
    VIEW_TYPE prev = getPrevView(vp);
    VIEW_TYPE next = getNextView(vp);
    float contentWidth = mViewPortWidth;
    if(isViewLayoutFullScreen(vp) == NO)
    {
        if(prev != INVALID_VIEW)
            contentWidth -= mBarWidth;
        if(next != INVALID_VIEW)
            contentWidth -= mBarWidth;
    }
    CGRect frame = CGRectMake(0, 0, contentWidth, mViewPortHeight);
    return frame;
}
- (void) initContentContainer
{
    for(int i = 0 ; i < VIEW_NUM ; i++)
    {
        if(i != INVALID_VIEW)
        {
            VIEW_TYPE vp = (VIEW_TYPE)i;
            /*
            VIEW_TYPE prev = getPrevView(vp);
            VIEW_TYPE next = getNextView(vp);
            float contentWidth = mViewPortWidth;
            if(prev != INVALID_VIEW)
                contentWidth -= mBarWidth;
            if(next != INVALID_VIEW)
                contentWidth -= mBarWidth;
             */
            CGRect frame = [self calculateContentContainerFrame:vp];
            SEContentViewContainer* viewContainer = [[SEContentViewContainer alloc] initWithFrame:frame];
            DBG_VIEW_BG(viewContainer, redColor);
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
        barView.mIsBarAnimationEnd = YES;
        barView.multipleTouchEnabled = NO;
        barView.mResLoader = mResLoader;
        barView.mHintRect = frame;
        barView.mContainerAnimHandler = self;
        BarViewType bvt = gBarViewType[i];
        barView.mBarViewType = bvt;
        barView.mCanStopInMid = bvt.canStopInMid;
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
    
    NSLog(@"#### viewsequence property count = %d ####", currViewSeqProp.count);
    NSLog(@"##### seq type = %d #####", currViewSeqProp.type);
    if(!isValidViewSeqProp(currViewSeqProp))
        return;
    VIEW_TYPE firstViewType = currViewSeqProp.viewseq[0];
    SEBarView* firstBarView = nil;
    if(firstViewType != MAIN_DISPLAY)
    {
        VIEW_TYPE vp = MAIN_DISPLAY;
        VIEW_TYPE vpNext = firstViewType;
        firstBarView = [self getBarView:vp :vpNext];
    }
    int count = currViewSeqProp.count;
    int totalWidth = 0;
    if(firstBarView != nil)
        totalWidth += mBarWidth;
    for(int i = 0 ; i < count - 1; i++)
    {
        VIEW_TYPE vp = currViewSeqProp.viewseq[i];
        SEContentViewContainer* containerView = mViewArray[vp];
        totalWidth += containerView.mHintRect.size.width;
        totalWidth += mBarWidth;
    }
    SEContentViewContainer* cv = mViewArray[currViewSeqProp.viewseq[count - 1]];
    totalWidth += cv.mHintRect.size.width;
    mContentContainerParent.frame = CGRectMake(0, 0, totalWidth, mViewPortHeight);
    float startx = 0;
    if(firstBarView != nil)
    {
        startx = mBarWidth;
        firstBarView.frame = CGRectMake(0, 0, mBarWidth, mViewPortHeight);
        [mContentContainerParent addSubview:firstBarView];
    }
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
        //[barView setNeedsDisplay];
    }
    VIEW_TYPE vp = currViewSeqProp.viewseq[count - 1];
    //if(count == 1 || (count > 1 && vp != currViewSeqProp.viewseq[0]))
    {
        SEContentViewContainer* viewContainer = mViewArray[vp];
        viewContainer.frame = CGRectMake(startx, 0, viewContainer.frame.size.width, viewContainer.frame.size.height);
        [mContentContainerParent addSubview:viewContainer];
    }
}

- (UIImage*) getDefaultImage
{
    return [UIImage imageNamed:@"image_background_001.png"];
}
- (UIView<SEAdjustContentView>*) createSelectedMusicView: (CGRect) rect
{
    SESelectedMusicView* view = (SESelectedMusicView*)[[[NSBundle mainBundle] loadNibNamed:@"MusicSelectedView" owner:self options:nil] lastObject];
    view.frame = rect;
    view.mViewNav = self;
    [view initData];
    return view;
}
- (UIView<SEAdjustContentView>*) createMusicPickerView: (CGRect) rect
{
    SEMusicPickerView* view  = (SEMusicPickerView*)[[[NSBundle mainBundle] loadNibNamed:@"MusicPicker" owner:self options:nil] lastObject];
    view.mViewNav = self;
    view.frame = rect;
    [view initData];
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
    [view setCurrentBarView:MAIN_SETTING];
    [view autorelease];
    return view;
}
- (UIView<SEAdjustContentView>*) createMainDisplayView : (CGRect) rect
{
    SEMainDisplay* mainDisplay = [[SEMainDisplay alloc] initWithResLoader:mResLoader withFrame:rect];
    [mainDisplay loadView];
    [mainDisplay setToolBarButtonHandleTarget:self withAction:@selector(handleToolBarButtonClick:)];
    [mainDisplay autorelease];
    return mainDisplay;
}
- (UIImage*) createPickerViewBackground: (CGRect) rect
{
    UIImage* image = [mResLoader getImage:@"ImagePickerViewBackground"];
    CGFloat w = image.size.width;
    CGFloat h = image.size.height;
    UIImage* ret = nil;
    UIGraphicsBeginImageContext(rect.size);
    [image drawAtPoint:CGPointMake(-rect.origin.x, -rect.origin.y)];
    ret = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    return ret;
}
- (UIImage*) createSelectViewBackground : (CGRect) rect
{
    UIImage* image = [mResLoader getImage:@"ImageSelectedViewBackground"];;
    UIImage* ret = nil;
    UIGraphicsBeginImageContext(rect.size);
    [image drawAtPoint:CGPointMake(-rect.origin.x, -rect.origin.y)];
    ret = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    return ret;
}

- (void)createFloatView: (SEPageHitProperty)hp scrollView:(SEPageUIScrollView*)currView
{
    CGRect r = [hp.imageView convertRect:hp.rect toView:mRootView];
    SEPageUIImageView* imageView = hp.imageView;
    mFloatView = [[SEUIFloatView alloc] initWithFrame:r];
    mFloatView.backgroundColor = [UIColor greenColor];
    mFloatView.contentMode = UIViewContentModeCenter;
    mFloatView.clipsToBounds = YES;
    mFloatView.origC = mFloatView.center;
    [mRootView addSubview:mFloatView];
    [mFloatView release];
    mFloatView.image = imageView.image;
    NSArray* highlightedView = [currView getHighlightedImageView];
    NSLog(@"selected count = %d", highlightedView.count);
    NSLog(@"select index = %d", hp.index);
    mSelectedPhotoURL = [NSMutableArray array];
    [mSelectedPhotoURL retain];
    mSelectedImage = [NSMutableArray array];
    [mSelectedImage retain];
    mSelectedImageViewIndex = [NSMutableArray array];
    [mSelectedImageViewIndex retain];
    if([highlightedView containsObject:imageView] == NO)
    {
        SEPageImageURL* url = [currView getImageURL:hp.index];
        NSLog(@"selected url = %@", url);
        if(url.url)
        {
            SEPageURLID* urlID = [SEPageURLID create:url.url : url.urlDate : url.origWidth : url.origHeight : url.orientation];
            [mSelectedPhotoURL addObject:urlID];
        }
        [mSelectedImageViewIndex addObject:[NSNumber numberWithInt:hp.index]];
        [mSelectedImage addObject:imageView.image];
    }
    else
    {
        for (SEPageUIImageView* v in highlightedView) 
        {
            int index = [currView getIndexForImageView:v];
            if(index != -1)
            {
                SEPageImageURL* url = [currView getImageURL:index];
                NSLog(@"selected url = %@", url);
                if(url.url)
                {
                    SEPageURLID* urlID = [SEPageURLID create:url.url :url.urlDate : url.origWidth :url.origHeight : url.orientation];
                    [mSelectedPhotoURL addObject:urlID];
                }
            }
            [mSelectedImageViewIndex addObject:[NSNumber numberWithInt:index]];
            [mSelectedImage addObject:v.image];
        }
    }
    [currView disableAllGestures];
    mPlacedViewIndex = SE_INVALID_IMAGE_INDEX;
}
/*
- (void) longPressGestureHandler: (UILongPressGestureRecognizer*)ges
{
    NSLog(@"long press\n");
    UIView* v = ges.view;
    if(v == mViewArray[IMAGE_PICKER])
    {
        CGPoint p = [ges locationInView:v];
        NSLog(@"p = %f, %f", p.x, p.y);
        SEPageUIScrollView* imagePickerView = (SEPageUIScrollView*)v;
        SEPageHitProperty hp = [imagePickerView hitRect:p];
        if(hp.imageView)
        {
            [self createFloatView:hp scrollView:imagePickerView];
        }
    }
}
 */
- (UIView<SEAdjustContentView>*) createImagePickerView : (CGRect) rect
{
    rect = CGRectMake(0, 67, rect.size.width, rect.size.height - 67);
    SEResLoader* resLoader = mResLoader;
    SEPageUIScrollView* scrollView = [[SEPageUIScrollView alloc] init];
    /*
    UILongPressGestureRecognizer* longGes = [[UILongPressGestureRecognizer alloc] initWithTarget:self action:@selector(longPressGestureHandler:)];
    [scrollView addGestureRecognizer:longGes];
    [longGes release];
     */
    scrollView.mScrollViewType = PHOTOLIB_SCROLLVIEW;
    scrollView.multipleTouchEnabled = NO;
    scrollView.mGetImageURLInMainThread = YES;
    scrollView.mResLoader = resLoader;
    scrollView.mName = @"image_picker";
    scrollView.mFrameImage = [mResLoader getImage:@"ImagePickerFrameImage"];
    scrollView.mHighlightedFrameImage = [mResLoader getImage:@"PageScrollViewImageViewHighlighted"];
    scrollView.mHandleMultiTouchDelegate = self;
    scrollView.pagingEnabled = YES;
    UIImage* image = [mResLoader getImage:@"ImagePickerPageViewBackground"];
    scrollView.mPageBackground = image;
    scrollView.mBackgroundImage = [self createPickerViewBackground:rect];
    scrollView.backgroundColor = [UIColor whiteColor];
    scrollView.frame = rect;
    scrollView.mPageCol = 4;
    scrollView.mPageRow = 6;
    scrollView.mLeftPadding = 7;
    scrollView.mTopPadding = 16;
    scrollView.mDefaultImage = [self getDefaultImage];
    scrollView.mPhotoWidth = [resLoader getInt: @"PhotoWidth"];
    scrollView.mPhotoHeight = [resLoader getInt:@"PhotoHeight"];
    scrollView.mVMargin = [resLoader getInt:@"VMargin"];
    scrollView.mHMargin = [resLoader getInt:@"HMargin"];
    scrollView.mViewNavigator = self;
    scrollView.mLongPressHandler = self;
    scrollView.mCanTouchResponse = YES;
    //SEPageUIPhotoLibLoaderDelegete* photoLibDelegate = [[SEPageUIPhotoLibLoaderDelegete alloc] init];
    //scrollView.mPhotoLoaderDelegate = photoLibDelegate;
    //photoLibDelegate.mScrollView = scrollView;
    [scrollView initState];
    [scrollView initPhotoLibUrl];
    [scrollView autorelease];
    return scrollView;
}
- (UIView<SEAdjustContentView>*) createImageSelectedView : (CGRect) rect
{
    rect = CGRectMake(0, 63, rect.size.width, rect.size.height - 63);
    SEResLoader* resLoader = mResLoader;
    SEPageUIScrollView* scrollView= [[SEPageUIScrollView alloc] init];
    scrollView.mScrollViewType = COREDATA_SCROLLVIEW;
    scrollView.pagingEnabled = YES;
    scrollView.multipleTouchEnabled = NO;
    scrollView.mResLoader = resLoader;
    scrollView.mName = @"image_selected_view";
    scrollView.mHandleMultiTouchDelegate = self;
    scrollView.mFrameImage = [mResLoader getImage:@"ImagePickerFrameImage"];
    scrollView.mHighlightedFrameImage = [mResLoader getImage:@"PageScrollViewImageViewHighlighted"];
    scrollView.mBackgroundImage = [self createSelectViewBackground:rect];
    scrollView.mPageBackground = [mResLoader getImage:@"SelectedImageViewPageViewBackground"];
    scrollView.frame = rect;
    //scrollView.backgroundColor = [UIColor redColor];
    scrollView.mPageCol = 4;
    scrollView.mPageRow = 6;
    
    scrollView.mLeftPadding = 7;
    scrollView.mTopPadding = 16;
    scrollView.mPhotoWidth = [resLoader getInt:@"PhotoWidth"];
    scrollView.mPhotoHeight = [resLoader getInt:@"PhotoHeight"];
    scrollView.mVMargin = [resLoader getInt:@"VMargin"];
    scrollView.mHMargin = [resLoader getInt:@"HMargin"];
    scrollView.mViewNavigator = self;
    scrollView.mDefaultImage = [self getDefaultImage];
    //SEPageUIPhotoFileLoaderDelegate* photoFileDelegate = [[SEPageUIPhotoFileLoaderDelegate alloc] init];
    //photoFileDelegate.mNavView = self;
    //photoFileDelegate.mScrollView = scrollView;
    //scrollView.mPhotoLoaderDelegate= photoFileDelegate;
    scrollView.mLongPressHandler = self;
    [scrollView initState];
    [scrollView initPhotoLibUrl];
    [scrollView createContent];
    [scrollView setNeedsDisplay];
    [scrollView autorelease];
    return scrollView;
}
- (UIView<SEAdjustContentView>*) createPreview3D: (CGRect)rect
{
    SE3DPreview* preview = [[SE3DPreview alloc] initWithFrame:rect];
    preview.mViewNav = self;
    [preview initData];
    [preview startDraw];
    [preview autorelease];
    return preview;
}
- (UIView*) loadViewFromNib: (NSString*)name
{
    NSBundle* mainBundle = [NSBundle mainBundle];
    UIView* view = [[mainBundle loadNibNamed:name owner:self options:NULL] lastObject];
    return view;
    
}


- (void) setupRootView
{
    if(mRootView == NULL)
    {
        mRootView = [[SEUIRootView alloc] initWithFrame:CGRectMake(0, 0, mViewPortWidth, mViewPortHeight)];
    }
    DBG_VIEW_BG(mRootView, greenColor);
    [self removeAllViewFromRoot];
    mContentContainerParent = [[UIView alloc] init];
    [mRootView addSubview:mContentContainerParent];
    [mContentContainerParent release];
    DBG_VIEW_BG(mContentContainerParent, blueColor);
    self.view = mRootView;
}
- (void) setCurrentContentContainerFrame
{
    ViewSeqProperty currViewSeqProp = getViewSeqProperty(mCurrentViewSeqType);
    for(int i = 0 ; i < currViewSeqProp.count ; i++)
    {
        VIEW_TYPE vp = currViewSeqProp.viewseq[i];
        mViewArray[vp].mHintRect = [self calculateContentContainerFrame:vp];
        mViewArray[vp].frame = [self calculateContentContainerFrame:vp];
    }
}
- (void)popupOperationViewGroup
{
    mOperationViewGroup = [[SEOperationViewGroup alloc] init];
    mOperationViewGroup.mViewNav = self;
    mOperationViewGroup.mOperationHandler = mOperationHandler;
    CGRect frame = [mOperationViewGroup calculateFrame];
    mOperationViewGroup.frame = CGRectMake(mViewPortWidth - frame.size.width, 0, frame.size.width, frame.size.height);
    [mOperationViewGroup initData];
    [mRootView addSubview:mOperationViewGroup];
    [mOperationViewGroup release];
    mOperationViewGroup.backgroundColor = [UIColor clearColor];//[UIColor redColor];
}
- (void) disappearOperationViewGroup
{
    [mOperationViewGroup operate];
    [mOperationViewGroup removeFromSuperview];
    mOperationViewGroup = nil;
}
@end
//////////////////////////////////
@implementation SEViewNavigator
@synthesize  mNewConfig;
@synthesize mResLoader;
@synthesize managedObjectContext;
@synthesize mUserInfoProperty;
@synthesize mViewPortWidth;
@synthesize mViewPortHeight;
@synthesize mCurrView;
@synthesize mBarWidth;
@synthesize mRootView;
@synthesize mContentContainerParent;
@synthesize mFontLoader;
@synthesize mOperationContainer;
@synthesize mSelectedImageViewIndex;
@synthesize mMusicFloatView;
@synthesize persistentStoreCoordinator;
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
        mCurrView = INVALID_VIEW;
        mCurrentViewSeqType = MAIN_DISPLAY_ONLY;
        mResLoader = resLoader;
        mFontLoader = [[SEFontLoader alloc] init];
        mBarWidth = [self getBarWidth];
        mUserUpgradeInfo = [[SEUserUpgrade alloc] init];
        mUserUpgradeInfo.mViewNav = self;
        mViewSeqTypeStack = [NSMutableArray array];
        [mViewSeqTypeStack retain];
        mOperationHandler = [[SEOperationHandlerConcrete alloc] init];
        mOperationHandler.mViewNav = self;
        SEWeiboImageShare* share = [[SEWeiboImageShare alloc] init];
        share.mViewNav = self;
        mShareImage = share;
        mDataUploadManager = [[SEDataUploadManager alloc] init];
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
    mPainterManager.mViewNav = self;
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
- (enum VIEW_TYPE) getPrevView
{
    return getPrevView(mCurrView);
}
- (enum VIEW_TYPE) getNextView
{
    return getNextView(mCurrView);
}

- (void) dealloc
{
    [mFontLoader release];
    for(int i = 0 ; i < VIEW_NUM ; i++)
    {
        [mViewArray[i] release];
    }
    for(int i = 0 ; i < [mBarViewArray count]; i++)
    {
        [[mBarViewArray objectAtIndex:i] release];
    }
    [mUserUpgradeInfo release];
    [mRootView release];
    [managedObjectContext release];
    [mUserInfoProperty release];
    [mOperationHandler release];
    [mShareImage release];
    [mDataUploadManager release];
    [persistentStoreCoordinator release];
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
    {
        [viewContainer updateContent];
        return;
    }
    else if(getMirrorType(vp) != INVALID_VIEW)
    {
        VIEW_TYPE mirrorType = getMirrorType(vp);
        SEContentViewContainer* mirrorViewContainer = [self getContainer:mirrorType];
        if([mirrorViewContainer hasContent])
        {
            UIView* contentView = [mirrorViewContainer contentView];
            [contentView retain];
            [contentView removeFromSuperview];
            [viewContainer addSubview:contentView];
            [contentView release];
            [viewContainer updateContent];
            return;
        }
    }
    CGRect frame = viewContainer.bounds;
    UIView* view = [self createContentView:vp withFrame:frame];
    int count = [view retainCount];
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
- (SEBarView*) getRightBarView: (VIEW_TYPE)currType
{
    for(int i = 0 ; i < mBarViewArray.count  ; i++)
    {
        SEBarView* v = [mBarViewArray objectAtIndex:i];
        if(v.mBarViewType.leftView == currType)
            return v;
    }
    return nil;
}
- (void)updateBarView
{
    for(UIView* v in mContentContainerParent.subviews)
    {
        if([v isMemberOfClass:[SEBarView class]])
        {
            [v setNeedsDisplay];
        }
    }
}
- (void) changeCurrentViewRelationType: (VIEW_TYPE) vp
{
    if(gViewRelationType == TYPE2 && vp == MUSIC_IMAGE_LIST_ATTACH)
        gViewRelationType = TYPE1;
}
- (CGFloat) getDeltax: (UIView*)currentView
{
    CGFloat total = 0;
    for(UIView* v in mContentContainerParent.subviews)
    {
        if(v != currentView)
            total += v.frame.size.width;
        else
            break;
    }
    return total;
}
- (void) afterSetCurrentView
{
    if(mCurrView == MAIN_DISPLAY)
    {
        for(int i = 0 ; i < VIEW_NUM; i++)
        {
            SEContentViewContainer* c = mViewArray[i];
            if(i != MAIN_DISPLAY)
            {
                [c removeContentView];
                //[c stop];
            }
        }
    }
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
    [self changeCurrentViewRelationType:view_type];
    mCurrentLeftBarView = nil;
    mCurrentRightBarView = nil;
    mCurrView = view_type;
    mCurrentLeftBarView = [self getBarView:self.prevView :mCurrView];
    mCurrentRightBarView = [self getBarView:mCurrView :self.nextView];
    SEContentViewContainer* currentContainer = [self getContainer:mCurrView];
    currentContainer.hidden = NO;
    currentContainer.backgroundColor = [UIColor redColor];
    ViewSeqProperty currViewSeqProp = getViewSeqProperty(mCurrentViewSeqType);
    int viewIndex = getViewIndexInSequence(currViewSeqProp, mCurrView);
    if(viewIndex != -1)
    {
        /*
        float deltax = 0;
        
        for(int i = 0 ; i < viewIndex ; i++)
        {
            VIEW_TYPE vp = currViewSeqProp.viewseq[i];
            SEContentViewContainer* view = mViewArray[vp];
            deltax += view.frame.size.width + mBarWidth;
        }
        if(deltax > 0)
            deltax -= mBarWidth;
        */
        float deltax = [self getDeltax:currentContainer];
        if(deltax > 0 && isViewLayoutFullScreen(mCurrView) == NO)
            deltax -= mBarWidth;
        CGRect frame = mContentContainerParent.frame;
        mContentContainerParent.frame = CGRectMake(-deltax, 0, frame.size.width, frame.size.height);
        
        [self addContentToContentContainer: mCurrView];
        if(bAnim)
        {
            SEContentViewContainer* prevContainer = [self getContainer:self.prevView];
            prevContainer.hidden = NO;
            SEContentViewContainer* nextContainer = [self getContainer:self.nextView];
            nextContainer.hidden = NO;
            frame = mContentContainerParent.frame;
            mContentContainerParent.frame = CGRectMake(0, 0, frame.size.width, frame.size.height);
            void (^animBlock) (void) = ^{
                mContentContainerParent.frame = frame;
            };
            void (^animEnd) (BOOL) = ^(BOOL) {
                SEContentViewContainer* prevContainer = [self getContainer:self.prevView];
                prevContainer.hidden = YES;
                SEContentViewContainer* nextContainer = [self getContainer:self.nextView];
                nextContainer.hidden = YES;
                [self afterSetCurrentView];
            };
            [UIView animateWithDuration:0.5 animations:animBlock completion:animEnd];
        }
        else
        {
            SEContentViewContainer* prevContainer = [self getContainer:self.prevView];
            prevContainer.hidden = YES;
            SEContentViewContainer* nextContainer = [self getContainer:self.nextView];
            nextContainer.hidden = YES;
            [self afterSetCurrentView];
        }
        [self updateBarView];
    }
    else
    {
        SEContentViewContainer* prevContainer = [self getContainer:self.prevView];
        prevContainer.hidden = YES;
        SEContentViewContainer* nextContainer = [self getContainer:self.nextView];
        nextContainer.hidden = YES;
        [self afterSetCurrentView];
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
        NSNumber* level = [NSNumber numberWithInt:1];
        [newUserInfo setValue:level forKey:@"level"];
        SEUserData* userData = [mUserUpgradeInfo getUserData:[level intValue]];
        int levelImageNum = userData.imageListNum;
        NSNumber* num = nil;
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
        for(int i = 0 ; i < levelImageNum ; i++)
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
    /*
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
     */
    //end
}
- (SelectedImage*) getSelectedImageByUrl : (NSString*) urlString andDate: (NSString*)date
{
    NSArray* selectedImageArray = [self getUserImageProperty];
    for(SelectedImage* i in selectedImageArray)
    {
        if([i.url isEqualToString:urlString] && [i.urldate isEqualToString:date])
            return i;
    }
    return nil;
}
- (SelectedImage*) getSelectedImageProperty: (int)index
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* imageList = userInfo.imagelist;
    NSString* currentImageList = userInfo.currentimagelist;
    for(ImageList* il in imageList)
    {
        if([il.name isEqualToString:currentImageList])
        {
            NSSet* selectedImageSet = il.selectedimage;
            for(SelectedImage* si in selectedImageSet)
            {
                if([si.seq isEqualToNumber:[NSNumber numberWithInt:index]])
                {
                    return si;
                }
            }
        }
    }
    return nil;
}
- (NSArray*) getUserImageProperty
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* imageListSet = userInfo.imagelist;
    NSString* currentImageList = userInfo.currentimagelist;
    for(ImageList* il in imageListSet)
    {
        if([il.name isEqualToString:currentImageList])
        {
            NSSet* selectedImageSet = il.selectedimage;
            NSArray* newArray = [selectedImageSet allObjects];
            newArray = [newArray sortedArrayUsingComparator:^NSComparisonResult(id obj1, id obj2) {
                SelectedImage* mo1 = (SelectedImage*)obj1;
                SelectedImage* mo2 = (SelectedImage*)obj2;
                NSNumber* left = mo1.seq;
                NSNumber* right = mo2.seq;
                return [left compare:right];
            }];
            return newArray;
        }
    }
    return nil;
}
- (void) saveContext
{
    for(int i = 0 ;i < VIEW_NUM ; i++)
    {
        SEContentViewContainer* c = mViewArray[i];
        [c saveContext: mUserInfoProperty];
    }
    [self saveCoreDataContext];
}
- (void) saveCoreDataContext
{
    NSError* error = nil;
    if(self.managedObjectContext != nil)
    {
        BOOL changed = [self.managedObjectContext hasChanges];
        if(changed && ![self.managedObjectContext save:&error])
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
        SEContentViewContainer* container = nil;
        UIImage* image = nil;
        switch (viewType) {
            case MAIN_DISPLAY:
                view = [self createMainDisplayView:r];
                container = mViewArray[MAIN_DISPLAY];
                container.mBackgroundImage = [mResLoader getImage:@"MainDisplayBg"];
                break;
            case IMAGE_PICKER:
                view = [self createImagePickerView:r];
                container = mViewArray[IMAGE_PICKER];
                image = [mResLoader getImage:@"ImagePickerViewBackground"];
                container.mBackgroundImage = image;
                break;
            case SELECTED_IMAGE_VIEW:
                view = [self createImageSelectedView:r];
                container = mViewArray[SELECTED_IMAGE_VIEW];
                image = [mResLoader getImage:@"ImageSelectedViewBackground"];
                container.mBackgroundImage = image;
                [container setNeedsDisplay];
                break;
            case MUSIC_PICKER:
                view = [self createMusicPickerView:r];
                container = mViewArray[MUSIC_PICKER];
                image = [mResLoader getImage:@"MusicPickerBackground"];
                container.mBackgroundImage = image;
                break;
            case SELECTED_MUSIC_VIEW:
                view = [self createSelectedMusicView:r];
                container = mViewArray[SELECTED_MUSIC_VIEW];
                image = [mResLoader getImage:@"SelectedMusicViewBackground"];
                container.mBackgroundImage = image;
                break;
            case MUSIC_IMAGE_LIST_ATTACH:
                view = [self createMusicImageListAttachView:r];
                container = mViewArray[MUSIC_IMAGE_LIST_ATTACH];
                container.mBackgroundImage = [mResLoader getImage:@"MusicImageAttachBackground"];
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
            case PREVIEW_3D:
                view = [self createPreview3D:r];
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
- (void) highlightPlacedView: (SEPageUIImageView*)imageView
{
    imageView.backgroundColor = [UIColor redColor];
    imageView.alpha = 0.5;
    [imageView setNeedsDisplay];
}
- (void) normalizePlacedView: (SEPageUIImageView*)imageView
{
    imageView.backgroundColor = [UIColor clearColor];
    imageView.alpha = 1.0;
    [imageView setNeedsDisplay];
}
- (void) intersectWithOperationView
{
    CGRect r = mFloatView.frame;
    r = [mRootView convertRect:r toView:mOperationViewGroup];
    //NSLog(@"float view rect = %f, %f, %f, %f",r.origin.x, r.origin.y, r.size.width,r.size.height);
    [mOperationViewGroup intersectOperationView:r];
}
- (void) moveFloatViewToPoint : (CGPoint)p
{
    if(mFloatView)
    {
        SEContentViewContainer* sc = mViewArray[SELECTED_IMAGE_VIEW];
        SEPageUIScrollView* selectedScrollView = (SEPageUIScrollView*)[sc contentView];
        mFloatView.center = p;
        CGPoint c = mFloatView.center;
        c = [mRootView convertPoint:c toView:selectedScrollView];
        
        SEPageHitProperty hp;
        hp.index = SE_INVALID_IMAGE_INDEX;
        hp.imageView = nil;
        hp.rect = CGRectMake(0, 0, 0, 0);
        if(selectedScrollView != nil)
            hp = [selectedScrollView hitRect:c];
        //NSLog(@"current selected index = %d\n",hp.index);
        if(mPlacedViewIndex == SE_INVALID_IMAGE_INDEX && hp.index != SE_INVALID_IMAGE_INDEX)
        {
            mPlacedViewIndex = hp.index;
            [self highlightPlacedView:hp.imageView];
        }
        else if (mPlacedViewIndex != SE_INVALID_IMAGE_INDEX && hp.index != SE_INVALID_IMAGE_INDEX)
        {
            SEPageUIImageView* currentPlacedView = [selectedScrollView imageView:mPlacedViewIndex];
            [self normalizePlacedView:currentPlacedView];
            [self highlightPlacedView:hp.imageView];
            mPlacedViewIndex = hp.index;
        }
        else if(mPlacedViewIndex != SE_INVALID_IMAGE_INDEX && hp.index == SE_INVALID_IMAGE_INDEX)
        {
            SEPageUIImageView* currentPlacedView = [selectedScrollView imageView:mPlacedViewIndex];
            [self normalizePlacedView:currentPlacedView];
            mPlacedViewIndex = SE_INVALID_IMAGE_INDEX;
        }
        else
        {
            mPlacedViewIndex = hp.index;
        }
        [self intersectWithOperationView];
    }
    else
    {
        NSLog(@"error: float view is nil");
    }
}
- (void)touchBegin:(NSArray *)points1 : (NSArray*)points2 withPointDelta: (CGPoint)deltap withScrollView:(SEPageUIScrollView *)scrollView
{
    SEContentViewContainer* imagePicker = mViewArray[IMAGE_PICKER];
    NSValue* v = [points1 lastObject];
    CGPoint p = [v CGPointValue];
    NSLog(@"p = %f, %f", p.x, p.y);
    if(imagePicker.contentView == scrollView && mFloatView == nil)
    {
        SEPageUIScrollView* currView = scrollView;
        SEPageHitProperty hp = [currView hitRect:p];
        if(hp.imageView != nil)
        {
            CGRect r = [hp.imageView convertRect:hp.rect toView:mRootView];
            SEPageUIImageView* imageView = hp.imageView;
            mFloatView = [[SEUIFloatView alloc] initWithFrame:r];
            mFloatView.backgroundColor = [UIColor greenColor];
            mFloatView.contentMode = UIViewContentModeCenter;
            mFloatView.clipsToBounds = YES;
            mFloatView.image = imageView.image;
            //mFloatView.p = p;
            mFloatView.origC = mFloatView.center;
            [mRootView addSubview:mFloatView];
            [mFloatView release];
            NSLog(@"select index = %d", hp.index);
            mSelectedPhotoURL = [NSMutableArray array];
            [mSelectedPhotoURL retain];
            [mSelectedPhotoURL addObject:[currView getImageURL:hp.index]];
            NSLog(@"selected url = %@", mSelectedPhotoURL);
            [currView disableAllGestures];
        }
        mPlacedViewIndex = SE_INVALID_IMAGE_INDEX;
    }
}
- (void) touchMove:(NSArray *)points1 : (NSArray*)points2 withPointDelta: (CGPoint)deltap withScrollView:(SEPageUIScrollView *)scrollView
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
- (void)saveImageURL: (SEPageImageURL*)imageURL withSeq:(int) seq
{
    SelectedImage* si = [self getSelectedImageProperty:seq];
    if(imageURL.url)
    {
        NSString* str = [imageURL.url absoluteString];
        si.url = str;
        si.urldate = imageURL.urlDate;
    }
    if(imageURL.filepath)
    {
        NSString* str = [imageURL.filepath absoluteString];
        si.filepath = str;
    }
}
- (void) addImageToSelectedImageScrollView: (NSMutableArray*) urlArray : (NSMutableArray*)imageArray
{
    assert(mPlacedViewIndex != SE_INVALID_IMAGE_INDEX);
    SEContentViewContainer* selectedContainer = mViewArray[SELECTED_IMAGE_VIEW];
    SEPageUIScrollView* selectedScrollView = (SEPageUIScrollView*)selectedContainer.contentView;
    [selectedScrollView insertURLToPhotoAsset:mPlacedViewIndex url:urlArray image: imageArray];
}
- (void) touchEnd:(NSArray *)points1 : (NSArray*)points2 withPointDelta: (CGPoint)deltap withScrollView:(SEPageUIScrollView *)scrollView
{
    SEContentViewContainer* imagePicker = mViewArray[IMAGE_PICKER];
    if(imagePicker.contentView == scrollView)
    {
        if(mPlacedViewIndex != SE_INVALID_IMAGE_INDEX)
        {
            SEContentViewContainer* selectedContainer = mViewArray[SELECTED_IMAGE_VIEW];
            SEPageUIScrollView* selectedScrollView = (SEPageUIScrollView*)selectedContainer.contentView;
            //[self addImageToSelectedImageScrollView:mSelectedPhotoURL];
            SEPageUIImageView* imageView = [selectedScrollView imageView:mPlacedViewIndex];
            [self normalizePlacedView:imageView];
            mPlacedViewIndex = SE_INVALID_IMAGE_INDEX;
            [mSelectedPhotoURL release];
        }
        mSelectedPhotoURL = nil;
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
- (void) setImageQuality:(int) q
{
    UserInfo* userInfo = [self getUserInfo];
    userInfo.currentimagequality = [NSNumber numberWithInt:q];
    //[self saveCoreDataContext];
}
- (void) setImageTimes: (int)t
{
    UserInfo* userInfo = [self getUserInfo];
    userInfo.currentimagetimes = [NSNumber numberWithInt:t];
    //[self saveCoreDataContext];
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
    NSString* currentMusicList = userInfo.currentmusiclist;
    NSSet* musicListSet = [userInfo valueForKey:@"musiclist"];
    NSManagedObject* musicList = nil;
    NSEnumerator* itMusicList = [musicListSet objectEnumerator];
    while((musicList = [itMusicList nextObject]) != nil)
    {
        NSString* name = [musicList valueForKey:@"name"];
        if([name isEqualToString:currentMusicList])
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
                currentSelectedMusic = [currentSelectedMusic arrayByAddingObject:item];
                [item release];
            }
        }
        
    }
    return currentSelectedMusic;
}
- (void) addSelectedMusicTitle: (NSString*)title aritist: (NSString*) artist album:(NSString*)album toMusicList: (NSString*) musicListName;
{
    MusicList* musicList = [self getMusicListByName:musicListName];
    SelectedMusic* selectedMusic = (SelectedMusic*)[self newObjectByEntityName:@"SelectedMusic"];
    selectedMusic.title = title;
    selectedMusic.singer = artist;
    selectedMusic.album = album;
    NSSet* selectMusicSet = musicList.selectedmusic;
    NSArray* selectMusicArray = [selectMusicSet allObjects];
    selectMusicArray = [selectMusicArray sortedArrayUsingComparator:^NSComparisonResult(id obj1, id obj2) {
        SelectedMusic* s1 = (SelectedMusic*)obj1;
        SelectedMusic* s2 = (SelectedMusic*)obj2;
        return [s1.seq compare:s2.seq];
    }];
    if(selectMusicArray.count == 0)
    {
        selectedMusic.seq = [NSNumber numberWithInt:0];
    }
    else
    {
        SelectedMusic* lastObj = [selectMusicArray lastObject];
        selectedMusic.seq = [NSNumber numberWithInt:[lastObj.seq intValue] + 1];
    }
    [musicList addSelectedmusicObject:selectedMusic];
}
- (void)displayNextImage
{
    UserInfo* userInfo = [self getUserInfo];
    NSString* currentImageList = userInfo.currentimagelist;
    NSArray* imagePropertyArray = [self getUserImageProperty];
    NSArray* imageURLArray = [NSArray array];
    NSArray* dateArray = [NSArray array];
    for(SelectedImage* si in imagePropertyArray)
    {
        if(si.url != nil)
        {
            imageURLArray = [imageURLArray arrayByAddingObject:si.url];
            dateArray = [dateArray arrayByAddingObject:si.urldate];
        }
    }
    if([mDrawingImageList isEqualToString:currentImageList])
    {
        if(mPainterManager.imageArray.count != imageURLArray.count)
        {
            if(mPainterManager.currentImageIndex >= imageURLArray.count)
                mPainterManager.currentImageIndex = 0;
                
        }
        else
        {
            //maybe the image sequence has change
        }
        
    }
    else
    {
        mPainterManager.currentImageIndex = 0;
    }
    self.mDrawingImageList = currentImageList;
    mPainterManager.imageArray = imageURLArray;
    mPainterManager.dateArray = dateArray;
    if([imageURLArray count] > 0)
    {
        int quality = [self getImageQuality];
        int times = [self getImageTimes];
        [mPainterManager initPainterState:quality withTimes:times];
        [mPainterManager nextDisplayStage];
    }
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
    [self setCurrentContentContainerFrame];
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
            [ml addAttachimagelistObject:imageList];
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
    [userInfo addMusiclistObject:ml];
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
    [userInfo addImagelistObject:il];
    //NSMutableSet* imageListSet = (NSMutableSet*)userInfo.imagelist;
    //[imageListSet addObject:il];
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
                [ml removeAttachimagelistObject:il];
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
    MusicList* musicList = [self getMusicListByName:musicListName];
    if(musicList)
    {
        NSNumber* seq = musicList.seq;
        [userInfo removeMusiclistObject:musicList];
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
- (MusicList*) getMusicListByImageList: (NSString*)imageListName
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* musicListSet = userInfo.musiclist;
    NSEnumerator* itMusicList = [musicListSet objectEnumerator];
    MusicList* ml = nil;
    while((ml = [itMusicList nextObject]) != nil)
    {
        NSSet* attachImage = ml.attachimagelist;
        NSEnumerator* itImageList = [attachImage objectEnumerator];
        ImageList* imageList = nil;
        while((imageList = [itImageList nextObject]) != nil)
        {
            if([imageList.name isEqualToString:imageListName])
                return ml;
        }
    }
    return nil;
}
- (int) getMaxSelectedImageCount
{
    UserInfo* userInfo = [self getUserInfo];
    NSNumber* level = userInfo.level;
    SEUserData* userData = [mUserUpgradeInfo getUserData:[level intValue]];
    if(userData)
    {
        return userData.imageListNum;    
    }
    else
        return 0;
}
- (int) getMaxSelectedMusicCount
{
    UserInfo* userInfo = [self getUserInfo];
    NSNumber* level = userInfo.level;
    SEUserData* userData = [mUserUpgradeInfo getUserData:[level intValue]];
    if(userData)
    {
        return userData.musicListNum;
    }
    else
        return 0;
}
- (int) getCurrentAllSelectedImageCount
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* imageList = userInfo.imagelist;
    int count = 0;
    for(ImageList* il in imageList)
    {
        NSSet* siset = il.selectedimage;
        for(SelectedImage* si in siset)
        {
            if(si.url != nil || si.filepath != nil)
                count++;
        }
    }
    return count;
}
- (int) getCurrentAllSelectedMusicCount
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* musicList = userInfo.musiclist;
    int count = 0;
    for(MusicList* ml in musicList)
    {
        NSSet* smset = ml.selectedmusic;
        for(SelectedMusic* sm in smset)
        {
            if(sm.title != nil)
                count++;
        }
    }
    return count;    
}
///////////////////////
- (void) clearPlacedView
{
    //mPlacedView = nil;
    mPlacedViewIndex = SE_INVALID_IMAGE_INDEX;
}
- (void) setViewRelationType:(VIEW_RELATION_TYPE) type
{
    gViewRelationType = type;
}
- (void) longPressBegin:(CGPoint)p scrollView:(SEPageUIScrollView *)scrollView
{
    if(scrollView == [mViewArray[IMAGE_PICKER] contentView])
    {
        SEPageHitProperty hp = [scrollView hitRect:p];
        if(hp.imageView && [hp.imageView isDefaultImage] == NO)
        {
            [self popupOperationViewGroup];
            [self createFloatView:hp scrollView:scrollView];
            
        }
    }
    else if(scrollView == [mViewArray[SELECTED_IMAGE_VIEW] contentView])
    {
        SEPageHitProperty hp = [scrollView hitRect:p];
        if(hp.imageView && [hp.imageView isDefaultImage] == NO)
        {
            [self popupOperationViewGroup];
            [self createFloatView:hp scrollView:scrollView];
        }
    }
}
- (void) longPressMove:(CGPoint)p scrollView:(SEPageUIScrollView *)scrollView
{
    if(scrollView == [mViewArray[IMAGE_PICKER] contentView])
    {
        if([self isFloatViewShow])
        {
            CGPoint currP = [scrollView convertPoint:p toView:mRootView];
            [self moveFloatViewToPoint:currP];
        }
    }
    else if(scrollView == [mViewArray[SELECTED_IMAGE_VIEW] contentView])
    {
        if([self isFloatViewShow])
        {
            CGPoint currP = [scrollView convertPoint:p toView:mRootView];
            [self moveFloatViewToPoint:currP];
        }
    }
}
- (void) longPressEnd:(CGPoint)p scrollView:(SEPageUIScrollView *)scrollView
{
    if(scrollView == [mViewArray[IMAGE_PICKER] contentView])
    {
        if(mPlacedViewIndex != SE_INVALID_IMAGE_INDEX)
        {
            SEContentViewContainer* selectedContainer = mViewArray[SELECTED_IMAGE_VIEW];
            SEPageUIScrollView* selectedScrollView = (SEPageUIScrollView*)selectedContainer.contentView;
            [self addImageToSelectedImageScrollView:mSelectedPhotoURL : mSelectedImage];
            SEPageUIImageView* imageView = [selectedScrollView imageView:mPlacedViewIndex];
            [self normalizePlacedView:imageView];
            mPlacedViewIndex = SE_INVALID_IMAGE_INDEX;
            for(NSNumber* n in mSelectedImageViewIndex)
            {
                int index = [n intValue];
                SEPageUIImageView* imageView = [scrollView imageView:index];
                imageView.highlighted = NO;
            }
        }
        mOperationEnable[DELETE_OP] = NO;
        mOperationEnable[SHARE_OP] = YES;
        mOperationContainer = mViewArray[IMAGE_PICKER];
    }
    else if(scrollView == [mViewArray[SELECTED_IMAGE_VIEW] contentView])
    {
        if(mPlacedViewIndex != SE_INVALID_IMAGE_INDEX)
        {
            SEPageUIImageView* imageView = [scrollView imageView:mPlacedViewIndex];
            [self normalizePlacedView:imageView];
            [scrollView changeImageView: mSelectedImageViewIndex toPos: mPlacedViewIndex];
            for(NSNumber* n in mSelectedImageViewIndex)
            {
                int index = [n intValue];
                SEPageUIImageView* imageView = [scrollView imageView:index];
                imageView.highlighted = NO;
            }
            mPlacedViewIndex = SE_INVALID_IMAGE_INDEX;
        }
        mOperationEnable[DELETE_OP] = YES;
        mOperationEnable[SHARE_OP] = YES;
        mOperationContainer = mViewArray[SELECTED_IMAGE_VIEW];
        
    }
    if(mFloatView)
    {
        [mFloatView removeFromSuperview];
        mFloatView = nil;
        [scrollView enableAllGestures];
    }
    [self disappearOperationViewGroup];
    [mSelectedImageViewIndex release];
    [mSelectedImage release];
    [mSelectedPhotoURL release];
    mSelectedPhotoURL = nil;
    mSelectedImage = nil;
    mSelectedImageViewIndex = nil;
    mOperationContainer = nil;
}
- (void) clicked:(CGPoint)p scrollView:(SEPageUIScrollView *)scrollView
{
    SEPageHitProperty hp = [scrollView hitRect:p];
    if(hp.imageView)
    {
        if(hp.imageView.highlighted == NO)
            hp.imageView.highlighted = YES;    
        else
            hp.imageView.highlighted = NO;        
    }
}
- (void) pressed:(CGPoint)p scrollView:(SEPageUIScrollView *)scrollView

{
    SEPageHitProperty hp = [scrollView hitRect:p];
    if(hp.imageView)
    {

    }
}
- (void) setSelectedMusicToCoreData: (NSArray*)musicArray
{
    UserInfo* userInfo = [self getUserInfo];
    MusicList* musicList = [self getMusicListByName:userInfo.currentmusiclist];
    NSSet* selectedMusic = musicList.selectedmusic;
    [musicList removeSelectedmusic:selectedMusic];
    for(int i = 0 ; i < musicArray.count ; i++)
    {
        SEMusicItemProperty* item = [musicArray objectAtIndex:i];
        SelectedMusic* sm = (SelectedMusic*)[self newObjectByEntityName:@"SelectedMusic"];
        sm.seq = [NSNumber numberWithInt:i];
        sm.title = item.title;
        sm.album = item.album;
        sm.singer = item.artist;
        [musicList addSelectedmusicObject:sm];
    }
    [self saveCoreDataContext];
}
- (void)setPhotoURLToCoreData: (NSMutableArray*)photoURLArray
{
    UserInfo* userInfo = [self getUserInfo];
    for(int i = 0 ; i < photoURLArray.count ; i++)
    {
        SEPageImageURL* imageURL = [photoURLArray objectAtIndex:i];
        SelectedImage* si = [self getSelectedImageProperty:i];
        if(si)
        {
            if(imageURL.url)
            {
                si.url = [imageURL.url absoluteString];
                si.urldate = imageURL.urlDate;
                si.width = [NSNumber numberWithInt: imageURL.origWidth];
                si.height = [NSNumber numberWithInt:imageURL.origHeight];
                si.urltype = [NSNumber numberWithInt:imageURL.type];
                si.orientation = [NSNumber numberWithInt: imageURL.orientation];
            }
            else
            {
                si.url = nil;
                si.urldate = nil;
                si.width = nil;
                si.height = nil;
                si.urltype = [NSNumber numberWithInt:SEPAGE_PHOTO_LIB_URL];
                si.orientation = [NSNumber numberWithInt:0];
            }
            if(imageURL.filepath)
                si.filepath = [imageURL.filepath absoluteString];
            else
                si.filepath = nil;
        }
        else
        {
            si = (SelectedImage*)[self newObjectByEntityName:@"SelectedImage"];
            si.seq = [NSNumber numberWithInt:i];
            if(imageURL.url)
            {
                si.url = [imageURL.url absoluteString];
                si.urldate = imageURL.urlDate;
                si.width = [NSNumber numberWithInt:imageURL.origWidth];
                si.height = [NSNumber numberWithInt:imageURL.origHeight];
                si.urltype = [NSNumber numberWithInt:imageURL.type];
                si.orientation = [NSNumber numberWithInt:imageURL.orientation];
            }
            else
            {
                si.url = nil;
                si.urldate = nil;
                si.width = nil;
                si.height = nil;
                si.urltype = [NSNumber numberWithInt:SEPAGE_PHOTO_LIB_URL];
                si.orientation = [NSNumber numberWithInt:0];
            }
            if(imageURL.filepath)
                si.filepath = [imageURL.filepath absoluteString];
            else
                si.filepath = nil;
            ImageList* il = [self getImageListByName:userInfo.currentimagelist];
            [il addSelectedimageObject:si];
        }
    }
    [self saveCoreDataContext];
}
- (BOOL) operationEnable: (int)index
{
    return mOperationEnable[index];
}
- (void) popupView:(UIView*)v
{
    mPopupView = [[SEPopupView alloc] initWithFrame:CGRectMake(0, 0, mViewPortWidth, mViewPortHeight)];
    mPopupView.mContentView = v;
    [mPopupView showAt:CGPointMake(0, 0) parent:mRootView];
}
- (void) dismissPopup
{
    [mPopupView dismiss];
    [mPopupView release];
    mPopupView = nil;
}
- (NSArray*) fetchFinishedImage: (NSString*)url urlDate: (NSString*)date managedObjectContext: (NSManagedObjectContext*)moc
{
    NSFetchRequest* fetchReq = [[NSFetchRequest alloc] init];
    NSEntityDescription* entity = [NSEntityDescription entityForName:@"FinishedImage" inManagedObjectContext:moc];
    [fetchReq setEntity:entity];
    NSPredicate* predicate = [NSPredicate predicateWithFormat:@"(url == %@) AND (urldate == %@)", url, date];
    //NSPredicate* predicate = [NSPredicate predicateWithFormat:@"url == %@", url];
    
    [fetchReq setPredicate:predicate];
    NSError* error = nil;
    NSArray* data = [moc executeFetchRequest:fetchReq error:&error];
    if(error)
    {
        NSLog(@"Unresolved error when init data : %@", [error userInfo]);
        abort();
    }
    [fetchReq release];
    return data;
}
- (NSArray*) fetchFinishedImage: (NSString*)url urlDate: (NSString*)date
{
    return [self fetchFinishedImage:url  urlDate:date managedObjectContext:self.managedObjectContext];
    /*
    NSFetchRequest* fetchReq = [[NSFetchRequest alloc] init];
    NSEntityDescription* entity = [NSEntityDescription entityForName:@"FinishedImage" inManagedObjectContext:self.managedObjectContext];
    [fetchReq setEntity:entity];
    NSPredicate* predicate = [NSPredicate predicateWithFormat:@"(url == %@) AND (urldate == %@)", url, date];
    //NSPredicate* predicate = [NSPredicate predicateWithFormat:@"url == %@", url];

    [fetchReq setPredicate:predicate];
    NSError* error = nil;
    NSArray* data = [self.managedObjectContext executeFetchRequest:fetchReq error:&error];
    if(error)
    {
        NSLog(@"Unresolved error when init data : %@", [error userInfo]);
        abort();
    }
    [fetchReq release];
    return data;
     */
}
- (void) removeImageFromCoreDate:(NSString*)url urlDate: (NSString*)date
{
    NSArray* data = [self fetchFinishedImage:url urlDate:date];
    assert(data.count == 1 || data.count == 0);
    if(data.count > 0)
    {
        FinishedImage* fi = [data objectAtIndex:0];
        [self.managedObjectContext deleteObject:fi];
    }
}
- (void) saveImageAndThumbnailToCoreData:(UIImage*)uiImage urlName:(NSString*)url urlDate: (NSString*)date index:(int)index
{
    NSArray* data = [self fetchFinishedImage:url urlDate:date];
    assert(data.count == 1 || data.count == 0);
    NSData* pngData = nil;
    NSData* thumbnailData = nil;
    if(uiImage)
    {
        pngData = UIImagePNGRepresentation(uiImage);
        CGSize size = CGSizeMake(128, 128);
        UIImage* thumbnail = [SEUtil createThumbnail:uiImage thumbnailSize:size];
        thumbnailData = UIImagePNGRepresentation(thumbnail);
    }
    
    if(data.count > 0)
    {
        FinishedImage* finishedImage = [data objectAtIndex:0];
        finishedImage.thumbnail = thumbnailData;
        finishedImage.data = pngData;
    }
    else
    {
        NSManagedObject* obj = [self newObjectByEntityName:@"FinishedImage"];
        [obj setValue:url forKey:@"url"];
        [obj setValue:date forKey:@"urldate"];
        [obj setValue:pngData forKey:@"thumbnail"];
        [obj setValue:thumbnailData forKey:@"data"];
    }
    [self saveCoreDataContext];    
}
- (void) saveImageThumbnailToCoreData:(UIImage*)uiImage urlName:(NSString*)url urlDate: (NSString*)date index:(int)index
{
    NSArray* data = [self fetchFinishedImage:url urlDate:date];
    assert(data.count == 1 || data.count == 0);
    NSData* pngData = nil;
    if(uiImage)
    {
        CGSize size = [self getThumbnailSize];
        UIImage* thumbnail = [SEUtil createThumbnail:uiImage thumbnailSize:size];
        pngData = UIImagePNGRepresentation(thumbnail);
    }
    
    if(data.count > 0)
    {
        FinishedImage* finishedImage = [data objectAtIndex:0];
        finishedImage.thumbnail = pngData;
    }
    else
    {
        NSManagedObject* obj = [self newObjectByEntityName:@"FinishedImage"];
        [obj setValue:url forKey:@"url"];
        [obj setValue:date forKey:@"urldate"];
        [obj setValue:pngData forKey:@"thumbnail"];
    }
    //use for test
    NSArray* paths = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES);
    NSString* docs = [paths objectAtIndex:0];
    NSString* strIndex = [NSString stringWithFormat:@"%d" , index];
    NSString* path = [docs stringByAppendingFormat:@"%@%@%@%@", @"/", @"ttestet_", strIndex, @".png"];
    const char* cpath = [path cStringUsingEncoding:NSASCIIStringEncoding];
    NSError* writeError = nil;
    BOOL ret = [pngData writeToFile:path options:NSDataWritingAtomic error:&writeError];
    if(ret == NO && writeError != nil)
    {
        NSLog(@" Error save image : %@", path);
    }
    //end
    [self saveCoreDataContext];
}
- (void) saveImageToCoreData: (UIImage*)uiImage urlName:(NSString*)url urlDate: (NSString*)date index:(int)index

{
    NSArray* data = [self fetchFinishedImage:url urlDate:date];
    assert(data.count == 1 || data.count == 0);
    NSData* pngData = nil;
    if(uiImage)
    {
        pngData = UIImagePNGRepresentation(uiImage);
    }
    /*
    //use for test
    NSArray* paths = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES);
    NSString* docs = [paths objectAtIndex:0];
    NSString* path = [docs stringByAppendingFormat:@"%@%@%@", @"/", @"ttestet", @".png"];
    const char* cpath = [path cStringUsingEncoding:NSASCIIStringEncoding];
    NSError* writeError = nil;
    BOOL ret = [pngData writeToFile:path options:NSDataWritingAtomic error:&writeError];
    if(ret == NO && writeError != nil)
    {
        NSLog(@" Error save image : %@", path);
    }
    //end
     */
    if(data.count > 0)
    {
        FinishedImage* finishedImage = [data objectAtIndex:0];
        finishedImage.data = pngData;
    }
    else
    {
        NSManagedObject* obj = [self newObjectByEntityName:@"FinishedImage"];
        [obj setValue:url forKey:@"url"];
        [obj setValue:date forKey:@"urldate"];
        [obj setValue:pngData forKey:@"data"];
    }
    [self saveCoreDataContext];
}
- (UIImage*) getThumbnailFromCoreData:(NSString*)url urlDate: (NSString*)date
{
    if(url == nil || date == nil)
        return nil;
    //NSLog(@"date = %@", date);
    //NSLog(@"url = %@", url);
    NSArray* data = [self fetchFinishedImage:url urlDate:date];
    //NSLog(@"thumb data = %@, %d", data, data.count);
    if(data == nil || data.count == 0)
        return nil;
    FinishedImage* fi = [data objectAtIndex:0];
    NSData* pngData = fi.thumbnail;
    UIImage* img = [UIImage imageWithData:pngData];    
    NSLog(@"# thumbnail orientation = %d ##", img.imageOrientation );
    return img;
}
- (UIImage*) getImageFromCoreData: (NSString*)url urlDate: (NSString*)date managedObjectContext: (NSManagedObjectContext*)moc
{
    if(url == nil || date == nil)
        return nil;
    //NSLog(@"date = %@", date);
    //NSLog(@"url = %@", url);
    NSArray* data = [self fetchFinishedImage:url urlDate:date managedObjectContext:moc];
    if(data == nil || data.count == 0)
        return nil;
    FinishedImage* fi = [data objectAtIndex:0];
    NSData* pngData = fi.data;
    //UIImageOrientation o = (UIImageOrientation)[fi.orientation intValue];
    return [UIImage imageWithData:pngData];
}
- (UIImage*) getImageFromCoreData: (NSString*)url urlDate: (NSString*)date
{
    if(url == nil || date == nil)
        return nil;
    //NSLog(@"date = %@", date);
    //NSLog(@"url = %@", url);
    NSArray* data = [self fetchFinishedImage:url urlDate:date];
    if(data == nil || data.count == 0)
        return nil;
    FinishedImage* fi = [data objectAtIndex:0];
    NSData* pngData = fi.data;
    //UIImageOrientation o = (UIImageOrientation)[fi.orientation intValue];
    return [UIImage imageWithData:pngData];
}
- (CGSize) getThumbnailSize
{
    return CGSizeMake(128, 128);
}
- (UIImage*) getImageFromPhotoLib : (NSString*)url urlDate: (NSString*)date
{
    return nil;
}
- (BOOL) isURLInPhotoURLs: (NSMutableArray*) photoURLs url:(NSURL*)url urlData : (NSString*)urlDate
{
    for(SEPageImageURL* imageURL in photoURLs)
    {
        if([imageURL.url isEqual:url] && [imageURL.urlDate isEqualToString:urlDate])
            return YES;
    }
    return NO;
}
- (void) setSelectedImageProperty: (NSString*)url urlDate: (NSString*)urlDate orientation: (int) orient image:(UIImage*)image
{
    SelectedImage* si =  [self getSelectedImageByUrl: url andDate:urlDate];
    si.orientation = [NSNumber numberWithInt:orient];
    si.width = [NSNumber numberWithInt: image.size.width];
    si.height = [NSNumber numberWithInt:image.size.height];
    NSLog(@"## %s, %d , width = %f, height = %f ##", __FUNCTION__, __LINE__, image.size.width, image.size.height);
}
- (void) removeSelectedImageNotInImageURLs:(NSMutableArray*)photoURLs
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* imageListSet = userInfo.imagelist;
    NSString* currentImageList = userInfo.currentimagelist;
    NSMutableArray* deleteSelectedImage = [NSMutableArray array];
    for(ImageList* il in imageListSet)
    {
        if([il.name isEqualToString:currentImageList])
        {
            NSSet* selectedImageSet = il.selectedimage;
            NSArray* newArray = [selectedImageSet allObjects];
            newArray = [newArray sortedArrayUsingComparator:^NSComparisonResult(id obj1, id obj2) {
                SelectedImage* mo1 = (SelectedImage*)obj1;
                SelectedImage* mo2 = (SelectedImage*)obj2;
                NSNumber* left = mo1.seq;
                NSNumber* right = mo2.seq;
                return [left compare:right];
            }];
            NSMutableArray* deleteArray = [NSMutableArray array];
            for(SelectedImage* si in newArray)
            {
                NSURL* url = [NSURL URLWithString:si.url];
                NSString* urldate = si.urldate;
                if(url != nil && [self isURLInPhotoURLs:photoURLs url:url urlData:urldate] == NO)
                {
                    [deleteArray addObject:si];
                    [deleteSelectedImage addObject:si];
                }
            }
            for(SelectedImage* si in deleteArray)
            {
                [il removeSelectedimageObject:si];
            }
        }
    }
    for(SelectedImage* si in deleteSelectedImage)
    {
        NSArray* data = [self fetchFinishedImage:si.url urlDate:si.urldate];
        if(data != nil && data.count > 0)
        {
            for(FinishedImage* fi in data)
            {
                [self.managedObjectContext deleteObject:fi];
            }
        }
    }
    [self saveCoreDataContext];
}
- (NSSet*) getUnsendIssueReport
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* issueReport = userInfo.issuereport;
    return issueReport;
}
- (void) saveIssueReport: (NSString*)devName : (double)date : (NSString*)title : (NSString*)descript
{
    UserInfo* userInfo = [self getUserInfo];
    IssueReport* isr = (IssueReport*)[self newObjectByEntityName:@"IssueReport"];
    [isr setValue:title forKey:@"title"];
    [isr setValue:[NSNumber numberWithDouble:date] forKey:@"date"];
    [isr setValue:descript forKey:@"descript"];
    [isr setValue:devName forKey:@"devicename"];
    [userInfo addIssuereportObject:isr];
    [self saveContext];
}
- (void) printSelectedImage
{
    NSArray* selectedImageArray = [self getUserImageProperty];
    int i = 0; 
    for(SelectedImage* si in selectedImageArray)
    {
        if(si.url != nil)
        {
            NSLog(@" selected %d image: %@ , %@, %@ ##", i, si.url, si.urldate, si.urltype);
            i++;
        }
    }
}
- (void) printThumbnail
{
    NSFetchRequest* fetchReq = [[NSFetchRequest alloc] init];
    NSEntityDescription* entity = [NSEntityDescription entityForName:@"FinishedImage" inManagedObjectContext:self.managedObjectContext];
    [fetchReq setEntity:entity];
    NSError* error = nil;
    NSArray* data = [self.managedObjectContext executeFetchRequest:fetchReq error:&error];
    if(error)
    {
        NSLog(@"Unresolved error when init data : %@", [error userInfo]);
        abort();
    }
    [fetchReq release];
    int index = 0;
    for(FinishedImage* fi in data)
    {
        NSLog(@"## thumbnail %d: %@, %@", index, fi.url, fi.urldate);
        index++;
    }

}
- (void) determineSelectedMusicRow
{
    if(mMusicFloatView == nil)
        return;
    SEContentViewContainer* container = mViewArray[SELECTED_MUSIC_VIEW];
    SESelectedMusicView* selectedMusicView = (SESelectedMusicView*)[container contentView];
    [selectedMusicView determineSelectedMusicRow: mMusicFloatView];
}
- (void) addMusicToSelectedView: (SEMusicItemProperty*)item
{
    if(item == nil)
        return;
    SEContentViewContainer* container = mViewArray[SELECTED_MUSIC_VIEW];
    SESelectedMusicView* selectedMusicView = (SESelectedMusicView*)[container contentView];
    [selectedMusicView addNewMusic:item];
}
- (Signature*) getSignature: (NSNumber*)seq
{
    UserInfo* userInfo = [self getUserInfo];
    NSSet* signatureSet = userInfo.signaturelist;
    Signature* sig = nil;
    NSEnumerator* it = [signatureSet objectEnumerator];
    while((sig = [it nextObject]) != nil)
    {
        NSNumber* tmpSeq = sig.seq;
        if([tmpSeq isEqualToNumber:seq])
        {
            return sig;
        }
    }
    return nil;
}
- (SignaturePointData) getSignaturePointsWithSeqName:(NSString*)name
{
    UserInfo* userInfo = [self getUserInfo];
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
- (SignaturePointData) getCurrentSignaturePoints
{
    UserInfo* userInfo = [self getUserInfo];
    return [self getSignaturePoints:userInfo.currentsignature];
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
- (void)writeDataToFile
{
    if(mRecvData == nil)
        return;
    if(mRecvData.length == 0)
    {
        [mRecvData release];
        return;
    }
    BOOL mydebug = YES;
    if(mydebug)
    {
        int len = mRecvData.length;
        const char* bytes = (const char*)[mRecvData bytes];
        char* data = (char*)malloc(len + 1);
        memset(data, 0, len + 1);
        memcpy(data, bytes, len);
        NSString* str = [NSString stringWithCString:data encoding:NSUTF8StringEncoding];
        NSLog(@"### get from internet : %@ ", str);
    }
    NSFileManager* fileManager = [NSFileManager defaultManager];
    NSArray* dir = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask,YES);
    NSString* directoryPath = [dir objectAtIndex:0];//[[dir objectAtIndex:0] stringByAppendingFormat:@"/config/"];
    NSURL* url = [NSURL fileURLWithPath:directoryPath isDirectory:YES];
    NSError* error = nil;
    BOOL succ = YES;
    //BOOL succ = [fileManager createDirectoryAtURL:url withIntermediateDirectories:YES attributes:nil error:&error];
    if(succ == YES)
    {
        NSString* filePath = [directoryPath stringByAppendingFormat:@"/paramset_url.txt"];
        succ = [fileManager createFileAtPath:filePath contents:mRecvData attributes:nil];
        if(succ)
        {
            NSLog(@"write data to file OK");
            self.mNewConfig = YES;
            if(mLabel)
            {
                UILabel* label = (UILabel*)mLabel;
                label.text = @"OK";
            }
        }
        else 
        {
            NSLog(@"write data to file error");
            self.mNewConfig = NO;
            if(mLabel)
            {
                UILabel* label = (UILabel*)mLabel;
                label.text = @"ERROR";
            }
        }
    }
    [mRecvData release];
}
- (void) downloadParamConfig: (id) label
{
    NSString* strURL = @"http://mobilefly.sinaapp.com/text_get_action.php";
    NSURL* url = [NSURL URLWithString:strURL];
    NSMutableURLRequest* req = [NSMutableURLRequest requestWithURL:url];
    [req setHTTPMethod:@"POST"];
    NSMutableData* postBody = [NSMutableData data];
    [postBody appendData:[[NSString stringWithFormat:@"user=%@&", @"aaa"] dataUsingEncoding:NSUTF8StringEncoding]];
    [postBody appendData:[[NSString stringWithFormat:@"password=%@&", @"bbb"] dataUsingEncoding:NSUTF8StringEncoding]];
    [req setHTTPBody:postBody];
    NSURLConnection* conn = [NSURLConnection connectionWithRequest:req delegate:self];
    mRecvData = [NSMutableData data];
    mRecvData = [mRecvData retain];
    mLabel = label;
    ((UILabel*)mLabel).text = @"download";
    [conn retain];
}
- (void)connection:(NSURLConnection *)connection didReceiveResponse:(NSURLResponse *)response
{
    NSLog(@"start response");
    NSThread* currentThread = [NSThread currentThread];
    NSThread* mainThread = [NSThread mainThread];
    assert(currentThread == mainThread);
    if([response isMemberOfClass:[NSHTTPURLResponse class]])
    {
        int statusCode = ((NSHTTPURLResponse*)response).statusCode;
        NSLog(@"response status code = %d ", statusCode);
    }
}
- (void)connection:(NSURLConnection *)connection didReceiveData:(NSData *)data
{
    NSLog(@"receive data");
    NSThread* currentThread = [NSThread currentThread];
    NSThread* mainThread = [NSThread mainThread];
    assert(currentThread == mainThread);
    [mRecvData appendData:data];
}
- (void)connection:(NSURLConnection *)connection didFailWithError:(NSError *)error
{
    NSLog(@"error response");
    if(mLabel)
    {
        UILabel* label = (UILabel*)mLabel;
        label.text = @"error";
    }
    [connection release];
}
- (void)connectionDidFinishLoading:(NSURLConnection *)connection
{
    NSLog(@"finished connection");
    [self writeDataToFile];
    [connection release];

}  


////////////
- (void) shareImage
{
    [mShareImage share];
}
- (void) addShareImage : (UIImage*) image
{
    [mShareImage addImage:image];
}
- (SEDataUploadManager*) getDataUploadManager
{
    return mDataUploadManager;
}

@end
