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
#import "UserInfo.h"
#import "SEUtil.h"
#import "SEResDefine.h"
#import "SESignaturePopupViewController.h"
#import "Signature.h"
enum ALERT_TYPE {FOR_ADD, FOR_CHANGE};
struct SignaturePointData
{
    CGFloat lineWidth;
    NSMutableArray* points;
};
///////
static int convertInt(int num)
{
    int v = num << 24 || num << 16 || num << 8 || (num & 0xFF);
    return v;
}
@interface SESignatureView (Private)
- (void) setButtonHandler;
- (void) addButtonHandler:(id)sender;
- (void) removeHandler: (id)sender;
- (void) initScrollView;
- (void) savePoints : (NSNumber*)seq;
- (Signature*) getSignature: (NSNumber*)seq;
- (void)initSignature;
- (SignaturePointData) getSignaturePoints:(NSNumber*)seq;
- (BOOL) isSignaturePointsEqual:(NSMutableArray*)src : (NSMutableArray*)dst;
- (void) popupAlertViewForSave;
- (void) setSignature: (NSNumber*)seq;
- (void) setCurrentSelectedSignatureImageView;
-(SignaturePointData)getSignaturePointsWithSeqName: (NSString*)name;
@end
/////////////////////////
@interface SEUISignatureScrollViewDelegate : NSObject <SEUIPhotoLoaderDelegate>
{
    SEUIScrollView* mScrollView;
    SEViewNavigator* mNavView;
    SESignatureView* mSignatureView;
}
@property (nonatomic, assign) SESignatureView* mSignatureView;
@property (nonatomic, assign) SEUIScrollView* mScrollView;
@property (nonatomic, assign) SEViewNavigator* mNavView;
- (void) initState;
- (void) createPhotoUrlArray: (NSMutableArray*)photoURLArray;
- (CGImageRef) getPhotoFromURL:(SEImageURL *)url;
@end
@implementation SEUISignatureScrollViewDelegate
@synthesize mScrollView;
@synthesize mNavView;
@synthesize mSignatureView;
- (void) initState
{
}
- (void)createPhotoUrlArray: (NSMutableArray*)photoURLArray
{
    NSArray* allSignatures = [mNavView getAllSignatures];
    for(Signature* sig in allSignatures)
    {
        SEImageURL* imageURL = [[SEImageURL alloc] init];
        NSString* str = [NSString stringWithFormat:@"%d", [sig.seq intValue]];
        imageURL.filepath = [NSURL URLWithString:str];
        imageURL.type = FILE_PATH;
        [photoURLArray addObject:imageURL];
    }
}
- (CGImageRef) getPhotoFromURL:(SEImageURL *)url
{
    NSURL* filePath = url.filepath;
    if(filePath != nil)
    {
        NSString* str = [filePath absoluteString];
        int seq = [str intValue];
        SignaturePointData sd = [mSignatureView getSignaturePointsWithSeqName:str];
        UIImage* background = nil;
        if(seq == mSignatureView.mCurrentSeq)
            background = mSignatureView.mSelectedSignatureImage;
        else
            background = mSignatureView.mNormalSignatureImage;
        UIImage* image = nil;
        if(sd.points.count > 0)
        {
            image = [SEUtil createUIImageFrom: background  withLineWidth: (CGFloat) sd.lineWidth drawPoints: sd.points];
            return CGImageRetain([image CGImage]);
        }
        else
        {
            CGImageRef imageRef = [SEUtil copyImageRef:[background CGImage]];
            return imageRef;
        }
        /*
        UIImage* uiImage = [UIImage imageNamed:str];
        uiImage = [SEUtil cropUIImage:uiImage withRect:CGSizeMake(mScrollView.mPhotoWidth, mScrollView.mPhotoHeight)];
        CGImageRef image = [uiImage CGImage];
        CGImageRetain(image);
         */
        
    }
    else
        return NULL;
}
- (void)dealloc
{
    [super dealloc];
}
@end

/////////////////////////

@implementation SESignatureView (Private)
- (Signature*) getSignature: (NSNumber*)seq
{
    UserInfo* userInfo = [mViewNav getUserInfo];
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
- (void) savePoints : (NSNumber*)seq;
{
    NSMutableArray* pointsList = [mDrawView getAllNormalizedPoints];
    if(pointsList.count <= 0)
        return;
    int size = sizeof(int) + sizeof(CGFloat);
    for(NSArray* obj in pointsList)
    {
        size += sizeof(int);
        for(NSValue* v in obj)
        {
            size += 2 * sizeof(CGFloat);
        }
    }
    UInt8* data = (UInt8*)malloc(size);
    NSOutputStream* output = [NSOutputStream outputStreamToBuffer:data capacity:size];
    [output open];
    NSStreamStatus ss = output.streamStatus;
    int num = pointsList.count;
    CGFloat f = mDrawView.lineWidth;
    int ret = [output write:(const uint8_t*)&num maxLength:sizeof(int)];
    assert(ret != -1);
    ret = [output write:(const uint8_t*)&f maxLength:sizeof(CGFloat)];
    assert(ret != -1);
    for(NSArray* obj in pointsList)
    {
        int count = obj.count;
        ret = [output write:(const uint8_t *)&count maxLength:sizeof(int)];
        assert(ret != -1);
        for(NSValue* v in obj)
        {
            CGPoint p = [v CGPointValue];
            ret = [output write:(const uint8_t *)(&p.x) maxLength:sizeof(CGFloat)];
            assert(ret != -1);
            ret = [output write:(const uint8_t *) (&p.y) maxLength:sizeof(CGFloat)];
            assert(ret != -1);
        }
    }
    [output close];
    NSData* nsdata = [NSData dataWithBytes:data length:size];
    free(data);
    Signature* sig = [self getSignature:seq];
    sig.data = nsdata;
    [mViewNav saveContext];
}
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
- (void) removeHandler:(id)sender
{
}
- (void) saveToDocument: (NSString*) fileName
{
    
}
- (void) setCurrentSelectedSignatureImageView
{
    //mCurrentSelectedSignatureImageView = [mScrollView imageView:mCurrentSeq];
    //mCurrentSelectedSignatureImageView.image = mSelectedSignatureImage;
}
- (void) initScrollView
{
    SEUIScrollView* scrollView = mScrollView;
    UITapGestureRecognizer* ges = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(tapHandler:)];
    [scrollView addGestureRecognizer:ges];
    [ges release];
    CGRect rect = scrollView.frame;
    scrollView.mSelectedIndex = mCurrentSeq;
    scrollView.userInteractionEnabled = YES;
    scrollView.mResLoader = mViewNav.mResLoader;
    scrollView.mName = @"signature view";
    scrollView.mHandleMultiTouchDelegate = nil;
    scrollView.backgroundColor = [UIColor whiteColor];
    scrollView.mViewWidth = rect.size.width;
    scrollView.mViewHeight = rect.size.height;
    scrollView.mPhotoWidth = mSignatureImageWidth;
    scrollView.mPhotoHeight = mSignatureImageHeight;
    scrollView.mVMargin = 5;
    scrollView.mHMargin = 0;
    scrollView.mViewNavigator = mViewNav;
    scrollView.mDefaultImage = mNormalSignatureImage;
    //scrollView.mSelectedFrameImage = [mResLoader getImage:@"SignatureSelectedFrameImage"];
    SEUISignatureScrollViewDelegate* photoFileDelegate = [[SEUISignatureScrollViewDelegate alloc] init];
    photoFileDelegate.mNavView = mViewNav;
    photoFileDelegate.mScrollView = scrollView;
    photoFileDelegate.mSignatureView = self;
    scrollView.mPhotoLoaderDelegate= photoFileDelegate;
    [scrollView initState];
    [scrollView initPhotoLibUrl];
    [scrollView createContent];
    scrollView.mUseMultiTouch = NO;
    
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
    [mViewNav saveContext];
    [self dismissPopup];
    
    CGRect r = mScrollView.frame;
    [mScrollView removeFromSuperview];
    mScrollView = [[SEUIScrollView alloc] init];
    mScrollView.frame = r;
    [self initScrollView];
    [self addSubview:mScrollView];
    [self setCurrentSelectedSignatureImageView];
    mDrawView.lineWidth = 8.0f;
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
    if(src == nil && dst != nil)
        return NO;
    if(dst == nil && src != nil)
        return NO;
    if(dst == nil && src == nil)
        return YES;
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
    }
    return YES;
}
- (void) setSignature: (NSNumber*)seq
{
    SignaturePointData sd = [self getSignaturePoints:seq];
    if(sd.points && sd.points.count > 0)
    {
        [mDrawView setLineWidth:sd.lineWidth];
        [mDrawView setNormalizePoints:sd.points];
    }
    else
    {
        mDrawView.lineWidth = 8.0f;
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
- (void) addButtonHandler:(id)sender
{
    NSMutableArray* currPoints = [mDrawView getAllNormalizedPoints];
    SignaturePointData sd = [self getSignaturePoints:[NSNumber numberWithInt:mCurrentSeq]];
    if(![self isSignaturePointsEqual:currPoints :sd.points])
    {
        mAlertType = FOR_ADD;
        [self popupAlertViewForSave];
    }
    else
    {
        [self popupSignatureSave];
    }

}
- (void) setButtonHandler
{
    [mAddButton addTarget:self action:@selector(addButtonHandler:) forControlEvents:UIControlEventTouchUpInside];
}
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

- (void) initSignature
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    NSNumber* currentseq = userInfo.currentsignature;
    mCurrentSeq = [currentseq intValue];
    [self setSignature:currentseq];
    [self setCurrentSelectedSignatureImageView];
}
- (void) alertView:(UIAlertView *)alertView clickedButtonAtIndex:(NSInteger)buttonIndex
{
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
}
@end
@implementation SESignatureView
@synthesize mViewNav;
@synthesize mSigViewController;
@synthesize mResLoader;
@synthesize mCurrentSeq;
@synthesize mSelectedSignatureImage;
@synthesize mNormalSignatureImage;
- (id)init
{
    self = [super init];
    if (self) {
        // Initialization code here.
    }
    
    return self;
}
- (void) dealloc
{
    [mNormalSignatureImage release];
    [mSelectedSignatureImage release];
    [mSigViewController release];
    [mPopup release];
    [super dealloc];
}
- (void) tapHandler:(UITapGestureRecognizer*)ges
{
    NSLog(@"sig scrll tap\n");
    CGPoint p = [ges locationInView:mScrollView];
    NSLog(@"p = %f, %f", p.x, p.y);
    HitProperty hitP = [mScrollView hitRect:p];
    if(hitP.index != -1 && hitP.index != mCurrentSeq)
    {
        //hitP.imageView.image = [mResLoader getImage:@"SelectedSignatureImage"];
        mCurrentPoint = p;
        mAlertType = FOR_CHANGE;
        NSMutableArray* currPoint = [mDrawView getAllNormalizedPoints];
        SignaturePointData sd = [self getSignaturePoints:[NSNumber numberWithInt:mCurrentSeq]];
        if(![self isSignaturePointsEqual:currPoint :sd.points])
        {
            [self popupAlertViewForSave];
        }
        else
        {
            //hitP.imageView.image= mSelectedSignatureImage;
            //mCurrentSelectedSignatureImageView.image = mNormalSignatureImage;
            //mCurrentSelectedSignatureImageView = hitP.imageView;
            int prevIndex = mCurrentSeq;
            
            mCurrentSeq = hitP.index;
            [self setSignature:[NSNumber numberWithInt:mCurrentSeq]];
            [mScrollView updateImage:prevIndex];
            [mScrollView updateImage:mCurrentSeq];
            
            //[mScrollView relayout];
        }
    }
    
}
- (void) initData
{
    mSelectedSignatureImage = [mResLoader getImage:@"SelectedSignatureImage"];
    [mSelectedSignatureImage retain];
    mNormalSignatureImage = [mResLoader getImage:@"NormalSignatureImage"];
    [mNormalSignatureImage retain];
    mSignatureImageWidth = mSelectedSignatureImage.size.width;
    mSignatureImageHeight = mSelectedSignatureImage.size.height;
    mScrollView = (SEUIScrollView*)[self viewWithTag:102];
    mDrawView = (SEDrawTouchView*)[self viewWithTag:101];
    [mDrawView initData];
    mDrawView.userInteractionEnabled = YES;
    mDrawView.backgroundColor = [UIColor greenColor];
    self.userInteractionEnabled = YES;
    mAddButton = (UIButton*)[self viewWithTag:103];
    mDeleteButton = (UIButton*)[self viewWithTag:104];
    [self setButtonHandler];
    [self initSignature];
    [self initScrollView];
    [self setCurrentSelectedSignatureImageView];
    /*
    UIImage* image = [mViewNav getCurrentSignature];
    if(image)
    {
        mDrawView.background = image;
    }
     */
}
- (void) saveContext:(UserInfo*) userInfo
{
    
}
@end
