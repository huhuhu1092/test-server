//
//  SEImageMusicListView.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-2-20.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEImageMusicListView.h"
#import "SEViewNavigator.h"
#import "SEUtil.h"
#import "SEResDefine.h"
#import "ImageList.h"
#import "MusicList.h"
#import "SelectedImage.h"
#import "SelectedMusic.h"
#import "UserInfo.h"
#import "SEMusicImageListPopup.h"
/////////
enum INDICTION_TYPE {IMAGE_INDICATION, MUSIC_INDICATION};
enum ALERT_TYPE {NO_ALERT, ADD_MUSIC_LIST, ADD_IMAGE_LIST};
enum TABLE_TYPE {NO_TABLE, MUSIC_TABLE, IMAGE_TABLE};
struct RowData
{
    NSIndexPath* indexPath;
    int tableType;
};
//////
@interface LineViewData : NSObject {
@private
    int mStartIndicatorTableType;
    int mStartIndicatorRow;
    int mEndIndicatorTableType;
    int mEndIndicatorRow;
    CGPoint mStartPoint;
    CGPoint mEndPoint;
    UIView* mView;
}
@property (nonatomic, assign) int mStartIndicatorTableType;
@property (nonatomic, assign) int mStartIndicatorRow;
@property (nonatomic, assign) int mEndIndicatorTableType;
@property (nonatomic, assign) int mEndIndicatorRow;
@property (nonatomic, retain) UIView* mView;
@property (nonatomic, assign) CGPoint mStartPoint;
@property (nonatomic, assign) CGPoint mEndPoint;
@end
@implementation LineViewData
@synthesize mStartIndicatorRow;
@synthesize mStartIndicatorTableType;
@synthesize mEndIndicatorRow;
@synthesize mEndIndicatorTableType;
@synthesize mView;
@synthesize mStartPoint;
@synthesize mEndPoint;
- (void) dealloc
{
    [mView release];
    [super dealloc];
}
@end
/////////////////////////////////
@interface SEMusicImageListView (Private)
- (void) addMusicListButtonHandler: (id)sender;
- (void) addImageListButtonHandler: (id)sender;
-(void) removeButtonHandler: (id)sender;
- (void) handleOK:(id)sender;
- (void) handleCancel: (id)sender;
- (void) dismissPopup;
- (void)setSelectedItem: (NSString*) name type: (int)type;
- (RowData) getIndexPathOnPoint: (CGPoint)p;
- (void) addAttach;
- (void) drawAttach;
- (void) drawAttachFrom: (int) seq tableType: (int) tableType offset: (CGPoint) offset;
- (SEIndicatorView*) getIndicatorView: (int)tableType row: (int)row;
- (UIView*) createLineView: (CGPoint) src  : (CGPoint) dst;
- (void) scrollHandler : (CGPoint) offset tableType: (int)currentTableType;
- (BOOL) hasAttachedToMusicList:(int)imageSeq;
- (BOOL) hasAttachedImageInMusicList: (int)musicSeq;
- (void) clearLineView;
- (void) handleImageClick: (UITapGestureRecognizer*)ges;
- (void) handleMusicClick: (UITapGestureRecognizer*)ges;
@end
@implementation SEMusicImageListView (Private)
- (void) handleMusicClick: (UITapGestureRecognizer*)ges
{
    UIView* v = ges.view;
    [mViewNav setViewRelationType:TYPE2];
    int index = v.tag;
    UserInfo* userInfo = [mViewNav getUserInfo];
    MusicList* musicList = [mViewNav getMusicListBySeq:[NSNumber numberWithInt:index]];
    userInfo.currentmusiclist = musicList.name;
    [mViewNav moveToView:MUSIC_IMAGE_LIST_ATTACH_MUSIC_PICKER :MUSIC_PICKER hasAnimation:YES isPush:YES];
}
- (void) handleImageClick: (UITapGestureRecognizer*)ges
{
    UIView* v = ges.view;
    NSLog(@"image click : %d\n", v.tag);
    int index = v.tag;
    ImageList* imageList = [mViewNav getImageListBySeq:[NSNumber numberWithInt:index]];
    UserInfo* userInfo = [mViewNav getUserInfo];
    userInfo.currentimagelist = imageList.name;
    NSLog(@"image list name = %@", imageList.name);
    [mViewNav setViewRelationType:TYPE2];
    [mViewNav moveToView:MUSIC_IMAGE_LIST_ATTACH_IMAGE_PICKER :IMAGE_PICKER hasAnimation:YES isPush:YES];
}
- (void) clearLineView
{
    for(LineViewData* lvd in mLineViewArray)
    {
        [lvd.mView removeFromSuperview];
    }
    [mLineViewArray release];
    mLineViewArray = [NSMutableArray array];
    [mLineViewArray retain];
}
- (BOOL) hasAttachedToMusicList:(int)imageSeq
{
    ImageList* imageList = [mViewNav getImageListBySeq:[NSNumber numberWithInt:imageSeq]];
    MusicList* ml = [mViewNav getMusicListByImageList:imageList.name];
    if(ml)
        return YES;
    else
        return NO;
    
}
- (BOOL) hasAttachedImageInMusicList: (int)musicSeq
{
    MusicList* ml = [mViewNav getMusicListBySeq:[NSNumber numberWithInt:musicSeq]];
    return ml.attachimagelist.count > 0;
}

- (void) scrollHandler : (CGPoint) offset tableType:(int) currentTableType
{
    for(LineViewData* lvd in mLineViewArray)
    {
        CGPoint p1 = lvd.mStartPoint;
        CGPoint p2 = lvd.mEndPoint;
        if(lvd.mStartIndicatorTableType == currentTableType)
        {
            p1.x -= offset.x;
            p1.y -= offset.y;
        }
        else if(lvd.mEndIndicatorTableType == currentTableType)
        {
            p2.x -= offset.x;
            p2.y -= offset.y;
        }
        [lvd.mView removeFromSuperview];
        lvd.mView = nil;
        UIView* v = [self createLineView:p1 :p2];
        lvd.mView = v;
        [self addSubview:v];
    }
}
- (UIView*) createLineView: (CGPoint) src  : (CGPoint) dst
{
    const float PI = 3.14159;
    float deltax = dst.x - src.x;
    float deltay = dst.y - src.y;
    float theta = atanf(fabsf(deltay) / fabsf(deltax));
    float width = sqrtf(deltax * deltax + deltay * deltay);
    float height = 20;
    float translatex = width / 2;;
    NSLog(@"deltax = %f, deltay = %f, theta = %f", deltax, deltay, theta * 180 / PI);
    if(deltax > 0 && deltay > 0)
    {
        
    }
    else if(deltax < 0 && deltay > 0)
    {
        theta = PI - theta;
    }
    else if(deltax < 0 && deltay < 0)
    {
        theta = - (PI - theta);
    }
    else if(deltax > 0 && deltay < 0)
    {
        theta = -theta;
    }
    UIImageView* lineView = [[UIImageView alloc] init];
    lineView.image = [UIImage imageNamed: @"list_line.png"];
    //lineView.backgroundColor = [UIColor blueColor];
    lineView.transform = CGAffineTransformIdentity;
    lineView.frame = CGRectMake(src.x - width / 2, src.y - height / 2, width, height);
    CGAffineTransform rotate = CGAffineTransformMakeRotation(theta);
    CGAffineTransform translate = CGAffineTransformMakeTranslation(translatex, 0);
    lineView.transform = CGAffineTransformConcat(translate, rotate);
    [lineView autorelease];
    return lineView;
}

- (SEIndicatorView*) getIndicatorView: (int)tableType row: (int)row
{
    if(tableType == NO_TABLE)
        return nil;
    if(tableType == MUSIC_TABLE)
    {
        UITableViewCell* cell = [mMusicListTableView cellForRowAtIndexPath:[NSIndexPath indexPathForRow:row inSection:0]];
        return (SEIndicatorView*)[cell viewWithTag:101];
    }
    else if(tableType == IMAGE_TABLE)
    {
        UITableViewCell* cell = [mImageListTableView cellForRowAtIndexPath:[NSIndexPath indexPathForRow:row inSection:0]];
        return (SEIndicatorView*)[cell viewWithTag:102];
    }
    return nil;
}
- (void) drawAttachFrom: (int) seq tableType: (int) tableType offset: (CGPoint)offset
{
    if(tableType == IMAGE_TABLE)
    {
        ImageList* il = [mViewNav getImageListBySeq:[NSNumber numberWithInt:seq]];
        MusicList* musicList = [mViewNav getMusicListByImageList:il.name];
        if(musicList)
        {
            int musicSeq = [musicList.seq intValue];
            CGPoint p1 = CGPointMake(mImageIndicatorPointInCell.x, mImageIndicatorPointInCell.y + seq * mImageCellHeight);
            CGPoint p2 = CGPointMake(mMusicIndicatorPointInCell.x, mMusicIndicatorPointInCell.y + musicSeq * mMusicCellHeight);
            p1 = [mImageListTableView convertPoint:p1 toView:self];
            p2 = [mMusicListTableView convertPoint:p2 toView:self];
            UIView* v = [self createLineView:p1 :p2];
            LineViewData* lvd = [[LineViewData alloc] init];
            lvd.mView = v;
            [mLineViewArray addObject:lvd];
            [lvd release];
            [self addSubview:v];
        }
    }
    else if(tableType == MUSIC_TABLE)
    {
        MusicList* musicList = [mViewNav getMusicListBySeq:[NSNumber numberWithInt:seq]];
        NSArray* imageListArray = [mViewNav getMusicAttachedImage:musicList.name];
        for(ImageList* il in imageListArray)
        {
            int imageListSeq = [il.seq intValue];
            CGPoint p1 = CGPointMake(mImageIndicatorPointInCell.x, mImageIndicatorPointInCell.y + imageListSeq * mImageCellHeight);
            CGPoint p2 = CGPointMake(mMusicIndicatorPointInCell.x, mMusicIndicatorPointInCell.y + seq * mMusicCellHeight);
            p1 = [mImageListTableView convertPoint:p1 toView:self];
            p2 = [mMusicListTableView convertPoint:p2 toView:self];
            UIView* v = [self createLineView:p1 :p2];
            LineViewData* lvd = [[LineViewData alloc] init];
            lvd.mView = v;
            [mLineViewArray addObject:lvd];
            [lvd release];
            [self addSubview:v];
        }
    }
}
- (void) drawAttach
{
    SEIndicatorView* startIndicator = [self getIndicatorView:mStartIndicatorTable row:mStartIndicatorRow];
    SEIndicatorView* endIndicator = [self getIndicatorView:mEndIndicatorTable row:mEndIndicatorRow];
    CGPoint p1Indicator;
    CGPoint p2Indicator;
    if(mStartIndicatorTable == IMAGE_TABLE)
        p1Indicator = mImageIndicatorCenter;
    else if(mStartIndicatorTable == MUSIC_TABLE)
        p1Indicator = mMusicIndicatorCenter;
    
    if(mEndIndicatorTable == IMAGE_TABLE)
        p2Indicator = mImageIndicatorCenter;
    else if(mEndIndicatorTable == MUSIC_TABLE)
        p2Indicator = mMusicIndicatorCenter;
    
    CGPoint p1 = [startIndicator convertPoint:CGPointMake(p1Indicator.x, p1Indicator.y) toView:self];
    CGPoint p2 = [endIndicator convertPoint:CGPointMake(p2Indicator.x, p2Indicator.y) toView:self];
    UIView* v = [self createLineView:p1 :p2];
    LineViewData* lvd = [[LineViewData alloc] init];
    lvd.mStartIndicatorRow = mStartIndicatorRow;
    lvd.mStartIndicatorTableType = mStartIndicatorTable;
    lvd.mEndIndicatorTableType = mEndIndicatorTable;
    lvd.mEndIndicatorRow = mEndIndicatorRow;
    lvd.mStartPoint = p1;
    lvd.mEndPoint = p2;
    lvd.mView = v;
    [mLineViewArray addObject:lvd];
    [lvd release];
    [self addSubview:v];
}
- (void) addAttach
{
    if(mStartIndicatorTable == NO_TABLE || mEndIndicatorTable == NO_TABLE )
        return;
    if(mStartIndicatorTable == mEndIndicatorTable)
        return;
    if(mStartIndicatorTable == IMAGE_TABLE && mEndIndicatorTable == MUSIC_TABLE)
    {
        ImageList* imageList = [mViewNav getImageListBySeq:[NSNumber numberWithInt:mStartIndicatorRow]];
        MusicList* musicList = [mViewNav getMusicListBySeq:[NSNumber numberWithInt:mEndIndicatorRow]];
        assert(imageList != nil);
        assert(musicList != nil);
        //[mViewNav attachImageToMusic:musicList.name imageListName:imageList.name];
        if([mViewNav getMusicListByImageList: imageList.name] == nil)
        {
            //[attachSet addObject:imageList];
            [musicList addAttachimagelistObject:imageList];
            [self drawAttach];
            [mMusicListTableView reloadData];
            [mImageListTableView reloadData];
            [mViewNav saveContext];
        }
    }
    else if(mStartIndicatorTable == MUSIC_TABLE && mEndIndicatorTable == IMAGE_TABLE)
    {
        MusicList* musicList = [mViewNav getMusicListBySeq:[NSNumber numberWithInt:mStartIndicatorRow]];
        ImageList* imageList = [mViewNav getImageListBySeq:[NSNumber numberWithInt:mEndIndicatorRow]];
        if([mViewNav getMusicListByImageList: imageList.name] == nil)
        {
            //[attachSet addObject:imageList];
            [musicList addAttachimagelistObject:imageList];
            [self drawAttach];
            [mMusicListTableView reloadData];
            [mImageListTableView reloadData];
            [mViewNav saveContext];
        }
    }
    else
        assert(0);
    
    
        
}
- (RowData) getIndexPathOnPoint: (CGPoint)p
{
    CGPoint musicTablePoint = [self convertPoint:p toView:mMusicListTableView];
    CGPoint imageTablePoint = [self convertPoint:p toView:mImageListTableView];
    NSIndexPath* indexPath = [mMusicListTableView indexPathForRowAtPoint:musicTablePoint];
    //NSInteger row = indexPath.row;
    RowData rd;
    rd.indexPath = nil;
    rd.tableType = NO_TABLE;
    if(indexPath == nil)
    {
        indexPath = [mImageListTableView indexPathForRowAtPoint:imageTablePoint];
        if(indexPath)
        {
            rd.tableType = IMAGE_TABLE;
            rd.indexPath = indexPath;
        }
    }
    else
    {
        rd.tableType = MUSIC_TABLE;
        rd.indexPath = indexPath;
    }
    return rd;
}
- (void)setSelectedItem: (NSString*) name type: (int)type
{
    mCurrentName = name;
    mTableType = type;
}
- (void)dismissPopup
{
    [mPopup dismissPopoverAnimated:YES];
    mPopup = nil;
    mDlg = nil;
}
- (void) handleOK:(id)sender
{
    NSString* text = mDlg.content.text;
    if([text isEqualToString:@""])
    {
        mAlertType = NO_ALERT;
        [self dismissPopup];
        return;
    }
    if(mAlertType == ADD_MUSIC_LIST)
    {
        MusicList* musicList = [mViewNav getMusicListByName:text];
        if(musicList == nil)
        {
            NSString* name = text;
            [mViewNav addMusicList:name];
            mMusicListTableView.mMusicListPropertyArray = [mViewNav getAllMusicList];
            NSIndexPath* indexPath = [NSIndexPath indexPathForRow:mMusicListTableView.mMusicListPropertyArray.count - 1 inSection:0];
            [mMusicListTableView reloadData];
            [mMusicListTableView scrollToRowAtIndexPath:indexPath atScrollPosition:UITableViewScrollPositionBottom animated:YES];
            [self dismissPopup];
            [mViewNav saveContext];
        }
        else
        {
            mDlg.errorMsg.text = @"has the same name, please input different name";
            int seq = [musicList.seq intValue];
            NSIndexPath* indexPath = [NSIndexPath indexPathForRow:seq inSection:0];
            [mMusicListTableView scrollToRowAtIndexPath:indexPath atScrollPosition:UITableViewScrollPositionTop animated:YES];
            mDlg.errorMsg.textColor = [UIColor redColor];
        }
    }
    else if(mAlertType == ADD_IMAGE_LIST)
    {
        ImageList* imageList = [mViewNav getImageListByName:text];
        if(imageList == nil)
        {
            NSString* name = text;
            imageList = [mViewNav addImageList:name];
            //ImageList* imageList = [mViewNav getImageListByName:name];
            UserInfo* userInfo = [mViewNav getUserInfo];
            NSNumber* levelNum = userInfo.level;
            int levelImageNum = [levelNum intValue];
            for(int i = 0 ; i < levelImageNum ; i++)
            {
                SelectedImage* si = (SelectedImage*)[mViewNav newObjectByEntityName:@"SelectedImage"];
                NSNumber* seq = [NSNumber numberWithInt:i];
                si.seq = seq;
                [imageList addSelectedimageObject:si];
            }
            mImageListTableView.mImageListPropertyArray = [mViewNav getAllImageList];
            NSIndexPath* indexPath = [NSIndexPath indexPathForRow:mImageListTableView.mImageListPropertyArray.count - 1 inSection:0];
            [mImageListTableView reloadData];
            [mImageListTableView scrollToRowAtIndexPath:indexPath atScrollPosition:UITableViewScrollPositionBottom animated:YES];
            [self dismissPopup];
            [mViewNav saveContext];
        }
        else
        {
            mDlg.errorMsg.text = @"has the same name, please input different name";
            int seq = [imageList.seq intValue];
            NSIndexPath* indexPath = [NSIndexPath indexPathForRow:seq inSection:0];
            [mImageListTableView scrollToRowAtIndexPath:indexPath atScrollPosition:UITableViewScrollPositionTop animated:YES];
            mDlg.errorMsg.textColor = [UIColor redColor];
        }
        
    }
    
    mAlertType = NO_ALERT;
}
- (void) handleCancel:(id)sender
{
    mAlertType = NO_ALERT;
    [self dismissPopup];
}
- (void) addMusicListButtonHandler: (id)sender
{
    NSLog(@"add music list\n");
    SEMusicImageListPopup* dlg = [[SEMusicImageListPopup alloc] init];
    dlg.modalInPopover = YES;
    UIPopoverController* pop = [[UIPopoverController alloc] initWithContentViewController:dlg];
    pop.popoverContentSize = CGSizeMake(320, 200);
    [dlg release];
    mPopup = pop;
    [dlg.okButton addTarget:self action:@selector(handleOK:) forControlEvents:UIControlEventTouchUpInside];
    [dlg.cancelButton addTarget:self action:@selector(handleCancel:) forControlEvents:UIControlEventTouchUpInside];
    dlg.title.text = @"input music list name";
    
    mDlg = dlg;
    mAlertType = ADD_MUSIC_LIST;
    [pop presentPopoverFromRect:CGRectMake(500, 0, 0, 0) inView:self permittedArrowDirections: UIPopoverArrowDirectionAny animated:YES];
    /*
    UIAlertView* alertView = [[UIAlertView alloc] initWithTitle:@"add new music list name" message:@"HgLL"delegate:self cancelButtonTitle:@"OK" otherButtonTitles:@"Cancel", nil];

    CGRect rect = alertView.bounds;
    NSArray* array = alertView.subviews;
    [alertView show];
    [alertView release];     
    NSLog(@"rect = %f, %f, %f, %f, %d", rect.origin.x, rect.origin.y, rect.size.width, rect.size.height, array.count);
    for(UIView* v  in array)
    {
        NSLog(@"view = %@\n", v);
    }
    UITextField* textField = [[UITextField alloc] initWithFrame:CGRectMake(12, 45, 260, 25)];
    textField.backgroundColor = [UIColor whiteColor];
    textField.tag = 204;
    [alertView addSubview:textField];
    [textField release];
    mAlertType = ADD_MUSIC_LIST;
     */
    /*
    NSArray* musicListArray = [mViewNav getAllMusicList];
    NSString* name = [NSString stringWithFormat:@"%@-%d", @"unknown", musicListArray.count];
    
    [mViewNav addMusicList:name];
    mMusicListTableView.mMusicListPropertyArray = [mViewNav getAllMusicList];
    NSIndexPath* indexPath = [NSIndexPath indexPathForRow:mMusicListTableView.mMusicListPropertyArray.count - 1 inSection:0];
    [mMusicListTableView reloadData];
    [mMusicListTableView scrollToRowAtIndexPath:indexPath atScrollPosition:UITableViewScrollPositionBottom animated:YES];
     */
}

- (void) addImageListButtonHandler: (id)sender
{
    NSLog(@"add image list \n");
    SEMusicImageListPopup* dlg = [[SEMusicImageListPopup alloc] init];
    dlg.modalInPopover = YES;
    UIPopoverController* pop = [[UIPopoverController alloc] initWithContentViewController:dlg];
    pop.popoverContentSize = CGSizeMake(320, 200);
    [dlg release];
    mPopup = pop;
    [dlg.okButton addTarget:self action:@selector(handleOK:) forControlEvents:UIControlEventTouchUpInside];
    [dlg.cancelButton addTarget:self action:@selector(handleCancel:) forControlEvents:UIControlEventTouchUpInside];
    dlg.title.text = @"input image list name";
    
    mDlg = dlg;
    mAlertType = ADD_IMAGE_LIST;
    [pop presentPopoverFromRect:CGRectMake(500, 0, 0, 0) inView:self permittedArrowDirections: UIPopoverArrowDirectionAny animated:YES];
}
-(void) removeButtonHandler: (id)sender
{
    NSLog(@"remove list\n");
    if(mTableType == NO_TABLE)
        return;
    UserInfo* userInfo = [mViewNav getUserInfo];
    if(mTableType == MUSIC_TABLE)
    {
        BOOL imageReload = mLineViewArray.count > 0;
        [self clearLineView];
        NSString* currentName = userInfo.currentmusiclist;
        MusicList* ml = [mViewNav getMusicListByName:currentName];
        int seq = [ml.seq intValue];
        [mViewNav removeMusicListByName:currentName];
        NSArray* allMusicList = [mViewNav getAllMusicList];
        if(seq < allMusicList.count)
        {
            ml = [allMusicList objectAtIndex:seq];
        }
        else
            ml = [allMusicList lastObject];
        currentName = ml.name;
        userInfo.currentmusiclist = ml.name;
        mMusicListTableView.mMusicListPropertyArray = [mViewNav getAllMusicList];
        [mMusicListTableView reloadData];
        if(imageReload)
            [mImageListTableView reloadData];
    }
    else if(mTableType == IMAGE_TABLE)
    {
        BOOL musicReload = mLineViewArray.count > 0;
        [self clearLineView];
        NSString* currentName = userInfo.currentimagelist;
        ImageList* il = [mViewNav getImageListByName:currentName];
        int seq = [il.seq intValue];
        [mViewNav removeImageListByName:currentName];
        NSArray* allImageList = [mViewNav getAllImageList];
        if(seq < allImageList.count)
        {
            il = [allImageList objectAtIndex:seq];
        }
        else
        {
            il = [allImageList lastObject];
        }
        userInfo.currentimagelist = il.name;
        mImageListTableView.mImageListPropertyArray = [mViewNav getAllImageList];
        [mImageListTableView reloadData];
        if(musicReload)
            [mMusicListTableView reloadData];
    }
    [mViewNav saveContext];
    mTableType = NO_TABLE;
    mCurrentName = nil;
}
@end
///////
@implementation SEIndicatorView
@synthesize mTouchHandler;
@synthesize mResLoader;
@synthesize mType;
- (void) setIndicationImage: (UITouchPhase)phase
{
    if(phase == UITouchPhaseBegan)
    {
        if(mType == IMAGE_INDICATION)
            self.image = [mResLoader getImage:@"MusicImageImageListAttachedIndicator"];
        else if(mType == MUSIC_INDICATION)
            self.image = [mResLoader getImage:@"MusicImageListIndicatorMusicAttatchedIcon"];
    }
    else if(phase == UITouchPhaseCancelled || phase == UITouchPhaseEnded)
    {
        if(mType == IMAGE_INDICATION)
            self.image = [mResLoader getImage:@"MusicImageImageListNormalIndicator"];
        else if(mType == MUSIC_INDICATION)
            self.image = [mResLoader getImage:@"MusicImageListIndicatorMusicNormalIcon"];
    }
}
- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"indicator touches began count = %u", [touches count]);
    NSArray* array = [touches allObjects];
    mSavedGesture = [NSArray array];
    for(UITouch* touch in array)
    {
        NSArray* gesArr = touch.gestureRecognizers;
        for(UIGestureRecognizer* ges in gesArr)
        {
            NSLog(@"ges = %@", ges);
            mSavedGesture = [mSavedGesture arrayByAddingObject:ges];
            ges.enabled = NO;
        }
    }
    [mSavedGesture retain];
    //[self setIndicationImage:UITouchPhaseBegan];
    CGPoint p = [[touches anyObject] locationInView:self];
    CGPoint loc = [self convertPoint:p toView:mTouchHandler];
    [mTouchHandler touchBegan:loc];
    //[super touchesBegan:touches withEvent:event];
}
- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"indicator touches move count = %u", [touches count]);
    CGPoint p = [[touches anyObject] locationInView:self];
    CGPoint loc = [self convertPoint:p toView:mTouchHandler];
    [mTouchHandler touchMove:loc];
    //[super touchesMoved:touches withEvent:event];
}
- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"indicator touches end count = %u", [touches count]);
    for(UIGestureRecognizer* ges in mSavedGesture)
    {
        ges.enabled = YES;
    }
    [mSavedGesture release];
    mSavedGesture = nil;
    CGPoint p = [[touches anyObject] locationInView:self];
    CGPoint loc = [self convertPoint:p toView:mTouchHandler];
    //[self setIndicationImage:UITouchPhaseEnded];
    [mTouchHandler touchEnd:loc];
    //[super touchesEnded: touches withEvent:event];
}
- (void)touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"indicator touch cancel");
    for(UIGestureRecognizer* ges in mSavedGesture)
    {
        ges.enabled = YES;
    }
    [mSavedGesture release];
    mSavedGesture = nil;
    //CGPoint p = [[touches anyObject] locationInView:self];
    //CGPoint loc = [self convertPoint:p toView:mTouchHandler];
    //[mTouchHandler touchEnd:loc];
    //[self setIndicationImage:UITouchPhaseCancelled];
    //[super touchesCancelled:touches withEvent:event];
}

@end
//////////////////////////////////////////
@implementation SEImageListTableView
@synthesize mViewNav;
@synthesize mImageListPropertyArray;
@synthesize mResLoader;
@synthesize mMusicImageListView;
- (void) dealloc
{
    [mImageListPropertyArray release];
    [super dealloc];
}
- (void) highlightCell: (NSIndexPath*)indexPath
{
    UITableViewCell* cell = [self cellForRowAtIndexPath:indexPath];
    UIImageView* fullBg = (UIImageView*)[cell viewWithTag:110];
    fullBg.image = [UIImage imageNamed:@"list_said_001.png"];
}
- (void)scrollViewDidScroll:(UIScrollView *)scrollView
{
    [mMusicImageListView scrollHandler: scrollView.contentOffset tableType:IMAGE_TABLE];
}
- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    return 1;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    int count = [mImageListPropertyArray count];
    return count;
}
- (CGFloat)tableView:(UITableView *)tableView heightForRowAtIndexPath:(NSIndexPath *)indexPath
{
    UITableViewCell* cell = [tableView dequeueReusableCellWithIdentifier:@"BaseCell"];
    if(cell == nil)
    {
        cell = [[[NSBundle mainBundle] loadNibNamed:@"ImageListItem" owner:self options:nil] lastObject];
    }
    return cell.frame.size.height;
}
// Customize the appearance of table view cells.
- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    UITableViewCell* cell = [tableView dequeueReusableCellWithIdentifier:@"BaseCell"];
    if(cell == nil)
    {
        cell = [[[NSBundle mainBundle] loadNibNamed:@"ImageListItem" owner:self options:nil] lastObject];
    }
    cell.tag = indexPath.row;
    
    UIImageView* fullBg = (UIImageView*)[cell viewWithTag:110];
    [fullBg retain];
    UIView* superview = fullBg.superview;
    [fullBg removeFromSuperview];
    [superview insertSubview:fullBg atIndex:0];
    [fullBg release];

    
    UIImageView* background = (UIImageView*)[cell viewWithTag:101];
    BOOL hasAttached = [mMusicImageListView hasAttachedToMusicList: indexPath.row];
    
    ImageList* il = [mViewNav getImageListBySeq:[NSNumber numberWithInt:indexPath.row]];
    UserInfo* userInfo = [mViewNav getUserInfo];
    BOOL isSelectedView = [il.name isEqualToString:userInfo.currentimagelist];
    if(isSelectedView)
    {
        background.image = [mResLoader getImage:@"MusicImageImageListItemAttachedBackground"];
    }
    else
    {
        background.image = [mResLoader getImage:@"MusicImageImageListItemNormalBackground"];
    }
    SEIndicatorView* indicator = (SEIndicatorView*)[cell viewWithTag:102];
    UIImageView* imageAlbum = (UIImageView*)[cell viewWithTag:103];
    UIImageView* titleBackground = (UIImageView*)[cell viewWithTag:104];
    UIImageView* numberTitleBackground = (UIImageView*)[cell viewWithTag:105];
    UIImageView* numberBackground = (UIImageView*)[cell viewWithTag:106];
    UILabel* title = (UILabel*)[cell viewWithTag:107];
    UILabel* numberTitle = (UILabel*)[cell viewWithTag:108];
    UILabel* number = (UILabel*)[cell viewWithTag:109];
    
    imageAlbum.tag = indexPath.row;
    imageAlbum.userInteractionEnabled = YES;
    UITapGestureRecognizer* ges = [[UITapGestureRecognizer alloc] initWithTarget:mMusicImageListView action:@selector(handleImageClick:)];
    [imageAlbum addGestureRecognizer:ges];
    [ges release];
    
    indicator.mType = IMAGE_INDICATION;
    indicator.userInteractionEnabled = YES;
    indicator.mTouchHandler = mMusicImageListView;
    indicator.mResLoader = mResLoader;
    if(hasAttached)
    {
        indicator.image = [mResLoader getImage:@"MusicImageImageListAttachedIndicator"];
    }
    else
    {
        indicator.image = [mResLoader getImage:@"MusicImageImageListNormalIndicator"];
    }
    
    BOOL hasImage = NO;
    if(hasImage)
    {
        imageAlbum.image = [mResLoader getImage:@""];
    }
    else
    {
        imageAlbum.image = [mResLoader getImage:@"MusicImageImageListNoImageIcon"];
    }
    
    if(hasAttached)
    {
        titleBackground.image = [mResLoader getImage:@"MusicImageImageListItemAttachedNameImage"];
    }
    else
    {
        titleBackground.image = [mResLoader getImage:@"MusicImageImageListItemNormalNameImage"];
    }
    if(hasAttached)
    {
        numberTitleBackground.image = [mResLoader getImage:@"MusicImageImageListItemAttachedNumberTitleImage"];
    }
    else
        numberTitleBackground.image = [mResLoader getImage:@"MusicImageImageListItemNormalNumberTitleImage"];
    
    if(hasAttached)
    {
        numberBackground.image = [mResLoader getImage:@"MusicImageImageListItemAttachedNumberImage"];
    }
    else
        numberBackground.image = [mResLoader getImage:@"MusicImageImageListItemNormalNumberImage"];
    
    ImageList* imageList = [mViewNav getImageListBySeq:[NSNumber numberWithInt:indexPath.row]];
    NSString* name = imageList.name;
    int count = imageList.selectedimage.count;
    title.text = name;
    numberTitle.text = @"count";
    number.text = [NSString stringWithFormat:@"%d", count];
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
    SEMusicListTableView* musicTableView = mMusicImageListView.mMusicListTableView;
    UserInfo* userInfo = [mViewNav getUserInfo];
    ImageList* currentImageList = [mViewNav getImageListByName:userInfo.currentimagelist];
    if([currentImageList.seq intValue] == indexPath.row)
        return;
    [musicTableView deselectMusicCellForImageList: currentImageList];
    [musicTableView deselectHighlightedCell];
    [self deselectImageList:currentImageList];
    ImageList* il = [mViewNav getImageListBySeq:[NSNumber numberWithInt:indexPath.row]];
    [mMusicImageListView setSelectedItem:il.name type:IMAGE_TABLE];
    [mMusicImageListView clearLineView];
    [mMusicImageListView drawAttachFrom: indexPath.row tableType: IMAGE_TABLE offset:tableView.contentOffset];
    [self deselectImageList:il];
    [self highlightCell:indexPath];
    userInfo.currentimagelist = il.name;
    
    MusicList* ml = [mViewNav getMusicListByImageList:il.name];
    userInfo.currentmusiclist = ml.name;
    [musicTableView selectMusicCellForImageList: il];
    [mViewNav saveContext];
    [mMusicImageListView.mImageListTableView selectImageCell:indexPath];
}
- (void) deselectImageList: (ImageList*)imageList
{
    NSArray* visibleCells = self.visibleCells;
    for(UITableViewCell* cell in visibleCells)
    {
        if(cell.tag == [imageList.seq intValue])
        {
            [self deselectImageCell:[NSIndexPath indexPathForRow:cell.tag inSection:0]];
            [self dehighlightImageCell:[NSIndexPath indexPathForRow:cell.tag inSection:0]];
        }
    }
}
- (UIImageView*) tableViewCellBackground: (NSIndexPath*)indexPath
{
    UITableViewCell* cell = [self cellForRowAtIndexPath:indexPath];
    UIImageView* background = (UIImageView*)[cell viewWithTag:101];
    return background;
}
- (void) deselectImageCell: (NSIndexPath*)indexPath
{
    UIImageView* background = [self tableViewCellBackground:indexPath];
    background.image = [mResLoader getImage:@"MusicImageImageListItemNormalBackground"];    
}
- (void) selectImageCell: (NSIndexPath*)indexPath
{

    UIImageView* background = [self tableViewCellBackground:indexPath];
    background.image = [mResLoader getImage:@"MusicImageImageListItemAttachedBackground"];    
}
- (void) dehighlightImageCell: (NSIndexPath*)indexPath
{
    UITableViewCell* cell = [self cellForRowAtIndexPath:indexPath];
    UIImageView* fullBg = (UIImageView*)[cell viewWithTag:110];
    fullBg.image = nil;
}
@end

@implementation SEMusicListTableView
@synthesize mViewNav;
@synthesize mMusicListPropertyArray;
@synthesize mResLoader;
@synthesize mMusicImageListView;
- (void) dealloc
{
    [mMusicListPropertyArray release];
    [super dealloc];
}
- (void) selectMusicCellForImageList: (ImageList*)imageList
{
    MusicList* ml = [mViewNav getMusicListByImageList:imageList.name];
    if(ml == nil)
        return;
    NSArray* visibleCells= [self visibleCells];
    
    for(UITableViewCell* cell in visibleCells)
    {
        if(cell.tag == [ml.seq intValue])
        {
            UIImageView* background = (UIImageView*)[cell viewWithTag:106];
            background.image = [mResLoader getImage:@"MusicImageMusicListItemAttachedBackground"];
        }
    }

}
- (UIImageView*) tableViewCellBackground: (UITableViewCell*)cell
{
    return (UIImageView*)[cell viewWithTag:106];
}
- (void) deselectMusicCellForImageList: (ImageList*)imageList
{
    MusicList* ml = [mViewNav getMusicListByImageList:imageList.name];
    if(ml == nil)
        return;
    NSArray* visibleCells= [self visibleCells];
    for(UITableViewCell* cell in visibleCells)
    {
        if(cell.tag == [ml.seq intValue])
        {
            UIImageView* background = [self tableViewCellBackground:cell];
            background.image = [mResLoader getImage:@"MusicImageMusicListItemNormalBackground"];
        }
    }
}
- (void) deselectHighlightedCell
{
    NSIndexPath* musicIndexPath = [self indexPathForSelectedRow];
    [self deselectRowAtIndexPath:musicIndexPath animated:NO];
    
    UITableViewCell* cell = [self cellForRowAtIndexPath:musicIndexPath];
    UIImageView* fullBg = (UIImageView*)[cell viewWithTag:110];
    fullBg.image = nil;
}
- (void) deselectPrevRow : (UITableView*)tableView
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    NSString* musicListName = userInfo.currentmusiclist;
    MusicList* musicList = [mViewNav getMusicListByName:musicListName];
    NSArray* visibleCells= [tableView visibleCells];
    for(UITableViewCell* cell in visibleCells)
    {
        if(cell.tag == [musicList.seq intValue])
        {
            UIImageView* fullBg = (UIImageView*)[cell viewWithTag:110];
            fullBg.image = nil;
        }
    }
    
}
 
- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    return 1;
}
- (void)scrollViewDidScroll:(UIScrollView *)scrollView
{
    [mMusicImageListView scrollHandler: scrollView.contentOffset tableType:MUSIC_TABLE];
}
- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    int count = [mMusicListPropertyArray count];
    return count;
}

// Customize the appearance of table view cells.
- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    UITableViewCell* cell = [tableView dequeueReusableCellWithIdentifier:@"BaseCell"];
    if(cell == nil)
    {
        cell = [[[NSBundle mainBundle] loadNibNamed:@"MusicListItem" owner:self options:nil] lastObject];
    }
    int row = indexPath.row;
    cell.tag = row;
    UIImageView* fullBg = (UIImageView*)[cell viewWithTag:110];
    [fullBg retain];
    UIView* superview = fullBg.superview;
    [fullBg removeFromSuperview];
    [superview insertSubview:fullBg atIndex:0];
    [fullBg release];

    SEIndicatorView* indicator = (SEIndicatorView*)[cell viewWithTag:101];
    UIImage* indicatorImage = nil;
    MusicList* ml = [mViewNav getMusicListBySeq:[NSNumber numberWithInt:row]];
    assert(ml);
    NSSet* attachImage = ml.attachimagelist;
    BOOL hasAttachedImageList = attachImage.count > 0;
    if(hasAttachedImageList > 0)
    {
        indicatorImage = [mResLoader getImage:@"MusicImageListIndicatorMusicAttachedIcon"];
    }
    else
    {
        indicatorImage = [mResLoader getImage:@"MusicImageListIndicatorMusicNormalIcon"];
    }
    indicator.mType = MUSIC_INDICATION;
    indicator.image = indicatorImage;
    indicator.userInteractionEnabled = YES;
    indicator.mTouchHandler = mMusicImageListView;
    indicator.mResLoader = mResLoader;
    UIImageView* musicAlbumView = (UIImageView*)[cell viewWithTag:102];
    
    musicAlbumView.tag = indexPath.row;
    musicAlbumView.userInteractionEnabled = YES;
    UITapGestureRecognizer* ges = [[UITapGestureRecognizer alloc] initWithTarget:mMusicImageListView action:@selector(handleMusicClick:)];
    [musicAlbumView addGestureRecognizer:ges];
    [ges release];
    
    BOOL hasAlbum = NO;
    if(hasAlbum)
    {
        musicAlbumView.image = [mResLoader getImage:@""];
    }
    else
    {
        musicAlbumView.image = [mResLoader getImage:@"MusicImageMusicNoAlbumIcon"];
    }
    UIImageView* listNameBackgroundView = (UIImageView*)[cell viewWithTag:103];
    if(hasAttachedImageList)
    {
        listNameBackgroundView.image = [mResLoader getImage:@"MusicImageMusicListItemAttachedNameImage"];
    }
    else
    {
        listNameBackgroundView.image = [mResLoader getImage:@"MusicImageMusicListItemNormalNameImage"];
    }
    UIImageView* listNumberTitleBackgroundView = (UIImageView*)[cell viewWithTag:104];
    if(hasAttachedImageList)
    {
        listNumberTitleBackgroundView.image = [mResLoader getImage:@"MusicImageMusicListItemAttachedNumberTitleImage"];
        
    }
    else
    {
        listNumberTitleBackgroundView.image = [mResLoader getImage:@"MusicImageMusicListItemNormalNumberTitleImage"];
    }
    UIImageView* listNumberBackgroundView = (UIImageView*)[cell viewWithTag:105];
    if(hasAttachedImageList)
    {
        listNumberBackgroundView.image = [mResLoader getImage:@"MusicImageMusicListItemAttachedNumberImage"];
    }
    else
    {
        listNumberBackgroundView.image = [mResLoader getImage:@"MusicImageMusicListItemNormalNumberImage"];
    }
    
    UILabel* musicListName = (UILabel*)[cell viewWithTag:107];
    musicListName.text = ml.name;
    UILabel* musicNumberTitle = (UILabel*)[cell viewWithTag:108];
    musicNumberTitle.text = @"count";
    UILabel* musicNumber = (UILabel*)[cell viewWithTag:109];
    musicNumber.text = [NSString stringWithFormat:@"%d", ml.selectedmusic.count];
    UIImageView* background = (UIImageView*)[cell viewWithTag:106];
    UserInfo* userInfo = [mViewNav getUserInfo];
    ImageList* currentImageList = [mViewNav getImageListByName:userInfo.currentimagelist];
    MusicList* attachedMusicList = [mViewNav getMusicListByImageList:currentImageList.name];
    BOOL isSelected = [ml.name isEqualToString:attachedMusicList.name];
    if(isSelected)
    {
        background.image = [mResLoader getImage:@"MusicImageMusicListItemAttachedBackground"];
    }
    else
    {
        background.image = [mResLoader getImage:@"MusicImageMusicListItemNormalBackground"];
    }
    return cell;
}
- (CGFloat)tableView:(UITableView *)tableView heightForRowAtIndexPath:(NSIndexPath *)indexPath
{
    UITableViewCell* cell = [tableView dequeueReusableCellWithIdentifier:@"BaseCell"];
    if(cell == nil)
    {
        cell = [[[NSBundle mainBundle] loadNibNamed:@"MusicListItem" owner:self options:nil] lastObject];
    }
    return cell.frame.size.height;
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
- (void)highlightCell: (NSIndexPath*) indexPath
{
    UITableViewCell* cell = [self cellForRowAtIndexPath:indexPath];
    UIImageView* fullBg = (UIImageView*)[cell viewWithTag:110];
    fullBg.image = [UIImage imageNamed:@"list_said.png"];
}
- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath
{
    SEImageListTableView* imageTableView = mMusicImageListView.mImageListTableView;
    NSIndexPath* imageIndexPath = [imageTableView indexPathForSelectedRow];
    //[imageTableView deselectRowAtIndexPath:imageIndexPath animated:NO];
    [imageTableView dehighlightImageCell: imageIndexPath];
    [self deselectPrevRow:self];
    MusicList* ml = [mViewNav getMusicListBySeq:[NSNumber numberWithInt:indexPath.row]];
    //[mMusicImageListView setSelectedItem:ml.name type:MUSIC_TABLE];
    //[mMusicImageListView clearLineView];
    //[mMusicImageListView drawAttachFrom: indexPath.row tableType: MUSIC_TABLE offset:tableView.contentOffset];
    [self highlightCell: indexPath];
    UserInfo* userInfo = [mViewNav getUserInfo];
    userInfo.currentmusiclist = ml.name;
    [mViewNav saveContext];
}

@end
@implementation SEMusicImageListView
@synthesize mViewNav;
@synthesize mBackground;
@synthesize mResLoader;
@synthesize mMusicListTableView;
@synthesize mImageListTableView;
- (void) initMusicImageTableView
{
    mImageListTableView.mResLoader = mResLoader;
    //mImageListTableView.separatorColor = [UIColor clearColor];
    UIImageView* imageView = [[UIImageView alloc] init];
    imageView.image = mImageListBackground;
    mImageListTableView.backgroundView = imageView;
    [imageView release];
    mImageListTableView.separatorStyle = UITableViewCellSeparatorStyleNone;
    //mImageListTableView.backgroundView = nil;
    mImageListTableView.dataSource = mImageListTableView;
    mImageListTableView.delegate = mImageListTableView;
    mImageListTableView.mViewNav = mViewNav;
    mImageListTableView.mImageListPropertyArray = [mViewNav getAllImageList];
    mImageListTableView.mMusicImageListView = self;
    mImageListTableView.delegate = mImageListTableView;
    UserInfo* userInfo = [mViewNav getUserInfo];
    NSString* imageListName = userInfo.currentimagelist;
    ImageList* imageList = [mViewNav getImageListByName:imageListName];
    NSIndexPath* indexPath = [NSIndexPath indexPathForRow:[imageList.seq intValue] inSection:0];
    [mImageListTableView selectRowAtIndexPath:indexPath animated:NO scrollPosition:UITableViewScrollPositionTop];
    
    mMusicListTableView.mResLoader = mResLoader;
    UIImageView* musicView = [[UIImageView alloc] init];
    musicView.image = mMusicListBackground;
    mMusicListTableView.backgroundView = musicView;
    mMusicListTableView.dataSource = mMusicListTableView;
    mMusicListTableView.delegate = mMusicListTableView;
    mMusicListTableView.mViewNav = mViewNav;
    mMusicListTableView.mMusicListPropertyArray = [mViewNav getAllMusicList]; 
    mMusicListTableView.mMusicImageListView = self;
    mMusicListTableView.delegate = mMusicListTableView;
    mMusicListTableView.separatorStyle = UITableViewCellSeparatorStyleNone;
    /*
    MusicList* musicList = [mViewNav getMusicListByImageList:imageListName];
    if(musicList)
    {
        indexPath = [NSIndexPath indexPathForRow:[musicList.seq intValue] inSection:0];
        [mMusicListTableView selectRowAtIndexPath:indexPath animated:NO scrollPosition:UITableViewScrollPositionTop];
    }
     */
     
}
- (void) initMusicImageBackground
{
    CGRect musicFrame = mMusicListTableView.frame;
    CGRect imageFrame = mImageListTableView.frame;
    UIGraphicsBeginImageContext(musicFrame.size);
    [mBackground drawAtPoint:CGPointMake(-musicFrame.origin.x, -musicFrame.origin.y)];
    mMusicListBackground = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    
    UIGraphicsBeginImageContext(imageFrame.size);
    [mBackground drawAtPoint:CGPointMake(-imageFrame.origin.x, -imageFrame.origin.y)];
    mImageListBackground = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
}
- (CGPoint) getIndicatorPointInCell: (UITableViewCell*) cell tableType:(int) tableType
{
    UIView*  v = nil;
    CGPoint p;
    if(tableType == IMAGE_TABLE)
    {
        v = [cell viewWithTag:102];
        p = mImageIndicatorCenter;
    }
    else if(tableType == MUSIC_TABLE)
    {
        v = [cell viewWithTag:101];
        p = mMusicIndicatorCenter;
    }
    CGRect frame = v.frame;
    return  CGPointMake(frame.origin.x + p.x, frame.origin.y + p.y); 
}
- (void) initIndicatorData
{
    UITableViewCell* imageCell = [[[NSBundle mainBundle] loadNibNamed:@"ImageListItem" owner:self options:nil] lastObject];
    UITableViewCell* musicCell = [[[NSBundle mainBundle] loadNibNamed:@"MusicListItem" owner:self options:nil] lastObject];
    CGPoint imageIndicatorPoint = [self getIndicatorPointInCell:imageCell tableType:IMAGE_TABLE];
    CGPoint musicIndicatorPoint = [self getIndicatorPointInCell:musicCell tableType:MUSIC_TABLE];
    CGFloat imageCellH = imageCell.frame.size.height;
    CGFloat musicCellH = musicCell.frame.size.height;
    mImageIndicatorPointInCell = imageIndicatorPoint;
    mMusicIndicatorPointInCell = musicIndicatorPoint;
    mImageCellHeight = imageCellH;
    mMusicCellHeight = musicCellH;
}
- (void) initLineViewData
{
    mLineViewArray = [NSMutableArray array];
    [mLineViewArray retain];
    [self initIndicatorData];
    UserInfo* userInfo = [mViewNav getUserInfo];
    NSString* imageListName = userInfo.currentimagelist;
    ImageList* imageList = [mViewNav getImageListByName:imageListName];
    MusicList* musicList  =[ mViewNav getMusicListByImageList:imageListName];
    if(musicList)
    {
        int imageIndex = [imageList.seq intValue];
        int musicIndex = [musicList.seq intValue];
        LineViewData* lvd = [[LineViewData alloc] init];
        CGPoint p1 = CGPointMake(mImageIndicatorPointInCell.x, mImageIndicatorPointInCell.y + imageIndex
                                 * mImageCellHeight);
        CGPoint p2 = CGPointMake(mMusicIndicatorPointInCell.x, mMusicIndicatorPointInCell.y + mMusicCellHeight * musicIndex);
        p1 = [mImageListTableView convertPoint:p1 toView:self];
        p2 = [mMusicListTableView convertPoint:p2 toView:self];
        lvd.mStartPoint = p1;
        lvd.mEndPoint = p2;
        lvd.mStartIndicatorTableType = IMAGE_TABLE;
        lvd.mEndIndicatorTableType = MUSIC_TABLE;
        [mLineViewArray addObject:lvd];
        [lvd release];
        UIView* v = [self createLineView:p1 :p2];
        lvd.mView = v;
        [self addSubview:v];
    }
    /*
    NSArray* musicArray = [mViewNav getAllMusicList];
    UITableViewCell* imageCell = [[[NSBundle mainBundle] loadNibNamed:@"ImageListItem" owner:self options:nil] lastObject];
    UITableViewCell* musicCell = [[[NSBundle mainBundle] loadNibNamed:@"MusicListItem" owner:self options:nil] lastObject];
    CGPoint imageIndicatorPoint = [self getIndicatorPointInCell:imageCell tableType:IMAGE_TABLE];
    CGPoint musicIndicatorPoint = [self getIndicatorPointInCell:musicCell tableType:MUSIC_TABLE];
    CGFloat imageCellH = imageCell.frame.size.height;
    CGFloat musicCellH = musicCell.frame.size.height;
    for(MusicList* ml in musicArray)
    {
        NSSet* attachedImage = ml.attachimagelist;
        for(ImageList* il in attachedImage)
        {
            LineViewData* lvd = [[LineViewData alloc] init];
            lvd.mStartIndicatorRow = [il.seq intValue];
            lvd.mStartIndicatorTableType = IMAGE_TABLE;
            lvd.mEndIndicatorRow = [ml.seq intValue];
            lvd.mEndIndicatorTableType = MUSIC_TABLE;
            CGPoint p1 = CGPointMake(imageIndicatorPoint.x, imageIndicatorPoint.y + lvd.mStartIndicatorRow * imageCellH);
            CGPoint p2 = CGPointMake(musicIndicatorPoint.x, musicIndicatorPoint.y + musicCellH * lvd.mEndIndicatorRow);
            p1 = [mImageListTableView convertPoint:p1 toView:self];
            p2 = [mMusicListTableView convertPoint:p2 toView:self];
            lvd.mStartPoint = p1;
            lvd.mEndPoint = p2;
            [mLineViewArray addObject:lvd];
            [lvd release];
        }
    }
     */
}
-(void) initData
{
    mImageIndicatorCenter.x = 68;
    mImageIndicatorCenter.y = 56;
    mMusicIndicatorCenter.x = 21;
    mMusicIndicatorCenter.y = 56;
    mBackgroundView = (UIImageView*)[self viewWithTag:101];
    mImageListTableView = (SEImageListTableView*)[self viewWithTag:102];
    mMusicListTableView = (SEMusicListTableView*)[self viewWithTag:103];
    mAddMusicListButton = (UIButton*) [self viewWithTag:104];
    mAddMusicListButton.backgroundColor = nil;
    UIImage* image = [mResLoader getImage:@"MusicImageListAddMusicListImageNormal"];
    [mAddMusicListButton setBackgroundImage:image forState:UIControlStateNormal];
    image = [mResLoader getImage:@"MusicImageListAddMusicListImageSelected"];
    [mAddMusicListButton setBackgroundImage:image forState:UIControlStateHighlighted];
    
    mRemoveButton = (UIButton*)[self viewWithTag:105];
    image = [mResLoader getImage:@"MusicImageListRemoveImageNormal"];
    [mRemoveButton setBackgroundImage:image forState:UIControlStateNormal];
    image = [mResLoader getImage:@"MusicImageListRemoveImageSelected"];
    [mRemoveButton setBackgroundImage:image forState:UIControlStateHighlighted];
    mAddImageListButton = (UIButton*) [self viewWithTag:106];
    image = [mResLoader getImage:@"MusicImageListAddImageListImageNormal"];
    [mAddImageListButton setBackgroundImage:image forState:UIControlStateNormal];
    image = [mResLoader getImage:@"MusicImageListAddImageListImageSelected"];
    [mAddImageListButton setBackgroundImage:image forState:UIControlStateHighlighted];
    ///////////
    [mAddMusicListButton addTarget:self action:@selector(addMusicListButtonHandler:) forControlEvents:UIControlEventTouchUpInside];
    [mAddImageListButton addTarget:self action:@selector(addImageListButtonHandler:) forControlEvents:UIControlEventTouchUpInside];
    [mRemoveButton addTarget:self action:@selector(removeButtonHandler:) forControlEvents:UIControlEventTouchUpInside];
    [self initMusicImageBackground];
    [self initMusicImageTableView];
    [self initLineViewData];
}
- (void)dealloc
{
    [mLineViewArray release];
    [mBackground release];
    //[mImageListTableView release];
    //[mMusicListTableView release];
    [super dealloc];
}
/*
- (void)drawRect:(CGRect)rect
{
    [mBackground drawInRect:rect];
}
 */
- (void) handlePoint: (CGPoint)p
{

}
- (void) touchBegan: (CGPoint) p
{
    mOrig = p;
    mLineView = [[UIImageView alloc] init];
    mLineView.image = [UIImage imageNamed:@"list_line.png"];
    //mLineView.backgroundColor = [UIColor blueColor];
    RowData rd = [self getIndexPathOnPoint:p];
    if(rd.indexPath)
    {
        mStartIndicatorRow = rd.indexPath.row;
        mStartIndicatorTable = rd.tableType;
    }
    [self addSubview:mLineView];
}
- (void) touchMove: (CGPoint) p
{
    const float PI = 3.14159;
    float deltax = p.x - mOrig.x;
    float deltay = p.y - mOrig.y;
    float theta = atanf(fabsf(deltay) / fabsf(deltax));
    float width = sqrtf(deltax * deltax + deltay * deltay);
    float height = 20;
    float translatex = width / 2;
    [self handlePoint:p];
    NSLog(@"deltax = %f, deltay = %f, theta = %f", deltax, deltay, theta * 180 / PI);
    if(deltax > 0 && deltay > 0)
    {
        
    }
    else if(deltax < 0 && deltay > 0)
    {
        theta = PI - theta;
    }
    else if(deltax < 0 && deltay < 0)
    {
        theta = - (PI - theta);
    }
    else if(deltax > 0 && deltay < 0)
    {
        theta = -theta;
    }
    if(fabsf(deltax) > 5 || fabsf(deltay) > 5)
    {
        mLineView.transform = CGAffineTransformIdentity;
        mLineView.frame = CGRectMake(mOrig.x - width / 2, mOrig.y - height / 2, width, height);
        CGAffineTransform rotate = CGAffineTransformMakeRotation(theta);
        CGAffineTransform translate = CGAffineTransformMakeTranslation(translatex, 0);
        mLineView.transform = CGAffineTransformConcat(translate, rotate);
    }
}
- (void) touchEnd: (CGPoint)p
{
    [mLineView removeFromSuperview];
    [mLineView release];
    mLineView = nil;
    mOrig = CGPointMake(0, 0);
    RowData rd = [self getIndexPathOnPoint:p];
    if(rd.indexPath)
    {
        mEndIndicatorRow = rd.indexPath.row;
        mEndIndicatorTable = rd.tableType;
        [self addAttach];
    }
}

- (void) update
{
    [mMusicListTableView reloadData];
    [mImageListTableView reloadData];
}

- (void)alertView:(UIAlertView *)alertView clickedButtonAtIndex:(NSInteger)buttonIndex
{
    if(buttonIndex == 0)
    {
        if(mAlertType == ADD_MUSIC_LIST)
        {
            UITextField* textField = (UITextField*)[alertView viewWithTag:204];
            NSString* text = textField.text;
            NSArray* musicListArray = [mViewNav getAllMusicList];
            NSString* name = [NSString stringWithFormat:@"%@-%d", @"unknown", musicListArray.count];
            
            [mViewNav addMusicList:name];
            mMusicListTableView.mMusicListPropertyArray = [mViewNav getAllMusicList];
            NSIndexPath* indexPath = [NSIndexPath indexPathForRow:mMusicListTableView.mMusicListPropertyArray.count - 1 inSection:0];
            [mMusicListTableView reloadData];
            [mMusicListTableView scrollToRowAtIndexPath:indexPath atScrollPosition:UITableViewScrollPositionBottom animated:YES];

        }
    }    
}

@end
