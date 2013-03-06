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
#import "SEImageAsyncLoader.h"
#import "SEUserUpgrate.h"
#import "SEPopupViewWidgets.h"
#import "SESystemConfig.h"
#import "PhotoFrameAppDelegate.h"
#import <MediaPlayer/MediaPlayer.h>
/////////
#define LINE_VIEW_HEIGHT 43
enum INDICTION_TYPE {IMAGE_INDICATION, MUSIC_INDICATION};
enum ALERT_TYPE {NO_ALERT, ADD_MUSIC_LIST, ADD_IMAGE_LIST};
enum TABLE_TYPE {NO_TABLE, MUSIC_TABLE, IMAGE_TABLE};
struct RowData
{
    NSIndexPath* indexPath;
    int tableType;
};
//////////////////////////////
static NSArray* getMusicInMusicList(MusicList* musicList)
{
    NSSet* selectedMusic = musicList.selectedmusic;
    NSArray* retArray = [selectedMusic allObjects];
    retArray = [retArray sortedArrayUsingComparator:^NSComparisonResult(id obj1, id obj2) {
        SelectedMusic* sm1 = (SelectedMusic*)obj1;
        SelectedMusic* sm2 = (SelectedMusic*)obj2;
        return [sm1.seq compare:sm2.seq];
    }];
    return retArray;
}
static NSString* createNextName(NSString* prefix, NSArray* currentNameArray, int count)
{
    return [SEUtil createNextName:prefix :currentNameArray :count];
}
//////////////////////////////
@implementation SEMusicItem
@synthesize play;
@synthesize row;
@synthesize currSeq;
@synthesize musicListName;
- (void) dealloc
{
    [musicListName release];
    [super dealloc];
}
@end
////////////////////////////
@interface SEImageViewForImageList : UIView
{
    UIImage* mContentImage;
    UIImage* mFrameImage;
    int row;
    int currSeq;
    BOOL play;
}
@property (nonatomic, retain) UIImage* frameImage;
//@property (nonatomic, retain) UIImage* image;
@property (nonatomic, assign) BOOL play;
@property (nonatomic, assign) int currSeq;
@property (nonatomic, assign) int row;
@end
@implementation SEImageViewForImageList
@synthesize frameImage = mFrameImage;
@synthesize play;
@synthesize currSeq;
@synthesize row;
/*
- (void) setFrameImage:(UIImage *)fi
{
    [mFrameImage release];
    mFrameImage = [fi retain];
}
- (UIImage*) frameImage
{
    return mFrameImage;
}
 */
- (void) setImage:(UIImage *)im
{
    [mContentImage release];
    mContentImage = [im retain];
    [self setNeedsDisplay];
    NSLog(@"mContentImage = %@", mContentImage);
}

- (UIImage*) image
{
    return mContentImage;
}

- (void) dealloc
{
    [mContentImage release];
    [mFrameImage release];
    [super dealloc];
}
- (void) drawRect: (CGRect)rect
{
    //CGFloat startx = (rect.size.width - mContentImage.size.width) / 2;
    //CGFloat starty = (rect.size.height - mContentImage.size.height) / 2;
    //CGBlendMode blendMode = kCGBlendModeSourceAtop;
    //[mContentImage drawAtPoint:CGPointMake(startx, starty)];
    float width = rect.size.width;
    float height = rect.size.height;
    NSLog(@"### contentimage = %@, %f, %f", mContentImage, mContentImage.size.width, mContentImage.size.height);
    NSLog(@"width = %f, height = %f", width, height);
    
    UIImage* frameImage = mFrameImage;
    if(frameImage == nil)
    {
        [mContentImage drawInRect:CGRectMake(0, 0, rect.size.width, rect.size.height)];
        return;
    }
    [mContentImage drawInRect:CGRectMake(9, 9, rect.size.width - 20, rect.size.height - 20)];
    //CGFloat topPadding = 3, leftPadding = 3;
    UIGraphicsBeginImageContext(CGSizeMake(9, 9));
    [frameImage drawAtPoint:CGPointMake(0, 0)];
    UIImage* leftTopCornerImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    //if(alpha == 1.0)
    {
        [leftTopCornerImage drawAtPoint:CGPointMake(0, 0)];
    }
    /*
    else
    {
        [leftTopCornerImage drawAtPoint:CGPointMake(startx - 9, starty - 9) blendMode:blendMode alpha:alpha];
    }
    */
    UIGraphicsBeginImageContext(CGSizeMake(frameImage.size.width - 20, 9));
    [frameImage drawAtPoint:CGPointMake(-9, 0)];
    UIImage* topImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    //if(alpha == 1.0)
    {
        [topImage drawInRect:CGRectMake(9, 0, width - 20, 9)];
    }
    /*
    else
    {
        [topImage drawAtPoint:CGPointMake(startx, starty - 9) blendMode:blendMode alpha:alpha];
    }
    */
    UIGraphicsBeginImageContext(CGSizeMake(11, 11));
    [frameImage drawAtPoint:CGPointMake(-frameImage.size.width + 11, 0)];
    UIImage* rightTopImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    //if(alpha == 1.0)
    {
        //[rightTopImage drawAtPoint:CGPointMake(width - 11, 0)];
        [rightTopImage drawInRect:CGRectMake(width - 11, 0, 11, 11)];
    }
    /*
    else
    {
        [rightTopImage drawAtPoint:CGPointMake(startx + mContentImage.size.width, starty - 9) blendMode:blendMode alpha:alpha];
    }
    */
    UIGraphicsBeginImageContext(CGSizeMake(9, frameImage.size.height - 20));
    [frameImage drawAtPoint:CGPointMake(0, -9)];
    UIImage* leftImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    //if(alpha == 1.0)
    {
        //[leftImage drawAtPoint:CGPointMake(0, 9)];
        [leftImage drawInRect:CGRectMake(0, 9, 9, height - 20)];
    }
    /*
    else
    {
        [leftImage drawAtPoint:CGPointMake(startx - 9, starty) blendMode:blendMode alpha:alpha];
    }
    */
    UIGraphicsBeginImageContext(CGSizeMake(11, frameImage.size.height - 20));
    [frameImage drawAtPoint:CGPointMake(-frameImage.size.width + 11, -9)];
    UIImage* rightImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    //if(alpha == 1.0)
    {
        //[rightImage drawAtPoint:CGPointMake(width - 11, 9)];
        [rightImage drawInRect:CGRectMake(width - 11, 9, 11, height - 20)];
    }
    /*
    else
    {
        [rightImage drawAtPoint:CGPointMake(startx + mContentImage.size.width, starty) blendMode:blendMode alpha:alpha];
    }
    */
    UIGraphicsBeginImageContext(CGSizeMake(9, 11));
    [frameImage drawAtPoint:CGPointMake(0, -frameImage.size.height + 11)];
    UIImage* leftBottomImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    //if(alpha == 1.0)
    {
        //[leftBottomImage drawAtPoint:CGPointMake(0, height - 11)];
        [leftBottomImage drawInRect:CGRectMake(0, height - 11, 9, 11)];
    }
    /*
    else
    {
        [leftBottomImage drawAtPoint:CGPointMake(startx - 9, starty + mContentImage.size.height) blendMode:blendMode alpha:alpha];
    }
    */
    UIGraphicsBeginImageContext(CGSizeMake(frameImage.size.width - 20, 11));
    [frameImage drawAtPoint:CGPointMake(-9, -frameImage.size.height + 11)];
    UIImage* bottomImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    //if(alpha == 1.0)
    {
        //[bottomImage drawAtPoint:CGPointMake(9, height - 11)];
        [bottomImage drawInRect:CGRectMake(9, height - 11, width - 20, 11)];
    }
    /*
    else
    {
        [bottomImage drawAtPoint:CGPointMake(startx, starty + mContentImage.size.height) blendMode:blendMode alpha:alpha];
    }
    */
    UIGraphicsBeginImageContext(CGSizeMake(11, 11));
    [frameImage drawAtPoint:CGPointMake(-frameImage.size.width + 11, -frameImage.size.height + 11)];
    UIImage* rightBottomImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    //if(alpha == 1.0)
    {
       // [rightBottomImage drawAtPoint:CGPointMake(width - 11, height - 11)];
        [rightBottomImage drawInRect:CGRectMake(width - 11, height - 11, 11, 11)];
    }
    /*
    else
    {
        [rightBottomImage drawAtPoint:CGPointMake(startx + mContentImage.size.width, starty + mContentImage.size.height) blendMode:blendMode alpha:alpha];
    }
    */
}

@end
///////
@implementation SELineView



@end
/////
@implementation SEImageListURLProperty

@synthesize currentIndex;
@synthesize selectedImageArray;
@synthesize name;
- (void) dealloc
{
    [name release];
    [selectedImageArray release];
    [super dealloc];
}

@end
/////
@interface SEImageListLoader : SEImageAsyncLoadHandler
{
    UIImage* currImage;
    SEImageViewForImageList* imageView;
    SEViewNavigator* viewNav;
}
@property (nonatomic, retain) SEImageViewForImageList* imageView;
@property (nonatomic, assign) SEViewNavigator* viewNav;
@end
@implementation SEImageListLoader
@synthesize imageView;
@synthesize viewNav;
- (void) setImage:(UIImage *)image
{
    [currImage release];
    currImage = [image retain];
    NSLog(@"currImage = %@", currImage);
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
    NSLog(@"handleimage = %@", currImage);
}

@end
////////
@implementation SEPlayView
@synthesize currSeq;
@synthesize row;
@synthesize play;
@end
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
- (void) addMusicListButtonHandler;
- (void) addImageListButtonHandler;
- (void) removeButtonHandler;
- (void) removeMusicRowLine: (int) row;
- (void) removeImageRowLine: (int) row;
- (void) cancelHandler: (id)sender;
- (void) handleInputPopupViewOK:(id)sender;
- (void) handleInputPopupViewCancel: (id)sender;
- (void) dismissPopup;
- (void)setSelectedItem: (NSString*) name type: (int)type;
- (RowData) getIndexPathOnPoint: (CGPoint)p;
- (void) addAttach;
- (void) drawAttach;
- (void) drawAttachFrom: (int) seq tableType: (int) tableType offset: (CGPoint) offset;
- (SEIndicatorView*) getIndicatorView: (int)tableType row: (int)row;
- (UIView*) createLineView: (CGPoint) src  : (CGPoint) dst : (BOOL) isSelected;
- (void) scrollHandler : (CGPoint) offset tableType: (int)currentTableType;
- (BOOL) hasAttachedToMusicList:(int)imageSeq;
- (BOOL) hasAttachedImageInMusicList: (int)musicSeq;
- (void) handleImageClick: (UITapGestureRecognizer*)ges;
- (void) handleMusicClick: (UITapGestureRecognizer*)ges;
- (void) removeLineViewWhenSwitch: (int)row tableType: (int)tableType;
- (void) changeLineViewWhenSwitch: (int)row tableType: (int)tableType;
- (void) realRemoveLineView: (int) row tableType: (int) tableType;
- (BOOL) isLineViewExist: (int)startRow : (int)endRow;
- (BOOL) canRemoveLineView: (int)row tableType: (int)tableType;
- (int) lineViewNum;
- (void) addLineViewToParent:(UIView*)v;
@end
@implementation SEMusicImageListView (Private)
- (void) addLineViewToParent: (UIView*)v
{
    [mClipLineView addSubview:v];
}
- (int) lineViewNum
{
    int num = 0;
    for(UIView* v in self.subviews)
    {
        if([v isMemberOfClass:[SELineView class]])
        {
            num++;
        }
    }
    return num;
}
//start row must be image table
//end row must be music table
- (BOOL) isLineViewExist: (int)startRow : (int)endRow
{
    for(int i = 0 ; i < mLineViewArray.count ; i++)
    {
        LineViewData* lvd = [mLineViewArray objectAtIndex:i];
        if(lvd.mStartIndicatorRow == startRow && lvd.mEndIndicatorRow == endRow)
            return YES;
    }
    return NO;
}
- (void)printLineViewNum
{
    NSLog(@"current line view num = %d", [self lineViewNum]);
}
- (BOOL) canRemoveLineView: (int)row tableType: (int)tableType
{
    for(int i = 0 ; i < mLineViewArray.count ; i++)
    {
        LineViewData* lvd = [mLineViewArray objectAtIndex:i];
        if(tableType == IMAGE_TABLE && lvd.mStartIndicatorRow == row)
        {
            return YES;
        }
        else if(lvd.mEndIndicatorRow == row && tableType == MUSIC_TABLE)
        {
            return YES;
        }
    }
    return NO;
}
//remove line view from mLineViewArray
- (void) realRemoveLineView: (int) row tableType: (int) tableType
{
    NSMutableArray* deleteArray = [NSMutableArray array];
    for(int i = 0 ; i < mLineViewArray.count ; i++)
    {
        LineViewData* lvd = [mLineViewArray objectAtIndex:i];
        if(tableType == IMAGE_TABLE && lvd.mStartIndicatorRow == row)
        {
            [deleteArray addObject:lvd];
            [lvd.mView removeFromSuperview];
            lvd.mView = nil;
        }
        else if(lvd.mEndIndicatorRow == row && tableType == MUSIC_TABLE)
        {
            [deleteArray addObject:lvd];
            [lvd.mView removeFromSuperview];
            lvd.mView = nil;
        }
    }
    [mLineViewArray removeObjectsInArray:deleteArray];
    [self printLineViewNum];
}
- (void) changeLineViewWhenSwitch: (int)row tableType: (int)tableType
{
    if(tableType == IMAGE_TABLE)
    {
        for(int i = 0 ; i < mLineViewArray.count ; i++)
        {
            LineViewData* lvd = [mLineViewArray objectAtIndex:i];
            if(tableType == IMAGE_TABLE && lvd.mStartIndicatorRow == row)
            {
                ((SELineView*)lvd.mView).image = mLineImageNotSelected;
            }
        }
    }
}
//remove line view about current row when you switch current image list or current music list
- (void) removeLineViewWhenSwitch: (int)row tableType: (int)tableType
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    BOOL needDelete = YES;
    if(tableType == IMAGE_TABLE)
    {
        /*
        NSString* currentMusicListName = userInfo.currentmusiclist;
        MusicList* ml = [mViewNav getMusicListByName:currentMusicListName];
        NSArray* imageAttached = [ml.attachimagelist allObjects];
        for(int i = 0 ; i < imageAttached.count ; i++)
        {
            ImageList* il = [imageAttached objectAtIndex:i];
            if([il.seq intValue] == row)
            {
                needDelete = NO;
                break;
            }
        }
        if(needDelete == NO)
            return;
         */
        [self realRemoveLineView:row tableType:tableType];
    }
    else if(tableType == MUSIC_TABLE)
    {
        NSString* currentImageListName = userInfo.currentimagelist;
        ImageList* il = [mViewNav getImageListByName:currentImageListName];
        MusicList* ml = [mViewNav getMusicListBySeq:[NSNumber numberWithInt:row]];
        NSArray* images = [ml.attachimagelist allObjects];
        NSArray* newArray = [NSArray array];
        for(int i = 0 ; i < images.count ; i++)
        {
            ImageList* imageList = [images objectAtIndex:i];
            if([il.seq isEqualToNumber: imageList.seq] == NO)
            {
                newArray = [newArray arrayByAddingObject:imageList];
            }
        }
        for(int i = 0 ; i < newArray.count ; i++)
        {
            ImageList* imageList = [newArray objectAtIndex:i];
            [self realRemoveLineView:[imageList.seq intValue] tableType:IMAGE_TABLE];
        }
    }

}
- (void) handleMusicClick: (UITapGestureRecognizer*)ges
{
    SEPlayView* v = (SEPlayView*)ges.view;
    [mViewNav setViewRelationType:TYPE2];
    int index = v.row;
    MusicList* musicList = [mViewNav getMusicListBySeq:[NSNumber numberWithInt:index]];
    mViewNav.mCurrentLoadedMusicListName = musicList.name;
    assert(musicList.name != nil);
    [mViewNav makeImageMusicPickerMoveToMid];
    [mViewNav moveToView:MUSIC_IMAGE_LIST_ATTACH_MUSIC_PICKER :MUSIC_PICKER hasAnimation:YES isPush:YES];
}
- (void) handleImageClick: (UITapGestureRecognizer*)ges
{
    SEImageViewForImageList* v = (SEImageViewForImageList*)ges.view;
    NSLog(@"image click : %d\n", v.row);
    int index = v.row;
    ImageList* imageList = [mViewNav getImageListBySeq:[NSNumber numberWithInt:index]];
    //UserInfo* userInfo = [mViewNav getUserInfo];
    //userInfo.currentimagelist = imageList.name;
    NSLog(@"image list name = %@", imageList.name);
    mViewNav.mCurrentLoadedImageListName = imageList.name;
    [mViewNav setViewRelationType:TYPE2];
    [mViewNav makeImageMusicPickerMoveToMid];
    [mViewNav moveToView:MUSIC_IMAGE_LIST_ATTACH_IMAGE_PICKER :IMAGE_PICKER hasAnimation:YES isPush:YES];
}

- (void) removeMusicRowLine: (int) row
{
    [self realRemoveLineView:row tableType:MUSIC_TABLE];
    MusicList* list = [mViewNav getMusicListBySeq:[NSNumber numberWithInt:row]];
    if(list.name != nil)
    {
        [mViewNav removeAllAttachedImageFromMusicList:list.name];
        [mViewNav saveCoreDataContext];
    }

}
// it just remove the row line and the attach between image and music
- (void) removeImageRowLine: (int) row
{
    [self realRemoveLineView:row tableType:IMAGE_TABLE];
    ImageList* list = [mViewNav getImageListBySeq:[NSNumber numberWithInt:row]];
    if(list.name != nil)
    {
        [mViewNav removeAttachedImageListFromMusicList:list.name];
        [mViewNav saveCoreDataContext];
    }
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
    NSLog(@"scrollHandler");
    CGPoint imageTableOffset = mImageListTableView.contentOffset;
    CGPoint musicTableOffset = mMusicListTableView.contentOffset;
    for(LineViewData* lvd in mLineViewArray)
    {
        CGPoint p1 = lvd.mStartPoint;
        CGPoint p2 = lvd.mEndPoint;

        p1 = [mImageListTableView convertPoint:p1 toView:self];
        p2 = [mMusicListTableView convertPoint:p2 toView:self];
        [lvd.mView removeFromSuperview];
        BOOL isSelected = ((SELineView*)lvd.mView).image == mLineImage;
        UIView* v = [self createLineView:p1 :p2: isSelected];
        lvd.mView = v;
        //[self addSubview:v];
        [self addLineViewToParent:v];
    }
    [self printLineViewNum];
}
- (UIView*) createLineView: (CGPoint) src  : (CGPoint) dst : (BOOL) isSelected
{
    src = [self convertPoint:src toView:mClipLineView];
    dst = [self convertPoint:dst toView:mClipLineView];
    const float PI = 3.14159;
    float deltax = dst.x - src.x;
    float deltay = dst.y - src.y;
    float theta = atanf(fabsf(deltay) / fabsf(deltax));
    float width = sqrtf(deltax * deltax + deltay * deltay);
    float translatex = width / 2;;
    //NSLog(@" %s, deltax = %f, deltay = %f, theta = %f", __FUNCTION__, deltax, deltay, theta * 180 / PI);
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
    SELineView* lineView = [[SELineView alloc] init];
    float height = LINE_VIEW_HEIGHT;
    if(isSelected)
    {
        lineView.image = mLineImage;//[SEUtil imageWithInsets:mLineImage top:0 bottom:0 left:0.05 right:0.05];
    }
    else
    {
        lineView.image = mLineImageNotSelected;    
    }
    lineView.transform = CGAffineTransformIdentity;
    lineView.frame = CGRectMake(src.x - width / 2, src.y - height / 2, width, height);
    //lineView.frame = CGRectMake(src.x - width / 2, src.y - mLineImage.size.height / 2, width, mLineImage.size.height);
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
        NSLog(@"## draw from IMAGE_TABLE to MUSIC_TABLE, %d to %d", seq, [musicList.seq intValue]);
        if(musicList && [self isLineViewExist:seq : [musicList.seq intValue]] == YES)
        {
            int musicSeq = [musicList.seq intValue];
            int imageIndex = seq;
            int musicIndex = [musicList.seq intValue];
            
            for(int i = 0 ; i < mLineViewArray.count ; i++)
            {
                LineViewData* lvd = [mLineViewArray objectAtIndex:i];
                if(lvd.mStartIndicatorRow == imageIndex && lvd.mEndIndicatorRow == musicIndex)
                {
                    ((SELineView*)lvd.mView).image = mLineImage;
                }
            }
            
            //SEIndicatorView* imageIndicatorView = [self getIndicatorView:IMAGE_TABLE row:seq];
            //SEIndicatorView* musicIndicatorView = [self getIndicatorView:MUSIC_TABLE row:musicSeq];
            //CGPoint p1 = [imageIndicatorView convertPoint:mImageIndicatorCenter toView:self];
            //CGPoint p2 = [musicIndicatorView convertPoint:mMusicIndicatorCenter toView:self];
            /*
            CGPoint p1 = CGPointMake(mImageIndicatorPointInCell.x, mImageIndicatorPointInCell.y + seq * mImageCellHeight);
            CGPoint p2 = CGPointMake(mMusicIndicatorPointInCell.x, mMusicIndicatorPointInCell.y + musicSeq * mMusicCellHeight);

            CGPoint imageListTableOffset = mImageListTableView.contentOffset;
            CGPoint musicListTableOffset = mMusicListTableView.contentOffset;
            LineViewData* lvd = [[LineViewData alloc] init];
            lvd.mStartPoint = p1;
            lvd.mEndPoint = p2;
            lvd.mStartIndicatorRow = [il.seq intValue];
            lvd.mEndIndicatorRow = [musicList.seq intValue];
            lvd.mStartIndicatorTableType = IMAGE_TABLE;
            lvd.mEndIndicatorTableType = MUSIC_TABLE;
            
            //p1.x -= imageListTableOffset.x;
            //p1.y -= imageListTableOffset.y;
            //p2.x -= musicListTableOffset.x;
            //p2.y -= musicListTableOffset.y;
            p1 = [mImageListTableView convertPoint:p1 toView:self];
            p2 = [mMusicListTableView convertPoint:p2 toView:self];
            UIView* v = [self createLineView:p1 :p2 : YES];
            lvd.mView = v;
            [mLineViewArray addObject:lvd];
            [lvd release];
            //[self addSubview:v];
            [self addLineViewToParent:v];
             */
        }
    }
    else if(tableType == MUSIC_TABLE)
    {
        MusicList* musicList = [mViewNav getMusicListBySeq:[NSNumber numberWithInt:seq]];
        NSArray* imageListArray = [mViewNav getMusicAttachedImage:musicList.name];
        for(ImageList* il in imageListArray)
        {
            int imageListSeq = [il.seq intValue];
            if([self isLineViewExist:imageListSeq : [musicList.seq intValue] ])
            {
                
            }
            /*
            CGPoint p1 = CGPointMake(mImageIndicatorPointInCell.x, mImageIndicatorPointInCell.y + imageListSeq * mImageCellHeight);
            CGPoint p2 = CGPointMake(mMusicIndicatorPointInCell.x, mMusicIndicatorPointInCell.y + seq * mMusicCellHeight);
            
            LineViewData* lvd = [[LineViewData alloc] init];
            lvd.mStartPoint = p1;
            lvd.mEndPoint = p2;
            lvd.mStartIndicatorRow = imageListSeq;
            lvd.mEndIndicatorRow = [musicList.seq intValue];
            lvd.mStartIndicatorTableType = IMAGE_TABLE;
            lvd.mEndIndicatorTableType = MUSIC_TABLE;
            CGPoint imageListTableOffset = mImageListTableView.contentOffset;
            CGPoint musicListTableOffset = mMusicListTableView.contentOffset;
            
            p1 = [mImageListTableView convertPoint:p1 toView:self];
            p2 = [mMusicListTableView convertPoint:p2 toView:self];
            UIView* v = [self createLineView:p1 :p2 : YES];
            lvd.mView = v;
            [mLineViewArray addObject:lvd];
            [lvd release];
            //[self addSubview:v];
            [self addLineViewToParent:v];
             */
        }
    }
    [self printLineViewNum];
}

- (void) drawAttach
{
    SEIndicatorView* startIndicator = [self getIndicatorView:mStartIndicatorTable row:mStartIndicatorRow];
    SEIndicatorView* endIndicator = [self getIndicatorView:mEndIndicatorTable row:mEndIndicatorRow];
    CGPoint p1Indicator;
    CGPoint p2Indicator;
    if(mStartIndicatorTable == IMAGE_TABLE)
    {
        p1Indicator = CGPointMake(mImageIndicatorPointInCell.x, mImageIndicatorPointInCell.y + mStartIndicatorRow * mImageCellHeight);
    }
    else if(mStartIndicatorTable == MUSIC_TABLE)
    {
        p1Indicator = CGPointMake(mMusicIndicatorPointInCell.x, mMusicIndicatorPointInCell.y + mStartIndicatorRow * mImageCellHeight);;
    }
    
    if(mEndIndicatorTable == IMAGE_TABLE)
    {
        p2Indicator = CGPointMake(mImageIndicatorPointInCell.x, mImageIndicatorPointInCell.y + mEndIndicatorRow * mImageCellHeight);;
    }
    else if(mEndIndicatorTable == MUSIC_TABLE)
    {
        p2Indicator = CGPointMake(mMusicIndicatorPointInCell.x, mMusicIndicatorPointInCell.y + mEndIndicatorRow * mImageCellHeight);
    }
    
    //CGPoint p1 = [startIndicator convertPoint:CGPointMake(p1Indicator.x, p1Indicator.y) toView:self];
    //CGPoint p2 = [endIndicator convertPoint:CGPointMake(p2Indicator.x, p2Indicator.y) toView:self];
    CGPoint p1 = p1Indicator;
    CGPoint p2 = p2Indicator;
    LineViewData* lvd = [[LineViewData alloc] init];
    if(mStartIndicatorTable == IMAGE_TABLE)
    {
        lvd.mStartIndicatorRow = mStartIndicatorRow;
        lvd.mStartPoint = p1;
    }
    else if(mStartIndicatorTable == MUSIC_TABLE)
    {
        lvd.mStartIndicatorRow = mEndIndicatorRow;
        lvd.mStartPoint = p2;
    }
    
    if(mEndIndicatorTable == MUSIC_TABLE)
    {
        lvd.mEndIndicatorRow = mEndIndicatorRow;
        lvd.mEndPoint = p2;
    }
    else if(mEndIndicatorTable == IMAGE_TABLE)
    {
        lvd.mEndIndicatorRow = mStartIndicatorRow;    
        lvd.mEndPoint = p1;
    }
    lvd.mStartIndicatorTableType = IMAGE_TABLE;
    lvd.mEndIndicatorTableType = MUSIC_TABLE;
    
    CGPoint imageListTableOffset = mImageListTableView.contentOffset;
    CGPoint musicListTableOffset = mMusicListTableView.contentOffset;
    p1 = lvd.mStartPoint;
    p2 = lvd.mEndPoint;
    //p1.x -= imageListTableOffset.x;
    //p1.y -= imageListTableOffset.y;
    //p2.x -= musicListTableOffset.x;
    //p2.y -= musicListTableOffset.y;
    ImageList* imageList = [mViewNav getImageListBySeq:[NSNumber numberWithInt: lvd.mStartIndicatorRow]];
    NSString* currentImageList = [[mViewNav getUserInfo] currentimagelist];
    BOOL isSelected = [imageList.name isEqualToString:currentImageList];
    p1 = [mImageListTableView convertPoint:p1 toView:self];
    p2 = [mMusicListTableView convertPoint:p2 toView:self];
    UIView* v = [self createLineView:p1 :p2 : isSelected];
    lvd.mView = v;
    [mLineViewArray addObject:lvd];
    [lvd release];

    [self addLineViewToParent:v];
    [self printLineViewNum];
}

- (void) addAttach
{
    if(mStartIndicatorTable == NO_TABLE || mEndIndicatorTable == NO_TABLE )
        return;
    if(mStartIndicatorTable == mEndIndicatorTable)
        return;
    if([self isLineViewExist:mStartIndicatorRow :mEndIndicatorRow])
    {
        return;
    }
    
    if(mStartIndicatorTable == IMAGE_TABLE && mEndIndicatorTable == MUSIC_TABLE)
    {
        ImageList* imageList = [mViewNav getImageListBySeq:[NSNumber numberWithInt:mStartIndicatorRow]];
        MusicList* musicList = [mViewNav getMusicListBySeq:[NSNumber numberWithInt:mEndIndicatorRow]];
        //assert(imageList != nil);
        //assert(musicList != nil);
        if(imageList == nil || musicList == nil)
            return;
        //[mViewNav attachImageToMusic:musicList.name imageListName:imageList.name];
        NSString* str = imageList.name;
        NSLog(@"image list name = %@", str);
        MusicList* currentMusicList = [mViewNav getMusicListByImageList:imageList.name];
        if(currentMusicList != nil)
        {
            //[currentMusicList removeAttachimagelistObject:imageList];
            [self realRemoveLineView:mStartIndicatorRow tableType:IMAGE_TABLE];
            [mViewNav removeAttachedImageListFromMusicList:imageList.name];
        }
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
        if(musicList == nil || imageList == nil)
            return;
        MusicList* currentMusicList = [mViewNav getMusicListByImageList:imageList.name];
        if(currentMusicList != nil)
        {
            [self realRemoveLineView:mEndIndicatorRow tableType:IMAGE_TABLE];
            [mViewNav removeAttachedImageListFromMusicList:imageList.name];
        }
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
    {
        assert(0);
    }
    [self printLineViewNum];
        
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
            ImageList* il = [mViewNav getImageListBySeq:[NSNumber numberWithInt:indexPath.row]];
            if(il != nil)
            {
                rd.tableType = IMAGE_TABLE;
                rd.indexPath = indexPath;
            }
        }
    }
    else
    {
        MusicList* ml = [mViewNav getMusicListBySeq:[NSNumber numberWithInt:indexPath.row]];
        if(ml != nil)
        {
            rd.tableType = MUSIC_TABLE;
            rd.indexPath = indexPath;
        }
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
    [mViewNav hideMusicImageAttachInputView];
}
- (void) handleInputPopupViewOK:(id)sender
{
    if(mViewNav.mCurrView != MUSIC_IMAGE_LIST_ATTACH)
    {
        [self dismissPopup];
        [self release];
        return;
    }
    SEMusicImageListPopupView* popupView = [mViewNav getMusicImageListPopupView];
    NSString* text = [popupView getContentText];
    if(text == nil || [text isEqualToString:@""])
    {
        mAlertType = NO_ALERT;
        [self dismissPopup];
        [self release];
        return;
    }
    if(mAlertType == ADD_MUSIC_LIST)
    {
        MusicList* musicList = [mViewNav getMusicListByName:text];
        if(musicList == nil)
        {
            NSString* name = text;
            BOOL bFirstItem = [[mViewNav getAllMusicList] count ] == 0;
            musicList = [mViewNav addMusicList:name];
            if(bFirstItem)
            {
                UserInfo* userInfo = [mViewNav getUserInfo];
                userInfo.currentmusiclist = musicList.name;
            }
            mMusicListTableView.mMusicListPropertyArray = [mViewNav getAllMusicList];
            SEMusicItem* item = [[SEMusicItem alloc] init];
            item.row = mMusicListTableView.mMusicItems.count;
            item.play = NO;
            item.currSeq = 0;
            item.musicListName = name;
            NSLog(@"add music row = %d", item.row);
            [mMusicListTableView.mMusicItems addObject:item];
            [item release];
            [mMusicListTableView reloadData];
            //[mMusicListTableView scrollToRowAtIndexPath:indexPath atScrollPosition:UITableViewScrollPositionBottom animated:YES];
            [self dismissPopup];
            [self release];
            [mViewNav saveContext];
        }
        else
        {
            [popupView setErrorMsgText: @"has the same name, please input different name"];
            int seq = [musicList.seq intValue];
            NSIndexPath* indexPath = [NSIndexPath indexPathForRow:seq inSection:0];
            [mMusicListTableView scrollToRowAtIndexPath:indexPath atScrollPosition:UITableViewScrollPositionTop animated:YES];
        }
    }
    else if(mAlertType == ADD_IMAGE_LIST)
    {
        ImageList* imageList = [mViewNav getImageListByName:text];
        if(imageList == nil)
        {
            NSString* name = text;
            BOOL firstItem = [[mViewNav getAllImageList] count] == 0;
            imageList = [mViewNav addImageList:name];
            UserInfo* userInfo = [mViewNav getUserInfo];
            if(firstItem)
            {
                userInfo.currentimagelist = imageList.name;
            }
            //mImageListTableView.mImageListPropertyArray = [mViewNav getAllImageList];
            [self updateImageListURL];
            [mImageListTableView reloadData];
            [self dismissPopup];
            [self release];
            [mViewNav saveContext];
            
        }
        else
        {
            [popupView setErrorMsgText: @"has the same name, please input different name"];
            int seq = [imageList.seq intValue];
            NSIndexPath* indexPath = [NSIndexPath indexPathForRow:seq inSection:0];
            [mImageListTableView scrollToRowAtIndexPath:indexPath atScrollPosition:UITableViewScrollPositionTop animated:YES];
            //popupView.errorMsg.textColor = [UIColor redColor];
        }
    }
}
- (void) handleInputPopupViewCancel:(id)sender
{
    mAlertType = NO_ALERT;
    [self dismissPopup];
    [self release];
}
- (void) setInputPopupViewHandler
{
    SEMusicImageListPopupView* popupView = [mViewNav getMusicImageListPopupView];
    [self retain];
    [popupView setHandler:self ok:@selector(handleInputPopupViewOK:) cancel:@selector(handleInputPopupViewCancel:)];
}
- (void) addButtonConfirmDlgHandler
{
    [mViewNav dismissConfirmDlg];

}
- (void) addMusicListButtonHandler
{
    NSLog(@"add music list\n");
    NSArray* allMusicList = [mViewNav getAllMusicList];
    const int maxCount = 10;
    if(allMusicList.count >= maxCount)
    {
        [mViewNav dismissConfirmDlg];
        [mViewNav createConfirmDlg:self ok:@selector(addButtonConfirmDlgHandler)];
        SEConfirmDlg* dlg = [mViewNav getConfirmDlg];
        [dlg setMessageText:@"Warning!"];
        NSString* str = [NSString stringWithFormat:@"You exceed the max music list count %d .", maxCount];
        [dlg setMessage2Text:str];
        [mViewNav showConfirmDlg];
        return;
    }
    mAlertType = ADD_MUSIC_LIST;
    
    if([mViewNav getMusicImageListPopupView] == nil)
    {
        [mViewNav createMusicImageAttachInputView];
        [self setInputPopupViewHandler];
    }
    SEMusicImageListPopupView* popupView = [mViewNav getMusicImageListPopupView];
    [popupView setTitleText:@"input music list name:"];
    //NSArray* mlArray = [mViewNav getAllMusicList];
    NSMutableArray* nameArray = [NSMutableArray array];
    for(int i = 0 ; i < allMusicList.count; i++)
    {
        [nameArray addObject:[[allMusicList objectAtIndex:i] name]];
    }
    int count  = allMusicList.count;
    NSString* str = createNextName(@"Music", nameArray, count + 1);//[NSString stringWithFormat:@"Music%d", allMusicList.count + 1];
    popupView.content.text = str;
    [mViewNav showMusicImageAttachInputView];
    
}
- (void) addImageListButtonHandler
{
    NSLog(@"add image list \n");
    NSArray* allImageList = [mViewNav getAllImageList];
    const int maxCount = 10;
    if(allImageList.count >= maxCount)
    {
        [mViewNav dismissConfirmDlg];
        [mViewNav createConfirmDlg:self ok:@selector(addButtonConfirmDlgHandler)];
        SEConfirmDlg* dlg = [mViewNav getConfirmDlg];
        [dlg setMessageText:@"Warning!"];
        NSString* str = [NSString stringWithFormat:@"You exceed the max image list count %d .", maxCount];
        [dlg setMessage2Text:str];
        [mViewNav showConfirmDlg];
        return;
    }
    mAlertType = ADD_IMAGE_LIST;
    if([mViewNav getMusicImageListPopupView] == nil)
    {
        [mViewNav createMusicImageAttachInputView];
        [self setInputPopupViewHandler];
    }
    SEMusicImageListPopupView* popupView = [mViewNav getMusicImageListPopupView];
    [popupView setTitleText:@"input image list name:"];
    NSMutableArray* nameArray = [NSMutableArray array];
    for(int i = 0 ; i < allImageList.count ;i++)
    {
        [nameArray addObject:[[allImageList objectAtIndex:i] name]];
    }
    int count = allImageList.count;
    NSString* str = createNextName(@"Image", nameArray, count + 1);//[NSString stringWithFormat:@"Image%d", allImageList.count + 1];
    popupView.content.text = str;
    [mViewNav showMusicImageAttachInputView];
}
- (void) cancelHandler: (id)sender
{
    if(mTableType == IMAGE_TABLE)
    {
        
    }
    else if(mTableType == MUSIC_TABLE)
    {
    }
    [mViewNav dismissConfirmDlg];
}
- (void) updateLine : (int)tableType
{
    NSString* currentImageListName = [[mViewNav getUserInfo] currentimagelist];
    if(tableType == MUSIC_TABLE)
    {
        NSMutableArray* greaterView = [NSMutableArray array];
        for(LineViewData* lvd in mLineViewArray)
        {
            if(lvd.mEndIndicatorRow > mNeedDeleteRow)
            {
                [greaterView addObject:lvd];
            }
        }
        for(LineViewData* lvd in greaterView)
        {
            lvd.mEndIndicatorRow -= 1;
            lvd.mEndPoint = CGPointMake(lvd.mEndPoint.x, lvd.mEndPoint.y - mImageCellHeight);
            CGPoint p1 = [mImageListTableView convertPoint:lvd.mStartPoint toView:self];
            CGPoint p2 = [mMusicListTableView convertPoint:lvd.mEndPoint toView:self];
            BOOL isSelected = ((SELineView*) lvd.mView).image == mLineImage;
            UIView* v = [self createLineView:p1 :p2: isSelected];
            [lvd.mView removeFromSuperview];
            lvd.mView = v;
            [self addLineViewToParent:v];
        }
    }
    else
    {
        NSMutableArray* greaterView = [NSMutableArray array];
        for(LineViewData* lvd in mLineViewArray)
        {
            if(lvd.mStartIndicatorRow > mNeedDeleteRow)
            {
                [greaterView addObject:lvd];
            }
        }
        for(LineViewData* lvd in greaterView)
        {
            lvd.mStartIndicatorRow -= 1;
            lvd.mStartPoint = CGPointMake(lvd.mStartPoint.x, lvd.mStartPoint.y - mImageCellHeight);
            CGPoint p1 = [mImageListTableView convertPoint:lvd.mStartPoint toView:self];
            CGPoint p2 = [mMusicListTableView convertPoint:lvd.mEndPoint toView:self];
            ImageList* imageList = [mViewNav getImageListBySeq: [NSNumber numberWithInt: lvd.mStartIndicatorRow]];
            BOOL isSelected = [imageList.name isEqualToString:currentImageListName];
            UIView* v = [self createLineView:p1 :p2: isSelected];
            [lvd.mView removeFromSuperview];
            lvd.mView = v;
            [self addLineViewToParent:v];
        }
        for(LineViewData* lvd in mLineViewArray)
        {
            ImageList* imageList = [mViewNav getImageListBySeq: [NSNumber numberWithInt: lvd.mStartIndicatorRow]];
            BOOL isSelected = [imageList.name isEqualToString:currentImageListName];
            UIImage* currentImage = ((SELineView*)lvd.mView).image;
            if(isSelected && currentImage != mLineImage)
            {
                ((SELineView*)lvd.mView).image = mLineImage;
            }
        }
    }
}

-(void) removeButtonHandler
{
    NSLog(@"remove list\n");
    if(mTableType == NO_TABLE)
        return;
    UserInfo* userInfo = [mViewNav getUserInfo];
    if(mTableType == MUSIC_TABLE)
    {
        [self removeMusicRowLine:mNeedDeleteRow];
        MusicList* ml = [mViewNav getMusicListBySeq:[NSNumber numberWithInt:mNeedDeleteRow]];
        ImageList* currentImageList = [mViewNav getImageListByName:userInfo.currentimagelist];
        BOOL removeCurrentImageListMusic = NO;
        if(currentImageList != nil)
        {
            MusicList* imageListAttachedMusic = [mViewNav getMusicListByImageList:currentImageList.name];
            if(imageListAttachedMusic && [imageListAttachedMusic.name isEqualToString:ml.name])
            {
                removeCurrentImageListMusic = YES;
            }
        }
        if(removeCurrentImageListMusic)
        {
            mViewNav.mMusicPlayListChange = YES;
        }
        //BOOL bDeleteCurrentMusicList = [userInfo.currentmusiclist isEqualToString:ml.name];
        [mViewNav removeMusicListByName:ml.name];
        NSArray* allMusicList = [mViewNav getAllMusicList];
        for(int i = 0 ; i < mMusicListTableView.mMusicItems.count; i++)
        {
            SEMusicItem* item = [mMusicListTableView.mMusicItems objectAtIndex:i];
            NSLog(@"row %d , %d", i, item.play);
            if(item.row == mNeedDeleteRow && item.play)
            {
                [SEUtil pauseCurrentMusic];
            }
        }
        [mMusicListTableView.mMusicItems removeObjectAtIndex:mNeedDeleteRow];
        for(int i = 0 ; i < mMusicListTableView.mMusicItems.count; i++)
        {
            SEMusicItem* item = [mMusicListTableView.mMusicItems objectAtIndex:i];
            item.row = i;
            NSLog(@"row %d , %d", i, item.play);
        }
        /*
        int seq = mNeedDeleteRow;
        if(allMusicList.count > 0)
        {
            if(seq < allMusicList.count)
            {
                ml = [allMusicList objectAtIndex:seq];
            }
            else
            {
                ml = [allMusicList lastObject];
            }
            if(bDeleteCurrentMusicList)
            {
                userInfo.currentmusiclist = ml.name;
            }
        }
        else 
        {
            userInfo.currentmusiclist = nil;
        }
         */
        mMusicListTableView.mMusicListPropertyArray = allMusicList;
        [mMusicListTableView reloadData];
        [self updateImageListTableViewIndicator];

    }
    else if(mTableType == IMAGE_TABLE)
    {
        ImageList* il = [mViewNav getImageListBySeq:[NSNumber numberWithInt:mNeedDeleteRow]];
        BOOL bDeleteCurrentImageList = [userInfo.currentimagelist isEqualToString:il.name];
        [self removeImageRowLine:mNeedDeleteRow];
        [mViewNav removeImageListByName:il.name];
        [self updateImageListURL];
        NSArray* newImageList = [mViewNav getAllImageList];
        if(newImageList.count > 0 && bDeleteCurrentImageList)
        {
            if(mNeedDeleteRow < newImageList.count)
            {
                il = [newImageList objectAtIndex:mNeedDeleteRow];
            }
            else
            {
                il = [newImageList lastObject];    
            }
            userInfo.currentimagelist = il.name;
            mViewNav.mMusicPlayListChange = YES;
        }
        else
        {
            if(newImageList.count == 0)
            {
                userInfo.currentimagelist = nil;
                mViewNav.mMusicPlayListChange = YES;
            }
        }
        mImageListTableView.mImageListPropertyArray = newImageList;
        [mImageListTableView reloadData];
        [self updateMusicListTableViewIndicator];
    }
    [self updateLine: mTableType];
    [mViewNav saveContext];
    mTableType = NO_TABLE;
    mCurrentName = nil;
    [mViewNav dismissConfirmDlg];
}
@end
///////
@implementation SEIndicatorView
@synthesize mTouchHandler;
@synthesize mResLoader;
@synthesize mType;
@synthesize row;
- (void) indicatorViewTapHandler: (UITapGestureRecognizer*)ges
{
    
}
- (void) setIndicationImage: (UITouchPhase)phase
{
    if(phase == UITouchPhaseBegan)
    {
        if(mType == IMAGE_INDICATION)
        {
            ImageList* il = [[PhotoFrameAppDelegate getViewNavigator] getImageListBySeq:[NSNumber numberWithInt:row]];
            if(il != nil)
            {
                self.image = [mResLoader getImage:@"MusicImageImageListAttachedIndicator"];
            }
            else {
                self.image = [mResLoader getImage:@"MusicImageImageListNormalIndicator"];
            }
        }
        else if(mType == MUSIC_INDICATION)
        {
            MusicList* ml = [[PhotoFrameAppDelegate getViewNavigator] getMusicListBySeq:[NSNumber numberWithInt:row]];
            if(ml != nil)
            {
                self.image = [mResLoader getImage:@"MusicImageImageListAttachedIndicator"];
            }
            else {
                self.image = [mResLoader getImage:@"MusicImageImageListNormalIndicator"];
            }
        }
    }
    else if(phase == UITouchPhaseCancelled || phase == UITouchPhaseEnded)
    {
        if(mType == IMAGE_INDICATION)
            self.image = [mResLoader getImage:@"MusicImageImageListNormalIndicator"];
        else if(mType == MUSIC_INDICATION)
            self.image = [mResLoader getImage:@"MusicImageImageListNormalIndicator"];
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
    [self setIndicationImage:UITouchPhaseBegan];
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
    BOOL ret = [mTouchHandler touchEnd:loc];
    BOOL hasAttached = NO;
    if(mType == IMAGE_INDICATION )
    {
        ImageList* currentImageList = [[PhotoFrameAppDelegate getViewNavigator] getImageListBySeq:[NSNumber numberWithInt:row]];
        MusicList* musicList  = [[PhotoFrameAppDelegate getViewNavigator] getMusicListByImageList:currentImageList.name];
        if(musicList != nil)
        {
            hasAttached = YES;
        }
    }
    else if(mType == MUSIC_INDICATION)
    {
        MusicList* ml = [[PhotoFrameAppDelegate getViewNavigator] getMusicListBySeq:[NSNumber numberWithInt:row]];
        NSSet* ilSet =  ml.attachimagelist;
        if(ilSet.count > 0)
        {
            hasAttached = YES;
        }
    }
    if(ret == NO && hasAttached == NO)
    {
        [self setIndicationImage:UITouchPhaseEnded];
    }
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
    CGPoint p = [[touches anyObject] locationInView:self];
    CGPoint loc = [self convertPoint:p toView:mTouchHandler];
    BOOL ret = [mTouchHandler touchEnd:loc];
    if(ret == NO)
    {
        [self setIndicationImage:UITouchPhaseCancelled];
    }
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
- (UIImageView*) getBackgroundView: (UITableViewCell*)cell
{
    UIImageView* imageView = (UIImageView*)[cell viewWithTag:101];
    return imageView;
}
- (UIImageView*) getLeftIconBgView: (UITableViewCell*)cell
{
    return (UIImageView*)[cell viewWithTag:103];
}
- (UIImageView*) getPlusIconView: (UITableViewCell*)cell
{
    UIView* v = [cell viewWithTag:125];
    return (UIImageView*)v;
}

- (SEImageViewForImageList*) getLeftIconView: (UITableViewCell*)cell
{
    return (SEImageViewForImageList*)[cell viewWithTag:120];
}
- (UIImageView*) getNameBgView: (UITableViewCell*)cell
{
    return (UIImageView*)[cell viewWithTag:104];
}
- (UILabel*) getNameLabel: (UITableViewCell*)cell
{
    return (UILabel*)[cell viewWithTag:107];
}
- (UIImageView*) getNumberBgView: (UITableViewCell*)cell
{
    return (UIImageView*)[cell viewWithTag:106];
}
- (UILabel*) getNumberLabel: (UITableViewCell*)cell
{
    return (UILabel*)[cell viewWithTag:109];
}
- (UIImageView*) getPlayBgView: (UITableViewCell*)cell
{
    return (UIImageView*)[cell viewWithTag:108];
}
- (SEPlayView*) getPlayBackView: (UITableViewCell*) cell
{
    return (SEPlayView*)[cell viewWithTag:121];
}
- (SEPlayView*) getPlayForwardView:(UITableViewCell*)cell
{
    return  (SEPlayView*)[cell viewWithTag:122];
}
- (SEIndicatorView*) getIndicatorView: (UITableViewCell*)cell
{
    return (SEIndicatorView*)[cell viewWithTag:102];
}
- (UIImageView*) getDeleteView: (UITableViewCell*)cell
{
    return (UIImageView*)[cell viewWithTag:123];
}
- (void) setIndicatorImage: (BOOL)highlighted indicator: (SEIndicatorView*)indicator
{
    UIImage* indicatorImage = nil;
    if(highlighted)
    {
        indicatorImage = [mResLoader getImage:@"MusicImageListIndicatorMusicNormalIconH"];
    }
    else
    {
        indicatorImage = [mResLoader getImage:@"MusicImageListIndicatorMusicNormalIcon"];
    }
    indicator.image = indicatorImage;
}
- (void) setIndicatorImage: (BOOL) highlighted cell: (UITableViewCell*)cell
{
    SEIndicatorView* indicator = (SEIndicatorView*)[self getIndicatorView:cell];
    [self setIndicatorImage:highlighted indicator:indicator];
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
    NSArray* array = [mViewNav getAllImageList]; //[mImageListPropertyArray count];
    int count = array.count;
    NSLog(@"image list count = %d", count);
    return count + 1;
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

- (void)indicatorTapHandler: (UITapGestureRecognizer*)tap
{
    NSLog(@"image indicator tap");
    SEIndicatorView* indicatorView = (SEIndicatorView*)tap.view;
    int row = indicatorView.row;
    NSLog(@"## remove line at %d", row);
    ImageList* il = [mViewNav getImageListBySeq:[NSNumber numberWithInt:row]];
    MusicList* ml = [mViewNav getMusicListByImageList: il.name];
    NSLog(@"music list |%@| contain image list |%@|", ml.name, il.name);
    if([mMusicImageListView canRemoveLineView:row tableType:IMAGE_TABLE])
    {
        UserInfo* userInfo = [mViewNav getUserInfo];
        if([il.name isEqualToString:userInfo.currentimagelist])
        {
            mViewNav.mMusicPlayListChange = YES;
        }
        //////
        [mMusicImageListView removeImageRowLine:row];
        [self setIndicatorImage:NO indicator:indicatorView];
        [mMusicImageListView updateMusicListTableViewIndicator];

    }
}
- (void) addHandler: (UITapGestureRecognizer*)tap
{
    NSLog(@"add handler");
    [mMusicImageListView addImageListButtonHandler];
}
- (void) deleteHandler: (UITapGestureRecognizer*)tap
{
    NSLog(@"delete image handler");
    UIView* v = tap.view;
    mMusicImageListView.mTableType = IMAGE_TABLE;
    int row = [mMusicImageListView getViewRow:v];
    mMusicImageListView.mNeedDeleteRow = row;
    [mViewNav createConfirmDlg:mMusicImageListView ok:@selector(removeButtonHandler) cancel:@selector(cancelHandler:)];
    ImageList* il = [mViewNav getImageListBySeq:[NSNumber numberWithInt: row]];
    SEConfirmDlg* dlg = [mViewNav getConfirmDlg];
    NSString* str = [NSString stringWithFormat:@"Do you want to delete image list \" %@ \"", il.name];
    [dlg setMessageText:str];
    [mViewNav showConfirmDlg];
}

- (void) loadImage: (SelectedImage*)si inView: (SEImageViewForImageList*)imageView
{
    SEImageListLoader* loader = [[SEImageListLoader alloc] init];
    loader.imageView = imageView;
    //SEImageAsyncLoader* asyncLoader = [[SEImageAsyncLoader alloc] init];
    //asyncLoader.mViewNav = (SEViewNavigator*)mViewNav;
    ALAssetsLibrary* lib = [[ALAssetsLibrary alloc] init];
    [loader setAssetLibOwn:lib];
    [lib release];
    NSURL* url = [NSURL URLWithString:si.url];
    //[asyncLoader loadImageThumbnailFromPhotoLib:url size:CGSizeMake(100, 100) withHandler:loader];
    [loader loadImageThumbnailFromPhotoLib:url size:CGSizeMake(100, 100)];
}
- (void) loadSelectedImageBySeq:(int) row : (SEImageViewForImageList*)imageView
{
    SEImageListURLProperty* pro = [mMusicImageListView.mImageListURLPropertyArray objectAtIndex:row];
    //UITableViewCell* cell = [self cellForRowAtIndexPath:[NSIndexPath indexPathForRow:row inSection:0]];
    //SEPlayView* view = [self getLeftIconView:cell];
    if(pro.selectedImageArray == nil)
    {
        imageView.image = [mResLoader getImage:@"MusicImageImageListNoImageIcon"];
        return;
    }
        NSArray* selectedImageArray = pro.selectedImageArray;
    SelectedImage* si = [selectedImageArray objectAtIndex:0];
    imageView.frameImage = [mResLoader getImage:@"ImagePickerFrameImage"];
    [self loadImage:si inView:imageView];
}
- (void) playBackHandler: (UITapGestureRecognizer*)tap
{
    NSLog(@"play back handler");
    SEPlayView* view = (SEPlayView*)tap.view;
    int row = view.row;
    UITableViewCell* cell = [mMusicImageListView.mImageListTableView cellForRowAtIndexPath:[NSIndexPath indexPathForRow:row inSection:0]];
    SEImageViewForImageList* imageView = [self getLeftIconView:cell];
    assert(imageView != nil);
    SEImageListURLProperty* pro = [mMusicImageListView.mImageListURLPropertyArray objectAtIndex:row];
    if(pro.selectedImageArray == nil)
        return;
    pro.currentIndex = pro.currentIndex - 1;
    if(pro.currentIndex < 0)
        pro.currentIndex = pro.selectedImageArray.count - 1;
    SelectedImage* si = [pro.selectedImageArray objectAtIndex:pro.currentIndex];
    [self loadImage:si inView:imageView];
}
- (void) playForwardHandler: (UITapGestureRecognizer*)tap
{
    NSLog(@"play forward handler");
    SEPlayView* view = (SEPlayView*)tap.view;
    int row = view.row;
    NSLog(@"## row = %d ###", row);
    UITableViewCell* cell = [mMusicImageListView.mImageListTableView cellForRowAtIndexPath:[NSIndexPath indexPathForRow:row inSection:0]];
    SEImageViewForImageList* imageView = [self getLeftIconView:cell];
    assert(imageView != nil);
    NSLog(@"image list url count = %d", mMusicImageListView.mImageListURLPropertyArray.count);
    SEImageListURLProperty* pro = [mMusicImageListView.mImageListURLPropertyArray objectAtIndex:row];
    if(pro.selectedImageArray == nil)
        return;
    pro.currentIndex = pro.currentIndex + 1;
    if(pro.currentIndex >= pro.selectedImageArray.count)
        pro.currentIndex = 0;
    SelectedImage* si = [pro.selectedImageArray objectAtIndex:pro.currentIndex];
    [self loadImage:si inView:imageView];
    /*
    ImageList* il = [mViewNav getImageListBySeq:[NSNumber numberWithInt:row]];
    NSSet* siSet = il.selectedimage;
    NSArray* array = [siSet allObjects];
    for(int i = 0 ; i < array.count ; i++)
    {
        SelectedImage* si = [array objectAtIndex:i];
        if(si == nil || si.url == nil)
            continue;
        SEImageListLoader* loader = [[SEImageListLoader alloc] init];
        loader.imageView = imageView;
        SEImageAsyncLoader* asyncLoader = [[SEImageAsyncLoader alloc] init];
        asyncLoader.mViewNav = (SEViewNavigator*)mViewNav;
        ALAssetsLibrary* lib = [[ALAssetsLibrary alloc] init];
        [asyncLoader setAssetLibOwn:lib];
        [lib release];
        NSURL* url = [NSURL URLWithString:si.url];
        //NSString* date = si.urldate;
        [asyncLoader loadImageThumbnailFromPhotoLib:url size:CGSizeMake(100, 100) withHandler:loader];
    }
     */
}
- (void) setHandler: (UITableViewCell*)cell
{
    SEIndicatorView* indicator = [self getIndicatorView:cell];
    UITapGestureRecognizer* tapGes = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(indicatorTapHandler:)];
    [indicator addGestureRecognizer:tapGes];
    [tapGes release];
    
    SEImageViewForImageList* imageAlbum = [self getLeftIconView:cell];
    imageAlbum.userInteractionEnabled = YES;
    UITapGestureRecognizer* ges = [[UITapGestureRecognizer alloc] initWithTarget:mMusicImageListView action:@selector(handleImageClick:)];
    [imageAlbum addGestureRecognizer:ges];
    [ges release];
    
    UIImageView* backView = [self getPlayBackView:cell];
    backView.userInteractionEnabled = YES;
    UITapGestureRecognizer* backViewGes = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(playBackHandler:)];
    [backView addGestureRecognizer:backViewGes];
    [backViewGes release];
    
    UIImageView* forwardView = [self getPlayForwardView:cell];
    forwardView.userInteractionEnabled = YES;
    UITapGestureRecognizer* forwardGes = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(playForwardHandler:)];
    [forwardView addGestureRecognizer:forwardGes];
    [forwardGes release];
    
    UIImageView* deleteView = [self getDeleteView:cell];
    deleteView.userInteractionEnabled = YES;
    UITapGestureRecognizer* deleteGes = [[UITapGestureRecognizer alloc] initWithTarget:self  action:@selector(deleteHandler:)];
    [deleteView addGestureRecognizer:deleteGes];
    [deleteGes release];
}
- (void) setAllWidgetBackground: (BOOL) highlighted cell:(UITableViewCell*)cell
{
    if(highlighted)
    {
        UIImageView* imageAlbumBg = [self getLeftIconBgView:cell];
        imageAlbumBg.image = [mResLoader getImage:@"MusicImageImageListNoImageIcon"];
        UIImageView* nameBackground = [self getNameBgView:cell];
        UIImage* image = [mResLoader getImage:@"MusicImageMusicListItemNormalNameImage"];
        nameBackground.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
        UIImageView* numberBackground = [self getNumberBgView:cell];
        image = [mResLoader getImage:@"MusicImageMusicListItemNormalNumberImage"];
        numberBackground.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
        UIImageView* playBgView = [self getPlayBgView:cell];
        playBgView.image = [mResLoader getImage:@"MusicImageListPlayBg" withCapInset:UIEdgeInsetsMake(0.1, 0.1, 0.1, 0.1)];
    }
    else 
    {
        UIImageView* imageAlbumBg = [self getLeftIconBgView:cell];
        imageAlbumBg.image = [mResLoader getImage:@"MusicImageImageListNoImageIcon"];
        UIImageView* nameBackground = [self getNameBgView:cell];
        UIImage* image = [mResLoader getImage:@"MusicImageMusicListItemNormalNameImage"];
        nameBackground.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
        UIImageView* numberBackground = [self getNumberBgView:cell];
        image = [mResLoader getImage:@"MusicImageMusicListItemNormalNumberImage"];
        numberBackground.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
        UIImageView* playBgView = [self getPlayBgView:cell];
        playBgView.image = [mResLoader getImage:@"MusicImageListPlayBg" withCapInset:UIEdgeInsetsMake(0.1, 0.1, 0.1, 0.1)];
    }
}
- (void) updateIndicator
{
    NSInteger rowNum = [self numberOfRowsInSection:0];
    for(NSInteger i = 0 ; i < rowNum ; i++)
    {
        UITableViewCell* cell = [self cellForRowAtIndexPath:[NSIndexPath indexPathForRow:i inSection:0]];
        ImageList* il = [mViewNav getImageListBySeq:[NSNumber numberWithInt:i]];
        if(il)
        {
            MusicList* ml = [ mViewNav getMusicListByImageList:il.name];
            if(ml)
            {
                [self setIndicatorImage:YES cell:cell];
            }
            else 
            {
                [self setIndicatorImage:NO cell:cell];
            }
        }
        else 
        {
            [self setIndicatorImage:NO cell:cell];
        }
        
    }

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
    
    UIImageView* background = [self getBackgroundView:cell];
    int row = indexPath.row;
    //BOOL hasAttached = [mMusicImageListView hasAttachedToMusicList: indexPath.row];
    ImageList* il = [mViewNav getImageListBySeq:[NSNumber numberWithInt:indexPath.row]];
    if(il == nil)
    {
        UIImageView* imageView = (UIImageView*) [self getPlusIconView:cell];
        imageView.userInteractionEnabled = YES;
        UIImage* image = [mViewNav.mResLoader getImage:@"MusicImageListPlusIcon"];
        imageView.image = image;
        NSLog(@"image plus icon frame = %f, %f", imageView.frame.size.width, imageView.frame.size.height);
        
        UITapGestureRecognizer* ges = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(addHandler:)];
        [imageView addGestureRecognizer:ges];
        [ges release];
        
        SEPlayView* playView = [self getPlayBackView:cell];
        playView.hidden = YES;
        
        playView = [self getPlayForwardView:cell];
        playView.hidden = YES;
        
    }
    NSLog(@"il.name = %@", il.name);
    
    UserInfo* userInfo = [mViewNav getUserInfo];
    NSLog(@"current list = %@", userInfo.currentimagelist);
    BOOL isSelectedView = [il.name isEqualToString:userInfo.currentimagelist];
    if(isSelectedView)
    {
        background.image = [mResLoader getImage:@"MusicImageImageListItemSelectedBackground"];
    }
    else
    {
        background.image = [mResLoader getImage:@"MusicImageImageListItemNormalBackground"];
    }
    
    if(isSelectedView)
    {
        [self setAllWidgetBackground:YES cell:cell];
    }
    else
    {
        [self setAllWidgetBackground:NO cell:cell];
    }
    UILabel* number = [self getNumberLabel:cell];
    number.font = [UIFont fontWithName:[SESystemConfig getFontName] size:20];
    UILabel* nameLabel = [self getNameLabel:cell];
    nameLabel.font = [UIFont fontWithName:[SESystemConfig getFontName] size:30];
    if(il == nil)
    {
        nameLabel.font = [UIFont fontWithName:[SESystemConfig getFontName] size:20];
        nameLabel.textColor = [UIColor whiteColor];
    }
    //imageAlbumBg.userInteractionEnabled = YES;
    
    SEImageViewForImageList* imageAlbum = [self getLeftIconView:cell];
    imageAlbum.row = row;
    
    SEPlayView* playView = [self getPlayBackView:cell];
    playView.row = row;
    
    playView = [self getPlayForwardView:cell];
    playView.row = row;
    
    SEIndicatorView* indicator = [self getIndicatorView:cell];
    indicator.row = indexPath.row;
    indicator.mType = IMAGE_INDICATION;
    indicator.userInteractionEnabled = YES;
    indicator.mTouchHandler = mMusicImageListView;
    indicator.mResLoader = mResLoader;
    if(il == nil)
    {
        indicator.image = [mResLoader getImage:@"MusicImageImageListNormalIndicator"];
    }
    else
    {
        NSArray* attachedMusicList = [mViewNav getAttachedMusicListByImageListName:il.name];
        if(attachedMusicList.count > 0)
        {
            indicator.image = [mResLoader getImage:@"MusicImageImageListNormalIndicatorH"];
        }
        else
        {
            indicator.image = [mResLoader getImage:@"MusicImageImageListNormalIndicator"];
        }
    }
    
    SEImageViewForImageList* imageDisplayView = [self getLeftIconView:cell];
    if(il.name != nil)
    {
        [self loadSelectedImageBySeq:row : imageDisplayView];
    }
    else
    {
        UIImageView* imageAlbumBg = [self getLeftIconBgView:cell];
        float startx = imageAlbumBg.frame.origin.x + (imageAlbumBg.frame.size.width - 82) / 2;
        float starty = imageAlbumBg.frame.origin.y + (imageAlbumBg.frame.size.height - 82) / 2;
        imageDisplayView.frame = CGRectMake(startx, starty, 82,82);
    }
    ImageList* imageList = [mViewNav getImageListBySeq:[NSNumber numberWithInt:indexPath.row]];
    NSString* name = imageList.name;
    if(il)
    {
        nameLabel.text = name;
    }
    else
    {
        nameLabel.text = @"please click + to add image list";
        nameLabel.numberOfLines = 2;
    }
    int count = imageList.selectedimage.count;
    
    if(il)
    {
        number.text = [NSString stringWithFormat:@"%d", count];
    }
    else {
        number.text = @"";
    }
    

    UIImageView* deleteView = [self getDeleteView:cell];
    if(il == nil || [il.name isEqualToString:[SESystemConfig getDefaultImageListName]])
    {
        deleteView.image = nil;
    }
    else 
    {
        deleteView.image = [mResLoader getImage:@"MusicImageDeleteIcon"];
    }
    [cell bringSubviewToFront:deleteView];
    if(il != nil)
        [self setHandler:cell];
    return cell;
}

- (void) removeOtherLineView: (int) row
{
}
- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath
{
    //SEMusicListTableView* musicTableView = mMusicImageListView.mMusicListTableView;
    NSLog(@"ImageListTableVeiw select change");
    UserInfo* userInfo = [mViewNav getUserInfo];
    ImageList* currentImageList = [mViewNav getImageListByName:userInfo.currentimagelist];
    if([currentImageList.seq intValue] == indexPath.row)
        return;
    int oldRow = [currentImageList.seq intValue];
    ImageList* il = [mViewNav getImageListBySeq:[NSNumber numberWithInt:indexPath.row]];
    if(il == nil)
        return;
    UITableViewCell* cell = [tableView cellForRowAtIndexPath:indexPath];
    [self deselectImageList:currentImageList];
    //[mMusicImageListView removeLineViewWhenSwitch:oldRow tableType:IMAGE_TABLE];
    [mMusicImageListView changeLineViewWhenSwitch:oldRow tableType:IMAGE_TABLE];
    [mMusicImageListView drawAttachFrom: indexPath.row tableType: IMAGE_TABLE offset:tableView.contentOffset];
    [self selectImageCell:cell];
    userInfo.currentimagelist = il.name;
    [mViewNav saveCoreDataContext];
}
- (void) deselectImageList: (ImageList*)imageList
{
    NSArray* visibleCells = self.visibleCells;
    for(UITableViewCell* cell in visibleCells)
    {
        if(cell.tag == [imageList.seq intValue])
        {
            [self deselectImageCell:cell];
        }
    }
}
- (UIImageView*) tableViewCellBackground: (NSIndexPath*)indexPath
{
    UITableViewCell* cell = [self cellForRowAtIndexPath:indexPath];
    UIImageView* background = (UIImageView*)[cell viewWithTag:101];
    return background;
}
- (void) deselectImageCell: (UITableViewCell*)cell
{
    UIImageView* background = [self getBackgroundView:cell];
    background.image = [mResLoader getImage:@"MusicImageImageListItemNormalBackground"];    
    [self setAllWidgetBackground:NO cell:cell];
}
- (void) selectImageCell: (UITableViewCell*)cell
{
    UIImageView* background = [self getBackgroundView:cell];
    background.image = [mResLoader getImage:@"MusicImageImageListItemSelectedBackground"];  
    [self setAllWidgetBackground:YES cell:cell];
}
@end

@interface SEMusicListTableView ()
- (UIImage*) getArtwork:(MusicList*)ml index: (int)index size: (CGSize)size;
- (CGSize) getArtworkSize : (UITableViewCell*)cell;
@end

@implementation SEMusicListTableView
@synthesize mViewNav;
@synthesize mMusicListPropertyArray;
@synthesize mResLoader;
@synthesize mMusicImageListView;
@synthesize mMusicItems;
- (void) dealloc
{
    [mMusicListPropertyArray release];
    [mMusicItems release];
    [super dealloc];
}
- (SEIndicatorView*) getIndicatorView: (UITableViewCell*)cell
{
    return (SEIndicatorView*)[cell viewWithTag:101];
}
- (UIImageView*) getNameBgView: (UITableViewCell*)cell
{
    return (UIImageView*) [cell viewWithTag:103];
}
- (UILabel*) getNameLabel: (UITableViewCell*)cell
{
    return (UILabel*)[cell viewWithTag:107];
}
- (UIImageView*) getNumberBgView: (UITableViewCell*)cell
{
    return (UIImageView*) [cell viewWithTag:105];
}
- (UILabel*) getNumberLabel: (UITableViewCell*)cell
{
    return (UILabel*) [cell viewWithTag:109];
}
- (UIImageView*) getBackgroundView: (UITableViewCell*)cell
{
    return (UIImageView*)[cell viewWithTag:106];
}
- (UIImageView*) getMusicIconBgView: (UITableViewCell*)cell
{
    return (UIImageView*)[cell viewWithTag:102];
}
- (UIImageView*) getMusicArtworkView: (UITableViewCell*)cell
{
    return (UIImageView*)[cell viewWithTag:150];
}
- (CGSize) getArtworkSize: (UITableViewCell*)cell
{
    UIImageView* view = [self getMusicArtworkView:cell];
    return view.frame.size;
}
- (SEPlayView*) getMusicIconView: (UITableViewCell*)cell
{
    return (SEPlayView*) [cell viewWithTag:120];
}
- (UIImageView*) getPlayBgView: (UITableViewCell*)cell
{
    return (UIImageView*)[cell viewWithTag:104];
}
- (SEPlayView*) getPlayPauseView: (UITableViewCell*)cell
{
    return (SEPlayView*) [cell viewWithTag:122];
}
- (SEPlayView*) getPlayNextView: (UITableViewCell*)cell
{
    return (SEPlayView*) [cell viewWithTag:123];
}
- (SEPlayView*) getPlayPrevView: (UITableViewCell*)cell
{
    return (SEPlayView*) [cell viewWithTag:121];
}
- (UIImageView*) getDeleteView: (UITableViewCell*) cell
{
    return (UIImageView*)[cell viewWithTag:124];
}
- (UIImageView*) getPlusIconView: (UITableViewCell*)cell
{
    return (UIImageView*)[cell viewWithTag:130];
}
- (void) pauseCellMusicPlay
{
    
}
- (void) selectMusicCellForImageList: (ImageList*)imageList
{
    /*
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
    */
}
/*
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
 */
- (void) deselectHighlightedCell
{
    /*
    NSIndexPath* musicIndexPath = [self indexPathForSelectedRow];
    [self deselectRowAtIndexPath:musicIndexPath animated:NO];
    
    UITableViewCell* cell = [self cellForRowAtIndexPath:musicIndexPath];
    UIImageView* fullBg = (UIImageView*)[cell viewWithTag:110];
    fullBg.image = nil;
     */
}
- (void) setAllWidgetBackground: (BOOL) highlighted cell: (UITableViewCell*)cell
{
    if(highlighted)
    {
        UIImageView* imageView = [self getNameBgView:cell];
        UIImage* image = [mViewNav.mResLoader getImage:@"MusicImageMusicListItemNormalNameImage"];
        imageView.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
        
        imageView = [self getNumberBgView:cell];
        image = [mViewNav.mResLoader getImage:@"MusicImageMusicListItemNormalNumberImage"];
        imageView.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
        
        imageView = [self getPlayBgView:cell];
        image = [mViewNav.mResLoader getImage:@"MusicImageMusicListItemPlayBg"];
        imageView.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
        
        imageView = [self getMusicIconBgView:cell];
        image = [mViewNav.mResLoader getImage:@"MusicImageMusicListItemNormalNameImage"];
        imageView.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
    }
    else
    {
        UIImageView* imageView = [self getNameBgView:cell];
        UIImage* image = [mViewNav.mResLoader getImage:@"MusicImageMusicListItemNormalNameImage"];
        imageView.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
        
        imageView = [self getNumberBgView:cell];
        image = [mViewNav.mResLoader getImage:@"MusicImageMusicListItemNormalNumberImage"];
        imageView.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
        
        imageView = [self getPlayBgView:cell];
        image = [mViewNav.mResLoader getImage:@"MusicImageMusicListItemPlayBg"];
        imageView.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
        
        imageView = [self getMusicIconBgView:cell];
        image = [mViewNav.mResLoader getImage:@"MusicImageMusicListItemNormalNameImage"];
        imageView.image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
    }
}
- (void) setIndicatorImage:(BOOL) highlighted indicator: (SEIndicatorView*)indicator
{
    UIImage* indicatorImage = nil;
    if(highlighted)
    {
        indicatorImage = [mResLoader getImage:@"MusicImageListIndicatorMusicNormalIconH"];
    }
    else
    {
        indicatorImage = [mResLoader getImage:@"MusicImageListIndicatorMusicNormalIcon"];
    }
    indicator.image = indicatorImage;
}
- (void) setIndicatorImage: (BOOL) highlighted cell: (UITableViewCell*)cell
{
    SEIndicatorView* indicator = (SEIndicatorView*)[self getIndicatorView:cell];
    [self setIndicatorImage:highlighted indicator:indicator];
}
- (void) updateIndicator
{
    NSInteger rowNum = [self numberOfRowsInSection:0];
    for(NSInteger i = 0 ; i < rowNum ; i++)
    {
        UITableViewCell* cell = [self cellForRowAtIndexPath:[NSIndexPath indexPathForRow:i inSection:0]];
        MusicList* ml = [mViewNav getMusicListBySeq:[NSNumber numberWithInt:i]];
        if(ml)
        {
            NSArray* imageListArray = [mViewNav getMusicAttachedImage:ml.name];
            if(imageListArray.count > 0)
            {
                [self setIndicatorImage:YES cell:cell];
            }
            else 
            {
                [self setIndicatorImage:NO cell:cell];
            }
        }
        else 
        {
            [self setIndicatorImage:NO cell:cell];
        }
        
    }
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
            UIImageView* background = [self getBackgroundView:cell];
            background.image = [mResLoader getImage:@"MusicImageMusicListItemNormalBackground"];
            [self setAllWidgetBackground:NO cell:cell];
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
    return count + 1;
}
- (void)indicatorTapHandler: (UITapGestureRecognizer*)tap
{
    NSLog(@"music indicator tap");
    SEIndicatorView* tapView = (SEIndicatorView*)tap.view;
    int row = tapView.row;
    NSLog(@"## remove line at %d", row);
    if([mMusicImageListView canRemoveLineView:row tableType:MUSIC_TABLE])
    {
        MusicList* ml = [mViewNav getMusicListBySeq:[NSNumber numberWithInt:row]];
        UserInfo* userInfo = [mViewNav getUserInfo];
        MusicList* currentAttachedMusic = [mViewNav getMusicListByImageList:userInfo.currentimagelist];
        if([currentAttachedMusic.name isEqualToString:ml.name])
        {
            mViewNav.mMusicPlayListChange = YES;
        }
        [mMusicImageListView removeMusicRowLine:row];
        //[self updateIndicator];
        [self setIndicatorImage:NO indicator:tapView];
        [mMusicImageListView updateImageListTableViewIndicator];
        
    }
}
- (void) playPauseHandler: (UITapGestureRecognizer*)tap;
{
    NSLog(@"play pause music list handler");
    MPMusicPlayerController* player = [MPMusicPlayerController applicationMusicPlayer];
    SEPlayView* playView = (SEPlayView*)tap.view;
    //UITableViewCell* cell = (UITableViewCell*)playView.superview.superview;
    MusicList* musicList  = [mViewNav getMusicListBySeq:[NSNumber numberWithInt:playView.row]];
    NSLog(@"#### musicList = %@ ######\n", musicList.name);
    if(musicList == nil)
        return;
    int row = playView.row;
    SEMusicItem* musicItem = [mMusicItems objectAtIndex:row];
    assert(row == musicItem.row);
    mViewNav.mMusicPlayListChange = YES;
    if(musicItem.play == YES)
    {
        playView.play = NO;
        musicItem.play = NO;
        [player pause];
        playView.image = [mViewNav.mResLoader getImage:@"MusicPlayIcon"];
        return;
    }
    
    NSArray* selectedMusic = getMusicInMusicList(musicList);
    int count = selectedMusic.count;
    if(count == 0)
        return;
    [self pauseMusic];
    playView.play = YES;
    musicItem.play = YES;
    playView.image = [mViewNav.mResLoader getImage:@"MusicPauseIcon"];
    for(SelectedMusic* sm in selectedMusic)
    {
        if([sm.seq intValue] == musicItem.currSeq)
        {
            NSLog(@"row = %d, sm title = %@, artist = %@, album = %@", row, sm.title, sm.singer, sm.album);
            [mViewNav playMusicWithTitle:sm.title artist:sm.singer album:sm.album];
            break;
        }
    }

}
- (void) playNextHandler: (UITapGestureRecognizer*)tap
{
    NSLog(@"play next handler");;
    SEPlayView* playView = (SEPlayView*)tap.view;
    MusicList* musicList  = [mViewNav getMusicListBySeq:[NSNumber numberWithInt:playView.row]];
    NSLog(@"#### musicList = %@ ######\n", musicList.name);
    if(musicList)
    {
        mViewNav.mMusicPlayListChange = YES;
        //NSSet* selectedMusic = musicList.selectedmusic;
        NSArray* selectedMusic = getMusicInMusicList(musicList);
        int count = selectedMusic.count;
        if(count == 0)
            return;
        
        int row = playView.row;
        UITableViewCell* cell = (UITableViewCell*)[self cellForRowAtIndexPath:[NSIndexPath indexPathForRow:row inSection:0]];
        SEMusicItem* musicItem = [mMusicItems objectAtIndex:row];
        assert(row == musicItem.row);
        
        musicItem.currSeq = musicItem.currSeq + 1;
        if(musicItem.currSeq >= count)
        {
            musicItem.currSeq = 0;
        }
        playView.currSeq = musicItem.currSeq;
        UIImage* artWork = [self getArtwork:musicList index:playView.currSeq size:[self getArtworkSize:cell]];
        UIImageView* artworkView = [self getMusicArtworkView:cell];
        artworkView.image = artWork;
        SEPlayView* currentPlayerView = [self getPlayPauseView:cell];
        currentPlayerView.image = [mViewNav.mResLoader getImage:@"MusicPlayIcon"];
        MPMusicPlayerController* player = [MPMusicPlayerController applicationMusicPlayer];
        [player pause];
        currentPlayerView.play = NO;
        musicItem.play = NO;
        currentPlayerView.currSeq = playView.currSeq;
        /*
        MPMediaQuery* query = [MPMediaQuery songsQuery];
        for(SelectedMusic* sm in selectedMusic)
        {
            if([sm.seq intValue] == playView.currSeq)
            {
                NSLog(@"sm title = %@, artist = %@, album = %@", sm.title, sm.singer, sm.album);
                NSString* title = sm.title;
                if(title == nil)
                {
                    title = @"unknown";
                    return;
                }
                NSString* artist = sm.singer;
                if(artist == nil)
                    artist = @"unknown";
                NSString* album = sm.album;
                if(album == nil)
                    album = @"unknown";
                [mViewNav playMusicWithTitle:title artist:artist album:album];
                break;
            }
        }
         */
    }

}
- (void) playPrevHandler: (UITapGestureRecognizer*)tap
{
    NSLog(@"play prev handler");
    SEPlayView* playView = (SEPlayView*)tap.view;
    MusicList* musicList  = [mViewNav getMusicListBySeq:[NSNumber numberWithInt:playView.row]];
    
    NSLog(@"#### musicLis = %@ ######\n", musicList.name);
    if(musicList)
    {
        /*
        NSSet* selectedMusic = musicList.selectedmusic;
        int count = selectedMusic.count;
         */
        mViewNav.mMusicPlayListChange = YES;
        NSArray* selectedMusic = getMusicInMusicList(musicList);
        int count = selectedMusic.count;
        if(count == 0)
            return;
        
        int row = playView.row;
        UITableViewCell* cell = (UITableViewCell*)[self cellForRowAtIndexPath:[NSIndexPath indexPathForRow:row inSection:0]];
        SEMusicItem* musicItem = [mMusicItems objectAtIndex:row];
        assert(musicItem.row == row);
        musicItem.currSeq = musicItem.currSeq - 1;
        if(musicItem.currSeq < 0)
        {
            musicItem.currSeq = count - 1;
        }
        playView.currSeq = musicItem.currSeq;
        
        UIImage* artWork = [self getArtwork:musicList index:playView.currSeq size:[self getArtworkSize:cell]];
        UIImageView* artworkView = [self getMusicArtworkView:cell];
        artworkView.image = artWork;
        MPMusicPlayerController* player = [MPMusicPlayerController applicationMusicPlayer];
        [player pause];
        
        
        SEPlayView* currentPlayerView = [self getPlayPauseView:cell];
        currentPlayerView.play = NO;
        musicItem.play = NO;
        currentPlayerView.image = [mViewNav.mResLoader getImage:@"MusicPlayIcon"];
        currentPlayerView.currSeq = playView.currSeq;
        /*
        for(SelectedMusic* sm in selectedMusic)
        {
            if([sm.seq intValue] == playView.currSeq)
            {
                NSLog(@"sm title = %@, artist = %@, album = %@", sm.title, sm.singer, sm.album);
                NSString* title = sm.title;
                if(title == nil)
                {
                    title = @"unknown";
                    return;
                }
                NSString* artist = sm.singer;
                if(artist == nil)
                    artist = @"unknown";
                NSString* album = sm.album;
                if(album == nil)
                    album = @"unknown";
                [mViewNav playMusicWithTitle:title artist:artist album:album];
                break;
            }
        }
         */
    }

}
- (void) addHandler: (UITapGestureRecognizer*) tap
{
    NSLog(@"add handler");
    [mMusicImageListView addMusicListButtonHandler];
}
- (void) deleteHandler: (UITapGestureRecognizer*)tap
{
    NSLog(@"delete music handler");
    UIView* v = tap.view;
    int row = [mMusicImageListView getViewRow:v];
    mMusicImageListView.mNeedDeleteRow = row;
    mMusicImageListView.mTableType = MUSIC_TABLE;
    [mViewNav createConfirmDlg:mMusicImageListView ok:@selector(removeButtonHandler) cancel:@selector(cancelHandler:)];
    MusicList* ml = [mViewNav getMusicListBySeq:[NSNumber numberWithInt:row]];
    NSString* str = [NSString stringWithFormat:@"Do you want to delete music list : \"%@\"", ml.name];
    SEConfirmDlg* dlg = [mViewNav getConfirmDlg];
    [dlg setMessageText:str];
    [mViewNav showConfirmDlg];
}
- (void) setHandler:(UITableViewCell*)cell
{
    SEIndicatorView* indicator = (SEIndicatorView*)[self getIndicatorView:cell];
    UITapGestureRecognizer* tapGes = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(indicatorTapHandler:)];
    [indicator addGestureRecognizer:tapGes];
    [tapGes release];
    
    UIImageView* musicAlbumView = [self getMusicIconView:cell];
    musicAlbumView.userInteractionEnabled = YES;
    UITapGestureRecognizer* ges = [[UITapGestureRecognizer alloc] initWithTarget:mMusicImageListView action:@selector(handleMusicClick:)];
    [musicAlbumView addGestureRecognizer:ges];
    [ges release];
    
    UIImageView* playPauseView = [self getPlayPauseView:cell];
    playPauseView.userInteractionEnabled = YES;
    UITapGestureRecognizer* playPauseGes = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(playPauseHandler:)];
    [playPauseView addGestureRecognizer:playPauseGes];
    [playPauseGes release];
    
    UIImageView* playNextView = [self getPlayNextView:cell];
    playNextView.userInteractionEnabled = YES;
    UITapGestureRecognizer* playNextGes = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(playNextHandler:)];
    [playNextView addGestureRecognizer:playNextGes];
    [playNextGes release];
    
    UIImageView* playPrevView = [self getPlayPrevView:cell];
    playPrevView.userInteractionEnabled = YES;
    UITapGestureRecognizer* playPrevGes = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(playPrevHandler:)];
    [playPrevView addGestureRecognizer:playPrevGes];
    [playPrevGes release];
    
    UIImageView* deleteView = [self getDeleteView:cell];
    deleteView.userInteractionEnabled = YES;
    UITapGestureRecognizer* deleteGes = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(deleteHandler:)];
    [deleteView addGestureRecognizer:deleteGes];
    [deleteGes release];
    
}
- (UIImage*) getArtwork:(MusicList*)ml index: (int)index size: (CGSize)size
{
    NSArray* set = getMusicInMusicList(ml);
    if(set.count == 0)
        return nil;
    if(index < 0 || index >= set.count)
        return nil;
    SelectedMusic* sm = [set objectAtIndex:index];
    NSString* title = sm.title;
    NSString* album = sm.album;
    NSString* artist = sm.singer;
    NSLog(@"title = %@", title);
    NSLog(@"album = %@", album);
    NSLog(@"artist = %@", sm.singer);
    MPMediaPropertyPredicate *artistNamePredicate = nil;
    if(artist != nil)
    {
        artistNamePredicate =  [MPMediaPropertyPredicate predicateWithValue: artist
                                                                forProperty: MPMediaItemPropertyArtist];
    }
    
    MPMediaPropertyPredicate *albumNamePredicate = nil;
    if(album != nil)
    {
        albumNamePredicate =  [MPMediaPropertyPredicate predicateWithValue: album
                                                               forProperty: MPMediaItemPropertyAlbumTitle];
    }
    
    MPMediaPropertyPredicate* titlePredicate = [MPMediaPropertyPredicate predicateWithValue:title forProperty:MPMediaItemPropertyTitle];
    
    MPMediaQuery *myComplexQuery = [[MPMediaQuery alloc] init];
    
    if(artist)
    {
        [myComplexQuery addFilterPredicate: artistNamePredicate];
    }
    if(album)
    {
        [myComplexQuery addFilterPredicate: albumNamePredicate];
    }
    [myComplexQuery addFilterPredicate:titlePredicate];
    NSArray *items = [myComplexQuery items];
    if(items.count == 0)
    {
        [myComplexQuery release];
        return nil;
    }
    MPMediaItem* mitem = [items objectAtIndex:0];
    MPMediaItemArtwork* artWork = [mitem valueForKey:MPMediaItemPropertyArtwork];
    
    UIImage* alphaImage = [mViewNav.mResLoader getImage:@"MusicImageMusicArtworkIcon"];
    CGSize s = CGSizeMake(alphaImage.size.width, alphaImage.size.height);
    UIImage* image = [artWork imageWithSize:s];
    if(image != nil)
    {
        UIGraphicsBeginImageContext(CGSizeMake(alphaImage.size.width, alphaImage.size.height));
        [image drawInRect:CGRectMake(0, 0, s.width, s.height)];
        image = UIGraphicsGetImageFromCurrentImageContext();
        UIGraphicsEndImageContext();
        image = [SEUtil createImageWithColorImage:image alphaImage:alphaImage];
    }
    [myComplexQuery release];
    return image;
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

    SEIndicatorView* indicator = (SEIndicatorView*)[self getIndicatorView:cell];
    indicator.row = row;
    UIImage* indicatorImage = nil;
    MusicList* ml = [mViewNav getMusicListBySeq:[NSNumber numberWithInt:row]];
    if(ml == nil)
    {
        UIImageView* imageView = (UIImageView*) [self getMusicIconView:cell];
        NSLog(@"plus icon frame = %f, %f", imageView.frame.size.width, imageView.frame.size.height);
        UIImage* image = [mViewNav.mResLoader getImage:@"MusicImageListPlusIcon"];
        imageView.image = image;
        UITapGestureRecognizer* addGes = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(addHandler:)];
        [imageView addGestureRecognizer:addGes];
        [addGes release];
        
        SEPlayView* playView = [self getPlayNextView:cell];
        playView.hidden = YES;
        playView = [self getPlayPrevView:cell];
        playView.hidden = YES;
        playView = [self getPlayPauseView:cell];
        playView.hidden = YES;
    }
    SEMusicItem* musicItem = nil;
    if(row < mMusicItems.count)
    {
        musicItem = [mMusicItems objectAtIndex:row];
        assert(row == musicItem.row);
    }
    UserInfo* userInfo = [mViewNav getUserInfo];
    //BOOL isSelected = [ml.name isEqualToString:userInfo.currentmusiclist];
    SEPlayView* playView = [self getPlayNextView:cell];
    playView.row = row;
    playView.currSeq = musicItem.currSeq;
    playView.play = musicItem.play;
    playView = [self getPlayPrevView:cell];
    playView.row = row;
    playView.currSeq = musicItem.currSeq;
    playView.play = musicItem.play;
    playView = [self getPlayPauseView:cell];
    playView.row = row;
    playView.currSeq = musicItem.currSeq;
    playView.play = musicItem.play;
    if(musicItem.play)
    {
        playView.image = [mViewNav.mResLoader getImage:@"MusicPauseIcon"];
    }
    else 
    {
        playView.image = [mViewNav.mResLoader getImage:@"MusicPlayIcon"];;
    }
    if(ml == nil)
    {
        indicatorImage = [mResLoader getImage:@"MusicImageListIndicatorMusicNormalIcon"];
    }
    else 
    {
        NSArray* attachedImageList = [mViewNav getMusicAttachedImage:ml.name];
        if(attachedImageList.count > 0)
        {
            indicatorImage = [mResLoader getImage:@"MusicImageListIndicatorMusicNormalIconH"];
        }
        else
        {
            indicatorImage = [mResLoader getImage:@"MusicImageListIndicatorMusicNormalIcon"];    
        }
    }
    indicator.mType = MUSIC_INDICATION;
    indicator.image = indicatorImage;
    indicator.userInteractionEnabled = YES;
    indicator.mTouchHandler = mMusicImageListView;
    indicator.mResLoader = mResLoader;
    
    
    SEPlayView* musicAlbumView = [self getMusicIconView:cell];
    musicAlbumView.row = indexPath.row;
    musicAlbumView.userInteractionEnabled = YES;
    musicAlbumView.image = [mResLoader getImage:@"MusicImageMusicNoAlbumIcon"];
    CGSize noSize = musicAlbumView.image.size;
    NSLog(@"nosize = %f , %f", noSize.width, noSize.height);
    UIImage* artwork = [self getArtwork:ml index:0 size:musicAlbumView.frame.size];
    BOOL hasAlbum = artwork != nil;
    if(hasAlbum)
    {
        UIImageView* artworkView = [self getMusicArtworkView:cell];
        artworkView.image = artwork;//[mResLoader getImage:@"MusicImageMusicNoAlbumIcon"];
    }
    /*
    if(isSelected)
    {
        [self setAllWidgetBackground:YES cell:cell];
    }
    else 
     */
    {
        [self setAllWidgetBackground:NO cell:cell];
    }
    
    UILabel* musicListName = (UILabel*)[self getNameLabel:cell];
    musicListName.font = [UIFont fontWithName:[SESystemConfig getFontName] size:30];
    if(ml == nil)
    {
        musicListName.font = [UIFont fontWithName:[SESystemConfig getFontName] size:20];
        musicListName.textColor = [UIColor whiteColor];
    }
    if (ml == nil) 
    {
        musicListName.text = @"please click + to add music list";
        musicListName.numberOfLines = 2;
    }
    else
    {
        musicListName.text = ml.name;
    }
    UILabel* musicNumber = (UILabel*)[self getNumberLabel:cell];
    musicNumber.font = [UIFont fontWithName:[SESystemConfig getFontName] size:20];
    musicNumber.textAlignment = UITextAlignmentCenter;
    if(ml)
    {
        musicNumber.text = [NSString stringWithFormat:@"%d", ml.selectedmusic.count];
    }
    else {
        musicNumber.text = @"";
    }
    UIImageView* background = [self getBackgroundView:cell];
    
    //ImageList* currentImageList = [mViewNav getImageListByName:userInfo.currentimagelist];
    //MusicList* attachedMusicList = [mViewNav getMusicListByImageList:currentImageList.name];
    
    /*
    if(isSelected)
    {
        background.image = [mResLoader getImage:@"MusicImageMusicListItemSelectedBackground"];
    }
    else
     */
    {
        background.image = [mResLoader getImage:@"MusicImageMusicListItemNormalBackground"];
    }
    UIImageView* deleteView = [self getDeleteView:cell];
    if(ml == nil || [ml.name isEqualToString:[SESystemConfig getDefaultMusicListName]])
    {
        deleteView.image = nil;
    }
    else
    {
        deleteView.image = [mResLoader getImage:@"MusicImageDeleteIcon"];
    }
    if(ml != nil)
        [self setHandler:cell];
    UIImageView* plusIconView = [self getPlusIconView:cell];
    if(ml == nil)
    {
        UIImage* plusImage = [mResLoader getImage:@"MusicImageListPlusIcon"];
        NSLog(@"plus icon frame = %f, %f", plusIconView.frame.size.width, plusIconView.frame.size.height);
        plusIconView.image = plusImage;
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
- (void)highlightCell: (NSIndexPath*) indexPath
{
    UITableViewCell* cell = [self cellForRowAtIndexPath:indexPath];
    UIImageView* fullBg = (UIImageView*)[self getBackgroundView:cell];
    fullBg.image = [mResLoader getImage:@"MusicImageMusicListItemSelectedBackground"];
    [self setAllWidgetBackground:YES cell:cell];
}
- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath
{
    NSLog(@"music list table select change");
    MusicList* ml = [mViewNav getMusicListBySeq:[NSNumber numberWithInt:indexPath.row]];
    if(ml == nil)
        return;
    UserInfo* userInfo = [mViewNav getUserInfo];
    NSString* musicListName = userInfo.currentmusiclist;
    MusicList* musicList = [mViewNav getMusicListByName:musicListName];
    int oldRow = [musicList.seq intValue];
    if(oldRow == indexPath.row)
        return;
    [self deselectPrevRow:self];
    [self highlightCell: indexPath];
    [mMusicImageListView removeLineViewWhenSwitch:oldRow tableType:MUSIC_TABLE];
    [mMusicImageListView drawAttachFrom: indexPath.row tableType: MUSIC_TABLE offset:tableView.contentOffset];
    userInfo.currentmusiclist = ml.name;
    [mViewNav saveCoreDataContext];
}
- (void) pauseMusic
{
    for(int i = 0 ; i < mMusicItems.count ; i++)
    {
        SEMusicItem* item = [mMusicItems objectAtIndex:i];
        if(item.play == YES)
        {
            item.play = NO;
            UITableViewCell* cell = [self cellForRowAtIndexPath:[NSIndexPath indexPathForRow:item.row inSection:0]];
            if(cell)
            {
                UIImageView* imageVeiw = [self getPlayPauseView:cell];
                imageVeiw.image = [mViewNav.mResLoader getImage:@"MusicPlayIcon"];
            }
        }
        
    }
}
@end
@implementation SEMusicImageListView
@synthesize mViewNav;
@synthesize mBackground;
@synthesize mResLoader;
@synthesize mMusicListTableView;
@synthesize mImageListTableView;
@synthesize mImageListURLPropertyArray;
@synthesize mLineImage;
@synthesize mNeedDeleteRow;
@synthesize mTableType;
@synthesize mLineImageNotSelected;
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
    //mImageListTableView.mImageListPropertyArray = [mViewNav getAllImageList];
    mImageListTableView.mMusicImageListView = self;
    mImageListTableView.delegate = mImageListTableView;
    /*
    UserInfo* userInfo = [mViewNav getUserInfo];
    NSString* imageListName = userInfo.currentimagelist;
    ImageList* imageList = [mViewNav getImageListByName:imageListName];
    NSIndexPath* indexPath = [NSIndexPath indexPathForRow:[imageList.seq intValue] inSection:0];
    [mImageListTableView selectRowAtIndexPath:indexPath animated:NO scrollPosition:UITableViewScrollPositionTop];
    */
    mMusicListTableView.mResLoader = mResLoader;
    UIImageView* musicView = [[UIImageView alloc] init];
    musicView.image = mMusicListBackground;
    mMusicListTableView.backgroundView = musicView;
    [musicView release];
    mMusicListTableView.dataSource = mMusicListTableView;
    mMusicListTableView.delegate = mMusicListTableView;
    mMusicListTableView.mViewNav = mViewNav;
    mMusicListTableView.mMusicListPropertyArray = [mViewNav getAllMusicList]; 
    NSMutableArray* musicItems = [NSMutableArray array];
    for(int i = 0 ; i < mMusicListTableView.mMusicListPropertyArray.count ; i++)
    {
        SEMusicItem* item = [[SEMusicItem alloc] init];
        MusicList* ml = [mMusicListTableView.mMusicListPropertyArray objectAtIndex:i];
        item.play = NO;
        item.currSeq = 0;
        item.row = i;
        item.musicListName = ml.name;
        [musicItems addObject:item];
        [item release];
    }
    mMusicListTableView.mMusicItems = musicItems;
    mMusicListTableView.mMusicImageListView = self;
    mMusicListTableView.delegate = mMusicListTableView;
    mMusicListTableView.separatorStyle = UITableViewCellSeparatorStyleNone;
    mMusicListTableView.allowsSelection = NO;

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
//when table cell can be seen, it will be remove from table, so we should use the init point value 
// to calculate the line view' center
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
- (void)createLineViewData: (int)imageIndex : (int)musicIndex : (BOOL) isSelected
{
    LineViewData* lvd = [[LineViewData alloc] init];
    CGPoint p1 = CGPointMake(mImageIndicatorPointInCell.x, mImageIndicatorPointInCell.y + imageIndex
                             * mImageCellHeight);
    CGPoint p2 = CGPointMake(mMusicIndicatorPointInCell.x, mMusicIndicatorPointInCell.y + mMusicCellHeight * musicIndex);
    lvd.mStartPoint = p1;
    lvd.mEndPoint = p2;
    p1 = [mImageListTableView convertPoint:p1 toView:self];
    p2 = [mMusicListTableView convertPoint:p2 toView:self];
    
    lvd.mStartIndicatorTableType = IMAGE_TABLE;
    lvd.mEndIndicatorTableType = MUSIC_TABLE;
    lvd.mStartIndicatorRow = imageIndex;
    lvd.mEndIndicatorRow = musicIndex;
    [mLineViewArray addObject:lvd];
    [lvd release];
    UIView* v = [self createLineView:p1 :p2 : isSelected];
    lvd.mView = v;
    //[self addSubview:v];
    [self addLineViewToParent:v];
}
- (void) initLineViewData
{
    [mLineViewArray release];
    mLineViewArray = [NSMutableArray array];
    [mLineViewArray retain];
    NSArray* imageListArray = [mViewNav getAllImageList];
    UserInfo* userInfo = [mViewNav getUserInfo];
    NSString* currentImageListName = userInfo.currentimagelist;
    for(int i = 0 ; i < imageListArray.count ; i++)
    {
        ImageList* imageList = [imageListArray objectAtIndex:i];
        MusicList* musicList  =[ mViewNav getMusicListByImageList:imageList.name];
        if(musicList)
        {
            int imageIndex = [imageList.seq intValue];
            int musicIndex = [musicList.seq intValue];
            if([imageList.name isEqualToString:currentImageListName])
            {
                [self createLineViewData:imageIndex :musicIndex : YES];
            }
            else 
            {
                [self createLineViewData:imageIndex :musicIndex :NO];    
            }
        }
    }
    /*
    NSString* currentMusicListName = userInfo.currentmusiclist;
    MusicList* currentMusicList = [mViewNav getMusicListByName:currentMusicListName];
    if(currentMusicList != nil && [currentMusicList.name isEqualToString:musicList.name] == NO)
    {
        NSSet* imageListSet = currentMusicList.attachimagelist;
        NSArray* array = [imageListSet allObjects];
        for(int i = 0 ; i < array.count ; i++)
        {
            ImageList* il = [array objectAtIndex:i];
            int imageIndex = [il.seq intValue];
            int musicIndex = [currentMusicList.seq intValue];
            [self createLineViewData:imageIndex :musicIndex];
        }
    }
     */
}
- (NSMutableArray*) loadImageUrl: (NSString*)name
{
    NSArray* siSet = [mViewNav getSelectedImageArrayByName:name];//[il.selectedimage allObjects];
    if(siSet == nil)
        return nil;
    NSMutableArray* urlArray = [NSMutableArray array];
    for(NSUInteger i = 0 ; i < [siSet count] ; i++)
    {
        SelectedImage*  si = [siSet objectAtIndex:i];
        if(si.url != nil || si.filepath != nil)
        {
            [urlArray addObject:si];
        }
    }
    if(urlArray.count > 0)
    {
        return urlArray;
    }
    else 
    {
        return nil;
    }
}
- (SEImageListURLProperty*) correspondImageListURL : (SEImageListURLProperty*) newProperty oldArray: (NSMutableArray*) oldArray
{
    for(int i = 0 ; i < oldArray.count ; i++)
    {
        SEImageListURLProperty* oldProp = [oldArray objectAtIndex:i];
        if([newProperty.name isEqualToString:oldProp.name])
            return oldProp;
    }
    return nil;
}
- (void) compareAndSetImageListURL : (NSMutableArray*) oldArray : (NSMutableArray*)newArray
{
    for(int i = 0 ; i < newArray.count ; i++)
    {
        SEImageListURLProperty* newProp = [newArray objectAtIndex:i];
        SEImageListURLProperty* oldProp = [self correspondImageListURL:newProp oldArray:oldArray];
        if(oldProp != nil)
        {
            if(oldProp.selectedImageArray.count <= newProp.selectedImageArray.count)
            {
                newProp.currentIndex = oldProp.currentIndex;
            }
        }
    }
}
- (void) updateMusicListProperty
{
    for(int i = 0 ; i < mMusicListTableView.mMusicItems.count ; i++)
    {
        SEMusicItem* item = [mMusicListTableView.mMusicItems objectAtIndex:i];
        item.currSeq = 0;
    }
}
- (void) updateImageListURL
{
    NSMutableArray* newArray = [NSMutableArray array];
    NSArray* imageLists = [mViewNav getAllImageList];
    NSLog(@"### image list num = %d ####", imageLists.count);
    for(int i = 0 ; i < imageLists.count ; i++)
    {
        ImageList* il = [imageLists objectAtIndex:i];
        SEImageListURLProperty* p = [[SEImageListURLProperty alloc] init];
        p.currentIndex = 0;
        p.selectedImageArray = [self loadImageUrl:il.name];
        p.name = il.name;
        [newArray addObject:p];
        [p release];
    }
    if(self.mImageListURLPropertyArray == nil)
    {
        self.mImageListURLPropertyArray = newArray;
    }
    else
    {
        [self compareAndSetImageListURL:self.mImageListURLPropertyArray :newArray];
        self.mImageListURLPropertyArray = newArray;
    }
}
- (void) testButtonHandler: (UIButton*)sender
{
    NSString* text = mTestTextField.text;
    UserInfo* userInfo = [mViewNav getUserInfo];
    if([text isEqualToString:@"p attach"])
    {
        NSArray* musicList = [mViewNav getAllMusicList];
        for(int i = 0 ; i < musicList.count; i++)
        {
            MusicList* ml = [musicList objectAtIndex:i];
            NSLog(@"############");
            NSLog(@"music list name = %@", ml.name);
            NSArray* attachImagelist = [ml.attachimagelist allObjects];
            for(int j = 0 ; j < attachImagelist.count ; j++)
            {
                ImageList* il = [attachImagelist objectAtIndex:j];
                NSLog(@"attach image list = %@", il.name);
            }
            NSLog(@"#############");
            
            
        }
    }
    else if([text isEqualToString:@"p musiclist"])
    {}
}
-(void) initData
{
    //mImageIndicatorCenter.x = 68;
    //mImageIndicatorCenter.y = 56;
    //mMusicIndicatorCenter.x = 21;
    //mMusicIndicatorCenter.y = 56;
    
    mImageIndicatorCenter.x = 68 / 2;
    mImageIndicatorCenter.y = 88 / 2;
    mMusicIndicatorCenter.x = 68 / 2;
    mMusicIndicatorCenter.y = 88 / 2;
    
    mBackgroundView = (UIImageView*)[self viewWithTag:101];
    mBackgroundView.image = mBackground;
    mImageListTableView = (SEImageListTableView*)[self viewWithTag:102];
    mMusicListTableView = (SEMusicListTableView*)[self viewWithTag:103];
    mImageListLabel = (SEPopupImageTextLabel*)[self viewWithTag:106];
    mMusicListLabel = (SEPopupImageTextLabel*)[self viewWithTag:107];
    [mImageListLabel setText:@"imagelist"];
    [mImageListLabel setBackground:@"MusicImageAttachListLabelBg"];
    [mMusicListLabel setText:@"musiclist"];
    [mMusicListLabel setBackground:@"MusicImageAttachListLabelBg"];
    mClipLineView = [self viewWithTag:108];
    mClipLineView.userInteractionEnabled = NO;
    
    mTestButton = (UIButton*)[self viewWithTag:2223];
    mTestTextField = (UITextField*)[self viewWithTag:2222];
    [mTestButton addTarget:self action:@selector(testButtonHandler:) forControlEvents:UIControlEventTouchUpInside] ;
    float width = mBackgroundView.frame.size.width / 2;
    float height = mBackgroundView.frame.size.height / 2;
    //UIImageView* leftBgImageView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, width, height)];
    
    /*
    //mAddMusicListButton = (UIButton*) [self viewWithTag:104];
    //mAddMusicListButton.backgroundColor = nil;
    
    //UIImage* image = [mResLoader getImage:@"MusicImageListAddMusicListImageNormal"];
    //[mAddMusicListButton setBackgroundImage:image forState:UIControlStateNormal];
    //image = [mResLoader getImage:@"MusicImageListAddMusicListImageSelected"];
    //[mAddMusicListButton setBackgroundImage:image forState:UIControlStateHighlighted];
    /
    //mRemoveButton = (UIButton*)[self viewWithTag:105];
    image = [mResLoader getImage:@"MusicImageListRemoveImageNormal"];
    //[mRemoveButton setBackgroundImage:image forState:UIControlStateNormal];
    image = [mResLoader getImage:@"MusicImageListRemoveImageSelected"];
    //[mRemoveButton setBackgroundImage:image forState:UIControlStateHighlighted];
    //mAddImageListButton = (UIButton*) [self viewWithTag:106];
    image = [mResLoader getImage:@"MusicImageListAddImageListImageNormal"];
    //[mAddImageListButton setBackgroundImage:image forState:UIControlStateNormal];
    image = [mResLoader getImage:@"MusicImageListAddImageListImageSelected"];
    //[mAddImageListButton setBackgroundImage:image forState:UIControlStateHighlighted];
    ///////////
    
    ///
    //[mAddMusicListButton addTarget:self action:@selector(addMusicListButtonHandler:) forControlEvents:UIControlEventTouchUpInside];
    //[mAddImageListButton addTarget:self action:@selector(addImageListButtonHandler:) forControlEvents:UIControlEventTouchUpInside];
    //[mRemoveButton addTarget:self action:@selector(removeButtonHandler:) forControlEvents:UIControlEventTouchUpInside];
     */
    self.mLineImage = [mViewNav.mResLoader getImage:@"MusicImageLineIcon"];
    self.mLineImageNotSelected = [mViewNav.mResLoader getImage:@"MusicImageLineIcon1"];
    [self initMusicImageBackground];
    [self initMusicImageTableView];
    [self initIndicatorData];
    [self initLineViewData];
    [self updateImageListURL];
}
- (void)dealloc
{
    [mLineViewArray release];
    [mBackground release];
    [mImageListURLPropertyArray release];
    [mLineImage release];
    [mLineImageNotSelected release];
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
- (BOOL) touchBegan: (CGPoint) p
{
    mOrig = p;
    mLineView = [[UIImageView alloc] init];
    mLineView.image = mLineImage;
    //mLineView.backgroundColor = [UIColor blueColor];
    RowData rd = [self getIndexPathOnPoint:p];
    if(rd.indexPath)
    {
        mStartIndicatorRow = rd.indexPath.row;
        mStartIndicatorTable = rd.tableType;
        
    }
    [self addSubview:mLineView];
    return YES;
}
- (CGPoint) getPointInSelf: (RowData)rowData
{
    int row = [rowData.indexPath row];
    int type = rowData.tableType;
    SEIndicatorView* startIndicator = [self getIndicatorView:type row:row];
    CGPoint p1Indicator = CGPointMake(0, 0);
    if(type == IMAGE_TABLE)
        p1Indicator = mImageIndicatorCenter;
    else if(type == MUSIC_TABLE)
        p1Indicator = mMusicIndicatorCenter;
    CGPoint p = [startIndicator convertPoint:p1Indicator toView:self];
    return p;
}
- (BOOL) touchMove: (CGPoint) p
{
    const float PI = 3.14159;
    float deltax = p.x - mOrig.x;
    float deltay = p.y - mOrig.y;
    float theta = atanf(fabsf(deltay) / fabsf(deltax));
    float width = sqrtf(deltax * deltax + deltay * deltay);
    float height = LINE_VIEW_HEIGHT;
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
        //mLineView.frame = CGRectMake(mOrig.x - width / 2, mOrig.y - height / 2, width, height);
        RowData rd = [self getIndexPathOnPoint:mOrig];
        if(rd.indexPath)
        {
            CGPoint newP = [self getPointInSelf:rd];
            mLineView.frame = CGRectMake(newP.x - width / 2, newP.y - height / 2, width, height);
            CGAffineTransform rotate = CGAffineTransformMakeRotation(theta);
            CGAffineTransform translate = CGAffineTransformMakeTranslation(translatex, 0);
            mLineView.transform = CGAffineTransformConcat(translate, rotate);
        }
    }
    return YES;
}
- (BOOL) touchEnd: (CGPoint)p
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
        if(mStartIndicatorTable == mEndIndicatorTable)
        {
            return NO;
        }
        else
        {
            return YES;
        }
    }
    else 
    {
        return NO;
    }
    
}
- (BOOL) isUrlArrayEqual: (NSArray*)first : (NSArray*)second
{
    if(first.count != second.count)
        return NO;
    for(int i = 0 ; i < first.count ; i++)
    {
        SelectedImage* siFirst = [first objectAtIndex:i];
        SelectedImage* siSecond = [second objectAtIndex:i];
        if([siFirst.url isEqualToString:siSecond.url] == NO ||
           [siFirst.urldate isEqualToString:siSecond.urldate] == NO)
            return NO;
    }
    return YES;
}

- (int) getViewRow: (UIView*)v
{
    if([v isMemberOfClass:[UITableViewCell class]])
    {
        return v.tag;
    }
    UIView* parent = v.superview;
    while(parent != nil)
    {
        if([parent isMemberOfClass:[UITableViewCell class]])
        {
            return parent.tag;
        }
        else {
            parent = parent.superview;
        }
    }
    return nil;
}
- (void) pauseMusic
{
    [mMusicListTableView pauseMusic];
    
}
- (void) update
{
    NSLog(@"music image list update");
    [self updateImageListURL];
    [self updateMusicListProperty];
    [mMusicListTableView reloadData];
    [mImageListTableView reloadData];
}
- (void) updateMusicListTableViewIndicator
{
    [mMusicListTableView updateIndicator];
}
- (void) updateImageListTableViewIndicator
{
    [mImageListTableView updateIndicator];
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

