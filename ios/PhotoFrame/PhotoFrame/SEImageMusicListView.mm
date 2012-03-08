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
@end
@implementation SEMusicImageListView (Private)
- (void)drawAttach
{
    
}
- (void)addAttach
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
        NSMutableSet* attachSet = (NSMutableSet*)musicList.attachimagelist;
        if(![mViewNav musicListContainImageList:musicList :imageList.name])
        {
            [attachSet addObject:imageList];
            
        }
    }
    else if(mStartIndicatorTable == MUSIC_TABLE && mEndIndicatorTable == IMAGE_TABLE)
    {
        MusicList* musicList = [mViewNav getMusicListBySeq:[NSNumber numberWithInt:mStartIndicatorRow]];
        ImageList* imageList = [mViewNav getImageListBySeq:[NSNumber numberWithInt:mEndIndicatorRow]];
        NSMutableSet* attachSet = (NSMutableSet*)musicList.attachimagelist;
        if(![mViewNav musicListContainImageList:musicList :imageList.name])
        {
            [attachSet addObject:imageList];
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
    NSInteger row = indexPath.row;
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
            [mViewNav addImageList:name];
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
    if(mTableType == MUSIC_TABLE)
    {
        [mViewNav removeMusicListByName:mCurrentName];
        mMusicListTableView.mMusicListPropertyArray = [mViewNav getAllMusicList];
        [mMusicListTableView reloadData];
    }
    else if(mTableType == IMAGE_TABLE)
    {
        [mViewNav removeImageListByName:mCurrentName];
        mImageListTableView.mImageListPropertyArray = [mViewNav getAllImageList];
        [mImageListTableView reloadData];
    }
    [mViewNav saveContext];
    mCurrentName = nil;
    mTableType = NO_TABLE;
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
    [self setIndicationImage:UITouchPhaseEnded];
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
    [self setIndicationImage:UITouchPhaseCancelled];
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
    UIImageView* background = (UIImageView*)[cell viewWithTag:101];
    BOOL hasAttached = NO;
    if(hasAttached)
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
    NSIndexPath* musicIndexPath = [musicTableView indexPathForSelectedRow];
    NSInteger row = musicIndexPath.row;
    [musicTableView deselectRowAtIndexPath:musicIndexPath animated:NO];
    ImageList* il = [mViewNav getImageListBySeq:[NSNumber numberWithInt:indexPath.row]];
    if(il)
    {
        [mMusicImageListView setSelectedItem:il.name type:IMAGE_TABLE];
    }
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
- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    return 1;
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
    SEIndicatorView* indicator = (SEIndicatorView*)[cell viewWithTag:101];
    UIImage* indicatorImage = nil;
    MusicList* ml = [mViewNav getMusicListBySeq:[NSNumber numberWithInt:indexPath.row]];
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
    musicNumber.text = [NSString stringWithFormat:@"%d", attachImage.count];
    UIImageView* background = (UIImageView*)[cell viewWithTag:106];
    if(hasAttachedImageList)
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

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath
{
    SEImageListTableView* imageTableView = mMusicImageListView.mImageListTableView;
    NSIndexPath* imageIndexPath = [imageTableView indexPathForSelectedRow];
    NSInteger row = imageIndexPath.row;
    [imageTableView deselectRowAtIndexPath:imageIndexPath animated:NO];
    MusicList* ml = [mViewNav getMusicListBySeq:[NSNumber numberWithInt:indexPath.row]];
    if(ml)
    {
        [mMusicImageListView setSelectedItem:ml.name type:MUSIC_TABLE];
    }
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
    //mImageListTableView.backgroundView = nil;
    mImageListTableView.dataSource = mImageListTableView;
    mImageListTableView.delegate = mImageListTableView;
    mImageListTableView.mViewNav = mViewNav;
    mImageListTableView.mImageListPropertyArray = [mViewNav getAllImageList];
    mImageListTableView.mMusicImageListView = self;

    mMusicListTableView.mResLoader = mResLoader;
    UIImageView* musicView = [[UIImageView alloc] init];
    musicView.image = mMusicListBackground;
    mMusicListTableView.backgroundView = musicView;
    mMusicListTableView.dataSource = mMusicListTableView;
    mMusicListTableView.delegate = mMusicListTableView;
    mMusicListTableView.mViewNav = mViewNav;
    mMusicListTableView.mMusicListPropertyArray = [mViewNav getAllMusicList]; 
    mMusicListTableView.mMusicImageListView = self;
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
-(void) initData
{
    mBackgroundView = (UIImageView*)[self viewWithTag:101];
    mImageListTableView = (SEImageListTableView*)[self viewWithTag:102];
    mMusicListTableView = (SEMusicListTableView*)[self viewWithTag:103];
    mAddMusicListButton = (UIButton*) [self viewWithTag:104];
    mRemoveButton = (UIButton*)[self viewWithTag:105];
    mAddImageListButton = (UIButton*) [self viewWithTag:106];
    [mAddMusicListButton addTarget:self action:@selector(addMusicListButtonHandler:) forControlEvents:UIControlEventTouchUpInside];
    [mAddImageListButton addTarget:self action:@selector(addImageListButtonHandler:) forControlEvents:UIControlEventTouchUpInside];
    [mRemoveButton addTarget:self action:@selector(removeButtonHandler:) forControlEvents:UIControlEventTouchUpInside];
    [self initMusicImageBackground];
    [self initMusicImageTableView];
}
- (void)dealloc
{
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
    mLineView = [[UIView alloc] init];
    mLineView.backgroundColor = [UIColor blueColor];
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
