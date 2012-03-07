//
//  SEMusicPickerView.mm
//  PhotoFrame
//
//  Created by 陈勇 on 12-2-27.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEMusicPickerView.h"
#import "SEUITableView.h"
#import <MediaPlayer/MediaPlayer.h>
#import "SEViewNavigator.h"
#define SEDB 1
/////////////////
@implementation SEMusicItemProperty
@synthesize title;
@synthesize artist;
@synthesize album;
@synthesize seq;
- (void) dealloc
{
    [title release];
    [artist release];
    [album release];
    [seq release];
    [super dealloc];
}

@end

////////
@implementation SEMusicPickerDelegate
@synthesize mMusicPickerView;
- (UITableViewCell*) createCell:(UITableView *)tableView
{
    static NSString *CellIdentifier = @"Cell";
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier];
    if (cell == nil) {
        cell = [[[NSBundle mainBundle] loadNibNamed:@"MusicPickerTableViewCell" owner:self options:nil] lastObject]; 
        cell.userInteractionEnabled = YES;
        cell.multipleTouchEnabled = YES;
        for(UIView* subv in cell.subviews)
        {
            subv.multipleTouchEnabled = YES;
            subv.userInteractionEnabled = YES;
        }
        //UIImageView* imageView = (UIImageView*)[cell viewWithTag:104];
        /*
        UITapGestureRecognizer* ges = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(tapHandler:)];
        ges.numberOfTouchesRequired = 2;
        [cell addGestureRecognizer:ges];
        [ges release];
         */
    }
    return cell;
}

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    return 1;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    return [mMusicPickerView getMusicItemPropertyCount];
}
- (void) tapHandler: (UITapGestureRecognizer*)ges
{
    NSLog(@"tap\n");
    UITableViewCell* v = (UITableViewCell*)ges.view;
    NSLog(@"v = %@", v);
    UITableViewCell* cell = [[[NSBundle mainBundle] loadNibNamed:@"MusicPickerTableViewCell" owner:self options:nil] lastObject];
    CGPoint p = [v convertPoint:v.center toView:mMusicPickerView];
    CGRect rect = v.frame;
    cell.frame = CGRectMake(p.x, p.y, rect.size.width, rect.size.height);
    [mMusicPickerView addSubview:cell];
    cell.backgroundColor = [UIColor redColor];
}
// Customize the appearance of table view cells.
- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    UITableViewCell* cell = [self createCell:tableView];
    SEMusicItemProperty* musicItemProperty = [mMusicPickerView getMusicItemProperty:indexPath.row];
    UILabel* title = (UILabel*)[cell viewWithTag:101];
    UILabel* artist = (UILabel*)[cell viewWithTag:102];
    UILabel* album = (UILabel*)[cell viewWithTag:103];
    UIImageView* imageView = (UIImageView*)[cell viewWithTag:104];
    title.text = musicItemProperty.title;
    artist.text = musicItemProperty.artist;
    album.text = musicItemProperty.album;
    imageView.image = [UIImage imageNamed:@"Koala.jpg"];
    //imageView.userInteractionEnabled = YES;
    // Configure the cell.
    return cell;
}
- (CGFloat)tableView:(UITableView *)tableView heightForRowAtIndexPath:(NSIndexPath *)indexPath
{
    UITableViewCell* cell = [self createCell:tableView];
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
    /*
     <#DetailViewController#> *detailViewController = [[<#DetailViewController#> alloc] initWithNibName:@"<#Nib name#>" bundle:nil];
     // ...
     // Pass the selected object to the new view controller.
     [self.navigationController pushViewController:detailViewController animated:YES];
     [detailViewController release];
     */
}
@end

@implementation SEMusicSelectedViewDelegate
@synthesize mMusicPickerView;
- (UITableViewCell*) createCell:(UITableView *)tableView
{
    static NSString *CellIdentifier = @"Cell";
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier];
    if (cell == nil) {
        cell = [[[NSBundle mainBundle] loadNibNamed:@"MusicPickerTableViewCell" owner:self options:nil] lastObject]; 
    }
    return cell;
}

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    return 1;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    return [mMusicPickerView getSelectedMusicItemProperyCount];
}

// Customize the appearance of table view cells.
- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    UITableViewCell* cell = [self createCell:tableView];
    SEMusicItemProperty* musicItemProperty = [mMusicPickerView getSelectedMusicItemProperty:indexPath.row];
    UILabel* title = (UILabel*)[cell viewWithTag:101];
    UILabel* artist = (UILabel*)[cell viewWithTag:102];
    UILabel* album = (UILabel*)[cell viewWithTag:103];
    title.text = musicItemProperty.title;
    artist.text = musicItemProperty.artist;
    album.text = musicItemProperty.album;
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
    /*
     <#DetailViewController#> *detailViewController = [[<#DetailViewController#> alloc] initWithNibName:@"<#Nib name#>" bundle:nil];
     // ...
     // Pass the selected object to the new view controller.
     [self.navigationController pushViewController:detailViewController animated:YES];
     [detailViewController release];
     */
}


@end
////
@implementation SEMusicPickerView
@synthesize mMusicPicker;
@synthesize mSelectedMusicView;
@synthesize mMusicItemCount;
@synthesize mViewNav;
@synthesize mSelectedMusicItemArray;
//this function execute on main thread
- (void) addMusicItemToArray: (NSArray*)musicItemArray
{
    for(SEMusicItemProperty* item in musicItemArray)
    {
        [mMusicItemArray addObject:item];
    }
    [musicItemArray release];
    [self reloadPickerTableData];
}
//this function execute on worker thread;
- (void) readMusicLibThreadFunc
{
    NSLog(@"read music lib\n");
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];    
    MPMediaQuery* query = [MPMediaQuery songsQuery];
    NSArray* result = [query collections];
    int count = 0;
    NSArray* musicItemPropertyArray = [NSArray array];
    for(MPMediaItemCollection* collection in result)
    {
        for(MPMediaItem* item in [collection items])
        {
            NSString* title  = [item valueForKey:MPMediaItemPropertyTitle];
            NSString* artist = [item valueForKey:MPMediaItemPropertyAlbumArtist];
            NSString* artist1 = [item valueForKey:MPMediaItemPropertyArtist];
            //NSLog(@"title = %@\n", title);
            //NSLog(@"artist = %@\n", artist);
            //NSLog(@"artist1 = %@\n", artist1);
            SEMusicItemProperty* itemProperty = [[SEMusicItemProperty alloc] init];
            itemProperty.title = title;
            itemProperty.artist = artist1;
            itemProperty.album = artist;
            musicItemPropertyArray = [musicItemPropertyArray arrayByAddingObject:itemProperty];
            [item release];
            count++;
            if(count > 20)
            {
                [musicItemPropertyArray retain];
                [self performSelectorOnMainThread:@selector(addMusicItemToArray) withObject:musicItemPropertyArray waitUntilDone: NO];
                musicItemPropertyArray = [NSArray array];;
                count = 0;
            }
        }
    }
    if([musicItemPropertyArray count] > 0)
    {
        [musicItemPropertyArray retain];
        [self performSelectorOnMainThread:@selector(addMusicItemToArray) withObject:musicItemPropertyArray waitUntilDone: NO];
    }
    [pool release];
}
- (void) loadMusicLib
{
    MPMediaQuery* query = [MPMediaQuery songsQuery];
    mMusicItemCount = [[query collections] count];
    [mMusicItemArray release];
    mMusicItemArray = nil;
    if(mMusicItemCount > 0)
    {
        mMusicItemArray = [NSMutableArray arrayWithCapacity:mMusicItemCount];
        [self performSelectorInBackground:@selector(readMusicLibThreadFunc) withObject:nil];
    }

}
- (void)loadSelectedMusic
{
    NSArray* musicItemPropertyArray = [mViewNav getSelectedMusicArray];
    [musicItemPropertyArray sortedArrayUsingComparator:^NSComparisonResult(id obj1, id obj2) {
        SEMusicItemProperty* item1 = (SEMusicItemProperty*)obj1;
        SEMusicItemProperty* item2 = (SEMusicItemProperty*)obj2;
        return [item1.seq compare:item2.seq];
    }];
    mSelectedMusicItemArray = musicItemPropertyArray;
}
- (void) setHandler
{
    [mMusicPicker setTouchBeganTarget:self withAction:@selector(touchBeginHandler:)];
    [mMusicPicker setTouchMoveTarget:self withAction:@selector(touchMoveHandler:)];
    [mMusicPicker setTouchEndTarget:self withAction:@selector(touchEndHandler)];
    
}
- (void) tapHandler: (UITapGestureRecognizer*) ges
{
    NSLog(@"tap \n");
}
- (void) initMusicPicker
{
    self.multipleTouchEnabled = YES;
    mMusicPicker = (SEUITableView*)[self viewWithTag:101];
    [mMusicPicker initData];
    mMusicPicker.multipleTouchEnabled = YES;
    //UITapGestureRecognizer* ges = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(tapHandler:)];
    //ges.numberOfTouchesRequired = 2;
    //[mMusicPicker addGestureRecognizer:ges];
    //[ges release];
    mSelectedMusicView = (SEUITableView*)[self viewWithTag:102];
    [mSelectedMusicView initData];
    mMusicPickerDelegate = [[SEMusicPickerDelegate alloc] init];
    mMusicPickerDelegate.mMusicPickerView = self;
    mMusicSelectedViewDelegate = [[SEMusicSelectedViewDelegate alloc] init];
    mMusicSelectedViewDelegate.mMusicPickerView = self;
    mMusicPicker.delegate = mMusicPickerDelegate;
    mMusicPicker.dataSource = mMusicPickerDelegate;
    mSelectedMusicView.delegate = mMusicSelectedViewDelegate;
    mSelectedMusicView.dataSource = mMusicSelectedViewDelegate;
    [self setHandler];
    [self loadMusicLib];
    [self loadSelectedMusic];
}
- (void)dealloc
{
    [mMusicPickerDelegate release];
    [mMusicSelectedViewDelegate release];
    [mMusicPicker release];
    [mSelectedMusicView release];
    [super dealloc];
}
- (void) touchBeginHandler: (UITableViewCell*) pressedView
{
    UITableViewCell* newv = (UITableViewCell*)[[[NSBundle mainBundle] loadNibNamed:@"MusicPickerTableViewCell" owner:self options:nil] lastObject];
    newv.backgroundColor = [UIColor redColor];
    [self addSubview:newv];
    mSelectedTableViewCell = newv;
    [mMusicPicker saveGesture];
    [mMusicPicker removeAllGestures];
}
- (void) touchMoveHandler: (NSValue*) vDeltap
{
    CGPoint deltap = [vDeltap CGPointValue];
    if(mSelectedTableViewCell)
    {
        CGPoint c = mSelectedTableViewCell.center;
        c.x += deltap.x;
        c.y += deltap.y;
        mSelectedTableViewCell.center = c;
    }
}
- (void) touchEndHandler
{
    if(mSelectedTableViewCell)
    {
        [mMusicPicker restoreGesture];
    }
    [mSelectedTableViewCell removeFromSuperview];
    //[mSelectedTableViewCell release];
    mSelectedTableViewCell = nil;
    
}
- (void) createSelectedTableViewCell: (NSValue*) frame
{
    [mMusicPicker saveGesture];
    [mMusicPicker removeAllGestures];
    CGRect sFrame = [frame CGRectValue];
    sFrame = CGRectMake(sFrame.origin.x, sFrame.origin.y, sFrame.size
                        .width, 120);
    UIView* parent = self;
    SEUITableViewCell* cell = [[[SEUITableViewCell alloc] initWithStyle:UITableViewCellStyleDefault reuseIdentifier:@"dd"] autorelease];
    cell.textLabel.text = @"lkkasdfkkkdfsdf";
    cell.textLabel.frame = CGRectMake(80, 0, 200, 120);
    cell.textLabel.backgroundColor = [UIColor redColor];
    cell.frame = sFrame;
    mSelectedTableViewCell = cell;
    [parent addSubview:cell];
}
- (int) getMusicItemPropertyCount
{
    if(SEDB)
    {
        return 10;
    }
    else
        return [mMusicItemArray count];
}
- (int) getSelectedMusicItemProperyCount
{
    return [mSelectedMusicItemArray count];
}
- (SEMusicItemProperty*)getMusicItemProperty: (int)index
{
    if(SEDB)
    {
        NSString* title = [NSString stringWithFormat:@"title %d", index];
        NSString* artist = [NSString stringWithFormat:@"artist %d", index];
        NSString* album = [NSString stringWithFormat:@"album %d", index];
        SEMusicItemProperty* item = [[SEMusicItemProperty alloc] init];
        item.title = title;
        item.artist = artist;
        item.album = album;
        [item autorelease];
        return item;
    }
    else
    {
        return [mMusicItemArray objectAtIndex:index];
    }
}
- (SEMusicItemProperty*)getSelectedMusicItemProperty: (int)index
{
    return [mSelectedMusicItemArray objectAtIndex:index];
}
- (void)reloadPickerTableData
{
    [mMusicPicker reloadData];
}
@end

