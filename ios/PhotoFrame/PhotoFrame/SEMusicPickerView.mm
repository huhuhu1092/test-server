//
//  SEMusicPickerView.mm
//  PhotoFrame
//
//  Created by 陈勇 on 12-2-27.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEMusicPickerView.h"
//#import "SEUITableView.h"
#import <MediaPlayer/MediaPlayer.h>
#import "SEViewNavigator.h"
#import "SEResDefine.h"
#import "SEUtil.h"
#import "UserInfo.h"
#import "MusicList.h"
#import "SelectedMusic.h"
#import "SEOperationView.h"
#import "PhotoFrameAppDelegate.h"
#import "SESystemConfig.h"
#define SEDB 0
enum SESONG_STATE {SESONG_PLAY, SESONG_PAUSE, SESONG_STOP};
enum SESORT_TYPE {SESORT_BYTITLE, SESORT_BYARTIST, SESORT_BYALBUM};
static void handleTitleSort(int* sortType, NSMutableArray* musicItemArray, UITableView* tableView)
{
    *sortType = SESORT_BYTITLE;
    [musicItemArray sortUsingComparator:^NSComparisonResult(id obj1, id obj2) {
        SEMusicItemProperty* item1 = (SEMusicItemProperty*)obj1;
        SEMusicItemProperty* item2 = (SEMusicItemProperty*)obj2;
        return [item1.title localizedCompare:item2.title];
    }];
    [tableView reloadData];
}
static void handleArtistSort(int* sortType , NSMutableArray* musicItemArray, UITableView* tableView)
{
    *sortType = SESORT_BYARTIST;
    [musicItemArray sortUsingComparator:^NSComparisonResult(id obj1, id obj2) {
        SEMusicItemProperty* item1 = (SEMusicItemProperty*)obj1;
        SEMusicItemProperty* item2 = (SEMusicItemProperty*)obj2;
        if(item1.artist != nil && item2.artist != nil)
        {
            return [item1.artist localizedCompare:item2.artist];
        }
        else if(item1.artist != nil && item2.artist == nil)
            return NSOrderedDescending;
        else if(item1.artist == nil && item2.artist != nil)
            return NSOrderedAscending;
        else
            return NSOrderedSame;
    }];
    
    [tableView reloadData];    
}
static void handleAlbumSort(int* sortType , NSMutableArray* musicItemArray, UITableView* tableView)
{
    *sortType = SESORT_BYALBUM;
    [musicItemArray sortUsingComparator:^NSComparisonResult(id obj1, id obj2) {
        SEMusicItemProperty* item1 = (SEMusicItemProperty*)obj1;
        SEMusicItemProperty* item2 = (SEMusicItemProperty*)obj2;
        if(item1.album != nil && item2.album != nil)
        {
            return [item1.album localizedCompare:item2.album];
        }
        else if(item1.album != nil && item2.album == nil)
        {
            return NSOrderedDescending;
        }
        else if(item1.album == nil && item2.album != nil)
            return NSOrderedAscending;
        else
            return NSOrderedSame;
    }];
    [tableView reloadData];
}

static void handlePlayerStop()
{
    
    
}
static UIImage* stretchCellBg(UIImage* image)
{
    int version = [SEUtil getSystemVersion];
    if(version <= 4)
    {
         return [image stretchableImageWithLeftCapWidth:23 topCapHeight:0];
    }
    else 
    {
        return  [image resizableImageWithCapInsets:UIEdgeInsetsMake(0, 23, 0, 23)];
    }
}
static UILabel* getTitleLabel(UITableViewCell* cell)
{
    return (UILabel*)[cell viewWithTag:655360];
}
static UILabel* getArtistAlbumLabel(UITableViewCell* cell)
{
    return (UILabel*)[cell viewWithTag:655370];
}
/////////////////
@interface MyTableCell : UITableViewCell

@end
@implementation MyTableCell

-(void) layoutSubviews
{
    NSLog(@"## cell layout subviews##");
    [super layoutSubviews];
    UIImageView* bg = (UIImageView*)[self viewWithTag:655350];
    NSLog(@"bg size = %f, %f", bg.frame.size.width, bg.frame.size.height);
}

@end
////////////////////
@implementation SESelectedMusicItem
@synthesize item;
@synthesize row;
- (void) dealloc
{
    [item release];
    [super dealloc];
}
@end
/////////////////
@implementation SEMusicItemProperty
@synthesize title;
@synthesize artist;
@synthesize album;
@synthesize seq;
@synthesize song;
@synthesize musicPlayState;
@dynamic highlighted;
- (void) dealloc
{
    [song release];
    [title release];
    [artist release];
    [album release];
    [seq release];
    [super dealloc];
}
- (void) setHighlighted:(BOOL)h
{
    if(highlighted != h)
    {
        highlighted = h;
    }
}
- (BOOL) highlighted
{
    return highlighted;
}
- (SEMusicItemProperty*) clone
{
    SEMusicItemProperty* itemProperty = [[SEMusicItemProperty alloc] init];
    itemProperty.title = title;
    itemProperty.artist = artist;
    itemProperty.album = album;
    itemProperty.seq = seq;
    //itemProperty.song = song;
    return [itemProperty autorelease];
}
- (BOOL) isEqualToMusicItemProperty: (SEMusicItemProperty*) item
{
    BOOL titleEqual = (title == nil && item.title == nil) || [title isEqualToString:item.title];
    BOOL artistEqual = (artist == nil && item.artist == nil) || [artist isEqualToString:item.artist];
    BOOL albumEqual = (album == nil && item.album == nil) || [album isEqualToString:item.album];
    if(titleEqual && artistEqual && albumEqual)
        return YES;
    return NO;
}
@end
////////////////////
@interface SEMusicSelectedImpl (PRIVATE)
- (void) readMusicLibThreadFunc;
- (void) addMusicItemToArray: (NSArray*)musicItemArray;
- (void) handleTitleSortButton: (id) sender;
- (void) handleArtistSortButton: (id) sender;
- (void) handleAlbumSortButton: (id) sender;
- (void) handlePlayPauseClick: (UITapGestureRecognizer*) tap;
- (void) handleStopClick: (UITapGestureRecognizer*)tap;
- (SEMusicItemProperty*) getMusicItemProperty: (int)index;
- (UITableViewCell*) findCellByRow: (int) row;
- (void) stopMusicPlay;
- (void) updateCellByData: (int)startIndex : (int) endIndex;
- (void) setCellProperty: (UITableViewCell*)cell : (SEMusicItemProperty*) musicItemProperty;
- (BOOL) isCellSelected: (int)row;
- (UIImage*) getNullCellBg;
- (UIImage*) getCellNormalBg;
- (UIImage*) getCellHighlightedBg;
- (void) setHighlightedCount;

@end
@implementation SEMusicSelectedImpl (PRIVATE)
- (void) setHighlightedCount
{
    NSString* str = @"";
    NSLog(@"highlighed num = %d", [self getHighlightedViewNum]);
    if([self getHighlightedViewNum] > 0)
    {
        str = [NSString stringWithFormat:@"%d", [self getHighlightedViewNum]];
    }
    if(mMusicResourceType == MUSIC_RESOURCE_LIB)
    {
        [mViewNav setSelectedMusicNumToMusicPicker:str];
    }
    else 
    {
        [mViewNav setSelectedMusicNumToSelectedMusicView:str];
    }

}
- (UIImage*) getNullCellBg
{
    if(mNullCellBg == nil)
    {
        UIImage* image = [mViewNav.mResLoader getImage:@"MusicSelectViewSlotBg"];
        image = [SEUtil imageWithCap:image top:0.1 bottom:0.9 left:0.1 right:0.9];
        mNullCellBg = [image retain];
    }
    return mNullCellBg;
}
- (UIImage*) getCellNormalBg
{
    if(mNormalCellBg == nil)
    {
        UIImage* image = [mViewNav.mResLoader getImage:mMusicCellBgStr];
        image = stretchCellBg(image);
        mNormalCellBg = [image retain];
    }
    return mNormalCellBg;
}
- (UIImage*) getCellHighlightedBg
{
    if(mHighlightedCellBg == nil)
    {
        UIImage* image = [mViewNav.mResLoader getImage:mMusicCellHBgStr];
        image = stretchCellBg(image);
        mHighlightedCellBg = [image retain];
    }
    return mHighlightedCellBg;
}

- (UITableViewCell*) findCellByRow: (int) row
{
    UITableViewCell* cell = [mMusicTableView cellForRowAtIndexPath:[NSIndexPath indexPathForRow:row inSection:0]];
    return cell;
}
- (BOOL) isCellSelected: (int)row
{
    if(row < 0 || row >= mMusicItemArray.count)
        return NO;
    SEMusicItemProperty* item = [mMusicItemArray objectAtIndex: row];
    return item.highlighted;
    
}
- (void) clearCell: (UITableViewCell*)cell
{
    UIImageView* background = (UIImageView*)[cell viewWithTag:655350];
    NSLog(@"background size = %f, %f", background.frame.size.width, background.frame.size.height);
    background.image = nil;

    //background.contentMode = UIViewContentModeScaleToFill;
    background.userInteractionEnabled = YES;
    UILabel* title1 = (UILabel*)[cell viewWithTag:655360];
    UILabel* title2 = (UILabel*)[cell viewWithTag:655370];
    title1.textColor = [UIColor whiteColor];
    title2.textColor = [UIColor whiteColor];
    UIImageView* play = (UIImageView*)[cell viewWithTag:655380];
    UIImageView* stop = (UIImageView*)[cell viewWithTag:655390];
    title1.text = @"";
    title2.text = @"";
    play.image = nil;
    stop.image = nil;

}
- (void) setCellProperty: (UITableViewCell*)cell : (SEMusicItemProperty*) musicItemProperty
{
    UIImageView* background = (UIImageView*)[cell viewWithTag:655350];
    assert([background isKindOfClass:[UIImageView class]]);
    //NSLog(@"background size = %f, %f", background.frame.size.width, background.frame.size.height);
    //CGSize s = cell.frame.size;
    if(cell.tag == 101)
    {
        NSLog(@"tag = 101");
    }
    NSArray* musicArray = nil;
    if(musicItemProperty)
    {
        musicArray = [SEUtil findMediaItemByTitle:musicItemProperty.title aritst:musicItemProperty.artist album:musicItemProperty.album];
    }
    UILabel* nomusicLabel = (UILabel*)[cell viewWithTag:655391];
    nomusicLabel.text = @"";
    if([self isCellSelected:cell.tag])
    {
        //UIImage* image = [mViewNav.mResLoader getImage:@"MusicPickerSelectedCellBg"];
        // background.image = stretchCellBg(image);
        background.image = [self getCellHighlightedBg];
        if(musicArray.count == 0 && musicItemProperty != nil)
        {
            musicItemProperty = nil;
            nomusicLabel.textColor = [UIColor yellowColor];
            nomusicLabel.text = @"Music Not Found";
            nomusicLabel.userInteractionEnabled = NO;
            nomusicLabel.textAlignment = UITextAlignmentCenter;
            nomusicLabel.font = [UIFont fontWithName:[SESystemConfig getFontName] size:23];
        }
    }
    else
    {
        //background.image = mMusicCellBackgroundImage;
        if(musicArray.count == 0 && musicItemProperty != nil)
        {
            background.image = [self getNullCellBg];//[mViewNav.mResLoader getImage:@"NoMusicFound"];
            musicItemProperty = nil;
            nomusicLabel.textColor = [UIColor yellowColor];
            nomusicLabel.text = @"Music Not Found";
            nomusicLabel.userInteractionEnabled = NO;
            nomusicLabel.textAlignment = UITextAlignmentCenter;
            nomusicLabel.font = [UIFont fontWithName:[SESystemConfig getFontName] size:23];

        }
        else if(musicItemProperty == nil && mMusicResourceType == MUSIC_RESOURCE_CORE_DATA)
        {
            background.image = [self getNullCellBg];
            return;
        }
        else
        {
            background.image = [self getCellNormalBg];
        }
    }
    //background.contentMode = UIViewContentModeScaleToFill;
    background.userInteractionEnabled = YES;
    UILabel* title1 = (UILabel*)[cell viewWithTag:655360];
    //NSLog(@"title1 = %@", title1);
    assert([title1 isKindOfClass:[UILabel class]]);
    UILabel* title2 = (UILabel*)[cell viewWithTag:655370];
    assert([title2 isKindOfClass:[UILabel class]]);
    title1.textColor = [UIColor colorWithRed:215.0/255 green:215.0/255 blue:215.0/255 alpha:1];
    title2.textColor = [UIColor colorWithRed:215.0/255 green:215.0/255 blue: 215.0/255 alpha:1];
    UIImageView* play = (UIImageView*)[cell viewWithTag:655380];
    assert([play isKindOfClass:[UIImageView class]]);
    UIImageView* stop = (UIImageView*)[cell viewWithTag:655390];
    assert([stop isKindOfClass:[UIImageView class]]);
    title1.text = musicItemProperty.title;
    if(mMusicResourceType == MUSIC_RESOURCE_CORE_DATA)
    {
        title1.textColor = [UIColor colorWithRed:54.0/255 green:54.0/255 blue:54.0/255 alpha:1];
        title2.textColor = [UIColor colorWithRed:54.0/255 green:54.0/255 blue:54.0/255 alpha:1];
    }
    if(musicItemProperty != nil)
    {
        if(mSortType == SESORT_BYTITLE || mSortType == SESORT_BYARTIST)
        {
            if(musicItemProperty.artist != nil)
            {
                title2.text = musicItemProperty.artist;
            }
            else
            {
                title2.text = @"unknown";    
            }
        }
        else
        {
            if(musicItemProperty.album == nil)
            {
                title2.text = @"unknown";
            }
            else
                title2.text = musicItemProperty.album;
        }
        play.userInteractionEnabled = YES;
        if(musicItemProperty.musicPlayState == MUSIC_PLAY)
        {
            play.image = [mViewNav.mResLoader getImage:@"MusicPauseIcon"];
        }
        else
        {
            play.image = [mViewNav.mResLoader getImage:@"MusicPickerMusicPlayIcon"];
        }
        stop.image = [mViewNav.mResLoader getImage:@"MusicPickerMusicStopIcon"];
        stop.userInteractionEnabled = YES;
        NSArray* gesArray = [play gestureRecognizers];
        for(UIGestureRecognizer* ges in gesArray)
        {
            [play removeGestureRecognizer:ges];
        }
        UITapGestureRecognizer* ges = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(handlePlayPauseClick:)];
        [play addGestureRecognizer:ges];
        [ges release];
        
        gesArray = [stop gestureRecognizers];
        for(UIGestureRecognizer* ges in gesArray)
        {
            [stop removeGestureRecognizer:ges];
        }

        
        ges = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(handleStopClick:)];
        [stop addGestureRecognizer:ges];
        [ges release];
    }
    NSArray* gesArray = [cell gestureRecognizers];
    for(UIGestureRecognizer* ges in gesArray)
    {
        [cell removeGestureRecognizer:ges];
    }
    //cell longpress
    UILongPressGestureRecognizer* longPress = [[UILongPressGestureRecognizer alloc] initWithTarget:self action:@selector(handleLongPress:)];
    [cell addGestureRecognizer:longPress];
    [longPress release];
    //end
    //cell tap
    gesArray = [background gestureRecognizers];
    for(UIGestureRecognizer* ges in gesArray)
    {
        [background removeGestureRecognizer:ges];
    }

    UITapGestureRecognizer* tap = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(handleTap:)];
    [background addGestureRecognizer:tap];
    [tap release];
}

- (void) updateCellByData: (int)startIndex : (int) endIndex
{
    for(int i = startIndex ; i < endIndex ; i++)
    {
        UITableViewCell* cell = [self findCellByRow:i];
        if(cell != nil)
        {
            assert(cell.tag == i);
            NSLog(@"update cell = %d", i);
            SEMusicItemProperty* item = [mMusicItemArray objectAtIndex:i];
            [self setCellProperty:cell :item];
        }
    }
}
- (SEMusicItemProperty*) getMusicItemProperty: (int)index
{
    if(index < 0 || index >= mMusicItemArray.count)
        return nil;
    return [mMusicItemArray objectAtIndex:index];
}
- (void) handlePlayPauseIcon: (int)currentIndex : (int)index
{
    
}
- (void) setPlaybackImage: (int) itemIndex : (BOOL) bPlay
{
    if(itemIndex == -1)
        return;
    if(itemIndex < 0 || itemIndex >= mMusicItemArray.count)
        return;
    UITableViewCell* cell = [self findCellByRow: itemIndex];
    UIImageView* playImageView = (UIImageView*)[cell viewWithTag:655380];
    UIImage* image = nil;
    if(bPlay == NO)
    {
        image = [mViewNav.mResLoader getImage:@"MusicPauseIcon"];
    }
    else
    {
        image = [mViewNav.mResLoader getImage:@"MusicPlayIcon"];    
    }
    playImageView.image = image;
}
- (void) setPlaybackIconPause: (int) itemIndex
{
    [self setPlaybackImage:itemIndex :NO];
}
- (void) setPlaybackIconPlay: (int)itemIndex
{
    [self setPlaybackImage:itemIndex :YES];
}
- (void) setMusicState: (int)currentItem: (int)wantItem
{
    SEMusicItemProperty* itemProperty = [mMusicItemArray objectAtIndex:wantItem];
    itemProperty.musicPlayState = MUSIC_PLAY;
    if(currentItem >= 0 && currentItem < mMusicItemArray.count)
    {
        SEMusicItemProperty* ci = [mMusicItemArray objectAtIndex:currentItem];
        ci.musicPlayState = MUSIC_PAUSE;
    }
}
- (void) handlePlayerPlayPause: (SEMusicItemProperty*)itemProperty: (int) currentItem: (int) wantItem
{
    NSLog(@"play\n");
    MPMusicPlayerController* player = [MPMusicPlayerController applicationMusicPlayer];
    MPMusicPlaybackState playbackState = player.playbackState;
    //MPMediaItem* nowPlayItem = player.nowPlayingItem;
    if(playbackState == MPMusicPlaybackStatePaused && currentItem == wantItem)
    {
        [player play];
        [self setPlaybackIconPause: currentItem];
        SEMusicItemProperty* itemProperty = [mMusicItemArray objectAtIndex:wantItem];
        SEMusicItemProperty* ci = [mMusicItemArray objectAtIndex:currentItem];
        itemProperty.musicPlayState = MUSIC_PLAY;
        return;
    }
    else if(playbackState == MPMusicPlaybackStatePlaying && currentItem == wantItem)
    {
        [player pause];
        [self setPlaybackIconPlay: currentItem];
        SEMusicItemProperty* ci = [mMusicItemArray objectAtIndex:currentItem];
        itemProperty.musicPlayState = MUSIC_PAUSE;
        return;
    }
    else if(playbackState == MPMusicPlaybackStateStopped || currentItem != wantItem)
    {
        NSArray* items = [SEUtil findMediaItemByTitle:itemProperty.title aritst:itemProperty.artist album:itemProperty.album];
        if(items.count == 0)
            return;
        MPMediaItem* item = [items objectAtIndex:0];
        NSMutableArray* array = [NSMutableArray array];
        [array addObject:item];
        MPMediaItemCollection* queue = [MPMediaItemCollection collectionWithItems:array];
        if(queue)
        {
            MPMusicPlayerController* player = [MPMusicPlayerController applicationMusicPlayer];
            [player setQueueWithItemCollection:queue];
            player.shuffleMode = MPMusicShuffleModeSongs;
            [player play];
        }
        [self setPlaybackIconPlay: currentItem];
        [self setPlaybackIconPause: wantItem];
        SEMusicItemProperty* itemProperty = [mMusicItemArray objectAtIndex:wantItem];
        itemProperty.musicPlayState = MUSIC_PLAY;
        if(currentItem >= 0 && currentItem < mMusicItemArray.count)
        {
            SEMusicItemProperty* ci = [mMusicItemArray objectAtIndex:currentItem];
            ci.musicPlayState = MUSIC_PAUSE;
        }
    }
}
- (int) findCurrentPlayMusicItem
{
    for(int i = 0 ; i < mMusicItemArray.count ; i++)
    {
        SEMusicItemProperty* item = [mMusicItemArray objectAtIndex:i];
        if(item.musicPlayState == MUSIC_PLAY)
            return i;
    }
    return -1;
}
- (void) handlePlayPauseClick: (UITapGestureRecognizer*) tap
{
    UIView* v = tap.view;
    UIView* parentView = v.superview.superview;
    assert([parentView isKindOfClass:[UITableViewCell class]]);
    UITableViewCell* cell = (UITableViewCell*)parentView;
    int index = cell.tag;
    NSLog(@"play index = %d\n", index);
    SEMusicItemProperty* itemProperty = [mMusicItemArray objectAtIndex:index];
    int currentPlayIndex = [self findCurrentPlayMusicItem];
    [mViewNav pauseMusicInMusicPicker:mParent];
    NSArray* items = [SEUtil findMediaItemByTitle:itemProperty.title aritst:itemProperty.artist album:itemProperty.album];
    if(items.count > 0)
    {
        [self handlePlayerPlayPause:itemProperty: currentPlayIndex: index];
        mViewNav.mMusicPlayListChange = YES;
    }
    else
    {
        NSLog(@"music song is nil");    
    }
    
}
- (void) stopMusicPlay
{
    MPMusicPlayerController* player = [MPMusicPlayerController applicationMusicPlayer];
    [player stop];
}

- (void) handleStopClick: (UITapGestureRecognizer*)tap
{
    UIView* v = tap.view;
    UIView* parentView = v.superview.superview;
    assert([parentView isKindOfClass:[UITableViewCell class]]);
    UITableViewCell* cell = (UITableViewCell*)parentView;
    int index = cell.tag;
    NSLog(@"play index = %d\n", index);
    SEMusicItemProperty* itemProperty = [mMusicItemArray objectAtIndex:index];
    int currentPlayIndex = [self findCurrentPlayMusicItem];
    if(currentPlayIndex == index)
    {
        MPMusicPlayerController* player = [MPMusicPlayerController applicationMusicPlayer];
        //MPMusicPlaybackState playbackState = player.playbackState;
        [player stop];
        itemProperty.musicPlayState = MUSIC_PAUSE;
        [self setPlaybackIconPlay:index];
        mViewNav.mMusicPlayListChange = YES;
    }
    
}
//this function execute on main thread
- (void) addMusicItemToArray: (NSArray*)musicItemArray
{
    int count = musicItemArray.count;
    int startIndex = mMusicItemArray.count;
    for(SEMusicItemProperty* item in musicItemArray)
    {
        [mMusicItemArray addObject:item];
    }
    int endIndex = mMusicItemArray.count;
    for(int i = 0 ; i < mMusicItemArray.count ; i++)
    {
        SEMusicItemProperty* item = [mMusicItemArray objectAtIndex:i];
        item.seq = [NSNumber numberWithInt:i];
    }
    NSLog(@"## music item array count = %d ##", mMusicItemArray.count);
    [musicItemArray release];
    [mMusicTableView reloadData];
    //[self updateCellByData:startIndex :endIndex];
}
//this function execute on worker thread;
- (void) readMusicLibThreadFunc
{
    NSLog(@"read music lib\n");
    NSThread* currentThread = [NSThread currentThread];
    assert([currentThread isEqual:[NSThread mainThread]] == NO);
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];    
    MPMediaQuery* query = [MPMediaQuery songsQuery];
    NSArray* result = [query collections];
    //int count = 0;
    NSMutableArray* musicItemPropertyArray = [NSMutableArray array];
    for(MPMediaItemCollection* collection in result)
    {
        for(MPMediaItem* item in [collection items])
        {
            NSString* title  = [item valueForKey:MPMediaItemPropertyTitle];
            NSString* album = [item valueForKey:MPMediaItemPropertyAlbumTitle];
            NSString* artist = [item valueForKey:MPMediaItemPropertyArtist];
            //MPMediaItemArtwork* artWork = [item valueForKey:MPMediaItemPropertyArtwork];
            //UIImage* image = [artWork imageWithSize:CGSizeMake(100, 100)];
            //NSLog(@"image size = %f, %f", image.size.width, image.size.height);
            //NSLog(@"title = %@\n", title);
            //NSLog(@"artist = %@\n", artist);
            //NSLog(@"artist1 = %@\n", artist1);
            SEMusicItemProperty* itemProperty = [[SEMusicItemProperty alloc] init];
            itemProperty.title = title;
            itemProperty.artist = artist;
            itemProperty.album = album;
            //itemProperty.song = item;
            //musicItemPropertyArray = [musicItemPropertyArray arrayByAddingObject:itemProperty];
            [musicItemPropertyArray addObject:itemProperty];
            [itemProperty release];
            /*
            if(musicItemPropertyArray.count > 60)
            {
                [musicItemPropertyArray retain];
                [self performSelectorOnMainThread:@selector(addMusicItemToArray:) withObject:musicItemPropertyArray waitUntilDone: NO];
                musicItemPropertyArray = [NSMutableArray array];
            }
             */
        }
    }
    if([musicItemPropertyArray count] > 0)
    {
        [musicItemPropertyArray retain];
        [self performSelectorOnMainThread:@selector(addMusicItemToArray:) withObject:musicItemPropertyArray waitUntilDone: NO];
    }
    [pool release];
}


- (void)initMusicArray
{
    mMusicItemArray = [NSMutableArray arrayWithCapacity:10];
    mMusicItemCount = 10;
    for(int index = 0 ; index < 10 ; index++)
    {
        int rt = rand();
        rt = rt % 10;
        int ra = rand();
        ra = ra % 10;
        
        int rb = rand();
        rb = rb % 10;
        NSString* title = [NSString stringWithFormat:@"tiadfsdfsdfsdftle %d", rt];
        NSString* artist = [NSString stringWithFormat:@"artsdfsdfsdfsdfsadfsdfsadfasdfist %d", ra];
        NSString* album = [NSString stringWithFormat:@"album %d", rb];
        SEMusicItemProperty* item = [[SEMusicItemProperty alloc] init];
        item.title = title;
        item.artist = artist;
        item.album = album;
        [item autorelease];
        [mMusicItemArray addObject:item];
    }
    [mMusicItemArray retain];
    [mMusicTableView reloadData];
}
- (void)loadSelectedMusic
{
}

- (void) loadMusic
{
    if(mMusicResourceType == MUSIC_RESOURCE_LIB)
    {
        MPMediaQuery* query = [MPMediaQuery songsQuery];
        NSArray* result = [query collections];
        int count = 0;
        for(MPMediaItemCollection* collection in result)
        {
            for(MPMediaItem* item in [collection items])
            {
                count++;
            }
        }
        mMusicItemCount = count;//[[query collections] count];
        NSLog(@"music item count = %d", mMusicItemCount);
        [mMusicItemArray release];
        mMusicItemArray = nil;
        if(mMusicItemCount > 0)
        {
            mMusicItemArray = [NSMutableArray arrayWithCapacity:mMusicItemCount];
            [mMusicItemArray retain];
            [self performSelectorInBackground:@selector(readMusicLibThreadFunc) withObject:nil];
        }
        else
        {
            mMusicItemArray = [[NSMutableArray arrayWithArray:[NSArray array]] retain];    
        }
    }
    else 
    {
        NSArray* musicItemPropertyArray = [mViewNav getCurrentSelectedMusicArray];
        musicItemPropertyArray = [musicItemPropertyArray sortedArrayUsingComparator:^NSComparisonResult(id obj1, id obj2) {
            SEMusicItemProperty* item1 = (SEMusicItemProperty*)obj1;
            SEMusicItemProperty* item2 = (SEMusicItemProperty*)obj2;
            return [item1.seq compare:item2.seq];
        }];
        [mMusicItemArray release];
        mMusicItemArray = [[NSMutableArray arrayWithArray:musicItemPropertyArray] retain];
        mMusicItemCount = mMusicItemArray.count + [mViewNav getRemainingMusicCount];
        /*
        for(SEMusicItemProperty* item in mMusicItemArray)
        {
            MPMediaPropertyPredicate *artistNamePredicate = nil;
            if(item.artist != nil)
            {
                artistNamePredicate =  [MPMediaPropertyPredicate predicateWithValue: item.artist
                                             forProperty: MPMediaItemPropertyArtist];
            }
            
            MPMediaPropertyPredicate *albumNamePredicate = nil;
            if(item.album != nil)
            {
                albumNamePredicate =  [MPMediaPropertyPredicate predicateWithValue: item.album
                                             forProperty: MPMediaItemPropertyAlbumTitle];
            }
            
            MPMediaPropertyPredicate* titlePredicate = [MPMediaPropertyPredicate predicateWithValue:item.title forProperty:MPMediaItemPropertyTitle];
            
            MPMediaQuery *myComplexQuery = [[MPMediaQuery alloc] init];
            
            if(item.artist)
            {
                [myComplexQuery addFilterPredicate: artistNamePredicate];
            }
            if(item.album)
            {
                [myComplexQuery addFilterPredicate: albumNamePredicate];
            }
            [myComplexQuery addFilterPredicate:titlePredicate];
            NSArray *items = [myComplexQuery items];
            if(items.count > 0)
            {
            //assert(items.count == 1);
                MPMediaItem* mitem = [items objectAtIndex:0];
                item.song = mitem;
                [myComplexQuery release];
            }
        }
         */
    }
}
- (void) handleTitleSortButton: (id) sender
{
    handleTitleSort(&mSortType, mMusicItemArray, mMusicTableView);
}
- (void) handleArtistSortButton: (id) sender
{
    handleArtistSort(&mSortType, mMusicItemArray, mMusicTableView);
}
- (void) handleAlbumSortButton: (id) sender
{
    handleAlbumSort(&mSortType, mMusicItemArray, mMusicTableView);
}

@end
/////
//////
@interface SEMusicOperationViewHandler : NSObject <SEOperationHandler>
{
    SEViewNavigator* mViewNav;
    SESelectedMusicView* mSelectedMusicView;
}
@property (nonatomic, assign) SESelectedMusicView* mSelectedMusicView;
@end
@implementation SEMusicOperationViewHandler
@synthesize mSelectedMusicView;
- (id)init
{
    self = [super init];
    if(self)
    {
        mViewNav = [PhotoFrameAppDelegate getViewNavigator];
    }
    return self;
}

- (void) handleOperation:(NSString *)op view:(SEOperationView *)opView
{
    if([op isEqualToString: @"delete_op"])
    {
        NSLog(@"delete selectec music");
        [mSelectedMusicView removeSelectedMusic];
    }
}
@end
//////////////////////////
@implementation SEMusicSelectedImpl
@synthesize mMusicTableView;
@synthesize mMusicCellBgStr;
@synthesize mMusicCellHBgStr;
@synthesize mViewNav;
@synthesize mMusicResourceType;
@synthesize mSelectedMusicView;
@synthesize mSelectedMusicItemArray;
@synthesize mAlbumButton;
@synthesize mTitleButton;
@synthesize mArtistButton;
@synthesize mTableCellType;
@synthesize mOrigRect;
@synthesize mParent;
- (void) initButton
{
    UIButton* titleButton = mTitleButton;
    UIButton* artistButton = mArtistButton;
    UIButton* albumButton = mAlbumButton;
    titleButton.backgroundColor = [UIColor clearColor];
    artistButton.backgroundColor = [UIColor clearColor];
    albumButton.backgroundColor = [UIColor clearColor];

    
    UIImage* titleButtonImageNormal = nil;
    UIImage* titleButtonImageSelected = nil;
    UIImage* artistButtonImageNormal = nil;
    UIImage* artistButtonImageSelected = nil;
    UIImage* albumButtonNormal = nil;
    UIImage* albumButtonSelected = nil;
    if(mMusicResourceType == MUSIC_RESOURCE_LIB)
    {
        titleButtonImageNormal =     [UIImage imageNamed:@"Filter_botton_004.png"];

        titleButtonImageSelected =     [UIImage imageNamed:@"Filter_botton_b004.png"];

        artistButtonImageNormal =     [UIImage imageNamed:@"Filter_botton_003.png"];

        artistButtonImageSelected =     [UIImage imageNamed:@"Filter_botton_b003.png"];

        albumButtonNormal =     [UIImage imageNamed:@"Filter_botton_002.png"];

        albumButtonSelected =     [UIImage imageNamed:@"Filter_botton_b002.png"];

    }
    else
    {
        titleButtonImageNormal = [UIImage imageNamed:@"Filter_botton_006.png"];
        titleButtonImageSelected = [UIImage imageNamed:@"Filter_botton_b006.png"];
        artistButtonImageNormal = [UIImage imageNamed:@"Filter_botton_005.png"];
        artistButtonImageSelected = [UIImage imageNamed:@"Filter_botton_b005.png"];
        albumButtonNormal = [UIImage imageNamed:@"Filter_botton_001.png"];
        albumButtonSelected = [UIImage imageNamed:@"Filter_botton_b001.png"];
    }
    [titleButton setBackgroundImage:titleButtonImageNormal forState:UIControlStateNormal];
    [titleButton setBackgroundImage:titleButtonImageSelected forState:UIControlStateHighlighted];
    
    [artistButton setBackgroundImage:artistButtonImageNormal forState:UIControlStateNormal];
    [artistButton setBackgroundImage:artistButtonImageSelected forState:UIControlStateHighlighted];
    
    [albumButton setBackgroundImage:albumButtonNormal forState:UIControlStateNormal];
    [albumButton setBackgroundImage:albumButtonSelected forState:UIControlStateHighlighted];
    
    [titleButton addTarget:self  action:@selector(handleTitleSortButton:) forControlEvents:UIControlEventTouchUpInside];
    [artistButton addTarget:self action:@selector(handleArtistSortButton:) forControlEvents:UIControlEventTouchUpInside];
    [albumButton addTarget:self  action:@selector(handleAlbumSortButton:) forControlEvents:UIControlEventTouchUpInside];
}
- (void) initData
{
    mMusicTableView.multipleTouchEnabled = NO;
    mMusicTableView.delegate = self;
    mMusicTableView.dataSource = self;
    mMusicTableView.separatorStyle = UITableViewCellSeparatorStyleNone;
    mMusicTableView.allowsSelection = NO;
    mMusicTableView.multipleTouchEnabled = NO;
    self.mSelectedMusicItemArray = [NSMutableArray array];
    if(SEDB)
    {
        [self initMusicArray];
    }
    else
    {
        [self loadMusic];
    }
    mSortType = SESORT_BYTITLE;
    mOrigRect = mMusicTableView.frame;
    mTableCellType = TABLE_CELL_BIG;
    [self initButton];
    mCurrentPlayItem = -1;
    mCollisionedRow= -1;
}
- (void) dealloc
{
    [mMusicCellBgStr release];
    [mMusicCellHBgStr release];
    [mSelectedMusicItemArray release];
    [mNormalCellBg release];
    [mHighlightedCellBg release];
    [mNullCellBg release];
    [mMusicItemArray release];
    [super dealloc];
}

- (UITableViewCell*) createCell:(UITableView *)tableView
{
    static NSString *CellIdentifierBig = @"MusicPickerCell";
    static NSString *CellIdentifierSmall = @"MusicPickerCellSmall";
    NSString* CellIdentifier = nil;
    if(mTableCellType == TABLE_CELL_SMALL)
    {
        CellIdentifier = CellIdentifierSmall;
    }
    else
    {
        CellIdentifier = CellIdentifierBig;
    }
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier];
    if (cell == nil) 
    {
        NSLog(@"create new cell");
        if(mTableCellType == TABLE_CELL_BIG)
        {
            cell = [[[NSBundle mainBundle] loadNibNamed:@"MusicPickerTableViewCellBig" owner:nil options:nil] lastObject];
        }
        else 
        {
            cell = [[[NSBundle mainBundle] loadNibNamed:@"MusicPickerTableViewCell" owner:nil options:nil] lastObject];
        }
        //cell.reuseIdentifier = CellIdentifier;
        cell.userInteractionEnabled = YES;
        //cell.multipleTouchEnabled = YES;
        CGRect frame = cell.frame;
        //NSLog(@"cell frame = %f, %f, %f, %f", frame.origin.x, frame.origin.y, frame.size.width, frame.size.height);
        for(UIView* subv in cell.subviews)
        {
            //subv.multipleTouchEnabled = YES;
            subv.userInteractionEnabled = YES;
        }
        
    }
    [self clearCell:cell];
    return cell;
}

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    return 1;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    return mMusicItemCount;
}
- (BOOL) isItemSelected: (SEMusicItemProperty*)item
{
    for(int i = 0 ; i < mSelectedMusicItemArray.count ; i++)
    {
        SEMusicItemProperty* ip = [mSelectedMusicItemArray objectAtIndex:i];
        if(ip == item)
            return YES;
    }
    return NO;
}
- (void) normalizeCell : (UITableViewCell*)cell
{
    UIImageView* background = (UIImageView*)[cell viewWithTag:655350];
    background.image = [self getCellNormalBg];
}
- (void) normalizeCell
{
    for(int i = 0 ; i < mSelectedMusicItemArray.count ; i++)
    {
        SEMusicItemProperty* si = [mSelectedMusicItemArray objectAtIndex:i];
        UITableViewCell* cell = [mMusicTableView cellForRowAtIndexPath:[NSIndexPath indexPathForRow:[si.seq intValue] inSection:0]];
        [self normalizeCell: cell];
    }
}
- (BOOL) isMusicItemInSelectedMusicItemArray: (SEMusicItemProperty*)item
{
    for(int i = 0 ; i < mSelectedMusicItemArray.count ; i++)
    {
        SEMusicItemProperty* si = [mSelectedMusicItemArray objectAtIndex:i];
        if(item == si)
            return YES;
    }
    return NO;
}
- (int) getHighlightedViewNum
{
    return mSelectedMusicItemArray.count;
}
- (UIView*) createFloatView
{
    SEUIFloatView* view = [[SEUIFloatView alloc] initWithFrame:CGRectMake(0, 0, 140, 140)];
    //mFloatView.backgroundColor = [UIColor greenColor];
    view.backgroundColor = [UIColor clearColor];
    view.contentMode = UIViewContentModeCenter;
    view.backgroundImage = [mViewNav.mResLoader getImage:@"ImageFloatViewBg"];
    view.image = [mViewNav.mResLoader getImage:@"MusicSelectFloatViewFg"];
    [view setCount:[self getHighlightedViewNum]];
    return [view autorelease];
}
- (void) handleLongPress: (UILongPressGestureRecognizer*)longGes
{
    //NSLog(@"## my long press %d ###", longGes.state);
    
    UIGestureRecognizerState state = longGes.state;
    CGPoint loc = [longGes locationInView:mViewNav.mRootView];
    CGFloat deltay = 20;
    
    UITableViewCell* cell = (UITableViewCell*)longGes.view;
    //UILabel* oldTitle1 = (UILabel*)[cell viewWithTag:102];
    //UILabel* oldTitle2 = (UILabel*)[cell viewWithTag:103];
    if(cell.tag >= mMusicItemArray.count)
        return;
    if(state == UIGestureRecognizerStateBegan)
    {
        //UIView* view = [[UIView alloc] initWithFrame:CGRectMake(loc.x - 100, loc.y - 100, 100, 100)];
        //UIView* view = [[[NSBundle mainBundle] loadNibNamed:@"MusicFloatView" owner:self options:nil] lastObject];
        if(mViewNav.mMusicFloatView != nil)
            return;
        UIView* view = [self createFloatView];
        view.frame = CGRectMake(loc.x, loc.y, view.frame.size.width, view.frame.size.height);
        view.center = CGPointMake(loc.x, loc.y - deltay);
        SEMusicItemProperty* item = [mMusicItemArray objectAtIndex:cell.tag];
        /*
        UILabel* title1 = (UILabel*)[view viewWithTag:102];
        UILabel* title2 = (UILabel*)[view viewWithTag:103];
        UIImageView* bgView = (UIImageView*)[view viewWithTag:101];
        bgView.image = [self getCellNormalBg];
        title1.text = item.title;
        title2.text = item.artist;
        title1.textColor = oldTitle1.textColor;
        title2.textColor = oldTitle2.textColor;
         */
        mViewNav.mMusicFloatView = view;
        [mViewNav.mRootView addSubview:view];
        view.backgroundColor = [UIColor clearColor];
        mViewNav.mCurrentFloatViewType = FLOATVIEW_MUSIC;
        if(mMusicResourceType == MUSIC_RESOURCE_CORE_DATA)
        {
            NSLog(@"show operation view");
            NSArray* operators = [NSArray arrayWithObjects:@"delete_op", nil];
            NSArray* deleteImages = [NSArray arrayWithObjects:@"OperationViewDeleteBackground", @"OperationViewDeleteForeground", @"OperationViewDeleteBackgroundH", @"OperationViewDeleteForegroundH", nil];
            NSArray* resources = [NSArray arrayWithObjects:deleteImages, nil];
            SEOperationViewGroup* operationViewGroup = [[[SEOperationViewGroup alloc] initWithOperators: operators withImageResource:resources] autorelease];
            SEMusicOperationViewHandler* handler = [[[SEMusicOperationViewHandler alloc] init] autorelease];
            handler.mSelectedMusicView = (SESelectedMusicView*)mParent;
            operationViewGroup.mOperationHandler = handler;
            
            [mViewNav popupOperationViewGroup:operationViewGroup];
        }
        [mViewNav.mRootView bringSubviewToFront:view];
        if([self isMusicItemInSelectedMusicItemArray: item] == NO)
        {
            [mSelectedMusicItemArray addObject:item];
            [self selectCell:cell];
            item.highlighted = YES;
        }
        NSLog(@"long press item = %@", item);
        NSLog(@"selected music array count = %d", mSelectedMusicItemArray.count);
    }
    else if(state == UIGestureRecognizerStateChanged)
    {
        NSLog(@"before move collision row = %d", mCollisionedRow);
        mViewNav.mMusicFloatView.center = CGPointMake(loc.x, loc.y - deltay);
        BOOL bIntersect = NO;
        if(mMusicResourceType != MUSIC_RESOURCE_LIB)
        {
            bIntersect = [mViewNav intersectWithOperationView];
        }
        [mViewNav determineSelectedMusicRow];
        if(bIntersect == YES && mMusicResourceType == MUSIC_RESOURCE_CORE_DATA)
        {
            NSLog(@"intersec with delete icon");
            [self resetCollisoinedRowRect];
            mCollisionedRow = -1;
        }
        NSLog(@"## move collision row = %d ##", mCollisionedRow);
    }
    else if(state == UIGestureRecognizerStateEnded || state == UIGestureRecognizerStateCancelled || state == UIGestureRecognizerStateFailed)
    {
        NSLog(@"## end collision row = %d ##", mCollisionedRow);
        if(mMusicResourceType == MUSIC_RESOURCE_LIB)
        {
            [mViewNav resetSelectedMusicViewCollisionedRow];
            NSMutableArray* addedArray = [NSMutableArray array];
            if(mSelectedMusicItemArray.count > 0)
            {
                for(int i = 0 ; i < mSelectedMusicItemArray.count ; i++)
                {
                    SEMusicItemProperty* si = [mSelectedMusicItemArray objectAtIndex:i];
                    SEMusicItemProperty* newItem = [[SEMusicItemProperty alloc] init];
                    newItem.title = si.title;
                    newItem.artist = si.artist;
                    newItem.album = si.album;
                    [addedArray addObject:newItem];
                    [newItem release];
                }
            }
            BOOL bAdded = [mViewNav addMusicToSelectedView: addedArray];
            if(bAdded)
            {
                [self normalizeCell];
                for(int i = 0 ; i < self.mSelectedMusicItemArray.count ; i++)
                {
                    SEMusicItemProperty* item = [mSelectedMusicItemArray objectAtIndex:i];
                    item.highlighted = NO;
                }
                self.mSelectedMusicItemArray = [NSMutableArray array];
                [self setHighlightedCount];
            }
        }
        else
        {
            if(mCollisionedRow >= 0)
            {
                //[mMusicItemArray removeObjectsInArray:mSelectedMusicItemArray]
                if(mCollisionedRow > mMusicItemArray.count)
                    mCollisionedRow = mMusicItemArray.count;
                for(int i = 0 ; i < mSelectedMusicItemArray.count ; i++)
                {
                    SEMusicItemProperty* item = [mSelectedMusicItemArray objectAtIndex:i];
                    NSLog(@"item seq = %d", [item.seq intValue]);
                }
                NSMutableArray* needRemovedRow = [NSMutableArray array];
                for(int i = 0 ; i < mSelectedMusicItemArray.count ; i++)
                {
                    int row = [[[mSelectedMusicItemArray objectAtIndex:i] seq] intValue];
                    [needRemovedRow addObject:[NSNumber numberWithInt:row]];
                }
                BOOL (^indexInRowArray)(int, NSArray*) = ^(int index, NSArray* rowArray)
                {
                    for(int i = 0 ; i < rowArray.count ; i++)
                    {
                        if(index == [[rowArray objectAtIndex:i] intValue])
                        {
                            return YES;
                        }
                    }
                    return NO;
                };
                if(mCollisionedRow >= mMusicItemArray.count)
                    mCollisionedRow = mMusicItemArray.count;
                NSMutableArray* beforeCollisionedRow = [NSMutableArray array];
                for(int i = 0 ; i < mMusicItemArray.count ; i++)
                {
                    if(i < mCollisionedRow && !indexInRowArray(i, needRemovedRow))
                    {
                        [beforeCollisionedRow addObject:[NSNumber numberWithInt:i]];
                    }
                }
                NSMutableArray* afterCollisionedRow = [NSMutableArray array];
                for(int i = 0 ; i < mMusicItemArray.count ; i++)
                {
                    if(i > mCollisionedRow && !indexInRowArray(i, needRemovedRow))
                    {
                        [afterCollisionedRow addObject:[NSNumber numberWithInt:i]];
                    }
                }
                NSMutableArray* newRowArray = [NSMutableArray array];
                for(int i = 0 ; i < beforeCollisionedRow.count ; i++)
                {
                    [newRowArray addObject:[beforeCollisionedRow objectAtIndex:i]];
                }
                for(int i = 0 ; i < needRemovedRow.count ; i++)
                {
                    [newRowArray addObject:[needRemovedRow objectAtIndex:i]];
                }
                if(mCollisionedRow < mMusicItemArray.count)
                {
                    if(!indexInRowArray(mCollisionedRow, needRemovedRow))
                    {
                        [newRowArray addObject:[NSNumber numberWithInt:mCollisionedRow]];
                    }
                }
                for(int i = 0 ; i < afterCollisionedRow.count ; i++)
                {
                    [newRowArray addObject:[afterCollisionedRow objectAtIndex:i]];
                }
                assert(newRowArray.count == mMusicItemArray.count);
                for(int i = 0 ; i < newRowArray.count; i++)
                {
                    int index = [[newRowArray objectAtIndex:i] intValue];
                    NSLog(@"%d ", index);
                }
                NSLog(@"\n");
                NSMutableArray* newItemArray = [NSMutableArray array];
                for(int i = 0 ; i < mMusicItemArray.count; i++)
                {
                    int index = [[newRowArray objectAtIndex:i] intValue];
                    [newItemArray addObject:[mMusicItemArray objectAtIndex: index]];
                }
                //[self normalizeCell];
                [mViewNav resetSelectedMusicViewCollisionedRow];
                self.mSelectedMusicItemArray = [NSMutableArray array];
                [mMusicItemArray release];
                mMusicItemArray = [newItemArray retain];
                for(int i = 0 ; i < mMusicItemArray.count ; i++)
                {
                    SEMusicItemProperty* item = [mMusicItemArray objectAtIndex:i];
                    item.seq = [NSNumber numberWithInt:i];
                    if(item.highlighted)
                    {
                        item.highlighted = NO;
                    }
                }
                [self setHighlightedCount];
                [mMusicTableView reloadData];
                [mViewNav setSelectedMusicToCoreData:mMusicItemArray];
            }
            
        }
        [mViewNav.mMusicFloatView removeFromSuperview];
        mViewNav.mMusicFloatView = nil;
        mViewNav.mCurrentFloatViewType = NO_FLOATVIEW;
        [mViewNav disappearOperationViewGroup];
        //mCurrentLongPressItem = nil;
        mCollisionedRow = -1;
        [mViewNav saveCoreDataContext];
    }
}


/*
- (void) removeItem: (SEMusicItemProperty*)item
{
    NSMutableArray* deleteArray = [NSMutableArray array];
    for(int i = 0 ; i < mSelectedMusicItemArray.count ; i++)
    {
        SEMusicItemProperty* ip = [mSelectedMusicItemArray objectAtIndex:i];
        if(ip == item)
        {
            [deleteArray addObject:ip];
        }
    }
    [mSelectedMusicItemArray removeObjectsInArray:deleteArray];
}
 */
- (void) selectCell: (UITableViewCell*)cell 
{
    //frameView.image = [mViewNav.mResLoader getImage:@"MusicPickerFrameIcon"];
    UIImageView* background = (UIImageView*)[cell viewWithTag:655350];
    background.image = [self getCellHighlightedBg];

}

- (UITableViewCell*) findCell: (UIView*)v
{
    UIView* parent = v.superview;
    while(parent != nil)
    {
        if([parent isKindOfClass:[UITableViewCell class]])
        {
            break;
        }
        else
        {
            parent = parent.superview;   
        }
    }
    return (UITableViewCell*)parent;
}
- (void) handleTap: (UITapGestureRecognizer*)tap
{
    NSLog(@"## tap ##");
    UITableViewCell* cell = [self findCell:tap.view];//(UITableViewCell*)background.superview;
    NSLog(@"tap cell index = %d", cell.tag);
    //cell.tag will be greater than mMusicItemArray.count
    if(cell.tag >= mMusicItemArray.count)
        return;
    SEMusicItemProperty* item = [mMusicItemArray objectAtIndex:cell.tag];
    //item.highlighted = !item.highlighted;
    NSLog(@"tap item = %@, %@, %@", item.title, item.artist, item.album);
    if(item.highlighted)
    {
        [self normalizeCell:cell];
        void (^removeItem)(SEMusicItemProperty*item) = ^(SEMusicItemProperty* item)
        {
            NSMutableArray* deleteArray = [NSMutableArray array];
            for(int i = 0 ; i < mSelectedMusicItemArray.count ; i++)
            {
                SEMusicItemProperty* ip = [mSelectedMusicItemArray objectAtIndex:i];
                if(ip == item)
                {
                    [deleteArray addObject:ip];
                }
            }
            [mSelectedMusicItemArray removeObjectsInArray:deleteArray];
        };
        removeItem(item);
        item.highlighted = NO;
        //[self removeItem:item];
    }
    else
    {
        [self selectCell:cell];
        [mSelectedMusicItemArray addObject:item];
        item.highlighted = YES;
    }
    [mSelectedMusicItemArray sortUsingComparator:^NSComparisonResult(id obj1, id obj2) {
        SEMusicItemProperty* item1 = (SEMusicItemProperty*)obj1;
        SEMusicItemProperty* item2 = (SEMusicItemProperty*)obj2;
        return [item1.seq compare:item2.seq];
        
    }];
    [self setHighlightedCount];
}
/*
- (void)tableView:(UITableView *)tableView willDisplayCell:(UITableViewCell *)cell forRowAtIndexPath:(NSIndexPath *)indexPath 
{
    SEMusicItemProperty* musicItemProperty = nil;
    if(mMusicItemArray.count > 0 && indexPath.row < mMusicItemArray.count)
    {
        musicItemProperty = [self getMusicItemProperty:indexPath.row];
    }
    [self setCellProperty:cell :musicItemProperty];    
}
 */
// Customize the appearance of table view cells.
- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    UITableViewCell* cell = [self createCell:tableView];
    cell.tag = indexPath.row;
    NSLog(@"cell row = %d",cell.tag);
    
    SEMusicItemProperty* musicItemProperty = nil;
    if(mMusicItemArray.count > 0 && indexPath.row < mMusicItemArray.count)
    {
        musicItemProperty = [self getMusicItemProperty:indexPath.row];
    }
    [self setCellProperty:cell :musicItemProperty];
    
    return cell;
}
- (CGFloat)tableView:(UITableView *)tableView heightForRowAtIndexPath:(NSIndexPath *)indexPath
{
    //UITableViewCell* cell = [self createCell:tableView];
    float height = 90;//this must be the same as MusicPickerTableVeiwCell.xib
    //NSLog(@"cell height = %f", height);
    return height;
}
- (int) findItemIndexInMusicItemArray: (SEMusicItemProperty*)item
{
    for(int i = 0 ; i < mMusicItemArray.count ; i++)
    {
        SEMusicItemProperty* mi = [mMusicItemArray objectAtIndex:i];
        if(mi == item)
            return i;
    }
    return -1;
}
- (BOOL) addNewMusic: (NSArray*)items
{
    if(mCollisionedRow == -1)
    {
        return NO;
    }
    else 
    {
        if((items.count + mMusicItemArray.count) > mMusicItemCount)
        {
            NSLog(@"the music num exceed total music slot");
            NSLog(@"added item count = %d", items.count);
            NSLog(@"current items count = %d", mMusicItemArray.count);
            NSLog(@"music all slot count = %d", mMusicItemCount);
            if(mMusicItemArray.count == mMusicItemCount)
            {
                return NO;
            }
            else
            {
                int remainCount = mMusicItemCount - mMusicItemArray.count;
                assert(remainCount > 0);
                NSArray* newItems = [NSArray array];
                for(int i = 0 ; i < remainCount ; i++)
                {
                    newItems = [newItems arrayByAddingObject:[items objectAtIndex:i]];
                }
                items = newItems;
            }
        }
        NSMutableArray* newArray = [NSMutableArray arrayWithArray:[NSArray array]];
        if(mCollisionedRow >= mMusicItemArray.count)
            mCollisionedRow = -2;
        if(mCollisionedRow >= 0)
        {
            for(int i = 0 ; i < mCollisionedRow ; i++)
            {
                [newArray addObject:[mMusicItemArray objectAtIndex:i]];
            }
            for(int i = 0 ; i < items.count ; i++)
            {
                [newArray addObject:[items objectAtIndex:i]];
            }
            for(int i = mCollisionedRow ; i < mMusicItemArray.count ; i++)
            {
                [newArray addObject:[mMusicItemArray objectAtIndex:i]];
            }
            [mMusicItemArray release];
            mMusicItemArray = [newArray retain];
        }
        else 
        {
            [mMusicItemArray addObjectsFromArray:items];
        }
        //mMusicItemCount = mMusicItemArray.count;
    }
    for(int i = 0 ; i < mMusicItemArray.count ; i++)
    {
        SEMusicItemProperty* item = [mMusicItemArray objectAtIndex:i];
        item.seq = [NSNumber numberWithInt:i];
    }
    
    //when add new music , the index of every row will be changed
    //NSArray* newSelectedArray = [NSArray array];
    for(int i = 0 ; i < mSelectedMusicItemArray.count ; i++)
    {
        SEMusicItemProperty* selectItem = [mSelectedMusicItemArray objectAtIndex:i];
        int index = [self findItemIndexInMusicItemArray:selectItem];
        assert(index != -1);
        selectItem.seq = [NSNumber numberWithInt:index];
    }
    //[mSelectedMusicItemArray release];
    //mSelectedMusicItemArray = [[NSMutableArray arrayWithArray:newSelectedArray] retain];
    [mMusicTableView reloadData];
    [mViewNav setSelectedMusicToCoreData:mMusicItemArray];
    mViewNav.mMusicPlayListChange = YES;
    mCollisionedRow = -1;
    return YES;
}

- (void) determineSelectedMusicRow: (UIView*)floatView
{
    CGPoint selectedViewPoint = floatView.center;//[mViewNav.mRootView convertPoint:floatView.center toView:mMusicTableView];
    UIView* musicTableSuperView = mMusicTableView.superview;
    CGPoint startMusicTablePoint = [musicTableSuperView convertPoint:CGPointMake(mMusicTableView.frame.origin.x, mMusicTableView.frame.origin.y) toView:mViewNav.mRootView];
    CGPoint endMusicPoint = [musicTableSuperView convertPoint:CGPointMake(mMusicTableView.frame.origin.x + mMusicTableView.frame.size.width, mMusicTableView.frame.origin.y + mMusicTableView.frame.size.height) toView:mViewNav.mRootView];
    BOOL inSelectedView = NO;
    NSLog(@"## in selected table view: %f, %f", selectedViewPoint.x, selectedViewPoint.y);
    NSLog(@"## musictable size = %f, %f", mMusicTableView.frame.size.width, mMusicTableView.frame.size.height);
    NSLog(@"## music in root = (%f, %f) , (%f, %f)", startMusicTablePoint.x, startMusicTablePoint.y, endMusicPoint.x, endMusicPoint.y);
    //if(selectedViewPoint.x >= 0 && selectedViewPoint.x < (mMusicTableView.frame.size.width) &&
    //   selectedViewPoint.y >= 0 && selectedViewPoint.y < (mMusicTableView.frame.size.height))
    if(selectedViewPoint.x >= startMusicTablePoint.x && selectedViewPoint.x <= endMusicPoint.x && selectedViewPoint.y >= startMusicTablePoint.y && selectedViewPoint.y <= endMusicPoint.y)
    {
        inSelectedView = YES;
    }
    if(inSelectedView == NO)
    {
        NSLog(@"not in selected music table");
        [self resetCollisoinedRowRect];
        mCollisionedRow = -1;
        return;
    }

    NSArray* subviews = [mMusicTableView visibleCells];
    NSLog(@"## subviews count = %d ##", subviews.count);
    BOOL found = NO;
    for(UIView* v in subviews)
    {
        CGPoint p = [mViewNav.mRootView convertPoint:floatView.center toView:v];
        if([v isKindOfClass:[UITableViewCell class]] == NO)
            continue;
        UITableViewCell* cell = (UITableViewCell*)v;

        if(p.x >= 0 && p.x < (v.frame.size.width) 
           && p.y >= 0 && p.y < (v.frame.size.height))
        {
            found = YES;
            //mHighlightedRow = cell.tag;
            NSLog(@"current collisioned row is %d, last collisioned row is %d", cell.tag, mCollisionedRow);
            if(cell.tag != mCollisionedRow)
            {
                UITableViewCell* prevCell = [self findCellByRow: mCollisionedRow];
                if(prevCell != nil)
                    prevCell.frame = mCurrentHighlightedCellRect;
                mCurrentHighlightedCellRect = cell.frame;
                mCollisionedRow = cell.tag;
                CGPoint center = cell.center;
                CGPoint p = CGPointMake(center.x, center.y + 20);
                void (^animBlock) (void) = ^{
                    cell.center = p;
                };
                void (^animFinished) (BOOL) = ^ (BOOL)
                {
                };
                [UIView animateWithDuration:0.1 animations:animBlock completion:animFinished];
            }
            break;
        }
    }
    if(found == NO)
    {
        //mHighlightedRow = -2;
        [self resetCollisoinedRowRect];
        mCollisionedRow = -2;
    }

}
- (void) resetCollisoinedRowRect
{
    NSLog(@"reseted collisioned row = %d", mCollisionedRow);
    if(mCollisionedRow >= 0)
    {
        UITableViewCell* cell = [self findCellByRow:mCollisionedRow];
        cell.frame = mCurrentHighlightedCellRect;
    }
}
- (SEMusicItemProperty*) findItemInMusicItemArray: (SEMusicItemProperty*)item
{
    NSLog(@"item = %@, %@, %@", item.title, item.artist, item.album);
    for(SEMusicItemProperty* i in mMusicItemArray)
    {
        NSLog(@"compared item = %@, %@, %@", i.title, i.artist, i.album);
        if([i isEqualToMusicItemProperty:item])
            return i;
    }
    return nil;
}

- (void) removeSelectedMusic
{
    NSLog(@"start remove music");
    NSMutableIndexSet* indexSet = [NSMutableIndexSet indexSet];
    
    for(int i = 0 ; i < mSelectedMusicItemArray.count; i++)
    {
        SEMusicItemProperty * item = [mSelectedMusicItemArray objectAtIndex:i];
        [indexSet addIndex:[item.seq intValue]];
        NSLog(@"music = %@, %@, %@", item.title, item.artist, item.album);
        if(item.musicPlayState == MUSIC_PLAY)
        {
            [self stopMusicPlay];
        }
    }
    [mMusicItemArray removeObjectsAtIndexes:indexSet];
    for(int i = 0 ; i < mMusicItemArray.count ; i++)
    {
        SEMusicItemProperty* item = [mMusicItemArray objectAtIndex:i];
        item.seq = [NSNumber numberWithInt:i];
    }
    [mViewNav setSelectedMusicToCoreData:mMusicItemArray];
    mViewNav.mMusicPlayListChange = YES;
    self.mSelectedMusicItemArray = [NSMutableArray arrayWithArray:[NSArray array]];
    [mMusicTableView reloadData];
    [self setHighlightedCount];
}
- (MPMediaItem*)findMediaItemByItemProperty: (SEMusicItemProperty*)item
{
    /*
    for(SEMusicItemProperty* pitem in mMusicItemArray)
    {
        if([pitem isEqualToMusicItemProperty:item])
            return pitem.song;
    }
     */
    NSArray* items = [SEUtil findMediaItemByTitle:item.title aritst:item.artist album:item.album];
    if(items.count > 0)
        return nil;
    else 
    {
        return [items objectAtIndex:0];    
    }
}
- (void) pauseAllMusic
{
    int currentPlayIndex = [self findCurrentPlayMusicItem];
    if(currentPlayIndex != -1)
    {
        SEMusicItemProperty* itemProperty = [mMusicItemArray objectAtIndex:currentPlayIndex];
        MPMusicPlayerController* player = [MPMusicPlayerController applicationMusicPlayer];
        //MPMusicPlaybackState playbackState = player.playbackState;
        [player stop];
        itemProperty.musicPlayState = MUSIC_PAUSE;
        [self setPlaybackIconPlay:currentPlayIndex];
        mViewNav.mMusicPlayListChange = YES;
    }
    
    
}

@end
//////
//////
@implementation SEMusicPickerView
@synthesize mViewNav;
- (BOOL)canAdjust
{
    return YES;
}
- (void) updateMusicView
{
    [mMusicSelectedImpl.mMusicTableView reloadData];
}
- (MPMediaItem*)findMediaItemByItemProperty: (SEMusicItemProperty*)item
{
    return [mMusicSelectedImpl findMediaItemByItemProperty:item];
}
- (void) handleWhenStop: (BOOL) bStopInMid
{
    
    if(bStopInMid)
    {
        float contentWidth = (mViewNav.mViewPortWidth - mViewNav.mBarWidth) / 2 - 22;
        NSLog(@"view port width = %d, bar width = %f", mViewNav.mViewPortWidth, mViewNav.mBarWidth);
        NSLog(@"music picker view = %f, %f, %f, %f", self.frame.origin.x, self.frame.origin.y, self.frame.size.width, self.frame.size.height);
        CGRect rect = CGRectMake(  mMusicSelectedImpl.mOrigRect.origin.x +  mMusicSelectedImpl.mOrigRect.size.width - contentWidth, mMusicSelectedImpl.mMusicTableView.frame.origin.y, contentWidth, mMusicSelectedImpl.mMusicTableView.frame.size.height);
        mMusicSelectedImpl.mMusicTableView.frame = rect;
        mMusicSelectedImpl.mTableCellType = TABLE_CELL_SMALL;
    }
    else 
    {
        mMusicSelectedImpl.mTableCellType = TABLE_CELL_BIG;
        mMusicSelectedImpl.mMusicTableView.frame = mMusicSelectedImpl.mOrigRect;
        
    }
    [mMusicSelectedImpl.mMusicTableView reloadData];
    //[self initData]; 
}
- (void) initBackground
{
    self.backgroundColor = [UIColor clearColor];
    CGRect pickerFrame = mMusicSelectedImpl.mMusicTableView.frame;
    UIImage* image = [mViewNav.mResLoader getImage:@"MusicPickerBackground"];
    UIGraphicsBeginImageContext(pickerFrame.size);
    [image drawAtPoint:CGPointMake(-pickerFrame.origin.x, -pickerFrame.origin.y)];
    UIImage* newImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    UIImageView* musicPickerImageView = [[UIImageView alloc] init];
    musicPickerImageView.image = newImage;
    mMusicSelectedImpl.mMusicTableView.backgroundView = musicPickerImageView;
    [musicPickerImageView release];
}

- (void) initData
{
    mMusicSelectedImpl = [[SEMusicSelectedImpl alloc] init];
    mMusicSelectedImpl.mViewNav = mViewNav;
    mMusicSelectedImpl.mParent = self;
    mMusicSelectedImpl.mMusicResourceType = MUSIC_RESOURCE_LIB;
    mMusicSelectedImpl.mMusicTableView = (UITableView*)[self viewWithTag:501];
    mMusicSelectedImpl.mTitleButton = (UIButton*)[self viewWithTag:502];
    mMusicSelectedImpl.mArtistButton = (UIButton*)[self viewWithTag:503];  
    mMusicSelectedImpl.mAlbumButton = (UIButton*)[self viewWithTag:504];
    mMusicSelectedImpl.mMusicTableView.multipleTouchEnabled = NO;
    mMusicSelectedImpl.mTableCellType = TABLE_CELL_BIG;
    mMusicSelectedImpl.mMusicCellBgStr = @"MusicPickerItemBackground";
    mMusicSelectedImpl.mMusicCellHBgStr = @"MusicPickerSelectedCellBg";
    [self initBackground];
    [mMusicSelectedImpl initData];
    //[self initButtonHandler];
    //[self setTouchHandler];
}

- (void) dealloc
{

    [mMusicSelectedImpl release];
    [super dealloc];
}
- (void) pauseMusic
{
    [mMusicSelectedImpl pauseAllMusic];
}
@end
//////
@implementation SESelectedMusicView
@synthesize mViewNav;
- (void) pauseMusic
{
    [mMusicSelectedImpl pauseAllMusic];
}
- (BOOL) canAdjust
{
    return YES;
}
- (void) updateMusicView
{
    [mMusicSelectedImpl.mMusicTableView reloadData];
}
- (void)handleWhenStop: (BOOL) bStopInMid
{
    if(bStopInMid)
    {
        //float margin = 30;
        float contentWidth = (mViewNav.mViewPortWidth - mViewNav.mBarWidth) / 2 - 22;
        CGRect rect = CGRectMake(10, mMusicSelectedImpl.mMusicTableView.frame.origin.y, contentWidth, mMusicSelectedImpl.mMusicTableView.frame.size.height);
        mMusicSelectedImpl.mMusicTableView.frame = rect;
        mMusicSelectedImpl.mTableCellType = TABLE_CELL_SMALL;
    }
    else 
    {
        mMusicSelectedImpl.mTableCellType = TABLE_CELL_BIG;
        mMusicSelectedImpl.mMusicTableView.frame = mMusicSelectedImpl.mOrigRect;
        
    }
    NSLog(@"stop in mid reload data");
    [mMusicSelectedImpl.mMusicTableView reloadData];    
    //[self initData];
}

- (void) initBackground
{
    CGRect selectedMusicFrame = mMusicSelectedImpl.mMusicTableView.frame;
    UIImage* image = [mViewNav.mResLoader getImage:@"SelectedMusicViewBackground"];
    UIGraphicsBeginImageContext(selectedMusicFrame.size);
    [image drawAtPoint:CGPointMake(-selectedMusicFrame.origin.x, -selectedMusicFrame.origin.y)];
    UIImage* newImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    UIImageView* selectedImageView = [[UIImageView alloc] init];
    selectedImageView.image = newImage;
    mMusicSelectedImpl.mMusicTableView.backgroundView = selectedImageView;
    [selectedImageView release];
}


- (void) determineSelectedMusicRow:(UIView *)floatView
{
    [mMusicSelectedImpl determineSelectedMusicRow:floatView];
}
- (BOOL) addNewMusic:(NSArray *)items
{
    return [mMusicSelectedImpl addNewMusic:items];
}
- (void)removeSelectedMusic
{
    [mMusicSelectedImpl removeSelectedMusic];
}
- (void) resetCollisoinedRowRect
{
    [mMusicSelectedImpl resetCollisoinedRowRect];
}
- (void) initData
{
    mMusicSelectedImpl = [[SEMusicSelectedImpl alloc] init];
    mMusicSelectedImpl.mParent = self;
    mMusicSelectedImpl.mMusicResourceType = MUSIC_RESOURCE_CORE_DATA;
    mMusicSelectedImpl.mMusicTableView.multipleTouchEnabled = NO;
    mMusicSelectedImpl.mTableCellType = TABLE_CELL_BIG;
    mMusicSelectedImpl.mMusicCellBgStr = @"SelectedMusicViewItemBackground";
    mMusicSelectedImpl.mMusicCellHBgStr = @"MusicSelectedViewSelectedCellBg";
    mMusicSelectedImpl.mMusicTableView = (UITableView*)[self viewWithTag:501];
    mMusicSelectedImpl.mTitleButton = (UIButton*)[self viewWithTag:502];
    mMusicSelectedImpl.mArtistButton = (UIButton*)[self viewWithTag:503];
    mMusicSelectedImpl.mAlbumButton = (UIButton*)[self viewWithTag:504];
    mMusicSelectedImpl.mViewNav = mViewNav;
    [self initBackground];
    [mMusicSelectedImpl initData];
    mMusicSelectedImpl.mMusicTableView.backgroundColor = [UIColor redColor];
    self.backgroundColor = [UIColor clearColor];

}
- (void) dealloc
{
    [mMusicSelectedImpl release];
    [super dealloc];
}
@end
////
