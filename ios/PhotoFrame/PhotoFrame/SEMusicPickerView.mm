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
static void handlePlayerPlayPause(SEMusicItemProperty* itemProperty)
{
    NSLog(@"play\n");
    MPMusicPlayerController* player = [MPMusicPlayerController applicationMusicPlayer];
    MPMusicPlaybackState playbackState = player.playbackState;
    if(playbackState == MPMusicPlaybackStatePaused)
    {
        [player play];
        return;
    }
    else if(playbackState == MPMusicPlaybackStatePlaying)
    {
        [player pause];
        return;
    }
    else if(playbackState == MPMusicPlaybackStateStopped)
    {
        MPMediaItem* item = itemProperty.song;
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
    }
}
static void handlePlayerStop()
{
    MPMusicPlayerController* player = [MPMusicPlayerController applicationMusicPlayer];
    //MPMusicPlaybackState playbackState = player.playbackState;
    [player stop];
}
static UILabel* getTitleLabel(UITableViewCell* cell)
{
    return (UILabel*)[cell viewWithTag:102];
}
static UILabel* getArtistAlbumLabel(UITableViewCell* cell)
{
    return (UILabel*)[cell viewWithTag:103];
}
/////////////////
@implementation SEMusicItemProperty
@synthesize title;
@synthesize artist;
@synthesize album;
@synthesize seq;
@synthesize song;

- (void) dealloc
{
    [song release];
    [title release];
    [artist release];
    [album release];
    [seq release];
    [super dealloc];
}

@end
//////
@interface SEMusicPickerView (Private)
- (void) touchBeginHandler: (UITableViewCell*) pressedView;
- (void) touchMoveHandler: (NSValue*) vDeltap;
- (void) touchEndHandler;
- (void) initMusicArray;
- (void) loadMusicLib;
- (void) readMusicLibThreadFunc;
- (void) addMusicItemToArray: (NSArray*)musicItemArray;
- (void) handleTitleSortButton: (id) sender;
- (void) handleArtistSortButton: (id) sender;
- (void) handleAlbumSortButton: (id) sender;
- (void) handlePlayPauseClick: (UITapGestureRecognizer*) tap;
- (void) handleStopClick: (UITapGestureRecognizer*)tap;
@end
@implementation SEMusicPickerView (Private)
- (void) handlePlayPauseClick: (UITapGestureRecognizer*) tap
{
    UIView* v = tap.view;
    int index = v.tag;
    NSLog(@"play index = %d\n", index);
    SEMusicItemProperty* itemProperty = (SEMusicItemProperty*)[mMusicItemArray objectAtIndex:index];
    handlePlayerPlayPause(itemProperty);
}
- (void) handleStopClick: (UITapGestureRecognizer*)tap
{
    handlePlayerStop();
}
//this function execute on main thread
- (void) addMusicItemToArray: (NSArray*)musicItemArray
{
    for(SEMusicItemProperty* item in musicItemArray)
    {
        [mMusicItemArray addObject:item];
    }
    NSLog(@"## music item array count = %d ##", mMusicItemArray.count);
    [musicItemArray release];
    [mMusicPicker reloadData];
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
            itemProperty.song = item;
            musicItemPropertyArray = [musicItemPropertyArray arrayByAddingObject:itemProperty];
            [item release];
            count++;
            if(count > 60)
            {
                [musicItemPropertyArray retain];
                [self performSelectorOnMainThread:@selector(addMusicItemToArray:) withObject:musicItemPropertyArray waitUntilDone: NO];
                musicItemPropertyArray = [NSArray array];
                count = 0;
            }
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
    [mMusicPicker reloadData];
}

- (void) touchBeginHandler: (UITableViewCell*) pressedView
{
    CGRect frame = [pressedView convertRect:pressedView.bounds toView:self];
    UITableViewCell* newv = (UITableViewCell*)[[[NSBundle mainBundle] loadNibNamed:@"MusicPickerTableViewCell" owner:self options:nil] lastObject];
    newv.backgroundColor = [UIColor redColor];
    newv.frame = frame;
    [mViewNav.mRootView addSubview:newv];
    mSelectedTableViewCell = newv;
    [mMusicPicker disableAllGestures];
    int index = pressedView.tag;
    SEMusicItemProperty* item = (SEMusicItemProperty*)[mMusicItemArray objectAtIndex: index];
    UILabel* titleLabel = getTitleLabel(newv);
    titleLabel.text = item.title;
    if(mSortType == SESORT_BYTITLE || mSortType == SESORT_BYARTIST)
    {
        UILabel* artist = getArtistAlbumLabel(newv);
        artist.text = item.artist;
    }
    else
    {
        UILabel* album = getArtistAlbumLabel(newv);
        if(item.album == nil)
        {
            album.text = @"unknown";
        }
        else
            album.text = item.album;
    }
    mCurrentMusicItemIndex = index;
    NSLog(@"selected music center = %f, %f\n", newv.center.x, newv.center.y);
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
        //NSLog(@"selected music center = %f, %f\n", c.x, c.y);
    }
}
- (void) touchEndHandler
{
    if(mSelectedTableViewCell)
    {
        [mMusicPicker enableAllGestures];
        CGPoint center = mSelectedTableViewCell.center;
        //NSLog(@"selected music center = %f, %f\n", center.x, center.y);
        CGRect selectMusicFrame = mSelectedMusicView.frame;
        if([SEUtil isRectContain: selectMusicFrame point: center])
        {
            SEMusicItemProperty* item = [mMusicItemArray objectAtIndex:mCurrentMusicItemIndex];
            UserInfo* userInfo = [mViewNav getUserInfo];
            NSString* musicListName = userInfo.currentmusiclist;
            [mViewNav addSelectedMusicTitle:item.title aritist:item.artist album:item.album toMusicList:musicListName];
            [mSelectedMusicView addNewMusic:item];
            [mViewNav saveContext];
        }
    }
    [mSelectedTableViewCell removeFromSuperview];
    mSelectedTableViewCell = nil;
}
- (void) loadMusicLib
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
    [mMusicItemArray release];
    mMusicItemArray = nil;
    if(mMusicItemCount > 0)
    {
        mMusicItemArray = [NSMutableArray arrayWithCapacity:mMusicItemCount];
        [mMusicItemArray retain];
        [self performSelectorInBackground:@selector(readMusicLibThreadFunc) withObject:nil];
    }
}
- (void) handleTitleSortButton: (id) sender
{
    handleTitleSort(&mSortType, mMusicItemArray, mMusicPicker);
}
- (void) handleArtistSortButton: (id) sender
{
    handleArtistSort(&mSortType, mMusicItemArray, mMusicPicker);
}
- (void) handleAlbumSortButton: (id) sender
{
    handleAlbumSort(&mSortType, mMusicItemArray, mMusicPicker);
}

@end
//////
@implementation SEMusicPickerView
@synthesize mViewNav;
@synthesize musicItemArray = mMusicItemArray;
@synthesize mSelectedMusicView;
- (void) setTouchHandler
{
    [mMusicPicker setTouchBeganTarget:self withAction:@selector(touchBeginHandler:)];
    [mMusicPicker setTouchMoveTarget:self withAction:@selector(touchMoveHandler:)];
    [mMusicPicker setTouchEndTarget:self withAction:@selector(touchEndHandler)];
}
- (BOOL)canAdjust
{
    return YES;
}
- (void) handleWhenStop: (BOOL) bStopInMid
{
    if(bStopInMid)
    {
        float contentWidth = (mViewNav.mViewPortWidth - mViewNav.mBarWidth) / 2;
        CGRect rect = CGRectMake(self.frame.size.width - contentWidth, mMusicPicker.frame.origin.y, contentWidth, mMusicPicker.frame.size.height);
        mMusicPicker.frame = rect;
        mTableCellType = TABLE_CELL_SMALL;
    }
    else {
        mTableCellType = TABLE_CELL_BIG;
        CGRect rect = mOrigRect;
        mMusicPicker.frame = rect;
        
    }
    [mMusicPicker reloadData];
}
- (void) initBackground
{
    CGRect pickerFrame = mMusicPicker.frame;
    UIImage* image = [mViewNav.mResLoader getImage:@"MusicPickerBackground"];
    UIGraphicsBeginImageContext(pickerFrame.size);
    [image drawAtPoint:CGPointMake(-pickerFrame.origin.x, -pickerFrame.origin.y)];
    mMusicPickerBackgroundImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
}
- (void) setButtonBackground: (UIButton*) button
{
    [button setBackgroundImage:[mViewNav.mResLoader getImage:@"MusicPickerViewButtonBackgroundNormal"] forState:UIControlStateNormal];
    [button setBackgroundImage:[mViewNav.mResLoader getImage:@"MusicPickerViewButtonBackgroundHighlight"] forState:UIControlStateHighlighted];
}
- (void) initButtonHandler
{
    UIButton* titleButton = (UIButton*)[self viewWithTag:102];
    UIButton* artistButton = (UIButton*)[self viewWithTag:103];
    UIButton* albumButton = (UIButton*)[self viewWithTag:104];
    [self setButtonBackground:titleButton];
    [self setButtonBackground:artistButton];
    [self setButtonBackground:albumButton];
    [titleButton addTarget:self  action:@selector(handleTitleSortButton:) forControlEvents:UIControlEventTouchUpInside];
    [artistButton addTarget:self action:@selector(handleArtistSortButton:) forControlEvents:UIControlEventTouchUpInside];
    [albumButton addTarget:self  action:@selector(handleAlbumSortButton:) forControlEvents:UIControlEventTouchUpInside];
}
- (void) initData
{
    mMusicPicker = (UITableView*)[self viewWithTag:101];
    [self initBackground];
    //[mMusicPicker initData];
    mMusicPicker.multipleTouchEnabled = NO;
    UIImageView* musicPickerImageView = [[UIImageView alloc] init];
    musicPickerImageView.image = mMusicPickerBackgroundImage;
    mMusicPicker.backgroundView = musicPickerImageView;
    [musicPickerImageView release];    
    mMusicPicker.delegate = self;
    mMusicPicker.dataSource = self;
    mMusicPicker.separatorStyle = UITableViewCellSeparatorStyleNone;
    [self initButtonHandler];
    //[self setTouchHandler];
    if(SEDB)
    {
        [self initMusicArray];
    }
    else
        [self loadMusicLib];
    mSortType = SESORT_BYTITLE;
    mOrigRect = mMusicPicker.frame;
}

- (UITableViewCell*) createCell:(UITableView *)tableView
{
    static NSString *CellIdentifier = @"Cell";
    if(mTableCellType == TABLE_CELL_SMALL)
    {
        CellIdentifier = @"Cell_Small";
    }
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier];
    if (cell == nil) {
        if(mTableCellType == TABLE_CELL_BIG)
        {
            cell = [[[NSBundle mainBundle] loadNibNamed:@"MusicPickerTableViewCellBig" owner:self options:nil] lastObject]; 
        }
        else 
        {
            cell = [[[NSBundle mainBundle] loadNibNamed:@"MusicPickerTableViewCell" owner:self options:nil] lastObject];
        }
        cell.userInteractionEnabled = YES;
        cell.multipleTouchEnabled = YES;
        
        for(UIView* subv in cell.subviews)
        {
            subv.multipleTouchEnabled = YES;
            subv.userInteractionEnabled = YES;
        }
        
    }
    return cell;
}

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    return 1;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    return mMusicItemArray.count;
}
- (void) handleLongPress: (UILongPressGestureRecognizer*)longGes
{
    NSLog(@"## my long press %d ###", longGes.state);
    UIGestureRecognizerState state = longGes.state;
    CGPoint loc = [longGes locationInView:mViewNav.mRootView];
    UITableViewCell* cell = (UITableViewCell*)longGes.view;
    if(state == UIGestureRecognizerStateBegan)
    {
        //UIView* view = [[UIView alloc] initWithFrame:CGRectMake(loc.x - 100, loc.y - 100, 100, 100)];
        UIView* view = [[[NSBundle mainBundle] loadNibNamed:@"MusicFloatView" owner:self options:nil] lastObject];
        view.frame = CGRectMake(loc.x, loc.y, view.frame.size.width, view.frame.size.height);
        SEMusicItemProperty* item = [mMusicItemArray objectAtIndex:cell.tag];
        UILabel* title1 = (UILabel*)[view viewWithTag:102];
        UILabel* title2 = (UILabel*)[view viewWithTag:103];
        title1.text = item.title;
        title2.text = item.artist;
        mViewNav.mMusicFloatView = view;
        [mViewNav.mRootView addSubview:view];
        view.backgroundColor = [UIColor redColor];
        mSelectedMusciItem = item;
    }
    else if(state == UIGestureRecognizerStateChanged)
    {
        mViewNav.mMusicFloatView.center = loc;
        [mViewNav determineSelectedMusicRow];
    }
    else if(state == UIGestureRecognizerStateEnded)
    {
        [mViewNav addMusicToSelectedView: mSelectedMusciItem];
        [mViewNav.mMusicFloatView removeFromSuperview];
        mViewNav.mMusicFloatView = nil;
        mSelectedMusciItem = nil;
    }
}
// Customize the appearance of table view cells.
- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    UITableViewCell* cell = [self createCell:tableView];
    cell.tag = indexPath.row;
    SEMusicItemProperty* musicItemProperty = [mMusicItemArray objectAtIndex:indexPath.row];
    UIImageView* background = (UIImageView*)[cell viewWithTag:101];
    if(mMusicCellBackgroundImage == nil)
    {
        mMusicCellBackgroundImage = [mViewNav.mResLoader getImage:@"MusicPickerItemBackground"];
    }
    background.image = mMusicCellBackgroundImage;
    background.contentMode = UIViewContentModeScaleToFill;
    UILabel* title1 = (UILabel*)[cell viewWithTag:102];
    UILabel* title2 = (UILabel*)[cell viewWithTag:103];
    title1.textColor = [UIColor whiteColor];
    title2.textColor = [UIColor whiteColor];
    UIImageView* play = (UIImageView*)[cell viewWithTag:104];
    play.tag = indexPath.row;
    UIImageView* stop = (UIImageView*)[cell viewWithTag:105];
    stop.tag = indexPath.row;
    title1.text = musicItemProperty.title;
    if(mSortType == SESORT_BYTITLE || mSortType == SESORT_BYARTIST)
    {
        title2.text = musicItemProperty.artist;
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
    play.image = [mViewNav.mResLoader getImage:@"MusicPickerMusicPlayIcon"];
    stop.image = [mViewNav.mResLoader getImage:@"MusicPickerMusicStopIcon"];
    stop.userInteractionEnabled = YES;
    UITapGestureRecognizer* ges = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(handlePlayPauseClick:)];
    [play addGestureRecognizer:ges];
    [ges release];
    
    ges = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(handleStopClick:)];
    [stop addGestureRecognizer:ges];
    [ges release];
    //cell longpress
    UILongPressGestureRecognizer* longPress = [[UILongPressGestureRecognizer alloc] initWithTarget:self action:@selector(handleLongPress:)];
    [cell addGestureRecognizer:longPress];
    [longPress release];
    //end
    return cell;
}
- (CGFloat)tableView:(UITableView *)tableView heightForRowAtIndexPath:(NSIndexPath *)indexPath
{
    UITableViewCell* cell = [self createCell:tableView];
    return cell.frame.size.height;
}

- (void) dealloc
{
    [mMusicCellBackgroundImage release];
    [mMusicItemArray release];
    [super dealloc];
}
@end
//////
@implementation SESelectedMusicView
@synthesize musicItemArray = mSelectedMusicItemArray;
@synthesize mViewNav;
- (BOOL) canAdjust
{
    return YES;
}
- (void)handleWhenStop: (BOOL) bStopInMid
{
    if(bStopInMid)
    {
        float contentWidth = (mViewNav.mViewPortWidth - mViewNav.mBarWidth) / 2;
        CGRect rect = CGRectMake(0, mSelectedTableView.frame.origin.y, contentWidth, mSelectedTableView.frame.size.height);
        mSelectedTableView.frame = rect;
        mTableCellType = TABLE_CELL_SMALL;
    }
    else {
        mTableCellType = TABLE_CELL_BIG;
        CGRect rect = mOrigRect;
        mSelectedTableView.frame = rect;
        
    }
    [mSelectedTableView reloadData];    
}
- (void) handleTitleSortButton: (id) sender
{
    handleTitleSort(&mSortType, mSelectedMusicItemArray, mSelectedTableView);
}
- (void) handleArtistSortButton: (id) sender
{
    handleArtistSort(&mSortType, mSelectedMusicItemArray, mSelectedTableView);
}
- (void) handleAlbumSortButton: (id) sender
{
    handleAlbumSort(&mSortType, mSelectedMusicItemArray, mSelectedTableView);
}

- (void)loadSelectedMusic
{
    NSArray* musicItemPropertyArray = [mViewNav getCurrentSelectedMusicArray];
    musicItemPropertyArray = [musicItemPropertyArray sortedArrayUsingComparator:^NSComparisonResult(id obj1, id obj2) {
        SEMusicItemProperty* item1 = (SEMusicItemProperty*)obj1;
        SEMusicItemProperty* item2 = (SEMusicItemProperty*)obj2;
        return [item1.seq compare:item2.seq];
    }];
    [mSelectedMusicItemArray removeAllObjects];
    for(SEMusicItemProperty* item in musicItemPropertyArray)
    {
        [mSelectedMusicItemArray addObject:item];
    }
}
- (void) addNewMusic: (SEMusicItemProperty*)item
{
    if(mHighlightedRow == -1)
    {
        return;
    }
    if(mHighlightedRow == -2)
    {
        [mSelectedMusicItemArray addObject:item];
    }
    else 
    {
        [mSelectedMusicItemArray insertObject:item atIndex:mHighlightedRow];
    }
    [mSelectedTableView reloadData];
    [mViewNav setSelectedMusicToCoreData:mSelectedMusicItemArray];
}
- (void) initBackground
{
    CGRect selectedMusicFrame = mSelectedTableView.frame;
    UIImage* image = [mViewNav.mResLoader getImage:@"SelectedMusicViewBackground"];
    UIGraphicsBeginImageContext(selectedMusicFrame.size);
    [image drawAtPoint:CGPointMake(-selectedMusicFrame.origin.x, -selectedMusicFrame.origin.y)];
    mSelectedMusicViewBackgroundImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
}
- (void) setButtonBackground: (UIButton*) button
{
    [button setBackgroundImage:[mViewNav.mResLoader getImage:@"MusicSelectedViewButtonBackgroundNormal"] forState:UIControlStateNormal];
    [button setBackgroundImage:[mViewNav.mResLoader getImage:@"MusicSelectedViewButtonBackgroundHighlight"] forState:UIControlStateHighlighted];
}
- (void) initButtonHandler
{
    UIButton* titleButton = (UIButton*)[self viewWithTag:102];
    UIButton* artistButton = (UIButton*)[self viewWithTag:103];
    UIButton* albumButton = (UIButton*)[self viewWithTag:104];
    [self setButtonBackground:titleButton];
    [self setButtonBackground:artistButton];
    [self setButtonBackground:albumButton];
    [titleButton addTarget:self  action:@selector(handleTitleSortButton:) forControlEvents:UIControlEventTouchUpInside];
    
    [artistButton addTarget:self action:@selector(handleArtistSortButton:) forControlEvents:UIControlEventTouchUpInside];
    
    [albumButton addTarget:self  action:@selector(handleAlbumSortButton:) forControlEvents:UIControlEventTouchUpInside];
}
- (void) initData
{
    mSelectedMusicItemArray = [NSMutableArray array];
    [mSelectedMusicItemArray retain];
    mSelectedTableView = (UITableView*)[self viewWithTag:101];
    [self initBackground];
    //[mSelectedTableView initData];
    UIImageView* selectedImageView = [[UIImageView alloc] init];
    selectedImageView.image = mSelectedMusicViewBackgroundImage;
    mSelectedTableView.backgroundView = selectedImageView;
    [selectedImageView release];
    mSelectedTableView.delegate = self;
    mSelectedTableView.dataSource = self;
    mSelectedTableView.separatorStyle = UITableViewCellSeparatorStyleNone;
    [self initButtonHandler];
    [self loadSelectedMusic];
    mSortType = SESORT_BYTITLE;
    mHighlightedRow = -1;
    mOrigRect = mSelectedTableView.frame;
}
- (UITableViewCell*) createCell:(UITableView *)tableView
{
    static NSString *CellIdentifier = @"SelectCell";
    if(mTableCellType == TABLE_CELL_SMALL)
    {
        CellIdentifier = @"SelectCell_Small";
    }
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier];
    if (cell == nil) {
        if(mTableCellType == TABLE_CELL_BIG)
        {
            cell = [[[NSBundle mainBundle] loadNibNamed:@"MusicPickerTableViewCellBig" owner:self options:nil] lastObject]; 
        }
        else 
        {
            cell = [[[NSBundle mainBundle] loadNibNamed:@"MusicPickerTableViewCell" owner:self options:nil] lastObject];
        }
        cell.userInteractionEnabled = YES;
        //cell.multipleTouchEnabled = YES;
        
        for(UIView* subv in cell.subviews)
        {
            //subv.multipleTouchEnabled = YES;
            subv.userInteractionEnabled = YES;
        }
        
    }
    return cell;
}

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    return 1;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    return [mSelectedMusicItemArray count];
}

// Customize the appearance of table view cells.
- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    UITableViewCell* cell = [self createCell:tableView];
    SEMusicItemProperty* musicItemProperty = [mSelectedMusicItemArray objectAtIndex:indexPath.row];
    UIImageView* background = (UIImageView*)[cell viewWithTag:101];
    if(mSelectedCellBackgroundImage == nil)
    {
        mSelectedCellBackgroundImage = [mViewNav.mResLoader getImage:@"SelectedMusicViewItemBackground"];
    }
    background.image = mSelectedCellBackgroundImage;
    UILabel* title1 = (UILabel*)[cell viewWithTag:102];
    UILabel* title2 = (UILabel*)[cell viewWithTag:103];
    title1.text = musicItemProperty.title;
    if(mSortType == SESORT_BYTITLE || mSortType == SESORT_BYARTIST)
    {
        title2.text = musicItemProperty.artist;
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
    return cell;
}
- (CGFloat)tableView:(UITableView *)tableView heightForRowAtIndexPath:(NSIndexPath *)indexPath
{
    UITableViewCell* cell = [self createCell:tableView];
    return cell.frame.size.height;
}

- (void) dealloc
{
    [mSelectedCellBackgroundImage release];
    [mSelectedMusicItemArray release];
    [super dealloc];
}
- (void) determineSelectedMusicRow: (UIView*)floatView;
{
    CGPoint selectedViewPoint = [mViewNav.mRootView convertPoint:floatView.center toView:mSelectedTableView];
    BOOL inSelectedView = NO;
    NSLog(@"## in selected table view: %f, %f", selectedViewPoint.x, selectedViewPoint.y);
    if(selectedViewPoint.x >= 0 && selectedViewPoint.x < (self.frame.size.width) &&
       selectedViewPoint.y >= 0 && selectedViewPoint.y < (self.frame.size.height))
    {
        inSelectedView = YES;
    }
    if(inSelectedView == NO)
    {
        mHighlightedRow = -1;
        NSArray* subviews = [mSelectedTableView visibleCells];
        for(UIView* v in subviews)
        {
            UITableViewCell* cell = (UITableViewCell*)v;
            UIImageView* imageView = (UIImageView*)[cell viewWithTag:101];
            imageView.image = [mViewNav.mResLoader getImage:@"SelectedMusicViewItemBackground"];
        }
        return;
    }
    NSArray* subviews = [mSelectedTableView visibleCells];
    NSLog(@"## subviews count = %d ##", subviews.count);
    BOOL found = NO;
    for(UIView* v in subviews)
    {
        CGPoint p = [mViewNav.mRootView convertPoint:floatView.center toView:v];
        UITableViewCell* cell = (UITableViewCell*)v;
        UIImageView* imageView = (UIImageView*)[cell viewWithTag:101];
        imageView.image = [mViewNav.mResLoader getImage:@"SelectedMusicViewItemBackground"];
        if(p.x >= 0 && p.x < (v.frame.size.width) 
           && p.y >= 0 && p.y < (v.frame.size.height))
        {
            found = YES;
            UIImage* image = [mViewNav.mResLoader getImage:@"MusicSelectedViewCellHighlighted"];
            imageView.image = image;
            mHighlightedRow = cell.tag;
        }
    }
    if(found == NO)
    {
        mHighlightedRow = -2;
    }
}
@end
////