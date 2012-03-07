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
/////////////////////////////////
@interface SEMusicImageListView (Private)
- (void) addMusicListButtonHandler: (id)sender;
- (void) addImageListButtonHandler: (id)sender;
-(void) removeButtonHandler: (id)sender;
@end
@implementation SEMusicListTableView (Private)

- (void) addMusicListButtonHandler: (id)sender
{}

- (void) addImageListButtonHandler: (id)sender
{}
-(void) removeButtonHandler: (id)sender
{}
@end
///////
@implementation SEIndicatorView
@synthesize mTouchHandler;
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

    //[super touchesCancelled:touches withEvent:event];
}

@end
//////////////////////////////////////////
@implementation SEImageListTableView
@synthesize mViewNav;
@synthesize mImageListPropertyArray;
@synthesize mResLoader;
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
    UILabel* text1 = (UILabel*)[cell viewWithTag:202];
    UILabel* text2 = (UILabel*)[cell viewWithTag:203];
    UIImageView* imageView = (UIImageView*)[cell viewWithTag:201];
    SEIndicatorView* indicator = (SEIndicatorView*)[cell viewWithTag:204];
    UIImage* i = [mResLoader getImage:@"MusicImageListIndicatorImageIcon"];
    indicator.image = i;
    indicator.userInteractionEnabled = YES;
    indicator.mTouchHandler = (SEMusicImageListView*)self.superview;
    UIView* imageViewBg = (UIView*)[cell viewWithTag:205];
    imageViewBg.backgroundColor = [UIColor clearColor];
    int index = indexPath.row;
    SEImageListProperty* lp = [mImageListPropertyArray objectAtIndex:index];
    text1.text = lp.name;
    text2.text = [NSString stringWithFormat:@"%d", lp.imageCount];
    if(lp.firstURLString != nil)
    {
        NSURL* url = [NSURL URLWithString:lp.firstURLString];
        ALAssetsLibrary* assetLib = [[ALAssetsLibrary alloc] init];
        CGImageRef image = [SEUtil getImageFromPhotoLib:url withAssetLib:assetLib];
        [assetLib release];
        UIImage* uiImage = [UIImage imageWithCGImage:image];
        imageView.image = uiImage;//[button setImage:uiImage forState:UIControlStateNormal];
        CGImageRelease(image);
    }
    
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

@implementation SEMusicListTableView
@synthesize mViewNav;
@synthesize mMusicListPropertyArray;
@synthesize mResLoader;
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
        cell = [[[NSBundle mainBundle] loadNibNamed:@"ImageListItem" owner:self options:nil] lastObject];
    }
    UILabel* text1 = (UILabel*)[cell viewWithTag:202];
    UILabel* text2 = (UILabel*)[cell viewWithTag:203];
    UIImageView* imageView = (UIImageView*)[cell viewWithTag:201];
    SEIndicatorView* indicator = (SEIndicatorView*)[cell viewWithTag:204];
    UIImage* i = [mResLoader getImage:@"MusicImageListIndicatorMusicIcon"];
    indicator.image = i;
    indicator.userInteractionEnabled = YES;
    indicator.mTouchHandler = (SEMusicImageListView*)self.superview;
    UIView* imageViewBg = (UIView*)[cell viewWithTag:205];
    imageViewBg.backgroundColor = [UIColor redColor];

    int index = indexPath.row;
    SEMusicListProperty* lp = [mMusicListPropertyArray objectAtIndex:index];
    text1.text = lp.name;
    text2.text = [NSString stringWithFormat:@"%d", lp.musicCount];
    if(lp.firstURLString != nil)
    {
        NSURL* url = [NSURL URLWithString:lp.firstURLString];
        ALAssetsLibrary* assetLib = [[ALAssetsLibrary alloc] init];
        CGImageRef image = [SEUtil getImageFromPhotoLib:url withAssetLib:assetLib];
        [assetLib release];
        UIImage* uiImage = [UIImage imageWithCGImage:image];
        imageView.image = uiImage;//[button setImage:uiImage forState:UIControlStateNormal];
        CGImageRelease(image);
    }
    CGRect frame = cell.frame;
    NSLog(@"cell frame = %f, %f, %f, %f", frame.origin.x, frame.origin.y, frame.size.width, frame.size.height);
    return cell;

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
@implementation SEMusicImageListView
@synthesize mViewNav;
@synthesize mBackground;
@synthesize mResLoader;
/*
- (id)initWithFrame:(CGRect)frame fromViewNav: (SEViewNavigator*)viewNav withBackground:(UIImage*)image withResLoader:(SEResLoader *)resLoader
{
    self = [super initWithFrame:frame];
    if (self) 
    {
        mResLoader = resLoader;
        self.mBackground = image;
        // Initialization code
        CGRect musicRect = CGRectMake(0, 0, frame.size.width / 2 - 5, frame.size.height);
        CGRect imageRect = CGRectMake(frame.size.width / 2 + 5, 0, frame.size.width, frame.size.height);
        mViewNav = viewNav;
        mImageListTableView = [[SEImageListTableView alloc] initWithFrame:imageRect];
        mImageListTableView.mResLoader = mResLoader;
        //mImageListTableView.separatorColor = [UIColor clearColor];
        UIImageView* imageView = [[UIImageView alloc] init];
        imageView.image = mBackground;
        mImageListTableView.backgroundView = imageView;
        [imageView release];
        //mImageListTableView.backgroundView = nil;
        mImageListTableView.dataSource = mImageListTableView;
        mImageListTableView.delegate = mImageListTableView;
        mImageListTableView.mViewNav = viewNav;
        mImageListTableView.mImageListPropertyArray = [viewNav getImageListProperty];
        mMusicListTableView = [[SEMusicListTableView alloc] initWithFrame:musicRect];
        mMusicListTableView.mResLoader = mResLoader;
        //mMusicListTableView.backgroundView = nil;
        mMusicListTableView.dataSource = mMusicListTableView;
        mMusicListTableView.delegate = mMusicListTableView;
        mMusicListTableView.mViewNav = viewNav;
        mMusicListTableView.mMusicListPropertyArray = [viewNav getMusicListProperty];
        [self addSubview:mImageListTableView];
        [self addSubview:mMusicListTableView];
    }
    return self;
}
 */
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
    mImageListTableView.mImageListPropertyArray = [mViewNav getImageListProperty];

    mMusicListTableView.mResLoader = mResLoader;
    UIImageView* musicView = [[UIImageView alloc] init];
    musicView.image = mMusicListBackground;
    mMusicListTableView.backgroundView = musicView;
    mMusicListTableView.dataSource = mMusicListTableView;
    mMusicListTableView.delegate = mMusicListTableView;
    mMusicListTableView.mViewNav = mViewNav;
    mMusicListTableView.mMusicListPropertyArray = [mViewNav getMusicListProperty];    
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
- (void) touchBegan: (CGPoint) p
{
    mOrig = p;
    mLineView = [[UIView alloc] init];
    mLineView.backgroundColor = [UIColor blueColor];
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
}




@end
