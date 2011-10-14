//
//  TestImageViewViewController.m
//  TestImageView
//
//  Created by 陈勇 on 11-9-27.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import "TestImageViewViewController.h"
#import "SSImageLoader.h"
#import <AssetsLibrary/AssetsLibrary.h>
#import "ParameterSetMainUI.h"
#import "PainterManager.h"
#define zEnter() NSLog(@"enter function: %s", __FUNCTION__);
/// test function
@interface CMTest : NSObject {
@private
    int i;
}
- (void)dealloc;
@end
@implementation CMTest : NSObject

- (void)dealloc
{
    [super dealloc];
    NSLog(@"CMTest be dealloced");
}
@end
@interface MyTextFieldDelegage :NSObject <UITextFieldDelegate>
@end
@implementation MyTextFieldDelegage
- (BOOL)textFieldShouldReturn: (UITextField*) tf 
{
    [tf resignFirstResponder];
    return YES; 
}

@end
///////
@implementation TestImageViewViewController
@synthesize infoNotification;
@synthesize imageView;
- (void)didReceiveMemoryWarning
{
    // Releases the view if it doesn't have a superview.
    [super didReceiveMemoryWarning];
    
    // Release any cached data, images, etc that aren't in use.
}

#pragma mark - View lifecycle

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
    return (interfaceOrientation == UIInterfaceOrientationPortrait);
}
- (void)displayImage:(UIImage*)image
{
    dispatch_queue_t queue= dispatch_get_main_queue();
    dispatch_async(queue , ^(void){
        imageView.image = image;
    });
}
- (void)test
{
    CMTest* test = [[CMTest alloc] init];
    [test autorelease];
}
- (void) loadBrush
{
    zEnter();
    /*
    UIImage* uiImage = [UIImage imageNamed:@"test1.jpg"];
    CGImageRef imageRef = [uiImage CGImage];
    ALAssetsLibrary* library = [[ALAssetsLibrary alloc] init];
    ALAssetsLibraryWriteImageCompletionBlock writeImageBlock = ^(NSURL *assetURL, NSError *error)
    {
        NSLog(@"## write image url = %@", assetURL);
    };
    [library writeImageToSavedPhotosAlbum: imageRef orientation:(ALAssetOrientation)uiImage.imageOrientation completionBlock:writeImageBlock];
    [library release];
     */
    /*
    BOOL isDir = true;
    NSString* pathStr = @"~/TestImageView.app/defaultbrush.pgm";
    pathStr = [pathStr stringByExpandingTildeInPath];
    NSLog(@"pathstr = %@", pathStr);
    NSFileManager* fileManager = [NSFileManager defaultManager];
    NSDirectoryEnumerator* dirEnumerator = [fileManager enumeratorAtPath:NSHomeDirectory()];
    NSString* currPath;
    while(currPath = [dirEnumerator nextObject])
    {
        NSLog(@"Found %@", currPath);
    }
    NSString* filePath = [[NSBundle mainBundle] pathForResource:@"defaultbrush" ofType:@"pgm"];
    NSData* fileData = [NSData dataWithContentsOfFile:filePath];
    if(fileData)
    {
        NSLog(@"file is ok");
        const unsigned char* data = [fileData bytes];
        
    }
    if([fileManager fileExistsAtPath:pathStr isDirectory:&isDir])
    {
        printf("file exists\n");
    }
     */
    /*
    void(^printFrom1To100)(void) = ^{
        //NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
        NSUInteger counter = 0;
        //[self test];
        CMTest* test = [[CMTest alloc] init];
        [test autorelease];
        for(counter = 1 ; counter < 100; counter++)
        {
            NSLog(@"counter = %lu, thread = %@", (unsigned long)counter, [NSThread currentThread]);
        }
        //[pool release];
    };
    NSLog(@"main thread = %@", [NSThread mainThread]);
    dispatch_queue_t queue = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0);
    dispatch_async(queue, printFrom1To100);
    dispatch_async(queue, printFrom1To100);
    */
    
}
- (void)loadView
{
    [super loadView];
    [self loadBrush];
    imageLoader = [[SSImageLoader alloc] init];
    imageLoader.setImageCallback = self;
    painterManager = [[PainterManager alloc] init];
    
    //[imageLoader createFromPhotoLib:nil];
    //[imageLoader createFromFileName:@"test1.jpg"];
}
- (void)dealloc
{
    [imageLoader release];
    [infoNotification release];
    [secondViewOwner release];
    [painterManager release];
    [scrollView release];
    [super dealloc];
}
- (void)setMainUIText
{
    secondViewOwner.paper_scale.text = NSLocalizedStringFromTable(@"paper_scale_text", @"ParamType", nil);
    secondViewOwner.paper_relief.text = NSLocalizedStringFromTable(@"paper_relief_text", @"ParamType", nil);
    secondViewOwner.brush_relief.text = NSLocalizedStringFromTable(@"brush_relief_text", @"ParamType", nil);
    //secondViewOwner.orient_type.titleLabel.text = NSLocalizedStringFromTable(@"orient_type_text", @"ParamType", nil);
    [secondViewOwner.orient_type setTitle:NSLocalizedStringFromTable(@"orient_type_text", @"ParamType", nil) forState:UIControlStateNormal];
    secondViewOwner.orient_num.text = NSLocalizedStringFromTable(@"orient_num_text", @"ParamType", nil);
    secondViewOwner.orient_first.text = NSLocalizedStringFromTable(@"orient_first_text", @"ParamType", nil);
    secondViewOwner.orient_last.text = NSLocalizedStringFromTable(@"orient_last_text", @"ParamType", nil);
    //secondViewOwner.size_type.titleLabel.text = NSLocalizedStringFromTable(@"size_type_text", @"ParamType", nil);
    [secondViewOwner.size_type setTitle:NSLocalizedStringFromTable(@"size_type_text", @"ParamType", nil) forState:UIControlStateNormal];
    secondViewOwner.size_num.text = NSLocalizedStringFromTable(@"size_num_text", @"ParamType", nil);
    secondViewOwner.size_first.text = NSLocalizedStringFromTable(@"size_first_text", @"ParamType", nil);
    secondViewOwner.size_last.text = NSLocalizedStringFromTable(@"size_last_text", @"ParamType", nil);
    //secondViewOwner.place_type.titleLabel.text = NSLocalizedStringFromTable(@"place_type_text", @"ParamType", nil);
    [secondViewOwner.place_type setTitle: NSLocalizedStringFromTable(@"place_type_text", @"ParamType", nil) forState:UIControlStateNormal];
    secondViewOwner.brush_density.text = NSLocalizedStringFromTable(@"brush_density_text", @"ParamType", nil);
    //secondViewOwner.color_type.titleLabel.text = NSLocalizedStringFromTable(@"color_type_text", @"ParamType", nil);
    [secondViewOwner.color_type setTitle: NSLocalizedStringFromTable(@"color_type_text", @"ParamType", nil) forState:UIControlStateNormal];
    //secondViewOwner.bg_type.titleLabel.text = NSLocalizedStringFromTable(@"bg_type_text", @"ParamType", nil);
    [secondViewOwner.bg_type setTitle:NSLocalizedStringFromTable(@"bg_type_text", @"ParamType", nil) forState:UIControlStateNormal];
    secondViewOwner.drawing_speed.text = NSLocalizedStringFromTable(@"drawing_speed_text", @"ParamType", nil);
    secondViewOwner.wait_time.text = NSLocalizedStringFromTable(@"wait_time_text",@"ParamType",nil);


}
- (IBAction)setButtonPressed:(id)sender
{
    NSLog(@"set button pressed");
    if(!secondViewOwner)
    {
        secondViewOwner = [[ParameterSetMainUI alloc] init];
        [[NSBundle mainBundle] loadNibNamed:@"MyNib" owner:secondViewOwner options:nil];
        secondViewOwner.painterManager = painterManager;
        [secondViewOwner initViewByParam];
        scrollView = [[UIScrollView alloc] initWithFrame:[[UIScreen mainScreen] applicationFrame]];
        scrollView.scrollEnabled = YES;
        NSArray* subviews = [secondViewOwner.topView subviews];
        NSLog(@"## subview num = %u", [subviews count]);
        for(NSUInteger i = 0 ; i < [subviews count] ; i++)
        {
            UIView* v = [subviews objectAtIndex:i];
            [v removeFromSuperview];
            [scrollView addSubview:v];
            Class textFieldClass = [UITextField class];
            if([v isMemberOfClass:textFieldClass])
            {
                MyTextFieldDelegage* md = [[MyTextFieldDelegage alloc] init];
                ((UITextField*)v).delegate = md;
            }
        }
        scrollView.backgroundColor = [UIColor whiteColor];
        scrollView.contentSize = CGSizeMake(1024, 768);
        //[scrollView addSubview:secondViewOwner.topView];
    }
    UIApplication* app = [UIApplication sharedApplication];
    UIWindow* window = [app keyWindow];
    secondViewOwner.currentView = scrollView;
    //UIView* topView= secondViewOwner.topView;
    [self setMainUIText];
    //topView.frame = [[UIScreen mainScreen] applicationFrame];
    [window addSubview:scrollView];
}
@end
