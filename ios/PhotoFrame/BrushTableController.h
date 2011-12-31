//
//  BrushTableController.h
//  PhotoFrame
//
//  Created by 陈勇 on 11-11-2.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
typedef enum
{
    SE_IMAGE_TYPE,
    SE_PPM_TYPE
} SE_SOURCE_TYPE;
@interface BrushTableController : UITableViewController
{
    id appDelegate;
    NSArray* brushes;
    id parent;
    SE_SOURCE_TYPE type;
}
@property (nonatomic, assign) id appDelegate;
@property (nonatomic, retain)NSArray* brushes;
@property (nonatomic, assign) id parent;
@property (nonatomic) SE_SOURCE_TYPE type;
@end
