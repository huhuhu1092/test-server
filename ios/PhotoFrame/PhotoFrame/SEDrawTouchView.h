//
//  SEDrawTouchView.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-2-28.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface SEDrawTouchView : UIView
{
    NSMutableArray* pointArrayList;
    NSMutableArray* currentPointArray;
    UIImage* background;
    CGFloat lineWidth;
}
@property (nonatomic, assign) CGFloat lineWidth;
@property (nonatomic, retain) UIImage* background;
- (void) initData;
// get points data's array
// return value is an array about arrays
- (NSMutableArray*) getAllNormalizedPoints;
- (void) setNormalizePoints: (NSMutableArray*)points;
- (void) clearPoints;
@end
