//
//  SEDrawCircleView.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-11-29.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface SEDrawCircleView : UIView
{
    NSMutableArray* mCirclePoints;
    NSMutableArray* mEllipsePoints;
    NSMutableArray* mDeltaPoints;
    NSMutableArray* mResultPoints;
    NSTimer* mUpdateTimer;
    float mCurrentTime;
}
@end
