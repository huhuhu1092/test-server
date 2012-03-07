//
//  SEProtocalDefine.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-2-16.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
@class SEContentViewContainer;
@protocol SEAdjustContentView <NSObject>

- (BOOL) canAdjust;
- (void) relayout;

@end

@protocol SENextImageDisplay <NSObject>

- (void) displayNextImage;

@end

@protocol SEContainerAnimationHandler <NSObject>

- (void) handleAnimEnd: (BOOL) stopInMid withDirection: (int) direct leftContainer: (SEContentViewContainer*)leftContainer rightContainer: (SEContentViewContainer*)rightContianer;

@end