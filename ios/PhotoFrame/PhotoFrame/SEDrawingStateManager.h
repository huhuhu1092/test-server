//
//  SEDrawingStateManager.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-11-26.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
@protocol SEDrawingImageProtocol
- (void) pressPlaypauseButton;
- (void) moveToOtherView: (int)toViewType;
- (void) moveFromOtherViewBack: (int)fromViewType;
- (void) computeEnd;
- (void) drawFinished;
- (NSString*) getCurrentStateName;
@end
@class SEDrawingState;
@interface SEDrawingStateManager : NSObject <SEDrawingImageProtocol>
{
    enum CURRENT_PLAY_STATE {NO_PLAY_STATE, PLAY_STATE, PAUSE_STATE};
    enum CURRENT_PLAY_STATE mCurrentPlayState;
    BOOL mComputeEnd;
    SEDrawingState* mCurrentState;
    id mActionTarget;
    SEL mAction;
}
@property (nonatomic, assign) enum CURRENT_PLAY_STATE mCurrentPlayState;
@property (nonatomic, assign) BOOL mComputeEnd;
- (void) changeState: (SEDrawingState*) s;
- (void) setTarget: (id) target action : (SEL) select;
- (void) doEntryActionFrom : (int) fromState toState: (int) toState;
//- (BOOL) checkProperty: (NSString*)propertyName;
@end
@interface SEDrawingState : NSObject <SEDrawingImageProtocol>
{
    SEDrawingStateManager* mStateManager;
}
@property (nonatomic, assign) SEDrawingStateManager* mStateManager;
@end

@interface SEInitDrawState : SEDrawingState

@end

@interface SEStartDrawState : SEDrawingState

@end
@interface SEPauseDrawState : SEDrawingState

@end

@interface SEStoppingDrawState : SEDrawingState

@end

@interface SEStopDrawState : SEDrawingState

@end

@interface SEStartDrawPendingState : SEDrawingState

@end