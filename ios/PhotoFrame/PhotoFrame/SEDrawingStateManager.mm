//
//  SEDrawingStateManager.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-11-26.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEDrawingStateManager.h"
#import "SEViewNavigator.h"
#import "PainterManager.h"
#import "PGMDataReader.h"
#import "PhotoFrameAppDelegate.h"
static NSString* gStateName[] = {@"init", @"startDrawImage", @"stoppingDrawImage", @"stopDrawImage", @"startDrawImagePendding", @"pauseDrawImage"};
@implementation SEDrawingStateManager
@synthesize mComputeEnd;
@synthesize mCurrentPlayState;
- (void) pressPlaypauseButton
{
    [mCurrentState pressPlaypauseButton];
}
- (void) moveToOtherView:(int)toViewType
{
    [mCurrentState moveToOtherView:toViewType];
}
- (void) moveFromOtherViewBack: (int)fromViewType
{
    [mCurrentState moveFromOtherViewBack: fromViewType];
}
- (void) computeEnd
{
    [mCurrentState computeEnd];
}
- (void) changeState:(SEDrawingState *)s
{
    [mCurrentState release];
    mCurrentState = [s retain];
    mCurrentState.mStateManager = self;
}
- (void )setTarget:(id)target action:(SEL)select
{
    mActionTarget = target;
    mAction = select;
}
- (id) init
{
    self = [super init];
    if(self)
    {
        mCurrentState = [[SEInitDrawState alloc] init];
        mCurrentState.mStateManager = self;
    }
    return self;
}
- (void) dealloc
{
    [mCurrentState release];
    [super dealloc];
}
- (void) doEntryActionFrom:(int) fromState toState: (int) toState
{
    NSMutableArray* data = [NSMutableArray array];
    [data addObject:[NSNumber numberWithInt:fromState]];
    [data addObject:[NSNumber numberWithInt:toState]];
    [mActionTarget performSelector:mAction withObject:data];
}
- (NSString*) getCurrentStateName
{
    return [mCurrentState getCurrentStateName];
}
- (void) drawFinished
{
    [mCurrentState drawFinished];
}
/*
- (BOOL) checkProperty: (NSString*)propertyName
{
    int i = -1;
    if([propertyName isEqualToString:@"computeEnd?"])
    {
        i = -1;
    }
    else 
    {
        assert(0);
    }
    //[mActionTarget performSelector:mAction withObject:[NSNumber numberWithInt:i]];
}
 */
@end

@implementation SEDrawingState
@synthesize mStateManager;
- (void) drawFinished
{}
- (void) pressPlaypauseButton
{}

- (void) moveToOtherView: (int)toViewType
{
    
}
- (void) moveFromOtherViewBack: (int)fromViewType
{
    
}
- (void) computeEnd
{
    
}
- (NSString*) getCurrentStateName
{
    return @"(null)";
}

@end

@implementation SEInitDrawState
- (void) printStateName
{
    NSLog(@"state = %@", gStateName[0]);
}
- (void) drawFinished
{}
- (NSString*) getCurrentStateName
{
    return @"initDrawState";
}
- (void) pressPlaypauseButton
{
    PainterManager* pm = [PainterManager painterManager];
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    [mStateManager doEntryActionFrom:INIT_DRAW_IMAGE_STATE toState:START_DRAW_IMAGE];
    mStateManager.mCurrentPlayState = PLAY_STATE;
    SEStartDrawState* s = [[[SEStartDrawState alloc] init] autorelease];
    //s.mStateManager = self.mStateManager;
    [mStateManager changeState:s];
    SS_NoPause((SS_PausePoint*)[pm currentPausePoint]);
    [viewNav displayNextImage];
    [viewNav playMusicByState];
    [viewNav changePlayPauseIcon:NO];
}
- (void) moveFromOtherViewBack: (int)fromViewType
{
    NSLog(@"init state respond to moveFromOtherViewBack");
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    [viewNav resumeMusicByState];
}
- (void) moveToOtherView: (int)toViewType
{
    NSLog(@"init state respond to moveToOtherView");
    if(toViewType == PREVIEW_3D)
    {
        SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
        [viewNav stopMusicByState];
        [viewNav playMusicByState];
    }
}
- (void) computeEnd
{
    NSLog(@"init state respond to computeEnd");
}


@end

@implementation SEStartDrawState
- (NSString*) getCurrentStateName
{
    return @"startDrawState";
}
- (void) drawFinished
{
    PainterManager* pm = [PainterManager painterManager];
    [pm drawFinishAction];
    [pm addLog:@"startDrawState drawFinished"];
}
- (void) pressPlaypauseButton
{
    mStateManager.mCurrentPlayState = PAUSE_STATE;
    [mStateManager doEntryActionFrom: START_DRAW_IMAGE toState:PAUSE_DRAW_IMAGE];
    SEPauseDrawState* s = [[[SEPauseDrawState alloc] init] autorelease];
    [mStateManager changeState:s];
    //SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    PainterManager* pm = [PainterManager painterManager];
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    [pm pauseDrawing];
    [viewNav pauseMusicByState];
    [viewNav changePlayPauseIcon:YES];
    [pm addLog:@"startDrawState pressPlaypauseButton"];
}
- (void) moveFromOtherViewBack:(int)fromViewType
{
    PainterManager* pm = [PainterManager painterManager];
    [pm addLog:@"startDrawState moveFromOtherViewBack"];
}
- (void) moveToOtherView: (int)toViewType
{
    PainterManager* pm = [PainterManager painterManager];
    BOOL computeEnd = [pm isComputeEnd];
    if(computeEnd)
    {
        [mStateManager doEntryActionFrom:START_DRAW_IMAGE toState:STOP_DRAW_IMAGE];
        SEStopDrawState* s = [[[SEStopDrawState alloc] init] autorelease];
        [mStateManager changeState:s];
        [pm releaseResourceForDraw];
        [pm addLog:@"startDrawState moveToOtherView with computeEnd"];
    }
    else
    {
        [mStateManager doEntryActionFrom:START_DRAW_IMAGE toState:STOPPING_DRAWING_IMAGE];
        SEStoppingDrawState* s = [[[SEStoppingDrawState alloc] init] autorelease];
        [mStateManager changeState:s];
        SS_AtomicCounter* statusPoint = (SS_AtomicCounter*)[pm currentStatusPoint];;
        SS_SetAtomicCounterValue(statusPoint, 0);
        SS_NoPause((SS_PausePoint*)[pm currentPausePoint]);
        [pm addLog:@"startDrawState moveToOtherView with computeEnd no"];
    }
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    if(toViewType == PREVIEW_3D)
    {
        [viewNav stopMusicByState];
        [viewNav playMusicByState];
    }
    else
    {
        [viewNav pauseMusicByState];
    }
    //[pm addLog:@"startDrawState moveToOtherView"];
}
- (void) computeEnd
{
    NSLog(@"startDraw state computeEnd");
    PainterManager* pm = [PainterManager painterManager];
    [pm nextDisplayStage];
    [pm addLog:@"startDrawState computeEnd"];
}

@end

@implementation SEStopDrawState
- (void) drawFinished
{}
- (NSString*) getCurrentStateName
{
    return @"stopDrawState";
}
- (void) pressPlaypauseButton
{
    [[PainterManager painterManager] addLog:@"stopDrawState pressPlaypauseButton"];
}
- (void) moveFromOtherViewBack: (int)fromViewType
{
    PainterManager* pm = [PainterManager painterManager];
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    if(mStateManager.mCurrentPlayState == PLAY_STATE)
    {
        [mStateManager doEntryActionFrom:STOP_DRAW_IMAGE toState:START_DRAW_IMAGE];
        SEStartDrawState* s = [[[SEStartDrawState alloc] init] autorelease];
        [mStateManager changeState:s];
        SS_NoPause((SS_PausePoint*)[pm currentPausePoint]);
        [pm setFirstTimeDraw:YES];
        [viewNav displayNextImage];
        [viewNav changePlayPauseIcon:NO];
        [viewNav playMusicByState];
        [pm addLog:@"stopDrawState moveFromOtherViewBack PLAY_STATE"];
    }
    else if(mStateManager.mCurrentPlayState == PAUSE_STATE)
    {
        [mStateManager doEntryActionFrom:STOP_DRAW_IMAGE toState:PAUSE_DRAW_IMAGE];
        SEPauseDrawState* s = [[[SEPauseDrawState alloc] init] autorelease];
        [mStateManager changeState:s];
        [pm setFirstTimeDraw:YES];
        SS_NoPause((SS_PausePoint*)[pm currentPausePoint]);
        [viewNav displayNextImage];
        [viewNav changePlayPauseIcon:YES];
        [pm pauseDrawing];
        [viewNav resumeMusicByState];
        [pm addLog:@"stopDrawState moveFromOtherViewBack PAUSE_STATE"];
    }
    
}
- (void) moveToOtherView: (int) toViewType
{
    [[PainterManager painterManager] addLog:@"stopDrawState moveToOtherView"];
}
- (void) computeEnd
{
    NSLog(@"stopState can not respond to computeEnd");
    assert(0);
}

@end

@implementation SEStoppingDrawState
- (NSString*) getCurrentStateName
{
    return @"stoppingDrawState";
}
- (void) drawFinished
{}
- (void) pressPlaypauseButton
{
    [[PainterManager painterManager] addLog:@"stoppingDrawState pressPlaypauseButton"];
}
- (void) moveFromOtherViewBack: (int)fromViewType
{
    PainterManager* pm = [PainterManager painterManager];
    BOOL computeEnd = [pm isComputeEnd];
    if(computeEnd == NO)
    {
        [mStateManager doEntryActionFrom:STOPPING_DRAWING_IMAGE toState:START_DRAW_IMAGE_PENDING];
        SEStartDrawPendingState* s = [[[SEStartDrawPendingState alloc] init] autorelease];
        [mStateManager changeState:s];
        SS_NoPause((SS_PausePoint*)[pm currentPausePoint]);
    }
    else
    {
        NSLog(@"stoppingState can not get computeEnd true");
        assert(0);    
    }
    [pm addLog:@"stoppingDrawState moveFromOtherViewBack"];
}
- (void) moveToOtherView: (int)toViewType
{
    [[PainterManager painterManager] addLog:@"stoppingDrawState moveToOtherView"];
}
- (void) computeEnd
{
    PainterManager* pm = [PainterManager painterManager];
    [mStateManager doEntryActionFrom:STOPPING_DRAWING_IMAGE toState:STOP_DRAW_IMAGE];
    SEStopDrawState* s = [[[SEStopDrawState alloc] init] autorelease];
    [mStateManager changeState:s];
    [pm releaseResourceForDraw];
    [pm addLog:@"stoppingDrawState computeEnd"];
}

@end

@implementation SEPauseDrawState
- (NSString*) getCurrentStateName
{
    return @"pauseDrawState";
}
- (void) drawFinished
{
    PainterManager* pm = [PainterManager painterManager];
    [pm drawFinishWhenPause];
    [pm pauseDrawing];
}
- (void) pressPlaypauseButton
{
    mStateManager.mCurrentPlayState = PLAY_STATE;
    [mStateManager doEntryActionFrom:PAUSE_DRAW_IMAGE toState:START_DRAW_IMAGE];
    SEStartDrawState* s = [[[SEStartDrawState alloc] init] autorelease];
    [mStateManager changeState:s];
    PainterManager* pm = [PainterManager painterManager];
    //SS_NoPause((SS_PausePoint*)[pm currentPausePoint]);
    [pm startDrawing];
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    [viewNav changePlayPauseIcon:NO];
    [viewNav resumeMusicByState];
    [[PainterManager painterManager] addLog:@"pauseDrawState pressPlaypauseButton"];
}
- (void) moveFromOtherViewBack: (int)fromViewType
{
    NSLog(@"can not respond to moveFromOtherViewBack");
    assert(0);
}
- (void) moveToOtherView: (int) toViewType
{
    PainterManager* pm = [PainterManager painterManager];
    BOOL computeEnd = [pm isComputeEnd];
    if(computeEnd)
    {
        [mStateManager doEntryActionFrom:PAUSE_DRAW_IMAGE toState:STOP_DRAW_IMAGE];
        SEStopDrawState* s = [[[SEStopDrawState alloc] init] autorelease];
        [mStateManager changeState:s];
        [pm addLog:@"pauseDrawState moveToOtherView computeEnd == YES"];
    }
    else
    {
        [mStateManager doEntryActionFrom: PAUSE_DRAW_IMAGE toState:STOPPING_DRAWING_IMAGE];
        SEStoppingDrawState* s = [[[SEStoppingDrawState alloc] init] autorelease];
        [mStateManager changeState:s];
        SS_AtomicCounter* statusPoint = (SS_AtomicCounter*)[pm currentStatusPoint];;
        SS_SetAtomicCounterValue(statusPoint, 0);
        SS_NoPause((SS_PausePoint*)[pm currentPausePoint]);
        [pm addLog:@"pauseDrawState moveToOtherView computeEnd = NO"];
    }
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    if(toViewType == PREVIEW_3D)
    {
        [viewNav stopMusicByState];
        [viewNav playMusicByState];
    }
    
}
- (void) computeEnd
{
    PainterManager* pm = [PainterManager painterManager];
    [pm nextDisplayStage];
    [pm pauseDrawing];
    [pm addLog:@"pauseDrawState computeEnd"];
}

@end
@implementation SEStartDrawPendingState
- (NSString*) getCurrentStateName
{
    return @"startDrawPendingState";
}
- (void) pressPlaypauseButton
{
    PainterManager* pm = [PainterManager painterManager];
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    BOOL computeEnd = [pm isComputeEnd];

    if(computeEnd == NO)
    {
        if(mStateManager.mCurrentPlayState == PLAY_STATE)
        {
            [viewNav pauseMusicByState];
        }
        else if(mStateManager.mCurrentPlayState == PAUSE_STATE)
        {
            [viewNav playMusicByState];
        }
        if(mStateManager.mCurrentPlayState == PLAY_STATE)
        {
            mStateManager.mCurrentPlayState = PAUSE_STATE;
            [viewNav changePlayPauseIcon:NO];
        }
        else if(mStateManager.mCurrentPlayState == PAUSE_STATE)
        {
            mStateManager.mCurrentPlayState = PLAY_STATE;
            [viewNav changePlayPauseIcon:YES];
        }
    }
    [[PainterManager painterManager] addLog:@"startDrawPending pressPlaypauseButton"];
}
- (void) moveFromOtherViewBack: (int) fromViewType
{
    [[PainterManager painterManager] addLog:@"startDrawPending moveFromOtherViewBack"];
}
- (void) moveToOtherView: (int) toViewType
{
    PainterManager* pm = [PainterManager painterManager];
    BOOL computeEnd = [pm isComputeEnd];
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    if(toViewType == PREVIEW_3D)
    {
        [viewNav stopMusicByState];
        [viewNav playMusicByState];
    }
    else
    {
        [viewNav pauseMusicByState];
    }
    if(computeEnd)
    {
        [mStateManager doEntryActionFrom: START_DRAW_IMAGE_PENDING toState:STOP_DRAW_IMAGE];
        SEStopDrawState* s = [[[SEStopDrawState alloc] init] autorelease];
        [mStateManager changeState:s];
        [pm releaseResourceForDraw];
        [pm addLog:@"startDrawPending moveToOtherView computeEnd == YES"];
    }
    else
    {
        [mStateManager doEntryActionFrom: START_DRAW_IMAGE_PENDING toState:STOPPING_DRAWING_IMAGE];
        SEStoppingDrawState* s = [[[SEStoppingDrawState alloc] init] autorelease];
        [mStateManager changeState:s];
        
        SS_AtomicCounter* statusPoint = (SS_AtomicCounter*)[pm currentStatusPoint];;
        SS_SetAtomicCounterValue(statusPoint, 0);
        SS_NoPause((SS_PausePoint*)[pm currentPausePoint]);
        [pm addLog:@"startDrawPending moveToOtherView computeEnd == NO"];
    }
    
}
- (void) computeEnd
{
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    PainterManager* pm = [PainterManager painterManager];
    if(mStateManager.mCurrentPlayState == PLAY_STATE)
    {
        [mStateManager doEntryActionFrom: START_DRAW_IMAGE_PENDING toState:START_DRAW_IMAGE];
        SEStartDrawState* s = [[[SEStartDrawState alloc] init] autorelease];
        [mStateManager changeState:s];
        [pm releaseResourceForDraw];
        [pm setFirstTimeDraw:YES];
        [viewNav displayNextImage];
        [viewNav changePlayPauseIcon:NO];
        [pm addLog:@"startDrawPending computeEnd PLAY_STATE"];
    }
    else if(mStateManager.mCurrentPlayState == PAUSE_STATE)
    {
        [mStateManager doEntryActionFrom: START_DRAW_IMAGE_PENDING toState:PAUSE_DRAW_IMAGE];
        SEPauseDrawState* s = [[[SEPauseDrawState alloc] init] autorelease];
        [mStateManager changeState: s];
        [pm releaseResourceForDraw];
        [pm setFirstTimeDraw:YES];
        [viewNav displayNextImage];
        [pm pauseDrawing];
        [viewNav changePlayPauseIcon:YES];
        [pm addLog:@"startDrawPending computeEnd PAUSE_STATE"];
    }
    
}
- (void) drawFinished
{}
@end