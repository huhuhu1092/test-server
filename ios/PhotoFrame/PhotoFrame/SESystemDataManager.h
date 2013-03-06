//
//  SESystemDataManager.h
//  PhotoFrame
//
//  Created by 陈勇 on 13-2-21.
//
//

#import <Foundation/Foundation.h>
@class NSManagedObject;
@interface SESystemDataManager : NSObject
{}
@property (nonatomic, assign) NSNumber * brushdensity;
@property (nonatomic, assign) NSNumber * brushedgedetect;
@property (nonatomic, assign) NSNumber * currentangle;
@property (nonatomic, assign) NSNumber * currentbrushid;
@property (nonatomic, assign) NSNumber * currentbrushtransparent;
@property (nonatomic, assign) NSNumber * currentimagequality;
@property (nonatomic, assign) NSNumber * currentimagetimes;
//@property (nonatomic, assign) NSNumber * drawimagetime;
@property (nonatomic, assign) NSNumber * drawingmedal;
@property (nonatomic, assign) NSNumber * drawingpoint;
@property (nonatomic, assign) NSNumber * exppointnum;
@property (nonatomic, assign) NSNumber * fansmedal;
@property (nonatomic, assign) NSNumber * fanspoint;
@property (nonatomic, assign) NSNumber * finishedcommentnum;
@property (nonatomic, assign) NSNumber * finishedimagenum;
@property (nonatomic, assign) NSNumber * finishedmusicnum;
@property (nonatomic, assign) NSNumber * level;
@property (nonatomic, assign) NSNumber * newpersonmedal;
@property (nonatomic, assign) NSNumber * newpersonpoint;
@property (nonatomic, assign) NSNumber * presentondutymedal;
@property (nonatomic, assign) NSNumber * presentondutypoint;
@property (nonatomic, assign) NSNumber * shareimagenum;
@property (nonatomic, assign) NSNumber * sharemedal;
@property (nonatomic, assign) NSNumber * sharepoint;
@property (nonatomic, assign) NSNumber * timetextstyle;
- (void) initData : (NSManagedObject*) userInfoObject;
@end
