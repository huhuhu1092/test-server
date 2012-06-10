//
//  UserInfo.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-5-21.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <CoreData/CoreData.h>

@class ImageList, IssueReport, MusicList, Signature;

@interface UserInfo : NSManagedObject

@property (nonatomic, retain) NSDate * applaststartdate;
@property (nonatomic, retain) NSString * currentimagelist;
@property (nonatomic, retain) NSNumber * currentimagequality;
@property (nonatomic, retain) NSNumber * currentimagetimes;
@property (nonatomic, retain) NSString * currentmusiclist;
@property (nonatomic, retain) NSNumber * currentsignature;
@property (nonatomic, retain) NSNumber * currentsignaturesite;
@property (nonatomic, retain) NSNumber * currentsignaturesize;
@property (nonatomic, retain) NSNumber * drawingmedal;
@property (nonatomic, retain) NSNumber * drawingpoint;
@property (nonatomic, retain) NSNumber * exppointnum;
@property (nonatomic, retain) NSNumber * fansmedal;
@property (nonatomic, retain) NSNumber * fanspoint;
@property (nonatomic, retain) NSNumber * finishedcommentnum;
@property (nonatomic, retain) NSNumber * finishedimagenum;
@property (nonatomic, retain) NSNumber * finishedmusicnum;
@property (nonatomic, retain) NSDate * lastfinishoneimagetime;
@property (nonatomic, retain) NSNumber * level;
@property (nonatomic, retain) NSNumber * newpersonmedal;
@property (nonatomic, retain) NSNumber * newpersonpoint;
@property (nonatomic, retain) NSNumber * presentondutymedal;
@property (nonatomic, retain) NSNumber * presentondutypoint;
@property (nonatomic, retain) NSNumber * shareimagenum;
@property (nonatomic, retain) NSNumber * sharemedal;
@property (nonatomic, retain) NSNumber * sharepoint;
@property (nonatomic, retain) NSSet *imagelist;
@property (nonatomic, retain) NSSet *issuereport;
@property (nonatomic, retain) NSSet *musiclist;
@property (nonatomic, retain) NSSet *signaturelist;
@end

@interface UserInfo (CoreDataGeneratedAccessors)

- (void)addImagelistObject:(ImageList *)value;
- (void)removeImagelistObject:(ImageList *)value;
- (void)addImagelist:(NSSet *)values;
- (void)removeImagelist:(NSSet *)values;

- (void)addIssuereportObject:(IssueReport *)value;
- (void)removeIssuereportObject:(IssueReport *)value;
- (void)addIssuereport:(NSSet *)values;
- (void)removeIssuereport:(NSSet *)values;

- (void)addMusiclistObject:(MusicList *)value;
- (void)removeMusiclistObject:(MusicList *)value;
- (void)addMusiclist:(NSSet *)values;
- (void)removeMusiclist:(NSSet *)values;

- (void)addSignaturelistObject:(Signature *)value;
- (void)removeSignaturelistObject:(Signature *)value;
- (void)addSignaturelist:(NSSet *)values;
- (void)removeSignaturelist:(NSSet *)values;

@end
