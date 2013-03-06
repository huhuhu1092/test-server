//
//  UserInfo.h
//  PhotoFrame
//
//  Created by 陈勇 on 13-2-21.
//
//

#import <Foundation/Foundation.h>
#import <CoreData/CoreData.h>

@class ImageList, IssueReport, MusicList, Signature, UpgradeInfo;

@interface UserInfo : NSManagedObject

@property (nonatomic, retain) NSDate * applaststartdate;
@property (nonatomic, retain) NSNumber * autofilterimage;
@property (nonatomic, retain) NSData * brushdensity;
@property (nonatomic, retain) NSData * brushedgedetect;
@property (nonatomic, retain) NSData * currentangle;
@property (nonatomic, retain) NSData * currentbrushid;
@property (nonatomic, retain) NSData * currentbrushtransparent;
@property (nonatomic, retain) NSString * currentimagelist;
@property (nonatomic, retain) NSData * currentimagequality;
@property (nonatomic, retain) NSData * currentimagetimes;
@property (nonatomic, retain) NSString * currentmusiclist;
@property (nonatomic, retain) NSNumber * currentsignature;
@property (nonatomic, retain) NSNumber * currentsignaturesite;
@property (nonatomic, retain) NSNumber * currentsignaturesize;
@property (nonatomic, retain) NSNumber * drawbrushmode;
@property (nonatomic, retain) NSNumber * drawimagetime;
@property (nonatomic, retain) NSData * drawingmedal;
@property (nonatomic, retain) NSData * drawingpoint;
@property (nonatomic, retain) NSData * exppointnum;
@property (nonatomic, retain) NSData * fansmedal;
@property (nonatomic, retain) NSData * fanspoint;
@property (nonatomic, retain) NSData * finishedcommentnum;
@property (nonatomic, retain) NSData * finishedimagenum;
@property (nonatomic, retain) NSData * finishedmusicnum;
@property (nonatomic, retain) NSNumber * imageplaymode;
@property (nonatomic, retain) NSNumber * imageplaywaitingtime;
@property (nonatomic, retain) NSNumber * imagesizefilter;
@property (nonatomic, retain) NSDate * lastfinishoneimagetime;
@property (nonatomic, retain) NSData * level;
@property (nonatomic, retain) NSData * newpersonmedal;
@property (nonatomic, retain) NSData * newpersonpoint;
@property (nonatomic, retain) NSNumber * powerviewsize;
@property (nonatomic, retain) NSData * presentondutymedal;
@property (nonatomic, retain) NSData * presentondutypoint;
@property (nonatomic, retain) NSNumber * rotatescreen;
@property (nonatomic, retain) NSData * shareimagenum;
@property (nonatomic, retain) NSData * sharemedal;
@property (nonatomic, retain) NSData * sharepoint;
@property (nonatomic, retain) NSNumber * showpowerview;
@property (nonatomic, retain) NSNumber * showsignatureview;
@property (nonatomic, retain) NSNumber * showtime;
@property (nonatomic, retain) NSNumber * signatureautocolor;
@property (nonatomic, retain) NSNumber * signaturecolorindex;
@property (nonatomic, retain) NSNumber * signaturelinewidth;
@property (nonatomic, retain) NSNumber * signaturesize;
@property (nonatomic, retain) NSNumber * sleep;
@property (nonatomic, retain) NSNumber * sleepautocontrol;
@property (nonatomic, retain) NSData * timetextstyle;
@property (nonatomic, retain) NSSet *imagelist;
@property (nonatomic, retain) NSSet *issuereport;
@property (nonatomic, retain) NSSet *musiclist;
@property (nonatomic, retain) NSSet *signaturelist;
@property (nonatomic, retain) NSSet *upgradelist;
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
- (void)addUpgradelistObject:(UpgradeInfo *)value;
- (void)removeUpgradelistObject:(UpgradeInfo *)value;
- (void)addUpgradelist:(NSSet *)values;
- (void)removeUpgradelist:(NSSet *)values;
@end
