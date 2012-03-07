//
//  UserInfo.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-3-4.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <CoreData/CoreData.h>


@interface UserInfo : NSManagedObject {
@private
}
@property (nonatomic, retain) NSString * currentimagelist;
@property (nonatomic, retain) NSNumber * currentimagequality;
@property (nonatomic, retain) NSNumber * currentimagetimes;
@property (nonatomic, retain) NSString * currentmusiclist;
@property (nonatomic, retain) NSNumber * currentsignature;
@property (nonatomic, retain) NSNumber * level;
@property (nonatomic, retain) NSNumber * currentsignaturesize;
@property (nonatomic, retain) NSNumber * currentsignaturesite;
@property (nonatomic, retain) NSSet *imagelist;
@property (nonatomic, retain) NSSet *musiclist;
@property (nonatomic, retain) NSSet *signaturelist;
@end

@interface UserInfo (CoreDataGeneratedAccessors)

- (void)addImagelistObject:(NSManagedObject *)value;
- (void)removeImagelistObject:(NSManagedObject *)value;
- (void)addImagelist:(NSSet *)values;
- (void)removeImagelist:(NSSet *)values;
- (void)addMusiclistObject:(NSManagedObject *)value;
- (void)removeMusiclistObject:(NSManagedObject *)value;
- (void)addMusiclist:(NSSet *)values;
- (void)removeMusiclist:(NSSet *)values;
- (void)addSignaturelistObject:(NSManagedObject *)value;
- (void)removeSignaturelistObject:(NSManagedObject *)value;
- (void)addSignaturelist:(NSSet *)values;
- (void)removeSignaturelist:(NSSet *)values;
@end
