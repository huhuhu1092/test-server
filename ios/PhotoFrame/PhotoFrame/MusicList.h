//
//  MusicList.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-3-7.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <CoreData/CoreData.h>

@class ImageList;

@interface MusicList : NSManagedObject {
@private
}
@property (nonatomic, retain) NSString * name;
@property (nonatomic, retain) NSNumber * seq;
@property (nonatomic, retain) NSSet *attachimagelist;
@property (nonatomic, retain) NSSet *selectedmusic;
@end

@interface MusicList (CoreDataGeneratedAccessors)

- (void)addAttachimagelistObject:(ImageList *)value;
- (void)removeAttachimagelistObject:(ImageList *)value;
- (void)addAttachimagelist:(NSSet *)values;
- (void)removeAttachimagelist:(NSSet *)values;
- (void)addSelectedmusicObject:(NSManagedObject *)value;
- (void)removeSelectedmusicObject:(NSManagedObject *)value;
- (void)addSelectedmusic:(NSSet *)values;
- (void)removeSelectedmusic:(NSSet *)values;
@end
