//
//  MusicList.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-5-27.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <CoreData/CoreData.h>

@class ImageList, SelectedMusic;

@interface MusicList : NSManagedObject

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

- (void)addSelectedmusicObject:(SelectedMusic *)value;
- (void)removeSelectedmusicObject:(SelectedMusic *)value;
- (void)addSelectedmusic:(NSSet *)values;
- (void)removeSelectedmusic:(NSSet *)values;

@end
