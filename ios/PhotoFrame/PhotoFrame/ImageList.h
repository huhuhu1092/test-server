//
//  ImageList.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-5-21.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <CoreData/CoreData.h>

@class SelectedImage;

@interface ImageList : NSManagedObject

@property (nonatomic, retain) NSString * name;
@property (nonatomic, retain) NSNumber * seq;
@property (nonatomic, retain) NSSet *selectedimage;
@end

@interface ImageList (CoreDataGeneratedAccessors)

- (void)addSelectedimageObject:(SelectedImage *)value;
- (void)removeSelectedimageObject:(SelectedImage *)value;
- (void)addSelectedimage:(NSSet *)values;
- (void)removeSelectedimage:(NSSet *)values;

@end
