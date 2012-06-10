//
//  SelectedImage.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-5-21.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <CoreData/CoreData.h>


@interface SelectedImage : NSManagedObject

@property (nonatomic, retain) NSString * filepath;
@property (nonatomic, retain) NSNumber * height;
@property (nonatomic, retain) NSNumber * seq;
@property (nonatomic, retain) NSString * url;
@property (nonatomic, retain) NSString * urldate;
@property (nonatomic, retain) NSNumber * urltype;
@property (nonatomic, retain) NSNumber * width;
@property (nonatomic, retain) NSNumber * orientation;

@end
