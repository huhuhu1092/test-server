//
//  FinishedImage.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-5-21.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <CoreData/CoreData.h>


@interface FinishedImage : NSManagedObject

@property (nonatomic, retain) NSData * data;
@property (nonatomic, retain) NSData * thumbnail;
@property (nonatomic, retain) NSString * url;
@property (nonatomic, retain) NSString * urldate;
@property (nonatomic, retain) NSNumber * orientation;

@end
