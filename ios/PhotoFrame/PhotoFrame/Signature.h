//
//  Signature.h
//  PhotoFrame
//
//  Created by 陈勇 on 13-1-13.
//  Copyright (c) 2013年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <CoreData/CoreData.h>


@interface Signature : NSManagedObject

@property (nonatomic, retain) NSData * data;
@property (nonatomic, retain) NSString * name;
@property (nonatomic, retain) NSNumber * seq;
@property (nonatomic, retain) NSNumber * totaltime;

@end
