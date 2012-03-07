//
//  Signature.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-3-5.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <CoreData/CoreData.h>


@interface Signature : NSManagedObject {
@private
}
@property (nonatomic, retain) NSString * name;
@property (nonatomic, retain) NSNumber * seq;
@property (nonatomic, retain) NSData * data;

@end
