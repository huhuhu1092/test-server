//
//  SelectedImage.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-2-24.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <CoreData/CoreData.h>


@interface SelectedImage : NSManagedObject {
@private
}
@property (nonatomic, retain) NSString * filepath;
@property (nonatomic, retain) NSNumber * seq;
@property (nonatomic, retain) NSString * url;
@property (nonatomic, retain) NSDate * urldate;

@end
