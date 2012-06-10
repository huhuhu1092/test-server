//
//  SelectedMusic.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-5-21.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <CoreData/CoreData.h>


@interface SelectedMusic : NSManagedObject

@property (nonatomic, retain) NSString * album;
@property (nonatomic, retain) NSString * date;
@property (nonatomic, retain) NSNumber * seq;
@property (nonatomic, retain) NSString * singer;
@property (nonatomic, retain) NSString * title;

@end
