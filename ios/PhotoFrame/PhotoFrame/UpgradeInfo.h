//
//  UpgradeInfo.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-10-3.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <CoreData/CoreData.h>


@interface UpgradeInfo : NSManagedObject

@property (nonatomic, retain) NSNumber * seq;
@property (nonatomic, retain) NSNumber * achievementtype;
@property (nonatomic, retain) NSNumber * medal;
@property (nonatomic, retain) NSNumber * fromlevel;
@property (nonatomic, retain) NSNumber * tolevel;

@end
