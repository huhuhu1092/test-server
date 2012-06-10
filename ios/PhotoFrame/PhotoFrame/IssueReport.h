//
//  IssueReport.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-5-21.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <CoreData/CoreData.h>


@interface IssueReport : NSManagedObject

@property (nonatomic, retain) NSString * date;
@property (nonatomic, retain) NSString * descript;
@property (nonatomic, retain) NSString * devicename;
@property (nonatomic, retain) NSString * title;

@end
