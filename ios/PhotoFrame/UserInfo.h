//
//  UserInfo.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-2-5.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <CoreData/CoreData.h>

@class SelectedImage;

@interface UserInfo : NSManagedObject {
@private
}
@property (nonatomic, retain) NSNumber * level;
@property (nonatomic, retain) SelectedImage *imageinfo;

@end
