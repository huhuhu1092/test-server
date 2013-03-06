//
//  SEJSONReader.h
//  TestJSON
//
//  Created by 陈勇 on 12-9-27.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <CoreData/CoreData.h>
@interface SEJSONReader : NSObject <NSFetchedResultsControllerDelegate>
{
    NSMutableData* mRecvData;
}
//Just For test
- (void) parseJSON:(NSString*)fileName;
- (void) parseJSONData: (const unsigned char*)buffer len: (int)len;
- (void) parseURL:(NSURL*) url;
//end
- (void) sendGooglePlusOAuthInfo: (NSString*)clinetID;
@end
