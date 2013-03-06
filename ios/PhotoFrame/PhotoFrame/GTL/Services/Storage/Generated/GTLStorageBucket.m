/* Copyright (c) 2012 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//
//  GTLStorageBucket.m
//

// ----------------------------------------------------------------------------
// NOTE: This file is generated from Google APIs Discovery Service.
// Service:
//   Cloud Storage API (storage/v1beta1)
// Description:
//   Lets you store and retrieve potentially-large, immutable data objects.
// Documentation:
//   https://developers.google.com/storage/docs/json_api/
// Classes:
//   GTLStorageBucket (0 custom class methods, 10 custom properties)
//   GTLStorageBucketOwner (0 custom class methods, 2 custom properties)
//   GTLStorageBucketWebsite (0 custom class methods, 2 custom properties)

#import "GTLStorageBucket.h"

#import "GTLStorageBucketAccessControl.h"
#import "GTLStorageObjectAccessControl.h"

// ----------------------------------------------------------------------------
//
//   GTLStorageBucket
//

@implementation GTLStorageBucket
@dynamic acl, defaultObjectAcl, identifier, kind, location, owner, projectId,
         selfLink, timeCreated, website;

+ (NSDictionary *)propertyToJSONKeyMap {
  NSDictionary *map =
    [NSDictionary dictionaryWithObject:@"id"
                                forKey:@"identifier"];
  return map;
}

+ (NSDictionary *)arrayPropertyToClassMap {
  NSDictionary *map =
    [NSDictionary dictionaryWithObjectsAndKeys:
      [GTLStorageBucketAccessControl class], @"acl",
      [GTLStorageObjectAccessControl class], @"defaultObjectAcl",
      nil];
  return map;
}

+ (void)load {
  [self registerObjectClassForKind:@"storage#bucket"];
}

@end


// ----------------------------------------------------------------------------
//
//   GTLStorageBucketOwner
//

@implementation GTLStorageBucketOwner
@dynamic entity, entityId;
@end


// ----------------------------------------------------------------------------
//
//   GTLStorageBucketWebsite
//

@implementation GTLStorageBucketWebsite
@dynamic mainPageSuffix, notFoundPage;
@end
