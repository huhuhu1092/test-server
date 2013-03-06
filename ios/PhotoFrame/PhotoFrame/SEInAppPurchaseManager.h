//
//  SEInAppPurchaseManager.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-8-29.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <StoreKit/StoreKit.h>
#define FAIL_TRANSACTION_ID @"FAILED"

@interface SEInAppPurchaseProduct : NSObject <SKProductsRequestDelegate>
{
    //SKProduct* mProduct;
    SKProductsRequest* mProductsRequest;
    NSString* mProductId;
    NSArray* mProductIdArray;
    //id mTarget;
    //SEL mAction;
}
@property (nonatomic, retain) NSString* mProductId;
@property (nonatomic, retain) NSArray* mProductIdArray;
//@property (nonatomic, retain) SKProduct* mProduct;
- (id) initWithProductId: (NSString*) productId;
- (void) requestProductData;
//- (void) setRequestObserver: (id) target withAction : (SEL) action;
//- (SKProduct*) getProduct;
@end
/*
@class SEInAppPurchaseTransactionObserver;
@interface SEInAppPurchaseProductManager : NSObject
{
    NSMutableArray* mProductArray;
    int mRequestIndex;
    SEInAppPurchaseTransactionObserver* mProductTransaction;
}
@property (nonatomic, assign) SEInAppPurchaseTransactionObserver* mProductTransaction;
- (SEInAppPurchaseProduct*) getProduct: (NSString*) productId;
- (NSArray*) getAllProduct;
- (void) addProduct: (NSString*) productId;
- (void) removeProduct: (NSString*) productId;
- (void) requestAllProduct;
- (void) removeAllProduct;
@end
 */
@interface SEInAppPurchaseTransactionObserver : NSObject<SKPaymentTransactionObserver>
{
    //NSMutableArray* mProductArray;
    //SEInAppPurchaseProductManager* mProductManager;
    //id mTarget;
    //SEL mAction;
}
- (void) loadStore;
+ (BOOL) canMakePurchases;
- (BOOL) purchaseProduct: (NSString*)productId;
/*
- (void) addProduct: (NSString*)productId;
- (void) removeProduct: (NSString*)productId;
- (void) removeAllProduct;
- (void) requestAllProduct;
- (void) addPayment : (SKProduct*) product;
- (void) purchaseProduct;
- (void) setTarget: (id) target action:(SEL)action;
 */
@end
