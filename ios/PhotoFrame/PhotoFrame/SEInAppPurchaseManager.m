//
//  SEInAppPurchaseManager.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-8-29.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEInAppPurchaseManager.h"
#import "SEUserDefaultManager.h"
#import "SESystemConfig.h"
#import "PhotoFrameAppDelegate.h"

///
@implementation SEInAppPurchaseProduct
@synthesize mProductId;
@synthesize mProductIdArray;
//@synthesize mProduct;
/*
- (SKProduct*) getProduct
{
    return mProduct;
}
 */
- (id) initWithProductId: (NSString*) productId
{
    self = [super init];
    if(self)
    {
        self.mProductId = productId;
    }
    return self;
}
- (void) dealloc
{
    [mProductId release];
    [mProductIdArray release];
    //[mProduct release];
    [mProductsRequest release];
    [super dealloc];
}
- (void) requestProductData
{
    NSSet* productSet = nil;
    if(mProductIdArray)
    {
        productSet = [NSSet setWithArray:mProductIdArray];
    }
    else
    {
        productSet = [NSSet setWithObject:mProductId];
    }
    mProductsRequest = [[SKProductsRequest alloc] initWithProductIdentifiers: productSet];
    mProductsRequest.delegate = self;
    [mProductsRequest start];
}
- (void) request:(SKRequest *)request didFailWithError:(NSError *)error
{
    NSLog(@"request product error");
    //NSDictionary *userInfo = [NSDictionary dictionaryWithObjectsAndKeys:[NSNull null], @"productId" , [NSNull null], @"price", [NSNumber numberWithBool:NO], @"requestOK",[NSNull null], @"priceLocale",nil];
    NSDictionary* userInfo = [NSDictionary dictionaryWithObjectsAndKeys:[NSNull null], @"products", [NSNumber numberWithBool:NO], @"requestOK", nil];
    [[NSNotificationCenter defaultCenter] postNotificationName:[SESystemConfig getProductRequectNotificationName] object:self userInfo:userInfo];
    [mProductsRequest release];
    mProductsRequest = nil;
    [self release];
}
- (void) requestDidFinish:(SKRequest *)request
{
    NSLog(@"request product finished");
    [mProductsRequest release];
    mProductsRequest = nil;
    [self release];
}
- (void) productsRequest:(SKProductsRequest *)request didReceiveResponse:(SKProductsResponse *)response
{
    NSArray* products = response.products;
    //NSMutableArray* productsIdArray = [NSMutableArray array];
    //NSMutableArray* priceArray = [NSMutableArray array];
    //NSMutableArray* priceLocaleArray = [NSMutableArray array];
    for(int i = 0 ; i < products.count ; i++)
    {
        SKProduct* product = [products objectAtIndex:i];
        NSLog(@"product title = %@", product.localizedTitle);
        NSLog(@"product description = %@", product.localizedDescription);
        NSLog(@"product price : %@", product.price);
        NSLog(@"product id : %@", product.productIdentifier);
        //[productsIdArray addObject:product.productIdentifier];
        //[priceArray addObject:product.price];
        //[priceLocaleArray addObject:product.priceLocale];
    }
    /*
    SKProduct * product = products.count == 1? [products objectAtIndex:0] : nil;
    if(product)
    {
        NSLog(@"product title = %@", product.localizedTitle);
        NSLog(@"product description = %@", product.localizedDescription);
        NSLog(@"product price : %@", product.price);
        NSLog(@"product id : %@", product.productIdentifier);
        //self.mProduct = product;
    }
     */
    for(NSString* invalidId in response.invalidProductIdentifiers)
    {
        NSLog(@"invalid product id = %@", invalidId);
    }
    if(products.count > 0)
    {
        //NSDictionary *userInfo = [NSDictionary dictionaryWithObjectsAndKeys:productsIdArray, @"productId" , priceArray, @"price", [NSNumber numberWithBool:YES], @"requestOK", priceLocaleArray, @"priceLocale",nil];
        NSDictionary* userInfo = [NSDictionary dictionaryWithObjectsAndKeys:products, @"products", [NSNumber numberWithBool:YES], @"requestOK", nil];
        [[NSNotificationCenter defaultCenter] postNotificationName:[SESystemConfig getProductRequectNotificationName] object:self userInfo:userInfo];
    }
    else
    {
       // NSDictionary *userInfo = [NSDictionary dictionaryWithObjectsAndKeys:[NSNull null], @"productId" , [NSNull null], @"price", [NSNumber numberWithBool:NO], @"requestOK", [NSNull null], @"priceLocale",nil];
        NSDictionary* userInfo = [NSDictionary dictionaryWithObjectsAndKeys:[NSNull null], @"products", [NSNumber numberWithBool:NO], @"requestOK", nil];
        [[NSNotificationCenter defaultCenter] postNotificationName:[SESystemConfig getProductRequectNotificationName] object:self userInfo:userInfo];
    }
    //[self release];
}
/*
- (void) setRequestObserver: (id) target withAction : (SEL) action
{
    mTarget = target;
    mAction = action;
}
 */
@end
/*
@implementation SEInAppPurchaseProductManager
@synthesize mProductTransaction;
- (id) init
{
    self = [super init];
    if(self)
    {
        mProductArray = [[NSMutableArray alloc] initWithArray:[NSArray array]];
    }
    return self;
}
- (void) dealloc
{
    [mProductArray release];
    [super dealloc];
}
- (SEInAppPurchaseProduct*) getProduct: (NSString*) productId
{
    for(SEInAppPurchaseProduct* product in mProductArray)
    {
        if([product.mProductId isEqualToString:productId])
        {
            return product;
        }
    }
    return nil;
}
- (void) requestObserver
{
    mRequestIndex++;
    if(mRequestIndex < mProductArray.count)
    {
        SEInAppPurchaseProduct* p = [mProductArray objectAtIndex:mRequestIndex];
        [p requestProductData];
    }
    else
    {
        // request all product
        NSLog(@"all product request ok");
        if([SEInAppPurchaseTransactionObserver canMakePurchases])
        {
            [mProductTransaction purchaseProduct];
        }
    }
}
- (void) addProduct: (NSString*) productId
{
    SEInAppPurchaseProduct* p = [[SEInAppPurchaseProduct alloc] initWithProductId:productId];
    [mProductArray addObject:p];
    [p setRequestObserver:self withAction:@selector(requestObserver)];
    [p release];
}
- (void) removeProduct: (NSString*) productId
{
    SEInAppPurchaseProduct* removedProduct = nil;
    for(int i = 0 ; i < mProductArray.count ; i++)
    {
        SEInAppPurchaseProduct* p = [mProductArray objectAtIndex:i];
        if([p.mProductId isEqualToString:productId])
        {
            removedProduct = p;
            break;
        }
    }
    if(removedProduct)
    {
        [mProductArray removeObject:removedProduct];
    }
}
- (void) removeAllProduct
{
    [mProductArray removeAllObjects];
}
- (void) requestAllProduct
{
    if(mProductArray.count > 0)
    {
        mRequestIndex = 0;
        SEInAppPurchaseProduct* p = [mProductArray objectAtIndex:mRequestIndex];
        [p requestProductData];
    }
}
- (NSArray*) getAllProduct
{
    NSArray* products = [NSArray arrayWithArray:mProductArray];
    return products;
}
@end
 */
@implementation SEInAppPurchaseTransactionObserver
- (id) init
{
    self = [super init];
    if(self)
    {
        //mProductManager = [[SEInAppPurchaseProductManager alloc] init];
        //mProductManager.mProductTransaction = self;
    }
    return self;
}
- (void) dealloc
{
    //[mProductManager release];
    [super dealloc];
}
/*
- (void) addProduct: (NSString*)productId
{
    //[mProductManager addProduct:productId];
}
- (void) removeProduct: (NSString*)productId
{
    [mProductManager removeProduct:productId];
}
- (void) removeAllProduct
{
    [mProductManager removeAllProduct];
}
- (void) requestAllProduct
{
    [mProductManager requestAllProduct];
}
 */
- (void) loadStore
{
    [[SKPaymentQueue defaultQueue] addTransactionObserver:self];
    //[[SKPaymentQueue defaultQueue] restoreCompletedTransactions];
}
+ (BOOL) canMakePurchases
{
    return [SKPaymentQueue canMakePayments]; 
}
/*
- (BOOL) productInProductManager: (NSString*)productId
{
    SEInAppPurchaseProduct* product = [mProductManager getProduct:productId];
    return product != nil;
}
 */
- (void) recordTransaction: (SKPaymentTransaction*) transaction
{
    /*
    if ([self productInProductManager: transaction.payment.productIdentifier])
    {
        // save the transaction receipt to disk
        NSLog(@"## product = %@, receipt = %@", transaction.payment.productIdentifier, transaction.transactionReceipt);
        [[NSUserDefaults standardUserDefaults] setValue:transaction.transactionReceipt forKey:transaction.payment.productIdentifier];
        [[NSUserDefaults standardUserDefaults] synchronize];
    }
     */
}
- (void) provideContent: (NSString*) productId
{
    [SEUserDefaultManager buyProduct:productId];
    [[NSUserDefaults standardUserDefaults] synchronize];
}


- (void) completeTransaction: (SKPaymentTransaction*)transaction
{
    NSLog(@"complete transaction");
    [self provideContent:transaction.payment.productIdentifier];
    [[SKPaymentQueue defaultQueue] finishTransaction:transaction];
    NSDictionary *userInfo = [NSDictionary dictionaryWithObjectsAndKeys:transaction.payment.productIdentifier, @"productId" , nil];
    [[NSNotificationCenter defaultCenter] postNotificationName:[SESystemConfig getProductPurchaseSuccessNotificationName] object:self userInfo:userInfo];
}
- (void) failedTransaction: (SKPaymentTransaction*)transaction
{
    NSLog(@"failed transaction");
    [[SKPaymentQueue defaultQueue] finishTransaction: transaction];
    NSDictionary *userInfo = [NSDictionary dictionaryWithObjectsAndKeys:transaction.payment.productIdentifier, @"productId" , nil];
    if (transaction.error.code != SKErrorPaymentCancelled) 
    {
        // Optionally, display an error here.
        NSLog(@"payment transaction error\n");
        
        [[NSNotificationCenter defaultCenter] postNotificationName:[SESystemConfig getProductPurchaseFailedNotificationName] object:self userInfo:userInfo];
    }
    else
    {
        [[NSNotificationCenter defaultCenter] postNotificationName:[SESystemConfig getproductPurchaseCancelNotificationName] object:self userInfo:userInfo];
    }
}
- (void) restoreTransaction: (SKPaymentTransaction*)transaction
{
    NSLog(@"restore transaction");
    [self provideContent: transaction.originalTransaction.payment.productIdentifier];
    [[SKPaymentQueue defaultQueue] finishTransaction:transaction];
    NSDictionary *userInfo = [NSDictionary dictionaryWithObjectsAndKeys:transaction.payment.productIdentifier, @"productId" , nil];
    [[NSNotificationCenter defaultCenter] postNotificationName:[SESystemConfig getProductPurchaseRestoreNotificationName] object:self userInfo:userInfo];
}

- (void) paymentQueue:(SKPaymentQueue *)queue updatedTransactions:(NSArray *)transactions
{
    for (SKPaymentTransaction *transaction in transactions)
    {
        NSLog(@"transaction product id = %@", transaction.originalTransaction.payment.productIdentifier);
        switch (transaction.transactionState)
        {
            case SKPaymentTransactionStatePurchased:
                [self completeTransaction:transaction];
                break;
            case SKPaymentTransactionStateFailed:
                [self failedTransaction:transaction];
                break;
            case SKPaymentTransactionStateRestored:
                [self restoreTransaction:transaction];
            default:
                break;
        }
    }
}
- (void)paymentQueueRestoreCompletedTransactionsFinished:(SKPaymentQueue *)queue
{
    NSLog(@"payment queue restore completed transaction finished");
}
- (void)paymentQueue:(SKPaymentQueue *)queue removedTransactions:(NSArray *)transactions
{
    NSLog(@"payment queue removed transaction");
}
- (void) paymentQueue:(SKPaymentQueue *)queue restoreCompletedTransactionsFailedWithError:(NSError *)error
{
    NSLog(@"payment queue restore completed transaction");
}
/*
- (void) addPayment : (SKProduct*) product
{
    SKPayment *payment = [SKPayment paymentWithProduct:product];
    [[SKPaymentQueue defaultQueue] addPayment:payment];
}
 */
- (BOOL) purchaseProduct: (NSString*)productId
{
    if(productId == nil)
    {
        return NO;
    }
    SKPayment* payment = [SKPayment paymentWithProductIdentifier:productId];
    [[SKPaymentQueue defaultQueue] addPayment: payment];
    return YES;
}
/*
- (void) purchaseProduct
{
    NSArray* allProducts = [mProductManager getAllProduct];
    for(SEInAppPurchaseProduct* product in allProducts)
    {
        SKProduct* p = [product getProduct];
        NSLog(@"wanted proudct = %@", product.mProductId);
        if(p != nil)
        {
            SKPayment* payment = [SKPayment paymentWithProductIdentifier:product.mProductId];
            [[SKPaymentQueue defaultQueue] addPayment: payment];
        }
    }
}
- (void) setTarget: (id) target action:(SEL)action
{
    mTarget = target;
    mAction = action;
}
 */
@end
