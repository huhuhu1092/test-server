//
//  SEUserDefaultManager.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-9-9.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>

enum USER_FUNCTION {BASIC_FUNC, LEVELUP_MAX_FUNC, ADD_IMAGE_NUMBER_FUNC, ADD_MUSIC_NUMBER_FUNC,
BUY_BRUSHES_FUNC, BUY_TIME_STYLE_FUNC, ADD_SIGNATURE_FUNC, DRAWING_BASIC_FUNC, BRUSH_SETTING_FUNC,
BRUSH_DITHER_FUNC, BRUSH_ALPHA_FUNC, BRUSH_QUALITY_FUNC, BRUSH_TIMES_FUNC, BRUSH_PREVIEW_FUNC, BRUSH_SELECT_FUNC, TIME_STYLE_SELECT_FUNC ,SIGNATURE_FUNC, SIGNATURE_LIMIT_FUNC, USERINFO_FUNC, USER_FUNCTION_NUM};
enum PRODUCT_TYPE {PRODUCT_BASIC_SETTING, PRODUCT_BRUSH_SETTING, PRODUCT_ADD_MUSIC, PRODUCT_ADD_IMAGE, PRODUCT_LEVELUP_MAX, PRODUCT_BRUSH, PRODUCT_TIME_STYLE, PRODUCT_NUM};
enum PRODUCT_CATEGORY_TYPE {PRODUCT_CATEGORY_BASIC, PRODUCT_CATEGORY_PLAY,PRODUCT_CATEGORY_BRUSH, PRODUCT_CATEGORY_TIME_STYLE, PRODUCT_CATEGORY_NUM};
enum ITEM_GETTING_WAY {ITEM_LEVELUP = 1, ITEM_ARCHIEVE = 2, ITEM_DEFAULT = 3,  ITEM_BUY = 4};
@interface SEUserDefaultManager : NSObject
+ (BOOL) isFunctionBuied: (enum USER_FUNCTION) func;
+ (BOOL) isFunctionOK: (enum USER_FUNCTION) func;
+ (void) setFunction: (enum USER_FUNCTION) f value:(BOOL) b;
+ (BOOL) isProductBuied: (NSString*) productId;
+ (void) buyProduct: (NSString*) productId;
+ (BOOL) isProductProcessing: (NSString*) productId;
+ (void) setProductProcessing: (NSString*)productId;
+ (void) addProductFunctionMap: (NSString*)productId func: (enum USER_FUNCTION) func;
+ (void) load: (NSString*)pn;
+ (void) test;
+ (void)restoreProductAndFunction;
@end
@interface SEProductCategory : NSObject
{
    enum PRODUCT_CATEGORY_TYPE type;
    NSString* description;
}
@property (nonatomic, retain) NSString* description;
@property (nonatomic, assign) enum PRODUCT_CATEGORY_TYPE type;
@end
@interface  SEProduct : NSObject  
{
    
    enum PRODUCT_TYPE productType;
    NSString* productId;
    enum PRODUCT_CATEGORY_TYPE categoryType;
    float productPrice;
    NSString* productDescript1;
    NSString* productDescript2;
    enum USER_FUNCTION func;
    NSString* imageName;
    
    NSString* startDate;
    NSString* endDate;
    
    NSArray* contentNames;
};
@property (nonatomic, retain) NSArray* contentNames;
@property (nonatomic, retain) NSString* productId;
@property (nonatomic, retain) NSString* productDescript1;
@property (nonatomic, retain) NSString* productDescript2;
@property (nonatomic, assign) enum PRODUCT_TYPE productType;
@property (nonatomic, assign) enum PRODUCT_CATEGORY_TYPE categoryType;
@property (nonatomic, assign) float productPrice;
@property (nonatomic, assign) enum USER_FUNCTION func;
@property (nonatomic, retain) NSString* imageName;
@property (nonatomic, retain) NSString* startDate;
@property (nonatomic, retain) NSString* endDate;
- (UIImage*) getIcon;
-(NSArray*) getStringSepByStar: (NSString*)str;
- (BOOL) isDiscount;
@end 
@interface SETimeStyle : NSObject   
{
    NSString* fontOutName;
    int gettingWay;
    int fontId;
}
@property (nonatomic, retain) NSString* fontOutName;
@property (nonatomic, assign) int gettingWay;
@property (nonatomic, assign) int fontId;
@end
@interface SEProductManager : NSObject
{
    NSMutableArray* mCategoryArray;
    NSMutableArray* mProductArray;
    NSMutableArray* mTimeStyleArray;
    NSMutableArray* mProductOneTimeBuyArray;
    NSMutableArray* mDiscountArray;
    NSMutableArray* mProductDependenceArray;
}
- (SEProductCategory*) getCategory: (enum PRODUCT_CATEGORY_TYPE) ct;
- (int) getCatetoryNum;
- (SEProductCategory*) getCategoryByIndex: (int)index;
- (SEProduct*) getProduct: (NSString*)productId;
- (SEProduct*) getProductWithDate: (NSString*) productId date: (NSDate*)date;
- (SEProduct*) getProductByIndex: (int) index;
- (SEProduct*) getDiscount: (NSString*) discountId;
- (NSString*) getProductIdFromDiscountId: (NSString*)discountId;
- (NSArray*) getAllDiscount;
- (NSArray*) getDiscountByDate: (NSDate*)date withCategory: (enum PRODUCT_CATEGORY_TYPE) categoryType;
- (SEProduct*) getDiscountByDate: (NSDate*) discountId: (NSString*)productId;
- (int) getProductNum;
- (NSMutableArray*) getProductArrayByCatetoryType:(enum PRODUCT_CATEGORY_TYPE) categoryType;
- (NSArray*) getProductAndDiscountArrayByCatetoryType:(enum PRODUCT_CATEGORY_TYPE) categoryType;
- (NSString*) getProductIdByBrushOutName: (NSString*)outName;
- (NSString*) getDiscountIdByBrushOutName: (NSString*)outName;
- (NSString*) getProductIdByTimeStyleOutName: (NSString*) outName;
- (int) getTimeStyleIdByOutName: (NSString*) outName;
- (SETimeStyle*) getTimeStyleById: (int)styleId;
- (void) loadTimeStyle: (NSString*) fileName;
- (BOOL) isProductTypeOneTimeBuy: (enum PRODUCT_TYPE)type;
+ (SEProductManager*) createFromFile:(NSString*) fileName;
- (int) getUserBuiedProductNum;
- (int) getProductDependenceNum;
- (NSString*) getProductDependenceMainName: (int)index;
- (NSArray*) getProductDependenceAffectedProduct: (int) index;
- (NSArray*) getProductDependenceByProductId : (NSString*) productId;
- (NSArray*) getProductByFunc: (enum USER_FUNCTION) func;
- (NSDate*) stringToDate:(NSString*) str;
@end