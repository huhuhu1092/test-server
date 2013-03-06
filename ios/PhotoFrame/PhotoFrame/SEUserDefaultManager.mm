//
//  SEUserDefaultManager.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-9-9.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEUserDefaultManager.h"
#import "SEUtil.h"
#import "PhotoFrameAppDelegate.h"
#import "SEResDefine.h"
#import "SEViewNavigator.h"
#import "PainterManager.h"
#import "SEKeyChainHelper.h"
#include <list>
#include <map>
#include <string>
struct UserFunctionProperty
{
    std::string key;
    std::list<USER_FUNCTION> precedence;
    std::list<USER_FUNCTION> affect;
};
struct UserFunctionDefine
{
    NSString* name;
    enum USER_FUNCTION fun;
};

static UserFunctionDefine gFunctionDefines[] = {
    {@"BASIC_FUNC",BASIC_FUNC},
    {@"LEVELUP_MAX_FUNC", LEVELUP_MAX_FUNC}, 
    {@"ADD_IMAGE_NUMBER_FUNC", ADD_IMAGE_NUMBER_FUNC}, 
    {@"ADD_MUSIC_NUMBER_FUNC", ADD_MUSIC_NUMBER_FUNC},
    {@"BUY_BRUSHES_FUNC", BUY_BRUSHES_FUNC}, 
    {@"BUY_TIME_STYLE_FUNC", BUY_TIME_STYLE_FUNC},
    {@"ADD_SIGNATURE_FUNC", ADD_SIGNATURE_FUNC}, 
    {@"DRAWING_BASIC_FUNC", DRAWING_BASIC_FUNC}, 
    {@"BRUSH_SETTING_FUNC", BRUSH_SETTING_FUNC},
    {@"BRUSH_DITHER_FUNC", BRUSH_DITHER_FUNC}, 
    {@"BRUSH_ALPHA_FUNC", BRUSH_ALPHA_FUNC}, 
    {@"BRUSH_QUALITY_FUNC", BRUSH_QUALITY_FUNC}, 
    {@"BRUSH_TIMES_FUNC", BRUSH_TIMES_FUNC}, 
    {@"BRUSH_PREVIEW_FUNC", BRUSH_PREVIEW_FUNC}, 
    {@"BRUSH_SELECT_FUNC", BRUSH_SELECT_FUNC}, 
    {@"TIME_STYLE_SELECT_FUNC", TIME_STYLE_SELECT_FUNC} ,
    {@"SIGNATURE_FUNC", SIGNATURE_FUNC}, 
    {@"SIGNATURE_LIMIT_FUNC", SIGNATURE_LIMIT_FUNC}, 
    {@"USERINFO_FUNC", USERINFO_FUNC}
};
static std::map<USER_FUNCTION, UserFunctionProperty> gFunctionNameKeyMap;
static std::map<std::string, USER_FUNCTION> gProductFuncMap;
static std::map<USER_FUNCTION, std::string> gFuncProductMap;
//static UserFunctionNameKey gFunctionNameKeyes[USER_FUNCTION_NUM];
/*
static UserFunctionKeyValue gFunctionKeyValue[] = {
    {BASIC_FUNC, @"basic_func"},
    {LEVELUP_MAX_FUNC, @"level_up_to_max"},
    {ADD_IMAGE_NUMBER_FUNC, @"add_image_number"},
    {ADD_MUSIC_NUMBER_FUNC, @"add_music_number"},
    {BUY_BRUSHES_FUNC, @"buy_brushes"},
    {BUY_TIME_STYLE_FUNC, @"buy_time_style"},
    {ADD_SIGNATURE_FUNC, @"add_signture"},
    {DRAWING_BASIC_FUNC, @"draw_basic_setting"},
    {BRUSH_SETTING_FUNC, @"brush_setting"},
    {BRUSH_DITHER_FUNC, @"brush_dither"},
    {BRUSH_ALPHA_FUNC, @"brush_alpha"},
    {SIGNATURE_FUNC, @"signature_work"},
    {SIGNATURE_LIMIT_FUNC, @"signature_limit"},
    {USERINFO_FUNC, @"user_upgrade"}
};
 */
struct UserFunctionRelation
{
    enum USER_FUNCTION fun;
    std::list<USER_FUNCTION> precedence; // if function will be ok, the precedence must first ok
    std::list<USER_FUNCTION> affect; // if function is ok the affect function will be ok
};
static UserFunctionRelation gAllUserFunction[USER_FUNCTION_NUM];
/*
static void createUserFunctionReleation()
{
    gAllUserFunction[BASIC_FUNC].fun = BASIC_FUNC;
    gAllUserFunction[BASIC_FUNC].affect.push_back(LEVELUP_MAX_FUNC);
    gAllUserFunction[BASIC_FUNC].affect.push_back(ADD_MUSIC_NUMBER_FUNC);
    gAllUserFunction[BASIC_FUNC].affect.push_back(ADD_IMAGE_NUMBER_FUNC);
    gAllUserFunction[BASIC_FUNC].affect.push_back(BUY_BRUSHES_FUNC);
    gAllUserFunction[BASIC_FUNC].affect.push_back(BUY_TIME_STYLE_FUNC);
    gAllUserFunction[BASIC_FUNC].affect.push_back(ADD_SIGNATURE_FUNC);
    
    gAllUserFunction[LEVELUP_MAX_FUNC].fun = LEVELUP_MAX_FUNC;
    gAllUserFunction[LEVELUP_MAX_FUNC].precedence.push_back(BASIC_FUNC);
    gAllUserFunction[ADD_IMAGE_NUMBER_FUNC].fun = ADD_IMAGE_NUMBER_FUNC;
    gAllUserFunction[ADD_IMAGE_NUMBER_FUNC].precedence.push_back(BASIC_FUNC);
    gAllUserFunction[ADD_MUSIC_NUMBER_FUNC].fun = ADD_MUSIC_NUMBER_FUNC;
    gAllUserFunction[ADD_MUSIC_NUMBER_FUNC].precedence.push_back(BASIC_FUNC);
    gAllUserFunction[BUY_BRUSHES_FUNC].fun = BUY_BRUSHES_FUNC;
    gAllUserFunction[BUY_BRUSHES_FUNC].precedence.push_back(BASIC_FUNC);
    gAllUserFunction[BUY_TIME_STYLE_FUNC].fun = BUY_TIME_STYLE_FUNC;
    gAllUserFunction[BUY_TIME_STYLE_FUNC].precedence.push_back(BASIC_FUNC);
    gAllUserFunction[ADD_SIGNATURE_FUNC].fun = ADD_SIGNATURE_FUNC;
    gAllUserFunction[ADD_SIGNATURE_FUNC].precedence.push_back(BASIC_FUNC);
    
    gAllUserFunction[DRAWING_BASIC_FUNC].fun = DRAWING_BASIC_FUNC;
    gAllUserFunction[DRAWING_BASIC_FUNC].affect.push_back(BRUSH_QUALITY_FUNC);
    gAllUserFunction[DRAWING_BASIC_FUNC].affect.push_back(BRUSH_TIMES_FUNC);
    gAllUserFunction[DRAWING_BASIC_FUNC].affect.push_back(BRUSH_PREVIEW_FUNC);
    
    gAllUserFunction[BRUSH_SETTING_FUNC].fun = BRUSH_SETTING_FUNC;
    gAllUserFunction[BRUSH_SETTING_FUNC].affect.push_back(BRUSH_SELECT_FUNC);
    gAllUserFunction[BRUSH_SETTING_FUNC].affect.push_back(BRUSH_ALPHA_FUNC);
    gAllUserFunction[BRUSH_SETTING_FUNC].affect.push_back(BRUSH_DITHER_FUNC);
    
    gAllUserFunction[BRUSH_DITHER_FUNC].fun = BRUSH_DITHER_FUNC;
    gAllUserFunction[BRUSH_DITHER_FUNC].precedence.push_back(BRUSH_SETTING_FUNC);
    
    gAllUserFunction[BRUSH_ALPHA_FUNC].fun = BRUSH_ALPHA_FUNC;
    gAllUserFunction[BRUSH_ALPHA_FUNC].precedence.push_back(BRUSH_SETTING_FUNC);
    
    gAllUserFunction[BRUSH_QUALITY_FUNC].fun = BRUSH_QUALITY_FUNC;
    gAllUserFunction[BRUSH_QUALITY_FUNC].precedence.push_back(DRAWING_BASIC_FUNC);
    
    gAllUserFunction[BRUSH_TIMES_FUNC].fun = BRUSH_TIMES_FUNC;
    gAllUserFunction[BRUSH_TIMES_FUNC].precedence.push_back(DRAWING_BASIC_FUNC);
    
    gAllUserFunction[BRUSH_PREVIEW_FUNC].fun = BRUSH_PREVIEW_FUNC;
    gAllUserFunction[BRUSH_PREVIEW_FUNC].precedence.push_back(DRAWING_BASIC_FUNC);
    
    gAllUserFunction[BRUSH_SELECT_FUNC].fun = BRUSH_SELECT_FUNC;
    gAllUserFunction[BRUSH_SELECT_FUNC].precedence.push_back(BRUSH_SETTING_FUNC);
    
    gAllUserFunction[TIME_STYLE_SELECT_FUNC].fun = TIME_STYLE_SELECT_FUNC;
    
    gAllUserFunction[SIGNATURE_FUNC].fun = SIGNATURE_FUNC;
    
    gAllUserFunction[SIGNATURE_LIMIT_FUNC].fun = SIGNATURE_LIMIT_FUNC;
    
    gAllUserFunction[USERINFO_FUNC].fun = USERINFO_FUNC;
    
    
}
*/
static NSString* getKey(enum USER_FUNCTION fun)
{
    std::map<USER_FUNCTION, UserFunctionProperty>::iterator it = gFunctionNameKeyMap.find(fun);
    if(it != gFunctionNameKeyMap.end())
        return [NSString stringWithFormat:@"%s", it->second.key.c_str()];
    else 
    {
        return nil;
    }
}

static enum USER_FUNCTION getUserFunctionByFuncName(NSString* str)
{
    int count = sizeof(gFunctionDefines) / sizeof(UserFunctionDefine);
    for(int i = 0 ; i < count ; i++)
    {
        if([gFunctionDefines[i].name isEqualToString:str])
            return gFunctionDefines[i].fun;
    }
    return USER_FUNCTION_NUM;
}
///////////////////////
@interface SEProductDepdence : NSObject
{
    NSString* mProductID;
    NSMutableArray* mAffectProducts;
}
@property (nonatomic, retain) NSString* mProductID;
@property (nonatomic, retain) NSMutableArray* mAffectProduct;
@end
@implementation SEProductDepdence
@synthesize mProductID;
@synthesize mAffectProduct;
- (void) dealloc
{
    [mProductID release];
    [mAffectProduct release];
    [super dealloc];
}
- (id) init
{
    self = [super init];
    if(self)
    {
        mAffectProduct = [[NSMutableArray array] retain];
    }
    return self;
}
@end
///////////////////////
@implementation SEUserDefaultManager

+ (void) load: (NSString*)pn
{
    NSString* data = [SEUtil readDataFromDocumentDir:pn];
    if(data == nil)
    {
        data = [SEUtil readDataFromBundle:pn];
    }
    if(data == nil)
    {
        NSLog(@"## can not find user default manager package : %@ ##\n", pn);
        return;
    }
    NSArray* dataLines = [data componentsSeparatedByString:@"\n"];
    enum BLOCK_TYPE {NO_BLOCK, ENUM_BLOCK, PRECEDENCE_BLOCK, AFFECT_BLOCK};
    BLOCK_TYPE blockType = NO_BLOCK;
    for(int i = 0 ; i < dataLines.count ; i++)
    {
        NSString* line = [dataLines objectAtIndex:i];
        if([SEUtil isWhitespaceLine:line])
            continue;
        line = [SEUtil stringTrim:line];
        NSUInteger len = line.length;
        unichar firstC = [line characterAtIndex:0];
        unichar lastC = [line characterAtIndex:len - 1];
        if(firstC == '#')
            continue;
        if(firstC == '[' && lastC == ']')
        {
            if([line isEqualToString:@"[ENUM]"])
            {
                blockType = ENUM_BLOCK;
            }
            else if([line isEqualToString:@"[PRECEDENCE]"])
            {
                blockType = PRECEDENCE_BLOCK;
            }
            else if([line isEqualToString:@"[AFFECT]"])
            {
                blockType = AFFECT_BLOCK;
            }
            continue;
        }
        NSCharacterSet* cs = [NSCharacterSet whitespaceAndNewlineCharacterSet];
        NSArray* tokens = [line componentsSeparatedByCharactersInSet:cs];
        NSArray* newTokens = [NSArray array];
        for(int i = 0 ; i < tokens.count ; i++)
        {
            NSString* s = [tokens objectAtIndex:i];
            if([SEUtil isWhitespaceLine:s] == NO)
                newTokens = [newTokens arrayByAddingObject:s];
        }
        tokens = newTokens;
        switch (blockType)
        {
            case ENUM_BLOCK:
            {
                assert(tokens.count == 2);
                NSString* funcName = [tokens objectAtIndex:0];
                NSString* funcKey = [tokens objectAtIndex:1];
                USER_FUNCTION f = getUserFunctionByFuncName(funcName);
                if([funcName isEqualToString:@"USERINFO_FUNC"])
                {
                    NSLog(@"userInfo func");
                }
                
                assert(f < USER_FUNCTION_NUM);
                gFunctionNameKeyMap[f].key = std::string([funcKey cStringUsingEncoding:NSASCIIStringEncoding]);
            }
                break;
            case PRECEDENCE_BLOCK:
            {
                assert(tokens.count >= 2);
                NSString* funcName = [tokens objectAtIndex:0];
                USER_FUNCTION f = getUserFunctionByFuncName(funcName);
                assert(f < USER_FUNCTION_NUM);
                for(int i = 1 ; i < tokens.count ; i++)
                {
                    NSString* precedence = [tokens objectAtIndex:i];
                    if([precedence isEqualToString:@"NULL"] == NO)
                    {
                        USER_FUNCTION pre = getUserFunctionByFuncName(precedence);
                        gFunctionNameKeyMap[f].precedence.push_back(pre);
                        gFunctionNameKeyMap[pre].affect.push_back(f);
                    }
                }
            }
                break;
                /*
            case AFFECT_BLOCK:
            {
                assert(tokens.count >= 2);
                NSString* funcName = [[tokens objectAtIndex:0] stringByTrimmingCharactersInSet:cs];
                USER_FUNCTION f = getUserFunctionByFuncName(funcName);
                for(int i = 1 ; i < tokens.count ; i++)
                {
                    NSString* affect = [tokens objectAtIndex:i];
                    USER_FUNCTION a = getUserFunctionByFuncName(affect);
                    gFunctionNameKeyMap[f].affect.push_back(a);
                }
            }
                break;
                 */
            default:
                break;
        }
        
    }
    //gProductFuncMap["com.thespeedsun.PhotoFrame.BasicSetting"] = BASIC_FUNC;
    //[self initUserDefault];
}
+ (void) test
{
    std::list<USER_FUNCTION> precedence = gFunctionNameKeyMap[BUY_BRUSHES_FUNC].precedence;
    assert(precedence.size()== 1);
    for(std::map<USER_FUNCTION, UserFunctionProperty>::iterator it = gFunctionNameKeyMap.begin();
        it != gFunctionNameKeyMap.end(); it++)
    {
        UserFunctionProperty& p = it->second;
        NSLog(@"%d --> ", it->first);
        NSLog(@" %s ", p.key.c_str());
        for(std::list<USER_FUNCTION>::iterator preIt = p.precedence.begin() ; preIt != p.precedence.end() ; preIt++)
        {
            NSLog(@"precedence -- %d --", *preIt);
        }
        for(std::list<USER_FUNCTION>::iterator affectIt = p.affect.begin() ; affectIt != p.affect.end() ; affectIt++)
        {
            NSLog(@"affect -- %d --", *affectIt);
        }
    }
}
+ (void) saveIntValue: (NSString*)key :(int) v
{
    NSData* data = [SEKeyChainHelper createMyStringFromIntValue:v];
    [[NSUserDefaults standardUserDefaults] setObject:data forKey:key];
}
+ (void) saveBoolValue: (NSString*)key :(BOOL) v
{
    NSData* data = [SEKeyChainHelper createMyStringFromBoolValue:v];
    [[NSUserDefaults standardUserDefaults] setObject:data forKey:key];
}
+ (BOOL) getIntValue: (NSString*)key : (int*)outValue
{
    id v = [[NSUserDefaults standardUserDefaults] valueForKey:key];
    if([v isKindOfClass:[NSData class]])
    {
        int value = 0;
        BOOL ret = [SEKeyChainHelper getIntValueFromString:v :&value];
        if(ret)
        {
            *outValue = value;
            return ret;
        }
    }
    return NO;
}
+ (BOOL) getBoolValue: (NSString*)key : (BOOL*) outValue
{
    id v = [[NSUserDefaults standardUserDefaults] valueForKey:key];
    if([v isKindOfClass:[NSData class]])
    {
        BOOL value = 0;
        BOOL ret = [SEKeyChainHelper getBoolValueFromString:v :&value];
        if(ret)
        {
            *outValue = value;
            return ret;
        }
    }
    return NO;
}
+ (BOOL) getBoolValueFromKeyChain: (NSString*)key
{
    NSString* str = [SEKeyChainHelper getPasswordWithService:key];
    if(str == nil)
        return NO;
    int v = [str intValue];
    return v == 1;
}

+ (BOOL) isFunctionBuied: (enum USER_FUNCTION) func
{
    std::string str = gFuncProductMap[func];
    if(str == "")
        return NO;
    NSString* productId = [NSString stringWithCString:str.c_str() encoding:NSASCIIStringEncoding];
    return [SEUserDefaultManager isProductBuied:productId];
}
+ (void)restoreProductAndFunction
{
    SEProductManager* productManager = [PhotoFrameAppDelegate getProductManager];
    int num = [productManager getProductNum];
    NSArray* discountProduct = [productManager getAllDiscount];
    for(int i = 0 ; i < num ; i++)
    {
        SEProduct* product = [productManager getProductByIndex:i];
        [SEKeyChainHelper deletePassword:product.productId];
        NSString* str = getKey(product.func);
        [SEKeyChainHelper deletePassword:str];
    }
    for(SEProduct* product in discountProduct)
    {
        [SEKeyChainHelper deletePassword:product.productId];
        NSString* str = getKey(product.func);
        [SEKeyChainHelper deletePassword:str];
    }
    for(int i = 0 ; i < USER_FUNCTION_NUM ; i++)
    {
        NSString* str = getKey((enum USER_FUNCTION)i);
        [SEKeyChainHelper deletePassword:str];
    }
}
+ (BOOL) isFunctionOK: (enum USER_FUNCTION) func
{
    //return YES;
    NSString* str = getKey(func);
    assert(str != nil);
    if(str == nil)
        return false;
    UserFunctionProperty& p = gFunctionNameKeyMap[func];
    std::list<USER_FUNCTION>::iterator it;
    BOOL allPrecedenceOK = YES;
    for(it = p.precedence.begin() ; it != p.precedence.end() ; it++)
    {
        USER_FUNCTION preFunc = *it;
        NSString* preKey = getKey(preFunc);
        //BOOL v = [SEUserDefaultManager getBoolValueFromKeyChain:preKey];//[[NSUserDefaults standardUserDefaults] boolForKey:preKey];
        BOOL v = NO;
        BOOL ret = [SEUserDefaultManager getBoolValue:preKey :&v];
        if(v == NO)
        {
            allPrecedenceOK = NO;
            break;
        }
    }
    if(allPrecedenceOK)
    {
        //return [[NSUserDefaults standardUserDefaults] boolForKey:str];
        //return [SEUserDefaultManager getBoolValueFromKeyChain:str];
        BOOL v = NO;
        [SEUserDefaultManager getBoolValue:str :&v];
        return v;
    }
    else 
    {
        return NO;
    }
}
+ (void) setFunction: (enum USER_FUNCTION) f value:(BOOL) b
{
    NSString* str = getKey(f);
    assert(str != nil);
    if(str == nil)
        return;
    //[[NSUserDefaults standardUserDefaults] setBool:b forKey:str];
    //[SEKeyChainHelper savePassword:@"1" pwdService:str];
    [SEUserDefaultManager saveBoolValue:str :b];
    UserFunctionProperty& p = gFunctionNameKeyMap[f];
    std::list<USER_FUNCTION>::iterator it;
    for(it = p.affect.begin() ; it != p.affect.end() ; it++)
    {
        USER_FUNCTION affectFunction = *it;
        NSString* affectKey = getKey(affectFunction);
        assert(affectKey != nil);
        if(affectFunction == USERINFO_FUNC)
        {
            NSLog(@"USERINFO buy");
        }
        //[[NSUserDefaults standardUserDefaults] setBool:b forKey:affectKey];
        //[SEKeyChainHelper savePassword:@"1" pwdService:affectKey];
        [SEUserDefaultManager saveBoolValue:affectKey :b];
    }
}
+ (BOOL) isProductBuied: (NSString*) productId
{
    //return YES;
    if(productId == nil)
        return NO;
    //NSString* productValue = [SEKeyChainHelper getPasswordWithService:productId];
    BOOL v = NO;
    [SEUserDefaultManager getBoolValue:productId :&v];
    return v;
    /*
    if(productValue == nil)
        return NO;
    int v = [productValue intValue];
    if(v == 1)
        return YES;
    else {
        return NO;
    }
     */
}
+ (void) buyProduct: (NSString*) productId
{
    //[[NSUserDefaults standardUserDefaults] setBool:YES forKey:productId];
    //[SEKeyChainHelper savePassword:@"1" pwdService:productId];
    [SEUserDefaultManager saveBoolValue:productId :YES];
    std::string str([productId cStringUsingEncoding:NSASCIIStringEncoding]);
    USER_FUNCTION fun = gProductFuncMap[str];
    assert(fun < USER_FUNCTION_NUM);
    [self setFunction:fun value:YES];
    SEProductManager* productManager = [PhotoFrameAppDelegate getProductManager];
    NSArray* depArray = [productManager getProductDependenceByProductId:productId];
    for(int i = 0 ; i < depArray.count ; i++)
    {
        NSString* affectProductId = [depArray objectAtIndex:i];
        [SEUserDefaultManager buyProduct:affectProductId];
    }
}
+ (BOOL) isProductProcessing: (NSString*) productId
{
    NSString* str = [NSString stringWithFormat:@"%@_%@", productId, @"processing"];
    return [[NSUserDefaults standardUserDefaults] boolForKey:str];
}
+ (void) setProductProcessing: (NSString*)productId
{
    NSString* str = [NSString stringWithFormat:@"%@_%@", productId, @"processing"];
    [[NSUserDefaults standardUserDefaults] setBool:YES forKey:str];
}
+ (void) addProductFunctionMap:(NSString *)productId func:(enum USER_FUNCTION)func
{
    std::string str = [productId cStringUsingEncoding:NSASCIIStringEncoding];
    std::map<std::string, enum USER_FUNCTION>::iterator it = gProductFuncMap.find(str);
    if(it == gProductFuncMap.end())
    {
        gProductFuncMap[str] = func;
    }
    gFuncProductMap[func] = str;
}
@end

@implementation SEProductCategory
@synthesize description;
@synthesize type;
- (void)dealloc
{
    [description release];
    [super dealloc];
}
@end

@implementation SEProduct
@synthesize productId;
@synthesize productDescript1;
@synthesize productDescript2;
@synthesize productType;
@synthesize categoryType;
@synthesize productPrice;
@synthesize func;
@synthesize imageName;
@synthesize startDate;
@synthesize endDate;
@synthesize contentNames;
- (void) dealloc
{
    [productId release];
    [productDescript1 release];
    [productDescript2 release];
    [imageName release];
    [startDate release];
    [endDate release];
    [contentNames release];
    [super dealloc];
}
- (BOOL) isDiscount
{
    return startDate != nil;
}
- (int) getStarNum: (NSString*)str
{
    int num = 0;
    for(NSUInteger i = 0 ; i < str.length ; i++)
    {
        unichar c = [str characterAtIndex:i];
        if(c == '*')
        {
            num++;
        }
    }
    return num;
}
-(NSArray*) getStringSepByStar: (NSString*)str
{
    NSArray* strArray = [str componentsSeparatedByString:@"*"];
    NSMutableArray* newArray = [NSMutableArray array];
    for(int i = 0 ; i < strArray.count ; i++)
    {
        NSString* str = [strArray objectAtIndex:i];
        if([SEUtil isWhitespaceLine:str] == NO)
        {
            [newArray addObject:str];
        }
    }
    return newArray;
    /*
    NSUInteger start = UINT_MAX;
    NSUInteger end = UINT_MAX;
    NSArray* array = [NSArray array];
    for(NSUInteger i = 0 ; i < str.length ; i++)
    {
        unichar c = [str characterAtIndex:i];
        if(c == '*')
        {
            if(start == UINT_MAX)
            {
                start = i;
            }
            else
            {
                end = i;    
            }
            if(start != UINT_MAX && end != UINT_MAX)
            {
                NSRange r;
                r.location = start;
                r.length = end;
                NSString* newStr = [str substringWithRange:r];
                array = [array arrayByAddingObject:newStr];
                start = end;
                NSLog(@"str = %@", newStr);
            }
        }
    }
    if(start == end && start != UINT_MAX)
    {
        NSString* s = [str substringFromIndex:start];
        array = [array arrayByAddingObject:s];
        NSLog(@"str = %@", s);
    }
    return array;
     */
}
- (UIImage*) getIcon
{
    unichar firstChar = [imageName characterAtIndex:0];
    NSString* name = [imageName substringFromIndex:1];
    if(firstChar == '#')
    {
        UIImage* image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:name];
        return image;
    }
    else if(firstChar == '$')
    {
        PainterManager* pm = [PainterManager painterManager];
        BrushDefine* bd = [pm getBrushDefineByOutName:name];
        assert(bd != nil);
        NSArray* brushes = [pm getBrushesById:bd.brushID];
        NSString* first = [brushes objectAtIndex:0];
        const char* str = [first cStringUsingEncoding:NSUTF8StringEncoding];
        ppm_t brush = {0, 0, NULL};
        ppm_load(str, &brush);
        CGImageRef imageRef = [SEUtil create32BitsCGimageWithCopy:brush];//[SEUtil createCGImageWithCopy:brush];
        UIImage* image = [UIImage imageWithCGImage:imageRef];
        CGImageRelease(imageRef);
        ppm_kill(&brush);
        return image;
    }
    else if(firstChar == '*')
    {
        int starNum = [self getStarNum:imageName];
        int style = 0;
        if(starNum == 1)
        {
            style = [[PhotoFrameAppDelegate getProductManager] getTimeStyleIdByOutName:name];
        }
        else
        {
            NSArray* array = [self getStringSepByStar:imageName];
            NSString* first = [array objectAtIndex:0];
            first = [first substringFromIndex:1];
            style = [[PhotoFrameAppDelegate getProductManager] getTimeStyleIdByOutName:first];
        }
        UIImage* image = [[[PhotoFrameAppDelegate getViewNavigator] mFontLoader] getImage:@"am" style:style size:FONT_NORMAL_SIZE];
        return image;
    }
    return nil;
}

@end
struct CategoryTypeNameMap
{
    NSString* name;
    PRODUCT_CATEGORY_TYPE type;
};
static CategoryTypeNameMap gCategoryTypeNameMaps[] = {
    {@"BASIC", PRODUCT_CATEGORY_BASIC},
    {@"PLAY", PRODUCT_CATEGORY_PLAY},
    {@"BRUSH", PRODUCT_CATEGORY_BRUSH},
    {@"TIME_STYLE", PRODUCT_CATEGORY_TIME_STYLE}
    
};
static PRODUCT_CATEGORY_TYPE getCategory(NSString* cat)
{
    int count = sizeof(gCategoryTypeNameMaps) / sizeof(CategoryTypeNameMap);
    for(int i = 0 ; i < count ; i++)
    {
        if([gCategoryTypeNameMaps[i].name isEqualToString: cat])
            return gCategoryTypeNameMaps[i].type;
    }
    return PRODUCT_CATEGORY_NUM;
}
/////
struct ProductTypeNameMap
{
    NSString* name;
    PRODUCT_TYPE type;
};
static ProductTypeNameMap gProductTypeNameMaps[] = 
{
    {@"BASIC_SETTING",PRODUCT_BASIC_SETTING},
    {@"LEVEL_MAX", PRODUCT_LEVELUP_MAX},
    {@"ADD_MUSIC", PRODUCT_ADD_MUSIC},
    {@"ADD_IMAGE", PRODUCT_ADD_IMAGE},
    {@"BRUSH_SETTING",PRODUCT_BRUSH_SETTING},
    {@"BRUSH",PRODUCT_BRUSH},
    {@"TIME_STYLE",PRODUCT_TIME_STYLE}
};
static PRODUCT_TYPE getProductType(NSString* str)
{
    int count = sizeof(gProductTypeNameMaps) / sizeof(ProductTypeNameMap);
    for(int i = 0 ; i < count ; i++)
    {
        if([gProductTypeNameMaps[i].name isEqualToString: str])
            return gProductTypeNameMaps[i].type;
    }
    return PRODUCT_NUM;
}
///
@implementation SETimeStyle

@synthesize fontOutName;
@synthesize gettingWay;
@synthesize fontId;
@end
///
@implementation SEProductManager
- (SEProductCategory*) getCategory: (enum PRODUCT_CATEGORY_TYPE) ct
{
    for(SEProductCategory* c in mCategoryArray)
    {
        if(c.type == ct)
            return c;
    }
    return nil;
}
- (NSString*) getProductIdFromDiscountId: (NSString*)discountId
{
    if(discountId == nil)
        return nil;
    NSArray* strs = [discountId componentsSeparatedByString:@"_"];
    assert(strs.count == 2);
    return [strs objectAtIndex:0];
}
- (NSString*) getDiscountIdFromProductId: (NSString*) productId
{
    return [NSString stringWithFormat:@"%@_Discount", productId];
}
- (int) getCatetoryNum
{
    return mCategoryArray.count;
}
- (SEProductCategory*) getCategoryByIndex: (int)index
{
    return [mCategoryArray objectAtIndex: index];
}
- (SEProduct*) getProduct: (NSString*)productId
{
    for(SEProduct* p in mProductArray)
    {
        if([p.productId isEqualToString:productId])
            return p;
    }
    return nil;
}
- (NSArray*) getProductByFunc: (enum USER_FUNCTION) func
{
    NSMutableArray* retArray = [NSMutableArray array];
    for(SEProduct* p in mProductArray)
    {
        if(p.func == func)
            [retArray addObject:p];
    }
    return retArray;
}
- (SEProduct*) getProductWithDate: (NSString*) productId date: (NSDate*)date
{
    //NSString* dicountId = [self getDiscountIdFromProductId:productId];
    SEProduct* discount = [self getDiscountByDate:date discountId:productId];
    SEProduct* product = [self getProduct:productId];
    if(discount != nil)
    {
        return  discount;
    }
    else 
    {
        return product;
    }
}
- (SEProduct*) getDiscount: (NSString*) productId
{
    for(SEProduct* p in mDiscountArray)
    {
        if([p.productId isEqualToString:productId])
            return p;
    }
    return nil;
}
- (NSArray*) getAllDiscount
{
    NSMutableArray* newArray = [NSMutableArray array];
    for(SEProduct* p in mDiscountArray)
    {
        [newArray addObject:p];
    }
    return newArray;
}
- (NSDate*) stringToDate:(NSString*) str
{
    NSCalendar* currentCalendar = [NSCalendar currentCalendar];
    NSArray* startComponent = [str componentsSeparatedByString:@"/"];
    assert(startComponent.count == 3);
    int year = [[startComponent objectAtIndex:0] intValue];
    int month = [[startComponent objectAtIndex:1] intValue];
    int day = [[startComponent objectAtIndex:2] intValue];
    //unsigned unitFlags = NSYearCalendarUnit | NSMonthCalendarUnit |  NSDayCalendarUnit;
    NSDateComponents* dateComponents = [[NSDateComponents alloc] init];
    [dateComponents setYear:year];
    [dateComponents setMonth:month];
    [dateComponents setDay:day];
    NSDate* startDate = [currentCalendar dateFromComponents:dateComponents];
    [dateComponents release];
    return startDate;
}
- (SEProduct*) getDiscountByDate: (NSDate*)date discountId: (NSString*)productId
{
    SEProduct* currentProduct = nil;
    for(SEProduct* p in mDiscountArray)
    {
        if([p.productId isEqualToString:productId])
        {
            currentProduct = p;
            break;
        }
    }
    if(currentProduct == nil)
        return nil;
    NSString* startDateStr = currentProduct.startDate;
    NSString* endDateStr = currentProduct.endDate;
    //NSCharacterSet* cs = [NSCharacterSet characterSetWithCharactersInString:@"/"];
    NSDate* startDate = [self stringToDate:startDateStr];
    NSDate* endDate = [self stringToDate:endDateStr];
    NSTimeInterval t = [date timeIntervalSince1970];
    NSTimeInterval t1 = [startDate timeIntervalSince1970];
    NSTimeInterval t2 = [endDate timeIntervalSince1970];
    if(t >= t1 && t <= t2)
    {
        return currentProduct;
    }
    else
    {
        BOOL isBuyed = [SEUserDefaultManager isProductBuied:currentProduct.productId];
        if(isBuyed)
            return currentProduct;
        else
            return nil;
    }
}
- (int) getUserBuiedProductNum
{
    int count = 0;
    int productNum = [self getProductNum];
    for(int i = 0 ; i < productNum ; i++)
    {
        SEProduct* product = [self getProductByIndex:i];
        if([SEUserDefaultManager isProductBuied:product.productId])
        {
            count++;
        }
    }
    NSArray* discountArray = [self getAllDiscount];
    for(SEProduct* product in discountArray)
    {
        if([SEUserDefaultManager isProductBuied:product.productId])
        {
            count++;
        }
    }
    return count;
}
- (NSArray*) getDiscountByDate: (NSDate*)date withCategory: (enum PRODUCT_CATEGORY_TYPE) categoryType
{
    NSMutableArray* ret = [NSMutableArray array];
    for(SEProduct* p in mDiscountArray)
    {
        NSString* startDateStr = p.startDate;
        NSString* endDateStr = p.endDate;
        //NSCharacterSet* cs = [NSCharacterSet characterSetWithCharactersInString:@"/"];
        NSDate* startDate = [self stringToDate:startDateStr];
        NSDate* endDate = [self stringToDate:endDateStr];
        NSTimeInterval t = [date timeIntervalSince1970];
        NSTimeInterval t1 = [startDate timeIntervalSince1970];
        NSTimeInterval t2 = [endDate timeIntervalSince1970];
        if(t >= t1 && t <= t2 && p.categoryType == categoryType)
        {
            NSLog(@"has product discount = %@", p.productId);
            [ret addObject:p];
        }
    }
    return ret;
    
}
- (SEProduct*) getProductByIndex: (int) index
{
    return [mProductArray objectAtIndex:index];
}
- (int) getProductNum
{
    return mProductArray.count;
}
- (NSArray*) getProductAndDiscountArrayByCatetoryType:(enum PRODUCT_CATEGORY_TYPE) categoryType
{
    NSMutableArray* products = [self getProductArrayByCatetoryType:categoryType];
    NSMutableArray* newProducts = [NSMutableArray array];
    NSDate* date = [NSDate date];
    NSArray* allDiscount = [self getAllDiscount];
    for(int i = 0 ; i < allDiscount.count ; i++)
    {
        SEProduct* discount = [allDiscount objectAtIndex:i];
        NSString* discountId = discount.productId;
        NSString* productId = [self getProductIdFromDiscountId:discount.productId];
        SEProduct* product = [self getProduct:productId];
        discount = [self getDiscountByDate:date discountId:discount.productId];
        if(discount != nil && discount.categoryType == categoryType)
        {
            [newProducts addObject:discount];
            if(product != nil)
                [products removeObject:product];
        }
    }
    [newProducts addObjectsFromArray:products];
    return newProducts;
    /*
    for(SEProduct* p in products)
    {
        NSString* productId = p.productId;
        SEProduct* discount = [self getDiscount:productId];
        if(discount != nil)
        {
            discount = [self getDiscountByDate:date productId:discount.productId];
        }
        if(discount != nil)
        {
            [newProducts addObject:discount];
        }
        else
        {
            [newProducts addObject:p];
        }
    }
     */
    /*
    NSArray* array = [self getDiscountByDate:date withCategory:categoryType];
    NSMutableArray* newArray = [NSMutableArray array];
    for(SEProduct* p in array)
    {
        for(SEProduct* tmp in newProducts)
        {
            if([p.productId isEqualToString:tmp.productId] == NO)
            {
                [newArray addObject:p];
            }
        }
    }
    NSArray* ret = [newProducts arrayByAddingObjectsFromArray:newArray];
    return ret;
     */
}
- (NSArray*) getDiscountArrayByCatetoryType:(enum PRODUCT_CATEGORY_TYPE) categoryType
{
    NSMutableArray* array = [NSMutableArray arrayWithArray:[NSArray array]];
    for(SEProduct* product in mDiscountArray)
    {
        if(product.categoryType == categoryType)
        {
            [array addObject:product];
        }
    }
    return array;
}
- (NSMutableArray*) getProductArrayByCatetoryType:(enum PRODUCT_CATEGORY_TYPE) categoryType
{
    NSMutableArray* array = [NSMutableArray arrayWithArray:[NSArray array]];
    for(SEProduct* product in mProductArray)
    {
        if(product.categoryType == categoryType)
        {
            [array addObject:product];
        }
    }
    return array;
}
+ (int) findDelimit: (NSArray*)tokens
{
    for(int i = 0 ; i < tokens.count ; i++)
    {
        NSString* s = [tokens objectAtIndex:i];
        if([s isEqualToString:@"/"])
            return i;
    }
    return -1;
}
+ (NSString*) concateTokens : (NSArray*) tokens start: (int) start end: (int) end
{
    NSString* s = nil;
    for(int i = start ; i < end ; i++)
    {
        NSString* des = [tokens objectAtIndex:i];
        if(s == nil)
            s = des;
        else
        {
            s = [s stringByAppendingFormat:@" %@", des];
        }
    }
    return s;
}
- (void) loadTimeStyle: (NSString*) fileName
{
    SEProductManager* productManager = [[SEProductManager alloc] init];
    [productManager autorelease];
    NSString* data = [SEUtil readDataFromDocumentDir:fileName];
    if(data == nil)
    {
        data = [SEUtil readDataFromBundle:fileName];
    }
    if(data == nil)
    {
        NSLog(@"## can not find user default manager package : %@ ##\n", fileName);
        return;
    }
    NSArray* dataLines = [data componentsSeparatedByString:@"\n"];
    enum BLOCK_TYPE {NO_BLOCK, TIME_STYLE_BLOCK};
    BLOCK_TYPE blockType = NO_BLOCK;
    for(int i = 0 ; i < dataLines.count ; i++)
    {
        NSString* line = [dataLines objectAtIndex:i];
        if([SEUtil isWhitespaceLine:line])
            continue;
        line = [SEUtil stringTrim:line];
        NSUInteger len = line.length;
        unichar firstC = [line characterAtIndex:0];
        unichar lastC = [line characterAtIndex:len - 1];
        if(firstC == '#')
            continue;
        if(firstC == '[' && lastC == ']')
        {
            if([line isEqualToString:@"[TIMESTYLE]"])
            {
                blockType = TIME_STYLE_BLOCK;
            }
            continue;
        }
        
        NSCharacterSet* cs = [NSCharacterSet whitespaceAndNewlineCharacterSet];
        NSArray* tokens = [line componentsSeparatedByCharactersInSet:cs];
        NSArray* newTokens = [NSArray array];
        for(int i = 0 ; i < tokens.count ; i++)
        {
            NSString* s = [tokens objectAtIndex:i];
            if([SEUtil isWhitespaceLine:s] == NO)
                newTokens = [newTokens arrayByAddingObject:s];
        }
        tokens = newTokens;
        switch (blockType)
        {
            case TIME_STYLE_BLOCK:
            {
                NSString* outname = [tokens objectAtIndex:0];
                NSString* gettintway = [tokens objectAtIndex:2];
                NSString* fontId = [tokens objectAtIndex:1];
                SETimeStyle* style = [[SETimeStyle alloc] init];
                style.fontOutName = outname;
                style.gettingWay = [gettintway intValue];
                style.fontId = [fontId intValue];
                [mTimeStyleArray addObject:style];
                [style release];
            }
                break;
                
            default:
                break;
        }
    }
}
- (int) getProductDependenceNum
{
    return mProductDependenceArray.count;
}
- (NSString*) getProductDependenceMainName: (int)index
{
    if(index < 0 || index >= mProductDependenceArray.count)
        return nil;
    SEProductDepdence* d = [mProductDependenceArray objectAtIndex:index];
    return d.mProductID;
}
- (NSArray*) getProductDependenceAffectedProduct: (int) index
{
    if(index < 0 || index >= mProductDependenceArray.count)
        return nil;
    SEProductDepdence* d = [mProductDependenceArray objectAtIndex:index];
    NSMutableArray* array = [NSMutableArray array];
    for(int i = 0 ; i < d.mAffectProduct.count ; i++)
    {
        [array addObject:[d.mAffectProduct objectAtIndex:i]];
    }
    return array;
}
- (NSArray*) getProductDependenceByProductId : (NSString*) productId
{
    NSMutableArray* retArray = [NSMutableArray array];
    for(int i = 0 ;i < mProductDependenceArray.count ; i++)
    {
        SEProductDepdence* d = [mProductDependenceArray objectAtIndex:i];
        if([d.mProductID isEqualToString:productId])
        {
            for(int j = 0 ; j < d.mAffectProduct.count ; j++)
            {
                [retArray addObject:[d.mAffectProduct objectAtIndex:j]];
            }
            return retArray;
        }
    }
    return retArray;
}

- (BOOL) isProductTypeOneTimeBuy: (enum PRODUCT_TYPE)type
{
    for(int i = 0 ; i < mProductOneTimeBuyArray.count ; i++)
    {
        NSNumber* num = [mProductOneTimeBuyArray objectAtIndex:i];
        if([num intValue] == type)
            return YES;
    }
    return NO;
}
+ (SEProductManager*) createFromFile:(NSString*) fileName
{
    SEProductManager* productManager = [[SEProductManager alloc] init];
    [productManager autorelease];
    NSString* data = [SEUtil readDataFromDocumentDir:fileName];
    if(data == nil)
    {
        data = [SEUtil readDataFromBundle:fileName];
    }
    if(data == nil)
    {
        NSLog(@"## can not find user default manager package : %@ ##\n", fileName);
        return nil;
    }
    NSArray* dataLines = [data componentsSeparatedByString:@"\n"];
    enum BLOCK_TYPE {NO_BLOCK, CATEGORY_BLOCK, PRODUCT_BLOCK, PRODUCT_FUNCTION_MAP_BLOCK, PRODUCT_ONETIME_BUY_BLOCK, DISCOUNT_BLOCK, PRODUCT_DEP_BLOCK};
    BLOCK_TYPE blockType = NO_BLOCK;
    for(int i = 0 ; i < dataLines.count ; i++)
    {
        NSString* line = [dataLines objectAtIndex:i];
        if([SEUtil isWhitespaceLine:line])
            continue;
        line = [SEUtil stringTrim:line];
        NSUInteger len = line.length;
        unichar firstC = [line characterAtIndex:0];
        unichar lastC = [line characterAtIndex:len - 1];
        if(firstC == '#')
            continue;
        if(firstC == '[' && lastC == ']')
        {
            if([line isEqualToString:@"[CATEGORY]"])
            {
                blockType = CATEGORY_BLOCK;
            }
            else if([line isEqualToString:@"[PRODUCT]"])
            {
                blockType = PRODUCT_BLOCK;
            }
            else if([line isEqualToString:@"[PRODUCT_FUNCTION_MAP]"])
            {
                blockType = PRODUCT_FUNCTION_MAP_BLOCK;
            }
            else if([line isEqualToString:@"[PRODUCT_ONE_TIME_BUY]"])
            {
                blockType = PRODUCT_ONETIME_BUY_BLOCK;
            }
            else if([line isEqualToString:@"[DISCOUNT]"])
            {
                blockType = DISCOUNT_BLOCK;
            }
            else if([line isEqualToString:@"[PRODUCT_DEP]"])
            {
                blockType = PRODUCT_DEP_BLOCK;
            }
            continue;
        }
        
        NSCharacterSet* cs = [NSCharacterSet whitespaceAndNewlineCharacterSet];
        NSArray* tokens = [line componentsSeparatedByCharactersInSet:cs];
        NSArray* newTokens = [NSArray array];
        for(int i = 0 ; i < tokens.count ; i++)
        {
            NSString* s = [tokens objectAtIndex:i];
            if([SEUtil isWhitespaceLine:s] == NO)
                newTokens = [newTokens arrayByAddingObject:s];
        }
        tokens = newTokens;
        switch (blockType)
        {
            case CATEGORY_BLOCK:
            {
                NSString* catTypeStr = [tokens objectAtIndex:0];
                SEProductCategory* category = [[SEProductCategory alloc] init];
                category.type = getCategory(catTypeStr);
                assert(category.type < PRODUCT_CATEGORY_NUM);
                NSString* des = [SEProductManager concateTokens:tokens start:1 end:tokens.count];
                category.description = des;
                [productManager->mCategoryArray addObject:category];
                [category release];
                NSLog(@"category type = %@", catTypeStr);
                NSLog(@"category des = %@", des);
            }
                break;
            case PRODUCT_BLOCK:
            {
                assert(tokens.count >= 2);
                NSString* productType = [tokens objectAtIndex:0];
                SEProduct* product = [[SEProduct alloc] init];
                product.productType = getProductType(productType);
                assert(product.productType < PRODUCT_NUM);
                NSString* func = [tokens objectAtIndex:1];
                product.func = getUserFunctionByFuncName(func);
                
                
                
                NSString* productId = [tokens objectAtIndex:2];
                product.productId = productId;
                
                NSString* imageName = [tokens objectAtIndex:3];
                product.imageName = imageName;
                
                NSString* contentName = [tokens objectAtIndex:4];
                product.contentNames = [product getStringSepByStar:contentName];
                NSString* catetory = [tokens objectAtIndex:5];
                product.categoryType = getCategory(catetory);
                assert(product.categoryType < PRODUCT_CATEGORY_NUM);
                NSString* price = [tokens objectAtIndex:6];
                product.productPrice = [price floatValue];
                
                int delimitIndex = [SEProductManager findDelimit:tokens];
                if(delimitIndex == -1)
                {
                    NSString* s = [SEProductManager concateTokens:tokens start:7 end:tokens.count];
                    product.productDescript1 = s;
                }
                else
                {
                    NSString* s1 = [SEProductManager concateTokens:tokens start:7 end:delimitIndex];
                    product.productDescript1 = s1;
                    NSString* s2 = [SEProductManager concateTokens:tokens start:delimitIndex + 1 end:tokens.count];
                    product.productDescript2 = s2;
                }
                [productManager->mProductArray addObject:product];
                [product release];
                NSLog(@"product type = %@", productType);
                NSLog(@"func = %@", func);
                NSLog(@"product id = %@", productId);
                NSLog(@"image name = %@", imageName);
                NSLog(@"catetory = %@", catetory);
                NSLog(@"price = %@", price);
                NSLog(@"cotent = %@", contentName);
                NSLog(@"des1 = %@", product.productDescript1);
                NSLog(@"des2 = %@", product.productDescript2);
            }
                break;
            case PRODUCT_FUNCTION_MAP_BLOCK:
            {
                assert(tokens.count == 2);
                NSString* func = [tokens objectAtIndex:0];
                NSString* productId = [tokens objectAtIndex:1];
                enum USER_FUNCTION f = getUserFunctionByFuncName(func);
                [SEUserDefaultManager addProductFunctionMap:productId func:f];
            }
                break;
            case PRODUCT_ONETIME_BUY_BLOCK:
            {
                assert(tokens.count == 1);
                NSString* productType = [tokens objectAtIndex:0];
                NSNumber* type = [NSNumber numberWithInt:getProductType(productType)];
                [productManager->mProductOneTimeBuyArray addObject:type];
            }
                break;
            case DISCOUNT_BLOCK:
            {
                assert(tokens.count >= 2);
                NSString* productType = [tokens objectAtIndex:0];
                SEProduct* product = [[SEProduct alloc] init];
                product.productType = getProductType(productType);
                assert(product.productType < PRODUCT_NUM);
                NSString* func = [tokens objectAtIndex:1];
                product.func = getUserFunctionByFuncName(func);
                
                
                
                NSString* productId = [tokens objectAtIndex:2];
                product.productId = productId;
                
                NSString* imageName = [tokens objectAtIndex:3];
                product.imageName = imageName;
                
                NSString* contentName = [tokens objectAtIndex:4];
                product.contentNames = [product getStringSepByStar:contentName];

                NSString* catetory = [tokens objectAtIndex:5];
                product.categoryType = getCategory(catetory);
                assert(product.categoryType < PRODUCT_CATEGORY_NUM);
                NSString* price = [tokens objectAtIndex:6];
                product.productPrice = [price floatValue];
                
                NSString* startDate = [tokens objectAtIndex:7];
                product.startDate = startDate;
                
                NSString* endDate = [tokens objectAtIndex:8];
                product.endDate = endDate;
                
                int delimitIndex = [SEProductManager findDelimit:tokens];
                if(delimitIndex == -1)
                {
                    NSString* s = [SEProductManager concateTokens:tokens start:9 end:tokens.count];
                    product.productDescript1 = s;
                }
                else
                {
                    NSString* s1 = [SEProductManager concateTokens:tokens start:9 end:delimitIndex];
                    product.productDescript1 = s1;
                    NSString* s2 = [SEProductManager concateTokens:tokens start:delimitIndex + 1 end:tokens.count];
                    product.productDescript2 = s2;
                }
                [productManager->mDiscountArray addObject:product];
                [product release];
                NSLog(@"product type = %@", productType);
                NSLog(@"func = %@", func);
                NSLog(@"product id = %@", productId);
                NSLog(@"image name = %@", imageName);
                NSLog(@"catetory = %@", catetory);
                NSLog(@"price = %@", price);
                NSLog(@"startdate = %@", startDate);
                NSLog(@"enddate = %@", endDate);
                NSLog(@"content = %@", contentName);
                NSLog(@"des1 = %@", product.productDescript1);
                NSLog(@"des2 = %@", product.productDescript2);
            }
                break;
            case PRODUCT_DEP_BLOCK:
            {
                SEProductDepdence* dependence = [[SEProductDepdence alloc] init];
                NSString* productID = [tokens objectAtIndex:0];
                NSLog(@"dep product id = %@", productID);
                dependence.mProductID = productID;
                for(int i = 1 ; i < tokens.count ; i++)
                {
                    NSString* str = [tokens objectAtIndex:i];
                    [dependence.mAffectProduct addObject:str];
                    NSLog(@"dep affect product id = %@", str);
                }
                [productManager->mProductDependenceArray addObject:dependence];
            }
                break;
            default:
                break;
        }
        
    }
    [productManager loadTimeStyle:@"time_style_define.txt"];
    return productManager;
}
- (NSString*) getDiscountIdByBrushOutName: (NSString*)outName
{
    for(SEProduct* product in mDiscountArray)
    {
        if(product.productType == PRODUCT_BRUSH)
        {
            NSArray* contents = product.contentNames;
            if([contents containsObject:outName])
            {
                return product.productId;
            }
        }
    }
    return nil;
}
- (NSString*) getProductIdByBrushOutName: (NSString*)outName
{
    for(SEProduct* product in mProductArray)
    {
        if(product.productType == PRODUCT_BRUSH)
        {
            NSArray* contents = product.contentNames;
            NSString* str = outName;//[NSString stringWithFormat:@"*%@", outName];
            if([contents containsObject:str])
            {
                return product.productId;
            }
        }
    }
    return nil;
}

- (NSString*) getProductIdByTimeStyleOutName: (NSString*) outName
{
    for(SEProduct* product in mProductArray)
    {
        if(product.productType == PRODUCT_TIME_STYLE)
        {
            /*
            NSString* name = product.imageName;
            NSArray* nameArray = [product getStringSepByStar:name];
            for(int i = 0 ; i < nameArray.count ; i++)
            {
                NSString* name = [nameArray objectAtIndex:i];
                NSString* str = [name substringFromIndex:1];
                NSLog(@"name = %@, outName = %@", str, outName);
                if([str isEqualToString:outName])
                {
                    return product.productId;
                }
            }
             */
            if([product.contentNames containsObject:outName])
                return product.productId;
        }
    }
    return nil;
}
- (int) getTimeStyleIdByOutName: (NSString*) outName
{
    for(int i = 0 ; i < mTimeStyleArray.count ; i++)
    {
        SETimeStyle* ts = [mTimeStyleArray objectAtIndex:i];
        if([ts.fontOutName isEqualToString:outName])
        {
            return ts.fontId;
        }
    }
    assert(0);
    return -1;
}
- (SETimeStyle*) getTimeStyleById: (int)styleId
{
    for(int i = 0 ; i < mTimeStyleArray.count ; i++)
    {
        SETimeStyle* ts = [mTimeStyleArray objectAtIndex:i];
        if(ts.fontId == styleId)
            return ts;
    }
    return nil;
}
- (id) init
{
    self = [super init];
    if(self)
    {
        mCategoryArray = [[NSMutableArray arrayWithArray:[NSArray array]] retain];
        mProductArray = [[NSMutableArray arrayWithArray:[NSArray array]] retain];
        mTimeStyleArray = [[NSMutableArray arrayWithArray:[NSArray array]] retain];
        mProductOneTimeBuyArray = [[NSMutableArray arrayWithArray:[NSArray array]] retain];
        mDiscountArray  = [[NSMutableArray array] retain];
        mProductDependenceArray = [[NSMutableArray array] retain];
    }
    return self;
}
- (void)dealloc
{
    [mProductArray release];
    [mCategoryArray release];
    [mTimeStyleArray release];
    [mProductOneTimeBuyArray release];
    [mDiscountArray release];
    [mProductDependenceArray release];
    [super dealloc];
}
@end
