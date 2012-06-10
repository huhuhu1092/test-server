//
//  SE_TimeProp.mm
//  PhotoFrame
//
//  Created by 陈勇 on 12-4-26.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SE_TimeProp.h"

double getCurrentTime()
{
    NSDate* date = [NSDate date];
    return [date timeIntervalSince1970];
}
double getInterval(double from, double to)
{
    if(from < to)
    {
        NSDate* date1 = [NSDate dateWithTimeIntervalSince1970:from];
        NSDate* date2 = [NSDate dateWithTimeIntervalSince1970:to];
        return [date2 timeIntervalSinceDate:date1];
    }
    else
    {
        NSDate* date2 = [NSDate dateWithTimeIntervalSince1970:from];
        NSDate* date1 = [NSDate dateWithTimeIntervalSince1970:to];
        return [date2 timeIntervalSinceDate:date1];

    }
}