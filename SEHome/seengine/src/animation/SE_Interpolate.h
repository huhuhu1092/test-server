#ifndef SE_INTERPOLATE_H
#define SE_INTERPOLATE_H
class SE_Interpolate
{
public:
    virtual ~SE_Interpolate() {}
    virtual float calc(float input);
	virtual SE_Interpolate* clone();
};
#endif
