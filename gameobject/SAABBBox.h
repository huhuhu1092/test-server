#ifndef SAABBBOX_H
#define SAABBBOX_H
class SAABBBox
{
public:
    SAABBBox(const SVector3& min, const SVector3& max );
    SAABBBox();
    void set(const SVector3& min, const SVector3& max);
    SVector3 getMin();
    SVector3 getMax();
    bool contain(const SVector3& point);
private:
    SVector3 mMin;
    SVector3 mMax;
};
#endif
