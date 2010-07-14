#ifndef SE_BOUNDINGVOLUME_H
#define SE_BOUNDINGVOLUME_H
class SE_BuffferInput;
class SE_BufferOutput;
class SE_BoundingVolume
{
public:
    enum BV_TYPE {SPHERE, AABB, OBB};
    SE_BoundingVolume();
    virtual ~SE_BoundingVolume();
    virtual void write(SE_BufferOutput& output);
    virtual void read(SE_BufferInput& input);
};
#endif
