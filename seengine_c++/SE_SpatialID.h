#ifndef SE_SPATIALID_H
#define SE_SPATIALID_H
class SE_SpatialID
{
public:
    static SE_SpatialID* create(const char* id);
    friend bool operator==(const SE_SpatialID& left, const SE_SpatialID& right);
    friend bool operator<(const SE_SpatialID& left, const SE_SpatialID& right);
    friend bool operator>(const SE_SpatialID& left, const SE_SpatialID& right);
    ~SE_SpatialID();
private:
    SE_SpatialID();
    SE_SpatialID(const char* id);
    SE_SpatialID(const SE_SpatialID&);
    SE_SpatialID& operator=(const SE_SpatialID&);
private:
    struct SE_SpatialIDImpl;
    SE_SpatialIDImpl* mImpl;
};
#endif
