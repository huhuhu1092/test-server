#ifndef SE_DELAYDESTROY_H
#define SE_DELAYDESTROY_H
class SE_DelayDestroy
{
public:
    virtual ~SE_DelayDestroy() {}
    virtual void destroy() = 0;
    bool operator==(const SE_DelayDestroy& right)
    {
        return eq(right);
    }
    bool operator !=(const SE_DelayDestroy& right)
    {
        return !this->operator==(right);
    }
protected:
    virtual bool eq(const SE_DelayDestroy& right) = 0;
};
template <typename T>
class SE_DelayDestroyPointer : public SE_DelayDestroy
{
public:
    SE_DelayDestroyPointer(T* p)
    {
        pointer = p;
    }
    void destroy()
    {
        delete pointer;
    }
protected:
    bool eq(const SE_DelayDestroy& right)
    {
        return pointer == right.pointer;
    }
private:
    T* pointer;
};
template <typename T>
class SE_DelayDestroyArray : public SE_DelayDestroy
{
public:
    SE_DelayDestroyArray(T* p)
    {
        pointer = p;
    }
    void destroy()
    {
        if(pointer)
        delete[] pointer;
    }
protected:
    bool eq(const SE_DelayDestroy& right)
    {
        return pointer == right.pointer;
    }

private:
    T* pointer;
};
#endif
