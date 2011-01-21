#ifndef SE_DYNAMICARRAY_H
#define SE_DYNAMICARRAY_H
template <typename T>
class SE_DynamicArray
{
public:
    enum {NO_ERROR, OVERFLOW, INDEX_ERROR};
    //size must > 1
    SE_DynamicArray(int size, int maxSize);
    ~SE_DynamicArray();
    T& operator[](int index);
    const T& operator[](int index) const;
    int size() const
    {
        return mSize;
    }
    void setInvalidValue(const T& v)
    {
        mInvalid = v;
    }
    T getInvalidValue() const
    {
        return mInvalid;
    }
    int getError() const
    {
        return mError;
    }
    void expand();
private:
    T* mArray;
    int mSize;
    int mMaxSize;
    T mInvalid;
    int mError;
};
template <typename T>
SE_DynamictArray<T>::SE_DynamicArray(int size) : mArray(NULL), mSize(0), mMaxSize(0), mError(0)
{
    if(size <= 1)
        return;
    mArray = new T[size];
    mSize = size;
    mMaxSize = size;
}
template <typename T>
SE_DynamictArray<T>::~SE_DynamicArray()
{
    if(mArray)
        delete[] mArray;
}
template <typename T>
void SE_DynamicArray<T>::expand()
{
    int size = mSize + (mSize * 3) / 4;
    if(size > mMaxSize)
    {
        mError = OVERFLOW;
        return;
    }
    T* newArray = new T[size];
    if(!newArray)
    {
        mError = OVERFLOW;
        return;
    }
    for(int i = 0 ; i < mSize ; i++)
    {
        newArray[i] = mArray[i];
    } 
    T* oldArray = mArray;
    mArray = newArray;
    mSize = size;
    delete[] oldArray;
}
template <typename T>
T& SE_DynamictArray<T>::operator[](int index)
{
    if(index >= 0 && index < mSize)
    {
        return mArray[index];
    }
    else
    {
        mError = INDEX_ERROR;
        return mInvalid;
    }
}
template <typename T>
const T& SE_DynamictArray<T>::operator[](int index) const
{
    if(index < 0 || index >= mSize)
    {
        mError = INDEX_ERROR;
        return mInvalid;
    }
    return mArray[index];
}

#endif
