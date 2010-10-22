#ifndef SE_KEYFRAME_H
#define SE_KEYFRAME_H
#include "SE_Quat.h"
#include "SE_Vector.h"
#include <list>
struct SE_Transfrom
{
    SE_Quat rotate;
    SE_Vector3f translate;
    SE_Vector3f scale;
};
template <typename T>
struct SE_KeyRange
{
    unsigned int first;
    unsigned int second;
    T calculate()
    {
        return T();
    }
};

template <typename T>
struct SE_KeyFrame
{
    SE_KeyFrame()
    {
        key = 0;
    }
    unsigned int key;// this the number of the  time sequence
    T data;
};
template <typename T>
class SE_KeyFrameSequence
{
public:
    SE_KeyFrameSequence();
    ~SE_KeyFrameSequence();
    SE_KeyFrameSequence(const SE_KeyFrameSequence&);
    SE_KeyFrameSequence& operator=(const SE_KeyFrameSequence&);
    void addKeyFrame(SE_KeyFrame<T>* kf)
    {
        mKeyFrameSequence.push_back(kf);
    }
    void setKeyFrame(unsigned int key, SE_KeyFrame<T>* kf);
    //if key is not in key frame sequnce , it will return the 
    //first key frame
    SE_KeyFrame<T> getKeyFrame(unsigned int key);
    int getKeyFrameNum()
    {
        return mKeyFrameSequence.size();
    }
private:

private:

    std::vector<SE_KeyRange> mKeys;
    typedef std::list<SE_KeyFrame<T>*> _KeyFrameSequence;
    _KeyFrameSequence mKeyFrameSequence;
};
template <typename T>
SE_KeyFrameSequence<T>::SE_KeyFrameSequence()
{}
template <typename T>
SE_KeyFrameSequence<T>::~SE_KeyFrameSequence()
{
    _KeyFrameSequence::iterator it ;
    for(it = mKeyFrameSequence.begin() ; it != mKeyFrameSequence.end() ; it++)
    {
        delete *it;
    }

}
template <typename T>
SE_KeyFrameSequence<T>::SE_KeyFrameSequence(const SE_KeyFrameSequence<T>& right)
{
    _KeyFrameSequence::iterator it ;
    for(it = right.begin() ; it != right.end() ; it++)   
    {
        SE_KeyFrame<T>* f = *it;
        SE_KeyFrame<T>* nf = new SE_KeyFrame<T>;
        *nf = *f;
        mKeyFrameSequence.push_back(nf);
    }
}
template <typename T>
SE_KeyFrameSequence<T>& SE_KeyFrameSequence<T>::operator=(const SE_KeyFrameSequence<T>& right)
{
    if(this == &right)
    {
        return;
    }
    mKeyFrameSequence.clear();
    _KeyFrameSequence::iterator it ;
    for(it = right.begin() ; it != right.end() ; it++)   
    {
        SE_KeyFrame<T>* f = *it;
        SE_KeyFrame<T>* nf = new SE_KeyFrame<T>;
        *nf = *f;
        mKeyFrameSequence.push_back(nf);
    }
    return *this;
}    
template <typename T>
void SE_KeyFrameSequence<T>::setKeyFrame(const SE_KeyFrame<T>& kf)
{
    _KeyFrameSequence::iterator it ;
    for(it = mKeyFrameSequence.begin() ; it != mKeyFrameSequence.end() ; it++)
    {
        if((*it)->key == kf.key)
        {
            (*it)->data = kf.data
            (*it)->key = kf.key;
            return;
        }
    }
    SE_KeyFrame<T>* newkf = new SE_KeyFrame<T>;
    newkf->key = kf.key;
    newkf->data = kf.data;
    mKeyFrameSequence.push_back(newkf);
}
template <typename T>
SE_KeyFrame<T>* SE_KeyFrameSequence::getKeyFrame(unsigned int key)
{
    _KeyFrameSequence::iterator it ;
    for(it = mKeyFrameSequence.begin() ; it != mKeyFrameSequence.end() ; it++)
    {
        if((*it)->key == key)
        {
            return *it;
        }
    }
    it = mKeyFrameSequence.begin();
    return *it;
}
#endif
