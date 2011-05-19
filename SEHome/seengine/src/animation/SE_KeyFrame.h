#ifndef SE_KEYFRAME_H
#define SE_KEYFRAME_H
#include "SE_Quat.h"
#include "SE_Vector.h"
#include "SE_TimeKey.h"
#include <list>
#include <vector>
struct SE_Transform
{
    SE_Quat rotate;
    SE_Vector3f translate;
    SE_Vector3f scale;
};

//T is the data type
//SE_KeyFrame is a container
//the content is the pair of SE_TimeKey and the data T
//if the data is a type of pointer, SE_KeyFrame will own it and will delete the pointer
//when SE_KeyFrame destructed. 
template <typename T>
class SE_KeyFrame
{
public:
    SE_KeyFrame()
    {
    }
    SE_TimeKey key;// this the number of the  time sequence
    T data;
};
template <typename T>
class SE_KeyFrame<T*>
{
public:
	SE_KeyFrame()
	{
		data = NULL;
	}
	~SE_KeyFrame()
	{
        if(data)
			delete data;
	}
    SE_TimeKey key;
	T* data;
};
template <typename T>
struct SE_KeyFrameCompare
{
    bool operator()(const SE_KeyFrame<T>* keyframe)
    {
        return false;
    }
};
template <typename T>
struct SE_KeyFrameSortCompare
{
	bool operator()(const SE_KeyFrame<T>* left, const SE_KeyFrame<T>* right) const
	{
		return left->key < right->key;
	}
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
		mKeyFrameSequence.sort(SE_KeyFrameSortCompare<T>());
    }
    void setKeyFrame(SE_KeyFrame<T>* kf);
    //if key is not in key frame sequnce , it will return the 
    //first key frame
    SE_KeyFrame<T>* getKeyFrame(const SE_TimeKey& key) const;
    int getKeyFrameNum() const
    {
        return mKeyFrameSequence.size();
    }
	std::vector<SE_TimeKey> getKeys() const;
    SE_KeyFrame<T>* find(SE_KeyFrameCompare<T> compare) const;
	void remove_if(SE_KeyFrameCompare<T> compare);
	//return the nearest time key in mKeyFrameSequence which is less than timeKey
	//if not found , return SE_TimeKey::INVALID
	SE_TimeKey getNearestLowerTimeKey(const SE_TimeKey& timeKey) const;
	SE_TimeKey getNearestHigherTimeKey(const SE_TimeKey& timeKey) const;
    SE_TimeKey getMaxKey() const;
	SE_TimeKey getMinKey() const;
	void clear()
	{
		typename _KeyFrameSequence::iterator it ;
        for(it = mKeyFrameSequence.begin() ; it != mKeyFrameSequence.end() ; it++)
        {
            delete *it;
        }
	}
private:
    typedef std::list<SE_KeyFrame<T>*> _KeyFrameSequence;
    _KeyFrameSequence mKeyFrameSequence;
};
template <typename T>
SE_KeyFrame<T>* SE_KeyFrameSequence<T>::find(SE_KeyFrameCompare<T> compare) const
{
    typename _KeyFrameSequence::const_iterator it ;
    for(it = mKeyFrameSequence.begin() ; it != mKeyFrameSequence.end() ; it++)
    {
        if(compare(*it))
            return *it;
    }
    return NULL;
}
template <typename T>
void SE_KeyFrameSequence<T>::remove_if(SE_KeyFrameCompare<T> compare)
{
    typename _KeyFrameSequence::iterator it ;
	typename _KeyFrameSequence::iterator removedIt;
    for(it = mKeyFrameSequence.begin() ; it != mKeyFrameSequence.end() ; it++)
    {
        if(compare(*it))
		{
            removedIt = it;
			break;
		}
    }
	if(removedIt != mKeyFrameSequence.end())
	{
		mKeyFrameSequence.erase(removedIt);
	}
}
template <typename T>
SE_KeyFrameSequence<T>::SE_KeyFrameSequence()
{}
template <typename T>
SE_KeyFrameSequence<T>::~SE_KeyFrameSequence()
{
    clear();
}
template <typename T>
std::vector<SE_TimeKey> SE_KeyFrameSequence<T>::getKeys() const
{
	std::vector<SE_TimeKey> keys(mKeyFrameSequence.size());
    typename _KeyFrameSequence::const_iterator it ;
	int i = 0 ;
    for(it = mKeyFrameSequence.begin() ; it != mKeyFrameSequence.end() ; it++)
    {
        keys[i++] = (*it)->key;
    }	
	return keys;
}
template <typename T>
SE_KeyFrameSequence<T>::SE_KeyFrameSequence(const SE_KeyFrameSequence<T>& right)
{
    typename _KeyFrameSequence::iterator it ;
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
	clear();
    mKeyFrameSequence.clear();
    typename _KeyFrameSequence::iterator it ;
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
void SE_KeyFrameSequence<T>::setKeyFrame(SE_KeyFrame<T>* kf)
{
    typename _KeyFrameSequence::iterator it ;
	SE_KeyFrame<T>* old = NULL;
    for(it = mKeyFrameSequence.begin() ; it != mKeyFrameSequence.end() ; it++)
    {
        if((*it)->key == kf->key)
        {
			old = *it;
            *it = kf;
			delete old;
            return;
        }
    }
    mKeyFrameSequence.push_back(kf);
}
template <typename T>
SE_KeyFrame<T>* SE_KeyFrameSequence<T>::getKeyFrame(const SE_TimeKey& key) const
{
    typename _KeyFrameSequence::const_iterator it ;
    for(it = mKeyFrameSequence.begin() ; it != mKeyFrameSequence.end() ; it++)
    {
        if((*it)->key == key)
        {
            return *it;
        }
    }
    return NULL;
}
template <typename T>
SE_TimeKey SE_KeyFrameSequence<T>::getMaxKey() const
{
	if(mKeyFrameSequence.empty())
		return SE_TimeKey::INVALID;
	typename _KeyFrameSequence::const_reverse_iterator it = mKeyFrameSequence.rbegin();
	return (*it)->key;
}
template <typename T>
SE_TimeKey SE_KeyFrameSequence<T>::getMinKey() const
{
	if(mKeyFrameSequence.empty())
		return SE_TimeKey::INVALID;
	typename _KeyFrameSequence::const_iterator it = mKeyFrameSequence.begin();
	return (*it)->key;	
}
template <typename T>
SE_TimeKey SE_KeyFrameSequence<T>::getNearestHigherTimeKey(const SE_TimeKey& timeKey) const
{
	typename _KeyFrameSequence::const_iterator it;
	if(mKeyFrameSequence.empty())
		return SE_TimeKey::INVALID;
	for(it = mKeyFrameSequence.begin() ; it != mKeyFrameSequence.end() ; it++)
	{
		if((*it)->key >= timeKey)
		{
			return (*it)->key;
		}
	}
	return SE_TimeKey::INVALID;
}
template <typename T>
SE_TimeKey SE_KeyFrameSequence<T>::getNearestLowerTimeKey(const SE_TimeKey& timeKey) const
{
	typename _KeyFrameSequence::const_iterator it;
	typename _KeyFrameSequence::const_iterator curr;
	bool found = false;
	if(mKeyFrameSequence.empty())
		return SE_TimeKey::INVALID;
	for(it = mKeyFrameSequence.begin() ; it != mKeyFrameSequence.end() ; it++)
	{
		if((*it)->key > timeKey)
		{
			curr = it;
			found = true;
			break;
		}
	}
	if(found)
	{
	    if(curr == mKeyFrameSequence.begin())
		{
			return SE_TimeKey::INVALID;
		}
		else
		{
			typename _KeyFrameSequence::const_iterator keyIt = curr--;
			return (*keyIt)->key;
		}
	}
	else
	{
	    SE_TimeKey maxKey = getMaxKey();
		if(timeKey >= maxKey)
			return maxKey;
		else
		    return SE_TimeKey::INVALID;
	}
}
#endif
