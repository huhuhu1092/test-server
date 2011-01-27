#ifndef SE_TREESTRUCTMANAGER_H
#define SE_TREESTRUCTMANAGER_H
// tree struct is used to manage tree struct
// and each tree node has a pointer, we can use
// an index to get the pointer, this index is the ID
#include "SE_ID.h"
#include "SE_Application.h"
#include "SE_DelayDestroy.h"
#include "SE_DynamicArray.h"
#include <list>
#include <vector>
/*
 * SE_TreeStructManager's T must derived from SE_TreeStruct or SE_ListStruct
 * */
template <typename T>
class SE_TreeStructManager
{
public:
    enum {MAX_SIZE = 2000000};
    enum {RELEASE_DELAY, RELEASE_NO_DELAY};
    enum {NO_ERROR, NO_SPACE, DUP_POINTER, EXCEPT_ERROR};
    SE_TreeStructManager(int size = 1000);
    ~SE_TreeStructManager();
    T* find(const SE_TreeStructID& id) const;
    T* remove(const SE_TreeStructID& id) const;
    void release(T* p, int delay = RELEASE_DELAY);
    void release(const SE_TreeStructID& id, int delay = RELEASE_DELAY);
    //return is child's id
    SE_TreeStructID add(const SE_TreeStructID& parent, T* child);
    T* getParent(const SE_TreeStructID& id) const;
    std::vector<T*> getChildren(const SE_TreeStructID& id) const;
    int getError() const
    {
        return mError;
    }
    void check();
    int getAllocatedNodeNum() const;
    T* clone(const SE_TreeStructID& id);
private:
    struct _Node
    {
        T* data;
        bool alloc;
        SE_TreeStructID parent;
        _Node()
        {
            data = NULL;
            alloc = false;
        }
    };
private:
    SE_TreeStructID createID(int index);
    _Node* findNode(const SE_TreeStructID& id);
    bool findIndex(int index)
    {
        _NodeIndexList::iterator it;
        bool ret = false;
        int index = id.get(0);
        for(it = mFreeNodeIndexList.begin() ; it != mFreeNodeIndexList.end() ; it++)
        {
            if(*it == index)
                return true;
        }
        return false;
    }
    void createFreeIndexList()
    {
        for(int i = 0 ; i < mNodes.size() ; i++)
        {
            _Node* node = &mNodes[i];
            if(!node->alloc)
            {
                SE_ASSERT(node->data == NULL);
                mFreeNodeIndexList.push_back(i);
            }
        }
    }
private:
    typedef std::list<int> _NodeIndexList;
    _NodeIndexList mFreeNodeIndexList;
    SE_DynamicArray<_Node> mNodes;
    int mError;
};
///////////////////////////////////////////////////////////////////
/*
class SE_TreeStruct
{
public:
    SE_TreeStructID getID() const;
    void setID(const SE_TreeStructID& id);
    SE_TreeStruct();
    void removeChild(const SE_TreeStructID& id);
    SE_TreeStructID getParent();
    void setParent(const SE_TreeStructID p);
private:
    typedef std::list<SE_TreeStructID> _ChildrenList;
    _ChildrenList mChildren;
    SE_TreeStructID mParent; 
    SE_TreeStructID mID;
};
*/
/////////////////////////////////////////////
/*
template <typename T>
SE_TreeStructID SE_TreeStructManager<T>::NULLID = SE_TreeStructID(-1, -2);
*/
template <typename T>
SE_TreeStructManager<T>::SE_TreeStructManager(int size) : mNodes(size ,MAX_SIZE), mError(0)
{
    /*
    if(size <= 0)
        return;
    mNodes = new _Node[size];
    if(!mNodes)
        return;
    mNodeSize = size;
    */
    for(int i = 0 ; i < mNodes.size() ; i++)
    {
        mFreeNodeIndexList.push_back(i);
    }
}
template <typename T>
SE_TreeStructManager<T>::~SE_TreeStructManager()
{
    for(int i = 0 ; i < mNodes.size() ; i++)
    {
        _Node* node = &mNodes[i];
        if(node->alloc)
        {
            SE_DelayDestroy* dd = new SE_DelayDestroyPointer<T>(node->data);
            bool ret = SE_Application::getInstance()->addDelayDestroy(dd);
            if(!ret)
                delete dd;
        }
    }
    /*
    if(mNodes)
        delete[] mNodes;
        */
}
template <typename T>
SE_TreeStructID SE_TreeStructManager::createID(int index)
{
    return SE_TreeStructID(index, SE_Application::getInstance()->getSeqNum());
}
template <typename T>
T* SE_TreeStructManager<T>::find(const SE_TreeStructID& id) const
{
    _Node* node = findNode(id);
    if(!node)
        return NULL;
    return node->data;
}
template <typename T>
_Node* SE_TreeStructNode<T>::findNode(const SE_TreeStructID& id) const
{
   int index = id.get(0);
   if(index < 0 || index >= mNodes.size()) 
       return NULL;
   _Node* node = &mNodes[index];
   if(!node->alloc || node->data == NULL)
       return NULL;
   const SE_TreeStructID& nodeDataID = node->data->getID();
   if(nodeDataID != id)
       return NULL;
   return node;    
}
template <typename T>
T* SE_TreeStructManager::remove(const SE_TreeStructID& id)
{
    _Node* node = findNode(id);
    if(!node)
        return NULL;
    T* retData = node->data;
    node->alloc = false;
    node->data = NULL;
    int index = id.get(0);
#ifdef _DEBUG
    {
        bool ret = findIndex(index);
        SE_ASSERT(!ret);
    }
#endif
    _Node* parentNode = findNode(node->parent);
    if(parentNode)
    {
        SE_ASSERT(parentNode->alloc && parentNode->data != NULL);
        parentNode->data->removeChild(node->data);
    }
    node->parent = SE_TreeStructID::NULLID;
    mFreeNodeIndexList.push_back(index);
    std::vector<T*> children = retData->getChildren();
    for(int i = 0 ; i < children.size() ; i++)
    {
        T* cc = children[i];
        remove(cc->getID());
    }
#ifdef _DEBUG
    check();
#endif
    return retData;
}
template <typename T>
void SE_TreeStructManager<T>::release(T* p, int delay)
{
    if(p == NULL)
        return;
    if(delay == RELEASE_NO_DELAY)
    {
        delete p;
    }
    else
    {
        SE_DelayDestroy* dd = new SE_DelayDestroyPointer<T>(p);
        bool ret = SE_Application::getInstance()->addDelayDestroy(dd);
        if(!ret)
        {
            delete dd;
        }
    }
}
template <typename T>
void SE_TreeStructManager<T>::release(const SE_TreeStructID& id, int delay)
{
    _Node* node = findNode(id);
    if(node)
    {
        SE_ASSERT(node->data);
        release(node->data, delay);
    }
}

template <typename T>
SE_TreeStructID SE_TreeStructManager<T>::add(const SE_TreeStructID& parentID, T* child)
{
    if(parentID != NULLID)
    {
        _Node* parentNode = findNode(parentID);
        if(!parentNode)
            return SE_TreeStructID::INVALID;
        if(mFreeNodeIndexList.empty())
        {
            mNodes.expand();
            createFreeIndexList();
            if(mFreeNodeIndexList.empty())
                return SE_TreeStructID::INVALID;
        }
        parentNode->data->addChild(child);
    }
    int index = mFreeNodeIndexList.front();
    mFreeNodeIndexList.pop_front();
    SE_ASSERT(index >= 0 && index < mNodes.size());
    _Node* node = &mNodes[index];
    node->alloc = true;
    node->data = child;
    node->parent = parentID;
    SE_TreeStructID childID = createID(index);
    child->setID(childID);
    std::vector<T*> children = child->getChildren();
    for(int i = 0 ; i < children.size() ; i++)
    {
        T* cc = children[i];
        add(childID, cc);
    }
#ifdef _DEBUG
    check();
#endif
    return childID;
}
template <typename T>
void SE_TreeStructManager<T>::check()
{
    if(mError != NO_ERROR)
    {
        LOGE("error: %d\n", mError);
    }
    int allocNum = getAllocatedNodeNum();
    int num = 0;
    for(int i = 0 ; i < mNodes.size() ; i++)
    {
        if(mNodes[i].alloc)
            num++;
    }
    if(num != allocNum)
    {
        LOGE("error : node list alloc num is not equal free node list num\n");
    }
}
template <typename T>
int SE_TreeStructManager<T>::getAllocatedNodeNum() const
{
    int size = mFreeNodeIndexList.size();
    return mNodeSize - size;
}
template <typename T>
T* SE_TreeStructManager<T>::getParent(const SE_TreeStructID& id) const
{
    _Node* node = findNode(id);
    if(!node)
        return NULL;
    _Node* parentNode = findNode(node->parent);
    if(!parentNode)
        return NULL;
    SE_ASSERT(parentNode->data);
    return parentNode->data;
    
}
template <typename T>
std::vector<T*> SE_TreeStructManager<T>::getChildren(const SE_TreeStructID& id) const
{
    std::vector<T*> ret;
    _Node* node = findNode(id);
    if(!node)
        return ret;
    SE_ASSERT(node->data);
    return node->data->getChildren();
}
#endif
