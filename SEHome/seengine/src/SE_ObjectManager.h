#ifndef SE_OBJECTMANAGER_H
#define SE_OBJECTMANAGER_H
#include <map>
template <class TID, class T>
class SE_ObjectManager
{
public:
    typedef std::map<TID, T*> RMap;
    T* get(const TID& id);
    void set(const TID& id, T* data);
    void remove(const TID& id);
    ~SE_ObjectManager();
private:
    RMap m;
};

template <class TID, class T>
void SE_ObjectManager<TID, T>::remove(const TID& id)
{
    typename RMap::iterator it = m.find(id);
    if(it != m.end())
    {
        delete it->second;
        m.erase(it);
    }
}

template <class TID, class T>
T* SE_ObjectManager<TID, T>::get(const TID& id)
{
    typename RMap::iterator it = m.find(id);
    if(it == m.end())
        return NULL;
    else
        return it->second;
}
template <class TID, class T>
void SE_ObjectManager<TID, T>::set(const TID& id, T* data)
{
    typename RMap::iterator it = m.find(id);
    if(it == m.end())
    {
        m.insert(std::pair<TID, T*>(id, data));
    }
    else
    {
        T* oldData = it->second;
        it->second = data;
        delete oldData;
    }
}
template <class TID, class T>
SE_ObjectManager<TID, T>::~SE_ObjectManager()
{
    typename RMap::iterator it;
    for(it = m.begin() ; it != m.end() ; it++)
    {
        T* data = it->second;
        delete data;
    }
}

#endif
