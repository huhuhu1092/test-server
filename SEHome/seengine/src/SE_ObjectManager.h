#ifndef SE_OBJECTMANAGER_H
#define SE_OBJECTMANAGER_H
#include <map>
template <typename T>
struct SE_FindObjCondition
{
	virtual ~SE_FindObjCondition() {}
	virtual bool isSatisfy(T data) = 0;
};
template <typename T>
struct SE_FindObjCondition<T*>
{
	virtual ~SE_FindObjCondition() {}
	virtual bool isSatisfy(T* data) = 0;
};
template <typename TID, typename T>
struct SE_ObjectManagerVisitor
{
	virtual ~SE_ObjectManagerVisitor()
	{}
	virtual void visit(const TID& id, const T& v) = 0;
};
template <typename TID, typename T>
struct SE_ObjectManagerVisitor<TID, T*>
{
	virtual ~SE_ObjectManagerVisitor()
	{}
	virtual void visit(const TID& id, const T* v) = 0;
};

////////////////////////////////////////////
template <typename TID, typename T>
class SE_ObjectManager
{
public:
    typedef std::map<TID, T> RMap;
    T get(const TID& id) const;
    void set(const TID& id, T data);
    void remove(const TID& id);
	T find(SE_FindObjCondition<T>& fc) const;
    bool isContain(const TID& id) const;
    ~SE_ObjectManager();
	size_t size() const
	{
		return m.size();
	}
	void traverse(SE_ObjectManagerVisitor<TID, T>& visitor) const;
private:
    RMap m;
};
template <typename TID, typename T>
void SE_ObjectManager<TID, T>::traverse(SE_ObjectManagerVisitor<TID, T>& visitor) const
{
	typename RMap::const_iterator it;
	for(it = m.begin() ; it != m.end() ; it++)
	{
		visitor.visit(it->first, it->second);
	}
}
template <typename TID, typename T>
void SE_ObjectManager<TID, T>::remove(const TID& id)
{
    typename RMap::iterator it = m.find(id);
    if(it != m.end())
    {
        m.erase(it);
    }
}

template <typename TID, typename T>
T SE_ObjectManager<TID, T>::get(const TID& id) const
{
    typename RMap::const_iterator it = m.find(id);
    if(it == m.end())
        return T();
    else
        return it->second;
}
template <typename TID, typename T>
void SE_ObjectManager<TID, T>::set(const TID& id, T data)
{
    typename RMap::iterator it = m.find(id);
    if(it == m.end())
    {
        m.insert(std::pair<TID, T>(id, data));
    }
    else
    {
        it->second = data;
    }
}
template <typename TID, typename T>
SE_ObjectManager<TID, T>::~SE_ObjectManager()
{

}
template <class TID, class T>
T SE_ObjectManager<TID, T>::find(SE_FindObjCondition<T>& fc) const
{
    typename RMap::iterator it;
    for(it = m.begin() ; it != m.end() ; it++)
    {
		T data = it->second;
		if(fc.isSatisfy(data))
		{
			return data;
		}
	}
	return T();
}
template <class TID, class T>
bool SE_ObjectManager<TID, T>::isContain(const TID& id) const
{
    typename RMap::iterator it = m.find(id);
    if(it == m.end())
        return false;
    else
        return true;
}
////////////////////////////////////////////
template <typename TID, typename T>
class SE_ObjectManager<TID, T*>
{
public:
    typedef std::map<TID, T*> RMap;
    T* get(const TID& id) const;
    void set(const TID& id, T* data);
    void remove(const TID& id);
	T* find(SE_FindObjCondition<T*>& fc) const;
    bool isContain(const TID& id) const;
    ~SE_ObjectManager();
	int size() const
	{
		return m.size();
	}
	void traverse(SE_ObjectManagerVisitor<TID, T*>& visitor) const;
private:
    RMap m;
};
template <typename TID, typename T>
void SE_ObjectManager<TID, T*>::traverse(SE_ObjectManagerVisitor<TID, T*>& visitor) const
{
	typename RMap::const_iterator it;
	for(it = m.begin() ; it != m.end() ; it++)
	{
		visitor.visit(it->first, it->second);
	}
}
template <typename TID, typename T>
void SE_ObjectManager<TID, T*>::remove(const TID& id)
{
    typename RMap::iterator it = m.find(id);
    if(it != m.end())
    {
        delete it->second;
        m.erase(it);
    }
}

template <typename TID, typename T>
T* SE_ObjectManager<TID, T*>::get(const TID& id) const
{
    typename RMap::const_iterator it = m.find(id);
    if(it == m.end())
        return NULL;
    else
        return it->second;
}
template <typename TID, typename T>
void SE_ObjectManager<TID, T*>::set(const TID& id, T* data)
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
template <typename TID, typename T>
SE_ObjectManager<TID, T*>::~SE_ObjectManager()
{
    typename RMap::iterator it;
    for(it = m.begin() ; it != m.end() ; it++)
    {
        T* data = it->second;
        delete data;
    }
}
template <typename TID, typename T>
T* SE_ObjectManager<TID, T*>::find(SE_FindObjCondition<T*>& fc) const
{
    typename RMap::const_iterator it;
    for(it = m.begin() ; it != m.end() ; it++)
    {
		T* data = it->second;
		if(fc.isSatisfy(data))
		{
			return data;
		}
	}
	return NULL;
}
template <typename TID, typename T>
bool SE_ObjectManager<TID, T*>::isContain(const TID& id) const
{
    typename RMap::iterator it = m.find(id);
    if(it == m.end())
        return false;
    else
        return true;
}
#endif
