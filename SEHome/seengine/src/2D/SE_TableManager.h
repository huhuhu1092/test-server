#ifndef SE_TABLEMANAGER_H
#define SE_TABLEMANAGER_H
#include "SE_ID.h"
#include "SE_ObjectManager.h"
#include "SE_Utils.h"
#include <map>
#include <utility>
#include <stdlib.h>
#include <stdio.h>

template <typename ID>
struct SE_IDTrait
{
     static ID SE_ConvertToID(const char* str)
     {
         ID id;
         return id;
     }
};
template <>
struct SE_IDTrait<int>
{
    static int SE_ConvertToID(const char* str)
    {
        return atoi(str);
    }
};
template <>
struct SE_IDTrait<SE_StringID>
{
    static SE_StringID SE_ConvertToID(const char* str)
    {
        return SE_StringID(str);
    }
};
template <>
struct SE_IDTrait<SE_CommonID>
{
    static SE_CommonID SE_ConvertToID(const char* str)
    {
        return SE_CommonID::INVALID;
    }
};
class SE_TableDefaultProperty
{};
template <typename ID, typename VALUE, typename PROPERTY = SE_TableDefaultProperty>
class SE_Table
{
public:
    typedef typename ID TABLE_ITEM_ID;
    typedef typename VALUE TABLE_ITEM_VALUE;
    void setItem(const ID& id, const VALUE& v)
    {
        _TableItemMap::iterator it = mItemMap.find(id);
        if(it != mItemMap.end())
        {
            LOGI("... warning: id has value\n");
            it->second = v;
        }
        else
        {
			mItemMap.insert(mItemMap.end(), std::make_pair(id, v));
        }
    }
    bool getItem(const ID& id, VALUE& outValue)
    {
        _TableItemMap::iterator it = mItemMap.find(id);
        if(it != mItemMap.end())
        {
            outValue = it->second;
            return true;
        }
        else
            return false;
    }
    void removeItem(const ID& id)
    {
        mItemMap.erase(id);
    }
    bool isContain(const ID& id)
    {
        _TableItemMap::iterator it = mItemMap.find(id);
        if(it != mItemMap.end())
            return true;
        else
            return false;
    }
	PROPERTY getProperty()
	{
		return mProperty;
	}
private:
    typedef std::map<ID, VALUE> _TableItemMap;
    _TableItemMap mItemMap;
	PROPERTY mProperty;
};
template <typename ID, typename VALUE, typename PROPERTY>
class SE_Table<ID, VALUE*, PROPERTY>
{
public:
    typedef typename ID TABLE_ITEM_ID;
    typedef typename VALUE* TABLE_ITEM_VALUE;
    void setItem(const ID& id, VALUE* v)
    {
        mTableItems.set(id, v);
    }
    VALUE* getItem(const ID& id)
    {
        return mTableItems.get(id);
    }
    void removeItem(const ID& id)
    {
        mTableItems.get(id);
    }
    bool isContain(const ID& id)
    {
        return mTableItems.isContain(id);
    }
	PROPERTY getProperty()
	{
		return mProperty;
	}
private:
    SE_ObjectManager<ID, VALUE*> mTableItems;
	PROPERTY mProperty;
};
/*
template <typename ID, typename TABLE, typename IDTRAIT = SE_IDTrait<ID> >
class SE_TableSet
{
public:
    typedef typename ID TABLE_ID;
    typedef typename TABLE::TABLE_ITEM_ID TABLE_ITEM_ID;
    typedef typename TABLE::TABLE_ITEM_VALUE TABLE_ITEM_VALUE;
    typedef typename TABLE::TABLE_ITEM_IDTRAIT TABLE_ITEM_IDTRAIT;
    typedef typename TABLE TABLE_TYPE;
    typedef typename IDTRAIT TABLE_IDTRAIT;
    void setTable(const ID& id, TABLE* table)
    {
        mTables.set(id, table);
    }
    TABLE* getTable(const ID& id)
    {
        return mTables.get(id);
    }
    void removeTable(const ID& id)
    {
        mTables.remove(id);
    }
private:
    SE_ObjectManager<ID, TABLE*> mTables;
};

template <typename ID, typename TABLESET, typename IDTRAIT = SE_IDTrait<ID> >
class SE_TableManager
{
public:
    typedef IDTRAIT TABLEMANAGER_IDTRAIT;
    void setTableSet(const ID& id, TABLESET* tableSet)
    {
        mTableSets.set(id, tableSet);
    }
    TABLESET* getTableSet(const ID& id)
    {
        return mTableSets.get(id);
    }
    void removeTableSet(const ID& id)
    {
        return mTableSets.remove(id);
    }
    bool getValue(const char* fullPath, typename TABLESET::TABLE_ITEM_VALUE& outValue)
    {
        std::vector<std::string> idList = SE_Util::splitString(fullPath, "/");
        if(idList.size() < 3)
            return false;
        std::string idStr = idList[0];
        ID id = IDTRAIT::SE_ConvertToID(idStr.c_str());
        TABLESET* tableSet = getTableSet(id);
        if(!tableSet)
        {
            return false;
        }
        idStr = idList[1];
        TABLESET::TABLE_ID tableID = TABLESET::TABLE_IDTRAIT::SE_ConvertToID(idStr.c_str());
        TABLESET::TABLE_TYPE* table = tableSet->getTable(tableID);
        if(!table)
           return false;
        idStr = idList[2];
        TABLESET::TABLE_ITEM_ID tableItemID = TABLESET::TABLE_ITEM_IDTRAIT::SE_ConvertToID(idStr.c_str());
        bool ret = table->getItem(tableItemID, outValue);
        return ret;
    }
private:
    SE_ObjectManager<ID, TABLESET*> mTableSets;
};
*/
#endif
