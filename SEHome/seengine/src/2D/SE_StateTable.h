#ifndef SE_STATETABLE_H
#define SE_STATETABLE_H
#include "SE_ID.h"
#include "SE_PropertySet.h"
#include "SE_TableManager.h"
#include "SE_URI.h"
#include <list>
/////////////////////////////
class SE_State;
class SE_StateAction
{
public:
	virtual ~SE_StateAction() {}
	virtual void action(SE_State* enterState, SE_State* exitState, 
		                SE_State* trigerState, const SE_StringID& actionURI) = 0;
};
class SE_State
{
public:
	SE_State();
	~SE_State();
    void setID(const SE_StateID& id);
	SE_StateID getID();
	void trigger(const SE_TriggerID& id);
	void setTriggerAction(const SE_TriggerID& id, SE_StateAction* action);
	void setTriggerAction(const SE_TriggerID& id, const SE_StringID& actionURI);
	SE_StateAction* getTriggerAction(const SE_TriggerID& id);
    SE_StringID getDefaultValue();
	SE_StringID getParamValue(const char* param);
	void setParam(const SE_StringID& param, const SE_StringID& value);
	void setDefaultValue(const SE_StringID& id, const SE_StringID& uri);
	/*
	SE_PropertySet& getPropertySet()
	{
		return mPropertySet;
	}
	*/
private:
	struct _Data
	{
		SE_StateAction* action;
		SE_StringID actionURI;
		SE_TriggerID triggerID;
		_Data()
		{
			action = 0;
		}
	};
	struct _Property
	{
		SE_StringID id;
		SE_StringID value;
	};
	class _FindAction
	{
	public:
		bool operator()(_Data& d) const
		{ 
			if(triggerID == d.triggerID)
				return true;
			else
				return false;
		}
		SE_TriggerID triggerID;
	};
    class _FindProperty
	{
	public:
		bool operator()(_Property& p) const
		{
			if(param == p.id)
				return true;
			else
				return false;
		}
		SE_StringID param;
	};
	SE_StateID mStateID;
	typedef std::list<_Data> _TriggerActionList;
	_TriggerActionList mTriggerActionList;
	typedef std::list<_Property> _PropertyList;
	_PropertyList mPropertyList;
	_Property mDefaultProperty;
};
class SE_StateTranslation
{
public:
	SE_StateTranslation()
	{
		mStateAction = NULL;
	}
	~SE_StateTranslation();
	void setFrom(const SE_StateID& from)
	{
		mFrom = from;
	}
	void setTo(const SE_StateID& to)
	{
		mTo = to;
	}
	void setAction(SE_StateAction* action)
	{
		mStateAction = action;
	}
	void set(const SE_StateID& from, const SE_StateID& to, const SE_StringID& actionURI)
	{
		mFrom = from;
		mTo = to;
		mActionURI = actionURI;
	}
	SE_StateID getFrom()
	{
		return mFrom;
	}
	SE_StateID getTo()
	{
		return mTo;
	}
	SE_StringID getActionURI()
	{
		return mActionURI;
	}

	SE_StateAction* getAction()
	{
		return mStateAction;
	}
	bool operator==(const SE_StateTranslation& t)
	{
		return t.mFrom == mFrom && t.mTo == mTo;
	}
	bool operator != (const SE_StateTranslation& t)
	{
		return !this->operator==(t);
	}
public:
	SE_StateID mFrom;
	SE_StateID mTo;
	SE_StringID mActionURI;
	SE_StateAction* mStateAction;
};
class SE_StateMachine
{
public:
	SE_StateMachine();
	~SE_StateMachine();
	void setID(const SE_StateMachineID& id);
	void setStartState(const SE_StringID& stateURI);
	SE_StringID getStartState();
	SE_StateMachineID getID();
    void addState(SE_State* state);
	void removeState(const SE_StateID& stateid);
	SE_State* getState(const SE_StateID& stateid);
	void addTranslation(const SE_StateID& from, const SE_StateID& to, const SE_StringID& actionURI);
	SE_StateTranslation* getTranslation(const SE_StateID& from, const SE_StateID& to);
	void removeTranslation(const SE_StateID& from, const SE_StateID& to);
	void trigger(const SE_TriggerID& id);
	void translateTo(const SE_StateID& to);
	bool canTranslateTo(const SE_StateID& to);
	SE_StateID getCurrentStateID();
	SE_State* getCurrentState();
private:
	void initStartState();
private:
	class _StateEqual
	{
	public:
		_StateEqual(const SE_StateID& sid) : id(sid)
		{}
		bool operator()(SE_State* s)
		{
			if(s->getID() == id)
				return true;
			else
				return false;
		}
		SE_StateID id;
	};
	class _TranslationEqual
	{
	public:
	     bool operator()(SE_StateTranslation* t)
		 {
			 return t->getFrom() == from && t->getTo() == to;
		 }
		 SE_StateID from;
		 SE_StateID to;
	};
private:
	SE_StateMachineID mStateMachineID;
	typedef std::list<SE_State*> _StateList;
	_StateList mStateList;
	typedef std::list<SE_StateTranslation*> _TranslationList;
	_TranslationList mTranslationList;
	SE_URI mStartURI;
	SE_State* mCurrentState;
};
///////////////////////
class SE_StateChange
{
public:
	SE_StateID from;
	SE_StateID to;
	SE_StringID actionURI;
};
class SE_StateChangeList
{
public:
	void setStateMachine(SE_StateMachine* stateMachine);
	void add(const SE_StateChange& sc);
private:
	typedef std::list<SE_StateChange> _StateChangeList;
    _StateChangeList mStateChangeList;
};
typedef SE_Table<SE_StringID, SE_StateMachine*> SE_StateMachineSet;
typedef SE_Table<SE_StringID, SE_StateMachineSet*> SE_StateMachineTable;
typedef SE_Table<SE_StringID, SE_StateChangeList*> SE_StateChangeSet;
typedef SE_Table<SE_StringID, SE_StateChangeSet*> SE_StateChangeTable;
#endif