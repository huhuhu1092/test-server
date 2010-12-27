#ifndef SE_STATETABLE_H
#define SE_STATETABLE_H
#include "SE_ID.h"
#include "SE_PropertySet.h"
#include <list>
class SE_State;
class SE_StateAction
{
public:
	virtual ~SE_StateAction() {}
	virtual void action(SE_State* enterState, SE_State* exitState, SE_State* trigerState) = 0;
};

class SE_State
{
public:
	SE_State();
	~SE_State();
    void setID(const SE_StateID& id);
	SE_StateID getID();
	void trigger(const SE_TriggerID& id);
	void addTriggerAction(const SE_TriggerID& id, SE_StateAction* action);
	SE_PropertySet& getPropertySet()
	{
		return mPropertySet;
	}
private:
	struct _Data
	{
		SE_StateAction* action;
		SE_TriggerID triggerID;
		_Data()
		{
			action = 0;
		}
	};
	SE_StateID mStateID;
	typedef std::list<_Data> _TriggerActionList;
	_TriggerActionList mTriggerActionList;
	SE_PropertySet mPropertySet;
};
class SE_StateTranslation
{
public:
	SE_StateTranslation()
	{}
	~SE_StateTranslation()
	{}
	void setFrom(const SE_StateID& from)
	{
		mFrom = from;
	}
	void setTo(const SE_StateID& to)
	{
		mTo = to;
	}
	void setStimulate(const SE_StimulateID& stimulate)
	{
		mStimulate = stimulate;
	}
	void setAction(SE_StateAction* action)
	{
		mStateAction = action;
	}
	void set(const SE_StateID& from, const SE_StateID& to, const SE_StimulateID& stimulate, SE_StateAction* action)
	{
		mFrom = from;
		mTo = to;
		mStimulate = stimulate;
		mStateAction = action;
	}
	SE_StateID getFrom()
	{
		return mFrom;
	}
	SE_StateID getTo()
	{
		return mTo;
	}
	SE_StimulateID getStimulate()
	{
		return mStimulate;
	}
	SE_StateAction* getAction()
	{
		return mStateAction;
	}
	bool operator==(const SE_StateTranslation& t)
	{
		return t.mFrom == mFrom && t.mTo == mTo && t.mStimulate == mStimulate;
	}
	bool operator != (const SE_StateTranslation& t)
	{
		return !this->operator==(t);
	}
public:
	SE_StateID mFrom;
	SE_StateID mTo;
	SE_StimulateID mStimulate;
	SE_StateAction* mStateAction;
};
class SE_StateTable
{
public:
	SE_StateTable();
	~SE_StateTable();
	void setID(const SE_StateTableID& id);
	SE_StateTableID getID();
    void addState(SE_State* state);
	void removeState(const SE_StateID& stateid);
	SE_State* getState(const SE_StateID& stateid);
	void addTranslation(const SE_StimulateID& stimulate, const SE_StateID& from, 
		                const SE_StateID& to, SE_StateAction* action);
	void removeTranslation(const SE_StimulateID& stimulate, const SE_StateID& from, const SE_StateID& to);
	void stimulate(const SE_StimulateID& id);
	void trigger(const SE_TriggerID& id);
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
	     bool operator()(SE_StateTranslation& t)
		 {
			 return st == t;
		 }
		 SE_StateTranslation st;
	};
	class _StimulateEqual
	{
	public:
		bool operator()(SE_StateTranslation& t)
		{
			if(stimulateID == t.getStimulate() && fromID == t.getFrom())
				return true;
			else 
				return false;
		}
		SE_StimulateID stimulateID;
		SE_StateID fromID;
	};
private:
	SE_StateTableID mStateTableID;
	typedef std::list<SE_State*> _StateList;
	_StateList mStateList;
	typedef std::list<SE_StateTranslation> _TranslationList;
	_TranslationList mTranslationList;
	SE_State* mCurrentState;
};
#endif