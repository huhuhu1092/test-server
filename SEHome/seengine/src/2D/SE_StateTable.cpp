#include "SE_StateTable.h"
#include <algorithm>
SE_State::SE_State()
{
}
SE_State::~SE_State()
{
    class _DeleteData
	{
	public:
		void operator()(_Data& d) const
		{
			if(d.action)
				delete d.action;
		}
	};
	for_each(mTriggerActionList.begin(), mTriggerActionList.end(), _DeleteData());
}
void SE_State::setID(const SE_StateID& id)
{
	mStateID = id;
}
SE_StateID SE_State::getID()
{
	return mStateID;
}
void SE_State::trigger(const SE_TriggerID& id)
{
	class InvokeTrigger
	{
	public:
		void operator()(_Data& d) const
		{
			if(id == d.triggerID)
			{
				d.action->action(NULL, NULL, triggerState);
			}
		}
		SE_TriggerID id;
		SE_State* triggerState;
	} ;
	InvokeTrigger it;
	it.id = id;
	it.triggerState = this;
	for_each(mTriggerActionList.begin(), mTriggerActionList.end(), it);
}
void SE_State::addTriggerAction(const SE_TriggerID& id, SE_StateAction* action)
{
	_Data d;
	d.action = action;
	d.triggerID = id;
	mTriggerActionList.push_back(d);
}
///////////////////////
SE_StateTable::SE_StateTable()
{
	mCurrentState = NULL;
}
SE_StateTable::~SE_StateTable()
{}
void SE_StateTable::setID(const SE_StateTableID& id)
{
	mStateTableID = id;
}
SE_StateTableID SE_StateTable::getID()
{
    return mStateTableID;
} 
void SE_StateTable::addState(SE_State* state)
{
	mStateList.push_back(state);
}

void SE_StateTable::removeState(const SE_StateID& stateid)
{

	_StateList::iterator it = find_if(mStateList.begin(), mStateList.end(), _StateEqual(stateid));
	if(it != mStateList.end())
	{
		delete *it;
		mStateList.erase(it);
	}
}
SE_State* SE_StateTable::getState(const SE_StateID& stateid)
{
	_StateList::iterator it = find_if(mStateList.begin(), mStateList.end(), _StateEqual(stateid));
	if(it != mStateList.end())
		return *it;
	else
		return NULL;
}
void SE_StateTable::addTranslation(const SE_StimulateID& stimulate, const SE_StateID& from,
								   const SE_StateID& to, SE_StateAction* action)
{
	SE_StateTranslation t;
	t.set(stimulate, from, to, action);
	mTranslationList.push_back(t);
}
void SE_StateTable::removeTranslation(const SE_StimulateID& stimulate, const SE_StateID& from, const SE_StateID& to)
{
	_TranslationEqual et;
    et.st.setStimulate(stimulate);
	et.st.setFrom(from);
	et.st.setTo(to);
	_TranslationList::iterator it = find_if(mTranslationList.begin(), mTranslationList.end(), et);
	if(it != mTranslationList.end())
	{
		mTranslationList.erase(it);
	}
}
void SE_StateTable::stimulate(const SE_StimulateID& id)
{
	if(!mCurrentState)
		return;
	_StimulateEqual se;
	se.stimulateID = id;
	se.fromID = mCurrentState->getID();
	_TranslationList::iterator it = find_if(mTranslationList.begin(), mTranslationList.end(), se);
	if(it != mTranslationList.end())
	{
		SE_StateID to = it->getTo();
		SE_State* toState = getState(to);
		SE_StateAction* action = it->getAction();
		action->action(mCurrentState, toState, NULL);
		mCurrentState = toState;
	}
}
void SE_StateTable::trigger(const SE_TriggerID& id)
{
	if(!mCurrentState)
		return;
	mCurrentState->trigger(id);
}