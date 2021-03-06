#include "SE_StateTable.h"
#include "SE_Utils.h"
#include "SE_URI.h"
#include "SE_Application.h"
#include "SE_ParamManager.h"
#include <algorithm>
#include <functional>

SE_State::SE_State()
{
}
SE_State::~SE_State()
{
    std::for_each(mTriggerActionList.begin(), mTriggerActionList.end(), _DeleteData());
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
	InvokeTrigger it;
	it.id = id;
	it.triggerState = this;
    std::for_each(mTriggerActionList.begin(), mTriggerActionList.end(), it);
}
void SE_State::setTriggerAction(const SE_TriggerID& id, const SE_StringID& actionURI)
{
	_FindAction fa;
	fa.triggerID = id;
	_TriggerActionList::iterator it = find_if(mTriggerActionList.begin(), mTriggerActionList.end(), fa);
	if(it != mTriggerActionList.end())
	{
		it->actionURI = actionURI;
	}
	else
	{
	    _Data d;
	    d.actionURI = actionURI;
	    d.triggerID = id;
	    mTriggerActionList.push_back(d);
	}
}
void SE_State::setTriggerAction(const SE_TriggerID& id, SE_StateAction* action)
{
	_FindAction fa;
	fa.triggerID = id;
	_TriggerActionList::iterator it = find_if(mTriggerActionList.begin(), mTriggerActionList.end(), fa);
	if(it != mTriggerActionList.end())
	{
		it->action = action;
	}
	else
	{
	    _Data d;
	    d.action = action;
	    d.triggerID = id;
	    mTriggerActionList.push_back(d);
	}
}
SE_StringID SE_State::getDefaultValue()
{
	return mDefaultProperty.value;
}
std::vector<SE_AddressID> SE_State::getParamAddress()
{
    std::vector<SE_AddressID> retparam(mPropertyList.size());
    _PropertyList::iterator it;
    int i = 0;
    for(it = mPropertyList.begin() , i = 0; it != mPropertyList.end(); it++, i++)
    {
        retparam[i] = it->id;
    }
    return retparam;
}
SE_StringID SE_State::getParamValue(const SE_AddressID& address)
{
    _FindProperty fp;
	fp.param = address;
	_PropertyList::iterator it = find_if(mPropertyList.begin(), mPropertyList.end(), fp);
	if(it != mPropertyList.end())
	{
		return it->value;
	}
	else
		return "";
}
void SE_State::setParam(const SE_AddressID& param, const SE_StringID& value)
{
	_Property p;
	p.id = param;
	p.value = value;
	mPropertyList.push_back(p);
}
void SE_State::setDefaultValue(const SE_StringID& id, const SE_StringID& uri)
{
	mDefaultProperty.id = id;
	mDefaultProperty.value =uri;
}
//////////////////////////
SE_StateTranslation::~SE_StateTranslation()
{
	if(mStateAction)
		delete mStateAction;
}
///////////////////////
SE_StateMachine::SE_StateMachine()
{
	mCurrentState = NULL;
}
SE_StateMachine::~SE_StateMachine()
{
	for_each(mStateList.begin(), mStateList.end(), SE_DeleteObject());
	for_each(mTranslationList.begin(), mTranslationList.end(), SE_DeleteObject());
}
void SE_StateMachine::setID(const SE_StateMachineID& id)
{
	mStateMachineID = id;
}
SE_StateMachineID SE_StateMachine::getID()
{
    return mStateMachineID;
} 
void SE_StateMachine::addState(SE_State* state)
{
	mStateList.push_back(state);
}

void SE_StateMachine::removeState(const SE_StateID& stateid)
{

	_StateList::iterator it = find_if(mStateList.begin(), mStateList.end(), _StateEqual(stateid));
	if(it != mStateList.end())
	{
		delete *it;
		mStateList.erase(it);
	}
}
SE_State* SE_StateMachine::getState(const SE_StateID& stateid)
{
	_StateList::iterator it = find_if(mStateList.begin(), mStateList.end(), _StateEqual(stateid));
	if(it != mStateList.end())
		return *it;
	else
		return NULL;
}
void SE_StateMachine::addTranslation(const SE_StateID& from, const SE_StateID& to, const SE_StringID& actionURI)
{
	SE_StateTranslation* t = new SE_StateTranslation;
	t->set(from, to, actionURI);
	mTranslationList.push_back(t);
}
SE_StateTranslation* SE_StateMachine::getTranslation(const SE_StateID& from, const SE_StateID& to)
{
	_TranslationEqual te;
	te.from = from;
	te.to = to;
	_TranslationList::iterator it = find_if(mTranslationList.begin(), mTranslationList.end(), te);
	if(it != mTranslationList.end())
		return *it;
	else
		return NULL;
}
void SE_StateMachine::removeTranslation(const SE_StateID& from, const SE_StateID& to)
{
	_TranslationEqual te;
	te.from = from;
	te.to = to;
	_TranslationList::iterator it = find_if(mTranslationList.begin(), mTranslationList.end(), te);
	if(it != mTranslationList.end())
	{
		delete *it;
		mTranslationList.erase(it);
	}
}
void SE_StateMachine::translateTo(const SE_StateID& to)
{
	if(!mCurrentState)
	{
		initStartState();
		if(!mCurrentState)
		    return;
	}
	_TranslationEqual te;
	te.to = to;
	te.from = mCurrentState->getID();
	_TranslationList::iterator it = find_if(mTranslationList.begin(), mTranslationList.end(), te);
	if(it != mTranslationList.end())
	{
		SE_StateID to = (*it)->getTo();
		SE_State* toState = getState(to);
		SE_StateAction* action = (*it)->getAction();
		if(action)
			action->action(mCurrentState, toState, NULL, (*it)->getActionURI());
		mCurrentState = toState;
        std::vector<SE_AddressID> paramID = mCurrentState->getParamAddress();
        SE_ParamManager* paramManager = SE_Application::getInstance()->getParamManager();
        for(int i = 0 ; i < paramID.size() ; it++)
        {
            SE_StringID value = mCurrentState->getParamValue(paramID[i]);
			paramManager->setString(paramID[i], value.getStr(), true);
        }
	}
}
void SE_StateMachine::trigger(const SE_TriggerID& id)
{
	if(!mCurrentState)
	{
		initStartState();
		if(!mCurrentState)
		    return;
	}
	mCurrentState->trigger(id);
}
bool SE_StateMachine::canTranslateTo(const SE_StateID& to)
{
	if(!mCurrentState)
	{
		initStartState();
		if(!mCurrentState)
		    return false;
	}
	SE_StateID from = mCurrentState->getID();
	_TranslationEqual te;
	te.from = from;
	te.to = to;
	_TranslationList::iterator it = find_if(mTranslationList.begin(), mTranslationList.end(), te);
	if(it != mTranslationList.end())
		return true;
	else
		return false;
}
void SE_StateMachine::initStartState()
{
	SE_StringID startURL = mStartURI.getURL();
	mCurrentState = getState(startURL);
}
SE_State* SE_StateMachine::getCurrentState()
{
	if(!mCurrentState)
	{
		initStartState();
	}
	return mCurrentState;

}
SE_StateID SE_StateMachine::getCurrentStateID()
{
	if(!mCurrentState)
	{
		initStartState();
		if(!mCurrentState)
		    return SE_StringID::INVALID;
	}
	return mCurrentState->getID();
}
void SE_StateMachine::setStartState(const SE_StringID& stateURI)
{
    mStartURI.setURI(stateURI);
}
SE_StringID SE_StateMachine::getStartState()
{
	return mStartURI.getURL();
}
///////////
void SE_StateChangeList::setStateMachine(SE_StateMachine* stateMachine)
{
	_StateChangeList::iterator it;
	for(it = mStateChangeList.begin() ; it != mStateChangeList.end() ; it++)
	{
		stateMachine->addTranslation(it->from, it->to, it->actionURI);
	}
}
void SE_StateChangeList::add(const SE_StateChange& sc)
{
	mStateChangeList.push_back(sc);
}
