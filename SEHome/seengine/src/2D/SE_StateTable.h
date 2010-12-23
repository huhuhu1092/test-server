#ifndef SE_STATETABLE_H
#define SE_STATETABLE_H
#include "SE_ID.h"
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
	void enter();
	void exit();
	void trigger();
	void setEnterAction(SE_StateAction* );
	void setExitAction(SE_StateAction* );
	void setTriggerAction(SE_StateAction* );
private:
	SE_StateID mStateID;
	SE_StateAction* mEnterAction;
	SE_StateAction* mExitAction;
	SE_StateAction* mTriggerAction;
};
class SE_StateTranslation
{
public:
	SE_StateTranslation();
	~SE_StateTranslation();
	void setFrom(const SE_StateID& from);
	void setTo(const SE_StateID& to);
	void set(const SE_StateID& from, const SE_StateID& to);
};
class SE_StateStimulateCondition
{
public:
	virtual ~SE_StateStimulateCondition() {}
	virtual bool 
};
class SE_StateStimulate
{
public:
	SE_StateStimulate();
	~SE_StateStimulate();
	void setID();
	void getID();
	bool canTranslate(SE_State* currState);
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
	void addTranslation(const SE_StimulateID& stimulate, const SE_StateID& from, const SE_StateID& to);
	void removeTranslation(const SE_StimulateID& stimulate, const SE_StateID& from, const SE_StateID& to);
	void addStimulate(SE_StateStimulate* stimulate);
	void removeStimulate(const SE_StimulateID& stimulateID);
	SE_StateStimulate* getStimulate(const SE_StimulateID& stimulateID);
	void trigger(const SE_TriggerID& id);
	void stimulate(const SE_StimulateID& id);
private:
	SE_StateTableID mStateTableID;
	typedef std::list<SE_State*> _StateList;
	_StateList mStateList;
	typedef std::list<SE_StateTranslation*> _TranslationList;
	_TranslationList mTranslationList;
	SE_State* mCurrentState;
};
#endif