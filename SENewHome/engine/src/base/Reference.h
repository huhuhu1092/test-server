#ifndef OMS_REFERENCE_H
#define OMS_REFERENCE_H
#ifndef ANDROID
#include "thread/Mutex.h"
namespace oms
{
#define COMPARE(_op_)									\
	inline bool operator _op_ (const sp<T>& o) const {	\
		return m_ptr _op_ o.m_ptr;						\
	}        
	class RefBase
	{
	public:
		void incStrong(void* id);
		void decStrong(void* id);
		//use for debug, please don't use it
		int32_t getStrongCount() const
		{
			return mRefCount;
		}
	protected:
		RefBase(bool threadSafe = true);
		virtual ~RefBase();
	private:
		RefBase(const RefBase& ref);
		RefBase& operator=(const RefBase& ref);		
	private:
		volatile int32_t mRefCount;
		Mutex* mMutex;
	};
	template <typename T>
	class sp
	{
	public:
    
		inline sp() : m_ptr(0) { }

		sp(T* other);
		sp(const sp<T>& other);
		template<typename U> sp(U* other);
		template<typename U> sp(const sp<U>& other);

		~sp();
    
		// Assignment

		sp& operator = (T* other);
		sp& operator = (const sp<T>& other);
    
		template<typename U> sp& operator = (const sp<U>& other);
		template<typename U> sp& operator = (U* other);
    
		// Reset
    
		void clear();
    
		// Accessors

		inline  T&      operator* () const  { return *m_ptr; }
		inline  T*      operator-> () const { return m_ptr;  }
		inline  T*      get() const         { return m_ptr; }

		// Operators
        
		COMPARE(==)
		COMPARE(!=)
		COMPARE(>)
		COMPARE(<)
		COMPARE(<=)
		COMPARE(>=)

	private:    
		T*              m_ptr;
	};
	
	template<typename T>
		sp<T>::sp(T* other)
		: m_ptr(other)
	{
		if (other) other->incStrong(this);
	}

	template<typename T>
		sp<T>::sp(const sp<T>& other)
		: m_ptr(other.m_ptr)
	{
		if (m_ptr) m_ptr->incStrong(this);
	}

	template<typename T> template<typename U>
		sp<T>::sp(U* other) : m_ptr(other)
	{
		if (other) other->incStrong(this);
	}

	template<typename T> template<typename U>
		sp<T>::sp(const sp<U>& other)
		: m_ptr(other.m_ptr)
	{
		if (m_ptr) m_ptr->incStrong(this);
	}

	template<typename T>
		sp<T>::~sp()
	{
		if (m_ptr) m_ptr->decStrong(this);
	}

	template<typename T>
		sp<T>& sp<T>::operator = (const sp<T>& other) {
		if (other.m_ptr) other.m_ptr->incStrong(this);
		if (m_ptr) m_ptr->decStrong(this);
		m_ptr = other.m_ptr;
		return *this;
	}

	template<typename T>
		sp<T>& sp<T>::operator = (T* other)
		{
			if (other) other->incStrong(this);
			if (m_ptr) m_ptr->decStrong(this);
			m_ptr = other;
			return *this;
		}

	template<typename T>
		template<typename U>
		sp<T>& sp<T>::operator = (const sp<U>& other)
		{
			if (other.m_ptr) other.m_ptr->incStrong(this);
			if (m_ptr) m_ptr->decStrong(this);
			m_ptr = other.m_ptr;
			return *this;
		}

	template<typename T>
		template<typename U>
		sp<T>& sp<T>::operator = (U* other)
		{
			if (other) other->incStrong(this);
			if (m_ptr) m_ptr->decStrong(this);
			m_ptr = other;
			return *this;
		}


	template<typename T>
	void sp<T>::clear()
	{
		if (m_ptr) {
			m_ptr->decStrong(this);
			m_ptr = 0;
		}
	}


}
#else
#include <utils/RefBase.h>
using namespace android;
#endif
#endif
