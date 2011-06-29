#ifndef OMS_REFERENCE_H
#define OMS_REFERENCE_H
#if !defined(ANDROID)
namespace oms
{
	class RefBase
	{
	public:
		Reference(bool threadSafe = false);
		Reference(const Reference& ref);
		Reference& operator=(const Reference& ref);
		virtual ~Reference();
		void ref();
		void unref();
	private:
		int mRefCount;
		
	};
	template <typename T>
	class sp
	{
	public:
		
	};
}
#else
#endif
#endif
