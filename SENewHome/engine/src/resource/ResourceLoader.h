#ifndef OMS_RESOURCELOADER_H
#define OMS_RESOURCELOADER_H
namespace oms
{
	class ResourceLoader
	{
	public:
		ResourceLoader(InputBuffer& inputBuffer);
		virtual void read();
	private:
		InputBuffer& mInputBuffer
	};
}
#endif
