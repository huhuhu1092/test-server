#ifndef OMS_RESOURCELOADER_H
#define OMS_RESOURCELOADER_H
namespace oms
{
	class ResourceLoader
	{
	public:
		ResourceLoader(InputBuffer& inputBuffer, ResourceManager* resourceManager) : mInputBuffer(inputBuffer), mResourceManager(resourceManager)
        {}
        virtual ~ResourceLoader() {}
		virtual void read() = 0;
	private:
		InputBuffer& mInputBuffer;
        ResourceManager* mResourceManager;
	};
}
#endif
